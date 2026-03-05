# ==========================================================
# CARGA DE DATOS
# ==========================================================

load_data <- function() {
  if (!file.exists(PATH_CSV))  stop("No se encontr\u00f3 el CSV base en data/.")
  if (!file.exists(PATH_SECC)) stop("No se encontr\u00f3 el GPKG de secciones en data/.")

  base_dt <- read_csv_flex(PATH_CSV)
  if ("SECCION"      %in% names(base_dt)) base_dt[, SECCION      := as.integer(SECCION)]
  if ("ID_ENTIDAD"   %in% names(base_dt)) base_dt[, ID_ENTIDAD   := as.integer(ID_ENTIDAD)]
  if ("ID_MUNICIPIO" %in% names(base_dt)) base_dt[, ID_MUNICIPIO := as.integer(ID_MUNICIPIO)]

  if (all(c("ID_ENTIDAD","ID_MUNICIPIO") %in% names(base_dt))) {
    base_dt <- base_dt[ID_ENTIDAD == ID_ENTIDAD_FIJA & ID_MUNICIPIO == ID_MUNICIPIO_FIJO]
  } else if ("ID_MUNICIPIO" %in% names(base_dt)) {
    base_dt <- base_dt[ID_MUNICIPIO == ID_MUNICIPIO_FIJO]
  }

  sec_sf  <- st_read(PATH_SECC, quiet = TRUE) |> safe_make_valid()
  nms     <- names(sec_sf)
  col_sec <- pick_col(nms, c("SECCION","SECC","SECCION_ID"))
  if (is.na(col_sec)) stop("El GPKG de secciones no tiene columna SECCION/SECC/SECCION_ID.")
  sec_sf[[col_sec]] <- as.integer(sec_sf[[col_sec]])

  col_ent <- pick_col(nms, c("ENTIDAD","ID_ENTIDAD","ID_ESTADO"))
  col_mun <- pick_col(nms, c("MUNICIPIO","ID_MUNICIPIO","MUN"))
  if (!is.na(col_ent)) sec_sf[[col_ent]] <- as.integer(sec_sf[[col_ent]])
  if (!is.na(col_mun)) sec_sf[[col_mun]] <- as.integer(sec_sf[[col_mun]])

  if (!is.na(col_ent) && !is.na(col_mun)) {
    sec_sf <- sec_sf[sec_sf[[col_ent]] == ID_ENTIDAD_FIJA & sec_sf[[col_mun]] == ID_MUNICIPIO_FIJO, ]
  } else if (!is.na(col_mun)) {
    sec_sf <- sec_sf[sec_sf[[col_mun]] == ID_MUNICIPIO_FIJO, ]
  }

  sec_sf <- tryCatch(st_transform(sec_sf, 4326), error = function(e) sec_sf)
  if (is.na(st_crs(sec_sf))) st_crs(sec_sf) <- 4326

  dl_sf <- NULL
  if (file.exists(PATH_DL)) {
    dl_sf <- st_read(PATH_DL, quiet = TRUE) |> safe_make_valid()
    dl_sf <- tryCatch(st_transform(dl_sf, 4326), error = function(e) dl_sf)
    if (is.na(st_crs(dl_sf))) st_crs(dl_sf) <- st_crs(sec_sf)
  }

  sec_dt <- as.data.table(st_drop_geometry(sec_sf))
  setnames(sec_dt, col_sec, "SECCION")
  sec_dt[, SECCION := as.integer(SECCION)]
  sec_dt[, geom := st_geometry(sec_sf)]
  setkey(sec_dt, SECCION)

  if (!("SECCION" %in% names(base_dt))) stop("El CSV no tiene columna SECCION.")
  setkey(base_dt, SECCION)
  dt <- merge(sec_dt, base_dt, by = "SECCION", all.x = TRUE)

  sf_all <- st_as_sf(as.data.frame(dt), sf_column_name = "geom", crs = st_crs(sec_sf))
  class(sf_all) <- setdiff(class(sf_all), "data.table")
  list(sf = sf_all, dl = dl_sf)
}

# ==========================================================
# ELECCIONES + HELPERS DE METRICAS
# ==========================================================

detect_elections <- function(nms) {
  m <- stringr::str_match(nms, "(AYU|DL)_(\\d{2})$")
  m <- m[!is.na(m[,1L]),, drop = FALSE]
  if (nrow(m) == 0L) return(data.table(office=character(0), yr2=character(0)))
  unique(data.table(office = m[,2L], yr2 = m[,3L]))
}

parse_key <- function(key) {
  sp <- strsplit(key, "_", fixed = TRUE)[[1L]]
  list(office = sp[1L] %||% "AYU", yr2 = sp[2L] %||% "24",
       year   = as.integer(paste0("20", sp[2L] %||% "24")))
}

metric_col <- function(df, key, base_name) {
  k <- parse_key(key)
  cands <- unique(c(
    if (k$office == "AYU" && k$yr2 == "24") base_name else NULL,
    paste0(base_name,"_",k$office,"_",k$yr2),
    paste0(base_name,"_",k$yr2),
    base_name
  ))
  hit <- cands[cands %in% names(df)]
  hit[1L] %||% NA_character_
}

valid_col <- function(df, key) {
  cands <- unique(c(
    metric_col(df, key, "NUM_VOTOS_VALIDOS"),
    metric_col(df, key, "VOTOS_VALIDOS"),
    metric_col(df, key, "TOTAL_VOTOS")
  ))
  cands <- cands[!is.na(cands)]
  cands[cands %in% names(df)][1L] %||% NA_character_
}
total_col <- function(df, key) metric_col(df, key, "TOTAL_VOTOS")
ln_col    <- function(df, key) metric_col(df, key, "LISTA_NOMINAL")

vote_cols_raw <- function(df, key, vote_type) {
  k   <- parse_key(key)
  nms <- names(df)
  if (vote_type == "CAND") {
    return(grep(paste0("^CAND_.*",k$office,"_",k$yr2,"$"), nms, value = TRUE))
  }
  grep(paste0("_",vote_type,"_",k$office,"_",k$yr2,"$"), nms, value = TRUE)
}

party_from_col <- function(col, key, vote_type) {
  k      <- parse_key(key)
  suffix <- paste0("_",k$office,"_",k$yr2,"$")
  if (vote_type == "CAND") {
    x <- sub(suffix,"",col)
    x <- sub("^CAND_","",x)
    return(gsub("_"," ",x))
  }
  sub(paste0("_",vote_type,suffix),"",col)
}

vote_cols_solo <- function(df, key, vote_type) {
  cols <- vote_cols_raw(df, key, vote_type)
  if (!length(cols)) return(character(0))
  if (vote_type == "CAND") return(cols)
  labs <- vapply(cols, party_from_col, character(1), key=key, vote_type=vote_type)
  cols[!grepl("_", labs)]
}

safe_num_matrix <- function(df, cols) {
  cols <- cols[cols %in% names(df)]
  if (!length(cols)) return(NULL)
  dat0 <- as.data.frame(st_drop_geometry(df))
  M    <- as.matrix(dat0[, cols, drop = FALSE])
  suppressWarnings(storage.mode(M) <- "numeric")
  M[is.na(M)] <- 0
  M
}

group_votes_matrix <- function(df, key, vote_type) {
  cols <- vote_cols_solo(df, key, vote_type)
  if (!length(cols)) return(list(G=NULL, parties=character(0), cols=character(0)))
  M <- safe_num_matrix(df, cols)
  if (is.null(M)) return(list(G=NULL, parties=character(0), cols=character(0)))
  groups <- vapply(cols, party_from_col, character(1), key=key, vote_type=vote_type)
  groups[is.na(groups)|groups==""] <- "OTROS"
  idx_list <- split(seq_along(cols), groups)
  G <- vapply(idx_list, function(idx) rowSums(M[,idx,drop=FALSE], na.rm=TRUE), numeric(nrow(df)))
  if (!is.matrix(G)) G <- matrix(G, nrow=nrow(df))
  colnames(G) <- names(idx_list)
  list(G=G, parties=colnames(G), cols=cols)
}

winner_by_row <- function(df, key, vote_type) {
  gv <- group_votes_matrix(df, key, vote_type)
  G  <- gv$G
  if (is.null(G)||ncol(G)==0L) return(rep(NA_character_, nrow(df)))
  idx <- max.col(G, ties.method="first")
  w   <- colnames(G)[idx]
  w[rowSums(G,na.rm=TRUE)<=0] <- NA_character_
  w
}

totals_for_view <- function(df, key, vote_type) {
  vote_type <- toupper(vote_type %||% "DISTRIBUIDO")
  if (vote_type == "CAND") {
    cols <- vote_cols_raw(df, key, "CAND")
    if (!length(cols)) return(setNames(numeric(0),character(0)))
    M   <- safe_num_matrix(df, cols)
    if (is.null(M)) return(setNames(numeric(0),character(0)))
    tot <- colSums(M, na.rm=TRUE)
    keep <- which(is.finite(tot) & tot > 0)
    if (!length(keep)) return(setNames(numeric(0),character(0)))
    cols_k <- cols[keep]
    labs   <- make.unique(vapply(cols_k, party_from_col, character(1), key=key, vote_type="CAND"))
    return(sort(setNames(as.numeric(tot[keep]),labs), decreasing=TRUE))
  }
  gv  <- group_votes_matrix(df, key, vote_type)
  G   <- gv$G
  if (is.null(G)||ncol(G)==0L) return(setNames(numeric(0),character(0)))
  tot <- colSums(G, na.rm=TRUE)
  tot <- tot[is.finite(tot)&tot>0]
  if (!length(tot)) return(setNames(numeric(0),character(0)))
  if (length(tot)>=2L && "OTROS" %in% names(tot)) {
    tot2 <- tot[names(tot) != "OTROS"]
    if (length(tot2) >= 2L) tot <- tot2
  }
  sort(tot, decreasing=TRUE)
}

top2_from_totals <- function(tot) {
  tot <- tot[is.finite(tot)&tot>0]
  if (!length(tot)) return(list(w1=NA_character_,v1=NA_real_,w2=NA_character_,v2=NA_real_))
  tot <- sort(tot, decreasing=TRUE)
  list(
    w1 = names(tot)[1L] %||% NA_character_,  v1 = as.numeric(tot[[1L]] %||% NA_real_),
    w2 = names(tot)[2L] %||% NA_character_,  v2 = as.numeric(tot[[2L]] %||% NA_real_)
  )
}

