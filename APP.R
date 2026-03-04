
suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(sf)
  library(leaflet)
  library(bslib)
  library(plotly)
  library(DT)
  library(stringr)
  library(htmltools)
  library(dplyr)
  library(shinymanager)
})

sf::sf_use_s2(FALSE)

# ──Credenciales ─────────────────────────────────────
# Credenciales se leen automáticamente de .Renviron:
# ── Credenciales desde .Renviron ─────────────────────────────
GOOGLE_API_KEY      <- Sys.getenv("GOOGLE_API_KEY", unset = "")
OPENAI_API_KEY      <- Sys.getenv("OPENAI_API_KEY", unset = "")
AWS_ACCESS_KEY_ID   <- Sys.getenv("AWS_ACCESS_KEY_ID", unset = "")
AWS_SECRET_ACCESS_KEY <- Sys.getenv("AWS_SECRET_ACCESS_KEY", unset = "")
AWS_DEFAULT_REGION  <- Sys.getenv("AWS_DEFAULT_REGION", unset = "")

# ==========================================================
# HELPERS GENERALES
# ==========================================================

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
}

withSpinner <- function(x, ...) x
if (requireNamespace("shinycssloaders", quietly = TRUE)) {
  withSpinner <- shinycssloaders::withSpinner
}

has_fullscreen <- requireNamespace("leaflet.extras", quietly = TRUE)

pick_col <- function(nms, candidates) {
  hit <- intersect(candidates, nms)
  if (length(hit) == 0L) NA_character_ else hit[1L]
}

safe_make_valid <- function(x) {
  tryCatch(st_make_valid(x), error = function(e) x)
}

read_csv_flex <- function(path) {
  for (enc in c("UTF-8", "Latin-1")) {
    out <- tryCatch(fread(path, encoding = enc), error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  fread(path)
}

as_num <- function(x) suppressWarnings(as.numeric(x))

fmt_int <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "\u2014", formatC(x, format = "f", digits = 0, big.mark = ","))
}
fmt_pct <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "\u2014", paste0(formatC(100 * x, format = "f", digits = 2), "%"))
}
fmt_signed_int <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "\u2014",
         paste0(ifelse(x >= 0, "+", "\u2212"),
                formatC(abs(x), format = "f", digits = 0, big.mark = ",")))
}
fmt_signed_pp <- function(x_pp) {
  x_pp <- suppressWarnings(as.numeric(x_pp))
  ifelse(is.na(x_pp), "\u2014",
         paste0(ifelse(x_pp >= 0, "+", "\u2212"),
                formatC(abs(x_pp), format = "f", digits = 2), " pp"))
}

safe_centroids_wgs84 <- function(x) {
  if (is.null(x) || NROW(x) == 0L) return(x)
  x2 <- tryCatch(st_transform(x, 4326), error = function(e) x)
  ctr <- suppressWarnings(st_centroid(x2))
  if (is.na(st_crs(ctr))) st_crs(ctr) <- 4326
  ctr
}

coords_from_sf <- function(x) {
  if (is.null(x) || NROW(x) == 0L) {
    return(data.frame(LNG = numeric(0), LAT = numeric(0)))
  }
  xy <- st_coordinates(x)
  data.frame(
    LNG = suppressWarnings(as.numeric(xy[, "X"])),
    LAT = suppressWarnings(as.numeric(xy[, "Y"]))
  )
}

# ==========================================================
# GOOGLE MAPS REVERSE GEOCODE
# ==========================================================

reverse_geocode <- function(lat, lng, api_key = NULL) {
  if (is.null(api_key) || !nzchar(api_key)) return(NA_character_)
  if (!is.finite(lat) || !is.finite(lng)) return(NA_character_)
  tryCatch({
    url <- paste0(
      "https://maps.googleapis.com/maps/api/geocode/json?latlng=",
      lat, ",", lng, "&key=", api_key, "&language=es"
    )
    resp <- jsonlite::fromJSON(url, simplifyVector = TRUE)
    if (!is.null(resp$results) && NROW(resp$results) > 0L) {
      resp$results$formatted_address[1L]
    } else NA_character_
  }, error = function(e) NA_character_)
}

# ==========================================================
# PATHS + CONFIG
# ==========================================================

PATH_DL   <- "data/dl_atizapan.gpkg"
PATH_SECC <- "data/secciones_atizapan.gpkg"
PATH_CSV  <- "data/EDOMEX_MUN_13__SECCIONES__JOIN_2018_2021_2024__AYU_BASE.csv"
PATH_TRAD <- "data/TRADUCTOR.csv"

ID_ENTIDAD_FIJA   <- 15L
ID_MUNICIPIO_FIJO <- 13L

APP_TITLE <- "electrend \u00b7 Atizap\u00e1n \u00b7 EDOMEX"
APP_SUB   <- "Explorador electoral por secci\u00f3n"

credentials <- data.frame(
  user     = c("admin", "Pau"),
  password = c("123", "Pau2026!"),
  admin    = c(TRUE, TRUE),
  stringsAsFactors = FALSE
)

logo_login <- NA_character_
for (nm in c("LOGO.PNG", "LOGO.png", "logo.png", "logo.jpg", "LOGO.jpg", "LOGO.PNG.png")) {
  if (file.exists(file.path("www", nm))) {
    logo_login <- nm
    break
  }
}

# ==========================================================
# COLORES Y LOGOS
# ==========================================================

ACCENT  <- "#D50000"
ACCENT2 <- "#8B0000"
BG      <- "#070A0F"
CARD    <- "rgba(255,255,255,.06)"
LINE    <- "rgba(255,255,255,.12)"
TXT     <- "#FFFFFF"
MUTED   <- "rgba(255,255,255,.88)"

party_colors <- c(
  PAN    = "#005BAC", PRI    = "#D50000", PRD    = "#FFD200",
  PVEM   = "#2EAD4A", PT     = "#E2001A", MC     = "#FF6A00",
  MORENA = "#7A013A", NAEM   = "#00B5E2", ES     = "#EC008C",
  PANAL  = "#00AEEF", PES    = "#5B2C83", RSP    = "#C71585",
  FXM    = "#9B1B72", SI     = "#00C1B2", VIDA   = "#4CAF50"
)

party_logo_map <- c(
  PAN    = "PAN.jpg",    PRI    = "PRI.jpg",    PRD    = "PRD.jpg",
  PVEM   = "PVEM.jpg",  PT     = "PT.jpg",     MC     = "MC.jpg",
  MORENA = "MORENA.jpg",PANAL  = "PANAL.png",  NAEM   = "NAEM.png",
  PES    = "PES.png",   ES     = "ESO.jpg",    RSP    = "RSP.png",
  FXM    = "FXM.png",   SI     = "PSI.png",    VIDA   = "VIDA.jpg"
)

www_files <- character(0)
if (dir.exists("www")) {
  www_files <- list.files("www", pattern = "\\.(jpg|jpeg|png|svg)$", ignore.case = TRUE)
  party_logo_map <- party_logo_map[party_logo_map %in% www_files]
  for (p in names(party_colors)) {
    if (!(p %in% names(party_logo_map))) {
      cand <- www_files[toupper(tools::file_path_sans_ext(www_files)) == toupper(p)]
      if (length(cand) > 0L) party_logo_map[[p]] <- cand[1L]
    }
  }
  for (f in www_files) {
    tag <- toupper(tools::file_path_sans_ext(f))
    if (!(tag %in% names(party_logo_map))) party_logo_map[[tag]] <- f
  }
}

# ---- party_logo_src mejorado: soporta coaliciones ----
party_logo_src <- function(party) {
  # Always scalar-safe
  party <- as.character(party %||% "")[1L]
  party <- toupper(trimws(party))
  if (!nzchar(party) || is.na(party)) return(NULL)

  # Coincidencia directa
  if (party %in% names(party_logo_map)) return(unname(party_logo_map[[party]]))

  # Sin espacios / con guión bajo
  party_under  <- gsub("[[:space:]]+", "_", party)
  party_nospace <- gsub("[[:space:]]+", "", party)
  for (p2 in c(party_under, party_nospace)) {
    if (p2 %in% names(party_logo_map)) return(unname(party_logo_map[[p2]]))
  }

  # Coalición: separar por - + / | & y probar cada parte
  parts <- unlist(strsplit(party, "[-+/|&,]"))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  for (pt in parts) {
    if (pt %in% names(party_logo_map)) return(unname(party_logo_map[[pt]]))
    for (p in names(party_logo_map)) {
      if (grepl(paste0("\\b", p, "\\b"), pt)) return(unname(party_logo_map[[p]]))
    }
  }

  # Búsqueda parcial en el label completo
  known <- names(party_logo_map)
  known <- known[order(-nchar(known))]
  for (p in known) {
    if (grepl(paste0("\\b", p, "\\b"), party) || grepl(p, party, fixed = TRUE)) {
      return(unname(party_logo_map[[p]]))
    }
  }

  # Por color conocido
  for (p in names(party_colors)) {
    if (grepl(paste0("\\b", p, "\\b"), party)) {
      src <- party_logo_map[[p]]
      if (!is.null(src)) return(unname(src))
    }
  }
  NULL
}

party_logo_inline <- function(party, height = "16px") {
  src <- party_logo_src(party)
  if (is.null(src)) return("")
  paste0(
    '<img src="', src, '" height="', height,
    '" style="border-radius:3px;object-fit:contain;vertical-align:middle;',
    'box-shadow:0 1px 4px rgba(0,0,0,.30);margin-right:4px;" alt="', htmltools::htmlEscape(party), '">'
  )
}

party_logo_b64 <- function(party) {
  src <- party_logo_src(party)
  if (is.null(src)) return(NULL)
  path <- file.path("www", src)
  if (!file.exists(path)) return(NULL)
  tryCatch({
    raw  <- readBin(path, "raw", file.info(path)$size)
    ext  <- tolower(tools::file_ext(src))
    mime <- switch(ext, jpg = "image/jpeg", jpeg = "image/jpeg", png = "image/png",
                   svg = "image/svg+xml", "image/png")
    paste0("data:", mime, ";base64,", base64enc::base64encode(raw))
  }, error = function(e) NULL)
}

guess_party_from_cand_label <- function(label) {
  up <- toupper(label %||% "")
  known <- names(party_colors)
  known <- known[order(-nchar(known))]
  for (p in known) if (grepl(paste0("\\b", p, "\\b"), up)) return(p)
  for (p in names(party_logo_map)) if (grepl(paste0("\\b", p, "\\b"), up)) return(p)
  NA_character_
}

# ---- Añadir logos de partidos a una gráfica plotly horizontal ----
add_logos_to_plotly_h <- function(p, party_labels, size = 0.055, gap = NULL) {
  n <- length(party_labels)
  if (n == 0L) return(p)

  # Step size between bars in paper coords (0..1)
  step <- if (n > 1L) 1 / (n - 1) else 0

  images <- list()
  for (i in seq_along(party_labels)) {
    lbl <- as.character(party_labels[i])
    b64 <- party_logo_b64(lbl)
    if (is.null(b64)) {
      pg <- guess_party_from_cand_label(lbl)
      if (!is.na(pg)) b64 <- party_logo_b64(pg)
    }
    if (is.null(b64)) next

    y_pos <- if (n > 1L) (i - 1) * step else 0.5

    images[[length(images) + 1L]] <- list(
      source  = b64,
      x       = -0.010,        # just left of the axis
      y       = y_pos,
      xref    = "paper",
      yref    = "paper",
      xanchor = "right",
      yanchor = "middle",
      sizex   = size,
      sizey   = size,           # square — cleaner
      layer   = "above"
    )
  }
  if (length(images) > 0L) p <- p |> layout(images = images)
  p
}

# ==========================================================
# BASEMAPS
# ==========================================================

BASEMAPS <- c(
  "Oscuro"       = "CartoDB.DarkMatter",
  "Claro"        = "CartoDB.Positron",
  "Calles"       = "OpenStreetMap.Mapnik",
  "Sat\u00e9lite" = "Esri.WorldImagery"
)
BASEMAP_DEFAULT <- "Oscuro"
DL_GROUP        <- "Distritos locales"

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

dat    <- load_data()
sf_all <- dat$sf
dl_sf  <- dat$dl

SECC_DL_COL <- pick_col(names(sf_all), c("DISTRITO_L","DISTRITO_LOCAL","ID_DISTRITO_LOCAL","DIST_L","DISTRITO"))
if (!is.na(SECC_DL_COL)) suppressWarnings(sf_all[[SECC_DL_COL]] <- as.integer(sf_all[[SECC_DL_COL]]))

if (!is.null(dl_sf) && nrow(dl_sf) > 0L) {
  DL_ID_COL <- pick_col(names(dl_sf), c("DISTRITO_L","DISTRITO_LOCAL","ID_DISTRITO_LOCAL","DIST_L","DISTRITO"))
  if (!is.na(DL_ID_COL)) {
    suppressWarnings(dl_sf[[DL_ID_COL]] <- as.integer(dl_sf[[DL_ID_COL]]))
    dl_sf$DISTRITO_L <- dl_sf[[DL_ID_COL]]
  } else if (!("DISTRITO_L" %in% names(dl_sf))) dl_sf$DISTRITO_L <- NA_integer_
}

# TRADUCTOR / INEGI
TRADUCTOR <- tryCatch({
  if (!file.exists(PATH_TRAD)) stop("TRADUCTOR no encontrado")
  trad <- read_csv_flex(PATH_TRAD)
  if (!all(c("Eje","Indicador","VARIABLE") %in% names(trad))) stop("TRADUCTOR incompleto")
  as.data.frame(trad)
}, error = function(e) data.frame(Eje=character(0),Indicador=character(0),VARIABLE=character(0), stringsAsFactors=FALSE))

INEGI_COLS  <- grep("_INEGI$", names(sf_all), value = TRUE)
INEGI_VARS  <- sub("_INEGI$", "", INEGI_COLS)
INEGI_COL_MAP <- setNames(INEGI_COLS, INEGI_VARS)

if (NROW(TRADUCTOR) > 0L) {
  TRADUCTOR <- TRADUCTOR[TRADUCTOR$VARIABLE %in% INEGI_VARS, , drop = FALSE]
  if (NROW(TRADUCTOR) > 0L) TRADUCTOR$COL_NAME <- INEGI_COL_MAP[TRADUCTOR$VARIABLE]
}
EJES_DISPONIBLES <- if (NROW(TRADUCTOR) > 0L) unique(TRADUCTOR$Eje) else character(0)

INIT_BBOX <- sf::st_bbox(if (!is.null(dl_sf) && nrow(dl_sf) > 0) dl_sf else sf_all)
INIT_LNG  <- as.numeric((INIT_BBOX["xmin"] + INIT_BBOX["xmax"]) / 2)
INIT_LAT  <- as.numeric((INIT_BBOX["ymin"] + INIT_BBOX["ymax"]) / 2)
INIT_ZOOM <- 12L

# ==========================================================
# ELECCIONES + HELPERS DE MÉTRICAS
# ==========================================================

detect_elections <- function(nms) {
  m <- stringr::str_match(nms, "(AYU|DL)_(\\d{2})$")
  m <- m[!is.na(m[,1L]),, drop = FALSE]
  if (nrow(m) == 0L) return(data.table(office=character(0), yr2=character(0)))
  unique(data.table(office = m[,2L], yr2 = m[,3L]))
}

elex <- detect_elections(names(sf_all))
if (all(c("TOTAL_VOTOS","LISTA_NOMINAL") %in% names(sf_all)) &&
    !any(elex$office == "AYU" & elex$yr2 == "24")) {
  elex <- unique(rbind(elex, data.table(office="AYU", yr2="24")))
}
if (nrow(elex) == 0L) elex <- data.table(office="AYU", yr2="24")

elex[, year       := as.integer(paste0("20", yr2))]
elex[, office_lbl := fifelse(office == "AYU","Ayuntamientos","Dip. Local")]
elex[, key        := paste0(office,"_",yr2)]
elex[, label      := paste0(year," \u00b7 ",office_lbl)]
elex[, office_ord := fifelse(office == "AYU",1L,2L)]
setorder(elex, year, office_ord)

ELECTION_CHOICES <- as.list(setNames(elex$key, elex$label))
DEFAULT_ELECTION <- {
  if ("AYU_24" %in% elex$key) "AYU_24"
  else if (any(elex$office == "AYU")) elex[office == "AYU"][.N, key]
  else tail(elex$key,1L)
} %||% "AYU_24"

parse_key <- function(key) {
  sp <- strsplit(key, "_", fixed = TRUE)[[1L]]
  list(office = sp[1L] %||% "AYU", yr2 = sp[2L] %||% "24",
       year   = as.integer(paste0("20", sp[2L] %||% "24")))
}

key_label <- function(key) elex$label[match(key, elex$key)] %||% key

REF_KEY <- {
  ayu <- elex$key[elex$office == "AYU"]
  if (length(ayu)) tail(ayu,1L) else tail(elex$key,1L)
} %||% DEFAULT_ELECTION

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

guess_party_for_color <- function(label) {
  up <- toupper(label %||% "")
  candidates <- names(party_colors)
  hit <- candidates[vapply(candidates, function(p) grepl(paste0("\\b",p,"\\b"), up), logical(1))]
  hit[1L] %||% NA_character_
}

fill_color_winner <- function(w) {
  p <- guess_party_for_color(w)
  if (!is.na(p) && p %in% names(party_colors)) return(unname(party_colors[[p]]))
  "#6B7280"
}

# ==========================================================
# PALETAS
# ==========================================================

pal_bw_accent <- function(domain, accent=ACCENT) {
  colorNumeric(
    palette  = grDevices::colorRampPalette(c("#111111","#FFFFFF",accent))(256),
    domain   = domain,
    na.color = "#00000000"
  )
}
make_pal_pos <- function(values, scale=c("linear","quantile"), accent=ACCENT) {
  scale <- match.arg(scale)
  v  <- suppressWarnings(as.numeric(values)); v[!is.finite(v)] <- NA_real_
  dom <- v[is.finite(v)]
  if (length(dom)<2L||length(unique(dom))<2L) {
    pal <- pal_bw_accent(c(0,1),accent=accent)
    return(list(pal=pal, values=ifelse(is.na(v),NA_real_,0)))
  }
  if (scale == "quantile") {
    br <- as.numeric(quantile(dom,probs=seq(0,1,length.out=8),na.rm=TRUE,type=7))
    br <- unique(br[is.finite(br)])
    if (length(br)>=3L) {
      n_bins <- max(3L,length(br)-1L)
      cols   <- grDevices::colorRampPalette(c("#111111","#FFFFFF",accent))(n_bins)
      pal    <- colorBin(cols,bins=br,domain=dom,na.color="#00000000",pretty=FALSE)
      return(list(pal=pal, values=v))
    }
  }
  pal <- pal_bw_accent(dom, accent=accent)
  list(pal=pal, values=v)
}
make_pal_delta <- function(values, scale=c("linear","quantile"), accent=ACCENT) {
  scale <- match.arg(scale)
  v  <- suppressWarnings(as.numeric(values)); v[!is.finite(v)] <- NA_real_
  dom <- v[is.finite(v)]
  if (length(dom)<2L||length(unique(dom))<2L) {
    pal <- pal_bw_accent(c(-1,1),accent=accent)
    return(list(pal=pal, values=ifelse(is.na(v),NA_real_,0)))
  }
  if (scale=="quantile") {
    br <- as.numeric(quantile(dom,probs=seq(0,1,length.out=8),na.rm=TRUE,type=7))
    br <- unique(br[is.finite(br)])
    if (length(br)>=3L) {
      n_bins <- max(3L,length(br)-1L)
      cols   <- grDevices::colorRampPalette(c("#111111","#FFFFFF",accent))(n_bins)
      pal    <- colorBin(cols,bins=br,domain=dom,na.color="#00000000",pretty=FALSE)
      return(list(pal=pal, values=v))
    }
  }
  M <- as.numeric(quantile(abs(dom),probs=0.98,na.rm=TRUE,type=7))
  if (!is.finite(M)||M<=0) M <- max(abs(dom),na.rm=TRUE)
  if (!is.finite(M)||M<=0) M <- 1
  pal <- pal_bw_accent(c(-M,M), accent=accent)
  list(pal=pal, values=v)
}

# ==========================================================
# ELECTORADO
# ==========================================================

ELECTORADO_CATALOG <- list(
  "Lista nominal"       = list(col="LISTA_NOMINAL",             tipo="abs",color="#00B5E2"),
  "Hombres"             = list(col="LISTA_HOMBRES",             tipo="abs",color="#005BAC"),
  "Mujeres"             = list(col="LISTA_MUJERES",             tipo="abs",color="#EC008C"),
  "Gen Z (N)"           = list(col="LISTA_GEN_Z",              tipo="abs",color="#2EAD4A"),
  "Millennials (N)"     = list(col="LISTA_MILLENNIALS",        tipo="abs",color="#FF6A00"),
  "Gen X (N)"           = list(col="LISTA_GEN_X",              tipo="abs",color="#FFD200"),
  "Boomers (N)"         = list(col="LISTA_BOOMERS",            tipo="abs",color="#D50000"),
  "Adultos mayores (N)" = list(col="LISTA_ADULTOS_MAYORES",    tipo="abs",color="#9B1B72"),
  "Gen Z (%)"           = list(col="PCT_LISTA_GEN_Z",          tipo="pct",color="#2EAD4A"),
  "Millennials (%)"     = list(col="PCT_LISTA_MILLENNIALS",    tipo="pct",color="#FF6A00"),
  "Gen X (%)"           = list(col="PCT_LISTA_GEN_X",          tipo="pct",color="#FFD200"),
  "Boomers (%)"         = list(col="PCT_LISTA_BOOMERS",        tipo="pct",color="#D50000"),
  "Adultos mayores (%)" = list(col="PCT_LISTA_ADULTOS_MAYORES",tipo="pct",color="#9B1B72")
)
ELECTORADO_AVAILABLE <- Filter(function(x) x$col %in% names(sf_all), ELECTORADO_CATALOG)
ELECTORADO_CHOICES   <- as.list(setNames(
  vapply(ELECTORADO_AVAILABLE, `[[`, character(1), "col"),
  names(ELECTORADO_AVAILABLE)
))

# ==========================================================
# MAP HELPERS
# ==========================================================

add_layers_control <- function(m) {
  og <- if (!is.null(dl_sf) && nrow(dl_sf) > 0) DL_GROUP else character(0)
  if (length(og) > 0) {
    m |> leaflet::addLayersControl(
      baseGroups    = names(BASEMAPS),
      overlayGroups = og,
      options       = leaflet::layersControlOptions(collapsed=TRUE, position="topright")
    )
  } else {
    m |> leaflet::addLayersControl(
      baseGroups = names(BASEMAPS),
      options    = leaflet::layersControlOptions(collapsed=TRUE, position="topright")
    )
  }
}

add_dl_layer <- function(m, dl_data=dl_sf) {
  if (!is.null(dl_data) && nrow(dl_data) > 0) {
    m |> leaflet::addPolygons(
      data        = dl_data,
      fillColor   = "white",
      fillOpacity = 0.06,
      label       = ~paste0("DL ", DISTRITO_L),
      color       = "rgba(255,255,255,.40)",
      weight      = 1.5,
      opacity     = 0.90,
      group       = DL_GROUP
    )
  } else m
}

restore_map_controls <- function(proxy) {
  proxy <- proxy |> leaflet::addScaleBar(position="bottomleft")
  if (has_fullscreen) {
    proxy <- leaflet.extras::addFullscreenControl(proxy, position="topleft", pseudoFullscreen=FALSE)
  }
  proxy
}

# ==========================================================
# THEME + CSS (TEXTO BLANCO, LOGOS VISIBLES)
# ==========================================================

theme <- bs_theme(
  bootswatch = "darkly",
  primary    = ACCENT,
  base_font  = "system-ui",
  font_scale = 1
)

css <- paste0("
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;900&display=swap');

/* ====================================================
   VARIABLES & RESET
==================================================== */
:root{
  --accent:   ", ACCENT, ";
  --accent2:  ", ACCENT2, ";
  --bg:       ", BG, ";
  --txt:      #FFFFFF;
  --muted:    rgba(255,255,255,.82);
  --glass-bg: rgba(255,255,255,.055);
  --glass-border: rgba(255,255,255,.11);
  --glow:     rgba(213,0,0,.45);
  --glow-soft:rgba(213,0,0,.18);
  --radius:   18px;
  --radius-sm:12px;
  --trans:    all .22s cubic-bezier(.4,0,.2,1);
}
*, *::before, *::after{ box-sizing:border-box; margin:0; padding:0; }

/* ====================================================
   ANIMATED BACKGROUND
==================================================== */
body{
  font-family:'Inter',system-ui,-apple-system,sans-serif !important;
  color:#FFFFFF !important;
  background:", BG, " !important;
  background-image:
    radial-gradient(ellipse 900px 600px at 8% -5%,  rgba(213,0,0,.30), transparent 55%),
    radial-gradient(ellipse 700px 500px at 95% 10%, rgba(139,0,0,.18), transparent 50%),
    radial-gradient(ellipse 500px 400px at 50% 80%, rgba(213,0,0,.10), transparent 55%),
    radial-gradient(ellipse 300px 300px at 20% 60%, rgba(255,100,0,.06), transparent 50%)
    !important;
  background-attachment: fixed !important;
  min-height:100vh;
  overflow-x:hidden;
}

/* Animated noise overlay */
body::before{
  content:'';
  position:fixed; inset:0; z-index:0; pointer-events:none;
  opacity:.028;
  background-image:url(\"data:image/svg+xml,%3Csvg viewBox='0 0 256 256' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.9' numOctaves='4' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)'/%3E%3C/svg%3E\");
  background-size: 180px 180px;
}

/* Floating ambient orbs */
body::after{
  content:'';
  position:fixed; inset:0; z-index:0; pointer-events:none;
  background:
    radial-gradient(circle 280px at 15% 25%, rgba(213,0,0,.07), transparent),
    radial-gradient(circle 200px at 80% 75%, rgba(139,0,0,.06), transparent);
  animation: orb-drift 18s ease-in-out infinite alternate;
}
@keyframes orb-drift{
  0%  { transform:translate(0,0) scale(1); }
  50% { transform:translate(30px,-20px) scale(1.05); }
  100%{ transform:translate(-20px,30px) scale(.97); }
}

/* Everything sits above the pseudo overlays */
.shiny-fill-page, .bslib-page-fill, .container-fluid,
.bslib-sidebar-layout, .bslib-main, .bslib-sidebar,
.bslib-navs-left, nav, .tab-content { position:relative; z-index:1; }

/* ====================================================
   GLASSMORPHISM CARDS
==================================================== */
.glass, .glass-card{
  background: linear-gradient(145deg,
    rgba(255,255,255,.09) 0%,
    rgba(255,255,255,.04) 60%,
    rgba(255,255,255,.07) 100%);
  border: 1px solid rgba(255,255,255,.12);
  border-top-color: rgba(255,255,255,.22);
  border-left-color: rgba(255,255,255,.16);
  border-radius: var(--radius);
  box-shadow:
    0 2px 0 rgba(255,255,255,.06) inset,
    0 20px 60px rgba(0,0,0,.50),
    0 4px 24px rgba(0,0,0,.30);
  backdrop-filter: blur(18px) saturate(160%);
  transition: var(--trans);
}
.glass:hover, .glass-card:hover{
  border-color: rgba(255,255,255,.18);
  box-shadow:
    0 2px 0 rgba(255,255,255,.08) inset,
    0 24px 72px rgba(0,0,0,.55),
    0 0 0 1px rgba(213,0,0,.12);
}

/* ====================================================
   TYPOGRAPHY
==================================================== */
h1,h2,h3,h4,h5,h6,p,label,span,div,li,td,th,
.dataTables_info,.dataTables_paginate a,
.dataTables_length label,.dataTables_filter label{
  color:#FFFFFF;
}
.text-muted{ color:rgba(255,255,255,.72) !important; }

.blockTitle{
  color:#FFFFFF !important;
  font-weight:900;
  font-family:'Inter',sans-serif;
  letter-spacing:-.02em;
}

/* Gradient accent text for big titles */
.grad-title{
  background: linear-gradient(135deg, #FFFFFF 0%, rgba(255,255,255,.75) 100%);
  -webkit-background-clip:text; -webkit-text-fill-color:transparent;
  background-clip:text;
}

.smallHelp{
  color:rgba(255,255,255,.90) !important;
  font-size:12px;
  line-height:1.6;
}

.section-label{
  font-size:9.5px; font-weight:900; letter-spacing:.22em; text-transform:uppercase;
  color:#FFFFFF !important;
  margin:18px 0 10px 0; padding-bottom:9px;
  display:flex; align-items:center; gap:8px;
  position:relative;
}
.section-label::before{
  content:'';
  display:inline-block;
  width:3px; height:12px;
  background: linear-gradient(180deg, var(--accent), transparent);
  border-radius:999px;
  flex-shrink:0;
}
.section-label::after{
  content:''; flex:1;
  height:1px;
  background: linear-gradient(90deg, rgba(213,0,0,.40), transparent);
}

/* ====================================================
   KPI CARDS
==================================================== */
.kpiRow{
  display:grid;
  grid-template-columns:repeat(auto-fit, minmax(210px,1fr));
  gap:12px;
}
.kpi{
  padding:18px 16px;
  position:relative; overflow:hidden;
  transition: var(--trans);
}
/* Shimmer strip on top */
.kpi::before{
  content:'';
  position:absolute; top:0; left:0; right:0; height:1px;
  background: linear-gradient(90deg,
    transparent 0%, rgba(255,255,255,.55) 40%,
    rgba(213,0,0,.70) 60%, transparent 100%);
  opacity:0; transition:opacity .35s;
}
.kpi:hover::before{ opacity:1; }

/* Glowing corner accent */
.kpi::after{
  content:'';
  position:absolute; top:-40px; right:-40px;
  width:100px; height:100px;
  background: radial-gradient(circle, rgba(213,0,0,.18), transparent 65%);
  opacity:0; transition:opacity .35s;
  border-radius:50%;
}
.kpi:hover::after{ opacity:1; }

.kpi .t{
  font-size:9.5px; color:rgba(255,255,255,.78);
  text-transform:uppercase; letter-spacing:.16em; font-weight:700;
}
.kpi .v{
  font-size:26px; font-weight:900; color:#FFFFFF;
  margin-top:10px; line-height:1.1;
  text-shadow: 0 0 30px rgba(213,0,0,.25), 0 2px 10px rgba(0,0,0,.40);
}
.kpi .s{
  font-size:11.5px; color:rgba(255,255,255,.88);
  margin-top:8px; line-height:1.55;
}

/* ====================================================
   PILLS & BADGES
==================================================== */
.pill{
  display:inline-flex; align-items:center; gap:8px;
  background: rgba(255,255,255,.07);
  border:1px solid rgba(255,255,255,.14);
  padding:6px 13px; border-radius:999px;
  font-size:11.5px; color:#FFFFFF; font-weight:600;
  backdrop-filter:blur(8px);
  box-shadow: 0 2px 12px rgba(0,0,0,.20);
  transition: var(--trans);
}
.pill:hover{
  background:rgba(255,255,255,.10);
  border-color:rgba(255,255,255,.22);
}

/* Status pill glow when active */
.pill b{ font-weight:800; }

/* ====================================================
   BUTTONS
==================================================== */
.btn-accent{
  background: linear-gradient(135deg, var(--accent) 0%, var(--accent2) 100%) !important;
  border-color: transparent !important;
  color:#fff !important; font-weight:900 !important;
  border-radius:var(--radius-sm) !important;
  box-shadow:
    0 4px 20px rgba(213,0,0,.40),
    0 1px 0 rgba(255,255,255,.12) inset !important;
  transition: var(--trans) !important;
  letter-spacing:.02em;
  position:relative; overflow:hidden;
}
/* Shine sweep */
.btn-accent::after{
  content:'';
  position:absolute; top:0; left:-100%; width:60%; height:100%;
  background:linear-gradient(90deg, transparent, rgba(255,255,255,.18), transparent);
  transform:skewX(-20deg);
  transition:left .5s ease;
}
.btn-accent:hover::after{ left:150%; }
.btn-accent:hover{
  background: linear-gradient(135deg, #FF1A1A 0%, var(--accent) 100%) !important;
  box-shadow:
    0 8px 32px rgba(213,0,0,.60),
    0 1px 0 rgba(255,255,255,.18) inset !important;
  transform:translateY(-2px) !important;
}
.btn-accent:active{ transform:translateY(0) !important; }

.btn-outline-light{
  border-radius:var(--radius-sm) !important;
  border:1px solid rgba(255,255,255,.18) !important;
  background:rgba(255,255,255,.04) !important;
  color:#FFFFFF !important;
  backdrop-filter:blur(8px);
  transition: var(--trans) !important;
}
.btn-outline-light:hover{
  background:rgba(255,255,255,.11) !important;
  border-color:rgba(255,255,255,.28) !important;
  transform:translateY(-1px) !important;
  box-shadow:0 4px 16px rgba(0,0,0,.25) !important;
}

/* ====================================================
   FORM CONTROLS
==================================================== */
.form-control, .form-select{
  background:rgba(255,255,255,.06) !important;
  border:1px solid rgba(255,255,255,.12) !important;
  color:#FFFFFF !important;
  border-radius:var(--radius-sm) !important;
  transition: var(--trans) !important;
}
.form-control:focus, .form-select:focus{
  background:rgba(255,255,255,.09) !important;
  border-color:rgba(213,0,0,.70) !important;
  box-shadow:0 0 0 3px rgba(213,0,0,.18) !important;
  outline:none !important;
}
.form-control::placeholder{ color:rgba(255,255,255,.38) !important; }

.selectize-control .selectize-input{
  background:rgba(255,255,255,.06) !important;
  border:1px solid rgba(255,255,255,.12) !important;
  color:#FFFFFF !important;
  border-radius:var(--radius-sm) !important;
  box-shadow:none !important;
  transition: var(--trans);
}
.selectize-control.focus .selectize-input,
.selectize-control .selectize-input.focus{
  border-color:rgba(213,0,0,.65) !important;
  box-shadow:0 0 0 3px rgba(213,0,0,.15) !important;
  background:rgba(255,255,255,.09) !important;
}
.selectize-control .selectize-input input{ color:#FFFFFF !important; }
.selectize-dropdown{
  background:rgba(8,10,16,.97) !important;
  border:1px solid rgba(255,255,255,.12) !important;
  border-radius:var(--radius-sm) !important;
  backdrop-filter:blur(20px);
  box-shadow:0 20px 60px rgba(0,0,0,.65) !important;
}
.selectize-dropdown .option{ color:#FFFFFF !important; padding:8px 12px; transition:.12s; }
.selectize-dropdown .option:hover,
.selectize-dropdown .option.active{
  background:linear-gradient(90deg, rgba(213,0,0,.30), rgba(213,0,0,.10)) !important;
  padding-left:16px;
}
.selectize-dropdown .option.selected{
  background:rgba(213,0,0,.20) !important;
  font-weight:700;
}

/* Radio & checkbox */
.form-check-label{ color:#FFFFFF !important; font-size:13px; }
.form-check-input{ accent-color:var(--accent) !important; }
.form-check-input:focus{ box-shadow:0 0 0 3px rgba(213,0,0,.20) !important; }

/* shiny switch */
.bslib-input-switch .form-check-input:checked{ background-color:var(--accent) !important; border-color:var(--accent) !important; }

/* Range sliders */
input[type=range]{ accent-color:var(--accent) !important; }
.irs--shiny .irs-bar,
.irs--shiny .irs-handle{ background:var(--accent) !important; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single{
  background:var(--accent) !important;
  color:#fff !important;
}

/* ====================================================
   SIDEBAR
==================================================== */
.sbWrap{
  display:flex; flex-direction:column;
  height:calc(100vh - 24px); min-height:620px; overflow:hidden;
}
.sbHeader{
  position:sticky; top:0; z-index:20;
  padding:16px 16px 12px;
  border-bottom:1px solid rgba(255,255,255,.08);
  background:linear-gradient(180deg,
    rgba(7,10,15,.80) 0%,
    rgba(7,10,15,.0) 100%);
  backdrop-filter:blur(20px);
}
/* Electrend wordmark */
.electrend-word{
  font-size:20px; font-weight:900; letter-spacing:-.04em;
  background:linear-gradient(135deg, #FFFFFF 0%, rgba(255,255,255,.65) 100%);
  -webkit-background-clip:text; -webkit-text-fill-color:transparent;
  background-clip:text;
  display:inline-block;
}
.electrend-badge{
  display:inline-block;
  font-size:8px; font-weight:800; letter-spacing:.14em; text-transform:uppercase;
  background:linear-gradient(135deg,var(--accent),var(--accent2));
  color:#fff; padding:2px 7px; border-radius:999px;
  margin-left:6px; vertical-align:middle;
  box-shadow:0 2px 8px rgba(213,0,0,.40);
}
.sbScroll{
  padding:8px 14px 10px;
  overflow-y:auto; overflow-x:hidden;
  flex:1 1 auto;
  scrollbar-width:thin;
  scrollbar-color:rgba(213,0,0,.35) transparent;
}
.sbScroll::-webkit-scrollbar{ width:3px; }
.sbScroll::-webkit-scrollbar-thumb{
  background:linear-gradient(180deg,rgba(213,0,0,.50),rgba(213,0,0,.20));
  border-radius:999px;
}
.sbScroll::-webkit-scrollbar-track{ background:transparent; }

.sbFooter{
  position:sticky; bottom:0; z-index:20;
  padding:12px 14px 16px;
  border-top:1px solid rgba(255,255,255,.07);
  background:linear-gradient(0deg,
    rgba(7,10,15,.82) 0%,
    rgba(7,10,15,.0) 100%);
  backdrop-filter:blur(20px);
}

/* ====================================================
   NAV TABS
==================================================== */
.nav-tabs{
  border-bottom:1px solid rgba(255,255,255,.08) !important;
  gap:2px;
}
.nav-tabs .nav-link{
  color:rgba(255,255,255,.60) !important;
  font-weight:600; font-size:13px;
  border-radius:10px 10px 0 0 !important;
  border:none !important;
  padding:9px 16px;
  transition: var(--trans);
  position:relative;
}
.nav-tabs .nav-link::after{
  content:'';
  position:absolute; bottom:0; left:20%; right:20%; height:2px;
  background:var(--accent);
  border-radius:2px 2px 0 0;
  transform:scaleX(0); transition:transform .22s ease;
}
.nav-tabs .nav-link:hover{ color:rgba(255,255,255,.88) !important; }
.nav-tabs .nav-link:hover::after{ transform:scaleX(.5); }
.nav-tabs .nav-link.active{
  background:rgba(255,255,255,.07) !important;
  color:#FFFFFF !important; font-weight:800 !important;
  box-shadow:none !important;
}
.nav-tabs .nav-link.active::after{ transform:scaleX(1); }

/* ====================================================
   LEAFLET
==================================================== */
.leaflet-container{
  border-radius:var(--radius);
  border:1px solid rgba(255,255,255,.09);
  box-shadow:
    0 20px 60px rgba(0,0,0,.55),
    0 0 0 1px rgba(213,0,0,.08);
}
.leaflet-control-layers{
  background:rgba(5,7,12,.80) !important;
  border:1px solid rgba(255,255,255,.12) !important;
  border-radius:var(--radius-sm) !important;
  backdrop-filter:blur(16px);
  box-shadow:0 8px 32px rgba(0,0,0,.45) !important;
}
.leaflet-control-layers label{ color:#FFFFFF !important; font-size:12px; }
.leaflet-popup-content-wrapper{
  background:rgba(5,8,14,.94) !important;
  border:1px solid rgba(255,255,255,.13) !important;
  border-top-color:rgba(255,255,255,.22) !important;
  border-radius:14px !important;
  color:#FFFFFF !important;
  backdrop-filter:blur(16px);
  box-shadow:0 20px 60px rgba(0,0,0,.65) !important;
}
.leaflet-popup-tip{ background:rgba(5,8,14,.94) !important; }
.leaflet-popup-content{ color:#FFFFFF !important; line-height:1.7; }
.leaflet-legend{
  background:rgba(5,8,14,.84) !important;
  border:1px solid rgba(255,255,255,.11) !important;
  border-radius:var(--radius-sm) !important;
  color:#FFFFFF !important;
  backdrop-filter:blur(12px);
  box-shadow:0 8px 32px rgba(0,0,0,.50) !important;
  padding:10px 14px !important;
}
.leaflet-bar a{
  background:rgba(5,8,14,.85) !important;
  color:#FFFFFF !important;
  border-color:rgba(255,255,255,.12) !important;
  backdrop-filter:blur(8px);
}
.leaflet-bar a:hover{ background:rgba(213,0,0,.25) !important; }

/* ====================================================
   DATATABLES
==================================================== */
.dataTables_wrapper *,
.dataTables_info,
.dataTables_paginate a,
.dataTables_length label,
.dataTables_filter label{ color:#FFFFFF !important; }

.dataTables_wrapper .dataTables_filter input,
.dataTables_wrapper .dataTables_length select{
  background:rgba(255,255,255,.06) !important;
  border:1px solid rgba(255,255,255,.12) !important;
  color:#FFFFFF !important;
  border-radius:10px !important;
  padding:5px 10px;
}
.dataTables_wrapper .dataTables_filter input:focus{
  border-color:rgba(213,0,0,.60) !important;
  box-shadow:0 0 0 3px rgba(213,0,0,.15) !important;
  outline:none !important;
}

table.dataTable{
  border-collapse:separate !important;
  border-spacing:0 3px !important;
}
table.dataTable thead th{
  background:rgba(0,0,0,.40) !important;
  color:#FFFFFF !important; font-weight:800 !important;
  font-size:11px; letter-spacing:.08em; text-transform:uppercase;
  border-bottom:2px solid rgba(213,0,0,.50) !important;
  padding:10px 12px !important;
}
table.dataTable tbody td{
  color:#FFFFFF !important;
  padding:8px 12px !important;
  border:none !important;
  transition:background .15s;
}
table.dataTable tbody tr{
  background:rgba(255,255,255,.025) !important;
  border-radius:8px;
  transition:var(--trans);
}
table.dataTable tbody tr:hover > td{
  background:rgba(213,0,0,.14) !important;
}
table.dataTable tbody tr.odd  > td{ background:rgba(255,255,255,.03) !important; }
table.dataTable tbody tr.even > td{ background:rgba(0,0,0,.12) !important; }
table.dataTable tbody tr:hover{ transform:translateX(2px); }

.dt-button{
  border-radius:9px !important;
  border:1px solid rgba(255,255,255,.16) !important;
  background:rgba(255,255,255,.05) !important;
  color:#FFFFFF !important;
  font-size:12px !important;
  transition:var(--trans) !important;
}
.dt-button:hover{
  background:rgba(213,0,0,.22) !important;
  border-color:rgba(213,0,0,.40) !important;
  transform:translateY(-1px) !important;
}

/* ====================================================
   SHINY NOTIFICATIONS
==================================================== */
.shiny-notification{
  background:rgba(5,8,14,.96) !important;
  border:1px solid rgba(255,255,255,.12) !important;
  border-left:3px solid var(--accent) !important;
  color:#FFFFFF !important;
  border-radius:var(--radius-sm) !important;
  backdrop-filter:blur(16px);
  box-shadow:0 16px 48px rgba(0,0,0,.60) !important;
  animation: notif-in .30s cubic-bezier(.34,1.56,.64,1);
}
@keyframes notif-in{
  from{ opacity:0; transform:translateY(12px) scale(.96); }
  to  { opacity:1; transform:translateY(0)   scale(1); }
}
.shiny-notification-close{ color:rgba(255,255,255,.55) !important; }

/* ====================================================
   MISC DETAILS
==================================================== */
hr{ border-color:rgba(255,255,255,.09) !important; margin:12px 0; }

.logo-label img{ vertical-align:middle; margin-right:4px; }
.addr-cell{ font-size:11px; color:#FFFFFF; max-width:260px; white-space:normal; }

/* Plotly */
.js-plotly-plot .plotly .gtitle,
.js-plotly-plot .plotly text{ fill:#FFFFFF !important; }
.js-plotly-plot .plotly .modebar{ background:rgba(0,0,0,.0) !important; }
.js-plotly-plot .plotly .modebar-btn path{ fill:rgba(255,255,255,.50) !important; }

/* Scrollbar global */
::-webkit-scrollbar{ width:4px; height:4px; }
::-webkit-scrollbar-thumb{
  background:rgba(213,0,0,.30);
  border-radius:999px;
}
::-webkit-scrollbar-thumb:hover{ background:rgba(213,0,0,.55); }
::-webkit-scrollbar-track{ background:transparent; }

/* Loading spinner override */
.shiny-spinner-output-container .load-container .loader{
  border-top-color:var(--accent) !important;
}

/* ====================================================
   CARD ENTRANCE ANIMATION
==================================================== */
.glass, .glass-card{
  animation: card-in .42s cubic-bezier(.22,1,.36,1) both;
}
.kpi{ animation: card-in .42s cubic-bezier(.22,1,.36,1) both; }

@keyframes card-in{
  from{ opacity:0; transform:translateY(14px); }
  to  { opacity:1; transform:translateY(0); }
}

.kpiRow > *:nth-child(1){ animation-delay:.04s; }
.kpiRow > *:nth-child(2){ animation-delay:.10s; }
.kpiRow > *:nth-child(3){ animation-delay:.16s; }
.kpiRow > *:nth-child(4){ animation-delay:.22s; }

/* ====================================================
   SKELETON LOADER
==================================================== */
@keyframes shimmer {
  0%   { background-position: -600px 0; }
  100% { background-position:  600px 0; }
}
.skeleton{
  display:inline-block;
  border-radius:6px;
  background: linear-gradient(90deg,
    rgba(255,255,255,.06) 25%,
    rgba(255,255,255,.14) 50%,
    rgba(255,255,255,.06) 75%);
  background-size:600px 100%;
  animation: shimmer 1.6s infinite linear;
}
.skeleton-title{ height:14px; width:55%; margin-bottom:10px; }
.skeleton-val  { height:28px; width:70%; margin-bottom:10px; }
.skeleton-sub  { height:10px; width:85%; margin-bottom:5px; }
.skeleton-sub2 { height:10px; width:60%; }

/* ====================================================
   GENERAR BUTTON — PULSE WHEN PENDING
==================================================== */
@keyframes pulse-glow{
  0%, 100%{ box-shadow: 0 4px 20px rgba(213,0,0,.40), 0 0 0 0   rgba(213,0,0,.35); }
  50%     { box-shadow: 0 4px 20px rgba(213,0,0,.70), 0 0 0 10px rgba(213,0,0,.00); }
}
.btn-accent.pending{
  animation: pulse-glow 2.2s ease-in-out infinite;
}

/* ====================================================
   SEGMENTED CONTROL (radioButtons override)
==================================================== */
.seg-control .shiny-input-container{ margin-bottom:0; }
.seg-control .control-label{ display:none; }
.seg-control .shiny-options-group{
  display:flex; gap:0; border-radius:12px; overflow:hidden;
  border:1px solid rgba(255,255,255,.14);
  background:rgba(255,255,255,.04);
  padding:3px;
}
.seg-control .form-check{
  flex:1; margin:0; padding:0;
}
.seg-control .form-check-input{ display:none; }
.seg-control .form-check-label{
  display:flex; align-items:center; justify-content:center;
  padding:6px 10px; border-radius:9px;
  font-size:11.5px; font-weight:600; cursor:pointer;
  color:rgba(255,255,255,.60) !important;
  transition: var(--trans);
  white-space:nowrap; user-select:none;
}
.seg-control .form-check-label:hover{
  color:rgba(255,255,255,.90) !important;
  background:rgba(255,255,255,.06);
}
.seg-control .form-check-input:checked + .form-check-label{
  background: linear-gradient(135deg, var(--accent), var(--accent2)) !important;
  color:#FFFFFF !important;
  font-weight:800 !important;
  box-shadow:0 2px 10px rgba(213,0,0,.40);
}

/* ====================================================
   CARD SECTION HEADER  (.card-hd)
==================================================== */
.card-hd{
  display:flex; align-items:center; gap:10px;
  padding-bottom:10px; margin-bottom:8px;
  border-bottom:1px solid rgba(255,255,255,.07);
}
.card-hd-icon{
  width:32px; height:32px; border-radius:9px; flex-shrink:0;
  display:flex; align-items:center; justify-content:center;
  font-size:16px;
  background:linear-gradient(135deg,rgba(213,0,0,.28),rgba(139,0,0,.18));
  border:1px solid rgba(213,0,0,.30);
  box-shadow:0 2px 10px rgba(213,0,0,.20);
}
.card-hd-text .t{ font-size:13px; font-weight:800; color:#FFFFFF; line-height:1.2; }
.card-hd-text .s{ font-size:10.5px; color:rgba(255,255,255,.72); margin-top:1px; }

/* Right slot in card-hd */
.card-hd-right{ margin-left:auto; display:flex; align-items:center; gap:8px; }

/* ====================================================
   STATUS BADGE
==================================================== */
.status-badge{
  display:inline-flex; align-items:center; gap:7px;
  padding:5px 11px; border-radius:999px;
  font-size:11px; font-weight:700;
  border:1px solid;
  transition: var(--trans);
}
.status-badge.idle{
  background:rgba(213,0,0,.12);
  border-color:rgba(213,0,0,.30);
  color:rgba(255,180,180,.95);
}
.status-badge.idle::before{
  content:''; width:7px; height:7px; border-radius:50%;
  background:rgba(213,0,0,.80); flex-shrink:0;
  animation: blink 1.8s ease-in-out infinite;
}
@keyframes blink{
  0%,100%{ opacity:1; } 50%{ opacity:.25; }
}
.status-badge.ok{
  background:rgba(46,173,74,.12);
  border-color:rgba(46,173,74,.30);
  color:rgba(160,240,175,.95);
}
.status-badge.ok::before{
  content:''; width:7px; height:7px; border-radius:50%;
  background:rgba(46,173,74,.90); flex-shrink:0;
}

/* ====================================================
   SUBTITLE BADGE (map / chart)
==================================================== */
.subtitle-badge{
  display:inline-flex; align-items:center; gap:5px;
  background:rgba(255,255,255,.06);
  border:1px solid rgba(255,255,255,.10);
  border-radius:999px; padding:3px 10px;
  font-size:11px; color:rgba(255,255,255,.82);
  font-weight:500; white-space:nowrap; overflow:hidden;
  text-overflow:ellipsis; max-width:100%;
}

/* ====================================================
   GLASS-CARD WITH PADDING FIX
==================================================== */
.glass-card.p-3{ padding:14px !important; }

/* ====================================================
   MINI PROGRESS BAR (inside KPI)
==================================================== */
.kpi-bar-track{
  width:100%; height:3px; border-radius:999px;
  background:rgba(255,255,255,.10); margin-top:8px; overflow:hidden;
}
.kpi-bar-fill{
  height:100%; border-radius:999px;
  background:linear-gradient(90deg, var(--accent), rgba(213,0,0,.50));
  transition: width .6s cubic-bezier(.4,0,.2,1);
}

/* ====================================================
   NAV TAB ICONS
==================================================== */
.nav-icon{ margin-right:5px; opacity:.85; }
")


# ==========================================================
# UI
# ==========================================================

ui_base <- bslib::page_fillable(
  title = APP_TITLE,
  theme = theme,
  tags$head(
    tags$style(HTML(css)),
    tags$script(HTML("
      // Mark GENERAR as pending until applied
      $(document).on('shiny:inputchanged', function(e) {
        if (['election','dl_sel','secciones','map_variable','map_view',
             'winner_vote_type','choro_vote_type','choro_party','choro_metric',
             'choro_scale','choro_opacity','electorado_var','electorado_scale',
             'electorado_opacity'].includes(e.name)) {
          $('#generar').addClass('pending');
        }
        // Mark Optimizar pending on PAUTA param changes
        if (['buf_mode','buf_party','buf_gen_vars','buf_eje','buf_inegi_var',
             'buf_cover_pct','buf_radius_m','buf_max_points'].includes(e.name)) {
          $('#buf_generate').addClass('pending');
        }
      });
      $(document).on('shiny:value', function(e) {
        if (e.name === 'ui_status_run') {
          $('#generar').removeClass('pending');
        }
        if (e.name === 'buf_status') {
          $('#buf_generate').removeClass('pending');
        }
      });
      // Smooth tab transitions
      $(document).on('shown.bs.tab', function() {
        $('.tab-pane.active .glass, .tab-pane.active .glass-card').css('animation','none');
        setTimeout(function(){
          $('.tab-pane.active .glass, .tab-pane.active .glass-card').css('animation','');
        }, 50);
      });
    "))
  ),

  bslib::layout_sidebar(
    fillable = TRUE,

    sidebar = bslib::sidebar(
      width = 360,
      div(class = "sbWrap glass",

          div(class = "sbHeader",
              div(style="display:flex;align-items:center;gap:6px;margin-bottom:4px;",
                  span(class="electrend-word","electrend"),
                  span(class="electrend-badge","MX")
              ),
              div(class = "smallHelp", style="opacity:.80;",
                  "\u26a1\ufe0f Atizap\u00e1n (MUN 13) \u00b7 EDOMEX"),
              div(style = "height:8px;"),
              uiOutput("ui_status_run")
          ),

          div(class = "sbScroll",

              div(class = "section-label", "Datos"),
              selectInput("election", "Elecci\u00f3n", choices=ELECTION_CHOICES, selected=DEFAULT_ELECTION),

              div(class = "section-label", "Filtros"),
              div(class = "smallHelp", "Distrito local (vac\u00edo = todos)"),
              selectizeInput("dl_sel", NULL, choices=NULL, selected=NULL, multiple=TRUE,
                             options=list(plugins=list("remove_button"), placeholder="Ej. DL 16\u2026")),

              div(class = "smallHelp", style="margin-top:10px;", "Secci\u00f3n (vac\u00edo = todas)"),
              selectizeInput("secciones", NULL, choices=NULL, selected=NULL, multiple=TRUE,
                             options=list(plugins=list("remove_button"), placeholder="Escribe una secci\u00f3n\u2026")),

              div(class = "section-label", "Visualizaci\u00f3n"),
              bslib::input_switch("map_variable", "Choropleth partido", value=FALSE),

              conditionalPanel(
                condition = "!input.map_variable",
                selectInput("map_view", "Vista",
                            choices=list("Ganador"="winner","Participaci\u00f3n (%)"="part",
                                         "Total votos"="tot","Lista nominal"="ln","Electorado"="electorado"),
                            selected="winner"),
                conditionalPanel(
                  condition = "input.map_view == 'winner'",
                  radioButtons("winner_vote_type","Tipo de voto",
                               choices=list("DISTRIBUIDO"="DISTRIBUIDO","PURO"="PURO","CANDIDATURAS"="CAND"),
                               selected="DISTRIBUIDO", inline=TRUE)
                ),
                conditionalPanel(
                  condition = "input.map_view == 'electorado'",
                  selectInput("electorado_var","Variable del electorado",
                              choices=ELECTORADO_CHOICES,
                              selected=ELECTORADO_CHOICES[["Lista nominal"]] %||% ELECTORADO_CHOICES[[1L]]),
                  radioButtons("electorado_scale","Escala",
                               choices=list("Lineal"="linear","Cuantiles"="quantile"),
                               selected="linear", inline=TRUE),
                  sliderInput("electorado_opacity","Opacidad",min=0.20,max=0.90,value=0.70,step=0.05)
                )
              ),

              conditionalPanel(
                condition = "input.map_variable",
                radioButtons("choro_vote_type","Votos (choropleth)",
                             choices=list("DISTRIBUIDO"="DISTRIBUIDO","PURO"="PURO"),
                             selected="DISTRIBUIDO", inline=TRUE),
                uiOutput("ui_choro_party"),
                radioButtons("choro_metric","M\u00e9trica",
                             choices=list("Votos"="votes","% (sobre v\u00e1lidos)"="pct"),
                             selected="pct", inline=TRUE),
                radioButtons("choro_scale","Escala",
                             choices=list("Lineal"="linear","Cuantiles"="quantile"),
                             selected="linear", inline=TRUE),
                sliderInput("choro_opacity","Opacidad",min=0.20,max=0.90,value=0.65,step=0.05)
              )
          ),

          div(class = "sbFooter",
              actionButton("generar","\u26a1 GENERAR", class="btn btn-accent w-100"),
              div(style="height:10px;"),
              div(style="display:grid;grid-template-columns:1fr 1fr;gap:10px;",
                  downloadButton("download_csv","\u2b07 CSV", class="btn btn-outline-light w-100"),
                  actionButton("reset","\u21ba Limpiar", class="btn btn-outline-light w-100")
              ),
              div(class="smallHelp", style="margin-top:10px;",
                  HTML("Tip: <b>GENERAR</b> fija la selecci\u00f3n para todas las pesta\u00f1as."))
          )
      )
    ),

    bslib::navset_tab(

      # ===================== EXPLORAR =====================
      bslib::nav_panel(
        HTML("<span class='nav-icon'>\U0001F5FA\uFE0F</span>Explorar"),
        div(style="padding:12px 12px 0 12px;",
            div(class="kpiRow",
                div(class="glass kpi", uiOutput("kpi1")),
                div(class="glass kpi", uiOutput("kpi2")),
                div(class="glass kpi", uiOutput("kpi3"))
            )
        ),
        div(style="padding:12px;",
            bslib::layout_columns(
              col_widths = c(7,5),
              div(class="glass", style="padding:12px;",
                  div(class="card-hd",
                      div(class="card-hd-icon","\U0001F5FA\uFE0F"),
                      div(class="card-hd-text",
                          div(class="t","Mapa"),
                          div(class="s", uiOutput("map_subtitle"))
                      )
                  ),
                  leafletOutput("map", height=520)
              ),
              div(class="glass", style="padding:12px;",
                  div(class="card-hd",
                      div(class="card-hd-icon","\U0001F4CA"),
                      div(class="card-hd-text",
                          div(class="t","Resultados"),
                          div(class="s", uiOutput("bar_subtitle"))
                      ),
                      div(class="card-hd-right",
                          bslib::input_switch("bar_is_cand","Candidaturas", value=FALSE)
                      )
                  ),
                  plotlyOutput("bar", height=520)
              )
            )
        ),
        div(style="padding:0 12px 12px 12px;",
            div(class="glass", style="padding:12px;",
                div(class="card-hd",
                    div(class="card-hd-icon","\U0001F4CB"),
                    div(class="card-hd-text",
                        div(class="t","Tabla por secci\u00f3n"),
                        div(class="s","M\u00e9tricas + top votos seg\u00fan vista.")
                    ),
                    div(class="card-hd-right",
                        div(class="seg-control",
                            radioButtons("table_view",NULL,
                                         choices=list("DISTRIBUIDO"="DISTRIBUIDO","PURO"="PURO","CAND"="CAND"),
                                         selected="DISTRIBUIDO", inline=TRUE)
                        )
                    )
                ),
                DTOutput("tbl")
            )
        )
      ),

      # ===================== TIEMPO =====================
      bslib::nav_panel(
        HTML("<span class='nav-icon'>\U0001F4C8</span>Tiempo"),
        div(style="padding:12px;",
            bslib::layout_columns(
              col_widths = c(4,8),
              div(class="glass", style="padding:14px;",
                  div(class="card-hd",
                      div(class="card-hd-icon","\u2699\uFE0F"),
                      div(class="card-hd-text",
                          div(class="t","Configuraci\u00f3n"),
                          div(class="s","Series y mapa usan la selecci\u00f3n de GENERAR")
                      )
                  ),
                  radioButtons("ts_office","Serie",
                               choices=list("Solo AYU"="AYU","Solo DL"="DL","AYU + DL"="BOTH"),
                               selected="BOTH", inline=TRUE),
                  radioButtons("ts_vote_type","Votos para partidos",
                               choices=list("DISTRIBUIDO"="DISTRIBUIDO","PURO"="PURO"),
                               selected="DISTRIBUIDO", inline=TRUE),
                  radioButtons("ts_party_metric","M\u00e9trica partidos",
                               choices=list("Votos"="votes","% (sobre v\u00e1lidos)"="pct"),
                               selected="pct", inline=TRUE),
                  selectizeInput("ts_parties","Partidos a mostrar (vac\u00edo = Top N)",
                                 choices=NULL, multiple=TRUE,
                                 options=list(plugins=list("remove_button"),
                                              placeholder="Ej. PRI, PAN, MORENA\u2026")),
                  sliderInput("ts_top_n","Top N (si no eliges partidos)",min=3,max=15,value=8,step=1),
                  checkboxInput("ts_include_other","Incluir OTROS",value=FALSE),
                  hr(),
                  h6(class="blockTitle","Mapa en el tiempo"),
                  div(class="smallHelp", HTML(paste0("Compara <b>Base</b> vs <b>Ref fija</b>: ",key_label(REF_KEY),"."))),
                  selectInput("ts_map_election","Elecci\u00f3n (base)",
                              choices=ELECTION_CHOICES, selected=head(elex$key,1L) %||% DEFAULT_ELECTION),
                  radioButtons("ts_map_view","Vista (mapa)",
                               choices=list("Participaci\u00f3n (pp)"="participacion","Lista nominal"="lista",
                                            "Total votos"="mas_votantes","Choropleth (Partido)"="choro_party"),
                               selected="choro_party", inline=TRUE),
                  radioButtons("ts_delta_scale","Escala (\u0394)",
                               choices=list("Lineal"="linear","Cuantiles"="quantile"),
                               selected="linear", inline=TRUE),
                  sliderInput("ts_map_opacity","Opacidad",min=0.20,max=0.90,value=0.70,step=0.05),
                  conditionalPanel(
                    condition = "input.ts_map_view == 'choro_party'",
                    uiOutput("ui_ts_choro_party"),
                    radioButtons("ts_choro_metric","M\u00e9trica (partido)",
                                 choices=list("Votos (\u0394)"="votes","% v\u00e1lidos (pp)"="pct"),
                                 selected="votes", inline=TRUE)
                  ),
                  div(style="height:8px;"),
                  downloadButton("download_ts_parties_csv","Descargar serie partidos (CSV)", class="btn btn-outline-light btn-sm"),
                  div(style="height:6px;"),
                  downloadButton("download_ts_metrics_csv","Descargar serie m\u00e9tricas (CSV)", class="btn btn-outline-light btn-sm")
              ),
              div(
                div(class="glass", style="padding:14px;",
                    div(class="card-hd",
                        div(class="card-hd-icon","\U0001F4C8"),
                        div(class="card-hd-text",
                            div(class="t","Partidos en el tiempo"),
                            div(class="s","Tendencia por elecci\u00f3n en la selecci\u00f3n aplicada.")
                        )
                    ),
                    withSpinner(plotlyOutput("ts_party_plot", height="360px"), type=5, size=1.0)
                ),
                div(style="height:12px;"),
                div(class="glass-card p-3",
                    div(class="card-hd",
                        div(class="card-hd-icon","\U0001F5FA\uFE0F"),
                        div(class="card-hd-text",
                            div(class="t","Mapa en el tiempo"),
                            div(class="s",paste0("\u0394 = Ref fija \u2212 Base \u00b7 Ref: ",key_label(REF_KEY)))
                        )
                    ),
                    leafletOutput("map_time", height=420)
                )
              )
            ),
            div(style="height:12px;"),
            bslib::layout_columns(
              col_widths = c(6,6),
              div(class="glass", style="padding:14px;",
                  div(class="card-hd",
                      div(class="card-hd-icon","\U0001F4CA"),
                      div(class="card-hd-text",
                          div(class="t","Votantes / Lista nominal"),
                          div(class="s","Evoluci\u00f3n de m\u00e9tricas por elecci\u00f3n")
                      ),
                      div(class="card-hd-right",
                          checkboxGroupInput("ts_metrics",NULL,
                                             choices=list("Votos"="total","LN"="lista",
                                                          "Casillas"="casillas","V\u00e1lidos"="validos","Nulos"="nulos"),
                                             selected=c("total","lista"), inline=TRUE)
                      )
                  ),
                  withSpinner(plotlyOutput("ts_metrics_plot", height="300px"), type=5, size=1.0)
              ),
              div(class="glass", style="padding:14px;",
                  div(class="card-hd",
                      div(class="card-hd-icon","\U0001F4AF"),
                      div(class="card-hd-text",
                          div(class="t","Participaci\u00f3n en el tiempo"),
                          div(class="s","% sobre lista nominal por elecci\u00f3n")
                      )
                  ),
                  withSpinner(plotlyOutput("ts_particip_plot", height="300px"), type=5, size=1.0)
              )
            )
        )
      ),

      # ===================== PAUTA =====================
      bslib::nav_panel(
        HTML("<span class='nav-icon'>\U0001F3AF</span>PAUTA"),
        div(style="padding:12px;",
            bslib::layout_columns(
              col_widths = c(4,8),

              div(class="glass", style="padding:14px;",
                  div(class="card-hd",
                      div(class="card-hd-icon","\U0001F3AF"),
                      div(class="card-hd-text",
                          div(class="t","PAUTA"),
                          div(class="s","Optimizaci\u00f3n de puntos de activaci\u00f3n")
                      )
                  ),
                  uiOutput("buf_hereda_info"),
                  hr(),
                  div(class="smallHelp",style="font-weight:700;margin-bottom:6px;","Modo de optimizaci\u00f3n"),
                  div(class="seg-control",
                      radioButtons("buf_mode", NULL,
                                   choices=list("\U0001F3C6 Electoral"="electoral",
                                                "\U0001F4CA Poblaci\u00f3n"="inegi",
                                                "\U0001F4F1 Generacional"="gen"),
                                   selected="electoral", inline=TRUE)
                  ),
                  div(style="height:10px;"),

                  # ---- Electoral ----
                  conditionalPanel(condition="input.buf_mode == 'electoral'", uiOutput("ui_buf_party")),

                  # ---- Generacional ----
                  conditionalPanel(
                    condition="input.buf_mode == 'gen'",
                    uiOutput("ui_buf_gen")
                  ),

                  # ---- INEGI ----
                  conditionalPanel(
                    condition="input.buf_mode == 'inegi'",
                    tagList(
                      if (length(EJES_DISPONIBLES)>0L) selectInput("buf_eje","Eje",choices=EJES_DISPONIBLES,selected=EJES_DISPONIBLES[1L]),
                      uiOutput("ui_buf_inegi")
                    )
                  ),
                  sliderInput("buf_cover_pct","Cobertura objetivo (%)",min=50,max=100,value=80,step=5),
                  sliderInput("buf_radius_m","Radio por punto (metros)",min=200,max=3000,value=800,step=50),
                  sliderInput("buf_max_points","M\u00e1ximo de puntos",min=1,max=10,value=3,step=1),
                  hr(),
                  actionButton("buf_generate","\U0001F3AF Optimizar", class="btn btn-accent w-100"),
                  div(style="height:8px;"),
                  downloadButton("download_buf_csv","\u2b07 CSV pauta", class="btn btn-outline-light w-100"),
                  div(style="height:10px;"),
                  uiOutput("buf_status")
              ),

              div(
                div(class="kpiRow",
                    div(class="glass kpi", uiOutput("buf_kpi1")),
                    div(class="glass kpi", uiOutput("buf_kpi2")),
                    div(class="glass kpi", uiOutput("buf_kpi3")),
                    div(class="glass kpi", uiOutput("buf_kpi4"))
                ),
                div(style="height:12px;"),
                div(class="glass-card p-3",
                    div(class="card-hd",
                        div(class="card-hd-icon","\U0001F5FA\uFE0F"),
                        div(class="card-hd-text",
                            div(class="t","Mapa PAUTA"),
                            div(class="s","Secciones cubiertas por los buffers")
                        )
                    ),
                    leafletOutput("map_buf", height=430)
                ),
                div(style="height:12px;"),
                bslib::layout_columns(
                  col_widths = c(6,6),
                  div(class="glass-card p-3",
                      div(class="card-hd",
                          div(class="card-hd-icon","\U0001F7E2"),
                          div(class="card-hd-text",
                              div(class="t","Dentro vs fuera"),
                              div(class="s","Cobertura del radio")
                          )
                      ),
                      withSpinner(plotlyOutput("buf_compare_bar", height="260px"), type=5, size=1.0)
                  ),
                  div(class="glass-card p-3",
                      div(class="card-hd",
                          div(class="card-hd-icon","\U0001F3C5"),
                          div(class="card-hd-text",
                              div(class="t","Top partidos dentro"),
                              div(class="s","Votos en secciones cubiertas")
                          )
                      ),
                      withSpinner(plotlyOutput("buf_party_bar", height="260px"), type=5, size=1.0)
                  )
                ),
                div(style="height:12px;"),
                div(class="glass-card p-3",
                    div(class="card-hd",
                        div(class="card-hd-icon","\U0001F4CD"),
                        div(class="card-hd-text",
                            div(class="t","Puntos de activaci\u00f3n"),
                            div(class="s","Semillas elegidas, cobertura acumulada y direcci\u00f3n")
                        )
                    ),
                    DTOutput("buf_tbl")
                )
              )
            )
        )
      ),

      # ===================== LEEME =====================
      bslib::nav_panel(
        HTML("<span class='nav-icon'>\U0001F4CB</span>LEEME"),
        div(style="padding:20px; max-width:1100px; margin:0 auto;",

          # ---- Hero ----
          div(class="glass", style="padding:28px 32px; margin-bottom:16px; text-align:center;",
              div(style="font-size:32px; margin-bottom:8px;", "\U0001F5FA\uFE0F"),
              div(style="font-size:22px; font-weight:900; letter-spacing:-.03em; color:#FFFFFF;",
                  "electrend \u00b7 Gu\u00eda de uso"),
              div(style="font-size:13px; color:rgba(255,255,255,.65); margin-top:6px;",
                  "Plataforma de inteligencia electoral y demogr\u00e1fica \u00b7 Atizap\u00e1n, EDOMEX")
          ),

          # ---- Paso a paso ----
          bslib::layout_columns(col_widths=c(4,4,4),
            div(class="glass", style="padding:22px;",
                div(style="font-size:28px; margin-bottom:10px;", "\u0031\uFE0F\u20E3"),
                div(style="font-size:14px; font-weight:800; color:#FFFFFF; margin-bottom:8px;",
                    "Configura tu an\u00e1lisis"),
                div(style="font-size:12.5px; color:rgba(255,255,255,.80); line-height:1.8;",
                    HTML("En el panel izquierdo elige la <b>elecci\u00f3n</b> que quieres analizar.<br><br>
                    Filtra por <b>Distrito Local</b> si te interesa una zona espec\u00edfica, o deja en blanco para ver todo el municipio.<br><br>
                    Tambi\u00e9n puedes filtrar por <b>n\u00famero de secci\u00f3n</b>."))
            ),
            div(class="glass", style="padding:22px;",
                div(style="font-size:28px; margin-bottom:10px;", "\u0032\uFE0F\u20E3"),
                div(style="font-size:14px; font-weight:800; color:#FFFFFF; margin-bottom:8px;",
                    "Presiona GENERAR"),
                div(style="font-size:12.5px; color:rgba(255,255,255,.80); line-height:1.8;",
                    HTML("El bot\u00f3n <b>\u26a1 GENERAR</b> fija tu selecci\u00f3n y actualiza <i>todas</i> las pesta\u00f1as al mismo tiempo.<br><br>
                    El bot\u00f3n pulsa en rojo cuando hay cambios pendientes \u2014 eso es la se\u00f1al de que debes volver a generarlo.<br><br>
                    Sin GENERAR, las vistas no cambian."))
            ),
            div(class="glass", style="padding:22px;",
                div(style="font-size:28px; margin-bottom:10px;", "\u0033\uFE0F\u20E3"),
                div(style="font-size:14px; font-weight:800; color:#FFFFFF; margin-bottom:8px;",
                    "Explora los resultados"),
                div(style="font-size:12.5px; color:rgba(255,255,255,.80); line-height:1.8;",
                    HTML("Navega entre las 3 pesta\u00f1as de an\u00e1lisis: <b>Explorar</b>, <b>Tiempo</b> y <b>PAUTA</b>.<br><br>
                    Puedes descargar los datos con los botones <b>\u2b07 CSV</b> que aparecen en cada secci\u00f3n."))
            )
          ),

          div(style="height:14px;"),

          # ---- Tabs explicadas ----
          bslib::layout_columns(col_widths=c(4,4,4),
            div(class="glass", style="padding:22px; border-top:3px solid rgba(213,0,0,.60);",
                div(style="display:flex; align-items:center; gap:10px; margin-bottom:12px;",
                    div(style="font-size:22px;", "\U0001F5FA\uFE0F"),
                    div(style="font-size:14px; font-weight:900; color:#FFFFFF;", "Explorar")
                ),
                div(style="font-size:12.5px; color:rgba(255,255,255,.80); line-height:1.8;",
                    HTML("<b>Mapa interactivo</b> con el ganador por secci\u00f3n, participaci\u00f3n, lista nominal o choropleth por partido.<br><br>
                    <b>Barras</b> con los votos totales por partido o por candidatura \u2014 usa el switch de la esquina.<br><br>
                    <b>Tabla</b> con todas las m\u00e9tricas por secci\u00f3n, filtrable y exportable."))
            ),
            div(class="glass", style="padding:22px; border-top:3px solid rgba(0,181,226,.60);",
                div(style="display:flex; align-items:center; gap:10px; margin-bottom:12px;",
                    div(style="font-size:22px;", "\U0001F4C8"),
                    div(style="font-size:14px; font-weight:900; color:#FFFFFF;", "Tiempo")
                ),
                div(style="font-size:12.5px; color:rgba(255,255,255,.80); line-height:1.8;",
                    HTML("<b>Series hist\u00f3ricas</b> de votos por partido desde 2018 hasta la elecci\u00f3n m\u00e1s reciente.<br><br>
                    <b>Mapa delta</b>: compara cualquier elecci\u00f3n contra la referencia fija para ver qu\u00e9 secci\u00f3n creci\u00f3 o baj\u00f3.<br><br>
                    <b>Participaci\u00f3n</b> y conteos de votos, lista nominal y casillas a lo largo del tiempo."))
            ),
            div(class="glass", style="padding:22px; border-top:3px solid rgba(46,173,74,.60);",
                div(style="display:flex; align-items:center; gap:10px; margin-bottom:12px;",
                    div(style="font-size:22px;", "\U0001F3AF"),
                    div(style="font-size:14px; font-weight:900; color:#FFFFFF;", "PAUTA")
                ),
                div(style="font-size:12.5px; color:rgba(255,255,255,.80); line-height:1.8;",
                    HTML("Herramienta de <b>optimizaci\u00f3n de puntos de activaci\u00f3n</b>.<br><br>
                    Elige el modo: <b>\U0001F3C6 Electoral</b> (maximiza votos de un partido), <b>\U0001F4CA Poblaci\u00f3n</b> (indicadores INEGI) o <b>\U0001F4F1 Generacional</b> (electores por generaci\u00f3n).<br><br>
                    Ajusta radio y cobertura, presiona <b>Optimizar</b> y obt\u00e9n los mejores puntos con direcci\u00f3n y link a Google Maps."))
            )
          ),

          div(style="height:14px;"),

          # ---- Tips rápidos ----
          div(class="glass", style="padding:20px 28px;",
              div(style="font-size:12px; font-weight:900; letter-spacing:.14em; text-transform:uppercase; color:rgba(213,0,0,.90); margin-bottom:14px;",
                  "\u26a1 Tips r\u00e1pidos"),
              div(style="display:grid; grid-template-columns:repeat(auto-fit,minmax(240px,1fr)); gap:10px 24px;",
                  lapply(list(
                    list("\U0001F50D", "Click en cualquier secci\u00f3n del mapa para ver su popup con todos los datos."),
                    list("\U0001F4CA", "En Tiempo, usa el selector de partidos para enfocarte en los que te interesan."),
                    list("\U0001F4E5", "Todos los gr\u00e1ficos y tablas tienen bot\u00f3n de descarga en CSV o Excel."),
                    list("\U0001F504", "Limpiar resetea todos los filtros a sus valores por defecto."),
                    list("\U0001F4CD", "En PAUTA, el link \u201cMaps\u201d te lleva directo a Google Maps al punto elegido."),
                    list("\U0001F465", "Modo Generacional suma Gen Z + Millennials (u otras) para optimizar activaci\u00f3n juvenil.")
                  ), function(tip) {
                    div(style="display:flex; align-items:flex-start; gap:10px; padding:8px 0;",
                        div(style="font-size:18px; flex-shrink:0; margin-top:1px;", tip[[1]]),
                        div(style="font-size:12px; color:rgba(255,255,255,.82); line-height:1.6;",
                            tip[[2]])
                    )
                  })
              )
          )
        )
      )
    )
  )
)

ui <- secure_app(
  ui_base,
  enable_admin = FALSE,
  theme = theme,
  tags_top = tags$div(
    style = "text-align:center; padding:20px 0 8px 0;",
    if (!is.na(logo_login)) {
      tags$img(src=logo_login, height="64px",
               style="display:block;margin:0 auto 10px auto;filter:drop-shadow(0 2px 14px rgba(213,0,0,.35));")
    } else {
      tags$div(
        style=paste0("font-size:30px;font-weight:900;letter-spacing:-0.03em;color:",ACCENT,";margin-bottom:6px;"),
        "electrend"
      )
    },
    tags$div(style="font-size:18px;font-weight:900;color:#FFFFFF;", APP_TITLE),
    tags$div(style="font-size:11px;letter-spacing:.08em;text-transform:uppercase;color:rgba(255,255,255,.75);margin-top:4px;", APP_SUB),
    tags$div(style=paste0("margin:12px auto 0 auto;width:42px;height:3px;border-radius:999px;background:",ACCENT,";"))
  ),
  background = "linear-gradient(135deg, #0B0F17 0%, #180505 45%, #0B0F17 100%)",
  language = "es"
)

# ==========================================================
# SERVER
# ==========================================================

server <- function(input, output, session) {

  auth <- secure_server(check_credentials = check_credentials(credentials))

  buf_applied <- reactiveVal(NULL)

  # Selectores DL
  observe({
    if (is.na(SECC_DL_COL)) { updateSelectizeInput(session,"dl_sel",choices=list(),selected=NULL,server=TRUE); return() }
    dls <- sort(unique(na.omit(sf_all[[SECC_DL_COL]])))
    ch  <- as.list(setNames(as.character(dls), paste0("DL ",dls)))
    updateSelectizeInput(session,"dl_sel",choices=ch,selected=NULL,server=TRUE)
  })

  # Selectores secciones
  observe({
    df0 <- sf_all
    dls_in <- suppressWarnings(as.integer(input$dl_sel %||% character(0)))
    dls_in <- dls_in[!is.na(dls_in)]
    if (!is.na(SECC_DL_COL) && length(dls_in)>0L) df0 <- df0[df0[[SECC_DL_COL]] %in% dls_in, ]
    secs <- sort(unique(df0$SECCION))
    ch   <- as.list(setNames(as.character(secs),as.character(secs)))
    cur  <- intersect(isolate(input$secciones) %||% character(0), as.character(secs))
    updateSelectizeInput(session,"secciones",choices=ch,selected=cur,server=TRUE)
  })

  winner_vt <- reactive(toupper(input$winner_vote_type %||% "DISTRIBUIDO"))
  applied    <- reactiveVal(NULL)

  observeEvent(input$reset, {
    updateSelectizeInput(session,"dl_sel",selected=NULL,server=TRUE)
    updateSelectizeInput(session,"secciones",selected=NULL,server=TRUE)
    updateSelectInput(session,"election",selected=DEFAULT_ELECTION)
    updateSelectInput(session,"map_view",selected="winner")
    updateRadioButtons(session,"winner_vote_type",selected="DISTRIBUIDO")
    bslib::update_switch(session,"map_variable",value=FALSE)
    if (length(ELECTORADO_CHOICES)>0L) updateSelectInput(session,"electorado_var",
                                                         selected=ELECTORADO_CHOICES[["Lista nominal"]] %||% ELECTORADO_CHOICES[[1L]])
    updateRadioButtons(session,"ts_office",selected="BOTH")
    updateRadioButtons(session,"ts_vote_type",selected="DISTRIBUIDO")
    updateRadioButtons(session,"ts_party_metric",selected="pct")
    updateSelectizeInput(session,"ts_parties",selected=NULL,server=TRUE)
    updateSelectInput(session,"ts_map_election",selected=head(elex$key,1L) %||% DEFAULT_ELECTION)
    updateRadioButtons(session,"buf_mode",selected="electoral")
    updateCheckboxGroupInput(session,"buf_gen_vars",selected=c("LISTA_GEN_Z","LISTA_MILLENNIALS"))
    if (length(EJES_DISPONIBLES)>0L) updateSelectInput(session,"buf_eje",selected=EJES_DISPONIBLES[1L])
    updateSliderInput(session,"buf_cover_pct",value=80)
    updateSliderInput(session,"buf_radius_m",value=800)
    updateSliderInput(session,"buf_max_points",value=3)
    buf_applied(NULL)
  }, ignoreInit=TRUE)

  observeEvent(input$generar, {
    ap <- list(
      election        = input$election %||% DEFAULT_ELECTION,
      winner_vote_type= winner_vt(),
      map_variable    = isTRUE(input$map_variable),
      map_view        = input$map_view %||% "winner",
      choro_vote_type = input$choro_vote_type %||% "DISTRIBUIDO",
      choro_party     = input$choro_party %||% "",
      choro_metric    = input$choro_metric %||% "pct",
      choro_scale     = input$choro_scale %||% "linear",
      choro_opacity   = input$choro_opacity %||% 0.65,
      electorado_var  = input$electorado_var %||% (ELECTORADO_CHOICES[[1L]] %||% "LISTA_NOMINAL"),
      electorado_scale= input$electorado_scale %||% "linear",
      electorado_opacity=input$electorado_opacity %||% 0.70,
      dl_sel          = input$dl_sel %||% character(0),
      secciones       = input$secciones %||% character(0),
      ts              = Sys.time()
    )
    applied(ap)
    showNotification("Generado \u2705", type="message", duration=1.2)
  }, ignoreInit=TRUE)

  has_applied <- reactive({ ap <- applied(); !is.null(ap) && !is.null(ap$ts) })

  output$ui_status_run <- renderUI({
    ap <- applied()
    if (!has_applied()) {
      div(class="status-badge idle",
          HTML("<b>Sin ejecutar</b> \u00b7 configura y presiona GENERAR"))
    } else {
      div(class="status-badge ok",
          HTML(paste0("<b>Aplicado</b> \u00b7 ", format(ap$ts,"%H:%M:%S"),
                      " \u00b7 <b>", key_label(ap$election),"</b>")))
    }
  })

  df_applied <- reactive({
    req(has_applied())
    ap  <- applied()
    df  <- sf_all
    dls <- suppressWarnings(as.integer(ap$dl_sel %||% character(0)))
    dls <- dls[!is.na(dls)]
    if (!is.na(SECC_DL_COL) && length(dls)>0L) df <- df[df[[SECC_DL_COL]] %in% dls, ]
    if (length(ap$secciones)>0L) {
      s <- suppressWarnings(as.integer(ap$secciones)); s <- s[!is.na(s)]
      if (length(s)>0L) df <- df[df$SECCION %in% s, ]
    }
    df
  })

  dl_applied <- reactive({
    if (is.null(dl_sf)||nrow(dl_sf)==0) return(dl_sf)
    if (!has_applied()) return(dl_sf)
    ap  <- applied()
    dls <- suppressWarnings(as.integer(ap$dl_sel %||% character(0)))
    dls <- dls[!is.na(dls)]
    if (length(dls)==0L) return(dl_sf)
    if ("DISTRITO_L" %in% names(dl_sf)) dl_sf[dl_sf$DISTRITO_L %in% dls, ] else dl_sf
  })

  output$ui_choro_party <- renderUI({
    if (!isTRUE(input$map_variable)) return(NULL)
    req(has_applied())
    df  <- df_applied(); req(nrow(df)>0L)
    key <- applied()$election
    vt  <- input$choro_vote_type %||% "DISTRIBUIDO"
    gv  <- group_votes_matrix(df, key, vt)
    parties <- sort(unique(setdiff(gv$parties,"OTROS")))
    validate(need(length(parties)>0L,"Sin partidos disponibles"))
    sel <- (input$choro_party %||% parties[1L])[1L]
    if (!(sel %in% parties)) sel <- parties[1L]
    # Dropdown con logos
    choices_html <- setNames(parties, vapply(parties, function(p) {
      logo <- party_logo_inline(p, "14px")
      paste0(logo, p)
    }, character(1)))
    selectizeInput("choro_party","Partido",
                   choices=as.list(setNames(parties, parties)),
                   selected=sel)
  })

  # ---- MÉTRICAS BASE ----
  df_metrics <- reactive({
    req(has_applied())
    ap  <- applied()
    df  <- df_applied()
    key <- ap$election
    c_tot <- total_col(df,key); c_ln <- ln_col(df,key)
    c_cas <- metric_col(df,key,"CASILLAS"); c_val <- valid_col(df,key)
    tot  <- if (!is.na(c_tot)&&c_tot %in% names(df)) as_num(df[[c_tot]]) else NA_real_
    ln   <- if (!is.na(c_ln) &&c_ln  %in% names(df)) as_num(df[[c_ln]])  else NA_real_
    cas  <- if (!is.na(c_cas)&&c_cas %in% names(df)) as_num(df[[c_cas]]) else NA_real_
    val  <- if (!is.na(c_val)&&c_val %in% names(df)) as_num(df[[c_val]]) else NA_real_
    part <- ifelse(is.finite(ln)&ln>0, tot/ln, NA_real_)
    list(df=df,key=key,c_tot=c_tot,c_ln=c_ln,c_cas=c_cas,c_val=c_val,
         tot=tot,ln=ln,cas=cas,val=val,part=part)
  })

  kpi_placeholder <- function(t, s="Presiona GENERAR") {
    tagList(
      div(class="t", t),
      div(class="skeleton skeleton-val"),
      div(class="skeleton skeleton-sub"),
      div(class="skeleton skeleton-sub2")
    )
  }

  output$kpi1 <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Resumen"))
    x       <- df_metrics()
    tot_sum <- if (!all(is.na(x$tot))) sum(x$tot,na.rm=TRUE) else NA_real_
    ln_sum  <- if (!all(is.na(x$ln)))  sum(x$ln, na.rm=TRUE) else NA_real_
    cas_sum <- if (!all(is.na(x$cas))) sum(x$cas,na.rm=TRUE) else NA_real_
    part_pp <- if (is.finite(ln_sum)&&ln_sum>0&&is.finite(tot_sum)) 100*tot_sum/ln_sum else NA_real_
    bar_w   <- if (is.finite(part_pp)) paste0(min(max(part_pp,0),100),"%") else "0%"
    tagList(
      div(class="t","Secci\u00f3n / Votos"),
      div(class="v", paste0(fmt_int(nrow(x$df))," secc.")),
      div(class="s", HTML(paste0(
        "Votos: <b>",fmt_int(tot_sum),"</b>",
        " \u00b7 LN: <b>",fmt_int(ln_sum),"</b>",
        "<br>Part: <b>",ifelse(is.finite(part_pp),paste0(formatC(part_pp,format="f",digits=2),"%"),"\u2014"),"</b>",
        " \u00b7 Casillas: <b>",fmt_int(cas_sum),"</b>"
      ))),
      div(class="kpi-bar-track",
          div(class="kpi-bar-fill", style=paste0("width:",bar_w,";")))
    )
  })

  output$kpi2 <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Ganador \u00b7 2\u00ba lugar"))
    ap  <- applied(); x <- df_metrics()
    tot <- totals_for_view(x$df, ap$election, ap$winner_vote_type %||% "DISTRIBUIDO")
    top <- top2_from_totals(tot)
    if (!nzchar(top$w1 %||%"")||!is.finite(top$v1)) return(kpi_placeholder("Ganador \u00b7 2\u00ba lugar","Sin ranking"))
    margin <- if (is.finite(top$v2)) top$v1-top$v2 else NA_real_
    total_v <- if (is.finite(top$v2)) top$v1+top$v2 else top$v1
    bar_w   <- if (is.finite(total_v)&&total_v>0) paste0(min(100*top$v1/total_v,100),"%") else "50%"
    tagList(
      div(class="t","Ganador \u00b7 2\u00ba lugar"),
      div(class="v", HTML(paste0(party_logo_inline(top$w1,"22px"), "<b>",htmltools::htmlEscape(top$w1),"</b> \u00b7 ",fmt_int(top$v1)))),
      div(class="s", if (is.finite(top$v2)) {
        HTML(paste0(party_logo_inline(top$w2,"15px"),"2\u00ba: <b>",htmltools::htmlEscape(top$w2),"</b> \u00b7 ",
                    fmt_int(top$v2)," | margen: <b>",fmt_int(margin),"</b>"))
      } else "Sin 2\u00ba lugar"),
      div(class="kpi-bar-track",
          div(class="kpi-bar-fill", style=paste0("width:",bar_w,";background:",
              if (top$w1 %in% names(party_colors)) party_colors[[top$w1]] else ACCENT,";")))
    )
  })

  output$kpi3 <- renderUI({
    if (!has_applied()) return(kpi_placeholder("PRI \u00b7 PURO vs DISTRIBUIDO"))
    ap       <- applied(); x <- df_metrics()
    tot_puro <- totals_for_view(x$df,ap$election,"PURO")
    tot_dist <- totals_for_view(x$df,ap$election,"DISTRIBUIDO")
    pri_puro <- as.numeric(tot_puro[["PRI"]] %||% NA_real_)
    pri_dist <- as.numeric(tot_dist[["PRI"]] %||% NA_real_)
    if (!is.finite(pri_puro)&&!is.finite(pri_dist)) return(kpi_placeholder("PRI \u00b7 PURO vs DISTRIBUIDO","No hay columnas PRI"))
    pri_puro <- pri_puro %||% 0; pri_dist <- pri_dist %||% 0
    aporte   <- pri_dist - pri_puro
    share    <- if (is.finite(pri_dist)&&pri_dist>0) aporte/pri_dist else NA_real_
    bar_w    <- if (is.finite(pri_dist)&&pri_dist>0) paste0(min(100*pri_puro/pri_dist,100),"%") else "0%"
    tagList(
      div(class="t","PRI \u00b7 PURO vs DISTRIBUIDO"),
      div(class="v", HTML(paste0(party_logo_inline("PRI","22px"),"<b>",fmt_int(pri_puro),"</b> | <b>",fmt_int(pri_dist),"</b>"))),
      div(class="s", HTML(paste0(
        "Aporte coalici\u00f3n: <b>",fmt_signed_int(aporte),"</b>",
        if (is.finite(share)) paste0(" (<b>",formatC(100*share,format="f",digits=1),"%</b> del distrib.)") else ""
      ))),
      div(class="kpi-bar-track",
          div(class="kpi-bar-fill", style=paste0("width:",bar_w,";background:",
              if ("PRI" %in% names(party_colors)) party_colors[["PRI"]] else ACCENT,";")))
    )
  })

  output$map_subtitle <- renderUI({
    if (!has_applied()) return(div(class="subtitle-badge","\u23f8\ufe0f Presiona GENERAR"))
    ap  <- applied()
    txt <- if (isTRUE(ap$map_variable)) {
      paste0("Choropleth \u00b7 ",ap$choro_vote_type," \u00b7 ",ap$choro_party," \u00b7 ",ap$election)
    } else {
      switch(ap$map_view %||% "winner",
             winner   = paste0("Ganador (",ap$winner_vote_type,") \u00b7 ",ap$election),
             part     = paste0("Participaci\u00f3n (%) \u00b7 ",ap$election),
             tot      = paste0("Total votos \u00b7 ",ap$election),
             ln       = paste0("Lista nominal \u00b7 ",ap$election),
             electorado = paste0("Electorado \u00b7 ",ap$electorado_var),
             ap$election)
    }
    div(class="subtitle-badge", "\U0001F5FA\uFE0F ", txt)
  })

  output$bar_subtitle <- renderUI({
    if (!has_applied()) return(div(class="subtitle-badge","\u23f8\ufe0f Presiona GENERAR"))
    div(class="subtitle-badge",
        "\U0001F4CA ", paste0("Suma en selecci\u00f3n aplicada \u00b7 ", applied()$election))
  })

  # ---- MAPA EXPLORAR ----
  output$map <- renderLeaflet({
    m <- leaflet(options=leafletOptions(preferCanvas=TRUE)) |>
      addProviderTiles(BASEMAPS[["Oscuro"]],    group="Oscuro")   |>
      addProviderTiles(BASEMAPS[["Claro"]],     group="Claro")    |>
      addProviderTiles(BASEMAPS[["Calles"]],    group="Calles")   |>
      addProviderTiles(BASEMAPS[["Sat\u00e9lite"]], group="Sat\u00e9lite") |>
      add_dl_layer() |> add_layers_control() |> hideGroup(DL_GROUP) |>
      addScaleBar(position="bottomleft") |>
      setView(lng=INIT_LNG, lat=INIT_LAT, zoom=INIT_ZOOM)
    if (has_fullscreen) m <- leaflet.extras::addFullscreenControl(m, position="topleft", pseudoFullscreen=FALSE)
    m
  })

  observe({
    req(has_applied())
    ap <- applied(); x <- df_metrics(); df <- x$df; req(nrow(df)>0L)
    proxy <- leafletProxy("map",session=session) |> clearShapes() |> clearMarkers() |> clearControls()
    proxy <- restore_map_controls(proxy)
    key   <- ap$election

    lab_base <- paste0(
      "<b style='color:#FFFFFF'>Secci\u00f3n:</b> <b style='color:",ACCENT,"'>", df$SECCION,"</b>",
      if (!is.na(x$c_cas)) paste0("<br>Casillas: <b>",fmt_int(df[[x$c_cas]]),"</b>") else "",
      if (!is.na(x$c_tot)) paste0("<br>Total votos: <b>",fmt_int(df[[x$c_tot]]),"</b>") else "",
      if (!is.na(x$c_ln))  paste0("<br>Lista nominal: <b>",fmt_int(df[[x$c_ln]]),"</b>")  else "",
      "<br>Participaci\u00f3n: <b>",fmt_pct(x$part),"</b>"
    )

    if (isTRUE(ap$map_variable)) {
      vt <- ap$choro_vote_type %||% "DISTRIBUIDO"
      gv <- group_votes_matrix(df,key,vt); G <- gv$G
      validate(need(!is.null(G)&&ncol(G)>0L,"Sin matriz para choropleth."))
      pick         <- ap$choro_party %||% colnames(G)[1L]
      if (!(pick %in% colnames(G))) pick <- colnames(G)[1L]
      party_accent <- if (pick %in% names(party_colors)) unname(party_colors[[pick]]) else ACCENT
      v_votes <- as.numeric(G[,pick])
      den <- if (!is.na(x$c_val)&&x$c_val %in% names(df)) as_num(df[[x$c_val]]) else rowSums(G,na.rm=TRUE)
      den[!is.finite(den)|den<=0] <- NA_real_
      scale <- ap$choro_scale %||% "linear"; opac <- ap$choro_opacity %||% 0.65

      if ((ap$choro_metric %||% "pct")=="pct") {
        v   <- ifelse(is.na(den),NA_real_,v_votes/den)
        res <- make_pal_pos(v,scale=scale,accent=party_accent)
        lab <- paste0(lab_base,"<br>",party_logo_inline(pick,"14px"),"<b>",pick," (%):</b> ",fmt_pct(v))
        ttl <- HTML(paste0(party_logo_inline(pick,"14px"),"<b>",pick,"</b> (% v\u00e1lidos)"))
        proxy <- proxy |>
          addPolygons(data=df,color="rgba(255,255,255,.18)",weight=1,
                      fillColor=res$pal(res$values),fillOpacity=opac,
                      label=lapply(lab,HTML),
                      highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE)) |>
          addLegend(position="bottomright",pal=res$pal,values=res$values,title=ttl,opacity=0.9)
      } else {
        v   <- v_votes
        res <- make_pal_pos(v,scale=scale,accent=party_accent)
        lab <- paste0(lab_base,"<br>",party_logo_inline(pick,"14px"),"<b>",pick," (votos):</b> ",fmt_int(v))
        ttl <- HTML(paste0(party_logo_inline(pick,"14px"),"<b>",pick,"</b> (votos)"))
        proxy <- proxy |>
          addPolygons(data=df,color="rgba(255,255,255,.18)",weight=1,
                      fillColor=res$pal(res$values),fillOpacity=opac,
                      label=lapply(lab,HTML),
                      highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE)) |>
          addLegend(position="bottomright",pal=res$pal,values=res$values,title=ttl,opacity=0.9)
      }
    } else {
      mv <- ap$map_view %||% "winner"
      if (mv=="electorado") {
        col_elec   <- ap$electorado_var %||% (ELECTORADO_CHOICES[[1L]] %||% "LISTA_NOMINAL")
        validate(need(col_elec %in% names(df),paste0("Columna '",col_elec,"' no encontrada.")))
        meta_match <- ELECTORADO_AVAILABLE[vapply(ELECTORADO_AVAILABLE,function(z) z$col==col_elec, logical(1))]
        meta       <- if (length(meta_match)) meta_match[[1L]] else list(tipo="abs",color=ACCENT)
        lbl_name   <- names(meta_match)[1L] %||% col_elec
        accent_col <- meta$color %||% ACCENT
        v   <- as_num(df[[col_elec]])
        res <- make_pal_pos(v,scale=ap$electorado_scale %||% "linear",accent=accent_col)
        fmt_val <- if (meta$tipo=="pct") paste0(formatC(v,format="f",digits=1,big.mark=","),"%") else fmt_int(v)
        lab <- paste0(lab_base,"<br><b>",lbl_name,":</b> ",fmt_val)
        proxy <- proxy |>
          addPolygons(data=df,color="rgba(255,255,255,.18)",weight=1,
                      fillColor=res$pal(res$values),fillOpacity=ap$electorado_opacity %||% 0.70,
                      label=lapply(lab,HTML),
                      highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE)) |>
          addLegend(position="bottomright",pal=res$pal,values=res$values,title=lbl_name,opacity=0.9)
      } else if (mv=="winner") {
        vt <- ap$winner_vote_type %||% "DISTRIBUIDO"
        w  <- winner_by_row(df,key,vt)
        df$WINNER <- w; df$FILL <- vapply(df$WINNER,fill_color_winner,character(1))
        winner_logo_html <- vapply(df$WINNER, function(ww) {
          if (is.na(ww)) return("\u2014")
          paste0(party_logo_inline(ww,"15px"),"<b>",htmltools::htmlEscape(ww),"</b>")
        }, character(1))
        lab <- paste0(lab_base,"<br>Ganador: ",winner_logo_html)
        proxy <- proxy |>
          addPolygons(data=df,color="rgba(255,255,255,.18)",weight=1,
                      fillColor=~FILL,fillOpacity=0.62,
                      label=lapply(lab,HTML),
                      highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE))
        leg_vals <- sort(unique(df$WINNER[!is.na(df$WINNER)]))
        if (length(leg_vals)>0L) {
          leg_cols <- vapply(leg_vals,fill_color_winner,character(1))
          # Leyenda con logos en HTML
          leg_labels <- vapply(leg_vals, function(p) {
            as.character(HTML(paste0(party_logo_inline(p,"13px"), p)))
          }, character(1))
          proxy <- proxy |> addLegend(position="bottomright",colors=as.vector(leg_cols),
                                      labels=leg_labels,
                                      title=paste0("Ganador (",vt,")"),opacity=0.9)
        }
      } else {
        v   <- switch(mv, part=x$part, tot=x$tot, ln=x$ln, x$tot)
        pal <- pal_bw_accent(v,accent=ACCENT)
        ttl <- switch(mv,"part"="Participaci\u00f3n (%)","tot"="Total votos","ln"="Lista nominal","M\u00e9trica")
        lab <- if (mv=="part") paste0(lab_base,"<br><b>",ttl,":</b> ",fmt_pct(v)) else paste0(lab_base,"<br><b>",ttl,":</b> ",fmt_int(v))
        proxy <- proxy |>
          addPolygons(data=df,color="rgba(255,255,255,.18)",weight=1,
                      fillColor=pal(v),fillOpacity=0.62,
                      label=lapply(lab,HTML),
                      highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE)) |>
          addLegend(position="bottomright",pal=pal,values=v,title=ttl,opacity=0.9)
      }
    }
    proxy <- add_dl_layer(proxy,dl_applied())
    proxy <- add_layers_control(proxy)
    proxy <- hideGroup(proxy,setdiff(names(BASEMAPS),BASEMAP_DEFAULT))
    bb    <- st_bbox(df)
    proxy |> fitBounds(bb["xmin"],bb["ymin"],bb["xmax"],bb["ymax"])
  })

  # ---- BARRAS (con logos en barras) ----
  plotly_dark_layout <- list(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font          = list(color="#FFFFFF"),
    xaxis         = list(color="#FFFFFF", gridcolor="rgba(255,255,255,.10)", zerolinecolor="rgba(255,255,255,.15)"),
    yaxis         = list(color="#FFFFFF", gridcolor="rgba(255,255,255,.10)")
  )

  output$bar <- renderPlotly({
    validate(need(has_applied(),"Presiona GENERAR"))
    ap  <- applied(); x <- df_metrics(); df <- x$df
    validate(need(nrow(df)>0L,"Sin datos"))
    key <- ap$election; vt <- ap$winner_vote_type %||% "DISTRIBUIDO"

    if (isTRUE(input$bar_is_cand)) {
      cols <- vote_cols_raw(df,key,"CAND")
      validate(need(length(cols)>0L,"Sin columnas CAND_"))
      M      <- safe_num_matrix(df,cols)
      totals <- colSums(M,na.rm=TRUE)
      keep   <- which(totals>0L)
      validate(need(length(keep)>0L,"CAND_ sin votos"))
      cols   <- cols[keep]; totals <- totals[keep]
      ord    <- order(totals,decreasing=TRUE)[seq_len(min(18L,length(totals)))]
      cols   <- cols[ord]; totals <- totals[ord]
      labs   <- make.unique(vapply(cols,party_from_col,character(1),key=key,vote_type="CAND"))
      dd     <- data.frame(lbl=labs,votos=as.numeric(totals),stringsAsFactors=FALSE)
      dd$lbl <- factor(dd$lbl,levels=rev(dd$lbl))
      cols_bar <- vapply(as.character(dd$lbl),function(l) {
        p <- guess_party_from_cand_label(l)
        if (!is.na(p)&&p %in% names(party_colors)) unname(party_colors[[p]]) else "#6B7280"
      },character(1))
      p <- plot_ly(dd, x=~votos, y=~lbl, type="bar", orientation="h",
                   marker=list(
                     color=cols_bar,
                     line=list(color="rgba(255,255,255,.18)", width=0.8),
                     opacity=0.92
                   ),
                   text=~paste0("<b>",lbl,"</b><br>Votos: <b>",fmt_int(votos),"</b>"),
                   hoverinfo="text") |>
        layout(
          xaxis=list(title="Votos", color="#FFFFFF",
                     gridcolor="rgba(255,255,255,.07)",
                     tickfont=list(color="#FFFFFF"),
                     zeroline=FALSE),
          yaxis=list(title="", showticklabels=FALSE, ticklen=0, color="#FFFFFF"),
          hoverlabel=list(
            bgcolor="rgba(5,8,14,.95)",
            bordercolor="rgba(255,255,255,.18)",
            font=list(color="#FFFFFF", size=12, family="Inter, system-ui, sans-serif")
          ),
          margin=list(l=60, r=20, b=40, t=10),
          paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
          font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
          showlegend=FALSE
        ) |> config(displayModeBar=FALSE, responsive=TRUE)
      lbl_levels <- levels(dd$lbl)
      add_logos_to_plotly_h(p, lbl_levels, size=0.052)
    } else {
      gv     <- group_votes_matrix(df,key,vt); G <- gv$G
      validate(need(!is.null(G)&&ncol(G)>0L,"Sin columnas de partidos"))
      totals <- colSums(G,na.rm=TRUE); totals <- totals[totals>0L]
      validate(need(length(totals)>0L,"Sin votos"))
      ord  <- order(totals,decreasing=TRUE)
      keep <- names(totals)[ord][seq_len(min(14L,length(ord)))]
      dd   <- data.frame(lbl=keep,votos=as.numeric(totals[keep]),stringsAsFactors=FALSE)
      dd$lbl <- factor(dd$lbl,levels=rev(dd$lbl))
      cols_bar <- vapply(as.character(dd$lbl),function(p) {
        if (p %in% names(party_colors)) unname(party_colors[[p]]) else fill_color_winner(p)
      },character(1))
      p <- plot_ly(dd, x=~votos, y=~lbl, type="bar", orientation="h",
                   marker=list(
                     color=cols_bar,
                     line=list(color="rgba(255,255,255,.18)", width=0.8),
                     opacity=0.92
                   ),
                   text=~paste0("<b>",lbl,"</b><br>Votos: <b>",fmt_int(votos),"</b>"),
                   hoverinfo="text") |>
        layout(
          xaxis=list(title="Votos", color="#FFFFFF",
                     gridcolor="rgba(255,255,255,.07)",
                     tickfont=list(color="#FFFFFF"),
                     zeroline=FALSE),
          yaxis=list(title="", showticklabels=FALSE, ticklen=0, color="#FFFFFF"),
          hoverlabel=list(
            bgcolor="rgba(5,8,14,.95)",
            bordercolor="rgba(255,255,255,.18)",
            font=list(color="#FFFFFF", size=12, family="Inter, system-ui, sans-serif")
          ),
          margin=list(l=60, r=20, b=40, t=10),
          paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
          font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
          showlegend=FALSE
        ) |> config(displayModeBar=FALSE, responsive=TRUE)
      lbl_levels <- levels(dd$lbl)
      add_logos_to_plotly_h(p, lbl_levels, size=0.058)
    }
  })

  # DT helper dark init
  dt_dark_init <- JS("function(settings, json) {
    $(this.api().table().container()).css({'color':'#FFFFFF','background':'transparent'});
  }")

  tbl_data <- reactive({
    validate(need(has_applied(),"Presiona GENERAR"))
    ap <- applied(); x <- df_metrics(); df <- x$df; key <- ap$election
    vt_tbl <- input$table_view %||% "DISTRIBUIDO"
    out <- data.frame(
      SECCION      = df$SECCION,
      CASILLAS     = if (!is.na(x$c_cas)) as_num(df[[x$c_cas]]) else NA_real_,
      TOTAL_VOTOS  = if (!is.na(x$c_tot)) as_num(df[[x$c_tot]]) else NA_real_,
      LISTA_NOMINAL= if (!is.na(x$c_ln))  as_num(df[[x$c_ln]])  else NA_real_,
      PARTICIPACION= x$part,
      stringsAsFactors=FALSE
    )
    if (vt_tbl %in% c("PURO","DISTRIBUIDO")) {
      gv <- group_votes_matrix(df,key,vt_tbl); G <- gv$G
      if (!is.null(G)&&ncol(G)>0L) {
        totals <- colSums(G,na.rm=TRUE)
        keep   <- names(sort(totals,decreasing=TRUE))[seq_len(min(8L,length(totals)))]
        out    <- cbind(out,as.data.frame(G[,keep,drop=FALSE]))
      }
    } else {
      cols <- vote_cols_raw(df,key,"CAND")
      if (length(cols)>0L) {
        M <- safe_num_matrix(df,cols)
        if (!is.null(M)&&ncol(M)>0L) {
          totals <- colSums(M,na.rm=TRUE)
          ord    <- order(totals,decreasing=TRUE)[seq_len(min(8L,length(totals)))]
          M2     <- M[,ord,drop=FALSE]
          colnames(M2) <- make.unique(vapply(cols[ord],party_from_col,character(1),key=key,vote_type="CAND"))
          out    <- cbind(out,as.data.frame(M2))
        }
      }
    }
    out[order(out$SECCION),,drop=FALSE]
  })

  output$tbl <- renderDT({
    validate(need(has_applied(),"Presiona GENERAR"))
    d <- tbl_data()
    base_cols <- c("SECCION","CASILLAS","TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION")
    # Headers con logos
    headers_html <- vapply(names(d), function(nm) {
      if (nm %in% base_cols) return(nm)
      logo <- party_logo_inline(nm,"16px")
      if (nzchar(logo)) paste0(logo,nm) else nm
    }, character(1))
    header_cb <- htmltools::withTags(table(
      class="display",
      thead(tr(lapply(headers_html, function(h) th(HTML(h)))))
    ))
    dt <- datatable(
      d, rownames=FALSE, filter="top", container=header_cb, escape=FALSE,
      extensions=c("Buttons"),
      options=list(
        dom="Bfrtip", scrollY="500px", scrollX=TRUE, scrollCollapse=TRUE,
        pageLength=1000, autoWidth=TRUE, searching=TRUE,
        buttons=c("copy","csv","excel","pdf","print","pageLength"),
        initComplete = dt_dark_init
      )
    )
    cols_round <- intersect(c("CASILLAS","TOTAL_VOTOS","LISTA_NOMINAL"),names(d))
    if (length(cols_round)>0L) dt <- dt |> formatRound(cols_round,digits=0)
    if ("PARTICIPACION" %in% names(d)) dt <- dt |> formatPercentage("PARTICIPACION",digits=2)
    dt
  })

  output$download_csv <- downloadHandler(
    filename = function() { ap <- applied(); if (is.null(ap)) return("atizapan_sin_generar.csv")
      paste0("atizapan_secciones_",ap$election,"_tabla_",(input$table_view %||% "DISTRIBUIDO"),".csv") },
    content = function(file) write.csv(tbl_data(),file,row.names=FALSE,fileEncoding="UTF-8")
  )

  # ---- TIEMPO ----
  ts_keys <- reactive({
    req(has_applied())
    sel  <- input$ts_office %||% "BOTH"
    keys <- elex$key
    if (sel %in% c("AYU","DL")) keys <- keys[grepl(paste0("^",sel,"_"),keys)]
    keys
  })

  observeEvent(list(has_applied(),input$ts_vote_type,input$ts_include_other,input$ts_office), {
    req(has_applied())
    keys <- ts_keys(); df <- df_applied(); vt <- input$ts_vote_type %||% "DISTRIBUIDO"
    out <- character(0)
    for (k in keys) { gv <- group_votes_matrix(df,k,vt); if (!is.null(gv$G)) out <- unique(c(out,colnames(gv$G))) }
    out <- out[!is.na(out)&nzchar(out)]
    if (!isTRUE(input$ts_include_other)) out <- setdiff(out,"OTROS")
    out <- sort(unique(out))
    updateSelectizeInput(session,"ts_parties",
                         choices=as.list(setNames(out,out)),
                         selected=intersect(c("PRI","PAN","MORENA","MC","PVEM","PT","PRD"),out),
                         server=TRUE)
  }, ignoreInit=FALSE)

  output$ui_ts_choro_party <- renderUI({
    validate(need(has_applied(),"Presiona GENERAR"))
    keys <- ts_keys(); df <- df_applied(); vt <- input$ts_vote_type %||% "DISTRIBUIDO"
    out <- character(0)
    for (k in keys) { gv <- group_votes_matrix(df,k,vt); if (!is.null(gv$G)) out <- unique(c(out,colnames(gv$G))) }
    out <- sort(unique(setdiff(out,"OTROS")))
    validate(need(length(out)>0L,"Sin partidos"))
    def <- if ("PRI" %in% out) "PRI" else out[1L]
    selectInput("ts_choro_party","Partido",choices=out,selected=def)
  })

  ts_party_series <- reactive({
    validate(need(has_applied(),"Presiona GENERAR"))
    df <- df_applied(); keys <- ts_keys()
    vt <- input$ts_vote_type %||% "DISTRIBUIDO"; metric <- input$ts_party_metric %||% "pct"
    sel <- toupper(input$ts_parties %||% character(0))
    if (!isTRUE(input$ts_include_other)) sel <- setdiff(sel,"OTROS")
    if (length(sel)==0L) {
      acc <- list()
      for (k in keys) {
        gv <- group_votes_matrix(df,k,vt); if (is.null(gv$G)) next
        tot <- colSums(gv$G,na.rm=TRUE)
        if (!isTRUE(input$ts_include_other)) tot <- tot[names(tot)!="OTROS"]
        for (nm in names(tot)) acc[[nm]] <- (acc[[nm]] %||% 0)+as.numeric(tot[[nm]])
      }
      n_top <- as.integer(input$ts_top_n %||% 8L)
      ord   <- order(unlist(acc),decreasing=TRUE)
      sel   <- names(acc)[ord][seq_len(min(n_top,length(ord)))]
    }
    rows <- list()
    for (k in keys) {
      gv <- group_votes_matrix(df,k,vt); G <- gv$G; if (is.null(G)||ncol(G)==0L) next
      c_val <- valid_col(df,k)
      den   <- if (!is.na(c_val)&&c_val %in% names(df)) sum(as_num(df[[c_val]]),na.rm=TRUE) else sum(G,na.rm=TRUE)
      if (!is.finite(den)||den<=0) den <- NA_real_
      tot   <- colSums(G,na.rm=TRUE)
      for (p in sel) {
        v <- if (p %in% names(tot)) as.numeric(tot[[p]]) else 0
        y <- if (metric=="pct") { if (is.finite(den)&&den>0) v/den else NA_real_ } else v
        rows[[length(rows)+1L]] <- data.frame(key=k,label=key_label(k),party=p,value=y,raw_votes=v,den=den,stringsAsFactors=FALSE)
      }
    }
    out <- do.call(rbind,rows)
    out$label <- factor(out$label,levels=vapply(keys,key_label,character(1)))
    out
  })

  output$ts_party_plot <- renderPlotly({
    validate(need(has_applied(),"Presiona GENERAR"))
    d      <- ts_party_series()
    validate(need(nrow(d)>0L,"Sin serie"))
    metric  <- input$ts_party_metric %||% "pct"
    y_title <- if (metric=="pct") "% (sobre v\u00e1lidos)" else "Votos"
    p <- plot_ly()
    parties <- unique(d$party)
    for (pp in parties) {
      dd       <- d[d$party==pp,,drop=FALSE]
      y        <- if (metric=="pct") 100*dd$value else dd$value
      col_line <- if (pp %in% names(party_colors)) unname(party_colors[[pp]]) else ACCENT
      col_fill <- sub("^(#[A-Fa-f0-9]{6})", "\\1", col_line)
      hover <- if (metric=="pct") {
        paste0("<b>",pp,"</b> · ",dd$label,
               "<br>%: ",ifelse(is.na(dd$value),"\u2014",paste0(formatC(100*dd$value,format="f",digits=2),"%")),
               "<br>Votos: ",fmt_int(dd$raw_votes))
      } else {
        paste0("<b>",pp,"</b> · ",dd$label,
               "<br>Votos: ",fmt_int(y))
      }
      p <- p |> add_trace(data=dd, x=~label, y=y, type="scatter",
                          mode="lines+markers", name=pp,
                          line=list(color=col_line, width=2.8, shape="spline", smoothing=0.5),
                          marker=list(size=9, color=col_line, line=list(color="#FFFFFF",width=1.5),
                                      symbol="circle"),
                          fill="tozeroy",
                          fillcolor=paste0(col_fill,"1A"),
                          text=hover, hoverinfo="text")
    }
    p |> layout(
      xaxis=list(title="", tickangle=-25, color="#FFFFFF",
                 gridcolor="rgba(255,255,255,.06)", zeroline=FALSE),
      yaxis=list(title=y_title, color="#FFFFFF",
                 gridcolor="rgba(255,255,255,.06)", zeroline=FALSE),
      margin=list(l=55,r=10,b=85,t=10),
      hovermode="x unified",
      hoverlabel=list(
        bgcolor  = "rgba(5,8,14,.95)",
        bordercolor = "rgba(255,255,255,.18)",
        font=list(color="#FFFFFF", size=12, family="Inter, system-ui, sans-serif")
      ),
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
      legend=list(orientation="h", x=0, y=1.15, font=list(color="#FFFFFF"),
                  bgcolor="rgba(0,0,0,0)")
    ) |> config(displayModeBar=FALSE, responsive=TRUE)
  })

  ts_metrics_series <- reactive({
    validate(need(has_applied(),"Presiona GENERAR"))
    df <- df_applied(); keys <- ts_keys()
    rows <- list()
    for (k in keys) {
      c_tot <- total_col(df,k); c_ln <- ln_col(df,k)
      c_cas <- metric_col(df,k,"CASILLAS"); c_val <- metric_col(df,k,"NUM_VOTOS_VALIDOS"); c_nul <- metric_col(df,k,"NUM_VOTOS_NULOS")
      tot  <- if (!is.na(c_tot)&&c_tot %in% names(df)) sum(as_num(df[[c_tot]]),na.rm=TRUE) else NA_real_
      ln   <- if (!is.na(c_ln) &&c_ln  %in% names(df)) sum(as_num(df[[c_ln]]), na.rm=TRUE) else NA_real_
      cas  <- if (!is.na(c_cas)&&c_cas %in% names(df)) sum(as_num(df[[c_cas]]),na.rm=TRUE) else NA_real_
      val  <- if (!is.na(c_val)&&c_val %in% names(df)) sum(as_num(df[[c_val]]),na.rm=TRUE) else NA_real_
      nul  <- if (!is.na(c_nul)&&c_nul %in% names(df)) sum(as_num(df[[c_nul]]),na.rm=TRUE) else NA_real_
      part <- if (is.finite(tot)&&is.finite(ln)&&ln>0) tot/ln else NA_real_
      rows[[length(rows)+1L]] <- data.frame(key=k,label=key_label(k),total=tot,lista=ln,casillas=cas,validos=val,nulos=nul,participacion=part,stringsAsFactors=FALSE)
    }
    out <- do.call(rbind,rows)
    out$label <- factor(out$label,levels=vapply(keys,key_label,character(1)))
    out
  })

  output$ts_metrics_plot <- renderPlotly({
    validate(need(has_applied(),"Presiona GENERAR"))
    d   <- ts_metrics_series(); validate(need(nrow(d)>0L,"Sin serie"))
    sel <- intersect(input$ts_metrics %||% c("total","lista"), c("total","lista","casillas","validos","nulos"))
    validate(need(length(sel)>0L,"Selecciona al menos una m\u00e9trica"))
    name_map  <- c(total="Total votos", lista="Lista nominal", casillas="Casillas",
                   validos="V\u00e1lidos", nulos="Nulos")
    color_map <- c(total=ACCENT, lista="#00B5E2", casillas="#FFD200",
                   validos="#2EAD4A", nulos="#FF6A00")
    p <- plot_ly()
    for (m in sel) {
      y   <- d[[m]]
      col <- color_map[[m]] %||% ACCENT
      p <- p |> add_trace(
        x=d$label, y=y, type="scatter", mode="lines+markers",
        name=name_map[[m]],
        line=list(color=col, width=2.5, shape="spline", smoothing=0.5),
        marker=list(size=8, color=col, line=list(color="#FFFFFF",width=1.5)),
        fill="tozeroy",
        fillcolor=paste0(col,"15"),
        text=paste0("<b>",d$label,"</b><br>",name_map[[m]],": <b>",fmt_int(y),"</b>"),
        hoverinfo="text"
      )
    }
    p |> layout(
      xaxis=list(title="", tickangle=-25, color="#FFFFFF",
                 gridcolor="rgba(255,255,255,.06)", zeroline=FALSE),
      yaxis=list(title="Conteos", color="#FFFFFF",
                 gridcolor="rgba(255,255,255,.06)", zeroline=FALSE),
      margin=list(l=55,r=10,b=85,t=10),
      hovermode="x unified",
      hoverlabel=list(
        bgcolor="rgba(5,8,14,.95)",
        bordercolor="rgba(255,255,255,.18)",
        font=list(color="#FFFFFF", size=12, family="Inter, system-ui, sans-serif")
      ),
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
      legend=list(orientation="h", x=0, y=1.15, font=list(color="#FFFFFF"),
                  bgcolor="rgba(0,0,0,0)")
    ) |> config(displayModeBar=FALSE, responsive=TRUE)
  })

  output$ts_particip_plot <- renderPlotly({
    validate(need(has_applied(),"Presiona GENERAR"))
    d     <- ts_metrics_series(); validate(need(nrow(d)>0L,"Sin serie"))
    hover <- paste0("<b>",d$label,"</b>",
                    "<br>\U0001F4CA Participaci\u00f3n: <b>",fmt_pct(d$participacion),"</b>",
                    "<br>Total votos: ",fmt_int(d$total),
                    "<br>Lista nominal: ",fmt_int(d$lista))
    plot_ly(d, x=~label, y=~I(100*participacion), type="scatter", mode="lines+markers",
            name="Participaci\u00f3n (%)", text=hover, hoverinfo="text",
            line=list(color=ACCENT, width=3, shape="spline", smoothing=0.6),
            marker=list(size=10, color=ACCENT, line=list(color="#FFFFFF",width=2)),
            fill="tozeroy",
            fillcolor=paste0(ACCENT,"25")) |>
      layout(
        xaxis=list(title="", tickangle=-25, color="#FFFFFF",
                   gridcolor="rgba(255,255,255,.06)", zeroline=FALSE),
        yaxis=list(title="Participaci\u00f3n (%)", color="#FFFFFF",
                   gridcolor="rgba(255,255,255,.06)", zeroline=FALSE,
                   ticksuffix="%"),
        margin=list(l=55,r=10,b=85,t=10),
        hovermode="x unified",
        hoverlabel=list(
          bgcolor="rgba(5,8,14,.95)",
          bordercolor="rgba(255,255,255,.18)",
          font=list(color="#FFFFFF", size=12, family="Inter, system-ui, sans-serif")
        ),
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
        font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
        showlegend=FALSE
      ) |> config(displayModeBar=FALSE, responsive=TRUE)
  })

  output$download_ts_parties_csv <- downloadHandler(
    filename=function() paste0("serie_partidos_",format(Sys.time(),"%Y%m%d_%H%M"),".csv"),
    content =function(file) write.csv(ts_party_series(),file,row.names=FALSE,fileEncoding="UTF-8")
  )
  output$download_ts_metrics_csv <- downloadHandler(
    filename=function() paste0("serie_metricas_",format(Sys.time(),"%Y%m%d_%H%M"),".csv"),
    content =function(file) write.csv(ts_metrics_series(),file,row.names=FALSE,fileEncoding="UTF-8")
  )

  output$map_time <- renderLeaflet({
    m <- leaflet(options=leafletOptions(preferCanvas=TRUE)) |>
      addProviderTiles(BASEMAPS[["Oscuro"]],    group="Oscuro")   |>
      addProviderTiles(BASEMAPS[["Claro"]],     group="Claro")    |>
      addProviderTiles(BASEMAPS[["Calles"]],    group="Calles")   |>
      addProviderTiles(BASEMAPS[["Sat\u00e9lite"]],group="Sat\u00e9lite") |>
      add_dl_layer() |> add_layers_control() |> hideGroup(DL_GROUP) |>
      addScaleBar(position="bottomleft") |>
      setView(lng=INIT_LNG, lat=INIT_LAT, zoom=INIT_ZOOM)
    if (has_fullscreen) m <- leaflet.extras::addFullscreenControl(m,position="topleft",pseudoFullscreen=FALSE)
    m
  })

  observe({
    validate(need(has_applied(),"Presiona GENERAR"))
    df <- df_applied(); req(nrow(df)>0L)
    req(input$ts_map_election, input$ts_map_view, input$ts_delta_scale)
    base_key <- input$ts_map_election; ref_key <- REF_KEY
    view     <- input$ts_map_view %||% "choro_party"; scale <- input$ts_delta_scale %||% "linear"
    proxy <- leafletProxy("map_time",session=session) |> clearShapes() |> clearMarkers() |> clearControls()
    proxy <- restore_map_controls(proxy)
    c_tot_b <- total_col(df,base_key); c_ln_b <- ln_col(df,base_key)
    tot_b   <- if (!is.na(c_tot_b)&&c_tot_b %in% names(df)) as_num(df[[c_tot_b]]) else NA_real_
    ln_b    <- if (!is.na(c_ln_b) &&c_ln_b  %in% names(df)) as_num(df[[c_ln_b]])  else NA_real_
    part_b  <- ifelse(is.finite(ln_b)&ln_b>0, tot_b/ln_b, NA_real_)
    c_tot_r <- total_col(df,ref_key); c_ln_r <- ln_col(df,ref_key)
    tot_r   <- if (!is.na(c_tot_r)&&c_tot_r %in% names(df)) as_num(df[[c_tot_r]]) else NA_real_
    ln_r    <- if (!is.na(c_ln_r) &&c_ln_r  %in% names(df)) as_num(df[[c_ln_r]])  else NA_real_
    part_r  <- ifelse(is.finite(ln_r)&ln_r>0, tot_r/ln_r, NA_real_)
    hdr <- paste0("<b>Secci\u00f3n:</b> <b style='color:",ACCENT,"'>",df$SECCION,"</b>",
                  "<br>Base: <b>",key_label(base_key),"</b>",
                  "<br>Ref fija: <b>",key_label(ref_key),"</b>")
    add_delta_layer <- function(proxy,delta,base_val,ref_val,ttl,fmt_fn,opac) {
      pal <- make_pal_delta(delta,scale=scale)
      lab <- paste0(hdr,"<br><br><b>",ttl,"</b>",
                    "<br>Base: <b>",fmt_fn(base_val),"</b>",
                    "<br>Ref: <b>",fmt_fn(ref_val),"</b>",
                    "<br><b>\u0394:</b> ",if (identical(fmt_fn,fmt_pct)) fmt_signed_pp(delta) else fmt_signed_int(delta))
      proxy |>
        addPolygons(data=df,color="rgba(255,255,255,.22)",weight=1,
                    fillColor=pal$pal(pal$values),fillOpacity=opac,
                    label=lapply(lab,HTML),
                    highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE)) |>
        addLegend(position="bottomright",pal=pal$pal,values=pal$values,
                  title=paste0("\u0394 ",ttl," \u00b7 Ref \u2212 Base"),opacity=0.9)
    }
    opac <- input$ts_map_opacity %||% 0.70
    if (view=="participacion") {
      proxy <- add_delta_layer(proxy,100*(part_r-part_b),part_b,part_r,"Participaci\u00f3n (%)",fmt_pct,opac)
    } else if (view=="lista") {
      proxy <- add_delta_layer(proxy,ln_r-ln_b,ln_b,ln_r,"Lista nominal",fmt_int,opac)
    } else if (view=="mas_votantes") {
      proxy <- add_delta_layer(proxy,tot_r-tot_b,tot_b,tot_r,"Total votos",fmt_int,opac)
    } else {
      req(input$ts_choro_party)
      vt   <- input$ts_vote_type %||% "DISTRIBUIDO"; pick <- input$ts_choro_party; met <- input$ts_choro_metric %||% "votes"
      gv_b <- group_votes_matrix(df,base_key,vt); G_b <- gv_b$G
      gv_r <- group_votes_matrix(df,ref_key,  vt); G_r <- gv_r$G
      validate(need(!is.null(G_b)&&!is.null(G_r),"Sin matrices de votos"))
      if (!(pick %in% colnames(G_b))) pick <- colnames(G_b)[1L]
      votes_b <- as.numeric(G_b[,pick])
      votes_r <- if (pick %in% colnames(G_r)) as.numeric(G_r[,pick]) else rep(0,nrow(df))
      den_b <- if (!is.na(valid_col(df,base_key))) as_num(df[[valid_col(df,base_key)]]) else rowSums(G_b,na.rm=TRUE)
      den_r <- if (!is.na(valid_col(df,ref_key)))  as_num(df[[valid_col(df,ref_key)]])  else rowSums(G_r,na.rm=TRUE)
      den_b[!is.finite(den_b)|den_b<=0] <- NA_real_; den_r[!is.finite(den_r)|den_r<=0] <- NA_real_
      party_accent <- if (pick %in% names(party_colors)) unname(party_colors[[pick]]) else ACCENT
      if (met=="pct") {
        pct_b    <- ifelse(is.na(den_b),NA_real_,votes_b/den_b)
        pct_r    <- ifelse(is.na(den_r),NA_real_,votes_r/den_r)
        delta_pp <- 100*(pct_r-pct_b)
        pal <- make_pal_delta(delta_pp,scale=scale,accent=party_accent)
        lab <- paste0(hdr,"<br><br>",party_logo_inline(pick,"14px"),"<b>",pick,"</b>",
                      "<br>Base %: <b>",fmt_pct(pct_b),"</b>",
                      "<br>Ref %: <b>",fmt_pct(pct_r),"</b>",
                      "<br><b>\u0394:</b> ",fmt_signed_pp(delta_pp))
        proxy |>
          addPolygons(data=df,color="rgba(255,255,255,.22)",weight=1,
                      fillColor=pal$pal(pal$values),fillOpacity=opac,
                      label=lapply(lab,HTML),
                      highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE)) |>
          addLegend(position="bottomright",pal=pal$pal,values=pal$values,
                    title=HTML(paste0(party_logo_inline(pick,"13px"),"\u0394 ",pick," (%, pp)")),opacity=0.9)
      } else {
        delta <- votes_r-votes_b
        pal   <- make_pal_delta(delta,scale=scale,accent=party_accent)
        lab   <- paste0(hdr,"<br><br>",party_logo_inline(pick,"14px"),"<b>",pick,"</b>",
                        "<br>Base votos: <b>",fmt_int(votes_b),"</b>",
                        "<br>Ref votos: <b>",fmt_int(votes_r),"</b>",
                        "<br><b>\u0394:</b> ",fmt_signed_int(delta))
        proxy |>
          addPolygons(data=df,color="rgba(255,255,255,.22)",weight=1,
                      fillColor=pal$pal(pal$values),fillOpacity=opac,
                      label=lapply(lab,HTML),
                      highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE)) |>
          addLegend(position="bottomright",pal=pal$pal,values=pal$values,
                    title=HTML(paste0(party_logo_inline(pick,"13px"),"\u0394 ",pick," (votos)")),opacity=0.9)
      }
    }
    proxy <- add_dl_layer(proxy,dl_applied())
    proxy <- add_layers_control(proxy)
    proxy <- hideGroup(proxy,setdiff(names(BASEMAPS),BASEMAP_DEFAULT))
    bb    <- st_bbox(df)
    proxy |> fitBounds(bb["xmin"],bb["ymin"],bb["xmax"],bb["ymax"])
  })

  # ---- PAUTA ----
  output$buf_hereda_info <- renderUI({
    if (!has_applied()) return(div(class="status-badge idle", HTML("<b>Sin ejecutar</b> \u00b7 primero usa GENERAR")))
    div(class="status-badge ok",
        HTML(paste0("<b>",key_label(applied()$election),"</b>",
                    " \u00b7 <b>",nrow(df_applied()),"</b> secciones")))
  })

  output$ui_buf_party <- renderUI({
    validate(need(has_applied(),"Presiona GENERAR"))
    df  <- df_applied(); key <- applied()$election
    gv  <- group_votes_matrix(df,key,"DISTRIBUIDO"); G <- gv$G
    validate(need(!is.null(G)&&ncol(G)>0L,"Sin votos distribuidos"))
    tot     <- colSums(G,na.rm=TRUE)
    parties <- sort(unique(setdiff(names(tot)[is.finite(tot)&tot>0],"OTROS")))
    validate(need(length(parties)>0L,"Sin partidos disponibles"))
    def <- if ("PRI" %in% parties) "PRI" else parties[1L]
    # Selector con logos en los labels
    ch_labels <- vapply(parties,function(p) {
      logo <- party_logo_inline(p,"14px")
      paste0(logo,p)
    },character(1))
    selectizeInput("buf_party","Partido objetivo",choices=as.list(setNames(parties,parties)),selected=def)
  })

  output$ui_buf_inegi <- renderUI({
    validate(need(has_applied(),"Presiona GENERAR"))
    validate(need(length(INEGI_VARS)>0L,"No detect\u00e9 columnas *_INEGI"))
    if (NROW(TRADUCTOR)>0L && !is.null(input$buf_eje) && nzchar(input$buf_eje)) {
      d0 <- TRADUCTOR[TRADUCTOR$Eje==input$buf_eje,,drop=FALSE]
      if (NROW(d0)>0L) {
        choices <- as.list(setNames(d0$VARIABLE,paste0(d0$Indicador," (",d0$VARIABLE,")")))
        return(selectInput("buf_inegi_var","Indicador",choices=choices,selected=d0$VARIABLE[1L]))
      }
    }
    selectInput("buf_inegi_var","Indicador",choices=as.list(setNames(INEGI_VARS,INEGI_VARS)),selected=INEGI_VARS[1L])
  })

  # ---- UI Generacional ----
  # Catálogo generacional para PAUTA (solo columnas absolutas sumables)
  GEN_PAUTA_ABS <- list(
    "Gen Z"           = list(col="LISTA_GEN_Z",           col_pct="PCT_LISTA_GEN_Z",           icon="\U0001F4F1", color="#2EAD4A"),
    "Millennials"     = list(col="LISTA_MILLENNIALS",     col_pct="PCT_LISTA_MILLENNIALS",     icon="\U0001F4BB", color="#FF6A00"),
    "Gen X"           = list(col="LISTA_GEN_X",           col_pct="PCT_LISTA_GEN_X",           icon="\U0001F4FC", color="#FFD200"),
    "Boomers"         = list(col="LISTA_BOOMERS",         col_pct="PCT_LISTA_BOOMERS",         icon="\U0001F399", color="#D50000"),
    "Adultos mayores" = list(col="LISTA_ADULTOS_MAYORES", col_pct="PCT_LISTA_ADULTOS_MAYORES", icon="\U0001F9D3", color="#9B1B72")
  )

  output$ui_buf_gen <- renderUI({
    # Solo mostrar generaciones que tengan columna en el sf
    avail <- Filter(function(g) g$col %in% names(sf_all), GEN_PAUTA_ABS)
    validate(need(length(avail)>0L,"No se detectaron columnas generacionales (LISTA_GEN_Z, LISTA_MILLENNIALS, etc.)"))

    cols_abs <- setNames(vapply(avail,`[[`,character(1),"col"), names(avail))

    # Estimar % sobre lista total para mostrar en el label
    df_est <- as.data.frame(st_drop_geometry(sf_all))
    total_ln <- if ("LISTA_NOMINAL" %in% names(df_est)) sum(as_num(df_est[["LISTA_NOMINAL"]]),na.rm=TRUE) else NA_real_

    ch_labels <- vapply(names(avail), function(nm) {
      g   <- avail[[nm]]
      n   <- if (g$col %in% names(df_est)) sum(as_num(df_est[[g$col]]),na.rm=TRUE) else NA_real_
      pct <- if (is.finite(n)&&is.finite(total_ln)&&total_ln>0) paste0(" · ",formatC(100*n/total_ln,format="f",digits=1),"%") else ""
      paste0(g$icon," ",nm,pct)
    }, character(1))

    tagList(
      div(class="smallHelp","Selecciona una o varias generaciones (se suman):"),
      div(style="height:4px;"),
      checkboxGroupInput(
        "buf_gen_vars", NULL,
        choiceNames  = lapply(seq_along(avail), function(i) {
          nm <- names(avail)[i]; g <- avail[[nm]]
          n  <- if (g$col %in% names(df_est)) sum(as_num(df_est[[g$col]]),na.rm=TRUE) else NA_real_
          pct_txt <- if (is.finite(n)&&is.finite(total_ln)&&total_ln>0) paste0(" (",formatC(100*n/total_ln,format="f",digits=1),"%)") else ""
          div(style=paste0("color:#FFFFFF;"),
              span(g$icon, style=paste0("margin-right:5px;")),
              span(nm, style=paste0("font-weight:700;color:",g$color,";")),
              span(paste0(" ",fmt_int(n),pct_txt), style="font-size:11px;color:rgba(255,255,255,.80);")
          )
        }),
        choiceValues = as.list(unname(cols_abs)),
        selected     = intersect(c("LISTA_GEN_Z","LISTA_MILLENNIALS"), unname(cols_abs))
      ),
      div(style="height:8px;"),
      div(class="smallHelp", style="font-style:italic;",
          HTML("\u26a1 La optimizaci\u00f3n maximiza la <b>suma de electores</b> de las generaciones seleccionadas dentro del radio.")),
      div(style="height:6px;"),
      uiOutput("buf_gen_preview")
    )
  })

  output$buf_gen_preview <- renderUI({
    req(has_applied())
    sel_cols <- input$buf_gen_vars %||% character(0)
    if (length(sel_cols)==0L) return(div(class="pill", HTML("\u26a0\ufe0f Selecciona al menos una generaci\u00f3n")))
    df_e <- as.data.frame(st_drop_geometry(df_applied()))
    valid_cols <- sel_cols[sel_cols %in% names(df_e)]
    if (length(valid_cols)==0L) return(div(class="pill", HTML("\u26a0\ufe0f Columnas no encontradas en datos")))

    total_gen <- sum(vapply(valid_cols, function(col) {
      v <- as_num(df_e[[col]]); sum(v[is.finite(v)], na.rm=TRUE)
    }, numeric(1)))

    total_ln  <- if ("LISTA_NOMINAL" %in% names(df_e)) sum(as_num(df_e[["LISTA_NOMINAL"]]),na.rm=TRUE) else NA_real_
    pct_txt   <- if (is.finite(total_ln)&&total_ln>0) paste0(" (",formatC(100*total_gen/total_ln,format="f",digits=1),"% LN)") else ""

    # Nombres display
    gen_names <- names(Filter(function(g) g$col %in% valid_cols, GEN_PAUTA_ABS))
    lbl <- if (length(gen_names)==1L) gen_names else paste0(gen_names,collapse=" + ")

    div(class="pill",
        HTML(paste0("\U0001F465 <b>",lbl,"</b>: ",fmt_int(total_gen),pct_txt)))
  })

  buf_metric_info <- reactive({
    validate(need(has_applied(),"Presiona GENERAR"))
    df   <- df_applied()
    mode <- input$buf_mode %||% "electoral"

    # ---- Electoral ----
    if (identical(mode,"electoral")) {
      key   <- applied()$election
      gv    <- group_votes_matrix(df,key,"DISTRIBUIDO"); G <- gv$G
      validate(need(!is.null(G)&&ncol(G)>0L,"Sin matriz electoral"))
      party <- input$buf_party %||% colnames(G)[1L]
      if (!(party %in% colnames(G))) party <- colnames(G)[1L]
      vals  <- as.numeric(G[,party]); vals[!is.finite(vals)] <- 0
      return(list(mode="electoral",label=party,short_lbl=party,values=vals,color=fill_color_winner(party)))
    }

    # ---- Generacional ----
    if (identical(mode,"gen")) {
      sel_cols <- input$buf_gen_vars %||% character(0)
      validate(need(length(sel_cols)>0L,"Selecciona al menos una generaci\u00f3n"))
      df_plain <- as.data.frame(st_drop_geometry(df))
      valid_cols <- sel_cols[sel_cols %in% names(df_plain)]
      validate(need(length(valid_cols)>0L,"Columnas generacionales no encontradas en datos"))

      # Suma ponderada de generaciones seleccionadas
      M <- matrix(0, nrow=nrow(df_plain), ncol=length(valid_cols))
      for (j in seq_along(valid_cols)) {
        v <- as_num(df_plain[[valid_cols[j]]]); v[!is.finite(v)] <- 0; M[,j] <- v
      }
      vals <- rowSums(M, na.rm=TRUE)
      vals[!is.finite(vals)] <- 0

      # Nombre display
      gen_names <- names(Filter(function(g) g$col %in% valid_cols, GEN_PAUTA_ABS))
      lbl <- if (length(gen_names)==0L) paste(valid_cols,collapse="+") else paste(gen_names,collapse=" + ")
      col_accent <- if (length(gen_names)==1L) {
        GEN_PAUTA_ABS[[gen_names[1L]]]$color %||% ACCENT
      } else "#00B5E2"

      return(list(mode="gen", label=lbl, short_lbl=lbl, values=vals, color=col_accent))
    }

    # ---- INEGI ----
    validate(need(length(INEGI_VARS)>0L,"No hay columnas INEGI"))
    var_nm <- input$buf_inegi_var %||% INEGI_VARS[1L]
    validate(need(var_nm %in% INEGI_VARS,"Indicador INEGI inv\u00e1lido"))
    col_nm <- INEGI_COL_MAP[[var_nm]]
    validate(need(!is.null(col_nm)&&col_nm %in% names(df),"No encontr\u00e9 la columna INEGI"))
    vals    <- as_num(as.data.frame(st_drop_geometry(df))[[col_nm]]); vals[!is.finite(vals)] <- 0
    nice_lbl <- var_nm
    if (NROW(TRADUCTOR)>0L) { idx <- match(var_nm,TRADUCTOR$VARIABLE); if (!is.na(idx)) nice_lbl <- TRADUCTOR$Indicador[idx] }
    list(mode="inegi", label=nice_lbl, short_lbl=var_nm, values=vals, color=ACCENT)
  })

  observeEvent(input$buf_generate, {
    req(has_applied())
    cfg <- list(
      mode=input$buf_mode %||% "electoral",
      party=input$buf_party %||% NULL,
      gen_vars=input$buf_gen_vars %||% character(0),
      eje=input$buf_eje %||% NULL,
      inegi_var=input$buf_inegi_var %||% NULL,
      cover_pct=as.numeric(input$buf_cover_pct %||% 80),
      radius_m=as.numeric(input$buf_radius_m %||% 800),
      max_points=as.integer(input$buf_max_points %||% 3),
      google_api_key=GOOGLE_API_KEY, ts=Sys.time()
    )
    buf_applied(cfg)
    showNotification("PAUTA optimizada \u2705", type="message", duration=1.2)
  }, ignoreInit=TRUE)

  has_buf <- reactive({ x <- buf_applied(); !is.null(x)&&!is.null(x$ts) })

  output$buf_status <- renderUI({
    if (!has_buf()) {
      div(class="status-badge idle",
          HTML("<b>Sin optimizar</b> \u00b7 configura y presiona Optimizar"))
    } else {
      x <- buf_applied()
      mode_lbl <- switch(x$mode %||% "electoral",
        gen      = "\U0001F4F1 Generacional",
        inegi    = "\U0001F4CA Poblaci\u00f3n",
        "\U0001F3C6 Electoral")
      div(class="status-badge ok",
          HTML(paste0("<b>Optimizado</b> \u00b7 ", format(x$ts,"%H:%M:%S"),
                      " \u00b7 ", mode_lbl)))
    }
  })

  buf_calc <- reactive({
    validate(need(has_applied(),"Presiona GENERAR"))
    validate(need(has_buf(),"Presiona Optimizar"))
    df <- df_applied(); validate(need(nrow(df)>0L,"Sin datos en la selecci\u00f3n"))
    info <- buf_metric_info(); cfg <- buf_applied()
    metric_vals <- as_num(info$values); metric_vals[!is.finite(metric_vals)] <- 0; metric_vals <- pmax(metric_vals,0)
    validate(need(sum(metric_vals,na.rm=TRUE)>0,"La variable objetivo suma 0"))
    ctr_4326 <- safe_centroids_wgs84(df)
    ctr_3857 <- tryCatch(st_transform(ctr_4326,3857),error=function(e) ctr_4326)
    xy_m <- st_coordinates(ctr_3857); xy_w <- st_coordinates(ctr_4326)
    n       <- nrow(df); dist_m <- as.matrix(stats::dist(xy_m)); cover_mat <- dist_m <= cfg$radius_m
    chosen  <- integer(0); covered <- rep(FALSE,n)
    target_cover_value <- sum(metric_vals,na.rm=TRUE)*(cfg$cover_pct/100)
    while (length(chosen)<cfg$max_points && sum(metric_vals[covered],na.rm=TRUE)+1e-9<target_cover_value) {
      remaining <- setdiff(seq_len(n),chosen); if (!length(remaining)) break
      gains     <- vapply(remaining, function(i) sum(metric_vals[cover_mat[i,]&!covered],na.rm=TRUE), numeric(1))
      best_pos  <- which.max(gains); best_idx <- remaining[best_pos]
      if (!is.finite(gains[best_pos])||gains[best_pos]<=0) break
      chosen  <- c(chosen,best_idx); covered <- covered|cover_mat[best_idx,]
    }
    if (length(chosen)==0L) { chosen <- which.max(metric_vals)[1L]; covered <- cover_mat[chosen,] }
    api_key   <- cfg$google_api_key %||% ""
    addresses <- rep(NA_character_,length(chosen))
    if (nzchar(api_key)) {
      withProgress(message="Geocodificando puntos...",value=0, {
        for (j in seq_along(chosen)) {
          i <- chosen[j]
          addresses[j] <- reverse_geocode(lat=as.numeric(xy_w[i,"Y"]),lng=as.numeric(xy_w[i,"X"]),api_key=api_key)
          incProgress(1/length(chosen))
        }
      })
    }
    key <- applied()$election; c_tot <- total_col(df,key); c_ln <- ln_col(df,key)
    rows <- lapply(seq_along(chosen), function(j) {
      i        <- chosen[j]; inside_i <- cover_mat[i,]
      tot_i    <- if (!is.na(c_tot)&&c_tot %in% names(df)) sum(as_num(df[[c_tot]][inside_i]),na.rm=TRUE) else NA_real_
      ln_i     <- if (!is.na(c_ln) &&c_ln  %in% names(df)) sum(as_num(df[[c_ln]][inside_i]), na.rm=TRUE) else NA_real_
      part_i   <- if (is.finite(tot_i)&&is.finite(ln_i)&&ln_i>0) tot_i/ln_i else NA_real_
      gv       <- group_votes_matrix(df[inside_i,],key,"DISTRIBUIDO"); G <- gv$G
      top_p    <- NA_character_
      if (!is.null(G)&&ncol(G)>0L) { tot_p <- colSums(G,na.rm=TRUE); tot_p <- tot_p[is.finite(tot_p)&tot_p>0]; if (length(tot_p)>0L) top_p <- names(sort(tot_p,decreasing=TRUE))[1L] }
      data.frame(PUNTO=j,SECCION_CENTRO=df$SECCION[i],SECCIONES_IMPACTADAS=sum(inside_i,na.rm=TRUE),
                 LNG=suppressWarnings(as.numeric(xy_w[i,"X"])),LAT=suppressWarnings(as.numeric(xy_w[i,"Y"])),
                 OBJETIVO=sum(metric_vals[inside_i],na.rm=TRUE),TOTAL_VOTOS=tot_i,LISTA_NOMINAL=ln_i,
                 PARTICIPACION=part_i,TOP_PARTIDO=top_p %||% NA_character_,DIRECCION=addresses[j] %||% NA_character_,
                 GOOGLE_MAPS=paste0("https://www.google.com/maps?q=",as.numeric(xy_w[i,"Y"]),",",as.numeric(xy_w[i,"X"])),
                 stringsAsFactors=FALSE)
    })
    tbl     <- rbindlist(rows,fill=TRUE)
    tot_all <- if (!is.na(c_tot)&&c_tot %in% names(df)) sum(as_num(df[[c_tot]][covered]),na.rm=TRUE) else NA_real_
    ln_all  <- if (!is.na(c_ln) &&c_ln  %in% names(df)) sum(as_num(df[[c_ln]][covered]), na.rm=TRUE) else NA_real_
    part_all <- if (is.finite(tot_all)&&is.finite(ln_all)&&ln_all>0) tot_all/ln_all else NA_real_
    total_row <- data.frame(PUNTO=NA_integer_,SECCION_CENTRO=NA_integer_,SECCIONES_IMPACTADAS=sum(covered,na.rm=TRUE),
                            LNG=NA_real_,LAT=NA_real_,OBJETIVO=sum(metric_vals[covered],na.rm=TRUE),
                            TOTAL_VOTOS=tot_all,LISTA_NOMINAL=ln_all,PARTICIPACION=part_all,
                            TOP_PARTIDO=NA_character_,DIRECCION="",GOOGLE_MAPS="",stringsAsFactors=FALSE)
    tbl <- rbind(tbl,total_row,fill=TRUE)
    list(df=df,covered=covered,chosen=chosen,xy_w=xy_w,metric_vals=metric_vals,
         metric_info=info,target_val=target_cover_value,
         covered_val=sum(metric_vals[covered],na.rm=TRUE),
         table=as.data.frame(tbl),addresses=addresses)
  })

  output$buf_kpi1 <- renderUI({
    if (!has_buf()) return(kpi_placeholder("Puntos"))
    z <- buf_calc(); cfg <- buf_applied()
    bar_w <- paste0(min(100*length(z$chosen)/max(cfg$max_points,1),100),"%")
    tagList(
      div(class="t","Puntos activos"),
      div(class="v", HTML(paste0(fmt_int(length(z$chosen)),
                                 " <span style='font-size:14px;font-weight:500;color:rgba(255,255,255,.65);'>/ ",cfg$max_points,"</span>"))),
      div(class="s", HTML(paste0("Radio: <b>",fmt_int(cfg$radius_m),"</b> m"))),
      div(class="kpi-bar-track", div(class="kpi-bar-fill",style=paste0("width:",bar_w,";")))
    )
  })
  output$buf_kpi2 <- renderUI({
    if (!has_buf()) return(kpi_placeholder("Secciones cubiertas"))
    z <- buf_calc()
    n_cov <- sum(z$covered,na.rm=TRUE); n_tot <- nrow(z$df)
    bar_w <- if (n_tot>0) paste0(min(100*n_cov/n_tot,100),"%") else "0%"
    tagList(
      div(class="t","Secciones cubiertas"),
      div(class="v", HTML(paste0(fmt_int(n_cov),
                                 " <span style='font-size:14px;font-weight:500;color:rgba(255,255,255,.65);'>/ ",fmt_int(n_tot),"</span>"))),
      div(class="s", HTML(paste0(formatC(100*n_cov/max(n_tot,1),format="f",digits=1),"% del total"))),
      div(class="kpi-bar-track", div(class="kpi-bar-fill",style=paste0("width:",bar_w,";")))
    )
  })
  output$buf_kpi3 <- renderUI({
    if (!has_buf()) return(kpi_placeholder("Cobertura objetivo"))
    z   <- buf_calc()
    pct <- if (z$target_val>0) z$covered_val/z$target_val else NA_real_
    bar_w <- if (is.finite(pct)) paste0(min(100*pct,100),"%") else "0%"
    tagList(
      div(class="t","Cobertura objetivo"),
      div(class="v", ifelse(is.na(pct),"\u2014",paste0(formatC(100*pct,format="f",digits=1),"%"))),
      div(class="s", HTML(paste0(fmt_int(z$covered_val)," / <b>",fmt_int(z$target_val),"</b>"))),
      div(class="kpi-bar-track", div(class="kpi-bar-fill",style=paste0("width:",bar_w,";")))
    )
  })
  output$buf_kpi4 <- renderUI({
    if (!has_buf()) return(kpi_placeholder("Cobertura total"))
    z         <- buf_calc()
    total_all <- sum(z$metric_vals,na.rm=TRUE)
    pct       <- if (total_all>0) z$covered_val/total_all else NA_real_
    accent_col <- z$metric_info$color %||% ACCENT
    icon_mode <- switch(z$metric_info$mode %||% "electoral",
                        gen      = "\U0001F465",
                        inegi    = "\U0001F4CA",
                        "\U0001F3AF")
    bar_w <- if (is.finite(pct)) paste0(min(100*pct,100),"%") else "0%"
    tagList(
      div(class="t","Cobertura total"),
      div(class="v", style=paste0("color:",accent_col,";"),
          ifelse(is.na(pct),"\u2014",paste0(formatC(100*pct,format="f",digits=1),"%"))),
      div(class="s", HTML(paste0(icon_mode," <b>",z$metric_info$label,"</b>"))),
      div(class="kpi-bar-track",
          div(class="kpi-bar-fill", style=paste0("width:",bar_w,";background:",accent_col,";")))
    )
  })

  output$map_buf <- renderLeaflet({
    m <- leaflet(options=leafletOptions(preferCanvas=TRUE)) |>
      addProviderTiles(BASEMAPS[["Oscuro"]],    group="Oscuro")   |>
      addProviderTiles(BASEMAPS[["Claro"]],     group="Claro")    |>
      addProviderTiles(BASEMAPS[["Calles"]],    group="Calles")   |>
      addProviderTiles(BASEMAPS[["Sat\u00e9lite"]],group="Sat\u00e9lite") |>
      add_dl_layer() |> add_layers_control() |> hideGroup(DL_GROUP) |>
      addScaleBar(position="bottomleft") |>
      setView(lng=INIT_LNG,lat=INIT_LAT,zoom=INIT_ZOOM)
    if (has_fullscreen) m <- leaflet.extras::addFullscreenControl(m,position="topleft",pseudoFullscreen=FALSE)
    m
  })

  observe({
    req(has_buf())
    z       <- buf_calc(); df <- z$df; covered <- z$covered; chosen <- z$chosen; xy_w <- z$xy_w
    rad     <- buf_applied()$radius_m; addresses <- z$addresses
    proxy   <- leafletProxy("map_buf",session=session) |> clearShapes() |> clearMarkers() |> clearControls()
    proxy   <- restore_map_controls(proxy)
    base_lab <- paste0("<b style='color:",ACCENT,"'>Secci\u00f3n:</b> <b>",df$SECCION,"</b>",
                       "<br>Cubierta: <b>",ifelse(covered,"S\u00ed","No"),"</b>")
    if (any(!covered)) proxy <- proxy |> addPolygons(data=df[!covered,],color="rgba(255,255,255,.12)",weight=1,
                                                     fillColor="#111827",fillOpacity=0.18,label=lapply(base_lab[!covered],HTML))
    if (any(covered))  proxy <- proxy |> addPolygons(data=df[covered,], color="rgba(255,255,255,.18)",weight=1.2,
                                                     fillColor=ACCENT,fillOpacity=0.42,label=lapply(base_lab[covered],HTML),
                                                     highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE))
    if (length(chosen)>0L) {
      point_popups <- vapply(seq_along(chosen), function(j) {
        addr_html <- if (!is.na(addresses[j])&&nzchar(addresses[j])) paste0("<br><b>Direcci\u00f3n:</b> ",htmltools::htmlEscape(addresses[j])) else ""
        gmaps_link <- paste0("https://www.google.com/maps?q=",xy_w[chosen[j],"Y"],",",xy_w[chosen[j],"X"])
        paste0("<div style='color:#FFFFFF;'>",
               "<b style='color:",ACCENT,"'>Punto ",j,"</b>",
               "<br>Secci\u00f3n centro: <b>",df$SECCION[chosen[j]],"</b>",
               addr_html,
               "<br><a href='",gmaps_link,"' target='_blank' style='color:",ACCENT,";'>\U0001F4CD Abrir en Google Maps</a>",
               "</div>")
      },character(1))
      proxy <- proxy |>
        addCircles(lng=xy_w[chosen,"X"],lat=xy_w[chosen,"Y"],radius=rad,
                   color=ACCENT,weight=2.5,fill=FALSE,opacity=0.9) |>
        addCircleMarkers(lng=xy_w[chosen,"X"],lat=xy_w[chosen,"Y"],radius=8,
                         color="#FFFFFF",weight=2,fillColor=ACCENT,fillOpacity=1,
                         popup=point_popups,
                         label=lapply(seq_along(chosen),function(j) {
                           addr_lbl <- if (!is.na(addresses[j])&&nzchar(addresses[j])) paste0("<br><span style='font-size:11px;color:#FFFFFF'>",htmltools::htmlEscape(addresses[j]),"</span>") else ""
                           HTML(paste0("<b>Punto ",j,"</b> \u00b7 Secc ",df$SECCION[chosen[j]],addr_lbl))
                         }))
    }
    proxy <- add_dl_layer(proxy,dl_applied())
    proxy <- add_layers_control(proxy)
    proxy <- hideGroup(proxy,setdiff(names(BASEMAPS),BASEMAP_DEFAULT))
    bb    <- st_bbox(df)
    proxy |> fitBounds(bb["xmin"],bb["ymin"],bb["xmax"],bb["ymax"])
  })

  output$buf_compare_bar <- renderPlotly({
    validate(need(has_buf(),"Presiona Optimizar"))
    z  <- buf_calc()
    accent_col <- z$metric_info$color %||% ACCENT
    val_inside <- sum(z$metric_vals[z$covered],na.rm=TRUE)
    val_outside<- sum(z$metric_vals[!z$covered],na.rm=TRUE)
    val_total  <- val_inside + val_outside
    pct_inside <- if (val_total>0) 100*val_inside/val_total else NA_real_

    plot_ly(
      labels = c(paste0("Dentro \u2713 · ",z$metric_info$label),
                 paste0("Fuera \u2715")),
      values = c(val_inside, val_outside),
      type   = "pie",
      hole   = 0.60,
      marker = list(
        colors = c(accent_col, "#1F2937"),
        line   = list(color = "rgba(255,255,255,.10)", width = 1.5)
      ),
      textinfo      = "none",
      hovertemplate = paste0("<b>%{label}</b><br>",
                             fmt_int(val_inside)," / ",fmt_int(val_total),
                             "<extra></extra>"),
      sort          = FALSE
    ) |>
    layout(
      annotations = list(list(
        text      = if (is.finite(pct_inside)) paste0(formatC(pct_inside,format="f",digits=1),"%") else "\u2014",
        x         = 0.5, y = 0.5, xanchor="center", yanchor="middle",
        showarrow = FALSE,
        font      = list(size=28, color="#FFFFFF", family="Inter, system-ui",
                         weight=900)
      )),
      showlegend  = TRUE,
      legend      = list(orientation="h", x=0, y=-0.12, font=list(color="#FFFFFF",size=11)),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
      margin        = list(l=10,r=10,b=30,t=10)
    ) |>
    config(displayModeBar=FALSE, responsive=TRUE)
  })

  output$buf_party_bar <- renderPlotly({
    validate(need(has_buf(),"Presiona Optimizar"))
    validate(need(has_applied(),"Presiona GENERAR"))
    z <- buf_calc(); df <- z$df; key <- applied()$election
    gv <- group_votes_matrix(df,key,"DISTRIBUIDO"); G <- gv$G
    validate(need(!is.null(G)&&ncol(G)>0L,"Sin votos distribuidos"))
    tot <- colSums(G[z$covered,,drop=FALSE],na.rm=TRUE)
    tot <- tot[is.finite(tot)&tot>0]
    if ("OTROS" %in% names(tot)&&length(tot)>1L) tot <- tot[names(tot)!="OTROS"]
    tot <- sort(tot,decreasing=TRUE)
    validate(need(length(tot)>0L,"Sin partidos dentro"))
    dd  <- data.frame(party=names(tot)[seq_len(min(8L,length(tot)))],votos=as.numeric(tot[seq_len(min(8L,length(tot)))]),stringsAsFactors=FALSE)
    dd$party <- factor(dd$party,levels=rev(dd$party))
    cols_bar <- vapply(as.character(dd$party),function(p) if (p %in% names(party_colors)) unname(party_colors[[p]]) else ACCENT, character(1))
    p <- plot_ly(dd,x=~votos,y=~party,type="bar",orientation="h",
                 marker=list(color=cols_bar,
                             line=list(color="rgba(255,255,255,.15)",width=0.7),
                             opacity=0.92),
                 text=~paste0("<b>",as.character(party),"</b><br>Votos: <b>",fmt_int(votos),"</b>"),
                 hoverinfo="text") |>
      layout(xaxis=list(title="Votos",color="#FFFFFF",gridcolor="rgba(255,255,255,.07)",
                        zeroline=FALSE),
             yaxis=list(title="",showticklabels=FALSE,ticklen=0,color="#FFFFFF"),
             hoverlabel=list(
               bgcolor="rgba(5,8,14,.95)",
               bordercolor="rgba(255,255,255,.18)",
               font=list(color="#FFFFFF",size=12,family="Inter, system-ui, sans-serif")
             ),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
             font=list(color="#FFFFFF",family="Inter, system-ui, sans-serif"),
             showlegend=FALSE,margin=list(l=55,r=20,b=40,t=10)) |>
      config(displayModeBar=FALSE,responsive=TRUE)
    add_logos_to_plotly_h(p, levels(dd$party), size=0.060)
  })

  output$buf_tbl <- renderDT({
    validate(need(has_buf(),"Presiona Optimizar"))
    d <- as.data.frame(buf_calc()$table); validate(need(NROW(d)>0,"Sin puntos"))
    is_total_row <- if ("PUNTO" %in% names(d)) is.na(d$PUNTO) else rep(FALSE,NROW(d))

    # ---- Enriquecer columnas ----
    if ("TOP_PARTIDO" %in% names(d)) {
      d$TOP_PARTIDO <- vapply(d$TOP_PARTIDO, function(p) {
        if (is.na(p)||!nzchar(p)) return("\u2014")
        paste0(party_logo_inline(p,"17px"),"<b>",p,"</b>")
      }, character(1))
    }
    if ("DIRECCION" %in% names(d)) {
      d$DIRECCION <- vapply(seq_len(NROW(d)), function(i) {
        a <- d$DIRECCION[i]
        if (is_total_row[i]) return("<span style='color:rgba(255,255,255,.45)'>TOTAL</span>")
        if (is.na(a)||!nzchar(trimws(a))) return("<span style='color:rgba(255,255,255,.45)'>\u2014 Sin direcci\u00f3n</span>")
        paste0("<span class='addr-cell'>",htmltools::htmlEscape(a),"</span>")
      }, character(1))
    }
    if ("GOOGLE_MAPS" %in% names(d)) {
      d$GOOGLE_MAPS <- vapply(seq_len(NROW(d)), function(i) {
        url <- d$GOOGLE_MAPS[i]
        if (is_total_row[i]) return("<span style='color:rgba(255,255,255,.35)'>\u2014</span>")
        if (is.na(url)||!nzchar(trimws(url))) return("\u2014")
        paste0(
          "<a href='",url,"' target='_blank' ",
          "style='display:inline-flex;align-items:center;gap:5px;",
          "background:rgba(213,0,0,.18);border:1px solid rgba(213,0,0,.35);",
          "color:#FFFFFF;font-size:11px;font-weight:700;padding:3px 9px;",
          "border-radius:999px;text-decoration:none;white-space:nowrap;'>",
          "\U0001F4CD Maps</a>"
        )
      }, character(1))
    }

    # ---- Reordenar columnas: DIRECCION y GOOGLE_MAPS van después de TOP_PARTIDO, LNG/LAT al final ----
    desired_order <- c("PUNTO","SECCION_CENTRO","SECCIONES_IMPACTADAS","TOP_PARTIDO",
                       "OBJETIVO","TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION",
                       "DIRECCION","GOOGLE_MAPS","LNG","LAT")
    present_cols <- intersect(desired_order, names(d))
    extra_cols   <- setdiff(names(d), desired_order)
    d <- d[, c(present_cols, extra_cols), drop=FALSE]

    # ---- Headers amigables ----
    header_labels <- c(
      PUNTO               = "#",
      SECCION_CENTRO      = "Secci\u00f3n",
      SECCIONES_IMPACTADAS= "Secc. impactadas",
      TOP_PARTIDO         = "Partido lider",
      OBJETIVO            = "Objetivo",
      TOTAL_VOTOS         = "Total votos",
      LISTA_NOMINAL       = "Lista nominal",
      PARTICIPACION       = "Part. %",
      DIRECCION           = "Direcci\u00f3n",
      GOOGLE_MAPS         = "Maps",
      LNG                 = "LNG",
      LAT                 = "LAT"
    )
    headers <- vapply(names(d), function(nm) {
      lbl <- header_labels[nm] %||% nm
      if (nm == "OBJETIVO") {
        p_lbl <- buf_metric_info()$short_lbl
        lbl <- paste0(party_logo_inline(p_lbl,"14px"),"<b>",p_lbl,"</b>")
      }
      lbl
    }, character(1))

    header_cb <- htmltools::withTags(table(
      class="display",
      thead(tr(lapply(headers, function(h) th(HTML(h)))))
    ))

    # Índices 0-based de LNG y LAT para ocultarlos por defecto
    lng_idx <- which(names(d) == "LNG") - 1L
    lat_idx <- which(names(d) == "LAT") - 1L
    hidden_cols <- as.list(c(lng_idx, lat_idx))

    dt <- datatable(
      d, rownames=FALSE, filter="top",
      container=header_cb, escape=FALSE,
      extensions=c("Buttons"),
      options=list(
        dom="Bfrtip", scrollX=TRUE, pageLength=25,
        buttons=list(
          list(extend="colvis", text="\U0001F441 Columnas"),
          "copy","csv","excel"
        ),
        columnDefs=list(list(visible=FALSE, targets=hidden_cols)),
        initComplete=dt_dark_init
      )
    )

    num_cols <- intersect(c("SECCIONES_IMPACTADAS","OBJETIVO","TOTAL_VOTOS","LISTA_NOMINAL"), names(d))
    if (length(num_cols)>0L) dt <- dt |> formatRound(num_cols, digits=0)
    if ("PARTICIPACION" %in% names(d)) dt <- dt |> formatPercentage("PARTICIPACION", digits=2)
    dt
  })

  output$download_buf_csv <- downloadHandler(
    filename=function() paste0("pauta_atizapan_",format(Sys.time(),"%Y%m%d_%H%M"),".csv"),
    content =function(file) write.csv(as.data.frame(buf_calc()$table),file,row.names=FALSE,fileEncoding="UTF-8")
  )

  outputOptions(output,"map_time",suspendWhenHidden=FALSE)
  outputOptions(output,"map_buf", suspendWhenHidden=FALSE)
}

shinyApp(ui, server)
