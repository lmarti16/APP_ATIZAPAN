# ==========================================================
# CREDENCIALES
# ==========================================================

GOOGLE_API_KEY      <- Sys.getenv("GOOGLE_API_KEY", unset = "")
OPENAI_API_KEY      <- Sys.getenv("OPENAI_API_KEY", unset = "")
AWS_ACCESS_KEY_ID   <- Sys.getenv("AWS_ACCESS_KEY_ID", unset = "")
AWS_SECRET_ACCESS_KEY <- Sys.getenv("AWS_SECRET_ACCESS_KEY", unset = "")
AWS_DEFAULT_REGION  <- Sys.getenv("AWS_DEFAULT_REGION", unset = "")

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

ACCENT  <- "#E60012"
ACCENT2 <- "#B80010"
BG      <- "#0D0808"
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
  party <- as.character(party %||% "")[1L]
  party <- toupper(trimws(party))
  if (!nzchar(party) || is.na(party)) return(NULL)

  if (party %in% names(party_logo_map)) return(unname(party_logo_map[[party]]))

  party_under  <- gsub("[[:space:]]+", "_", party)
  party_nospace <- gsub("[[:space:]]+", "", party)
  for (p2 in c(party_under, party_nospace)) {
    if (p2 %in% names(party_logo_map)) return(unname(party_logo_map[[p2]]))
  }

  parts <- unlist(strsplit(party, "[-+/|&,]"))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  for (pt in parts) {
    if (pt %in% names(party_logo_map)) return(unname(party_logo_map[[pt]]))
    for (p in names(party_logo_map)) {
      if (grepl(paste0("\\b", p, "\\b"), pt)) return(unname(party_logo_map[[p]]))
    }
  }

  known <- names(party_logo_map)
  known <- known[order(-nchar(known))]
  for (p in known) {
    if (grepl(paste0("\\b", p, "\\b"), party) || grepl(p, party, fixed = TRUE)) {
      return(unname(party_logo_map[[p]]))
    }
  }

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

# ---- Logos en plotly horizontal ----
add_logos_to_plotly_h <- function(p, party_labels, size = 0.055, gap = NULL) {
  n <- length(party_labels)
  if (n == 0L) return(p)

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
      x       = -0.010,
      y       = y_pos,
      xref    = "paper",
      yref    = "paper",
      xanchor = "right",
      yanchor = "middle",
      sizex   = size,
      sizey   = size,
      layer   = "above"
    )
  }
  if (length(images) > 0L) p <- p |> layout(images = images)
  p
}

# ==========================================================
# ELECTORADO CATALOG
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

# Generational catalog for PAUTA
GEN_PAUTA_ABS <- list(
  "Gen Z"           = list(col="LISTA_GEN_Z",           col_pct="PCT_LISTA_GEN_Z",           icon="\U0001F4F1", color="#2EAD4A"),
  "Millennials"     = list(col="LISTA_MILLENNIALS",     col_pct="PCT_LISTA_MILLENNIALS",     icon="\U0001F4BB", color="#FF6A00"),
  "Gen X"           = list(col="LISTA_GEN_X",           col_pct="PCT_LISTA_GEN_X",           icon="\U0001F4FC", color="#FFD200"),
  "Boomers"         = list(col="LISTA_BOOMERS",         col_pct="PCT_LISTA_BOOMERS",         icon="\U0001F399", color="#D50000"),
  "Adultos mayores" = list(col="LISTA_ADULTOS_MAYORES", col_pct="PCT_LISTA_ADULTOS_MAYORES", icon="\U0001F9D3", color="#9B1B72")
)
