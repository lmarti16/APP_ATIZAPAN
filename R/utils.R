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

# DT helper dark init
dt_dark_init <- JS("function(settings, json) {
  $(this.api().table().container()).css({'color':'#FFFFFF','background':'transparent'});
}")

# Plotly dark layout defaults
plotly_dark_layout <- list(
  paper_bgcolor = "rgba(0,0,0,0)",
  plot_bgcolor  = "rgba(0,0,0,0)",
  font          = list(color="#FFFFFF"),
  xaxis         = list(color="#FFFFFF", gridcolor="rgba(255,255,255,.10)", zerolinecolor="rgba(255,255,255,.15)"),
  yaxis         = list(color="#FFFFFF", gridcolor="rgba(255,255,255,.10)")
)

# KPI placeholder (skeleton loader)
kpi_placeholder <- function(t, s="Presiona GENERAR") {
  tagList(
    div(class="t", t),
    div(class="skeleton skeleton-val"),
    div(class="skeleton skeleton-sub"),
    div(class="skeleton skeleton-sub2")
  )
}
