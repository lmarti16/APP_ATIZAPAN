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

# Helper to create a base leaflet map (used by all 3 map outputs)
create_base_leaflet <- function() {
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
}
