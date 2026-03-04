# electrend - Atizapan de Zaragoza, EDOMEX

Explorador electoral interactivo por seccion para el municipio de Atizapan de Zaragoza, Estado de Mexico. Aplicacion construida con R/Shiny.

## Funcionalidades

- **Explorar**: Mapa interactivo (Leaflet) con vista de ganador, participacion, lista nominal, electorado y choropleth por partido.
- **Tabla de resultados**: Resultados por seccion con votos distribuidos, puros y por candidatura.
- **Series de tiempo**: Evolucion historica de votacion por partido, participacion, lista nominal y metricas electorales entre elecciones.
- **Pauta / Buffers**: Optimizacion de cobertura territorial con modos electoral, poblacional (INEGI) y generacional.
- **Descarga de datos**: Exportacion a CSV de resultados filtrados y series de partidos.

## Requisitos

- R >= 4.1
- Paquetes: `shiny`, `data.table`, `sf`, `leaflet`, `bslib`, `plotly`, `DT`, `stringr`, `htmltools`, `dplyr`, `shinymanager`
- Paquetes opcionales: `leaflet.extras`, `shinycssloaders`, `readxl`

## Estructura

```
APP_ATIZAPAN/
  APP.R        # Aplicacion completa (UI + Server)
  data/        # CSV base y GPKG de secciones electorales
  www/         # Logos de partidos e imagenes
  .Renviron    # Variables de entorno (API keys)
```

## Ejecucion

```r
shiny::runApp("APP.R")
```

## Variables de entorno

Configurar en `.Renviron`:

```
GOOGLE_API_KEY=...
OPENAI_API_KEY=...
AWS_ACCESS_KEY_ID=...
AWS_SECRET_ACCESS_KEY=...
AWS_DEFAULT_REGION=...
```
