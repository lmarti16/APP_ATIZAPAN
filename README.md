# APP_ATIZAPAN

Aplicación Shiny para visualización y análisis de resultados electorales del municipio de Atizapán de Zaragoza, Estado de México.

---

## Características

- **Mapa electoral interactivo** — choropleth por sección, ganador, participación, lista nominal y variables del electorado.
- **Serie de tiempo** — evolución de votos y métricas por elección (AYU y DL).
- **Análisis comparativo** — delta entre elecciones de referencia y base.
- **PAUTA** — optimización geoespacial de puntos de activación (electoral, INEGI, generacional).
- **Descarga de datos** — exportación CSV de tablas y series.

---

## Requisitos

- R ≥ 4.2
- Paquetes principales: `shiny`, `bslib`, `leaflet`, `plotly`, `DT`, `data.table`, `sf`, `jsonlite`, `shinycssloaders`

Instala dependencias con:

```r
install.packages(c(
  "shiny", "bslib", "leaflet", "plotly", "DT",
  "data.table", "sf", "jsonlite", "shinycssloaders"
))
```

---

## Ejecución

```r
shiny::runApp("APP.R")
```

O directamente desde la terminal:

```bash
Rscript -e "shiny::runApp('APP.R', port=6953)"
```

---

## Estructura del proyecto

```
APP_ATIZAPAN/
├── APP.R        # Aplicación Shiny completa (UI + Server)
└── README.md    # Este archivo
```

Los datos geoespaciales y electorales se cargan dinámicamente desde archivos CSV / GeoPackage / RDS definidos al inicio de `APP.R`.

---

## Notas técnicas

### Corrección de advertencia jsonlite

Las opciones de los controles Shiny (`selectInput`, `radioButtons`, etc.) se pasan como **listas nombradas** (`list(...)`) en lugar de vectores nombrados (`c(...)`), y las variables globales `ELECTION_CHOICES` y `ELECTORADO_CHOICES` se definen con `as.list(setNames(...))`.

Esto elimina la advertencia de `jsonlite`:

```
Input to asJSON(keep_vec_names=TRUE) is a named vector.
In a future version of jsonlite, this option will not be supported...
```

---

## Licencia

Uso interno — Municipio de Atizapán de Zaragoza.
