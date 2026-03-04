# ==========================================================
# MODULO: LEEME (UI only)
# ==========================================================

leeme_ui <- function() {
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

      # ---- Tips ----
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
                    div(style="font-size:12px; color:rgba(255,255,255,.82); line-height:1.6;", tip[[2]])
                )
              })
          )
      )
    )
  )
}
