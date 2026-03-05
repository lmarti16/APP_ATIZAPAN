# ==========================================================
# MODULO: COMPETITIVIDAD — Análisis por sección
# ==========================================================

competitividad_ui <- function() {
  bslib::nav_panel(
    HTML("<span class='nav-icon'>\u2694\uFE0F</span>Competitividad"),
    div(style="padding:12px;",

        # ---- KPIs ----
        div(class="kpiRow", style="margin-bottom:14px;",
            div(class="glass kpi", uiOutput("comp_kpi_avg")),
            div(class="glass kpi", uiOutput("comp_kpi_battle")),
            div(class="glass kpi", uiOutput("comp_kpi_safe")),
            div(class="glass kpi", uiOutput("comp_kpi_most_comp"))
        ),

        bslib::layout_columns(
          col_widths = c(7, 5),

          # ---- Mapa competitividad ----
          div(class="glass", style="padding:14px;",
              div(class="card-hd",
                  div(class="card-hd-icon", "\U0001F5FA\uFE0F"),
                  div(class="card-hd-text",
                      div(class="t", "Mapa de competitividad"),
                      div(class="s", "Margen 1\u00ba \u2212 2\u00ba lugar por secci\u00f3n")
                  ),
                  div(class="card-hd-right",
                      radioButtons("comp_metric", NULL,
                                   choices=list("% (margen)"="pct", "Votos (margen)"="abs"),
                                   selected="pct", inline=TRUE)
                  )
              ),
              leafletOutput("comp_map", height="520px")
          ),

          div(
            # ---- Distribution ----
            div(class="glass", style="padding:14px; margin-bottom:12px;",
                div(class="card-hd",
                    div(class="card-hd-icon", "\U0001F4CA"),
                    div(class="card-hd-text",
                        div(class="t", "Distribuci\u00f3n del margen"),
                        div(class="s", "Histograma: 1\u00ba \u2212 2\u00ba lugar")
                    )
                ),
                plotlyOutput("comp_hist", height="240px")
            ),
            # ---- Classification pie ----
            div(class="glass", style="padding:14px;",
                div(class="card-hd",
                    div(class="card-hd-icon", "\U0001F3AF"),
                    div(class="card-hd-text",
                        div(class="t", "Clasificaci\u00f3n de secciones"),
                        div(class="s", "Zona de batalla / Competitiva / Segura")
                    )
                ),
                plotlyOutput("comp_class_pie", height="240px")
            )
          )
        ),

        div(style="height:14px;"),

        # ---- Table ----
        div(class="glass", style="padding:14px;",
            div(class="card-hd",
                div(class="card-hd-icon", "\U0001F4CB"),
                div(class="card-hd-text",
                    div(class="t", "Tabla de competitividad"),
                    div(class="s", "Todas las secciones con margen, ganador, 2\u00ba lugar")
                )
            ),
            DTOutput("comp_tbl")
        )
    )
  )
}

competitividad_server <- function(input, output, session, has_applied, applied, df_applied, dl_applied, df_metrics) {

  comp_data <- reactive({
    validate(need(has_applied(), "Presiona GENERAR"))
    df <- df_applied(); key <- applied()$election
    gv <- group_votes_matrix(df, key, "DISTRIBUIDO"); G <- gv$G
    validate(need(!is.null(G) && ncol(G) >= 2, "Se necesitan al menos 2 partidos"))

    n <- nrow(G)
    top1_val <- numeric(n); top2_val <- numeric(n)
    top1_name <- character(n); top2_name <- character(n)
    for (i in seq_len(n)) {
      ord <- order(G[i, ], decreasing=TRUE)
      top1_val[i] <- G[i, ord[1L]]; top1_name[i] <- colnames(G)[ord[1L]]
      top2_val[i] <- G[i, ord[2L]]; top2_name[i] <- colnames(G)[ord[2L]]
    }
    row_tot <- rowSums(G, na.rm=TRUE)
    margin_abs <- top1_val - top2_val
    margin_pct <- ifelse(row_tot > 0, margin_abs / row_tot, NA_real_)

    # Classification
    class_lbl <- ifelse(margin_pct < 0.05, "Zona de batalla",
                        ifelse(margin_pct < 0.15, "Competitiva", "Segura"))
    class_lbl[!is.finite(margin_pct)] <- NA_character_

    list(df=df, margin_abs=margin_abs, margin_pct=margin_pct,
         top1_name=top1_name, top2_name=top2_name,
         top1_val=top1_val, top2_val=top2_val,
         row_tot=row_tot, class=class_lbl)
  })

  # ---- KPIs ----
  output$comp_kpi_avg <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Margen promedio"))
    d <- comp_data()
    avg <- mean(d$margin_pct, na.rm=TRUE)
    tagList(
      div(class="t", "Margen promedio"),
      div(class="v", ifelse(is.finite(avg), paste0(formatC(100*avg, format="f", digits=1), " pp"), "\u2014")),
      div(class="s", "1\u00ba \u2212 2\u00ba lugar (% total votos)")
    )
  })

  output$comp_kpi_battle <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Zona de batalla"))
    d <- comp_data()
    n_bat <- sum(d$class == "Zona de batalla", na.rm=TRUE)
    n_tot <- sum(!is.na(d$class))
    bar_w <- if (n_tot > 0) paste0(min(100*n_bat/n_tot, 100), "%") else "0%"
    tagList(
      div(class="t", "Zona de batalla (<5pp)"),
      div(class="v", style="color:#D50000;", fmt_int(n_bat)),
      div(class="s", HTML(paste0(formatC(100*n_bat/max(n_tot,1), format="f", digits=1), "% del total"))),
      div(class="kpi-bar-track", div(class="kpi-bar-fill", style=paste0("width:", bar_w, ";background:#D50000;")))
    )
  })

  output$comp_kpi_safe <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Segura"))
    d <- comp_data()
    n_safe <- sum(d$class == "Segura", na.rm=TRUE)
    n_tot <- sum(!is.na(d$class))
    bar_w <- if (n_tot > 0) paste0(min(100*n_safe/n_tot, 100), "%") else "0%"
    tagList(
      div(class="t", "Segura (>15pp)"),
      div(class="v", style="color:#2EAD4A;", fmt_int(n_safe)),
      div(class="s", HTML(paste0(formatC(100*n_safe/max(n_tot,1), format="f", digits=1), "% del total"))),
      div(class="kpi-bar-track", div(class="kpi-bar-fill", style=paste0("width:", bar_w, ";background:#2EAD4A;")))
    )
  })

  output$comp_kpi_most_comp <- renderUI({
    if (!has_applied()) return(kpi_placeholder("M\u00e1s competida"))
    d <- comp_data()
    valid <- which(is.finite(d$margin_pct))
    if (length(valid) == 0L) return(kpi_placeholder("M\u00e1s competida", "N/A"))
    idx <- valid[which.min(d$margin_pct[valid])]
    tagList(
      div(class="t", "Secci\u00f3n m\u00e1s competida"),
      div(class="v", paste0("Secc ", d$df$SECCION[idx])),
      div(class="s", HTML(paste0(
        party_logo_inline(d$top1_name[idx], "14px"), "<b>", d$top1_name[idx], "</b> vs ",
        party_logo_inline(d$top2_name[idx], "14px"), "<b>", d$top2_name[idx], "</b>",
        " \u00b7 ", formatC(100*d$margin_pct[idx], format="f", digits=1), "pp"
      )))
    )
  })

  # ---- MAP ----
  output$comp_map <- renderLeaflet({ create_base_leaflet() })
  outputOptions(output, "comp_map", suspendWhenHidden = FALSE)

  observe({
    req(has_applied())
    d <- comp_data(); df <- d$df; req(nrow(df) > 0L)
    proxy <- leafletProxy("comp_map", session=session) |> clearShapes() |> clearMarkers() |> clearControls()
    proxy <- restore_map_controls(proxy)

    met <- input$comp_metric %||% "pct"
    if (met == "pct") {
      vals <- d$margin_pct * 100
      ttl <- "Margen (pp)"
    } else {
      vals <- d$margin_abs
      ttl <- "Margen (votos)"
    }

    # Color scale: red (competitive) -> yellow -> green (safe)
    pal <- colorNumeric(
      palette = grDevices::colorRampPalette(c("#D50000", "#FFD200", "#2EAD4A"))(256),
      domain = vals[is.finite(vals)],
      na.color = "#00000000"
    )

    lab <- paste0(
      "<b style='color:", ACCENT, "'>Secci\u00f3n ", df$SECCION, "</b>",
      "<br>1\u00ba: ", party_logo_inline(d$top1_name, "14px"), "<b>", d$top1_name, "</b> (", fmt_int(d$top1_val), ")",
      "<br>2\u00ba: ", party_logo_inline(d$top2_name, "14px"), "<b>", d$top2_name, "</b> (", fmt_int(d$top2_val), ")",
      "<br><b>Margen:</b> ", if (met=="pct") paste0(formatC(vals, format="f", digits=1), " pp") else fmt_int(vals),
      "<br>Clase: <b>", d$class, "</b>"
    )

    proxy <- proxy |>
      addPolygons(data=df, color="rgba(255,255,255,.18)", weight=1,
                  fillColor=pal(vals), fillOpacity=0.65,
                  label=lapply(lab, HTML),
                  highlightOptions=highlightOptions(color="#ffffff", weight=2, bringToFront=TRUE)) |>
      addLegend(position="bottomright", pal=pal, values=vals[is.finite(vals)],
                title=ttl, opacity=0.9)

    proxy <- add_dl_layer(proxy, dl_applied())
    proxy <- add_layers_control(proxy)
    proxy <- hideGroup(proxy, setdiff(names(BASEMAPS), BASEMAP_DEFAULT))
    bb <- st_bbox(df)
    proxy |> fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
  })

  # ---- Histogram ----
  output$comp_hist <- renderPlotly({
    validate(need(has_applied(), "Presiona GENERAR"))
    d <- comp_data()
    vals <- 100 * d$margin_pct[is.finite(d$margin_pct)]
    validate(need(length(vals) > 2, "Insuficientes datos"))

    plot_ly(x=vals, type="histogram", nbinsx=30,
            marker=list(
              color=vapply(vals, function(v) {
                if (v < 5) "#D50000" else if (v < 15) "#FFD200" else "#2EAD4A"
              }, character(1)),
              line=list(color="rgba(255,255,255,.15)", width=0.5)
            ),
            hovertemplate="Margen: %{x:.1f}pp<br>Secciones: %{y}<extra></extra>") |>
      layout(
        xaxis=list(title="Margen (pp)", color="#FFFFFF", gridcolor="rgba(255,255,255,.06)"),
        yaxis=list(title="Secciones", color="#FFFFFF", gridcolor="rgba(255,255,255,.06)"),
        shapes=list(
          list(type="line", x0=5, x1=5, y0=0, y1=1, yref="paper",
               line=list(color="#D50000", width=2, dash="dash")),
          list(type="line", x0=15, x1=15, y0=0, y1=1, yref="paper",
               line=list(color="#2EAD4A", width=2, dash="dash"))
        ),
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
        font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
        margin=list(l=50, r=10, t=10, b=50), bargap=0.06
      ) |> config(displayModeBar=FALSE, responsive=TRUE)
  })

  # ---- Classification pie ----
  output$comp_class_pie <- renderPlotly({
    validate(need(has_applied(), "Presiona GENERAR"))
    d <- comp_data()
    cls <- d$class[!is.na(d$class)]
    validate(need(length(cls) > 0, "Sin datos"))
    tbl <- table(factor(cls, levels=c("Zona de batalla","Competitiva","Segura")))
    cols <- c("#D50000", "#FFD200", "#2EAD4A")
    plot_ly(labels=names(tbl), values=as.numeric(tbl), type="pie", hole=0.55,
            marker=list(colors=cols, line=list(color="rgba(255,255,255,.12)", width=1)),
            textinfo="label+percent", textfont=list(color="#FFFFFF", size=11),
            hovertemplate="<b>%{label}</b><br>Secciones: %{value}<br>%{percent}<extra></extra>") |>
      layout(showlegend=FALSE,
             paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
             font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
             margin=list(l=10, r=10, t=10, b=10)) |>
      config(displayModeBar=FALSE, responsive=TRUE)
  })

  # ---- Table ----
  output$comp_tbl <- renderDT({
    validate(need(has_applied(), "Presiona GENERAR"))
    d <- comp_data()
    tbl_d <- data.frame(
      SECCION = d$df$SECCION,
      GANADOR = vapply(d$top1_name, function(p) paste0(party_logo_inline(p,"15px"),"<b>",p,"</b>"), character(1)),
      VOTOS_1 = d$top1_val,
      SEGUNDO = vapply(d$top2_name, function(p) paste0(party_logo_inline(p,"15px"),"<b>",p,"</b>"), character(1)),
      VOTOS_2 = d$top2_val,
      MARGEN_PP = round(100 * d$margin_pct, 2),
      MARGEN_VOTOS = d$margin_abs,
      CLASE = d$class,
      stringsAsFactors = FALSE
    )
    tbl_d <- tbl_d[order(tbl_d$MARGEN_PP), ]

    datatable(tbl_d, rownames=FALSE, escape=FALSE, filter="top",
              extensions="Buttons",
              options=list(dom="Bfrtip", scrollX=TRUE, pageLength=20,
                           buttons=c("copy","csv","excel"),
                           initComplete=dt_dark_init)) |>
      formatRound(c("VOTOS_1","VOTOS_2","MARGEN_VOTOS"), digits=0)
  })
}
