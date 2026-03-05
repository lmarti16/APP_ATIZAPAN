# ==========================================================
# MODULO: HOME — Dashboard de KPIs Resumen
# ==========================================================

home_ui <- function() {
  bslib::nav_panel(
    HTML("<span class='nav-icon'>\U0001F3E0</span>Inicio"),
    div(style="padding:12px;",

        # ---- Hero row ----
        div(class="glass", style="padding:20px 24px; margin-bottom:14px; text-align:center;",
            div(style="font-size:24px; font-weight:900; letter-spacing:-.03em;",
                class="grad-title", "electrend \u00b7 Atizap\u00e1n"),
            div(style="font-size:12px; color:rgba(255,255,255,.60); margin-top:4px;",
                HTML("Presiona <b>\u26a1 GENERAR</b> en el panel izquierdo para cargar los datos."))
        ),

        # ---- KPI row 1: overview ----
        div(class="kpiRow", style="margin-bottom:14px;",
            div(class="glass kpi", uiOutput("home_kpi_secciones")),
            div(class="glass kpi", uiOutput("home_kpi_votos")),
            div(class="glass kpi", uiOutput("home_kpi_participacion")),
            div(class="glass kpi", uiOutput("home_kpi_ganador"))
        ),

        # ---- KPI row 2: detail ----
        div(class="kpiRow", style="margin-bottom:14px;",
            div(class="glass kpi", uiOutput("home_kpi_max_part")),
            div(class="glass kpi", uiOutput("home_kpi_min_part")),
            div(class="glass kpi", uiOutput("home_kpi_tendencia")),
            div(class="glass kpi", uiOutput("home_kpi_competitividad"))
        ),

        # ---- Charts row ----
        bslib::layout_columns(
          col_widths = c(6, 6),
          div(class="glass", style="padding:14px;",
              div(class="card-hd",
                  div(class="card-hd-icon", "\U0001F3C6"),
                  div(class="card-hd-text",
                      div(class="t", "Distribuci\u00f3n de votos por partido"),
                      div(class="s", "Elecci\u00f3n seleccionada")
                  )
              ),
              plotlyOutput("home_pie", height="340px")
          ),
          div(class="glass", style="padding:14px;",
              div(class="card-hd",
                  div(class="card-hd-icon", "\U0001F4CA"),
                  div(class="card-hd-text",
                      div(class="t", "Participaci\u00f3n por secci\u00f3n"),
                      div(class="s", "Distribuci\u00f3n (histograma)")
                  )
              ),
              plotlyOutput("home_hist_part", height="340px")
          )
        ),

        div(style="height:14px;"),

        # ---- Top secciones table ----
        div(class="glass", style="padding:14px;",
            div(class="card-hd",
                div(class="card-hd-icon", "\U0001F4CB"),
                div(class="card-hd-text",
                    div(class="t", "Top 10 secciones por participaci\u00f3n"),
                    div(class="s", "Mayor y menor participaci\u00f3n en la selecci\u00f3n")
                ),
                div(class="card-hd-right",
                    div(class="seg-control",
                        radioButtons("home_top_dir", NULL,
                                     choices=list("\u2b06 Mayor"="desc","\u2b07 Menor"="asc"),
                                     selected="desc", inline=TRUE)
                    )
                )
            ),
            DTOutput("home_top_tbl")
        )
    )
  )
}

home_server <- function(input, output, session, has_applied, applied, df_applied, df_metrics) {

  output$home_kpi_secciones <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Secciones"))
    x <- df_metrics()
    tagList(
      div(class="t", "Secciones"),
      div(class="v", fmt_int(nrow(x$df))),
      div(class="s", HTML(paste0("Elecci\u00f3n: <b>", key_label(applied()$election), "</b>")))
    )
  })

  output$home_kpi_votos <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Total votos"))
    x <- df_metrics()
    tot <- sum(x$tot, na.rm=TRUE); ln <- sum(x$ln, na.rm=TRUE)
    tagList(
      div(class="t", "Total votos"),
      div(class="v", fmt_int(tot)),
      div(class="s", HTML(paste0("Lista nominal: <b>", fmt_int(ln), "</b>")))
    )
  })

  output$home_kpi_participacion <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Participaci\u00f3n"))
    x <- df_metrics()
    tot <- sum(x$tot, na.rm=TRUE); ln <- sum(x$ln, na.rm=TRUE)
    part_pp <- if (is.finite(ln) && ln > 0) 100 * tot / ln else NA_real_
    bar_w <- if (is.finite(part_pp)) paste0(min(max(part_pp, 0), 100), "%") else "0%"
    tagList(
      div(class="t", "Participaci\u00f3n"),
      div(class="v", ifelse(is.finite(part_pp), paste0(formatC(part_pp, format="f", digits=2), "%"), "\u2014")),
      div(class="s", "Promedio ponderado sobre LN"),
      div(class="kpi-bar-track", div(class="kpi-bar-fill", style=paste0("width:", bar_w, ";")))
    )
  })

  output$home_kpi_ganador <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Partido dominante"))
    ap <- applied(); x <- df_metrics()
    tot <- totals_for_view(x$df, ap$election, "DISTRIBUIDO")
    top <- top2_from_totals(tot)
    if (!nzchar(top$w1 %||% "")) return(kpi_placeholder("Partido dominante", "Sin datos"))
    share <- if (is.finite(top$v1) && sum(tot, na.rm=TRUE) > 0) 100 * top$v1 / sum(tot, na.rm=TRUE) else NA_real_
    tagList(
      div(class="t", "Partido dominante"),
      div(class="v", HTML(paste0(party_logo_inline(top$w1, "24px"), " <b>", top$w1, "</b>"))),
      div(class="s", HTML(paste0(fmt_int(top$v1), " votos",
                                 if (is.finite(share)) paste0(" (<b>", formatC(share, format="f", digits=1), "%</b>)") else "")))
    )
  })

  output$home_kpi_max_part <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Mayor participaci\u00f3n"))
    x <- df_metrics()
    p <- x$part; idx <- which.max(p)
    if (length(idx) == 0L || !is.finite(p[idx])) return(kpi_placeholder("Mayor participaci\u00f3n", "N/A"))
    tagList(
      div(class="t", "Mayor participaci\u00f3n"),
      div(class="v", fmt_pct(p[idx])),
      div(class="s", HTML(paste0("Secci\u00f3n: <b>", x$df$SECCION[idx], "</b>")))
    )
  })

  output$home_kpi_min_part <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Menor participaci\u00f3n"))
    x <- df_metrics()
    p <- x$part; valid_p <- which(is.finite(p) & p > 0)
    if (length(valid_p) == 0L) return(kpi_placeholder("Menor participaci\u00f3n", "N/A"))
    idx <- valid_p[which.min(p[valid_p])]
    tagList(
      div(class="t", "Menor participaci\u00f3n"),
      div(class="v", fmt_pct(p[idx])),
      div(class="s", HTML(paste0("Secci\u00f3n: <b>", x$df$SECCION[idx], "</b>")))
    )
  })

  output$home_kpi_tendencia <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Tendencia vs anterior"))
    ap <- applied(); key <- ap$election
    pk <- parse_key(key)
    # Find previous election of same office
    prev_keys <- elex$key[elex$office == pk$office & elex$year < pk$year]
    if (length(prev_keys) == 0L) return(tagList(div(class="t","Tendencia"), div(class="v","\u2014"), div(class="s","Sin elecci\u00f3n anterior")))
    prev_key <- tail(prev_keys, 1L)
    df <- df_applied()
    c_tot_cur <- total_col(df, key); c_ln_cur <- ln_col(df, key)
    c_tot_pre <- total_col(df, prev_key); c_ln_pre <- ln_col(df, prev_key)
    tot_cur <- if (!is.na(c_tot_cur) && c_tot_cur %in% names(df)) sum(as_num(df[[c_tot_cur]]), na.rm=TRUE) else NA_real_
    ln_cur  <- if (!is.na(c_ln_cur) && c_ln_cur %in% names(df)) sum(as_num(df[[c_ln_cur]]), na.rm=TRUE) else NA_real_
    tot_pre <- if (!is.na(c_tot_pre) && c_tot_pre %in% names(df)) sum(as_num(df[[c_tot_pre]]), na.rm=TRUE) else NA_real_
    ln_pre  <- if (!is.na(c_ln_pre) && c_ln_pre %in% names(df)) sum(as_num(df[[c_ln_pre]]), na.rm=TRUE) else NA_real_
    part_cur <- if (is.finite(ln_cur) && ln_cur > 0) tot_cur / ln_cur else NA_real_
    part_pre <- if (is.finite(ln_pre) && ln_pre > 0) tot_pre / ln_pre else NA_real_
    delta_pp <- if (is.finite(part_cur) && is.finite(part_pre)) 100 * (part_cur - part_pre) else NA_real_
    col <- if (is.finite(delta_pp) && delta_pp >= 0) "#2EAD4A" else "#D50000"
    tagList(
      div(class="t", "Tendencia participaci\u00f3n"),
      div(class="v", style=paste0("color:", col, ";"), fmt_signed_pp(delta_pp / 100)),
      div(class="s", HTML(paste0("vs <b>", key_label(prev_key), "</b>")))
    )
  })

  output$home_kpi_competitividad <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Competitividad"))
    ap <- applied(); x <- df_metrics(); df <- x$df; key <- ap$election
    gv <- group_votes_matrix(df, key, "DISTRIBUIDO"); G <- gv$G
    if (is.null(G) || ncol(G) < 2L) return(kpi_placeholder("Competitividad", "Datos insuficientes"))
    # Per-section margin
    top1 <- apply(G, 1, function(r) sort(r, decreasing=TRUE)[1L])
    top2 <- apply(G, 1, function(r) { s <- sort(r, decreasing=TRUE); if (length(s) >= 2) s[2L] else 0 })
    row_tot <- rowSums(G, na.rm=TRUE)
    margin_pct <- ifelse(row_tot > 0, (top1 - top2) / row_tot, NA_real_)
    avg_margin <- mean(margin_pct, na.rm=TRUE)
    n_battle <- sum(margin_pct < 0.05, na.rm=TRUE)
    tagList(
      div(class="t", "Competitividad promedio"),
      div(class="v", ifelse(is.finite(avg_margin), paste0(formatC(100 * avg_margin, format="f", digits=1), " pp"), "\u2014")),
      div(class="s", HTML(paste0("<b>", n_battle, "</b> secciones battleground (<5pp)")))
    )
  })

  # ---- Pie chart ----
  output$home_pie <- renderPlotly({
    validate(need(has_applied(), "Presiona GENERAR"))
    ap <- applied(); x <- df_metrics(); df <- x$df
    tot <- totals_for_view(df, ap$election, "DISTRIBUIDO")
    validate(need(length(tot) > 0, "Sin datos"))
    top_n <- head(sort(tot, decreasing=TRUE), 8)
    if (sum(tot) - sum(top_n) > 0) top_n <- c(top_n, OTROS = sum(tot) - sum(top_n))
    cols <- vapply(names(top_n), function(p) {
      if (p %in% names(party_colors)) unname(party_colors[[p]]) else "#6B7280"
    }, character(1))
    plot_ly(labels=names(top_n), values=as.numeric(top_n), type="pie", hole=0.50,
            marker=list(colors=cols, line=list(color="rgba(255,255,255,.12)", width=1)),
            textinfo="label+percent", textfont=list(color="#FFFFFF", size=11),
            hovertemplate="<b>%{label}</b><br>Votos: %{value:,.0f}<br>%{percent}<extra></extra>") |>
      layout(showlegend=FALSE,
             paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
             font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
             margin=list(l=10, r=10, t=10, b=10)) |>
      config(displayModeBar=FALSE, responsive=TRUE)
  })

  # ---- Histogram ----
  output$home_hist_part <- renderPlotly({
    validate(need(has_applied(), "Presiona GENERAR"))
    x <- df_metrics()
    p <- x$part[is.finite(x$part)]
    validate(need(length(p) > 2, "Muy pocas secciones"))
    plot_ly(x=100 * p, type="histogram", nbinsx=25,
            marker=list(color=ACCENT, line=list(color="rgba(255,255,255,.15)", width=0.5)),
            hovertemplate="Rango: %{x}%<br>Secciones: %{y}<extra></extra>") |>
      layout(
        xaxis=list(title="Participaci\u00f3n (%)", color="#FFFFFF", gridcolor="rgba(255,255,255,.06)", ticksuffix="%"),
        yaxis=list(title="Secciones", color="#FFFFFF", gridcolor="rgba(255,255,255,.06)"),
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
        font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
        margin=list(l=50, r=10, t=10, b=50),
        bargap=0.08
      ) |> config(displayModeBar=FALSE, responsive=TRUE)
  })

  # ---- Top table ----
  output$home_top_tbl <- renderDT({
    validate(need(has_applied(), "Presiona GENERAR"))
    x <- df_metrics(); df <- x$df; key <- applied()$election
    d <- data.frame(
      SECCION = df$SECCION,
      TOTAL_VOTOS = x$tot,
      LISTA_NOMINAL = x$ln,
      PARTICIPACION = x$part,
      stringsAsFactors = FALSE
    )
    # Add winner
    w <- winner_by_row(df, key, "DISTRIBUIDO")
    d$GANADOR <- vapply(w, function(p) {
      if (is.na(p)) return("\u2014")
      paste0(party_logo_inline(p, "15px"), "<b>", p, "</b>")
    }, character(1))

    d <- d[is.finite(d$PARTICIPACION), ]
    asc <- identical(input$home_top_dir %||% "desc", "asc")
    d <- d[order(d$PARTICIPACION, decreasing=!asc), ]
    d <- head(d, 10)

    datatable(d, rownames=FALSE, escape=FALSE,
              options=list(dom="t", pageLength=10, ordering=FALSE,
                           initComplete=dt_dark_init)) |>
      formatRound(c("TOTAL_VOTOS", "LISTA_NOMINAL"), digits=0) |>
      formatPercentage("PARTICIPACION", digits=2)
  })
}
