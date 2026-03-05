# ==========================================================
# MODULO: COMPARADOR — Side-by-side de secciones
# ==========================================================

comparador_ui <- function() {
  bslib::nav_panel(
    HTML("<span class='nav-icon'>\U0001F50D</span>Comparador"),
    div(style="padding:12px;",

        div(class="glass", style="padding:14px; margin-bottom:14px;",
            div(class="card-hd",
                div(class="card-hd-icon", "\U0001F50D"),
                div(class="card-hd-text",
                    div(class="t", "Comparador de secciones"),
                    div(class="s", "Selecciona 2\u20133 secciones para comparar lado a lado")
                )
            ),
            bslib::layout_columns(
              col_widths = c(8, 4),
              selectizeInput("cmp_secciones", NULL, choices=NULL, multiple=TRUE,
                             options=list(maxItems=3, plugins=list("remove_button"),
                                          placeholder="Escribe hasta 3 secciones\u2026")),
              actionButton("cmp_compare", "\U0001F50D Comparar", class="btn btn-accent w-100")
            )
        ),

        # ---- Radar chart ----
        div(class="glass", style="padding:14px; margin-bottom:14px;",
            div(class="card-hd",
                div(class="card-hd-icon", "\U0001F4CA"),
                div(class="card-hd-text",
                    div(class="t", "Radar comparativo"),
                    div(class="s", "M\u00e9tricas normalizadas por secci\u00f3n")
                )
            ),
            plotlyOutput("cmp_radar", height="420px")
        ),

        # ---- Detail tables side by side ----
        div(class="glass", style="padding:14px; margin-bottom:14px;",
            div(class="card-hd",
                div(class="card-hd-icon", "\U0001F4CB"),
                div(class="card-hd-text",
                    div(class="t", "M\u00e9tricas detalladas"),
                    div(class="s", "Comparaci\u00f3n directa de valores")
                )
            ),
            DTOutput("cmp_detail_tbl")
        ),

        # ---- Bar chart comparison ----
        div(class="glass", style="padding:14px;",
            div(class="card-hd",
                div(class="card-hd-icon", "\U0001F4CA"),
                div(class="card-hd-text",
                    div(class="t", "Votos por partido"),
                    div(class="s", "Comparaci\u00f3n de resultados electorales")
                )
            ),
            plotlyOutput("cmp_bars", height="380px")
        )
    )
  )
}

comparador_server <- function(input, output, session, has_applied, applied, df_applied, df_metrics) {

  # Populate section choices
  observe({
    req(has_applied())
    df <- df_applied()
    secs <- sort(unique(df$SECCION))
    ch <- as.list(setNames(as.character(secs), as.character(secs)))
    updateSelectizeInput(session, "cmp_secciones", choices=ch, server=TRUE)
  })

  cmp_data <- reactiveVal(NULL)

  observeEvent(input$cmp_compare, {
    req(has_applied())
    secs <- suppressWarnings(as.integer(input$cmp_secciones %||% character(0)))
    secs <- secs[!is.na(secs)]
    validate(need(length(secs) >= 2, "Selecciona al menos 2 secciones"))
    cmp_data(secs)
  }, ignoreInit=TRUE)

  has_cmp <- reactive(!is.null(cmp_data()) && length(cmp_data()) >= 2)

  # Build comparison matrix
  cmp_matrix <- reactive({
    validate(need(has_cmp() && has_applied(), "Presiona Comparar"))
    secs <- cmp_data(); df <- df_applied(); key <- applied()$election
    df_sub <- df[df$SECCION %in% secs, ]
    validate(need(nrow(df_sub) >= 2, "No se encontraron las secciones"))
    df_plain <- as.data.frame(st_drop_geometry(df_sub))

    # Build metric rows
    metrics <- list()

    # Basic metrics
    c_tot <- total_col(df_sub, key); c_ln <- ln_col(df_sub, key)
    c_cas <- metric_col(df_sub, key, "CASILLAS")
    if (!is.na(c_tot) && c_tot %in% names(df_sub)) metrics[["Total votos"]] <- as_num(df_sub[[c_tot]])
    if (!is.na(c_ln) && c_ln %in% names(df_sub)) metrics[["Lista nominal"]] <- as_num(df_sub[[c_ln]])
    if (!is.na(c_cas) && c_cas %in% names(df_sub)) metrics[["Casillas"]] <- as_num(df_sub[[c_cas]])

    # Participation
    if (!is.na(c_tot) && !is.na(c_ln) && c_tot %in% names(df_sub) && c_ln %in% names(df_sub)) {
      tot <- as_num(df_sub[[c_tot]]); ln <- as_num(df_sub[[c_ln]])
      metrics[["Participaci\u00f3n (%)"]] <- ifelse(is.finite(ln) & ln > 0, 100 * tot / ln, NA_real_)
    }

    # Winner
    w <- winner_by_row(df_sub, key, "DISTRIBUIDO")
    metrics[["Ganador"]] <- w

    # Party votes
    gv <- group_votes_matrix(df_sub, key, "DISTRIBUIDO"); G <- gv$G
    if (!is.null(G) && ncol(G) > 0) {
      tot_all <- colSums(G, na.rm=TRUE)
      top_parties <- names(sort(tot_all, decreasing=TRUE))[seq_len(min(6, length(tot_all)))]
      top_parties <- setdiff(top_parties, "OTROS")
      for (p in top_parties) {
        metrics[[paste0("Votos: ", p)]] <- as.numeric(G[, p])
      }
    }

    # Electorado
    for (nm in c("LISTA_HOMBRES","LISTA_MUJERES","PCT_LISTA_GEN_Z","PCT_LISTA_MILLENNIALS","PCT_LISTA_GEN_X","PCT_LISTA_BOOMERS")) {
      if (nm %in% names(df_plain)) {
        lbl <- nm
        for (e in names(ELECTORADO_CATALOG)) {
          if (ELECTORADO_CATALOG[[e]]$col == nm) { lbl <- e; break }
        }
        metrics[[lbl]] <- as_num(df_plain[[nm]])
      }
    }

    list(secciones=df_sub$SECCION, metrics=metrics, df_sub=df_sub, key=key, G=G)
  })

  # ---- Radar ----
  output$cmp_radar <- renderPlotly({
    validate(need(has_cmp(), "Selecciona secciones y presiona Comparar"))
    cm <- cmp_matrix()

    # Get numeric metrics for radar
    num_metrics <- list()
    num_labels <- character(0)
    for (nm in names(cm$metrics)) {
      v <- cm$metrics[[nm]]
      if (is.numeric(v)) { num_metrics[[nm]] <- v; num_labels <- c(num_labels, nm) }
    }
    validate(need(length(num_metrics) >= 3, "Menos de 3 m\u00e9tricas num\u00e9ricas"))

    M <- do.call(cbind, num_metrics)
    # Normalize 0-1 per column
    for (j in seq_len(ncol(M))) {
      rng <- range(M[, j], na.rm=TRUE)
      if (diff(rng) > 0) M[, j] <- (M[, j] - rng[1]) / diff(rng)
      else M[, j] <- 0.5
    }

    radar_colors <- c("#D50000", "#005BAC", "#2EAD4A")
    p <- plot_ly(type="scatterpolar", fill="toself")
    for (i in seq_along(cm$secciones)) {
      r <- c(M[i, ], M[i, 1L])
      theta <- c(num_labels, num_labels[1L])
      col <- radar_colors[((i - 1) %% length(radar_colors)) + 1]
      p <- p |> add_trace(r=r, theta=theta, name=paste0("Secc ", cm$secciones[i]),
                          line=list(color=col, width=2.5),
                          fillcolor=paste0(col, "33"),
                          marker=list(color=col, size=6))
    }
    p |> layout(
      polar=list(
        bgcolor="rgba(0,0,0,0)",
        radialaxis=list(visible=TRUE, range=c(0,1), color="rgba(255,255,255,.30)", gridcolor="rgba(255,255,255,.08)"),
        angularaxis=list(color="#FFFFFF", gridcolor="rgba(255,255,255,.08)")
      ),
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
      legend=list(orientation="h", x=0, y=-0.15, font=list(color="#FFFFFF")),
      margin=list(l=80, r=80, t=40, b=60)
    ) |> config(displayModeBar=FALSE, responsive=TRUE)
  })

  # ---- Detail table ----
  output$cmp_detail_tbl <- renderDT({
    validate(need(has_cmp(), "Selecciona secciones y presiona Comparar"))
    cm <- cmp_matrix()

    # Build table: metrics as rows, secciones as columns
    rows <- list()
    for (nm in names(cm$metrics)) {
      v <- cm$metrics[[nm]]
      row <- data.frame(Metrica=nm, stringsAsFactors=FALSE)
      for (i in seq_along(cm$secciones)) {
        val <- v[i]
        if (is.numeric(val)) {
          row[[paste0("Secc_", cm$secciones[i])]] <- if (is.finite(val)) formatC(val, format="f", digits=2, big.mark=",") else "\u2014"
        } else {
          row[[paste0("Secc_", cm$secciones[i])]] <- as.character(val %||% "\u2014")
        }
      }
      rows[[length(rows) + 1L]] <- row
    }
    d <- do.call(rbind, rows)

    # Winner rows: add logos
    for (i in seq_along(cm$secciones)) {
      col_nm <- paste0("Secc_", cm$secciones[i])
      winner_idx <- which(d$Metrica == "Ganador")
      if (length(winner_idx) > 0) {
        w <- d[[col_nm]][winner_idx]
        if (!is.na(w) && nzchar(w) && w != "\u2014") {
          d[[col_nm]][winner_idx] <- paste0(party_logo_inline(w, "15px"), "<b>", w, "</b>")
        }
      }
    }

    datatable(d, rownames=FALSE, escape=FALSE,
              options=list(dom="t", pageLength=50, ordering=FALSE,
                           initComplete=dt_dark_init))
  })

  # ---- Grouped bar chart ----
  output$cmp_bars <- renderPlotly({
    validate(need(has_cmp(), "Selecciona secciones y presiona Comparar"))
    cm <- cmp_matrix()
    G <- cm$G
    validate(need(!is.null(G) && ncol(G) > 0, "Sin votos"))

    tot_all <- colSums(G, na.rm=TRUE)
    top_parties <- names(sort(tot_all, decreasing=TRUE))[seq_len(min(8, ncol(G)))]
    top_parties <- setdiff(top_parties, "OTROS")
    validate(need(length(top_parties) > 0, "Sin partidos"))

    bar_colors <- c("#D50000", "#005BAC", "#2EAD4A")
    p <- plot_ly()
    for (i in seq_along(cm$secciones)) {
      vals <- as.numeric(G[i, top_parties])
      col <- bar_colors[((i - 1) %% length(bar_colors)) + 1]
      p <- p |> add_trace(x=top_parties, y=vals, type="bar", name=paste0("Secc ", cm$secciones[i]),
                          marker=list(color=col, opacity=0.85),
                          text=paste0("<b>Secc ", cm$secciones[i], "</b><br>", top_parties, ": ", fmt_int(vals)),
                          hoverinfo="text")
    }

    p |> layout(
      barmode="group",
      xaxis=list(title="", color="#FFFFFF", gridcolor="rgba(255,255,255,.06)"),
      yaxis=list(title="Votos", color="#FFFFFF", gridcolor="rgba(255,255,255,.06)"),
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
      legend=list(orientation="h", x=0, y=1.1, font=list(color="#FFFFFF")),
      margin=list(l=60, r=10, t=30, b=50)
    ) |> config(displayModeBar=FALSE, responsive=TRUE)
  })
}
