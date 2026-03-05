# ==========================================================
# MODULO: CORRELACION Electoral-Demográfico
# ==========================================================

correlacion_ui <- function() {
  bslib::nav_panel(
    HTML("<span class='nav-icon'>\U0001F4C9</span>Correlaci\u00f3n"),
    div(style="padding:12px;",
        bslib::layout_columns(
          col_widths = c(3, 9),

          # ---- Panel config ----
          div(class="glass", style="padding:14px;",
              div(class="card-hd",
                  div(class="card-hd-icon", "\U0001F4C9"),
                  div(class="card-hd-text",
                      div(class="t", "Correlaci\u00f3n"),
                      div(class="s", "Electoral vs Demogr\u00e1fico")
                  )
              ),
              div(class="smallHelp", "Eje X: variable demogr\u00e1fica"),
              uiOutput("cor_x_selector"),
              hr(),
              div(class="smallHelp", "Eje Y: variable electoral"),
              radioButtons("cor_y_type", NULL,
                           choices=list("% Votos partido"="party_pct",
                                        "Participaci\u00f3n (%)"="participacion",
                                        "Total votos"="total_votos",
                                        "Lista nominal"="lista_nominal"),
                           selected="party_pct"),
              conditionalPanel(
                condition = "input.cor_y_type == 'party_pct'",
                uiOutput("cor_party_selector")
              ),
              hr(),
              div(class="smallHelp", "Color de puntos"),
              radioButtons("cor_color", NULL,
                           choices=list("Ganador"="winner", "Participaci\u00f3n"="part", "Ninguno"="none"),
                           selected="winner", inline=TRUE),
              hr(),
              checkboxInput("cor_trend", "L\u00ednea de tendencia", value=TRUE),
              checkboxInput("cor_labels", "Etiquetar secciones", value=FALSE),
              hr(),
              div(class="pill", uiOutput("cor_r_value"))
          ),

          # ---- Main content ----
          div(
            div(class="glass", style="padding:14px;",
                div(class="card-hd",
                    div(class="card-hd-icon", "\U0001F4CA"),
                    div(class="card-hd-text",
                        div(class="t", "Scatter Plot"),
                        div(class="s", uiOutput("cor_subtitle"))
                    )
                ),
                plotlyOutput("cor_scatter", height="520px")
            ),
            div(style="height:12px;"),
            div(class="glass", style="padding:14px;",
                div(class="card-hd",
                    div(class="card-hd-icon", "\U0001F4CB"),
                    div(class="card-hd-text",
                        div(class="t", "Matriz de correlaci\u00f3n"),
                        div(class="s", "Top partidos vs variables demogr\u00e1ficas")
                    )
                ),
                plotlyOutput("cor_heatmap", height="400px")
            )
          )
        )
    )
  )
}

correlacion_server <- function(input, output, session, has_applied, applied, df_applied, df_metrics) {

  # ---- X selector: INEGI + Electorado ----
  output$cor_x_selector <- renderUI({
    # Build choices from INEGI and electorado
    choices <- list()
    if (length(ELECTORADO_CHOICES) > 0L) {
      choices[["Electorado"]] <- ELECTORADO_CHOICES
    }
    if (NROW(TRADUCTOR) > 0L) {
      for (eje in unique(TRADUCTOR$Eje)) {
        d0 <- TRADUCTOR[TRADUCTOR$Eje == eje, , drop=FALSE]
        ch <- setNames(d0$COL_NAME, paste0(d0$Indicador, " (", d0$VARIABLE, ")"))
        choices[[eje]] <- as.list(ch)
      }
    } else if (length(INEGI_COLS) > 0L) {
      choices[["INEGI"]] <- as.list(setNames(INEGI_COLS, INEGI_VARS))
    }
    validate(need(length(choices) > 0L, "Sin variables demogr\u00e1ficas"))
    default <- if (length(ELECTORADO_CHOICES) > 0L) ELECTORADO_CHOICES[[1L]] else {
      if (length(INEGI_COLS) > 0L) INEGI_COLS[1L] else NULL
    }
    selectInput("cor_x_var", NULL, choices=choices, selected=default)
  })

  output$cor_party_selector <- renderUI({
    validate(need(has_applied(), "Presiona GENERAR"))
    df <- df_applied(); key <- applied()$election
    gv <- group_votes_matrix(df, key, "DISTRIBUIDO")
    parties <- sort(unique(setdiff(gv$parties, "OTROS")))
    validate(need(length(parties) > 0L, "Sin partidos"))
    def <- if ("PRI" %in% parties) "PRI" else parties[1L]
    selectInput("cor_party", "Partido", choices=parties, selected=def)
  })

  # Build scatter data
  cor_data <- reactive({
    validate(need(has_applied(), "Presiona GENERAR"))
    req(input$cor_x_var, input$cor_y_type)
    df <- df_applied(); x_col <- input$cor_x_var
    validate(need(x_col %in% names(df), paste0("Columna '", x_col, "' no encontrada")))

    x_vals <- as_num(df[[x_col]])
    key <- applied()$election

    y_vals <- switch(input$cor_y_type,
      "party_pct" = {
        req(input$cor_party)
        gv <- group_votes_matrix(df, key, "DISTRIBUIDO"); G <- gv$G
        validate(need(!is.null(G), "Sin datos"))
        party <- input$cor_party
        if (!(party %in% colnames(G))) party <- colnames(G)[1L]
        votes <- as.numeric(G[, party])
        den <- rowSums(G, na.rm=TRUE)
        ifelse(den > 0, 100 * votes / den, NA_real_)
      },
      "participacion" = {
        x <- df_metrics(); 100 * x$part
      },
      "total_votos" = {
        x <- df_metrics(); x$tot
      },
      "lista_nominal" = {
        x <- df_metrics(); x$ln
      }
    )

    # Colors
    color_vals <- switch(input$cor_color %||% "none",
      "winner" = {
        w <- winner_by_row(df, key, "DISTRIBUIDO")
        vapply(w, fill_color_winner, character(1))
      },
      "part" = {
        p <- df_metrics()$part; p[!is.finite(p)] <- 0; p
      },
      NULL
    )

    y_label <- switch(input$cor_y_type,
      "party_pct" = paste0("% ", input$cor_party %||% ""),
      "participacion" = "Participaci\u00f3n (%)",
      "total_votos" = "Total votos",
      "lista_nominal" = "Lista nominal"
    )

    # X label
    x_label <- x_col
    if (x_col %in% vapply(ELECTORADO_CATALOG, `[[`, character(1), "col")) {
      nm <- names(which(vapply(ELECTORADO_CATALOG, function(z) z$col == x_col, logical(1))))
      if (length(nm)) x_label <- nm[1L]
    } else if (NROW(TRADUCTOR) > 0L) {
      idx <- match(sub("_INEGI$", "", x_col), TRADUCTOR$VARIABLE)
      if (!is.na(idx)) x_label <- TRADUCTOR$Indicador[idx]
    }

    list(x=x_vals, y=y_vals, seccion=df$SECCION, color=color_vals,
         x_label=x_label, y_label=y_label, color_mode=input$cor_color %||% "none")
  })

  output$cor_subtitle <- renderUI({
    if (!has_applied()) return(div(class="subtitle-badge", "\u23f8\ufe0f Presiona GENERAR"))
    d <- cor_data()
    div(class="subtitle-badge", paste0(d$x_label, " vs ", d$y_label))
  })

  output$cor_r_value <- renderUI({
    if (!has_applied()) return(HTML("R = \u2014"))
    d <- cor_data()
    valid <- is.finite(d$x) & is.finite(d$y)
    if (sum(valid) < 3) return(HTML("R = \u2014 (pocos datos)"))
    r <- cor(d$x[valid], d$y[valid], use="complete.obs")
    col <- if (abs(r) > 0.5) "#2EAD4A" else if (abs(r) > 0.3) "#FFD200" else "rgba(255,255,255,.70)"
    HTML(paste0("<b style='color:", col, ";'>R = ", formatC(r, format="f", digits=3), "</b>",
                " &middot; n = ", sum(valid)))
  })

  output$cor_scatter <- renderPlotly({
    validate(need(has_applied(), "Presiona GENERAR"))
    d <- cor_data()
    valid <- is.finite(d$x) & is.finite(d$y)
    validate(need(sum(valid) > 2, "Insuficientes datos v\u00e1lidos"))

    hover <- paste0("<b>Secc ", d$seccion, "</b>",
                    "<br>", d$x_label, ": ", formatC(d$x, format="f", digits=2, big.mark=","),
                    "<br>", d$y_label, ": ", formatC(d$y, format="f", digits=2, big.mark=","))

    p <- plot_ly()

    if (d$color_mode == "winner") {
      p <- p |> add_trace(x=d$x[valid], y=d$y[valid], type="scatter", mode="markers",
                          marker=list(color=d$color[valid], size=8, opacity=0.80,
                                      line=list(color="rgba(255,255,255,.25)", width=0.8)),
                          text=hover[valid], hoverinfo="text",
                          showlegend=FALSE)
    } else if (d$color_mode == "part") {
      p <- p |> add_trace(x=d$x[valid], y=d$y[valid], type="scatter", mode="markers",
                          marker=list(color=d$color[valid], colorscale="Reds", size=8, opacity=0.80,
                                      line=list(color="rgba(255,255,255,.25)", width=0.8),
                                      colorbar=list(title="Part.", ticksuffix="%", len=0.5,
                                                    tickfont=list(color="#FFFFFF"),
                                                    titlefont=list(color="#FFFFFF"))),
                          text=hover[valid], hoverinfo="text", showlegend=FALSE)
    } else {
      p <- p |> add_trace(x=d$x[valid], y=d$y[valid], type="scatter", mode="markers",
                          marker=list(color=ACCENT, size=8, opacity=0.70,
                                      line=list(color="rgba(255,255,255,.25)", width=0.8)),
                          text=hover[valid], hoverinfo="text", showlegend=FALSE)
    }

    if (isTRUE(input$cor_trend)) {
      fit <- lm(d$y[valid] ~ d$x[valid])
      x_seq <- seq(min(d$x[valid], na.rm=TRUE), max(d$x[valid], na.rm=TRUE), length.out=50)
      y_pred <- predict(fit, newdata=data.frame(`d$x[valid]`=x_seq))
      p <- p |> add_trace(x=x_seq, y=y_pred, type="scatter", mode="lines",
                          line=list(color="rgba(255,255,255,.50)", width=2, dash="dash"),
                          showlegend=FALSE, hoverinfo="skip")
    }

    if (isTRUE(input$cor_labels)) {
      p <- p |> add_trace(x=d$x[valid], y=d$y[valid], type="scatter", mode="text",
                          text=d$seccion[valid], textfont=list(color="rgba(255,255,255,.55)", size=8),
                          showlegend=FALSE, hoverinfo="skip")
    }

    p |> layout(
      xaxis=list(title=d$x_label, color="#FFFFFF", gridcolor="rgba(255,255,255,.06)"),
      yaxis=list(title=d$y_label, color="#FFFFFF", gridcolor="rgba(255,255,255,.06)"),
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
      margin=list(l=60, r=20, t=10, b=60)
    ) |> config(displayModeBar=FALSE, responsive=TRUE)
  })

  # ---- Correlation heatmap ----
  output$cor_heatmap <- renderPlotly({
    validate(need(has_applied(), "Presiona GENERAR"))
    df <- df_applied(); key <- applied()$election
    gv <- group_votes_matrix(df, key, "DISTRIBUIDO"); G <- gv$G
    validate(need(!is.null(G) && ncol(G) > 0, "Sin datos electorales"))

    # Get party percentages
    row_tot <- rowSums(G, na.rm=TRUE)
    party_pct <- sweep(G, 1, pmax(row_tot, 1), "/")

    # Top parties by total votes
    tot <- colSums(G, na.rm=TRUE)
    top_parties <- names(sort(tot, decreasing=TRUE))[seq_len(min(8L, length(tot)))]
    top_parties <- setdiff(top_parties, "OTROS")

    # Collect demographic columns
    demo_cols <- character(0)
    demo_labels <- character(0)
    for (nm in names(ELECTORADO_CATALOG)) {
      col <- ELECTORADO_CATALOG[[nm]]$col
      if (col %in% names(df)) { demo_cols <- c(demo_cols, col); demo_labels <- c(demo_labels, nm) }
    }
    # Add some INEGI
    if (length(INEGI_COLS) > 0L) {
      use_inegi <- head(INEGI_COLS, min(8L, length(INEGI_COLS)))
      demo_cols <- c(demo_cols, use_inegi)
      inegi_labels <- sub("_INEGI$", "", use_inegi)
      if (NROW(TRADUCTOR) > 0L) {
        for (i in seq_along(use_inegi)) {
          var_nm <- sub("_INEGI$", "", use_inegi[i])
          idx <- match(var_nm, TRADUCTOR$VARIABLE)
          if (!is.na(idx)) inegi_labels[i] <- TRADUCTOR$Indicador[idx]
        }
      }
      demo_labels <- c(demo_labels, inegi_labels)
    }
    validate(need(length(demo_cols) > 0, "Sin variables demogr\u00e1ficas"))

    df_plain <- as.data.frame(st_drop_geometry(df))
    cor_mat <- matrix(NA_real_, nrow=length(demo_cols), ncol=length(top_parties))
    rownames(cor_mat) <- demo_labels
    colnames(cor_mat) <- top_parties

    for (i in seq_along(demo_cols)) {
      x <- as_num(df_plain[[demo_cols[i]]])
      for (j in seq_along(top_parties)) {
        y <- party_pct[, top_parties[j]]
        valid <- is.finite(x) & is.finite(y)
        if (sum(valid) > 5) cor_mat[i, j] <- cor(x[valid], y[valid])
      }
    }

    # Truncate long labels
    rn <- rownames(cor_mat)
    rn <- ifelse(nchar(rn) > 30, paste0(substr(rn, 1, 28), "\u2026"), rn)

    plot_ly(z=cor_mat, x=colnames(cor_mat), y=rn, type="heatmap",
            colorscale=list(c(0, "#005BAC"), c(0.5, "#111111"), c(1, "#D50000")),
            zmin=-1, zmax=1,
            hovertemplate="<b>%{y}</b> vs <b>%{x}</b><br>R = %{z:.3f}<extra></extra>",
            colorbar=list(title="R", tickfont=list(color="#FFFFFF"),
                          titlefont=list(color="#FFFFFF"))) |>
      layout(
        xaxis=list(title="", color="#FFFFFF", tickangle=-30, tickfont=list(size=11)),
        yaxis=list(title="", color="#FFFFFF", tickfont=list(size=10), autorange="reversed"),
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
        font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
        margin=list(l=200, r=20, t=10, b=60)
      ) |> config(displayModeBar=FALSE, responsive=TRUE)
  })
}
