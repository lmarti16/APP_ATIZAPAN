# ==========================================================
# MODULO: CLUSTERING — Agrupación de secciones
# ==========================================================

clustering_ui <- function() {
  bslib::nav_panel(
    HTML("<span class='nav-icon'>\U0001F9E9</span>Clusters"),
    div(style="padding:12px;",
        bslib::layout_columns(
          col_widths = c(3, 9),

          # ---- Config ----
          div(class="glass", style="padding:14px;",
              div(class="card-hd",
                  div(class="card-hd-icon", "\U0001F9E9"),
                  div(class="card-hd-text",
                      div(class="t", "Clustering"),
                      div(class="s", "Agrupa secciones similares")
                  )
              ),
              sliderInput("clust_k", "N\u00famero de clusters (k)",
                          min=2, max=8, value=4, step=1),
              hr(),
              div(class="smallHelp", "Variables para clustering:"),
              checkboxGroupInput("clust_vars", NULL,
                                 choices=list(
                                   "Participaci\u00f3n (%)" = "participacion",
                                   "% Ganador (margen)" = "margin_pct",
                                   "Lista nominal" = "lista_nominal",
                                   "Electorado: Gen Z (%)" = "pct_gen_z",
                                   "Electorado: Millennials (%)" = "pct_mill",
                                   "Electorado: Boomers (%)" = "pct_boom",
                                   "Top partido (% votos)" = "top_party_pct"
                                 ),
                                 selected=c("participacion","margin_pct","pct_gen_z","pct_boom")),
              hr(),
              radioButtons("clust_method", "M\u00e9todo",
                           choices=list("K-means"="kmeans","Jer\u00e1rquico"="hclust"),
                           selected="kmeans", inline=TRUE),
              hr(),
              actionButton("clust_run", "\U0001F9E9 Ejecutar clustering", class="btn btn-accent w-100"),
              div(style="height:10px;"),
              uiOutput("clust_status")
          ),

          # ---- Results ----
          div(
            div(class="glass", style="padding:14px; margin-bottom:12px;",
                div(class="card-hd",
                    div(class="card-hd-icon", "\U0001F5FA\uFE0F"),
                    div(class="card-hd-text",
                        div(class="t", "Mapa de clusters"),
                        div(class="s", "Secciones coloreadas por grupo")
                    )
                ),
                leafletOutput("clust_map", height="480px")
            ),

            bslib::layout_columns(
              col_widths = c(6, 6),
              div(class="glass", style="padding:14px;",
                  div(class="card-hd",
                      div(class="card-hd-icon", "\U0001F4CA"),
                      div(class="card-hd-text",
                          div(class="t", "Perfil de clusters"),
                          div(class="s", "Promedios por grupo (radar)")
                      )
                  ),
                  plotlyOutput("clust_radar", height="360px")
              ),
              div(class="glass", style="padding:14px;",
                  div(class="card-hd",
                      div(class="card-hd-icon", "\U0001F4CB"),
                      div(class="card-hd-text",
                          div(class="t", "Resumen por cluster"),
                          div(class="s", "Tama\u00f1o, participaci\u00f3n, ganador")
                      )
                  ),
                  DTOutput("clust_summary_tbl")
              )
            ),

            div(style="height:12px;"),
            div(class="glass", style="padding:14px;",
                div(class="card-hd",
                    div(class="card-hd-icon", "\U0001F4CB"),
                    div(class="card-hd-text",
                        div(class="t", "Tabla de secciones"),
                        div(class="s", "Todas las secciones con su cluster asignado")
                    )
                ),
                DTOutput("clust_detail_tbl")
            )
          )
        )
    )
  )
}

clustering_server <- function(input, output, session, has_applied, applied, df_applied, dl_applied, df_metrics) {

  clust_result <- reactiveVal(NULL)

  build_feature_matrix <- function(df, key, selected_vars) {
    df_plain <- as.data.frame(st_drop_geometry(df))
    n <- nrow(df_plain)
    features <- list()
    labels <- character(0)

    if ("participacion" %in% selected_vars) {
      c_tot <- total_col(df, key); c_ln <- ln_col(df, key)
      tot <- if (!is.na(c_tot) && c_tot %in% names(df)) as_num(df[[c_tot]]) else rep(NA_real_, n)
      ln  <- if (!is.na(c_ln) && c_ln %in% names(df)) as_num(df[[c_ln]]) else rep(NA_real_, n)
      features[["participacion"]] <- ifelse(is.finite(ln) & ln > 0, tot / ln, NA_real_)
      labels <- c(labels, "Participaci\u00f3n")
    }
    if ("margin_pct" %in% selected_vars) {
      gv <- group_votes_matrix(df, key, "DISTRIBUIDO"); G <- gv$G
      if (!is.null(G) && ncol(G) >= 2) {
        top1 <- apply(G, 1, function(r) sort(r, decreasing=TRUE)[1L])
        top2 <- apply(G, 1, function(r) { s <- sort(r, decreasing=TRUE); if (length(s)>=2) s[2L] else 0 })
        rt <- rowSums(G, na.rm=TRUE)
        features[["margin_pct"]] <- ifelse(rt > 0, (top1-top2)/rt, NA_real_)
        labels <- c(labels, "Margen")
      }
    }
    if ("lista_nominal" %in% selected_vars) {
      c_ln <- ln_col(df, key)
      if (!is.na(c_ln) && c_ln %in% names(df)) {
        features[["lista_nominal"]] <- as_num(df[[c_ln]])
        labels <- c(labels, "Lista nominal")
      }
    }
    if ("top_party_pct" %in% selected_vars) {
      gv <- group_votes_matrix(df, key, "DISTRIBUIDO"); G <- gv$G
      if (!is.null(G) && ncol(G) > 0) {
        top1 <- apply(G, 1, max); rt <- rowSums(G, na.rm=TRUE)
        features[["top_party_pct"]] <- ifelse(rt > 0, top1/rt, NA_real_)
        labels <- c(labels, "% Top partido")
      }
    }
    gen_map <- list(pct_gen_z="PCT_LISTA_GEN_Z", pct_mill="PCT_LISTA_MILLENNIALS", pct_boom="PCT_LISTA_BOOMERS")
    gen_labels <- list(pct_gen_z="Gen Z %", pct_mill="Millennials %", pct_boom="Boomers %")
    for (v in intersect(selected_vars, names(gen_map))) {
      col <- gen_map[[v]]
      if (col %in% names(df_plain)) {
        features[[v]] <- as_num(df_plain[[col]])
        labels <- c(labels, gen_labels[[v]])
      }
    }

    if (length(features) == 0L) return(NULL)
    M <- do.call(cbind, features)
    colnames(M) <- labels
    M
  }

  observeEvent(input$clust_run, {
    req(has_applied())
    df <- df_applied(); key <- applied()$election
    sel_vars <- input$clust_vars %||% character(0)
    validate(need(length(sel_vars) >= 2, "Selecciona al menos 2 variables"))

    M <- build_feature_matrix(df, key, sel_vars)
    validate(need(!is.null(M) && ncol(M) >= 2, "No se pudieron construir las variables"))

    # Handle NAs: impute with column median
    for (j in seq_len(ncol(M))) {
      na_idx <- !is.finite(M[, j])
      if (any(na_idx)) M[na_idx, j] <- median(M[!na_idx, j], na.rm=TRUE)
    }

    # Scale
    M_scaled <- scale(M)
    M_scaled[!is.finite(M_scaled)] <- 0

    k <- input$clust_k %||% 4L

    if (identical(input$clust_method, "hclust")) {
      hc <- hclust(dist(M_scaled), method="ward.D2")
      clusters <- cutree(hc, k=k)
    } else {
      set.seed(42)
      km <- kmeans(M_scaled, centers=k, nstart=25, iter.max=100)
      clusters <- km$cluster
    }

    clust_result(list(clusters=clusters, M=M, M_scaled=M_scaled, k=k, var_labels=colnames(M)))
    showNotification("Clustering completado \u2705", type="message", duration=1.2)
  }, ignoreInit=TRUE)

  has_clust <- reactive(!is.null(clust_result()))

  output$clust_status <- renderUI({
    if (!has_clust()) return(div(class="status-badge idle", HTML("<b>Sin ejecutar</b>")))
    cr <- clust_result()
    div(class="status-badge ok",
        HTML(paste0("<b>", cr$k, " clusters</b> \u00b7 ", length(cr$clusters), " secciones")))
  })

  # ---- MAP ----
  CLUSTER_COLORS <- c("#D50000","#005BAC","#2EAD4A","#FF6A00","#7A013A","#00B5E2","#FFD200","#EC008C")

  output$clust_map <- renderLeaflet({ create_base_leaflet() })

  observe({
    req(has_clust(), has_applied())
    cr <- clust_result(); df <- df_applied(); req(nrow(df) == length(cr$clusters))
    proxy <- leafletProxy("clust_map", session=session) |> clearShapes() |> clearMarkers() |> clearControls()
    proxy <- restore_map_controls(proxy)

    cls <- cr$clusters
    cols <- CLUSTER_COLORS[((cls - 1) %% length(CLUSTER_COLORS)) + 1]

    lab <- paste0(
      "<b style='color:", ACCENT, "'>Secci\u00f3n ", df$SECCION, "</b>",
      "<br>Cluster: <b style='color:", cols, "'>", cls, "</b>",
      vapply(seq_len(ncol(cr$M)), function(j) {
        paste0("<br>", cr$var_labels[j], ": <b>", formatC(cr$M[,j], format="f", digits=2), "</b>")
      }, character(nrow(df))) |> apply(2, paste0, collapse="")
    )

    proxy <- proxy |>
      addPolygons(data=df, color="rgba(255,255,255,.18)", weight=1,
                  fillColor=cols, fillOpacity=0.62,
                  label=lapply(lab, HTML),
                  highlightOptions=highlightOptions(color="#ffffff", weight=2, bringToFront=TRUE))

    # Legend
    u_cls <- sort(unique(cls))
    leg_cols <- CLUSTER_COLORS[((u_cls - 1) %% length(CLUSTER_COLORS)) + 1]
    proxy <- proxy |>
      addLegend(position="bottomright", colors=leg_cols,
                labels=paste0("Cluster ", u_cls), title="Clusters", opacity=0.9)

    proxy <- add_dl_layer(proxy, dl_applied())
    proxy <- add_layers_control(proxy)
    proxy <- hideGroup(proxy, setdiff(names(BASEMAPS), BASEMAP_DEFAULT))
    bb <- st_bbox(df)
    proxy |> fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
  })

  # ---- Radar / parallel coords ----
  output$clust_radar <- renderPlotly({
    validate(need(has_clust(), "Ejecuta el clustering"))
    cr <- clust_result()
    # Compute per-cluster means of scaled values
    cls <- cr$clusters; M <- cr$M_scaled
    k <- cr$k; vars <- cr$var_labels

    p <- plot_ly(type="scatterpolar", fill="toself")
    for (cl in seq_len(k)) {
      idx <- cls == cl
      if (sum(idx) == 0) next
      means <- colMeans(M[idx, , drop=FALSE], na.rm=TRUE)
      # Close the polygon
      r <- c(means, means[1L])
      theta <- c(vars, vars[1L])
      col <- CLUSTER_COLORS[((cl - 1) %% length(CLUSTER_COLORS)) + 1]
      p <- p |> add_trace(r=r, theta=theta, name=paste0("Cluster ", cl),
                          line=list(color=col, width=2.5),
                          fillcolor=paste0(col, "33"),
                          marker=list(color=col, size=6))
    }
    p |> layout(
      polar=list(
        bgcolor="rgba(0,0,0,0)",
        radialaxis=list(visible=TRUE, color="rgba(255,255,255,.30)", gridcolor="rgba(255,255,255,.08)"),
        angularaxis=list(color="#FFFFFF", gridcolor="rgba(255,255,255,.08)")
      ),
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
      legend=list(orientation="h", x=0, y=-0.15, font=list(color="#FFFFFF")),
      margin=list(l=60, r=60, t=40, b=60)
    ) |> config(displayModeBar=FALSE, responsive=TRUE)
  })

  # ---- Summary table ----
  output$clust_summary_tbl <- renderDT({
    validate(need(has_clust() && has_applied(), "Ejecuta el clustering"))
    cr <- clust_result(); df <- df_applied(); key <- applied()$election

    cls <- cr$clusters
    rows <- lapply(sort(unique(cls)), function(cl) {
      idx <- cls == cl
      df_sub <- df[idx, ]
      c_tot <- total_col(df_sub, key); c_ln <- ln_col(df_sub, key)
      tot <- if (!is.na(c_tot) && c_tot %in% names(df_sub)) sum(as_num(df_sub[[c_tot]]), na.rm=TRUE) else NA_real_
      ln  <- if (!is.na(c_ln) && c_ln %in% names(df_sub)) sum(as_num(df_sub[[c_ln]]), na.rm=TRUE) else NA_real_
      part <- if (is.finite(tot) && is.finite(ln) && ln > 0) tot/ln else NA_real_
      w <- winner_by_row(df_sub, key, "DISTRIBUIDO")
      top_party <- names(sort(table(w), decreasing=TRUE))[1L] %||% NA_character_

      data.frame(CLUSTER=cl, SECCIONES=sum(idx), TOTAL_VOTOS=tot,
                 LISTA_NOMINAL=ln, PARTICIPACION=part,
                 GANADOR_MODA=if (is.na(top_party)) "\u2014" else paste0(party_logo_inline(top_party,"15px"),"<b>",top_party,"</b>"),
                 stringsAsFactors=FALSE)
    })
    d <- do.call(rbind, rows)

    datatable(d, rownames=FALSE, escape=FALSE,
              options=list(dom="t", pageLength=10, ordering=FALSE,
                           initComplete=dt_dark_init)) |>
      formatRound(c("TOTAL_VOTOS","LISTA_NOMINAL"), digits=0) |>
      formatPercentage("PARTICIPACION", digits=2)
  })

  # ---- Detail table ----
  output$clust_detail_tbl <- renderDT({
    validate(need(has_clust(), "Ejecuta el clustering"))
    cr <- clust_result(); df <- df_applied()
    d <- data.frame(SECCION=df$SECCION, CLUSTER=cr$clusters, stringsAsFactors=FALSE)
    # Add raw feature values
    for (j in seq_len(ncol(cr$M))) {
      d[[cr$var_labels[j]]] <- round(cr$M[,j], 4)
    }
    d <- d[order(d$CLUSTER, d$SECCION), ]

    datatable(d, rownames=FALSE, filter="top",
              extensions="Buttons",
              options=list(dom="Bfrtip", scrollX=TRUE, pageLength=20,
                           buttons=c("copy","csv","excel"),
                           initComplete=dt_dark_init))
  })
}
