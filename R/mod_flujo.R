# ==========================================================
# MODULO: FLUJO DE VOTOS — Sankey Diagram
# ==========================================================

flujo_ui <- function() {
  bslib::nav_panel(
    HTML("<span class='nav-icon'>\U0001F500</span>Flujo"),
    div(style="padding:12px;",

        div(class="glass", style="padding:14px; margin-bottom:14px;",
            div(class="card-hd",
                div(class="card-hd-icon", "\U0001F500"),
                div(class="card-hd-text",
                    div(class="t", "An\u00e1lisis de flujo de votos"),
                    div(class="s", "Visualiza c\u00f3mo se mueven los votos entre elecciones (Sankey)")
                )
            ),
            bslib::layout_columns(
              col_widths = c(4, 4, 4),
              selectInput("flujo_from", "Elecci\u00f3n origen", choices=NULL),
              selectInput("flujo_to",   "Elecci\u00f3n destino", choices=NULL),
              radioButtons("flujo_vote_type", "Tipo de voto",
                           choices=list("DISTRIBUIDO"="DISTRIBUIDO","PURO"="PURO"),
                           selected="DISTRIBUIDO", inline=TRUE)
            ),
            div(style="text-align:right;",
                actionButton("flujo_generar", "\U0001F500 Generar flujo", class="btn btn-accent")
            )
        ),

        # ---- Sankey diagram ----
        div(class="glass", style="padding:14px; margin-bottom:14px;",
            div(class="card-hd",
                div(class="card-hd-icon", "\U0001F4CA"),
                div(class="card-hd-text",
                    div(class="t", "Diagrama Sankey"),
                    div(class="s", "Flujo de votos entre partidos de una elecci\u00f3n a otra")
                )
            ),
            plotlyOutput("flujo_sankey", height="520px")
        ),

        # ---- Summary table: vote changes ----
        bslib::layout_columns(
          col_widths = c(6, 6),
          div(class="glass", style="padding:14px;",
              div(class="card-hd",
                  div(class="card-hd-icon", "\U0001F4CB"),
                  div(class="card-hd-text",
                      div(class="t", "Resumen origen"),
                      div(class="s", "Votos por partido en la elecci\u00f3n de origen")
                  )
              ),
              DTOutput("flujo_tbl_from")
          ),
          div(class="glass", style="padding:14px;",
              div(class="card-hd",
                  div(class="card-hd-icon", "\U0001F4CB"),
                  div(class="card-hd-text",
                      div(class="t", "Resumen destino"),
                      div(class="s", "Votos por partido en la elecci\u00f3n destino")
                  )
              ),
              DTOutput("flujo_tbl_to")
          )
        ),

        div(style="height:14px;"),

        # ---- Delta bar chart ----
        div(class="glass", style="padding:14px;",
            div(class="card-hd",
                div(class="card-hd-icon", "\U0001F4CA"),
                div(class="card-hd-text",
                    div(class="t", "Cambio neto por partido"),
                    div(class="s", "Diferencia de votos entre elecciones")
                )
            ),
            plotlyOutput("flujo_delta_bar", height="380px")
        )
    )
  )
}

flujo_server <- function(input, output, session, has_applied, applied, df_applied, df_metrics) {

  # Populate election selectors
  observe({
    req(has_applied())
    keys <- elex$key
    labels <- elex$label
    ch <- as.list(setNames(keys, labels))
    # Default: first election as origin, last as destination
    from_sel <- if (length(keys) >= 2) keys[1L] else keys[1L]
    to_sel   <- if (length(keys) >= 2) tail(keys, 1L) else keys[1L]
    updateSelectInput(session, "flujo_from", choices=ch, selected=from_sel)
    updateSelectInput(session, "flujo_to",   choices=ch, selected=to_sel)
  })

  flujo_data <- reactiveVal(NULL)

  observeEvent(input$flujo_generar, {
    req(has_applied())
    from_key <- input$flujo_from %||% elex$key[1L]
    to_key   <- input$flujo_to %||% tail(elex$key, 1L)
    vt       <- input$flujo_vote_type %||% "DISTRIBUIDO"
    validate(need(from_key != to_key, "Selecciona dos elecciones diferentes"))

    df <- df_applied()

    # Get grouped votes for both elections
    gv_from <- group_votes_matrix(df, from_key, vt)
    gv_to   <- group_votes_matrix(df, to_key,   vt)
    validate(need(!is.null(gv_from$G) && !is.null(gv_to$G), "Sin datos de votos para una o ambas elecciones"))

    G_from <- gv_from$G; G_to <- gv_to$G

    # Totals per party
    tot_from <- colSums(G_from, na.rm=TRUE)
    tot_to   <- colSums(G_to,   na.rm=TRUE)

    # Keep top parties (at least >0 votes)
    tot_from <- tot_from[is.finite(tot_from) & tot_from > 0]
    tot_to   <- tot_to[is.finite(tot_to) & tot_to > 0]
    tot_from <- sort(tot_from, decreasing=TRUE)
    tot_to   <- sort(tot_to, decreasing=TRUE)

    # Limit to top 8 per election for clarity
    parties_from <- names(head(tot_from, 8))
    parties_to   <- names(head(tot_to,   8))
    parties_from <- setdiff(parties_from, "OTROS")
    parties_to   <- setdiff(parties_to,   "OTROS")
    validate(need(length(parties_from) > 0 && length(parties_to) > 0, "Sin partidos"))

    # Estimate flow using section-level winner transition
    # For each section, assign all votes to winner party; track transitions
    winner_from <- winner_by_row(df, from_key, vt)
    winner_to   <- winner_by_row(df, to_key,   vt)

    # Row totals for weighting
    row_tot_from <- rowSums(G_from, na.rm=TRUE)
    row_tot_to   <- rowSums(G_to,   na.rm=TRUE)

    # Build flow matrix: for each section, proportional flow from parties_from to parties_to
    # Use min(row_from, row_to) as the flow weight per section
    flow_mat <- matrix(0, nrow=length(parties_from), ncol=length(parties_to),
                       dimnames=list(parties_from, parties_to))

    # For each section, distribute votes proportionally
    for (r in seq_len(nrow(df))) {
      # Source distribution
      src <- G_from[r, , drop=TRUE]
      src <- src[parties_from]; src[is.na(src)] <- 0
      src_total <- sum(src)
      if (src_total <= 0) next

      # Destination distribution
      dst <- G_to[r, , drop=TRUE]
      dst <- dst[parties_to]; dst[is.na(dst)] <- 0
      dst_total <- sum(dst)
      if (dst_total <= 0) next

      # Proportional flow: fraction of source * fraction of destination * min total
      flow_weight <- min(src_total, dst_total)
      src_pct <- src / src_total
      dst_pct <- dst / dst_total

      # Outer product gives proportional flow
      flow_mat <- flow_mat + outer(src_pct, dst_pct) * flow_weight
    }

    flujo_data(list(
      from_key = from_key, to_key = to_key,
      parties_from = parties_from, parties_to = parties_to,
      tot_from = tot_from, tot_to = tot_to,
      flow_mat = flow_mat
    ))

    showNotification("Flujo calculado \u2705", type="message", duration=1.2)
  }, ignoreInit=TRUE)

  has_flujo <- reactive(!is.null(flujo_data()))

  # ---- Sankey diagram ----
  output$flujo_sankey <- renderPlotly({
    validate(need(has_flujo(), "Configura las elecciones y presiona Generar flujo"))
    fd <- flujo_data()

    pf <- fd$parties_from; pt <- fd$parties_to
    n_from <- length(pf); n_to <- length(pt)

    # Node labels: from parties (suffix with year) and to parties
    from_pk <- parse_key(fd$from_key); to_pk <- parse_key(fd$to_key)
    node_labels <- c(
      paste0(pf, " (", from_pk$year, ")"),
      paste0(pt, " (", to_pk$year, ")")
    )

    # Node colors
    node_colors <- vapply(c(pf, pt), function(p) {
      if (p %in% names(party_colors)) unname(party_colors[[p]]) else "#6B7280"
    }, character(1))

    # Build links
    sources <- integer(0); targets <- integer(0); values <- numeric(0); link_colors <- character(0)
    for (i in seq_along(pf)) {
      for (j in seq_along(pt)) {
        val <- fd$flow_mat[i, j]
        if (is.finite(val) && val > 0) {
          sources <- c(sources, i - 1L)       # 0-indexed
          targets <- c(targets, n_from + j - 1L)
          values  <- c(values, round(val))
          # Link color: semi-transparent source party color
          base_col <- if (pf[i] %in% names(party_colors)) unname(party_colors[[pf[i]]]) else "#6B7280"
          # Convert hex to rgba
          r <- strtoi(substr(base_col, 2, 3), 16)
          g <- strtoi(substr(base_col, 4, 5), 16)
          b <- strtoi(substr(base_col, 6, 7), 16)
          link_colors <- c(link_colors, paste0("rgba(", r, ",", g, ",", b, ",0.40)"))
        }
      }
    }

    validate(need(length(values) > 0, "Sin flujos calculados"))

    plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        pad = 20,
        thickness = 25,
        line = list(color = "rgba(255,255,255,.15)", width = 0.5),
        label = node_labels,
        color = node_colors
      ),
      link = list(
        source = sources,
        target = targets,
        value  = values,
        color  = link_colors
      )
    ) |> layout(
      title = list(
        text = paste0("Flujo de votos: ", key_label(fd$from_key), " \u2192 ", key_label(fd$to_key)),
        font = list(color = "#FFFFFF", size = 14, family = "Inter, system-ui, sans-serif"),
        x = 0.01
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      font = list(color = "#FFFFFF", size = 11, family = "Inter, system-ui, sans-serif"),
      margin = list(l = 10, r = 10, t = 50, b = 20)
    ) |> config(displayModeBar = FALSE, responsive = TRUE)
  })

  # ---- Summary tables ----
  output$flujo_tbl_from <- renderDT({
    validate(need(has_flujo(), "Genera el flujo primero"))
    fd <- flujo_data()
    tot <- fd$tot_from
    total <- sum(tot, na.rm=TRUE)
    d <- data.frame(
      Partido = names(tot),
      Votos   = as.integer(tot),
      Pct     = if (total > 0) tot / total else rep(NA_real_, length(tot)),
      stringsAsFactors = FALSE
    )
    d$Partido <- vapply(d$Partido, function(p) paste0(party_logo_inline(p, "15px"), "<b>", p, "</b>"), character(1))
    datatable(d, rownames=FALSE, escape=FALSE, colnames=c("Partido","Votos","%"),
              options=list(dom="t", pageLength=20, ordering=FALSE,
                           initComplete=dt_dark_init)) |>
      formatRound("Votos", digits=0) |>
      formatPercentage("Pct", digits=2)
  })

  output$flujo_tbl_to <- renderDT({
    validate(need(has_flujo(), "Genera el flujo primero"))
    fd <- flujo_data()
    tot <- fd$tot_to
    total <- sum(tot, na.rm=TRUE)
    d <- data.frame(
      Partido = names(tot),
      Votos   = as.integer(tot),
      Pct     = if (total > 0) tot / total else rep(NA_real_, length(tot)),
      stringsAsFactors = FALSE
    )
    d$Partido <- vapply(d$Partido, function(p) paste0(party_logo_inline(p, "15px"), "<b>", p, "</b>"), character(1))
    datatable(d, rownames=FALSE, escape=FALSE, colnames=c("Partido","Votos","%"),
              options=list(dom="t", pageLength=20, ordering=FALSE,
                           initComplete=dt_dark_init)) |>
      formatRound("Votos", digits=0) |>
      formatPercentage("Pct", digits=2)
  })

  # ---- Delta bar chart ----
  output$flujo_delta_bar <- renderPlotly({
    validate(need(has_flujo(), "Genera el flujo primero"))
    fd <- flujo_data()

    # All parties in either election
    all_parties <- union(names(fd$tot_from), names(fd$tot_to))
    all_parties <- setdiff(all_parties, "OTROS")

    delta <- vapply(all_parties, function(p) {
      v_to   <- if (p %in% names(fd$tot_to))   fd$tot_to[[p]]   else 0
      v_from <- if (p %in% names(fd$tot_from)) fd$tot_from[[p]] else 0
      v_to - v_from
    }, numeric(1))

    # Sort by absolute delta
    ord <- order(abs(delta), decreasing=TRUE)
    all_parties <- all_parties[ord]
    delta <- delta[ord]

    bar_cols <- vapply(seq_along(delta), function(i) {
      if (delta[i] >= 0) "#2EAD4A" else "#D50000"
    }, character(1))

    plot_ly(x=all_parties, y=delta, type="bar",
            marker=list(color=bar_cols, opacity=0.85),
            text=paste0("<b>", all_parties, "</b><br>",
                        ifelse(delta >= 0, "+", ""), fmt_int(delta), " votos"),
            hoverinfo="text") |>
      layout(
        xaxis=list(title="", color="#FFFFFF", gridcolor="rgba(255,255,255,.06)",
                   categoryorder="array", categoryarray=all_parties),
        yaxis=list(title="Diferencia de votos", color="#FFFFFF", gridcolor="rgba(255,255,255,.06)"),
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
        font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
        margin=list(l=60, r=10, t=20, b=60),
        shapes=list(
          list(type="line", x0=-0.5, x1=length(all_parties)-0.5,
               y0=0, y1=0, line=list(color="rgba(255,255,255,.30)", width=1, dash="dot"))
        )
      ) |> config(displayModeBar=FALSE, responsive=TRUE)
  })
}
