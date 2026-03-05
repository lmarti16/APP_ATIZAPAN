# ==========================================================
# MODULO: EXPLORAR
# ==========================================================

explorar_ui <- function() {
  bslib::nav_panel(
    HTML("<span class='nav-icon'>\U0001F5FA\uFE0F</span>Explorar"),
    div(style="padding:12px 12px 0 12px;",
        div(class="kpiRow",
            div(class="glass kpi", uiOutput("kpi1")),
            div(class="glass kpi", uiOutput("kpi2")),
            div(class="glass kpi", uiOutput("kpi3"))
        )
    ),
    div(style="padding:12px;",
        bslib::layout_columns(
          col_widths = c(7,5),
          div(class="glass", style="padding:12px;",
              div(class="card-hd",
                  div(class="card-hd-icon","\U0001F5FA\uFE0F"),
                  div(class="card-hd-text",
                      div(class="t","Mapa"),
                      div(class="s", uiOutput("map_subtitle"))
                  )
              ),
              leafletOutput("map", height=520)
          ),
          div(class="glass", style="padding:12px;",
              div(class="card-hd",
                  div(class="card-hd-icon","\U0001F4CA"),
                  div(class="card-hd-text",
                      div(class="t","Resultados"),
                      div(class="s", uiOutput("bar_subtitle"))
                  ),
                  div(class="card-hd-right",
                      bslib::input_switch("bar_is_cand","Candidaturas", value=FALSE)
                  )
              ),
              plotlyOutput("bar", height=520)
          )
        )
    ),
    div(style="padding:0 12px 12px 12px;",
        div(class="glass", style="padding:12px;",
            div(class="card-hd",
                div(class="card-hd-icon","\U0001F4CB"),
                div(class="card-hd-text",
                    div(class="t","Tabla por secci\u00f3n"),
                    div(class="s","M\u00e9tricas + top votos seg\u00fan vista.")
                ),
                div(class="card-hd-right",
                    div(class="seg-control",
                        radioButtons("table_view",NULL,
                                     choices=list("Distribuido"="DISTRIBUIDO","Puro"="PURO","Candidaturas"="CAND"),
                                     selected="DISTRIBUIDO", inline=TRUE)
                    )
                )
            ),
            DTOutput("tbl")
        )
    )
  )
}

explorar_server <- function(input, output, session, has_applied, applied, df_applied, dl_applied, df_metrics) {

  winner_vt <- reactive(toupper(input$winner_vote_type %||% "DISTRIBUIDO"))

  # ---- KPIs ----
  output$kpi1 <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Resumen"))
    x       <- df_metrics()
    tot_sum <- if (!all(is.na(x$tot))) sum(x$tot,na.rm=TRUE) else NA_real_
    ln_sum  <- if (!all(is.na(x$ln)))  sum(x$ln, na.rm=TRUE) else NA_real_
    cas_sum <- if (!all(is.na(x$cas))) sum(x$cas,na.rm=TRUE) else NA_real_
    part_pp <- if (is.finite(ln_sum)&&ln_sum>0&&is.finite(tot_sum)) 100*tot_sum/ln_sum else NA_real_
    bar_w   <- if (is.finite(part_pp)) paste0(min(max(part_pp,0),100),"%") else "0%"
    tagList(
      div(class="t","Secci\u00f3n / Votos"),
      div(class="v", paste0(fmt_int(nrow(x$df))," secc.")),
      div(class="s", HTML(paste0(
        "Votos: <b>",fmt_int(tot_sum),"</b>",
        " \u00b7 LN: <b>",fmt_int(ln_sum),"</b>",
        "<br>Part: <b>",ifelse(is.finite(part_pp),paste0(formatC(part_pp,format="f",digits=2),"%"),"\u2014"),"</b>",
        " \u00b7 Casillas: <b>",fmt_int(cas_sum),"</b>"
      ))),
      div(class="kpi-bar-track",
          div(class="kpi-bar-fill", style=paste0("width:",bar_w,";")))
    )
  })

  output$kpi2 <- renderUI({
    if (!has_applied()) return(kpi_placeholder("Ganador \u00b7 2\u00ba lugar"))
    ap  <- applied(); x <- df_metrics()
    tot <- totals_for_view(x$df, ap$election, ap$winner_vote_type %||% "DISTRIBUIDO")
    top <- top2_from_totals(tot)
    if (!nzchar(top$w1 %||%"")||!is.finite(top$v1)) return(kpi_placeholder("Ganador \u00b7 2\u00ba lugar","Sin ranking"))
    margin <- if (is.finite(top$v2)) top$v1-top$v2 else NA_real_
    total_v <- if (is.finite(top$v2)) top$v1+top$v2 else top$v1
    bar_w   <- if (is.finite(total_v)&&total_v>0) paste0(min(100*top$v1/total_v,100),"%") else "50%"
    tagList(
      div(class="t","Ganador \u00b7 2\u00ba lugar"),
      div(class="v", HTML(paste0(party_logo_inline(top$w1,"22px"), "<b>",htmltools::htmlEscape(top$w1),"</b> \u00b7 ",fmt_int(top$v1)))),
      div(class="s", if (is.finite(top$v2)) {
        HTML(paste0(party_logo_inline(top$w2,"15px"),"2\u00ba: <b>",htmltools::htmlEscape(top$w2),"</b> \u00b7 ",
                    fmt_int(top$v2)," | margen: <b>",fmt_int(margin),"</b>"))
      } else "Sin 2\u00ba lugar"),
      div(class="kpi-bar-track",
          div(class="kpi-bar-fill", style=paste0("width:",bar_w,";background:",
              if (top$w1 %in% names(party_colors)) party_colors[[top$w1]] else ACCENT,";")))
    )
  })

  output$kpi3 <- renderUI({
    if (!has_applied()) return(kpi_placeholder("PRI \u00b7 PURO vs DISTRIBUIDO"))
    ap       <- applied(); x <- df_metrics()
    tot_puro <- totals_for_view(x$df,ap$election,"PURO")
    tot_dist <- totals_for_view(x$df,ap$election,"DISTRIBUIDO")
    pri_puro <- as.numeric(tot_puro[["PRI"]] %||% NA_real_)
    pri_dist <- as.numeric(tot_dist[["PRI"]] %||% NA_real_)
    if (!is.finite(pri_puro)&&!is.finite(pri_dist)) return(kpi_placeholder("PRI \u00b7 PURO vs DISTRIBUIDO","No hay columnas PRI"))
    pri_puro <- pri_puro %||% 0; pri_dist <- pri_dist %||% 0
    aporte   <- pri_dist - pri_puro
    share    <- if (is.finite(pri_dist)&&pri_dist>0) aporte/pri_dist else NA_real_
    bar_w    <- if (is.finite(pri_dist)&&pri_dist>0) paste0(min(100*pri_puro/pri_dist,100),"%") else "0%"
    tagList(
      div(class="t","PRI \u00b7 PURO vs DISTRIBUIDO"),
      div(class="v", HTML(paste0(party_logo_inline("PRI","22px"),"<b>",fmt_int(pri_puro),"</b> | <b>",fmt_int(pri_dist),"</b>"))),
      div(class="s", HTML(paste0(
        "Aporte coalici\u00f3n: <b>",fmt_signed_int(aporte),"</b>",
        if (is.finite(share)) paste0(" (<b>",formatC(100*share,format="f",digits=1),"%</b> del distrib.)") else ""
      ))),
      div(class="kpi-bar-track",
          div(class="kpi-bar-fill", style=paste0("width:",bar_w,";background:",
              if ("PRI" %in% names(party_colors)) party_colors[["PRI"]] else ACCENT,";")))
    )
  })

  # ---- Subtitles ----
  output$map_subtitle <- renderUI({
    if (!has_applied()) return(div(class="subtitle-badge","\u23f8\ufe0f Presiona GENERAR"))
    ap  <- applied()
    vt_lbl <- switch(ap$winner_vote_type %||% "DISTRIBUIDO",
                     DISTRIBUIDO="Distribuido", PURO="Puro", CAND="Candidaturas", ap$winner_vote_type)
    elec_lbl <- key_label(ap$election)
    txt <- if (isTRUE(ap$map_variable)) {
      cvt <- switch(ap$choro_vote_type %||% "DISTRIBUIDO", DISTRIBUIDO="Distribuido", PURO="Puro", ap$choro_vote_type)
      paste0("Choropleth \u00b7 ",cvt," \u00b7 ",ap$choro_party," \u00b7 ",elec_lbl)
    } else {
      switch(ap$map_view %||% "winner",
             winner     = paste0("Ganador (",vt_lbl,") \u00b7 ",elec_lbl),
             part       = paste0("Participaci\u00f3n (%) \u00b7 ",elec_lbl),
             tot        = paste0("Total votos \u00b7 ",elec_lbl),
             ln         = paste0("Lista nominal \u00b7 ",elec_lbl),
             electorado = {
               ev <- ap$electorado_var %||% ""
               ev_lbl <- names(which(vapply(ELECTORADO_CATALOG, function(z) z$col == ev, logical(1))))
               if (length(ev_lbl) == 0L) ev_lbl <- ev
               paste0("Electorado: ",ev_lbl," \u00b7 ",elec_lbl)
             },
             elec_lbl)
    }
    div(class="subtitle-badge", "\U0001F5FA\uFE0F ", txt)
  })

  output$bar_subtitle <- renderUI({
    if (!has_applied()) return(div(class="subtitle-badge","\u23f8\ufe0f Presiona GENERAR"))
    div(class="subtitle-badge",
        "\U0001F4CA ", paste0("Suma en selecci\u00f3n aplicada \u00b7 ", key_label(applied()$election)))
  })

  # ---- MAPA EXPLORAR ----
  output$map <- renderLeaflet({ create_base_leaflet() })

  observe({
    req(has_applied())
    ap <- applied(); x <- df_metrics(); df <- x$df; req(nrow(df)>0L)
    proxy <- leafletProxy("map",session=session) |> clearShapes() |> clearMarkers() |> clearControls()
    proxy <- restore_map_controls(proxy)
    key   <- ap$election

    lab_base <- paste0(
      "<b style='color:#FFFFFF'>Secci\u00f3n:</b> <b style='color:",ACCENT,"'>", df$SECCION,"</b>",
      if (!is.na(x$c_cas)) paste0("<br>Casillas: <b>",fmt_int(df[[x$c_cas]]),"</b>") else "",
      if (!is.na(x$c_tot)) paste0("<br>Total votos: <b>",fmt_int(df[[x$c_tot]]),"</b>") else "",
      if (!is.na(x$c_ln))  paste0("<br>Lista nominal: <b>",fmt_int(df[[x$c_ln]]),"</b>")  else "",
      "<br>Participaci\u00f3n: <b>",fmt_pct(x$part),"</b>"
    )

    if (isTRUE(ap$map_variable)) {
      vt <- ap$choro_vote_type %||% "DISTRIBUIDO"
      gv <- group_votes_matrix(df,key,vt); G <- gv$G
      validate(need(!is.null(G)&&ncol(G)>0L,"Sin matriz para choropleth."))
      pick         <- ap$choro_party %||% colnames(G)[1L]
      if (!(pick %in% colnames(G))) pick <- colnames(G)[1L]
      party_accent <- if (pick %in% names(party_colors)) unname(party_colors[[pick]]) else ACCENT
      v_votes <- as.numeric(G[,pick])
      den <- if (!is.na(x$c_val)&&x$c_val %in% names(df)) as_num(df[[x$c_val]]) else rowSums(G,na.rm=TRUE)
      den[!is.finite(den)|den<=0] <- NA_real_
      scale <- ap$choro_scale %||% "linear"; opac <- ap$choro_opacity %||% 0.65

      if ((ap$choro_metric %||% "pct")=="pct") {
        v   <- ifelse(is.na(den),NA_real_,v_votes/den)
        res <- make_pal_pos(v,scale=scale,accent=party_accent)
        lab <- paste0(lab_base,"<br>",party_logo_inline(pick,"14px"),"<b>",pick," (%):</b> ",fmt_pct(v))
        ttl <- HTML(paste0(party_logo_inline(pick,"14px"),"<b>",pick,"</b> (% v\u00e1lidos)"))
        proxy <- proxy |>
          addPolygons(data=df,color="rgba(255,255,255,.18)",weight=1,
                      fillColor=res$pal(res$values),fillOpacity=opac,
                      label=lapply(lab,HTML),
                      highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE)) |>
          addLegend(position="bottomright",pal=res$pal,values=res$values,title=ttl,opacity=0.9)
      } else {
        v   <- v_votes
        res <- make_pal_pos(v,scale=scale,accent=party_accent)
        lab <- paste0(lab_base,"<br>",party_logo_inline(pick,"14px"),"<b>",pick," (votos):</b> ",fmt_int(v))
        ttl <- HTML(paste0(party_logo_inline(pick,"14px"),"<b>",pick,"</b> (votos)"))
        proxy <- proxy |>
          addPolygons(data=df,color="rgba(255,255,255,.18)",weight=1,
                      fillColor=res$pal(res$values),fillOpacity=opac,
                      label=lapply(lab,HTML),
                      highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE)) |>
          addLegend(position="bottomright",pal=res$pal,values=res$values,title=ttl,opacity=0.9)
      }
    } else {
      mv <- ap$map_view %||% "winner"
      if (mv=="electorado") {
        col_elec   <- ap$electorado_var %||% (ELECTORADO_CHOICES[[1L]] %||% "LISTA_NOMINAL")
        validate(need(col_elec %in% names(df),paste0("Columna '",col_elec,"' no encontrada.")))
        meta_match <- ELECTORADO_AVAILABLE[vapply(ELECTORADO_AVAILABLE,function(z) z$col==col_elec, logical(1))]
        meta       <- if (length(meta_match)) meta_match[[1L]] else list(tipo="abs",color=ACCENT)
        lbl_name   <- names(meta_match)[1L] %||% col_elec
        accent_col <- meta$color %||% ACCENT
        v   <- as_num(df[[col_elec]])
        res <- make_pal_pos(v,scale=ap$electorado_scale %||% "linear",accent=accent_col)
        fmt_val <- if (meta$tipo=="pct") paste0(formatC(v,format="f",digits=1,big.mark=","),"%") else fmt_int(v)
        lab <- paste0(lab_base,"<br><b>",lbl_name,":</b> ",fmt_val)
        proxy <- proxy |>
          addPolygons(data=df,color="rgba(255,255,255,.18)",weight=1,
                      fillColor=res$pal(res$values),fillOpacity=ap$electorado_opacity %||% 0.70,
                      label=lapply(lab,HTML),
                      highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE)) |>
          addLegend(position="bottomright",pal=res$pal,values=res$values,title=lbl_name,opacity=0.9)
      } else if (mv=="winner") {
        vt <- ap$winner_vote_type %||% "DISTRIBUIDO"
        w  <- winner_by_row(df,key,vt)
        df$WINNER <- w; df$FILL <- vapply(df$WINNER,fill_color_winner,character(1))
        winner_logo_html <- vapply(df$WINNER, function(ww) {
          if (is.na(ww)) return("\u2014")
          paste0(party_logo_inline(ww,"15px"),"<b>",htmltools::htmlEscape(ww),"</b>")
        }, character(1))
        lab <- paste0(lab_base,"<br>Ganador: ",winner_logo_html)
        proxy <- proxy |>
          addPolygons(data=df,color="rgba(255,255,255,.18)",weight=1,
                      fillColor=~FILL,fillOpacity=0.62,
                      label=lapply(lab,HTML),
                      highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE))
        leg_vals <- sort(unique(df$WINNER[!is.na(df$WINNER)]))
        if (length(leg_vals)>0L) {
          leg_cols <- vapply(leg_vals,fill_color_winner,character(1))
          leg_labels <- vapply(leg_vals, function(p) {
            as.character(HTML(paste0(party_logo_inline(p,"13px"), p)))
          }, character(1))
          proxy <- proxy |> addLegend(position="bottomright",colors=as.vector(leg_cols),
                                      labels=leg_labels,
                                      title=paste0("Ganador (",vt,")"),opacity=0.9)
        }
      } else {
        v   <- switch(mv, part=x$part, tot=x$tot, ln=x$ln, x$tot)
        pal <- pal_bw_accent(v,accent=ACCENT)
        ttl <- switch(mv,"part"="Participaci\u00f3n (%)","tot"="Total votos","ln"="Lista nominal","M\u00e9trica")
        lab <- if (mv=="part") paste0(lab_base,"<br><b>",ttl,":</b> ",fmt_pct(v)) else paste0(lab_base,"<br><b>",ttl,":</b> ",fmt_int(v))
        proxy <- proxy |>
          addPolygons(data=df,color="rgba(255,255,255,.18)",weight=1,
                      fillColor=pal(v),fillOpacity=0.62,
                      label=lapply(lab,HTML),
                      highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE)) |>
          addLegend(position="bottomright",pal=pal,values=v,title=ttl,opacity=0.9)
      }
    }
    proxy <- add_dl_layer(proxy,dl_applied())
    proxy <- add_layers_control(proxy)
    proxy <- hideGroup(proxy,setdiff(names(BASEMAPS),BASEMAP_DEFAULT))
    bb    <- st_bbox(df)
    proxy |> fitBounds(bb["xmin"],bb["ymin"],bb["xmax"],bb["ymax"])
  })

  # ---- BARRAS ----
  output$bar <- renderPlotly({
    validate(need(has_applied(),"Presiona GENERAR"))
    ap  <- applied(); x <- df_metrics(); df <- x$df
    validate(need(nrow(df)>0L,"Sin datos"))
    key <- ap$election; vt <- ap$winner_vote_type %||% "DISTRIBUIDO"

    if (isTRUE(input$bar_is_cand)) {
      cols <- vote_cols_raw(df,key,"CAND")
      validate(need(length(cols)>0L,"Sin columnas CAND_"))
      M      <- safe_num_matrix(df,cols)
      totals <- colSums(M,na.rm=TRUE)
      keep   <- which(totals>0L)
      validate(need(length(keep)>0L,"CAND_ sin votos"))
      cols   <- cols[keep]; totals <- totals[keep]
      ord    <- order(totals,decreasing=TRUE)[seq_len(min(18L,length(totals)))]
      cols   <- cols[ord]; totals <- totals[ord]
      labs   <- make.unique(vapply(cols,party_from_col,character(1),key=key,vote_type="CAND"))
      dd     <- data.frame(lbl=labs,votos=as.numeric(totals),stringsAsFactors=FALSE)
      dd$lbl <- factor(dd$lbl,levels=rev(dd$lbl))
      cols_bar <- vapply(as.character(dd$lbl),function(l) {
        p <- guess_party_from_cand_label(l)
        if (!is.na(p)&&p %in% names(party_colors)) unname(party_colors[[p]]) else "#6B7280"
      },character(1))
      p <- plot_ly(dd, x=~votos, y=~lbl, type="bar", orientation="h",
                   marker=list(color=cols_bar, line=list(color="rgba(255,255,255,.18)", width=0.8), opacity=0.92),
                   text=~paste0("<b>",lbl,"</b><br>Votos: <b>",fmt_int(votos),"</b>"),
                   hoverinfo="text") |>
        layout(
          xaxis=list(title="Votos", color="#FFFFFF", gridcolor="rgba(255,255,255,.07)", tickfont=list(color="#FFFFFF"), zeroline=FALSE),
          yaxis=list(title="", showticklabels=FALSE, ticklen=0, color="#FFFFFF"),
          hoverlabel=list(bgcolor="rgba(5,8,14,.95)", bordercolor="rgba(255,255,255,.18)",
                          font=list(color="#FFFFFF", size=12, family="Inter, system-ui, sans-serif")),
          margin=list(l=60, r=20, b=40, t=10),
          paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
          font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"), showlegend=FALSE
        ) |> config(displayModeBar=FALSE, responsive=TRUE)
      add_logos_to_plotly_h(p, levels(dd$lbl), size=0.052)
    } else {
      gv     <- group_votes_matrix(df,key,vt); G <- gv$G
      validate(need(!is.null(G)&&ncol(G)>0L,"Sin columnas de partidos"))
      totals <- colSums(G,na.rm=TRUE); totals <- totals[totals>0L]
      validate(need(length(totals)>0L,"Sin votos"))
      ord  <- order(totals,decreasing=TRUE)
      keep <- names(totals)[ord][seq_len(min(14L,length(ord)))]
      dd   <- data.frame(lbl=keep,votos=as.numeric(totals[keep]),stringsAsFactors=FALSE)
      dd$lbl <- factor(dd$lbl,levels=rev(dd$lbl))
      cols_bar <- vapply(as.character(dd$lbl),function(p) {
        if (p %in% names(party_colors)) unname(party_colors[[p]]) else fill_color_winner(p)
      },character(1))
      p <- plot_ly(dd, x=~votos, y=~lbl, type="bar", orientation="h",
                   marker=list(color=cols_bar, line=list(color="rgba(255,255,255,.18)", width=0.8), opacity=0.92),
                   text=~paste0("<b>",lbl,"</b><br>Votos: <b>",fmt_int(votos),"</b>"),
                   hoverinfo="text") |>
        layout(
          xaxis=list(title="Votos", color="#FFFFFF", gridcolor="rgba(255,255,255,.07)", tickfont=list(color="#FFFFFF"), zeroline=FALSE),
          yaxis=list(title="", showticklabels=FALSE, ticklen=0, color="#FFFFFF"),
          hoverlabel=list(bgcolor="rgba(5,8,14,.95)", bordercolor="rgba(255,255,255,.18)",
                          font=list(color="#FFFFFF", size=12, family="Inter, system-ui, sans-serif")),
          margin=list(l=60, r=20, b=40, t=10),
          paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
          font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"), showlegend=FALSE
        ) |> config(displayModeBar=FALSE, responsive=TRUE)
      add_logos_to_plotly_h(p, levels(dd$lbl), size=0.058)
    }
  })

  # ---- TABLE ----
  tbl_data <- reactive({
    validate(need(has_applied(),"Presiona GENERAR"))
    ap <- applied(); x <- df_metrics(); df <- x$df; key <- ap$election
    vt_tbl <- input$table_view %||% "DISTRIBUIDO"
    out <- data.frame(
      SECCION      = df$SECCION,
      CASILLAS     = if (!is.na(x$c_cas)) as_num(df[[x$c_cas]]) else NA_real_,
      TOTAL_VOTOS  = if (!is.na(x$c_tot)) as_num(df[[x$c_tot]]) else NA_real_,
      LISTA_NOMINAL= if (!is.na(x$c_ln))  as_num(df[[x$c_ln]])  else NA_real_,
      PARTICIPACION= x$part,
      stringsAsFactors=FALSE
    )
    if (vt_tbl %in% c("PURO","DISTRIBUIDO")) {
      gv <- group_votes_matrix(df,key,vt_tbl); G <- gv$G
      if (!is.null(G)&&ncol(G)>0L) {
        totals <- colSums(G,na.rm=TRUE)
        keep   <- names(sort(totals,decreasing=TRUE))[seq_len(min(8L,length(totals)))]
        out    <- cbind(out,as.data.frame(G[,keep,drop=FALSE]))
      }
    } else {
      cols <- vote_cols_raw(df,key,"CAND")
      if (length(cols)>0L) {
        M <- safe_num_matrix(df,cols)
        if (!is.null(M)&&ncol(M)>0L) {
          totals <- colSums(M,na.rm=TRUE)
          ord    <- order(totals,decreasing=TRUE)[seq_len(min(8L,length(totals)))]
          M2     <- M[,ord,drop=FALSE]
          colnames(M2) <- make.unique(vapply(cols[ord],party_from_col,character(1),key=key,vote_type="CAND"))
          out    <- cbind(out,as.data.frame(M2))
        }
      }
    }
    out[order(out$SECCION),,drop=FALSE]
  })

  output$tbl <- renderDT({
    validate(need(has_applied(),"Presiona GENERAR"))
    d <- tbl_data()
    base_cols <- c("SECCION","CASILLAS","TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION")
    headers_html <- vapply(names(d), function(nm) {
      if (nm %in% base_cols) return(nm)
      logo <- party_logo_inline(nm,"16px")
      if (nzchar(logo)) paste0(logo,nm) else nm
    }, character(1))
    header_cb <- htmltools::withTags(table(
      class="display",
      thead(tr(lapply(headers_html, function(h) th(HTML(h)))))
    ))
    dt <- datatable(
      d, rownames=FALSE, filter="top", container=header_cb, escape=FALSE,
      extensions=c("Buttons"),
      options=list(
        dom="Bfrtip", scrollY="500px", scrollX=TRUE, scrollCollapse=TRUE,
        pageLength=1000, autoWidth=TRUE, searching=TRUE,
        buttons=c("copy","csv","excel","pdf","print","pageLength"),
        initComplete = dt_dark_init
      )
    )
    cols_round <- intersect(c("CASILLAS","TOTAL_VOTOS","LISTA_NOMINAL"),names(d))
    if (length(cols_round)>0L) dt <- dt |> formatRound(cols_round,digits=0)
    if ("PARTICIPACION" %in% names(d)) dt <- dt |> formatPercentage("PARTICIPACION",digits=2)
    dt
  })

  # Return tbl_data for download handler in main app
  tbl_data
}
