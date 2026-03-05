# ==========================================================
# MODULO: TIEMPO
# ==========================================================

tiempo_ui <- function() {
  bslib::nav_panel(
    HTML("<span class='nav-icon'>\U0001F4C8</span>Tiempo"),
    div(style="padding:12px;",
        bslib::layout_columns(
          col_widths = c(4,8),
          div(class="glass", style="padding:14px;",
              div(class="card-hd",
                  div(class="card-hd-icon","\u2699\uFE0F"),
                  div(class="card-hd-text",
                      div(class="t","Configuraci\u00f3n"),
                      div(class="s","Series y mapa usan la selecci\u00f3n de GENERAR")
                  )
              ),
              radioButtons("ts_office","Serie",
                           choices=list("Ayuntamientos"="AYU","Dip. Local"="DL","Ambos"="BOTH"),
                           selected="BOTH", inline=TRUE),
              radioButtons("ts_vote_type","Votos para partidos",
                           choices=list("Distribuido"="DISTRIBUIDO","Puro"="PURO"),
                           selected="DISTRIBUIDO", inline=TRUE),
              radioButtons("ts_party_metric","M\u00e9trica partidos",
                           choices=list("Votos"="votes","% (sobre v\u00e1lidos)"="pct"),
                           selected="pct", inline=TRUE),
              selectizeInput("ts_parties","Partidos a mostrar (vac\u00edo = Top N)",
                             choices=NULL, multiple=TRUE,
                             options=list(plugins=list("remove_button"),
                                          placeholder="Ej. PRI, PAN, MORENA\u2026")),
              sliderInput("ts_top_n","Top N (si no eliges partidos)",min=3,max=15,value=8,step=1),
              checkboxInput("ts_include_other","Incluir OTROS",value=FALSE),
              hr(),
              h6(class="blockTitle","Mapa en el tiempo"),
              div(class="smallHelp", HTML(paste0("Compara <b>Base</b> vs <b>Ref fija</b>: ",key_label(REF_KEY),"."))),
              selectInput("ts_map_election","Elecci\u00f3n (base)",
                          choices=ELECTION_CHOICES, selected=head(elex$key,1L) %||% DEFAULT_ELECTION),
              radioButtons("ts_map_view","Vista (mapa)",
                           choices=list("Participaci\u00f3n (pp)"="participacion","Lista nominal"="lista",
                                        "Total votos"="mas_votantes","Choropleth (Partido)"="choro_party"),
                           selected="choro_party", inline=TRUE),
              radioButtons("ts_delta_scale","Escala (\u0394)",
                           choices=list("Lineal"="linear","Cuantiles"="quantile"),
                           selected="linear", inline=TRUE),
              sliderInput("ts_map_opacity","Opacidad",min=0.20,max=0.90,value=0.70,step=0.05),
              conditionalPanel(
                condition = "input.ts_map_view == 'choro_party'",
                uiOutput("ui_ts_choro_party"),
                radioButtons("ts_choro_metric","M\u00e9trica (partido)",
                             choices=list("Votos (\u0394)"="votes","% v\u00e1lidos (pp)"="pct"),
                             selected="votes", inline=TRUE)
              ),
              div(style="height:8px;"),
              downloadButton("download_ts_parties_csv","Descargar serie partidos (CSV)", class="btn btn-outline-light btn-sm"),
              div(style="height:6px;"),
              downloadButton("download_ts_metrics_csv","Descargar serie m\u00e9tricas (CSV)", class="btn btn-outline-light btn-sm")
          ),
          div(
            div(class="glass", style="padding:14px;",
                div(class="card-hd",
                    div(class="card-hd-icon","\U0001F4C8"),
                    div(class="card-hd-text",
                        div(class="t","Partidos en el tiempo"),
                        div(class="s","Tendencia por elecci\u00f3n en la selecci\u00f3n aplicada.")
                    )
                ),
                withSpinner(plotlyOutput("ts_party_plot", height="360px"), type=5, size=1.0)
            ),
            div(style="height:12px;"),
            div(class="glass-card p-3",
                div(class="card-hd",
                    div(class="card-hd-icon","\U0001F5FA\uFE0F"),
                    div(class="card-hd-text",
                        div(class="t","Mapa en el tiempo"),
                        div(class="s",paste0("\u0394 = Ref fija \u2212 Base \u00b7 Ref: ",key_label(REF_KEY)))
                    )
                ),
                leafletOutput("map_time", height=420)
            )
          )
        ),
        div(style="height:12px;"),
        bslib::layout_columns(
          col_widths = c(6,6),
          div(class="glass", style="padding:14px;",
              div(class="card-hd",
                  div(class="card-hd-icon","\U0001F4CA"),
                  div(class="card-hd-text",
                      div(class="t","Votantes / Lista nominal"),
                      div(class="s","Evoluci\u00f3n de m\u00e9tricas por elecci\u00f3n")
                  ),
                  div(class="card-hd-right",
                      checkboxGroupInput("ts_metrics",NULL,
                                         choices=list("Votos"="total","LN"="lista",
                                                      "Casillas"="casillas","V\u00e1lidos"="validos","Nulos"="nulos"),
                                         selected=c("total","lista"), inline=TRUE)
                  )
              ),
              withSpinner(plotlyOutput("ts_metrics_plot", height="300px"), type=5, size=1.0)
          ),
          div(class="glass", style="padding:14px;",
              div(class="card-hd",
                  div(class="card-hd-icon","\U0001F4AF"),
                  div(class="card-hd-text",
                      div(class="t","Participaci\u00f3n en el tiempo"),
                      div(class="s","% sobre lista nominal por elecci\u00f3n")
                  )
              ),
              withSpinner(plotlyOutput("ts_particip_plot", height="300px"), type=5, size=1.0)
          )
        )
    )
  )
}

tiempo_server <- function(input, output, session, has_applied, applied, df_applied, dl_applied) {

  ts_keys <- reactive({
    req(has_applied())
    sel  <- input$ts_office %||% "BOTH"
    keys <- elex$key
    if (sel %in% c("AYU","DL")) keys <- keys[grepl(paste0("^",sel,"_"),keys)]
    keys
  })

  observeEvent(list(has_applied(),input$ts_vote_type,input$ts_include_other,input$ts_office), {
    req(has_applied())
    keys <- ts_keys(); df <- df_applied(); vt <- input$ts_vote_type %||% "DISTRIBUIDO"
    out <- character(0)
    for (k in keys) { gv <- group_votes_matrix(df,k,vt); if (!is.null(gv$G)) out <- unique(c(out,colnames(gv$G))) }
    out <- out[!is.na(out)&nzchar(out)]
    if (!isTRUE(input$ts_include_other)) out <- setdiff(out,"OTROS")
    out <- sort(unique(out))
    updateSelectizeInput(session,"ts_parties",
                         choices=as.list(setNames(out,out)),
                         selected=intersect(c("PRI","PAN","MORENA","MC","PVEM","PT","PRD"),out),
                         server=TRUE)
  }, ignoreInit=FALSE)

  output$ui_ts_choro_party <- renderUI({
    validate(need(has_applied(),"Presiona GENERAR"))
    keys <- ts_keys(); df <- df_applied(); vt <- input$ts_vote_type %||% "DISTRIBUIDO"
    out <- character(0)
    for (k in keys) { gv <- group_votes_matrix(df,k,vt); if (!is.null(gv$G)) out <- unique(c(out,colnames(gv$G))) }
    out <- sort(unique(setdiff(out,"OTROS")))
    validate(need(length(out)>0L,"Sin partidos"))
    def <- if ("PRI" %in% out) "PRI" else out[1L]
    selectInput("ts_choro_party","Partido",choices=out,selected=def)
  })

  ts_party_series <- reactive({
    validate(need(has_applied(),"Presiona GENERAR"))
    df <- df_applied(); keys <- ts_keys()
    vt <- input$ts_vote_type %||% "DISTRIBUIDO"; metric <- input$ts_party_metric %||% "pct"
    sel <- toupper(input$ts_parties %||% character(0))
    if (!isTRUE(input$ts_include_other)) sel <- setdiff(sel,"OTROS")
    if (length(sel)==0L) {
      acc <- list()
      for (k in keys) {
        gv <- group_votes_matrix(df,k,vt); if (is.null(gv$G)) next
        tot <- colSums(gv$G,na.rm=TRUE)
        if (!isTRUE(input$ts_include_other)) tot <- tot[names(tot)!="OTROS"]
        for (nm in names(tot)) acc[[nm]] <- (acc[[nm]] %||% 0)+as.numeric(tot[[nm]])
      }
      n_top <- as.integer(input$ts_top_n %||% 8L)
      ord   <- order(unlist(acc),decreasing=TRUE)
      sel   <- names(acc)[ord][seq_len(min(n_top,length(ord)))]
    }
    rows <- list()
    for (k in keys) {
      gv <- group_votes_matrix(df,k,vt); G <- gv$G; if (is.null(G)||ncol(G)==0L) next
      c_val <- valid_col(df,k)
      den   <- if (!is.na(c_val)&&c_val %in% names(df)) sum(as_num(df[[c_val]]),na.rm=TRUE) else sum(G,na.rm=TRUE)
      if (!is.finite(den)||den<=0) den <- NA_real_
      tot   <- colSums(G,na.rm=TRUE)
      for (p in sel) {
        v <- if (p %in% names(tot)) as.numeric(tot[[p]]) else 0
        y <- if (metric=="pct") { if (is.finite(den)&&den>0) v/den else NA_real_ } else v
        rows[[length(rows)+1L]] <- data.frame(key=k,label=key_label(k),party=p,value=y,raw_votes=v,den=den,stringsAsFactors=FALSE)
      }
    }
    out <- do.call(rbind,rows)
    out$label <- factor(out$label,levels=vapply(keys,key_label,character(1)))
    out
  })

  output$ts_party_plot <- renderPlotly({
    validate(need(has_applied(),"Presiona GENERAR"))
    d      <- ts_party_series()
    validate(need(nrow(d)>0L,"Sin serie"))
    metric  <- input$ts_party_metric %||% "pct"
    y_title <- if (metric=="pct") "% (sobre v\u00e1lidos)" else "Votos"
    p <- plot_ly()
    parties <- unique(d$party)
    for (pp in parties) {
      dd       <- d[d$party==pp,,drop=FALSE]
      y        <- if (metric=="pct") 100*dd$value else dd$value
      col_line <- if (pp %in% names(party_colors)) unname(party_colors[[pp]]) else ACCENT
      col_fill <- sub("^(#[A-Fa-f0-9]{6})", "\\1", col_line)
      hover <- if (metric=="pct") {
        paste0("<b>",pp,"</b> \u00b7 ",dd$label,
               "<br>%: ",ifelse(is.na(dd$value),"\u2014",paste0(formatC(100*dd$value,format="f",digits=2),"%")),
               "<br>Votos: ",fmt_int(dd$raw_votes))
      } else {
        paste0("<b>",pp,"</b> \u00b7 ",dd$label, "<br>Votos: ",fmt_int(y))
      }
      p <- p |> add_trace(data=dd, x=~label, y=y, type="scatter",
                          mode="lines+markers", name=pp,
                          line=list(color=col_line, width=2.8, shape="spline", smoothing=0.5),
                          marker=list(size=9, color=col_line, line=list(color="#FFFFFF",width=1.5), symbol="circle"),
                          fill="tozeroy", fillcolor=paste0(col_fill,"1A"),
                          text=hover, hoverinfo="text")
    }
    p |> layout(
      xaxis=list(title="", tickangle=-25, color="#FFFFFF", gridcolor="rgba(255,255,255,.06)", zeroline=FALSE),
      yaxis=list(title=y_title, color="#FFFFFF", gridcolor="rgba(255,255,255,.06)", zeroline=FALSE),
      margin=list(l=55,r=10,b=85,t=10), hovermode="x unified",
      hoverlabel=list(bgcolor="rgba(5,8,14,.95)", bordercolor="rgba(255,255,255,.18)",
                      font=list(color="#FFFFFF", size=12, family="Inter, system-ui, sans-serif")),
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
      legend=list(orientation="h", x=0, y=1.15, font=list(color="#FFFFFF"), bgcolor="rgba(0,0,0,0)")
    ) |> config(displayModeBar=FALSE, responsive=TRUE)
  })

  ts_metrics_series <- reactive({
    validate(need(has_applied(),"Presiona GENERAR"))
    df <- df_applied(); keys <- ts_keys()
    rows <- list()
    for (k in keys) {
      c_tot <- total_col(df,k); c_ln <- ln_col(df,k)
      c_cas <- metric_col(df,k,"CASILLAS"); c_val <- metric_col(df,k,"NUM_VOTOS_VALIDOS"); c_nul <- metric_col(df,k,"NUM_VOTOS_NULOS")
      tot  <- if (!is.na(c_tot)&&c_tot %in% names(df)) sum(as_num(df[[c_tot]]),na.rm=TRUE) else NA_real_
      ln   <- if (!is.na(c_ln) &&c_ln  %in% names(df)) sum(as_num(df[[c_ln]]), na.rm=TRUE) else NA_real_
      cas  <- if (!is.na(c_cas)&&c_cas %in% names(df)) sum(as_num(df[[c_cas]]),na.rm=TRUE) else NA_real_
      val  <- if (!is.na(c_val)&&c_val %in% names(df)) sum(as_num(df[[c_val]]),na.rm=TRUE) else NA_real_
      nul  <- if (!is.na(c_nul)&&c_nul %in% names(df)) sum(as_num(df[[c_nul]]),na.rm=TRUE) else NA_real_
      part <- if (is.finite(tot)&&is.finite(ln)&&ln>0) tot/ln else NA_real_
      rows[[length(rows)+1L]] <- data.frame(key=k,label=key_label(k),total=tot,lista=ln,casillas=cas,validos=val,nulos=nul,participacion=part,stringsAsFactors=FALSE)
    }
    out <- do.call(rbind,rows)
    out$label <- factor(out$label,levels=vapply(keys,key_label,character(1)))
    out
  })

  output$ts_metrics_plot <- renderPlotly({
    validate(need(has_applied(),"Presiona GENERAR"))
    d   <- ts_metrics_series(); validate(need(nrow(d)>0L,"Sin serie"))
    sel <- intersect(input$ts_metrics %||% c("total","lista"), c("total","lista","casillas","validos","nulos"))
    validate(need(length(sel)>0L,"Selecciona al menos una m\u00e9trica"))
    name_map  <- c(total="Total votos", lista="Lista nominal", casillas="Casillas", validos="V\u00e1lidos", nulos="Nulos")
    color_map <- c(total=ACCENT, lista="#00B5E2", casillas="#FFD200", validos="#2EAD4A", nulos="#FF6A00")
    p <- plot_ly()
    for (m in sel) {
      y   <- d[[m]]; col <- color_map[[m]] %||% ACCENT
      p <- p |> add_trace(
        x=d$label, y=y, type="scatter", mode="lines+markers", name=name_map[[m]],
        line=list(color=col, width=2.5, shape="spline", smoothing=0.5),
        marker=list(size=8, color=col, line=list(color="#FFFFFF",width=1.5)),
        fill="tozeroy", fillcolor=paste0(col,"15"),
        text=paste0("<b>",d$label,"</b><br>",name_map[[m]],": <b>",fmt_int(y),"</b>"), hoverinfo="text"
      )
    }
    p |> layout(
      xaxis=list(title="", tickangle=-25, color="#FFFFFF", gridcolor="rgba(255,255,255,.06)", zeroline=FALSE),
      yaxis=list(title="Conteos", color="#FFFFFF", gridcolor="rgba(255,255,255,.06)", zeroline=FALSE),
      margin=list(l=55,r=10,b=85,t=10), hovermode="x unified",
      hoverlabel=list(bgcolor="rgba(5,8,14,.95)", bordercolor="rgba(255,255,255,.18)",
                      font=list(color="#FFFFFF", size=12, family="Inter, system-ui, sans-serif")),
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"),
      legend=list(orientation="h", x=0, y=1.15, font=list(color="#FFFFFF"), bgcolor="rgba(0,0,0,0)")
    ) |> config(displayModeBar=FALSE, responsive=TRUE)
  })

  output$ts_particip_plot <- renderPlotly({
    validate(need(has_applied(),"Presiona GENERAR"))
    d     <- ts_metrics_series(); validate(need(nrow(d)>0L,"Sin serie"))
    hover <- paste0("<b>",d$label,"</b>",
                    "<br>\U0001F4CA Participaci\u00f3n: <b>",fmt_pct(d$participacion),"</b>",
                    "<br>Total votos: ",fmt_int(d$total),
                    "<br>Lista nominal: ",fmt_int(d$lista))
    plot_ly(d, x=~label, y=~I(100*participacion), type="scatter", mode="lines+markers",
            name="Participaci\u00f3n (%)", text=hover, hoverinfo="text",
            line=list(color=ACCENT, width=3, shape="spline", smoothing=0.6),
            marker=list(size=10, color=ACCENT, line=list(color="#FFFFFF",width=2)),
            fill="tozeroy", fillcolor=paste0(ACCENT,"25")) |>
      layout(
        xaxis=list(title="", tickangle=-25, color="#FFFFFF", gridcolor="rgba(255,255,255,.06)", zeroline=FALSE),
        yaxis=list(title="Participaci\u00f3n (%)", color="#FFFFFF", gridcolor="rgba(255,255,255,.06)", zeroline=FALSE, ticksuffix="%"),
        margin=list(l=55,r=10,b=85,t=10), hovermode="x unified",
        hoverlabel=list(bgcolor="rgba(5,8,14,.95)", bordercolor="rgba(255,255,255,.18)",
                        font=list(color="#FFFFFF", size=12, family="Inter, system-ui, sans-serif")),
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
        font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"), showlegend=FALSE
      ) |> config(displayModeBar=FALSE, responsive=TRUE)
  })

  # ---- Downloads ----
  output$download_ts_parties_csv <- downloadHandler(
    filename=function() paste0("serie_partidos_",format(Sys.time(),"%Y%m%d_%H%M"),".csv"),
    content =function(file) write.csv(ts_party_series(),file,row.names=FALSE,fileEncoding="UTF-8")
  )
  output$download_ts_metrics_csv <- downloadHandler(
    filename=function() paste0("serie_metricas_",format(Sys.time(),"%Y%m%d_%H%M"),".csv"),
    content =function(file) write.csv(ts_metrics_series(),file,row.names=FALSE,fileEncoding="UTF-8")
  )

  # ---- MAPA TIEMPO ----
  output$map_time <- renderLeaflet({ create_base_leaflet() })
  outputOptions(output, "map_time", suspendWhenHidden = FALSE)

  observe({
    validate(need(has_applied(),"Presiona GENERAR"))
    df <- df_applied(); req(nrow(df)>0L)
    req(input$ts_map_election, input$ts_map_view, input$ts_delta_scale)
    base_key <- input$ts_map_election; ref_key <- REF_KEY
    view     <- input$ts_map_view %||% "choro_party"; scale <- input$ts_delta_scale %||% "linear"
    proxy <- leafletProxy("map_time",session=session) |> clearShapes() |> clearMarkers() |> clearControls()
    proxy <- restore_map_controls(proxy)
    c_tot_b <- total_col(df,base_key); c_ln_b <- ln_col(df,base_key)
    tot_b   <- if (!is.na(c_tot_b)&&c_tot_b %in% names(df)) as_num(df[[c_tot_b]]) else NA_real_
    ln_b    <- if (!is.na(c_ln_b) &&c_ln_b  %in% names(df)) as_num(df[[c_ln_b]])  else NA_real_
    part_b  <- ifelse(is.finite(ln_b)&ln_b>0, tot_b/ln_b, NA_real_)
    c_tot_r <- total_col(df,ref_key); c_ln_r <- ln_col(df,ref_key)
    tot_r   <- if (!is.na(c_tot_r)&&c_tot_r %in% names(df)) as_num(df[[c_tot_r]]) else NA_real_
    ln_r    <- if (!is.na(c_ln_r) &&c_ln_r  %in% names(df)) as_num(df[[c_ln_r]])  else NA_real_
    part_r  <- ifelse(is.finite(ln_r)&ln_r>0, tot_r/ln_r, NA_real_)
    hdr <- paste0("<b>Secci\u00f3n:</b> <b style='color:",ACCENT,"'>",df$SECCION,"</b>",
                  "<br>Base: <b>",key_label(base_key),"</b>",
                  "<br>Ref fija: <b>",key_label(ref_key),"</b>")
    add_delta_layer <- function(proxy,delta,base_val,ref_val,ttl,fmt_fn,opac) {
      pal <- make_pal_delta(delta,scale=scale)
      lab <- paste0(hdr,"<br><br><b>",ttl,"</b>",
                    "<br>Base: <b>",fmt_fn(base_val),"</b>",
                    "<br>Ref: <b>",fmt_fn(ref_val),"</b>",
                    "<br><b>\u0394:</b> ",if (identical(fmt_fn,fmt_pct)) fmt_signed_pp(delta) else fmt_signed_int(delta))
      proxy |>
        addPolygons(data=df,color="rgba(255,255,255,.22)",weight=1,
                    fillColor=pal$pal(pal$values),fillOpacity=opac,
                    label=lapply(lab,HTML),
                    highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE)) |>
        addLegend(position="bottomright",pal=pal$pal,values=pal$values,
                  title=paste0("\u0394 ",ttl," \u00b7 Ref \u2212 Base"),opacity=0.9)
    }
    opac <- input$ts_map_opacity %||% 0.70
    if (view=="participacion") {
      proxy <- add_delta_layer(proxy,100*(part_r-part_b),part_b,part_r,"Participaci\u00f3n (%)",fmt_pct,opac)
    } else if (view=="lista") {
      proxy <- add_delta_layer(proxy,ln_r-ln_b,ln_b,ln_r,"Lista nominal",fmt_int,opac)
    } else if (view=="mas_votantes") {
      proxy <- add_delta_layer(proxy,tot_r-tot_b,tot_b,tot_r,"Total votos",fmt_int,opac)
    } else {
      req(input$ts_choro_party)
      vt   <- input$ts_vote_type %||% "DISTRIBUIDO"; pick <- input$ts_choro_party; met <- input$ts_choro_metric %||% "votes"
      gv_b <- group_votes_matrix(df,base_key,vt); G_b <- gv_b$G
      gv_r <- group_votes_matrix(df,ref_key,  vt); G_r <- gv_r$G
      validate(need(!is.null(G_b)&&!is.null(G_r),"Sin matrices de votos"))
      if (!(pick %in% colnames(G_b))) pick <- colnames(G_b)[1L]
      votes_b <- as.numeric(G_b[,pick])
      votes_r <- if (pick %in% colnames(G_r)) as.numeric(G_r[,pick]) else rep(0,nrow(df))
      den_b <- if (!is.na(valid_col(df,base_key))) as_num(df[[valid_col(df,base_key)]]) else rowSums(G_b,na.rm=TRUE)
      den_r <- if (!is.na(valid_col(df,ref_key)))  as_num(df[[valid_col(df,ref_key)]])  else rowSums(G_r,na.rm=TRUE)
      den_b[!is.finite(den_b)|den_b<=0] <- NA_real_; den_r[!is.finite(den_r)|den_r<=0] <- NA_real_
      party_accent <- if (pick %in% names(party_colors)) unname(party_colors[[pick]]) else ACCENT
      if (met=="pct") {
        pct_b    <- ifelse(is.na(den_b),NA_real_,votes_b/den_b)
        pct_r    <- ifelse(is.na(den_r),NA_real_,votes_r/den_r)
        delta_pp <- 100*(pct_r-pct_b)
        pal <- make_pal_delta(delta_pp,scale=scale,accent=party_accent)
        lab <- paste0(hdr,"<br><br>",party_logo_inline(pick,"14px"),"<b>",pick,"</b>",
                      "<br>Base %: <b>",fmt_pct(pct_b),"</b>",
                      "<br>Ref %: <b>",fmt_pct(pct_r),"</b>",
                      "<br><b>\u0394:</b> ",fmt_signed_pp(delta_pp))
        proxy |>
          addPolygons(data=df,color="rgba(255,255,255,.22)",weight=1,
                      fillColor=pal$pal(pal$values),fillOpacity=opac,
                      label=lapply(lab,HTML),
                      highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE)) |>
          addLegend(position="bottomright",pal=pal$pal,values=pal$values,
                    title=HTML(paste0(party_logo_inline(pick,"13px"),"\u0394 ",pick," (%, pp)")),opacity=0.9)
      } else {
        delta <- votes_r-votes_b
        pal   <- make_pal_delta(delta,scale=scale,accent=party_accent)
        lab   <- paste0(hdr,"<br><br>",party_logo_inline(pick,"14px"),"<b>",pick,"</b>",
                        "<br>Base votos: <b>",fmt_int(votes_b),"</b>",
                        "<br>Ref votos: <b>",fmt_int(votes_r),"</b>",
                        "<br><b>\u0394:</b> ",fmt_signed_int(delta))
        proxy |>
          addPolygons(data=df,color="rgba(255,255,255,.22)",weight=1,
                      fillColor=pal$pal(pal$values),fillOpacity=opac,
                      label=lapply(lab,HTML),
                      highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE)) |>
          addLegend(position="bottomright",pal=pal$pal,values=pal$values,
                    title=HTML(paste0(party_logo_inline(pick,"13px"),"\u0394 ",pick," (votos)")),opacity=0.9)
      }
    }
    proxy <- add_dl_layer(proxy,dl_applied())
    proxy <- add_layers_control(proxy)
    proxy <- hideGroup(proxy,setdiff(names(BASEMAPS),BASEMAP_DEFAULT))
    bb    <- st_bbox(df)
    proxy |> fitBounds(bb["xmin"],bb["ymin"],bb["xmax"],bb["ymax"])
  })

  outputOptions(output,"map_time",suspendWhenHidden=FALSE)
}
