# ==========================================================
# MODULO: PAUTA
# ==========================================================

pauta_ui <- function() {
  bslib::nav_panel(
    HTML("<span class='nav-icon'>\U0001F3AF</span>PAUTA"),
    div(style="padding:12px;",
        bslib::layout_columns(
          col_widths = c(4,8),

          div(class="glass", style="padding:14px;",
              div(class="card-hd",
                  div(class="card-hd-icon","\U0001F3AF"),
                  div(class="card-hd-text",
                      div(class="t","PAUTA"),
                      div(class="s","Optimizaci\u00f3n de puntos de activaci\u00f3n")
                  )
              ),
              uiOutput("buf_hereda_info"),
              hr(),
              div(class="smallHelp",style="font-weight:700;margin-bottom:6px;","Modo de optimizaci\u00f3n"),
              div(class="seg-control",
                  radioButtons("buf_mode", NULL,
                               choices=list("\U0001F3C6 Electoral"="electoral",
                                            "\U0001F4CA Poblaci\u00f3n"="inegi",
                                            "\U0001F4F1 Generacional"="gen"),
                               selected="electoral", inline=TRUE)
              ),
              div(style="height:10px;"),

              conditionalPanel(condition="input.buf_mode == 'electoral'", uiOutput("ui_buf_party")),
              conditionalPanel(condition="input.buf_mode == 'gen'", uiOutput("ui_buf_gen")),
              conditionalPanel(
                condition="input.buf_mode == 'inegi'",
                tagList(
                  if (length(EJES_DISPONIBLES)>0L) selectInput("buf_eje","Eje",choices=EJES_DISPONIBLES,selected=EJES_DISPONIBLES[1L]),
                  uiOutput("ui_buf_inegi")
                )
              ),
              sliderInput("buf_cover_pct","Cobertura objetivo (%)",min=50,max=100,value=80,step=5),
              sliderInput("buf_radius_m","Radio por punto (metros)",min=200,max=3000,value=800,step=50),
              sliderInput("buf_max_points","M\u00e1ximo de puntos",min=1,max=10,value=3,step=1),
              hr(),
              actionButton("buf_generate","\U0001F3AF Optimizar", class="btn btn-accent w-100"),
              div(style="height:8px;"),
              downloadButton("download_buf_csv","\u2b07 CSV pauta", class="btn btn-outline-light w-100"),
              div(style="height:10px;"),
              uiOutput("buf_status")
          ),

          div(
            div(class="kpiRow",
                div(class="glass kpi", uiOutput("buf_kpi1")),
                div(class="glass kpi", uiOutput("buf_kpi2")),
                div(class="glass kpi", uiOutput("buf_kpi3")),
                div(class="glass kpi", uiOutput("buf_kpi4"))
            ),
            div(style="height:12px;"),
            div(class="glass-card p-3",
                div(class="card-hd",
                    div(class="card-hd-icon","\U0001F5FA\uFE0F"),
                    div(class="card-hd-text",
                        div(class="t","Mapa PAUTA"),
                        div(class="s","Secciones cubiertas por los buffers")
                    )
                ),
                leafletOutput("map_buf", height=430)
            ),
            div(style="height:12px;"),
            bslib::layout_columns(
              col_widths = c(6,6),
              div(class="glass-card p-3",
                  div(class="card-hd",
                      div(class="card-hd-icon","\U0001F7E2"),
                      div(class="card-hd-text",
                          div(class="t","Dentro vs fuera"),
                          div(class="s","Cobertura del radio")
                      )
                  ),
                  withSpinner(plotlyOutput("buf_compare_bar", height="260px"), type=5, size=1.0)
              ),
              div(class="glass-card p-3",
                  div(class="card-hd",
                      div(class="card-hd-icon","\U0001F3C5"),
                      div(class="card-hd-text",
                          div(class="t","Top partidos dentro"),
                          div(class="s","Votos en secciones cubiertas")
                      )
                  ),
                  withSpinner(plotlyOutput("buf_party_bar", height="260px"), type=5, size=1.0)
              )
            ),
            div(style="height:12px;"),
            div(class="glass-card p-3",
                div(class="card-hd",
                    div(class="card-hd-icon","\U0001F4CD"),
                    div(class="card-hd-text",
                        div(class="t","Puntos de activaci\u00f3n"),
                        div(class="s","Semillas elegidas, cobertura acumulada y direcci\u00f3n")
                    )
                ),
                DTOutput("buf_tbl")
            )
          )
        )
    )
  )
}

pauta_server <- function(input, output, session, has_applied, applied, df_applied, dl_applied, buf_applied) {

  has_buf <- reactive({ x <- buf_applied(); !is.null(x)&&!is.null(x$ts) })

  output$buf_hereda_info <- renderUI({
    if (!has_applied()) return(div(class="status-badge idle", HTML("<b>Sin ejecutar</b> \u00b7 primero usa GENERAR")))
    div(class="status-badge ok",
        HTML(paste0("<b>",key_label(applied()$election),"</b>",
                    " \u00b7 <b>",nrow(df_applied()),"</b> secciones")))
  })

  output$ui_buf_party <- renderUI({
    validate(need(has_applied(),"Presiona GENERAR"))
    df  <- df_applied(); key <- applied()$election
    gv  <- group_votes_matrix(df,key,"DISTRIBUIDO"); G <- gv$G
    validate(need(!is.null(G)&&ncol(G)>0L,"Sin votos distribuidos"))
    tot     <- colSums(G,na.rm=TRUE)
    parties <- sort(unique(setdiff(names(tot)[is.finite(tot)&tot>0],"OTROS")))
    validate(need(length(parties)>0L,"Sin partidos disponibles"))
    def <- if ("PRI" %in% parties) "PRI" else parties[1L]
    selectizeInput("buf_party","Partido objetivo",choices=as.list(setNames(parties,parties)),selected=def)
  })

  output$ui_buf_inegi <- renderUI({
    validate(need(has_applied(),"Presiona GENERAR"))
    validate(need(length(INEGI_VARS)>0L,"No detect\u00e9 columnas *_INEGI"))
    if (NROW(TRADUCTOR)>0L && !is.null(input$buf_eje) && nzchar(input$buf_eje)) {
      d0 <- TRADUCTOR[TRADUCTOR$Eje==input$buf_eje,,drop=FALSE]
      if (NROW(d0)>0L) {
        choices <- as.list(setNames(d0$VARIABLE,paste0(d0$Indicador," (",d0$VARIABLE,")")))
        return(selectInput("buf_inegi_var","Indicador",choices=choices,selected=d0$VARIABLE[1L]))
      }
    }
    selectInput("buf_inegi_var","Indicador",choices=as.list(setNames(INEGI_VARS,INEGI_VARS)),selected=INEGI_VARS[1L])
  })

  # ---- UI Generacional ----
  output$ui_buf_gen <- renderUI({
    avail <- Filter(function(g) g$col %in% names(sf_all), GEN_PAUTA_ABS)
    validate(need(length(avail)>0L,"No se detectaron columnas generacionales (LISTA_GEN_Z, LISTA_MILLENNIALS, etc.)"))

    cols_abs <- setNames(vapply(avail,`[[`,character(1),"col"), names(avail))
    df_est <- as.data.frame(st_drop_geometry(sf_all))
    total_ln <- if ("LISTA_NOMINAL" %in% names(df_est)) sum(as_num(df_est[["LISTA_NOMINAL"]]),na.rm=TRUE) else NA_real_

    tagList(
      div(class="smallHelp","Selecciona una o varias generaciones (se suman):"),
      div(style="height:4px;"),
      checkboxGroupInput(
        "buf_gen_vars", NULL,
        choiceNames  = lapply(seq_along(avail), function(i) {
          nm <- names(avail)[i]; g <- avail[[nm]]
          n  <- if (g$col %in% names(df_est)) sum(as_num(df_est[[g$col]]),na.rm=TRUE) else NA_real_
          pct_txt <- if (is.finite(n)&&is.finite(total_ln)&&total_ln>0) paste0(" (",formatC(100*n/total_ln,format="f",digits=1),"%)") else ""
          div(style=paste0("color:#FFFFFF;"),
              span(g$icon, style=paste0("margin-right:5px;")),
              span(nm, style=paste0("font-weight:700;color:",g$color,";")),
              span(paste0(" ",fmt_int(n),pct_txt), style="font-size:11px;color:rgba(255,255,255,.80);")
          )
        }),
        choiceValues = as.list(unname(cols_abs)),
        selected     = intersect(c("LISTA_GEN_Z","LISTA_MILLENNIALS"), unname(cols_abs))
      ),
      div(style="height:8px;"),
      div(class="smallHelp", style="font-style:italic;",
          HTML("\u26a1 La optimizaci\u00f3n maximiza la <b>suma de electores</b> de las generaciones seleccionadas dentro del radio.")),
      div(style="height:6px;"),
      uiOutput("buf_gen_preview")
    )
  })

  output$buf_gen_preview <- renderUI({
    req(has_applied())
    sel_cols <- input$buf_gen_vars %||% character(0)
    if (length(sel_cols)==0L) return(div(class="pill", HTML("\u26a0\ufe0f Selecciona al menos una generaci\u00f3n")))
    df_e <- as.data.frame(st_drop_geometry(df_applied()))
    valid_cols <- sel_cols[sel_cols %in% names(df_e)]
    if (length(valid_cols)==0L) return(div(class="pill", HTML("\u26a0\ufe0f Columnas no encontradas en datos")))

    total_gen <- sum(vapply(valid_cols, function(col) {
      v <- as_num(df_e[[col]]); sum(v[is.finite(v)], na.rm=TRUE)
    }, numeric(1)))

    total_ln  <- if ("LISTA_NOMINAL" %in% names(df_e)) sum(as_num(df_e[["LISTA_NOMINAL"]]),na.rm=TRUE) else NA_real_
    pct_txt   <- if (is.finite(total_ln)&&total_ln>0) paste0(" (",formatC(100*total_gen/total_ln,format="f",digits=1),"% LN)") else ""

    gen_names <- names(Filter(function(g) g$col %in% valid_cols, GEN_PAUTA_ABS))
    lbl <- if (length(gen_names)==1L) gen_names else paste0(gen_names,collapse=" + ")

    div(class="pill",
        HTML(paste0("\U0001F465 <b>",lbl,"</b>: ",fmt_int(total_gen),pct_txt)))
  })

  buf_metric_info <- reactive({
    validate(need(has_applied(),"Presiona GENERAR"))
    df   <- df_applied()
    mode <- input$buf_mode %||% "electoral"

    if (identical(mode,"electoral")) {
      key   <- applied()$election
      gv    <- group_votes_matrix(df,key,"DISTRIBUIDO"); G <- gv$G
      validate(need(!is.null(G)&&ncol(G)>0L,"Sin matriz electoral"))
      party <- input$buf_party %||% colnames(G)[1L]
      if (!(party %in% colnames(G))) party <- colnames(G)[1L]
      vals  <- as.numeric(G[,party]); vals[!is.finite(vals)] <- 0
      return(list(mode="electoral",label=party,short_lbl=party,values=vals,color=fill_color_winner(party)))
    }

    if (identical(mode,"gen")) {
      sel_cols <- input$buf_gen_vars %||% character(0)
      validate(need(length(sel_cols)>0L,"Selecciona al menos una generaci\u00f3n"))
      df_plain <- as.data.frame(st_drop_geometry(df))
      valid_cols <- sel_cols[sel_cols %in% names(df_plain)]
      validate(need(length(valid_cols)>0L,"Columnas generacionales no encontradas en datos"))

      M <- matrix(0, nrow=nrow(df_plain), ncol=length(valid_cols))
      for (j in seq_along(valid_cols)) {
        v <- as_num(df_plain[[valid_cols[j]]]); v[!is.finite(v)] <- 0; M[,j] <- v
      }
      vals <- rowSums(M, na.rm=TRUE)
      vals[!is.finite(vals)] <- 0

      gen_names <- names(Filter(function(g) g$col %in% valid_cols, GEN_PAUTA_ABS))
      lbl <- if (length(gen_names)==0L) paste(valid_cols,collapse="+") else paste(gen_names,collapse=" + ")
      col_accent <- if (length(gen_names)==1L) {
        GEN_PAUTA_ABS[[gen_names[1L]]]$color %||% ACCENT
      } else "#00B5E2"

      return(list(mode="gen", label=lbl, short_lbl=lbl, values=vals, color=col_accent))
    }

    validate(need(length(INEGI_VARS)>0L,"No hay columnas INEGI"))
    var_nm <- input$buf_inegi_var %||% INEGI_VARS[1L]
    validate(need(var_nm %in% INEGI_VARS,"Indicador INEGI inv\u00e1lido"))
    col_nm <- INEGI_COL_MAP[[var_nm]]
    validate(need(!is.null(col_nm)&&col_nm %in% names(df),"No encontr\u00e9 la columna INEGI"))
    vals    <- as_num(as.data.frame(st_drop_geometry(df))[[col_nm]]); vals[!is.finite(vals)] <- 0
    nice_lbl <- var_nm
    if (NROW(TRADUCTOR)>0L) { idx <- match(var_nm,TRADUCTOR$VARIABLE); if (!is.na(idx)) nice_lbl <- TRADUCTOR$Indicador[idx] }
    list(mode="inegi", label=nice_lbl, short_lbl=var_nm, values=vals, color=ACCENT)
  })

  output$buf_status <- renderUI({
    if (!has_buf()) {
      div(class="status-badge idle", HTML("<b>Sin optimizar</b> \u00b7 configura y presiona Optimizar"))
    } else {
      x <- buf_applied()
      mode_lbl <- switch(x$mode %||% "electoral",
        gen="\U0001F4F1 Generacional", inegi="\U0001F4CA Poblaci\u00f3n", "\U0001F3C6 Electoral")
      div(class="status-badge ok",
          HTML(paste0("<b>Optimizado</b> \u00b7 ", format(x$ts,"%H:%M:%S"), " \u00b7 ", mode_lbl)))
    }
  })

  buf_calc <- reactive({
    validate(need(has_applied(),"Presiona GENERAR"))
    validate(need(has_buf(),"Presiona Optimizar"))
    df <- df_applied(); validate(need(nrow(df)>0L,"Sin datos en la selecci\u00f3n"))
    info <- buf_metric_info(); cfg <- buf_applied()
    metric_vals <- as_num(info$values); metric_vals[!is.finite(metric_vals)] <- 0; metric_vals <- pmax(metric_vals,0)
    validate(need(sum(metric_vals,na.rm=TRUE)>0,"La variable objetivo suma 0"))
    ctr_4326 <- safe_centroids_wgs84(df)
    ctr_3857 <- tryCatch(st_transform(ctr_4326,3857),error=function(e) ctr_4326)
    xy_m <- st_coordinates(ctr_3857); xy_w <- st_coordinates(ctr_4326)
    n       <- nrow(df); dist_m <- as.matrix(stats::dist(xy_m)); cover_mat <- dist_m <= cfg$radius_m
    chosen  <- integer(0); covered <- rep(FALSE,n)
    target_cover_value <- sum(metric_vals,na.rm=TRUE)*(cfg$cover_pct/100)
    while (length(chosen)<cfg$max_points && sum(metric_vals[covered],na.rm=TRUE)+1e-9<target_cover_value) {
      remaining <- setdiff(seq_len(n),chosen); if (!length(remaining)) break
      gains     <- vapply(remaining, function(i) sum(metric_vals[cover_mat[i,]&!covered],na.rm=TRUE), numeric(1))
      best_pos  <- which.max(gains); best_idx <- remaining[best_pos]
      if (!is.finite(gains[best_pos])||gains[best_pos]<=0) break
      chosen  <- c(chosen,best_idx); covered <- covered|cover_mat[best_idx,]
    }
    if (length(chosen)==0L) { chosen <- which.max(metric_vals)[1L]; covered <- cover_mat[chosen,] }
    api_key   <- cfg$google_api_key %||% ""
    addresses <- rep(NA_character_,length(chosen))
    if (nzchar(api_key)) {
      withProgress(message="Geocodificando puntos...",value=0, {
        for (j in seq_along(chosen)) {
          i <- chosen[j]
          addresses[j] <- reverse_geocode(lat=as.numeric(xy_w[i,"Y"]),lng=as.numeric(xy_w[i,"X"]),api_key=api_key)
          incProgress(1/length(chosen))
        }
      })
    }
    key <- applied()$election; c_tot <- total_col(df,key); c_ln <- ln_col(df,key)
    rows <- lapply(seq_along(chosen), function(j) {
      i        <- chosen[j]; inside_i <- cover_mat[i,]
      tot_i    <- if (!is.na(c_tot)&&c_tot %in% names(df)) sum(as_num(df[[c_tot]][inside_i]),na.rm=TRUE) else NA_real_
      ln_i     <- if (!is.na(c_ln) &&c_ln  %in% names(df)) sum(as_num(df[[c_ln]][inside_i]), na.rm=TRUE) else NA_real_
      part_i   <- if (is.finite(tot_i)&&is.finite(ln_i)&&ln_i>0) tot_i/ln_i else NA_real_
      gv       <- group_votes_matrix(df[inside_i,],key,"DISTRIBUIDO"); G <- gv$G
      top_p    <- NA_character_
      if (!is.null(G)&&ncol(G)>0L) { tot_p <- colSums(G,na.rm=TRUE); tot_p <- tot_p[is.finite(tot_p)&tot_p>0]; if (length(tot_p)>0L) top_p <- names(sort(tot_p,decreasing=TRUE))[1L] }
      data.frame(PUNTO=j,SECCION_CENTRO=df$SECCION[i],SECCIONES_IMPACTADAS=sum(inside_i,na.rm=TRUE),
                 LNG=suppressWarnings(as.numeric(xy_w[i,"X"])),LAT=suppressWarnings(as.numeric(xy_w[i,"Y"])),
                 OBJETIVO=sum(metric_vals[inside_i],na.rm=TRUE),TOTAL_VOTOS=tot_i,LISTA_NOMINAL=ln_i,
                 PARTICIPACION=part_i,TOP_PARTIDO=top_p %||% NA_character_,DIRECCION=addresses[j] %||% NA_character_,
                 GOOGLE_MAPS=paste0("https://www.google.com/maps?q=",as.numeric(xy_w[i,"Y"]),",",as.numeric(xy_w[i,"X"])),
                 stringsAsFactors=FALSE)
    })
    tbl     <- rbindlist(rows,fill=TRUE)
    tot_all <- if (!is.na(c_tot)&&c_tot %in% names(df)) sum(as_num(df[[c_tot]][covered]),na.rm=TRUE) else NA_real_
    ln_all  <- if (!is.na(c_ln) &&c_ln  %in% names(df)) sum(as_num(df[[c_ln]][covered]), na.rm=TRUE) else NA_real_
    part_all <- if (is.finite(tot_all)&&is.finite(ln_all)&&ln_all>0) tot_all/ln_all else NA_real_
    total_row <- data.frame(PUNTO=NA_integer_,SECCION_CENTRO=NA_integer_,SECCIONES_IMPACTADAS=sum(covered,na.rm=TRUE),
                            LNG=NA_real_,LAT=NA_real_,OBJETIVO=sum(metric_vals[covered],na.rm=TRUE),
                            TOTAL_VOTOS=tot_all,LISTA_NOMINAL=ln_all,PARTICIPACION=part_all,
                            TOP_PARTIDO=NA_character_,DIRECCION="",GOOGLE_MAPS="",stringsAsFactors=FALSE)
    tbl <- rbind(tbl,total_row,fill=TRUE)
    list(df=df,covered=covered,chosen=chosen,xy_w=xy_w,metric_vals=metric_vals,
         metric_info=info,target_val=target_cover_value,
         covered_val=sum(metric_vals[covered],na.rm=TRUE),
         table=as.data.frame(tbl),addresses=addresses)
  })

  # ---- KPIs ----
  output$buf_kpi1 <- renderUI({
    if (!has_buf()) return(kpi_placeholder("Puntos"))
    z <- buf_calc(); cfg <- buf_applied()
    bar_w <- paste0(min(100*length(z$chosen)/max(cfg$max_points,1),100),"%")
    tagList(
      div(class="t","Puntos activos"),
      div(class="v", HTML(paste0(fmt_int(length(z$chosen)),
                                 " <span style='font-size:14px;font-weight:500;color:rgba(255,255,255,.65);'>/ ",cfg$max_points,"</span>"))),
      div(class="s", HTML(paste0("Radio: <b>",fmt_int(cfg$radius_m),"</b> m"))),
      div(class="kpi-bar-track", div(class="kpi-bar-fill",style=paste0("width:",bar_w,";")))
    )
  })
  output$buf_kpi2 <- renderUI({
    if (!has_buf()) return(kpi_placeholder("Secciones cubiertas"))
    z <- buf_calc()
    n_cov <- sum(z$covered,na.rm=TRUE); n_tot <- nrow(z$df)
    bar_w <- if (n_tot>0) paste0(min(100*n_cov/n_tot,100),"%") else "0%"
    tagList(
      div(class="t","Secciones cubiertas"),
      div(class="v", HTML(paste0(fmt_int(n_cov),
                                 " <span style='font-size:14px;font-weight:500;color:rgba(255,255,255,.65);'>/ ",fmt_int(n_tot),"</span>"))),
      div(class="s", HTML(paste0(formatC(100*n_cov/max(n_tot,1),format="f",digits=1),"% del total"))),
      div(class="kpi-bar-track", div(class="kpi-bar-fill",style=paste0("width:",bar_w,";")))
    )
  })
  output$buf_kpi3 <- renderUI({
    if (!has_buf()) return(kpi_placeholder("Cobertura objetivo"))
    z   <- buf_calc()
    pct <- if (z$target_val>0) z$covered_val/z$target_val else NA_real_
    bar_w <- if (is.finite(pct)) paste0(min(100*pct,100),"%") else "0%"
    tagList(
      div(class="t","Cobertura objetivo"),
      div(class="v", ifelse(is.na(pct),"\u2014",paste0(formatC(100*pct,format="f",digits=1),"%"))),
      div(class="s", HTML(paste0(fmt_int(z$covered_val)," / <b>",fmt_int(z$target_val),"</b>"))),
      div(class="kpi-bar-track", div(class="kpi-bar-fill",style=paste0("width:",bar_w,";")))
    )
  })
  output$buf_kpi4 <- renderUI({
    if (!has_buf()) return(kpi_placeholder("Cobertura total"))
    z         <- buf_calc()
    total_all <- sum(z$metric_vals,na.rm=TRUE)
    pct       <- if (total_all>0) z$covered_val/total_all else NA_real_
    accent_col <- z$metric_info$color %||% ACCENT
    icon_mode <- switch(z$metric_info$mode %||% "electoral",
                        gen="\U0001F465", inegi="\U0001F4CA", "\U0001F3AF")
    bar_w <- if (is.finite(pct)) paste0(min(100*pct,100),"%") else "0%"
    tagList(
      div(class="t","Cobertura total"),
      div(class="v", style=paste0("color:",accent_col,";"),
          ifelse(is.na(pct),"\u2014",paste0(formatC(100*pct,format="f",digits=1),"%"))),
      div(class="s", HTML(paste0(icon_mode," <b>",z$metric_info$label,"</b>"))),
      div(class="kpi-bar-track",
          div(class="kpi-bar-fill", style=paste0("width:",bar_w,";background:",accent_col,";")))
    )
  })

  # ---- MAPA PAUTA ----
  output$map_buf <- renderLeaflet({ create_base_leaflet() })

  observe({
    req(has_buf())
    z       <- buf_calc(); df <- z$df; covered <- z$covered; chosen <- z$chosen; xy_w <- z$xy_w
    rad     <- buf_applied()$radius_m; addresses <- z$addresses
    proxy   <- leafletProxy("map_buf",session=session) |> clearShapes() |> clearMarkers() |> clearControls()
    proxy   <- restore_map_controls(proxy)
    base_lab <- paste0("<b style='color:",ACCENT,"'>Secci\u00f3n:</b> <b>",df$SECCION,"</b>",
                       "<br>Cubierta: <b>",ifelse(covered,"S\u00ed","No"),"</b>")
    if (any(!covered)) proxy <- proxy |> addPolygons(data=df[!covered,],color="rgba(255,255,255,.12)",weight=1,
                                                     fillColor="#111827",fillOpacity=0.18,label=lapply(base_lab[!covered],HTML))
    if (any(covered))  proxy <- proxy |> addPolygons(data=df[covered,], color="rgba(255,255,255,.18)",weight=1.2,
                                                     fillColor=ACCENT,fillOpacity=0.42,label=lapply(base_lab[covered],HTML),
                                                     highlightOptions=highlightOptions(color="#ffffff",weight=2,bringToFront=TRUE))
    if (length(chosen)>0L) {
      point_popups <- vapply(seq_along(chosen), function(j) {
        addr_html <- if (!is.na(addresses[j])&&nzchar(addresses[j])) paste0("<br><b>Direcci\u00f3n:</b> ",htmltools::htmlEscape(addresses[j])) else ""
        gmaps_link <- paste0("https://www.google.com/maps?q=",xy_w[chosen[j],"Y"],",",xy_w[chosen[j],"X"])
        paste0("<div style='color:#FFFFFF;'>",
               "<b style='color:",ACCENT,"'>Punto ",j,"</b>",
               "<br>Secci\u00f3n centro: <b>",df$SECCION[chosen[j]],"</b>",
               addr_html,
               "<br><a href='",gmaps_link,"' target='_blank' style='color:",ACCENT,";'>\U0001F4CD Abrir en Google Maps</a>",
               "</div>")
      },character(1))
      proxy <- proxy |>
        addCircles(lng=xy_w[chosen,"X"],lat=xy_w[chosen,"Y"],radius=rad,
                   color=ACCENT,weight=2.5,fill=FALSE,opacity=0.9) |>
        addCircleMarkers(lng=xy_w[chosen,"X"],lat=xy_w[chosen,"Y"],radius=8,
                         color="#FFFFFF",weight=2,fillColor=ACCENT,fillOpacity=1,
                         popup=point_popups,
                         label=lapply(seq_along(chosen),function(j) {
                           addr_lbl <- if (!is.na(addresses[j])&&nzchar(addresses[j])) paste0("<br><span style='font-size:11px;color:#FFFFFF'>",htmltools::htmlEscape(addresses[j]),"</span>") else ""
                           HTML(paste0("<b>Punto ",j,"</b> \u00b7 Secc ",df$SECCION[chosen[j]],addr_lbl))
                         }))
    }
    proxy <- add_dl_layer(proxy,dl_applied())
    proxy <- add_layers_control(proxy)
    proxy <- hideGroup(proxy,setdiff(names(BASEMAPS),BASEMAP_DEFAULT))
    bb    <- st_bbox(df)
    proxy |> fitBounds(bb["xmin"],bb["ymin"],bb["xmax"],bb["ymax"])
  })

  # ---- CHARTS ----
  output$buf_compare_bar <- renderPlotly({
    validate(need(has_buf(),"Presiona Optimizar"))
    z  <- buf_calc()
    accent_col <- z$metric_info$color %||% ACCENT
    val_inside <- sum(z$metric_vals[z$covered],na.rm=TRUE)
    val_outside<- sum(z$metric_vals[!z$covered],na.rm=TRUE)
    val_total  <- val_inside + val_outside
    pct_inside <- if (val_total>0) 100*val_inside/val_total else NA_real_

    plot_ly(
      labels = c(paste0("Dentro \u2713 \u00b7 ",z$metric_info$label), paste0("Fuera \u2715")),
      values = c(val_inside, val_outside), type="pie", hole=0.60,
      marker = list(colors=c(accent_col,"#1F2937"), line=list(color="rgba(255,255,255,.10)",width=1.5)),
      textinfo="none",
      hovertemplate=paste0("<b>%{label}</b><br>",fmt_int(val_inside)," / ",fmt_int(val_total),"<extra></extra>"),
      sort=FALSE
    ) |> layout(
      annotations=list(list(text=if (is.finite(pct_inside)) paste0(formatC(pct_inside,format="f",digits=1),"%") else "\u2014",
                            x=0.5, y=0.5, xanchor="center", yanchor="middle", showarrow=FALSE,
                            font=list(size=28, color="#FFFFFF", family="Inter, system-ui", weight=900))),
      showlegend=TRUE, legend=list(orientation="h", x=0, y=-0.12, font=list(color="#FFFFFF",size=11)),
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      font=list(color="#FFFFFF", family="Inter, system-ui, sans-serif"), margin=list(l=10,r=10,b=30,t=10)
    ) |> config(displayModeBar=FALSE, responsive=TRUE)
  })

  output$buf_party_bar <- renderPlotly({
    validate(need(has_buf(),"Presiona Optimizar"))
    validate(need(has_applied(),"Presiona GENERAR"))
    z <- buf_calc(); df <- z$df; key <- applied()$election
    gv <- group_votes_matrix(df,key,"DISTRIBUIDO"); G <- gv$G
    validate(need(!is.null(G)&&ncol(G)>0L,"Sin votos distribuidos"))
    tot <- colSums(G[z$covered,,drop=FALSE],na.rm=TRUE)
    tot <- tot[is.finite(tot)&tot>0]
    if ("OTROS" %in% names(tot)&&length(tot)>1L) tot <- tot[names(tot)!="OTROS"]
    tot <- sort(tot,decreasing=TRUE)
    validate(need(length(tot)>0L,"Sin partidos dentro"))
    dd  <- data.frame(party=names(tot)[seq_len(min(8L,length(tot)))],votos=as.numeric(tot[seq_len(min(8L,length(tot)))]),stringsAsFactors=FALSE)
    dd$party <- factor(dd$party,levels=rev(dd$party))
    cols_bar <- vapply(as.character(dd$party),function(p) if (p %in% names(party_colors)) unname(party_colors[[p]]) else ACCENT, character(1))
    p <- plot_ly(dd,x=~votos,y=~party,type="bar",orientation="h",
                 marker=list(color=cols_bar, line=list(color="rgba(255,255,255,.15)",width=0.7), opacity=0.92),
                 text=~paste0("<b>",as.character(party),"</b><br>Votos: <b>",fmt_int(votos),"</b>"),
                 hoverinfo="text") |>
      layout(xaxis=list(title="Votos",color="#FFFFFF",gridcolor="rgba(255,255,255,.07)", zeroline=FALSE),
             yaxis=list(title="",showticklabels=FALSE,ticklen=0,color="#FFFFFF"),
             hoverlabel=list(bgcolor="rgba(5,8,14,.95)", bordercolor="rgba(255,255,255,.18)",
                             font=list(color="#FFFFFF",size=12,family="Inter, system-ui, sans-serif")),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
             font=list(color="#FFFFFF",family="Inter, system-ui, sans-serif"),
             showlegend=FALSE,margin=list(l=55,r=20,b=40,t=10)) |>
      config(displayModeBar=FALSE,responsive=TRUE)
    add_logos_to_plotly_h(p, levels(dd$party), size=0.060)
  })

  # ---- TABLE ----
  output$buf_tbl <- renderDT({
    validate(need(has_buf(),"Presiona Optimizar"))
    d <- as.data.frame(buf_calc()$table); validate(need(NROW(d)>0,"Sin puntos"))
    is_total_row <- if ("PUNTO" %in% names(d)) is.na(d$PUNTO) else rep(FALSE,NROW(d))

    if ("TOP_PARTIDO" %in% names(d)) {
      d$TOP_PARTIDO <- vapply(d$TOP_PARTIDO, function(p) {
        if (is.na(p)||!nzchar(p)) return("\u2014")
        paste0(party_logo_inline(p,"17px"),"<b>",p,"</b>")
      }, character(1))
    }
    if ("DIRECCION" %in% names(d)) {
      d$DIRECCION <- vapply(seq_len(NROW(d)), function(i) {
        a <- d$DIRECCION[i]
        if (is_total_row[i]) return("<span style='color:rgba(255,255,255,.45)'>TOTAL</span>")
        if (is.na(a)||!nzchar(trimws(a))) return("<span style='color:rgba(255,255,255,.45)'>\u2014 Sin direcci\u00f3n</span>")
        paste0("<span class='addr-cell'>",htmltools::htmlEscape(a),"</span>")
      }, character(1))
    }
    if ("GOOGLE_MAPS" %in% names(d)) {
      d$GOOGLE_MAPS <- vapply(seq_len(NROW(d)), function(i) {
        url <- d$GOOGLE_MAPS[i]
        if (is_total_row[i]) return("<span style='color:rgba(255,255,255,.35)'>\u2014</span>")
        if (is.na(url)||!nzchar(trimws(url))) return("\u2014")
        paste0("<a href='",url,"' target='_blank' ",
          "style='display:inline-flex;align-items:center;gap:5px;",
          "background:rgba(213,0,0,.18);border:1px solid rgba(213,0,0,.35);",
          "color:#FFFFFF;font-size:11px;font-weight:700;padding:3px 9px;",
          "border-radius:999px;text-decoration:none;white-space:nowrap;'>",
          "\U0001F4CD Maps</a>")
      }, character(1))
    }

    desired_order <- c("PUNTO","SECCION_CENTRO","SECCIONES_IMPACTADAS","TOP_PARTIDO",
                       "OBJETIVO","TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION",
                       "DIRECCION","GOOGLE_MAPS","LNG","LAT")
    present_cols <- intersect(desired_order, names(d))
    extra_cols   <- setdiff(names(d), desired_order)
    d <- d[, c(present_cols, extra_cols), drop=FALSE]

    header_labels <- c(PUNTO="#", SECCION_CENTRO="Secci\u00f3n", SECCIONES_IMPACTADAS="Secc. impactadas",
                       TOP_PARTIDO="Partido lider", OBJETIVO="Objetivo", TOTAL_VOTOS="Total votos",
                       LISTA_NOMINAL="Lista nominal", PARTICIPACION="Part. %", DIRECCION="Direcci\u00f3n",
                       GOOGLE_MAPS="Maps", LNG="LNG", LAT="LAT")
    headers <- vapply(names(d), function(nm) {
      lbl <- header_labels[nm] %||% nm
      if (nm == "OBJETIVO") {
        p_lbl <- buf_metric_info()$short_lbl
        lbl <- paste0(party_logo_inline(p_lbl,"14px"),"<b>",p_lbl,"</b>")
      }
      lbl
    }, character(1))

    header_cb <- htmltools::withTags(table(class="display", thead(tr(lapply(headers, function(h) th(HTML(h)))))))

    lng_idx <- which(names(d) == "LNG") - 1L
    lat_idx <- which(names(d) == "LAT") - 1L
    hidden_cols <- as.list(c(lng_idx, lat_idx))

    dt <- datatable(
      d, rownames=FALSE, filter="top", container=header_cb, escape=FALSE,
      extensions=c("Buttons"),
      options=list(dom="Bfrtip", scrollX=TRUE, pageLength=25,
                   buttons=list(list(extend="colvis", text="\U0001F441 Columnas"),"copy","csv","excel"),
                   columnDefs=list(list(visible=FALSE, targets=hidden_cols)),
                   initComplete=dt_dark_init)
    )

    num_cols <- intersect(c("SECCIONES_IMPACTADAS","OBJETIVO","TOTAL_VOTOS","LISTA_NOMINAL"), names(d))
    if (length(num_cols)>0L) dt <- dt |> formatRound(num_cols, digits=0)
    if ("PARTICIPACION" %in% names(d)) dt <- dt |> formatPercentage("PARTICIPACION", digits=2)
    dt
  })

  output$download_buf_csv <- downloadHandler(
    filename=function() paste0("pauta_atizapan_",format(Sys.time(),"%Y%m%d_%H%M"),".csv"),
    content =function(file) write.csv(as.data.frame(buf_calc()$table),file,row.names=FALSE,fileEncoding="UTF-8")
  )

  outputOptions(output,"map_buf", suspendWhenHidden=FALSE)
}
