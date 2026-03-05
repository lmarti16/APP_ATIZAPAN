# ==========================================================
# electrend · Atizapán · EDOMEX
# Punto de entrada principal
# ==========================================================

suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(sf)
  library(leaflet)
  library(bslib)
  library(plotly)
  library(DT)
  library(stringr)
  library(htmltools)
  library(dplyr)
  library(shinymanager)
})

sf::sf_use_s2(FALSE)

# ---- Cargar módulos ----
# Shiny auto-sources files in R/ when using runApp(),
# but we source explicitly for compatibility with shiny::shinyApp()
for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) {
  source(f, local = TRUE)
}

# ==========================================================
# CARGA DE DATOS (global)
# ==========================================================

dat    <- load_data()
sf_all <- dat$sf
dl_sf  <- dat$dl

SECC_DL_COL <- pick_col(names(sf_all), c("DISTRITO_L","DISTRITO_LOCAL","ID_DISTRITO_LOCAL","DIST_L","DISTRITO"))
if (!is.na(SECC_DL_COL)) suppressWarnings(sf_all[[SECC_DL_COL]] <- as.integer(sf_all[[SECC_DL_COL]]))

if (!is.null(dl_sf) && nrow(dl_sf) > 0L) {
  DL_ID_COL <- pick_col(names(dl_sf), c("DISTRITO_L","DISTRITO_LOCAL","ID_DISTRITO_LOCAL","DIST_L","DISTRITO"))
  if (!is.na(DL_ID_COL)) {
    suppressWarnings(dl_sf[[DL_ID_COL]] <- as.integer(dl_sf[[DL_ID_COL]]))
    dl_sf$DISTRITO_L <- dl_sf[[DL_ID_COL]]
  } else if (!("DISTRITO_L" %in% names(dl_sf))) dl_sf$DISTRITO_L <- NA_integer_
}

# TRADUCTOR / INEGI
TRADUCTOR <- tryCatch({
  if (!file.exists(PATH_TRAD)) stop("TRADUCTOR no encontrado")
  trad <- read_csv_flex(PATH_TRAD)
  if (!all(c("Eje","Indicador","VARIABLE") %in% names(trad))) stop("TRADUCTOR incompleto")
  as.data.frame(trad)
}, error = function(e) data.frame(Eje=character(0),Indicador=character(0),VARIABLE=character(0), stringsAsFactors=FALSE))

INEGI_COLS  <- grep("_INEGI$", names(sf_all), value = TRUE)
INEGI_VARS  <- sub("_INEGI$", "", INEGI_COLS)
INEGI_COL_MAP <- setNames(INEGI_COLS, INEGI_VARS)

if (NROW(TRADUCTOR) > 0L) {
  TRADUCTOR <- TRADUCTOR[TRADUCTOR$VARIABLE %in% INEGI_VARS, , drop = FALSE]
  if (NROW(TRADUCTOR) > 0L) TRADUCTOR$COL_NAME <- INEGI_COL_MAP[TRADUCTOR$VARIABLE]
}
EJES_DISPONIBLES <- if (NROW(TRADUCTOR) > 0L) unique(TRADUCTOR$Eje) else character(0)

INIT_BBOX <- sf::st_bbox(if (!is.null(dl_sf) && nrow(dl_sf) > 0) dl_sf else sf_all)
INIT_LNG  <- as.numeric((INIT_BBOX["xmin"] + INIT_BBOX["xmax"]) / 2)
INIT_LAT  <- as.numeric((INIT_BBOX["ymin"] + INIT_BBOX["ymax"]) / 2)
INIT_ZOOM <- 12L

# Elecciones
elex <- detect_elections(names(sf_all))
if (all(c("TOTAL_VOTOS","LISTA_NOMINAL") %in% names(sf_all)) &&
    !any(elex$office == "AYU" & elex$yr2 == "24")) {
  elex <- unique(rbind(elex, data.table(office="AYU", yr2="24")))
}
if (nrow(elex) == 0L) elex <- data.table(office="AYU", yr2="24")

elex[, year       := as.integer(paste0("20", yr2))]
elex[, office_lbl := fifelse(office == "AYU","Ayuntamientos","Dip. Local")]
elex[, key        := paste0(office,"_",yr2)]
elex[, label      := paste0(year," \u00b7 ",office_lbl)]
elex[, office_ord := fifelse(office == "AYU",1L,2L)]
setorder(elex, year, office_ord)

ELECTION_CHOICES <- as.list(setNames(elex$key, elex$label))
DEFAULT_ELECTION <- {
  if ("AYU_24" %in% elex$key) "AYU_24"
  else if (any(elex$office == "AYU")) elex[office == "AYU"][.N, key]
  else tail(elex$key,1L)
} %||% "AYU_24"

key_label <- function(key) elex$label[match(key, elex$key)] %||% key

REF_KEY <- {
  ayu <- elex$key[elex$office == "AYU"]
  if (length(ayu)) tail(ayu,1L) else tail(elex$key,1L)
} %||% DEFAULT_ELECTION

# Electorado disponible
ELECTORADO_AVAILABLE <- Filter(function(x) x$col %in% names(sf_all), ELECTORADO_CATALOG)
ELECTORADO_CHOICES   <- as.list(setNames(
  vapply(ELECTORADO_AVAILABLE, `[[`, character(1), "col"),
  names(ELECTORADO_AVAILABLE)
))

# ==========================================================
# UI
# ==========================================================

ui_base <- bslib::page_fillable(
  title = APP_TITLE,
  theme = app_theme,
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(app_js)
  ),

  bslib::layout_sidebar(
    fillable = TRUE,

    sidebar = bslib::sidebar(
      width = 360,
      div(class = "sbWrap glass",

          div(class = "sbHeader",
              div(style="display:flex;align-items:center;gap:6px;margin-bottom:4px;",
                  span(class="electrend-word","electrend"),
                  span(class="electrend-badge","MX")
              ),
              div(class = "smallHelp", style="opacity:.80;",
                  "\u26a1\ufe0f Atizap\u00e1n (MUN 13) \u00b7 EDOMEX"),
              div(style = "height:8px;"),
              uiOutput("ui_status_run")
          ),

          div(class = "sbScroll",

              div(class = "section-label", "Datos"),
              selectInput("election", "Elecci\u00f3n", choices=ELECTION_CHOICES, selected=DEFAULT_ELECTION),

              div(class = "section-label", "Filtros"),
              div(class = "smallHelp", "Distrito local (vac\u00edo = todos)"),
              selectizeInput("dl_sel", NULL, choices=NULL, selected=NULL, multiple=TRUE,
                             options=list(plugins=list("remove_button"), placeholder="Ej. DL 16\u2026")),

              div(class = "smallHelp", style="margin-top:10px;", "Secci\u00f3n (vac\u00edo = todas)"),
              selectizeInput("secciones", NULL, choices=NULL, selected=NULL, multiple=TRUE,
                             options=list(plugins=list("remove_button"), placeholder="Escribe una secci\u00f3n\u2026")),

              div(class = "section-label", "Visualizaci\u00f3n"),
              bslib::input_switch("map_variable", "Choropleth partido", value=FALSE),

              conditionalPanel(
                condition = "!input.map_variable",
                selectInput("map_view", "Vista",
                            choices=list("Ganador"="winner","Participaci\u00f3n (%)"="part",
                                         "Total votos"="tot","Lista nominal"="ln","Electorado"="electorado"),
                            selected="winner"),
                conditionalPanel(
                  condition = "input.map_view == 'winner'",
                  radioButtons("winner_vote_type","Tipo de voto",
                               choices=list("Distribuido"="DISTRIBUIDO","Puro"="PURO","Candidaturas"="CAND"),
                               selected="DISTRIBUIDO", inline=TRUE)
                ),
                conditionalPanel(
                  condition = "input.map_view == 'electorado'",
                  selectInput("electorado_var","Variable del electorado",
                              choices=ELECTORADO_CHOICES,
                              selected=ELECTORADO_CHOICES[["Lista nominal"]] %||% ELECTORADO_CHOICES[[1L]]),
                  radioButtons("electorado_scale","Escala",
                               choices=list("Lineal"="linear","Cuantiles"="quantile"),
                               selected="linear", inline=TRUE),
                  sliderInput("electorado_opacity","Opacidad",min=0.20,max=0.90,value=0.70,step=0.05)
                )
              ),

              conditionalPanel(
                condition = "input.map_variable",
                radioButtons("choro_vote_type","Votos (choropleth)",
                             choices=list("Distribuido"="DISTRIBUIDO","Puro"="PURO"),
                             selected="DISTRIBUIDO", inline=TRUE),
                uiOutput("ui_choro_party"),
                radioButtons("choro_metric","M\u00e9trica",
                             choices=list("Votos"="votes","% (sobre v\u00e1lidos)"="pct"),
                             selected="pct", inline=TRUE),
                radioButtons("choro_scale","Escala",
                             choices=list("Lineal"="linear","Cuantiles"="quantile"),
                             selected="linear", inline=TRUE),
                sliderInput("choro_opacity","Opacidad",min=0.20,max=0.90,value=0.65,step=0.05)
              )
          ),

          div(class = "sbFooter",
              actionButton("generar","\u26a1 GENERAR", class="btn btn-accent w-100"),
              div(style="height:10px;"),
              div(style="display:grid;grid-template-columns:1fr 1fr;gap:10px;",
                  downloadButton("download_csv","\u2b07 CSV", class="btn btn-outline-light w-100"),
                  actionButton("reset","\u21ba Limpiar", class="btn btn-outline-light w-100")
              ),
              div(class="smallHelp", style="margin-top:10px;",
                  HTML("Tip: <b>GENERAR</b> fija la selecci\u00f3n para todas las pesta\u00f1as."))
          )
      )
    ),

    bslib::navset_tab(
      home_ui(),
      explorar_ui(),
      correlacion_ui(),
      competitividad_ui(),
      clustering_ui(),
      comparador_ui(),
      flujo_ui(),
      tiempo_ui(),
      pauta_ui(),
      leeme_ui()
    )
  )
)

ui <- secure_app(
  ui_base,
  enable_admin = FALSE,
  theme = app_theme,
  tags_top = tags$div(
    style = "text-align:center; padding:20px 0 8px 0;",
    if (!is.na(logo_login)) {
      tags$img(src=logo_login, height="64px",
               style="display:block;margin:0 auto 10px auto;filter:drop-shadow(0 2px 14px rgba(213,0,0,.35));")
    } else {
      tags$div(
        style=paste0("font-size:30px;font-weight:900;letter-spacing:-0.03em;color:",ACCENT,";margin-bottom:6px;"),
        "electrend"
      )
    },
    tags$div(style="font-size:18px;font-weight:900;color:#FFFFFF;", APP_TITLE),
    tags$div(style="font-size:11px;letter-spacing:.08em;text-transform:uppercase;color:rgba(255,255,255,.75);margin-top:4px;", APP_SUB),
    tags$div(style=paste0("margin:12px auto 0 auto;width:42px;height:3px;border-radius:999px;background:",ACCENT,";"))
  ),
  background = "linear-gradient(135deg, #0B0F17 0%, #180505 45%, #0B0F17 100%)",
  language = "es"
)

# ==========================================================
# SERVER
# ==========================================================

server <- function(input, output, session) {

  auth <- secure_server(check_credentials = check_credentials(credentials))

  buf_applied <- reactiveVal(NULL)
  applied     <- reactiveVal(NULL)

  # ---- Selectores DL ----
  observe({
    if (is.na(SECC_DL_COL)) { updateSelectizeInput(session,"dl_sel",choices=list(),selected=NULL,server=TRUE); return() }
    dls <- sort(unique(na.omit(sf_all[[SECC_DL_COL]])))
    ch  <- as.list(setNames(as.character(dls), paste0("DL ",dls)))
    updateSelectizeInput(session,"dl_sel",choices=ch,selected=NULL,server=TRUE)
  })

  # ---- Selectores secciones ----
  observe({
    df0 <- sf_all
    dls_in <- suppressWarnings(as.integer(input$dl_sel %||% character(0)))
    dls_in <- dls_in[!is.na(dls_in)]
    if (!is.na(SECC_DL_COL) && length(dls_in)>0L) df0 <- df0[df0[[SECC_DL_COL]] %in% dls_in, ]
    secs <- sort(unique(df0$SECCION))
    ch   <- as.list(setNames(as.character(secs),as.character(secs)))
    cur  <- intersect(isolate(input$secciones) %||% character(0), as.character(secs))
    updateSelectizeInput(session,"secciones",choices=ch,selected=cur,server=TRUE)
  })

  # ---- Reset ----
  observeEvent(input$reset, {
    updateSelectizeInput(session,"dl_sel",selected=NULL,server=TRUE)
    updateSelectizeInput(session,"secciones",selected=NULL,server=TRUE)
    updateSelectInput(session,"election",selected=DEFAULT_ELECTION)
    updateSelectInput(session,"map_view",selected="winner")
    updateRadioButtons(session,"winner_vote_type",selected="DISTRIBUIDO")
    bslib::update_switch(session,"map_variable",value=FALSE)
    if (length(ELECTORADO_CHOICES)>0L) updateSelectInput(session,"electorado_var",
                                                         selected=ELECTORADO_CHOICES[["Lista nominal"]] %||% ELECTORADO_CHOICES[[1L]])
    updateRadioButtons(session,"ts_office",selected="BOTH")
    updateRadioButtons(session,"ts_vote_type",selected="DISTRIBUIDO")
    updateRadioButtons(session,"ts_party_metric",selected="pct")
    updateSelectizeInput(session,"ts_parties",selected=NULL,server=TRUE)
    updateSelectInput(session,"ts_map_election",selected=head(elex$key,1L) %||% DEFAULT_ELECTION)
    updateRadioButtons(session,"buf_mode",selected="electoral")
    updateCheckboxGroupInput(session,"buf_gen_vars",selected=c("LISTA_GEN_Z","LISTA_MILLENNIALS"))
    if (length(EJES_DISPONIBLES)>0L) updateSelectInput(session,"buf_eje",selected=EJES_DISPONIBLES[1L])
    updateSliderInput(session,"buf_cover_pct",value=80)
    updateSliderInput(session,"buf_radius_m",value=800)
    updateSliderInput(session,"buf_max_points",value=3)
    buf_applied(NULL)
  }, ignoreInit=TRUE)

  # ---- GENERAR ----
  observeEvent(input$generar, {
    ap <- list(
      election        = input$election %||% DEFAULT_ELECTION,
      winner_vote_type= toupper(input$winner_vote_type %||% "DISTRIBUIDO"),
      map_variable    = isTRUE(input$map_variable),
      map_view        = input$map_view %||% "winner",
      choro_vote_type = input$choro_vote_type %||% "DISTRIBUIDO",
      choro_party     = input$choro_party %||% "",
      choro_metric    = input$choro_metric %||% "pct",
      choro_scale     = input$choro_scale %||% "linear",
      choro_opacity   = input$choro_opacity %||% 0.65,
      electorado_var  = input$electorado_var %||% (ELECTORADO_CHOICES[[1L]] %||% "LISTA_NOMINAL"),
      electorado_scale= input$electorado_scale %||% "linear",
      electorado_opacity=input$electorado_opacity %||% 0.70,
      dl_sel          = input$dl_sel %||% character(0),
      secciones       = input$secciones %||% character(0),
      ts              = Sys.time()
    )
    applied(ap)
    showNotification("Generado \u2705", type="message", duration=1.2)
  }, ignoreInit=TRUE)

  has_applied <- reactive({ ap <- applied(); !is.null(ap) && !is.null(ap$ts) })

  # ---- Status ----
  output$ui_status_run <- renderUI({
    ap <- applied()
    if (!has_applied()) {
      div(class="status-badge idle", HTML("<b>Sin ejecutar</b> \u00b7 configura y presiona GENERAR"))
    } else {
      div(class="status-badge ok",
          HTML(paste0("<b>Aplicado</b> \u00b7 ", format(ap$ts,"%H:%M:%S"),
                      " \u00b7 <b>", key_label(ap$election),"</b>")))
    }
  })

  # ---- Shared reactives ----
  df_applied <- reactive({
    req(has_applied())
    ap  <- applied()
    df  <- sf_all
    dls <- suppressWarnings(as.integer(ap$dl_sel %||% character(0)))
    dls <- dls[!is.na(dls)]
    if (!is.na(SECC_DL_COL) && length(dls)>0L) df <- df[df[[SECC_DL_COL]] %in% dls, ]
    if (length(ap$secciones)>0L) {
      s <- suppressWarnings(as.integer(ap$secciones)); s <- s[!is.na(s)]
      if (length(s)>0L) df <- df[df$SECCION %in% s, ]
    }
    df
  })

  dl_applied <- reactive({
    if (is.null(dl_sf)||nrow(dl_sf)==0) return(dl_sf)
    if (!has_applied()) return(dl_sf)
    ap  <- applied()
    dls <- suppressWarnings(as.integer(ap$dl_sel %||% character(0)))
    dls <- dls[!is.na(dls)]
    if (length(dls)==0L) return(dl_sf)
    if ("DISTRITO_L" %in% names(dl_sf)) dl_sf[dl_sf$DISTRITO_L %in% dls, ] else dl_sf
  })

  df_metrics <- reactive({
    req(has_applied())
    ap  <- applied()
    df  <- df_applied()
    key <- ap$election
    c_tot <- total_col(df,key); c_ln <- ln_col(df,key)
    c_cas <- metric_col(df,key,"CASILLAS"); c_val <- valid_col(df,key)
    tot  <- if (!is.na(c_tot)&&c_tot %in% names(df)) as_num(df[[c_tot]]) else NA_real_
    ln   <- if (!is.na(c_ln) &&c_ln  %in% names(df)) as_num(df[[c_ln]])  else NA_real_
    cas  <- if (!is.na(c_cas)&&c_cas %in% names(df)) as_num(df[[c_cas]]) else NA_real_
    val  <- if (!is.na(c_val)&&c_val %in% names(df)) as_num(df[[c_val]]) else NA_real_
    part <- ifelse(is.finite(ln)&ln>0, tot/ln, NA_real_)
    list(df=df,key=key,c_tot=c_tot,c_ln=c_ln,c_cas=c_cas,c_val=c_val,
         tot=tot,ln=ln,cas=cas,val=val,part=part)
  })

  # ---- Choropleth party selector ----
  output$ui_choro_party <- renderUI({
    if (!isTRUE(input$map_variable)) return(NULL)
    req(has_applied())
    df  <- df_applied(); req(nrow(df)>0L)
    key <- applied()$election
    vt  <- input$choro_vote_type %||% "DISTRIBUIDO"
    gv  <- group_votes_matrix(df, key, vt)
    parties <- sort(unique(setdiff(gv$parties,"OTROS")))
    validate(need(length(parties)>0L,"Sin partidos disponibles"))
    sel <- (input$choro_party %||% parties[1L])[1L]
    if (!(sel %in% parties)) sel <- parties[1L]
    selectizeInput("choro_party","Partido",
                   choices=as.list(setNames(parties, parties)),
                   selected=sel)
  })

  # ---- PAUTA trigger ----
  observeEvent(input$buf_generate, {
    req(has_applied())
    cfg <- list(
      mode=input$buf_mode %||% "electoral",
      party=input$buf_party %||% NULL,
      gen_vars=input$buf_gen_vars %||% character(0),
      eje=input$buf_eje %||% NULL,
      inegi_var=input$buf_inegi_var %||% NULL,
      cover_pct=as.numeric(input$buf_cover_pct %||% 80),
      radius_m=as.numeric(input$buf_radius_m %||% 800),
      max_points=as.integer(input$buf_max_points %||% 3),
      google_api_key=GOOGLE_API_KEY, ts=Sys.time()
    )
    buf_applied(cfg)
    showNotification("PAUTA optimizada \u2705", type="message", duration=1.2)
  }, ignoreInit=TRUE)

  # ---- Module servers ----
  home_server(input, output, session, has_applied, applied, df_applied, df_metrics)
  tbl_data <- explorar_server(input, output, session, has_applied, applied, df_applied, dl_applied, df_metrics)
  correlacion_server(input, output, session, has_applied, applied, df_applied, df_metrics)
  competitividad_server(input, output, session, has_applied, applied, df_applied, dl_applied, df_metrics)
  clustering_server(input, output, session, has_applied, applied, df_applied, dl_applied, df_metrics)
  comparador_server(input, output, session, has_applied, applied, df_applied, df_metrics)
  flujo_server(input, output, session, has_applied, applied, df_applied, df_metrics)
  tiempo_server(input, output, session, has_applied, applied, df_applied, dl_applied)
  pauta_server(input, output, session, has_applied, applied, df_applied, dl_applied, buf_applied)

  # ---- Download CSV (explorar) ----
  output$download_csv <- downloadHandler(
    filename = function() { ap <- applied(); if (is.null(ap)) return("atizapan_sin_generar.csv")
      paste0("atizapan_secciones_",ap$election,"_tabla_",(input$table_view %||% "DISTRIBUIDO"),".csv") },
    content = function(file) write.csv(tbl_data(),file,row.names=FALSE,fileEncoding="UTF-8")
  )
}

shinyApp(ui, server)
