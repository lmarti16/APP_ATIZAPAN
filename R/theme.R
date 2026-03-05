# ==========================================================
# THEME + CSS
# ==========================================================

app_theme <-  bslib::bs_theme(
  bootswatch = "darkly",
  primary    = ACCENT,
  base_font  = "system-ui",
  font_scale = 1
)

app_css <- paste0("
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;900&display=swap');

/* ====================================================
   VARIABLES & RESET
==================================================== */
:root{
  --accent:   ", ACCENT, ";
  --accent2:  ", ACCENT2, ";
  --bg:       ", BG, ";
  --txt:      #FFFFFF;
  --muted:    rgba(255,255,255,.82);
  --glass-bg: rgba(255,255,255,.055);
  --glass-border: rgba(255,255,255,.11);
  --glow:     rgba(213,0,0,.45);
  --glow-soft:rgba(213,0,0,.18);
  --radius:   18px;
  --radius-sm:12px;
  --trans:    all .22s cubic-bezier(.4,0,.2,1);
}
*, *::before, *::after{ box-sizing:border-box; margin:0; padding:0; }

/* ====================================================
   ANIMATED BACKGROUND
==================================================== */
body{
  font-family:'Inter',system-ui,-apple-system,sans-serif !important;
  color:#FFFFFF !important;
  background:", BG, " !important;
  background-image:
    radial-gradient(ellipse 900px 600px at 8% -5%,  rgba(213,0,0,.30), transparent 55%),
    radial-gradient(ellipse 700px 500px at 95% 10%, rgba(139,0,0,.18), transparent 50%),
    radial-gradient(ellipse 500px 400px at 50% 80%, rgba(213,0,0,.10), transparent 55%),
    radial-gradient(ellipse 300px 300px at 20% 60%, rgba(255,100,0,.06), transparent 50%)
    !important;
  background-attachment: fixed !important;
  min-height:100vh;
  overflow-x:hidden;
}

/* Animated noise overlay */
body::before{
  content:'';
  position:fixed; inset:0; z-index:0; pointer-events:none;
  opacity:.028;
  background-image:url(\"data:image/svg+xml,%3Csvg viewBox='0 0 256 256' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.9' numOctaves='4' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)'/%3E%3C/svg%3E\");
  background-size: 180px 180px;
}

/* Floating ambient orbs */
body::after{
  content:'';
  position:fixed; inset:0; z-index:0; pointer-events:none;
  background:
    radial-gradient(circle 280px at 15% 25%, rgba(213,0,0,.07), transparent),
    radial-gradient(circle 200px at 80% 75%, rgba(139,0,0,.06), transparent);
  animation: orb-drift 18s ease-in-out infinite alternate;
}
@keyframes orb-drift{
  0%  { transform:translate(0,0) scale(1); }
  50% { transform:translate(30px,-20px) scale(1.05); }
  100%{ transform:translate(-20px,30px) scale(.97); }
}

/* Everything sits above the pseudo overlays */
.shiny-fill-page, .bslib-page-fill, .container-fluid,
.bslib-sidebar-layout, .bslib-main, .bslib-sidebar,
.bslib-navs-left, nav, .tab-content { position:relative; z-index:1; }

/* ====================================================
   GLASSMORPHISM CARDS
==================================================== */
.glass, .glass-card{
  background: linear-gradient(145deg,
    rgba(255,255,255,.09) 0%,
    rgba(255,255,255,.04) 60%,
    rgba(255,255,255,.07) 100%);
  border: 1px solid rgba(255,255,255,.12);
  border-top-color: rgba(255,255,255,.22);
  border-left-color: rgba(255,255,255,.16);
  border-radius: var(--radius);
  box-shadow:
    0 2px 0 rgba(255,255,255,.06) inset,
    0 20px 60px rgba(0,0,0,.50),
    0 4px 24px rgba(0,0,0,.30);
  backdrop-filter: blur(18px) saturate(160%);
  transition: var(--trans);
}
.glass:hover, .glass-card:hover{
  border-color: rgba(255,255,255,.18);
  box-shadow:
    0 2px 0 rgba(255,255,255,.08) inset,
    0 24px 72px rgba(0,0,0,.55),
    0 0 0 1px rgba(213,0,0,.12);
}

/* ====================================================
   TYPOGRAPHY
==================================================== */
h1,h2,h3,h4,h5,h6,p,label,span,div,li,td,th,
.dataTables_info,.dataTables_paginate a,
.dataTables_length label,.dataTables_filter label{
  color:#FFFFFF;
}
.text-muted{ color:rgba(255,255,255,.72) !important; }

.blockTitle{
  color:#FFFFFF !important;
  font-weight:900;
  font-family:'Inter',sans-serif;
  letter-spacing:-.02em;
}

/* Gradient accent text for big titles */
.grad-title{
  background: linear-gradient(135deg, #FFFFFF 0%, rgba(255,255,255,.75) 100%);
  -webkit-background-clip:text; -webkit-text-fill-color:transparent;
  background-clip:text;
}

.smallHelp{
  color:rgba(255,255,255,.90) !important;
  font-size:12px;
  line-height:1.6;
}

.section-label{
  font-size:9.5px; font-weight:900; letter-spacing:.22em; text-transform:uppercase;
  color:#FFFFFF !important;
  margin:18px 0 10px 0; padding-bottom:9px;
  display:flex; align-items:center; gap:8px;
  position:relative;
}
.section-label::before{
  content:'';
  display:inline-block;
  width:3px; height:12px;
  background: linear-gradient(180deg, var(--accent), transparent);
  border-radius:999px;
  flex-shrink:0;
}
.section-label::after{
  content:''; flex:1;
  height:1px;
  background: linear-gradient(90deg, rgba(213,0,0,.40), transparent);
}

/* ====================================================
   KPI CARDS
==================================================== */
.kpiRow{
  display:grid;
  grid-template-columns:repeat(auto-fit, minmax(210px,1fr));
  gap:12px;
}
.kpi{
  padding:18px 16px;
  position:relative; overflow:hidden;
  transition: var(--trans);
}
/* Shimmer strip on top */
.kpi::before{
  content:'';
  position:absolute; top:0; left:0; right:0; height:1px;
  background: linear-gradient(90deg,
    transparent 0%, rgba(255,255,255,.55) 40%,
    rgba(213,0,0,.70) 60%, transparent 100%);
  opacity:0; transition:opacity .35s;
}
.kpi:hover::before{ opacity:1; }

/* Glowing corner accent */
.kpi::after{
  content:'';
  position:absolute; top:-40px; right:-40px;
  width:100px; height:100px;
  background: radial-gradient(circle, rgba(213,0,0,.18), transparent 65%);
  opacity:0; transition:opacity .35s;
  border-radius:50%;
}
.kpi:hover::after{ opacity:1; }

.kpi .t{
  font-size:9.5px; color:rgba(255,255,255,.78);
  text-transform:uppercase; letter-spacing:.16em; font-weight:700;
}
.kpi .v{
  font-size:26px; font-weight:900; color:#FFFFFF;
  margin-top:10px; line-height:1.1;
  text-shadow: 0 0 30px rgba(213,0,0,.25), 0 2px 10px rgba(0,0,0,.40);
}
.kpi .s{
  font-size:11.5px; color:rgba(255,255,255,.88);
  margin-top:8px; line-height:1.55;
}

/* ====================================================
   PILLS & BADGES
==================================================== */
.pill{
  display:inline-flex; align-items:center; gap:8px;
  background: rgba(255,255,255,.07);
  border:1px solid rgba(255,255,255,.14);
  padding:6px 13px; border-radius:999px;
  font-size:11.5px; color:#FFFFFF; font-weight:600;
  backdrop-filter:blur(8px);
  box-shadow: 0 2px 12px rgba(0,0,0,.20);
  transition: var(--trans);
}
.pill:hover{
  background:rgba(255,255,255,.10);
  border-color:rgba(255,255,255,.22);
}

/* Status pill glow when active */
.pill b{ font-weight:800; }

/* ====================================================
   BUTTONS
==================================================== */
.btn-accent{
  background: linear-gradient(135deg, var(--accent) 0%, var(--accent2) 100%) !important;
  border-color: transparent !important;
  color:#fff !important; font-weight:900 !important;
  border-radius:var(--radius-sm) !important;
  box-shadow:
    0 4px 20px rgba(213,0,0,.40),
    0 1px 0 rgba(255,255,255,.12) inset !important;
  transition: var(--trans) !important;
  letter-spacing:.02em;
  position:relative; overflow:hidden;
}
/* Shine sweep */
.btn-accent::after{
  content:'';
  position:absolute; top:0; left:-100%; width:60%; height:100%;
  background:linear-gradient(90deg, transparent, rgba(255,255,255,.18), transparent);
  transform:skewX(-20deg);
  transition:left .5s ease;
}
.btn-accent:hover::after{ left:150%; }
.btn-accent:hover{
  background: linear-gradient(135deg, #FF1A1A 0%, var(--accent) 100%) !important;
  box-shadow:
    0 8px 32px rgba(213,0,0,.60),
    0 1px 0 rgba(255,255,255,.18) inset !important;
  transform:translateY(-2px) !important;
}
.btn-accent:active{ transform:translateY(0) !important; }

.btn-outline-light{
  border-radius:var(--radius-sm) !important;
  border:1px solid rgba(255,255,255,.18) !important;
  background:rgba(255,255,255,.04) !important;
  color:#FFFFFF !important;
  backdrop-filter:blur(8px);
  transition: var(--trans) !important;
}
.btn-outline-light:hover{
  background:rgba(255,255,255,.11) !important;
  border-color:rgba(255,255,255,.28) !important;
  transform:translateY(-1px) !important;
  box-shadow:0 4px 16px rgba(0,0,0,.25) !important;
}

/* ====================================================
   FORM CONTROLS
==================================================== */
.form-control, .form-select{
  background:rgba(255,255,255,.06) !important;
  border:1px solid rgba(255,255,255,.12) !important;
  color:#FFFFFF !important;
  border-radius:var(--radius-sm) !important;
  transition: var(--trans) !important;
}
.form-control:focus, .form-select:focus{
  background:rgba(255,255,255,.09) !important;
  border-color:rgba(213,0,0,.70) !important;
  box-shadow:0 0 0 3px rgba(213,0,0,.18) !important;
  outline:none !important;
}
.form-control::placeholder{ color:rgba(255,255,255,.38) !important; }

.selectize-control .selectize-input{
  background:rgba(255,255,255,.06) !important;
  border:1px solid rgba(255,255,255,.12) !important;
  color:#FFFFFF !important;
  border-radius:var(--radius-sm) !important;
  box-shadow:none !important;
  transition: var(--trans);
}
.selectize-control.focus .selectize-input,
.selectize-control .selectize-input.focus{
  border-color:rgba(213,0,0,.65) !important;
  box-shadow:0 0 0 3px rgba(213,0,0,.15) !important;
  background:rgba(255,255,255,.09) !important;
}
.selectize-control .selectize-input input{ color:#FFFFFF !important; }
.selectize-dropdown{
  background:rgba(8,10,16,.97) !important;
  border:1px solid rgba(255,255,255,.12) !important;
  border-radius:var(--radius-sm) !important;
  backdrop-filter:blur(20px);
  box-shadow:0 20px 60px rgba(0,0,0,.65) !important;
}
.selectize-dropdown .option{ color:#FFFFFF !important; padding:8px 12px; transition:.12s; }
.selectize-dropdown .option:hover,
.selectize-dropdown .option.active{
  background:linear-gradient(90deg, rgba(213,0,0,.30), rgba(213,0,0,.10)) !important;
  padding-left:16px;
}
.selectize-dropdown .option.selected{
  background:rgba(213,0,0,.20) !important;
  font-weight:700;
}

/* Radio & checkbox */
.form-check-label{ color:#FFFFFF !important; font-size:13px; }
.form-check-input{ accent-color:var(--accent) !important; }
.form-check-input:focus{ box-shadow:0 0 0 3px rgba(213,0,0,.20) !important; }

/* shiny switch */
.bslib-input-switch .form-check-input:checked{ background-color:var(--accent) !important; border-color:var(--accent) !important; }

/* Range sliders */
input[type=range]{ accent-color:var(--accent) !important; }
.irs--shiny .irs-bar,
.irs--shiny .irs-handle{ background:var(--accent) !important; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single{
  background:var(--accent) !important;
  color:#fff !important;
}

/* ====================================================
   SIDEBAR
==================================================== */
.sbWrap{
  display:flex; flex-direction:column;
  height:calc(100vh - 24px); min-height:620px; overflow:hidden;
}
.sbHeader{
  position:sticky; top:0; z-index:20;
  padding:16px 16px 12px;
  border-bottom:1px solid rgba(255,255,255,.08);
  background:linear-gradient(180deg,
    rgba(7,10,15,.80) 0%,
    rgba(7,10,15,.0) 100%);
  backdrop-filter:blur(20px);
}
/* Electrend wordmark */
.electrend-word{
  font-size:20px; font-weight:900; letter-spacing:-.04em;
  background:linear-gradient(135deg, #FFFFFF 0%, rgba(255,255,255,.65) 100%);
  -webkit-background-clip:text; -webkit-text-fill-color:transparent;
  background-clip:text;
  display:inline-block;
}
.electrend-badge{
  display:inline-block;
  font-size:8px; font-weight:800; letter-spacing:.14em; text-transform:uppercase;
  background:linear-gradient(135deg,var(--accent),var(--accent2));
  color:#fff; padding:2px 7px; border-radius:999px;
  margin-left:6px; vertical-align:middle;
  box-shadow:0 2px 8px rgba(213,0,0,.40);
}
.sbScroll{
  padding:8px 14px 10px;
  overflow-y:auto; overflow-x:hidden;
  flex:1 1 auto;
  scrollbar-width:thin;
  scrollbar-color:rgba(213,0,0,.35) transparent;
}
.sbScroll::-webkit-scrollbar{ width:3px; }
.sbScroll::-webkit-scrollbar-thumb{
  background:linear-gradient(180deg,rgba(213,0,0,.50),rgba(213,0,0,.20));
  border-radius:999px;
}
.sbScroll::-webkit-scrollbar-track{ background:transparent; }

.sbFooter{
  position:sticky; bottom:0; z-index:20;
  padding:12px 14px 16px;
  border-top:1px solid rgba(255,255,255,.07);
  background:linear-gradient(0deg,
    rgba(7,10,15,.82) 0%,
    rgba(7,10,15,.0) 100%);
  backdrop-filter:blur(20px);
}

/* ====================================================
   NAV TABS
==================================================== */
.nav-tabs{
  border-bottom:1px solid rgba(255,255,255,.08) !important;
  gap:2px;
}
.nav-tabs .nav-link{
  color:rgba(255,255,255,.60) !important;
  font-weight:600; font-size:13px;
  border-radius:10px 10px 0 0 !important;
  border:none !important;
  padding:9px 16px;
  transition: var(--trans);
  position:relative;
}
.nav-tabs .nav-link::after{
  content:'';
  position:absolute; bottom:0; left:20%; right:20%; height:2px;
  background:var(--accent);
  border-radius:2px 2px 0 0;
  transform:scaleX(0); transition:transform .22s ease;
}
.nav-tabs .nav-link:hover{ color:rgba(255,255,255,.88) !important; }
.nav-tabs .nav-link:hover::after{ transform:scaleX(.5); }
.nav-tabs .nav-link.active{
  background:rgba(255,255,255,.07) !important;
  color:#FFFFFF !important; font-weight:800 !important;
  box-shadow:none !important;
}
.nav-tabs .nav-link.active::after{ transform:scaleX(1); }

/* ====================================================
   LEAFLET
==================================================== */
.leaflet-container{
  border-radius:var(--radius);
  border:1px solid rgba(255,255,255,.09);
  box-shadow:
    0 20px 60px rgba(0,0,0,.55),
    0 0 0 1px rgba(213,0,0,.08);
}
.leaflet-control-layers{
  background:rgba(5,7,12,.80) !important;
  border:1px solid rgba(255,255,255,.12) !important;
  border-radius:var(--radius-sm) !important;
  backdrop-filter:blur(16px);
  box-shadow:0 8px 32px rgba(0,0,0,.45) !important;
}
.leaflet-control-layers label{ color:#FFFFFF !important; font-size:12px; }
.leaflet-popup-content-wrapper{
  background:rgba(5,8,14,.94) !important;
  border:1px solid rgba(255,255,255,.13) !important;
  border-top-color:rgba(255,255,255,.22) !important;
  border-radius:14px !important;
  color:#FFFFFF !important;
  backdrop-filter:blur(16px);
  box-shadow:0 20px 60px rgba(0,0,0,.65) !important;
}
.leaflet-popup-tip{ background:rgba(5,8,14,.94) !important; }
.leaflet-popup-content{ color:#FFFFFF !important; line-height:1.7; }
.leaflet-legend{
  background:rgba(5,8,14,.84) !important;
  border:1px solid rgba(255,255,255,.11) !important;
  border-radius:var(--radius-sm) !important;
  color:#FFFFFF !important;
  backdrop-filter:blur(12px);
  box-shadow:0 8px 32px rgba(0,0,0,.50) !important;
  padding:10px 14px !important;
}
.leaflet-bar a{
  background:rgba(5,8,14,.85) !important;
  color:#FFFFFF !important;
  border-color:rgba(255,255,255,.12) !important;
  backdrop-filter:blur(8px);
}
.leaflet-bar a:hover{ background:rgba(213,0,0,.25) !important; }

/* ====================================================
   DATATABLES
==================================================== */
.dataTables_wrapper *,
.dataTables_info,
.dataTables_paginate a,
.dataTables_length label,
.dataTables_filter label{ color:#FFFFFF !important; }

.dataTables_wrapper .dataTables_filter input,
.dataTables_wrapper .dataTables_length select{
  background:rgba(255,255,255,.06) !important;
  border:1px solid rgba(255,255,255,.12) !important;
  color:#FFFFFF !important;
  border-radius:10px !important;
  padding:5px 10px;
}
.dataTables_wrapper .dataTables_filter input:focus{
  border-color:rgba(213,0,0,.60) !important;
  box-shadow:0 0 0 3px rgba(213,0,0,.15) !important;
  outline:none !important;
}

table.dataTable{
  border-collapse:separate !important;
  border-spacing:0 3px !important;
}
table.dataTable thead th{
  background:rgba(0,0,0,.40) !important;
  color:#FFFFFF !important; font-weight:800 !important;
  font-size:11px; letter-spacing:.08em; text-transform:uppercase;
  border-bottom:2px solid rgba(213,0,0,.50) !important;
  padding:10px 12px !important;
}
table.dataTable tbody td{
  color:#FFFFFF !important;
  padding:8px 12px !important;
  border:none !important;
  transition:background .15s;
}
table.dataTable tbody tr{
  background:rgba(255,255,255,.025) !important;
  border-radius:8px;
  transition:var(--trans);
}
table.dataTable tbody tr:hover > td{
  background:rgba(213,0,0,.14) !important;
}
table.dataTable tbody tr.odd  > td{ background:rgba(255,255,255,.03) !important; }
table.dataTable tbody tr.even > td{ background:rgba(0,0,0,.12) !important; }
table.dataTable tbody tr:hover{ transform:translateX(2px); }

.dt-button{
  border-radius:9px !important;
  border:1px solid rgba(255,255,255,.16) !important;
  background:rgba(255,255,255,.05) !important;
  color:#FFFFFF !important;
  font-size:12px !important;
  transition:var(--trans) !important;
}
.dt-button:hover{
  background:rgba(213,0,0,.22) !important;
  border-color:rgba(213,0,0,.40) !important;
  transform:translateY(-1px) !important;
}

/* ====================================================
   SHINY NOTIFICATIONS
==================================================== */
.shiny-notification{
  background:rgba(5,8,14,.96) !important;
  border:1px solid rgba(255,255,255,.12) !important;
  border-left:3px solid var(--accent) !important;
  color:#FFFFFF !important;
  border-radius:var(--radius-sm) !important;
  backdrop-filter:blur(16px);
  box-shadow:0 16px 48px rgba(0,0,0,.60) !important;
  animation: notif-in .30s cubic-bezier(.34,1.56,.64,1);
}
@keyframes notif-in{
  from{ opacity:0; transform:translateY(12px) scale(.96); }
  to  { opacity:1; transform:translateY(0)   scale(1); }
}
.shiny-notification-close{ color:rgba(255,255,255,.55) !important; }

/* ====================================================
   MISC DETAILS
==================================================== */
hr{ border-color:rgba(255,255,255,.09) !important; margin:12px 0; }

.logo-label img{ vertical-align:middle; margin-right:4px; }
.addr-cell{ font-size:11px; color:#FFFFFF; max-width:260px; white-space:normal; }

/* Plotly */
.js-plotly-plot .plotly .gtitle,
.js-plotly-plot .plotly text{ fill:#FFFFFF !important; }
.js-plotly-plot .plotly .modebar{ background:rgba(0,0,0,.0) !important; }
.js-plotly-plot .plotly .modebar-btn path{ fill:rgba(255,255,255,.50) !important; }

/* Scrollbar global */
::-webkit-scrollbar{ width:4px; height:4px; }
::-webkit-scrollbar-thumb{
  background:rgba(213,0,0,.30);
  border-radius:999px;
}
::-webkit-scrollbar-thumb:hover{ background:rgba(213,0,0,.55); }
::-webkit-scrollbar-track{ background:transparent; }

/* Loading spinner override */
.shiny-spinner-output-container .load-container .loader{
  border-top-color:var(--accent) !important;
}

/* ====================================================
   CARD ENTRANCE ANIMATION
==================================================== */
.glass, .glass-card{
  animation: card-in .42s cubic-bezier(.22,1,.36,1) both;
}
.kpi{ animation: card-in .42s cubic-bezier(.22,1,.36,1) both; }

@keyframes card-in{
  from{ opacity:0; transform:translateY(14px); }
  to  { opacity:1; transform:translateY(0); }
}

.kpiRow > *:nth-child(1){ animation-delay:.04s; }
.kpiRow > *:nth-child(2){ animation-delay:.10s; }
.kpiRow > *:nth-child(3){ animation-delay:.16s; }
.kpiRow > *:nth-child(4){ animation-delay:.22s; }

/* ====================================================
   SKELETON LOADER
==================================================== */
@keyframes shimmer {
  0%   { background-position: -600px 0; }
  100% { background-position:  600px 0; }
}
.skeleton{
  display:inline-block;
  border-radius:6px;
  background: linear-gradient(90deg,
    rgba(255,255,255,.06) 25%,
    rgba(255,255,255,.14) 50%,
    rgba(255,255,255,.06) 75%);
  background-size:600px 100%;
  animation: shimmer 1.6s infinite linear;
}
.skeleton-title{ height:14px; width:55%; margin-bottom:10px; }
.skeleton-val  { height:28px; width:70%; margin-bottom:10px; }
.skeleton-sub  { height:10px; width:85%; margin-bottom:5px; }
.skeleton-sub2 { height:10px; width:60%; }

/* ====================================================
   GENERAR BUTTON -- PULSE WHEN PENDING
==================================================== */
@keyframes pulse-glow{
  0%, 100%{ box-shadow: 0 4px 20px rgba(213,0,0,.40), 0 0 0 0   rgba(213,0,0,.35); }
  50%     { box-shadow: 0 4px 20px rgba(213,0,0,.70), 0 0 0 10px rgba(213,0,0,.00); }
}
.btn-accent.pending{
  animation: pulse-glow 2.2s ease-in-out infinite;
}

/* ====================================================
   SEGMENTED CONTROL (radioButtons override)
==================================================== */
.seg-control .shiny-input-container{ margin-bottom:0; }
.seg-control .control-label{ display:none; }
.seg-control .shiny-options-group{
  display:flex; gap:0; border-radius:12px; overflow:hidden;
  border:1px solid rgba(255,255,255,.14);
  background:rgba(255,255,255,.04);
  padding:3px;
}
.seg-control .form-check{
  flex:1; margin:0; padding:0;
}
.seg-control .form-check-input{ display:none; }
.seg-control .form-check-label{
  display:flex; align-items:center; justify-content:center;
  padding:6px 10px; border-radius:9px;
  font-size:11.5px; font-weight:600; cursor:pointer;
  color:rgba(255,255,255,.60) !important;
  transition: var(--trans);
  white-space:nowrap; user-select:none;
}
.seg-control .form-check-label:hover{
  color:rgba(255,255,255,.90) !important;
  background:rgba(255,255,255,.06);
}
.seg-control .form-check-input:checked + .form-check-label{
  background: linear-gradient(135deg, var(--accent), var(--accent2)) !important;
  color:#FFFFFF !important;
  font-weight:800 !important;
  box-shadow:0 2px 10px rgba(213,0,0,.40);
}

/* ====================================================
   CARD SECTION HEADER  (.card-hd)
==================================================== */
.card-hd{
  display:flex; align-items:center; gap:10px;
  padding-bottom:10px; margin-bottom:8px;
  border-bottom:1px solid rgba(255,255,255,.07);
}
.card-hd-icon{
  width:32px; height:32px; border-radius:9px; flex-shrink:0;
  display:flex; align-items:center; justify-content:center;
  font-size:16px;
  background:linear-gradient(135deg,rgba(213,0,0,.28),rgba(139,0,0,.18));
  border:1px solid rgba(213,0,0,.30);
  box-shadow:0 2px 10px rgba(213,0,0,.20);
}
.card-hd-text .t{ font-size:13px; font-weight:800; color:#FFFFFF; line-height:1.2; }
.card-hd-text .s{ font-size:10.5px; color:rgba(255,255,255,.72); margin-top:1px; }

/* Right slot in card-hd */
.card-hd-right{ margin-left:auto; display:flex; align-items:center; gap:8px; }

/* ====================================================
   STATUS BADGE
==================================================== */
.status-badge{
  display:inline-flex; align-items:center; gap:7px;
  padding:5px 11px; border-radius:999px;
  font-size:11px; font-weight:700;
  border:1px solid;
  transition: var(--trans);
}
.status-badge.idle{
  background:rgba(213,0,0,.12);
  border-color:rgba(213,0,0,.30);
  color:rgba(255,180,180,.95);
}
.status-badge.idle::before{
  content:''; width:7px; height:7px; border-radius:50%;
  background:rgba(213,0,0,.80); flex-shrink:0;
  animation: blink 1.8s ease-in-out infinite;
}
@keyframes blink{
  0%,100%{ opacity:1; } 50%{ opacity:.25; }
}
.status-badge.ok{
  background:rgba(46,173,74,.12);
  border-color:rgba(46,173,74,.30);
  color:rgba(160,240,175,.95);
}
.status-badge.ok::before{
  content:''; width:7px; height:7px; border-radius:50%;
  background:rgba(46,173,74,.90); flex-shrink:0;
}

/* ====================================================
   SUBTITLE BADGE (map / chart)
==================================================== */
.subtitle-badge{
  display:inline-flex; align-items:center; gap:5px;
  background:rgba(255,255,255,.06);
  border:1px solid rgba(255,255,255,.10);
  border-radius:999px; padding:3px 10px;
  font-size:11px; color:rgba(255,255,255,.82);
  font-weight:500; white-space:nowrap; overflow:hidden;
  text-overflow:ellipsis; max-width:100%;
}

/* ====================================================
   GLASS-CARD WITH PADDING FIX
==================================================== */
.glass-card.p-3{ padding:14px !important; }

/* ====================================================
   MINI PROGRESS BAR (inside KPI)
==================================================== */
.kpi-bar-track{
  width:100%; height:3px; border-radius:999px;
  background:rgba(255,255,255,.10); margin-top:8px; overflow:hidden;
}
.kpi-bar-fill{
  height:100%; border-radius:999px;
  background:linear-gradient(90deg, var(--accent), rgba(213,0,0,.50));
  transition: width .6s cubic-bezier(.4,0,.2,1);
}

/* ====================================================
   NAV TAB ICONS
==================================================== */
.nav-icon{ margin-right:5px; opacity:.85; }
")

# JavaScript for pending button states and tab transitions
app_js <- HTML("
  // Mark GENERAR as pending until applied
  $(document).on('shiny:inputchanged', function(e) {
    if (['election','dl_sel','secciones','map_variable','map_view',
         'winner_vote_type','choro_vote_type','choro_party','choro_metric',
         'choro_scale','choro_opacity','electorado_var','electorado_scale',
         'electorado_opacity'].includes(e.name)) {
      $('#generar').addClass('pending');
    }
    // Mark Optimizar pending on PAUTA param changes
    if (['buf_mode','buf_party','buf_gen_vars','buf_eje','buf_inegi_var',
         'buf_cover_pct','buf_radius_m','buf_max_points'].includes(e.name)) {
      $('#buf_generate').addClass('pending');
    }
  });
  $(document).on('shiny:value', function(e) {
    if (e.name === 'ui_status_run') {
      $('#generar').removeClass('pending');
    }
    if (e.name === 'buf_status') {
      $('#buf_generate').removeClass('pending');
    }
  });
  // Smooth tab transitions
  $(document).on('shown.bs.tab', function() {
    $('.tab-pane.active .glass, .tab-pane.active .glass-card').css('animation','none');
    setTimeout(function(){
      $('.tab-pane.active .glass, .tab-pane.active .glass-card').css('animation','');
    }, 50);
  });
")
