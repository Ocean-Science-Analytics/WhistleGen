# ============================================================
#  Whistle Generator -- Shiny App
#  Ocean Science Analytics styling (matches SpectraReel)
# ============================================================

#required_packages <- c("shiny", "shinyjs", "bslib", "DT", "tuneR", "seewave", "RcppAlgos", "soundgen", "R.devices", "signal", "zip")
#install.packages(required_packages, repos = "https://cran.rstudio.com/")

###### New users will need to download the phonfieldwork tar.gz file from CRAN archive ######
library(shiny)
library(shinyjs)
library(bslib)
library(DT)
library(tuneR)
library(seewave)
library(RcppAlgos)
library(soundgen)
library(R.devices)
library(signal)
library(zip)      

####################################################### FUNCTIONS #####################################################

# Make sure the 'funcs.R' file is in the same folder as this app.R file
source("funcs.R")


# -- CSS ----------------------------------------------------------------------
css <- "
/* ===== Ocean Science Analytics theme ===================================== */
:root {
  --osa-navy:#1E3F6E;   --osa-blue:#2D5DA8;  --osa-steel:#3A74A6;
  --osa-sea:#3796CC;    --osa-sky:#62ADD7;   --osa-foam:#99D0E5;
  --bg-page:#F3F7FA;    --bg-card:#FFFFFF;   --bg-input:#FFFFFF;
  --border:#D6E2EC;     --border-strong:#B9CCDC;
  --text:#1B2B3A;       --muted:#5F7385;     --success:#2E9E6B;
}
body,.shiny-server-status{background:var(--bg-page)!important;color:var(--text)}
.container-fluid{max-width:1500px;padding:0 1.2rem}
h1,h2,h3,.osa-title{font-family:'Questrial','Century Gothic','Inter',sans-serif}

/* ---- Header band with subtle wave ---- */
.osa-header{position:relative;overflow:hidden;margin:1.1rem 0 1.1rem;border-radius:14px;
  background:linear-gradient(120deg,var(--osa-navy) 0%,var(--osa-blue) 55%,var(--osa-steel) 100%);
  padding:1.05rem 1.5rem 2.1rem;color:#fff;box-shadow:0 6px 22px rgba(30,63,110,.18)}
.osa-header-inner{position:relative;z-index:2;display:flex;align-items:center;gap:1.1rem}
.osa-header img{height:44px;width:auto;display:block}
.osa-header .osa-divider{width:1px;align-self:stretch;background:rgba(255,255,255,.35)}
.osa-title{color:#fff;font-size:1.65rem;font-weight:400;letter-spacing:.02em;margin:0;line-height:1.1}
.osa-sub{color:rgba(255,255,255,.82);font-size:.84rem;margin:.2rem 0 0}
.osa-wave{position:absolute;left:0;bottom:0;width:200%;height:46px;z-index:1;pointer-events:none;
  animation:osa-drift 48s linear infinite}
.osa-wave path{vector-effect:non-scaling-stroke}
@keyframes osa-drift{from{transform:translateX(0)}to{transform:translateX(-50%)}}
@media (prefers-reduced-motion:reduce){.osa-wave{animation:none}}

/* Help button sits on the right of the header */
.btn-help{margin-left:auto;background:rgba(255,255,255,.12)!important;color:#fff!important;
  border:1px solid rgba(255,255,255,.55)!important;border-radius:8px!important;
  font-size:.82rem!important;font-weight:600!important;padding:.4rem 1rem!important;transition:background .15s}
.btn-help:hover{background:rgba(255,255,255,.24)!important}

/* ---- Footer wave ---- */
.osa-footer{position:relative;margin:1.6rem 0 0;padding:1.4rem 0 1.1rem;text-align:center;
  color:var(--muted);font-size:.74rem;overflow:hidden}
.osa-footer svg{position:absolute;left:0;top:0;width:100%;height:28px;opacity:.55}
.osa-footer a{color:var(--osa-blue);text-decoration:none}

/* ---- Panels & cards ---- */
.sidebar-panel{background:var(--bg-card)!important;border:1px solid var(--border)!important;
  border-radius:12px;padding:1.2rem!important;height:fit-content;
  box-shadow:0 1px 3px rgba(30,63,110,.06)}
.main-panel{padding-left:1.2rem}
.sv-card{background:var(--bg-card);border:1px solid var(--border);border-radius:12px;
  padding:1.1rem 1.3rem;margin-bottom:.9rem;box-shadow:0 1px 3px rgba(30,63,110,.06)}
.sec-head{color:var(--osa-blue);font-size:.68rem;font-weight:700;letter-spacing:.11em;
  text-transform:uppercase;margin:.9rem 0 .6rem;display:flex;align-items:center;gap:.45rem}
.sec-head::after{content:'';flex:1;height:1px;background:linear-gradient(90deg,var(--border-strong),transparent)}
.sec-head .ico{color:var(--osa-sea)}
.sidebar-panel .sec-head:first-child,.sv-card .sec-head:first-child{margin-top:0}

/* ---- Inputs ---- */
.form-control{background:var(--bg-input)!important;color:var(--text)!important;
  border-color:var(--border-strong)!important;border-radius:6px!important;box-shadow:none!important}
.form-control:focus{border-color:var(--osa-sea)!important;box-shadow:0 0 0 3px rgba(55,150,204,.18)!important}
label,.control-label{color:#3E5468!important;font-size:.79rem!important;font-weight:600!important}
.form-group{margin-bottom:.85rem}

/* Radio buttons: same fix SpectraReel uses for checkboxes under Bootstrap 5 */
.shiny-input-radiogroup .radio{margin:.1rem 0 .4rem;padding:0}
.shiny-input-radiogroup .radio label{display:flex!important;align-items:center;gap:.55rem;
  padding-left:0!important;margin:0;cursor:pointer;color:var(--text)!important;
  font-weight:500!important;font-size:.82rem!important}
.shiny-input-radiogroup .radio input[type=radio]{position:static!important;margin:0!important;
  flex:0 0 auto;width:1rem;height:1rem;accent-color:var(--osa-blue);cursor:pointer}

/* ---- Buttons ---- */
.btn-render{background:linear-gradient(135deg,var(--osa-blue),var(--osa-sea))!important;
  border:none!important;color:#fff!important;font-weight:700!important;width:100%!important;
  padding:.65rem!important;border-radius:8px!important;font-size:.9rem!important;margin-bottom:.45rem!important;
  letter-spacing:.02em;transition:box-shadow .15s,transform .1s,filter .15s!important}
.btn-render:hover{filter:brightness(1.06);box-shadow:0 0 0 3px rgba(55,150,204,.25),0 6px 18px rgba(45,93,168,.3)!important;transform:translateY(-1px)!important}
.btn-render:active{transform:translateY(0)!important}
.btn-render:disabled{filter:grayscale(.4);opacity:.7;transform:none!important;box-shadow:none!important}

/* ---- Status badges ---- */
.badge-idle,.badge-running,.badge-done,.badge-error,.badge-warn{padding:.28rem .85rem;border-radius:20px;font-size:.73rem;font-weight:600;border:1px solid}
.badge-idle{background:#EEF3F7;color:var(--muted);border-color:var(--border)}
.badge-running{background:#E6F1FA;color:var(--osa-blue);border-color:#9CC4E4}
.badge-done{background:#E7F6EF;color:#1F7A51;border-color:#9ED6BC}
.badge-error{background:#FDECEC;color:#B42318;border-color:#F3B5B0}
.badge-warn{background:#FFF6E5;color:#9A5B00;border-color:#F2CF8C}

/* ---- Run / result box ---- */
#sv-output-box{width:100%;min-height:250px;border-radius:12px;position:relative;overflow:hidden;
  display:flex;flex-direction:column;align-items:center;justify-content:center;
  gap:1rem;transition:all .35s ease;box-sizing:border-box;padding:1.5rem;margin-bottom:.9rem}
#sv-output-box.state-idle{display:none}
#sv-output-box.state-rendering{background:linear-gradient(160deg,#EAF3FA 0%,#fff 70%);border:1px solid #9CC4E4}
#sv-output-box.state-done{background:linear-gradient(160deg,#BFE6D1 0%,#E2F4EA 100%);border:1.5px solid #4CAF80}
#sv-output-box.state-error{background:linear-gradient(160deg,#FDECEC 0%,#fff 70%);border:1px solid #F3B5B0}
.sv-box-spinner{width:52px;height:52px;border:4px solid rgba(55,150,204,.2);
  border-top-color:var(--osa-blue);border-radius:50%;animation:sv-spin .8s linear infinite}
@keyframes sv-spin{to{transform:rotate(360deg)}}
@keyframes sv-pulse{0%,100%{opacity:1}50%{opacity:.45}}
.sv-box-title{font-size:1.25rem;font-weight:700;letter-spacing:.04em;text-align:center}
.sv-box-sub{font-size:1rem;color:var(--muted);text-align:center;line-height:1.5;word-break:break-word}
.sv-box-pct{font-family:'Courier New',monospace;font-size:2rem;font-weight:700;
  color:var(--osa-blue);animation:sv-pulse 1.8s ease-in-out infinite}
.sv-box-progress{width:80%;height:6px;background:#E3ECF3;border-radius:20px;overflow:hidden}
.sv-box-progress-bar{height:100%;width:0%;border-radius:20px;
  background:linear-gradient(90deg,var(--osa-blue),var(--osa-sky));transition:width .3s ease}
.sv-checkmark{font-size:2.8rem;color:var(--success);line-height:1}
.sv-path{font-family:'Courier New',monospace;font-size:.82rem;color:var(--osa-navy);
  background:rgba(255,255,255,.7);border:1px solid #9ED6BC;border-radius:6px;padding:.35rem .7rem;
  word-break:break-all;text-align:center}
.sv-dl-btn{background:linear-gradient(135deg,var(--osa-blue),var(--osa-sea))!important;
  border:none!important;color:#fff!important;font-weight:700!important;
  padding:.65rem 1.8rem!important;border-radius:8px!important;font-size:.9rem!important;margin-top:.5rem!important;
  letter-spacing:.02em;transition:box-shadow .15s,transform .1s,filter .15s!important}
.sv-dl-btn:hover{filter:brightness(1.06);box-shadow:0 0 0 3px rgba(55,150,204,.25),0 6px 18px rgba(45,93,168,.3)!important;transform:translateY(-1px)!important}
.sv-dl-btn:active{transform:translateY(0)!important}

/* ---- Summary / tables ---- */
pre.shiny-text-output{background:#F5F9FC!important;color:#24384B!important;
  border:1px solid var(--border)!important;border-radius:8px;font-size:.76rem;line-height:1.55;margin:0;padding:.75rem .95rem!important}
table.dataTable{color:var(--text)!important;font-size:.82rem}
table.dataTable thead th{color:var(--muted)!important;font-size:.72rem;text-transform:uppercase;
  letter-spacing:.04em;border-bottom:1px solid var(--border-strong)!important}
table.dataTable tbody td{border-color:var(--border)!important}
.dataTables_wrapper .dataTables_info,div.dt-info{color:var(--muted)!important;font-size:.74rem!important;padding-top:.9rem!important}
.dataTables_wrapper .pagination,div.dt-paging .pagination{margin-top:.6rem}
.dataTables_wrapper .page-link,div.dt-paging .page-link{font-size:.76rem;padding:.25rem .6rem;
  color:var(--osa-blue);border-color:var(--border)}
.dataTables_wrapper .page-item.active .page-link,div.dt-paging .page-item.active .page-link{
  background:var(--osa-blue);border-color:var(--osa-blue);color:#fff}
.dataTables_wrapper .page-item.disabled .page-link,div.dt-paging .page-item.disabled .page-link{
  background:#fff;color:var(--muted)}
.freq-chip{display:inline-block;min-width:1.4rem;text-align:center;padding:.05rem .4rem;border-radius:5px;
  font-weight:700;font-size:.74rem;color:#fff;margin-right:2px}
.chip-1{background:#1E3F6E}.chip-2{background:#2D5DA8}.chip-3{background:#3796CC}.chip-4{background:#62ADD7}
.warn-line{color:#9A5B00;font-size:.78rem;margin-top:.6rem}
hr.sv-hr{border-color:var(--border);opacity:1;margin:.8rem 0}
.hint{color:var(--muted);font-size:.72rem;line-height:1.4;margin:-.4rem 0 .8rem}
.modal-content{border-radius:12px;border:none}
.modal-header{background:linear-gradient(120deg,var(--osa-navy),var(--osa-blue));color:#fff;
  border-top-left-radius:12px;border-top-right-radius:12px}
.modal-title{font-family:'Questrial','Inter',sans-serif;color:#fff}
.modal-body li{margin-bottom:.45rem;font-size:.88rem}
"

# -- Subtle wave graphics (OSA brand blues) -----------------------------------
# Two copies of the same wave sit side by side so the slow drift loops seamlessly.
wave_path <- function(y, amp, color, opacity, sw) {
  d <- sprintf("M0 %d C 150 %d, 350 %d, 500 %d S 850 %d, 1000 %d S 1350 %d, 1500 %d S 1850 %d, 2000 %d",
               y, y-amp, y+amp, y, y-amp, y, y+amp, y, y-amp, y)
  tags$path(d=d, fill="none", stroke=color, `stroke-opacity`=opacity, `stroke-width`=sw,
            `stroke-linecap`="round")
}
header_wave <- tags$svg(class="osa-wave", viewBox="0 0 2000 46", preserveAspectRatio="none",
  `aria-hidden`="true",
  wave_path(14, 10, "#99D0E5", .55, 5),
  wave_path(24, 12, "#62ADD7", .45, 4),
  wave_path(34, 9,  "#3796CC", .50, 3.5),
  wave_path(42, 7,  "#ffffff", .18, 2)
)
footer_wave <- tags$svg(viewBox="0 0 2000 28", preserveAspectRatio="none", `aria-hidden`="true",
  wave_path(8,  6, "#99D0E5", .9, 3),
  wave_path(15, 7, "#62ADD7", .8, 2.5),
  wave_path(21, 5, "#2D5DA8", .7, 2)
)

# Labelled text input with a hover tooltip (same tooltips as before)
tip_input <- function(id, label, value, tip, placeholder = NULL) {
  tags$div(title = tip, textInput(id, label, value = value, width = "100%", placeholder = placeholder))
}

# -- UI ------------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "icon", type = "image/jpeg", href = "white_square_OSA_med.jpg"),
    tags$style(HTML(css))
  ),
  title = "Whistle Generator | Ocean Science Analytics",
  theme = bs_theme(
    version = 5,
    bg = "#F3F7FA", fg = "#1B2B3A",
    primary = "#2D5DA8", secondary = "#3A74A6", info = "#3796CC",
    success = "#2E9E6B", danger = "#C2410C",
    base_font    = font_google("Inter"),
    heading_font = font_google("Questrial")   # close match to the OSA logotype
  ),

  # -- Header -----------------------------------------------------------------
  div(class = "osa-header",
      div(class = "osa-header-inner",
          tags$img(src = "osa_monogram_white.png", alt = "Ocean Science Analytics"),
          div(class = "osa-divider"),
          div(
            h2(class = "osa-title", "Whistle Generator"),
            p(class = "osa-sub", "Generate artificial whistle sets from four tone frequencies")
          ),
          actionButton("help_btn", HTML("&#63;&nbsp; Help"), class = "btn btn-help")
      ),
      header_wave
  ),

  sidebarLayout(
    # -- SIDEBAR --------------------------------------------------------------
    sidebarPanel(width = 4, class = "sidebar-panel",

      div(class = "sec-head", span(class = "ico", icon("music")), "Frequencies"),
      fluidRow(
        column(6, tip_input("user.freq1", "First (Hz)",  "4186",  "Enter the first frequency value in Hz.")),
        column(6, tip_input("user.freq2", "Second (Hz)", "6272",  "Enter the second frequency value in Hz."))
      ),
      fluidRow(
        column(6, tip_input("user.freq3", "Third (Hz)",  "8372",  "Enter the third frequency value in Hz.")),
        column(6, tip_input("user.freq4", "Fourth (Hz)", "10548", "Enter the fourth frequency value in Hz."))
      ),
      tip_input("samprate", "Sampling Rate (Hz)", "96000",
                "Specify the sampling rate for the signal in Hz."),

      div(class = "sec-head", span(class = "ico", icon("wave-square")), "Whistles"),
      fluidRow(
        column(6, tip_input("totdur", "Duration (ms)", "700",
                            "Specify the total duration of the whistle in milliseconds.")),
        column(6, tip_input("num.whistles", "Number of Whistles", "64",
                            "Specify the total number of whistles to generate."))
      ),
      tip_input("whistle_tapering", "Whistle Tapering (ms)", "5",
                "Specify the tapering duration of the whistle in milliseconds."),

      div(class = "sec-head", span(class = "ico", icon("water")), "Background Noise"),
      fluidRow(
        column(6, tip_input("noise.before", "Before Whistle (ms)", "400",
                            "Specify the duration of noise before the whistle in milliseconds.")),
        column(6, tip_input("noise.after", "After Whistle (ms)", "400",
                            "Specify the duration of noise after the whistle in milliseconds."))
      ),
      tip_input("tapering_duration", "Noise Tapering (ms)", "10",
                "Specify the tapering duration of noise in milliseconds."),

      div(class = "sec-head", span(class = "ico", icon("file-audio")), "Files to Create"),
      radioButtons("file_mode", NULL,
        choices = c("Merged and individual files" = "both",
                    "Merged file only"            = "merged",
                    "Individual files only"       = "individual"),
        selected = "both"),
      tip_input("folder_name", "Download Folder Name", "",
                "Name of the folder inside the downloaded .zip. Leave blank to name it after the frequencies.",
                placeholder = "Leave blank for automatic name"),

      tags$hr(class = "sv-hr"),
      actionButton("run", HTML("&#9654;&nbsp; Generate Whistles"), class = "btn btn-render")
    ),

    # -- MAIN PANEL ------------------------------------------------------------
    mainPanel(width = 8, class = "main-panel",

      # Status row
      div(style = "display:flex;align-items:center;gap:.9rem;margin-bottom:.8rem",
        uiOutput("status_badge"),
        uiOutput("status_text")
      ),

      # -- RUN / RESULT BOX (hidden until Generate is clicked) ----------------
      div(id = "sv-output-box", class = "state-idle",

        div(id = "sv-box-rendering",
          style = "display:none;flex-direction:column;align-items:center;gap:1rem;width:100%",
          div(class = "sv-box-spinner"),
          div(class = "sv-box-title", style = "color:#2D5DA8", "Making Whistles"),
          div(class = "sv-box-pct", id = "sv-box-pct-text", "0%"),
          div(class = "sv-box-progress",
            div(class = "sv-box-progress-bar", id = "sv-box-prog-bar")
          ),
          div(class = "sv-box-sub", id = "sv-box-frame-text", "Starting...")
        ),

        div(id = "sv-box-done",
          style = "display:none;flex-direction:column;align-items:center;gap:.8rem;width:100%",
          div(class = "sv-checkmark", HTML("&#10003;")),
          div(class = "sv-box-title", style = "color:var(--success)", "Whistles Ready!"),
          uiOutput("done_msg_ui")
        ),

        div(id = "sv-box-error",
          style = "display:none;flex-direction:column;align-items:center;gap:.8rem;width:100%",
          div(style = "font-size:2.5rem;line-height:1", HTML("&#9888;")),
          div(class = "sv-box-title", style = "color:#B42318", "Generation Failed"),
          uiOutput("error_msg_ui")
        )
      ),

      # Settings summary
      div(class = "sv-card",
        div(class = "sec-head", span(class = "ico", icon("sliders")), "Run Summary"),
        verbatimTextOutput("settings_summary"),
        uiOutput("nyquist_warning")
      ),

      # Pattern table
      div(class = "sv-card",
        div(class = "sec-head", span(class = "ico", icon("table-list")), "Whistle Patterns"),
        div(class = "hint", style = "margin-top:0",
            "Each whistle is three tones in a row. Numbers 1-4 refer to the four frequencies, in order."),
        DTOutput("pattern_table")
      )
    )
  ),

  # -- Footer -----------------------------------------------------------------
  div(class = "osa-footer", footer_wave,
      HTML("Whistle Generator &middot; <a href='https://oceanscienceanalytics.com' target='_blank'>Ocean Science Analytics</a>"))
)

############################################################

server <- function(input, output, session) {

  rv <- reactiveValues(status = "idle", error_msg = NULL, done_msg = NULL,
                       zip_path = NULL, zip_name = NULL, run_dir = NULL)

  # Each run works in its own temporary folder on the server; clean up when the user leaves
  session$onSessionEnded(function() {
    d <- isolate(rv$run_dir)
    if (!is.null(d)) unlink(d, recursive = TRUE)
  })

  # Folder name inside the zip: the user's name (made file-safe) or one built from the frequencies
  download_folder <- reactive({
    nm <- trimws(input$folder_name)
    nm <- gsub("[^A-Za-z0-9 ._-]+", "_", nm)   # strip characters not allowed in folder names
    nm <- gsub("^[ .]+|[ .]+$", "", nm)
    if (nzchar(nm)) return(nm)
    paste0("Whistles_", input$user.freq1, "_", input$user.freq2, "_", input$user.freq3, "_",
           input$user.freq4, "_", input$totdur, "ms_SR_", input$samprate)
  })

  observeEvent(input$help_btn, {
    showModal(modalDialog(
      title = "Help",
      p("Welcome to the Whistle Generator! Here's how to use the application:"),
      tags$ol(
        tags$li("Enter the desired frequencies (Hz) for whistle generation. This app will generate up to 64 unique whistle patterns using these 4 frequencies."),
        tags$li("Specify the sample rate (Hz). It should be higher than the upper frequency so that all whistles fit within the window."),
        tags$li("Specify the duration of the individual whistles as well as the number of whistles produced. Note that there are only 64 unique whistle patterns that can be produced from the four frequencies specified above."),
        tags$li("Specify the tapering of the whistles and noise (ms). Tapering refers to the time before the whistles begin and after the whistles end. A 0 ms tapering will cause a clicking sound at both the beginning and end of the whistles. The default 5 ms tapering removes this clicking sound while not producing any empty breaks in the noise."),
        tags$li("Specify the duration of noise before and after each whistle (ms)."),
        tags$li("Select which files to create. 'Merged file only' produces a single WAV file containing all generated whistles. 'Individual files only' produces individual WAV files for each generated whistle."),
        tags$li("Optionally name the download folder. Leave it blank to name it after the frequencies."),
        tags$li("Click 'Generate Whistles', then 'Download Whistles'. You'll get a .zip file; open it to find a folder containing all the WAV files.")
      ),
      p("For more detailed documentation, please see the READ.ME file on github."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # -- Numeric inputs, checked ------------------------------------------------
  num_fields <- c(user.freq1 = "First frequency", user.freq2 = "Second frequency",
                  user.freq3 = "Third frequency", user.freq4 = "Fourth frequency",
                  samprate = "Sampling rate", totdur = "Whistle duration",
                  num.whistles = "Number of whistles", whistle_tapering = "Whistle tapering",
                  noise.before = "Noise before whistle", noise.after = "Noise after whistle",
                  tapering_duration = "Noise tapering")

  bad_fields <- reactive({
    vals <- vapply(names(num_fields), function(id) {
      v <- suppressWarnings(as.numeric(input[[id]]))
      length(v) == 1 && !is.na(v) && v >= 0
    }, logical(1))
    unname(num_fields[!vals])
  })

  # Same permutation logic as the generator itself
  patterns <- reactive({
    req(length(bad_fields()) == 0)
    all.freq <- c(as.numeric(input$user.freq1), as.numeric(input$user.freq2),
                  as.numeric(input$user.freq3), as.numeric(input$user.freq4))
    r     <- RcppAlgos::permuteGeneral(all.freq, 3, repetition = TRUE)
    r.val <- RcppAlgos::permuteGeneral(1:4, 3, repetition = TRUE)
    n <- min(as.numeric(input$num.whistles), nrow(r))
    list(r = r, r.val = r.val, n = n, all.freq = all.freq)
  })

  # -- Status row ---------------------------------------------------------------
  output$status_badge <- renderUI({
    switch(rv$status,
      idle    = span(class = "badge-idle",    "Ready"),
      running = span(class = "badge-running", "Generating..."),
      done    = span(class = "badge-done",    "Complete"),
      error   = span(class = "badge-error",   "Error"))
  })
  output$status_text <- renderUI({
    if (length(bad_fields()) > 0)
      return(span(style = "color:#B42318;font-size:.8rem",
                  paste("Check these values:", paste(bad_fields(), collapse = ", "))))
    p <- patterns()
    span(style = "color:var(--muted);font-size:.8rem",
         sprintf("%d whistle%s will be generated", p$n, if (p$n == 1) "" else "s"))
  })

  # -- Run summary --------------------------------------------------------------
  output$settings_summary <- renderText({
    validate(need(length(bad_fields()) == 0,
                  paste("Enter a number (0 or more) for:", paste(bad_fields(), collapse = ", "))))
    p <- patterns()
    mode_lbl <- c(both = "Merged and individual files", merged = "Merged file only",
                  individual = "Individual files only")[[input$file_mode]]
    folder <- paste0(download_folder(), ".zip")
    sprintf(
"Download        : %s
Files to create : %s
Frequencies     : %s Hz
Sampling rate   : %s Hz
Whistles        : %d of %d possible patterns, %s ms each
Whistle taper   : %s ms
Noise           : %s ms before, %s ms after, %s ms taper",
      folder, mode_lbl,
      paste(format(p$all.freq, big.mark = ",", scientific = FALSE, trim = TRUE), collapse = " / "),
      format(as.numeric(input$samprate), big.mark = ",", scientific = FALSE),
      p$n, nrow(p$r), input$totdur, input$whistle_tapering,
      input$noise.before, input$noise.after, input$tapering_duration)
  })

  output$nyquist_warning <- renderUI({
    req(length(bad_fields()) == 0)
    sr <- as.numeric(input$samprate); top <- max(patterns()$all.freq)
    if (top >= sr / 2)
      div(class = "warn-line", HTML(sprintf(
        "&#9888; The highest frequency (%s Hz) is at or above half the sampling rate (%s Hz). Raise the sampling rate so every tone can be recorded.",
        format(top, big.mark = ","), format(sr / 2, big.mark = ","))))
  })

  # -- Pattern table ------------------------------------------------------------
  output$pattern_table <- renderDT({
    p <- patterns()
    idx <- seq_len(p$n)
    chips <- apply(p$r.val[idx, , drop = FALSE], 1, function(v)
      paste(sprintf("<span class='freq-chip chip-%d'>%d</span>", v, v), collapse = ""))
    df <- data.frame(
      `#`        = idx,
      Pattern    = chips,
      `Tone 1 (Hz)` = p$r[idx, 1],
      `Tone 2 (Hz)` = p$r[idx, 2],
      `Tone 3 (Hz)` = p$r[idx, 3],
      check.names = FALSE)
    datatable(df, escape = FALSE, rownames = FALSE, selection = "none",
              options = list(pageLength = 8, lengthChange = FALSE, searching = FALSE,
                             ordering = FALSE, dom = "tip",
                             columnDefs = list(list(className = "dt-center", targets = "_all"))))
  })

  # -- Box state helpers (same approach as SpectraReel) ------------------------
  show_box_state <- function(state) {
    # state: "idle" | "rendering" | "done" | "error"
    runjs(sprintf('
      var box = document.getElementById("sv-output-box");
      box.className = "state-%s";
      ["sv-box-rendering","sv-box-done","sv-box-error"].forEach(function(id){
        var el = document.getElementById(id);
        if(el) el.style.display = "none";
      });
      var active = document.getElementById("sv-box-%s");
      if(active) active.style.display = "flex";
      if("%s" !== "idle") box.scrollIntoView({behavior:"smooth", block:"nearest"});
    ', state, state, state))
  }

  update_progress <- function(frac, label = "") {
    pct <- round(frac * 100)
    runjs(sprintf('
      var pb = document.getElementById("sv-box-prog-bar");
      var pt = document.getElementById("sv-box-pct-text");
      var ft = document.getElementById("sv-box-frame-text");
      if(pb) pb.style.width = "%d%%";
      if(pt) pt.textContent = "%d%%";
      if(ft) ft.textContent = %s;
    ', pct, pct, jsonlite::toJSON(label, auto_unbox = TRUE)))
  }

  fail <- function(msg) {
    rv$status <- "error"; rv$error_msg <- msg
    show_box_state("error")
  }

  output$error_msg_ui <- renderUI({
    req(rv$error_msg)
    div(class = "sv-box-sub", style = "color:#B42318", rv$error_msg)
  })
  output$done_msg_ui <- renderUI({
    req(rv$done_msg)
    tagList(
      div(class = "sv-box-sub", rv$done_msg),
      div(class = "sv-path", rv$zip_name),
      downloadButton("dl_zip", "Download Whistles", class = "btn sv-dl-btn")
    )
  })

  output$dl_zip <- downloadHandler(
    filename = function() rv$zip_name,
    content  = function(file) file.copy(rv$zip_path, file, overwrite = TRUE),
    contentType = "application/zip"
  )

  ########## Creates functionality in the run button to generate signals ################
  observeEvent(input$run, {

    rv$error_msg <- NULL; rv$done_msg <- NULL

    # Check inputs before starting
    if (length(bad_fields()) > 0)
      return(fail(paste("Enter a number (0 or more) for:", paste(bad_fields(), collapse = ", "))))

    # Fresh temporary folder for this run (the previous run's files are removed).
    # Files are made in <run_dir>/<folder name>/ and that folder is zipped for download.
    if (!is.null(rv$run_dir)) unlink(rv$run_dir, recursive = TRUE)
    rv$zip_path <- NULL; rv$zip_name <- NULL
    run_dir     <- tempfile("whistles_")
    folder_name <- download_folder()
    work_dir    <- file.path(run_dir, folder_name)
    dir.create(work_dir, recursive = TRUE)
    rv$run_dir  <- run_dir

    old_wd <- setwd(work_dir)
    on.exit(setwd(old_wd), add = TRUE)

    # Folder path must end in "/": create.triplet.signal() in funcs.R joins
    # folder + file name with no separator.
    out_dir <- paste0(work_dir, "/")

    p <- patterns()
    r <- p$r; r.val <- p$r.val
    # Number of whistles to generate (capped at the number of unique patterns)
    num_whistles <- p$n
    mode <- input$file_mode

    rv$status <- "running"
    disable("run")
    on.exit(enable("run"), add = TRUE)
    show_box_state("rendering")
    update_progress(0, "Starting...")

    # File names: the "both" option has always used the short "_ms_SR_" naming,
    # the other two options the longer "_Dur..ms_SamplRate_" naming. Kept as-is.
    long_names <- mode != "both"

    tryCatch({
      ########## Generating Signals #########
      for (i in 1:num_whistles) {
        freq1.new <- r[i, 1]
        freq2.new <- r[i, 2]
        freq3.new <- r[i, 3]

        code <- paste0(as.character(r.val[i,1]), as.character(r.val[i,2]), as.character(r.val[i,3]))
        triplet.name <- if (long_names) {
          paste0("Trip_", code, "_", as.character(freq1.new), "_", as.character(freq2.new), "_", as.character(freq3.new),
                 "_Dur", as.character(input$totdur), "ms", "_SamplRate_", as.character(input$samprate), ".wav")
        } else {
          paste0("Trip_", code, "_", as.character(freq1.new), "_", as.character(freq2.new), "_", as.character(freq3.new),
                 "_", as.character(input$totdur), "ms", "_SR_", as.character(input$samprate), ".wav")
        }

        # soundgen warns that pitch should be under 3500 Hz. funcs.R already tells it
        # to ignore that limit (invalidArgAction = 'ignore'), so hide just that warning.
        out <- withCallingHandlers(
          create.triplet.signal(
            freq1 = freq1.new,
            freq2 = freq2.new,
            freq3 = freq3.new,
            dir = out_dir,
            triplet.name = triplet.name,
            sample.rate = as.numeric(input$samprate),
            duration = as.numeric(input$totdur),
            noise.before = as.numeric(input$noise.before),
            noise.after = as.numeric(input$noise.after),
            whistle.tapering = as.numeric(input$whistle_tapering),
            tapering.duration = as.numeric(input$tapering_duration)
          ),
          warning = function(w) {
            if (grepl("should be between", conditionMessage(w))) invokeRestart("muffleWarning")
          })

        # Remove all variables for this iteration
        rm(freq1.new, freq2.new, freq3.new, triplet.name, out)

        # Merging takes a moment, so leave the last 5% of the bar for it
        frac <- if (mode == "individual") i / num_whistles else 0.95 * i / num_whistles
        update_progress(frac, sprintf("Whistle %d of %d", i, num_whistles))
      }

      directory_path <- out_dir
      merged_name <- NULL

      if (mode == "both") {
        update_progress(0.97, "Merging whistles into one file...")
        merged_name <- paste0(
          "AllArtificialWhistles_",
          input$user.freq1, "_", input$user.freq2, "_", input$user.freq3, "_", input$user.freq4,
          "_", as.character(input$totdur), "ms", "_SR_", as.character(input$samprate))
        merge.wav(out_dir, merged_name)

      } else if (mode == "merged") {
        # Get a list of the individual whistle files in the specified directory
        files_to_remove <- list.files(path = directory_path, pattern = "^Trip")

        update_progress(0.97, "Merging whistles into one file...")
        merged_name <- paste0(
          "AllArtificialWhistles_",
          input$user.freq1, "_", input$user.freq2, "_", input$user.freq3, "_", input$user.freq4,
          "_Dur", as.character(input$totdur), "ms", "_SamplRate_", as.character(input$samprate))
        merge.wav(out_dir, merged_name)

        # Remove the individual files
        file.remove(file.path(directory_path, files_to_remove))
      }

      # Zip the whole folder so the download opens to one folder of WAV files
      update_progress(0.99, "Packing files into a .zip...")
      zip_path <- file.path(run_dir, paste0(folder_name, ".zip"))
      zip::zip(zip_path, files = folder_name, root = run_dir)

      update_progress(1, "Complete!")
      rv$done_msg <- switch(mode,
        both       = sprintf("%d individual whistle files and 1 merged file are ready in:", num_whistles),
        merged     = sprintf("1 merged file with %d whistles is ready in:", num_whistles),
        individual = sprintf("%d individual whistle files are ready in:", num_whistles))
      rv$zip_path <- zip_path
      rv$zip_name <- basename(zip_path)
      rv$status <- "done"
      show_box_state("done")

    }, error = function(e) {
      fail(conditionMessage(e))
    })

  })  # End of observeEvent(input$run, {...}
} # End of server


shinyApp(ui, server)
