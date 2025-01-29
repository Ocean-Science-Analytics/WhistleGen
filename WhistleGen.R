# Shiny GUI to create dolphin whistles
# Version 3

####################################################################################################
#required_packages <- c("shiny", "shinyjs", "shinyBS", "bslib", DT", "tuneR", "seewave", "RcppAlgos", "shinyWidgets", "soundgen", "shinyFiles", "R.devices", "signal")
#install.packages(required_packages, repos = "https://cran.rstudio.com/")

###### New users will need to download the phonfieldwork tar.gz file from CRAN archive ######
library(shiny)
library(shinyjs)
library(shinyBS)
library(bslib)
library(DT)
library(tuneR)
library(seewave)
library(RcppAlgos)
library(shinyWidgets)
library(soundgen)
library(shinyFiles)
library(R.devices)
library(signal)

####################################################### FUNCTIONS #####################################################

# Make sure the 'funcs.R' file is in the same directory as this ('WhistleGen.R') file
current_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_directory)
source("funcs.R")

ui <- fluidPage(
  
  tags$style(HTML("
    .help-btn {
      position: absolute;
      top: 10px;
      right: 10px;
      z-index: 1000;
    }
  ")),
  
  # Help button
  actionButton("help_btn", "Help", class = "btn btn-primary help-btn"),
  
  titlePanel(
    h1("Whistle Generator", style = "color: black; font-size: 24px; font-weight: bold;", align = "center")
  ),
  
  # Making background blue for now
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "radial",
    direction = c("top", "left")
  ),
  
  fluidRow(
    column(5, offset = 1,
           # Using text input for directory for now ... don't really seem to be any better options  
           tags$div(
             title = "Provide the directory path where data will be downloaded.",
             textInput('sound.library', 'Path for data download', value = "C:\\Users\\16614\\Documents\\OSA\\Whistle Gen\\Signals\\")),
           
           div(style = "width: 10px;"),
           
           # Specify four signal frequencies
           tags$div(
             title = "Enter the first frequency value in Hz.",
             textInput('user.freq1', 'First Frequency (Hz)', value = '4186', width = '52%')),
           
           tags$div(
             title = "Enter the second frequency value in Hz.",
             textInput('user.freq2', 'Second Frequency (Hz)', value = '6272', width = "52%")),
           
           tags$div(
             title = "Enter the third frequency value in Hz.",
             textInput('user.freq3', 'Third Frequency (Hz)', value = '8372', width = "52%")),
           
           tags$div(
             title = "Enter the fourth frequency value in Hz.",
             textInput('user.freq4', 'Fourth Frequency (Hz)', value = '10548', width = "52%")),
           
           tags$div(
             title = "Specify the sampling rate for the signal in Hz.",
             textInput('samprate', 'Sampling Rate (Hz)', value = '96000', width = "52%")),
           
           tags$div(
             title = "Check this box to generate both merged and individual files.",
             checkboxInput('generate_both', 'Generate Merged and Individual Files', value = TRUE)),
           
           tags$div(
             title = "Check this box to generate only a merged file.",
             checkboxInput('generate_merged', 'Generate Merged File', value = FALSE)),
           
           tags$div(
             title = "Check this box to generate only individual files.",
             checkboxInput('generate_individual', 'Generate Individual Files', value = FALSE))
    ),
    column(5, offset = 1,
           # Specify total signal duration
           tags$div(
             title = "Specify the total duration of the whistle in milliseconds.",
             textInput('totdur', 'Whistle duration (ms)', value = '700', width = "52%")),
           
           tags$div(
             title = "Specify the total number of whistles to generate.",
             textInput('num.whistles', 'Number of Whistles', value = '64', width = "52%")),
           
           tags$div(
             title = "Specify the tapering duration of the whistle in milliseconds.",
             textInput('whistle_tapering', 'Whistle Tapering (ms)', value = '5', width = "52%")),
           
           # Specify noise before and after whistles
           tags$div(
             title = "Specify the duration of noise before the whistle in milliseconds.",
             textInput('noise.before', 'Noise Before Whistle (ms)', value = '400', width = "52%")),
           
           tags$div(
             title = "Specify the duration of noise after the whistle in milliseconds.",
             textInput('noise.after', 'Noise After Whistle (ms)', value = '400', width = "52%")),
           
           tags$div(
             title = "Specify the tapering duration of noise in milliseconds.",
             textInput('tapering_duration', 'Noise Tapering (ms)', value = '10', width = "52%")),
           
           tags$head(
             tags$style(HTML('#run{background-color:orange}'))
           )
    ),
    column(6, offset=4,
           actionButton("run", "Generate Whistles", width = 300, height=200),
           
           
    )
  )
)

############################################################

server <- function(input, output, session) {
  
  observeEvent(input$help_btn, {
    showModal(modalDialog(
      title = "Help",
      p("Welcome to the Whistle Generator! Here’s how to use the application:"),
      tags$ol(
        tags$li("Provide the directory path where data will be downloaded too."),
        tags$li("Enter the desired frequencies (Hz) for whistle generation. This app will generate up to 64 unique whistle patterns using these 4 frequencies."),
        tags$li("Specify the sample rate (Hz). It should be higher than the upper frequency so that all whistles fit within the window."),
        tags$li("Specify the duration of the individual whistles as well as the number of whistles produced. Note that there are only 64 unique whistle patterns that can be produced from the four frequencies specified above."),
        tags$li("Specify the tapering of the whistles and noise (ms). Tapering refers to the time before the whistles begin and after the whistles end. A 0 ms tapering will cause a clicking sound at both the beginning and end of the whistles. The default 5 ms tapering removes this clicking sound while not producing any empty breaks in the noise."),
        tags$li("Specify the duration of noise before and after each whistle (ms)."),
        tags$li("Select a file option. 'Generate Merged File' produces a single WAV file conatining all generated whistles. 'Generate Individual Files' produces individual WAV files for each generated whistle."),
        tags$li("Click 'Generate Whistles' to create the whistle files.")
      ),
      p("For more detailed documentation, please see the READ.ME file on github."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  ########## Creates functionality in the run button to generate signals ################
  observeEvent(input$run, {
    
    setwd(input$sound.library)
    
    # Total signal length in sample points
    amp.env2 <- as.numeric(input$amp.env)*-1
    
    # Using total length and user input percent to impute the flat segment time and the ramp segment time.
    flatseg.time2 <- amp.env2 * (as.numeric(input$flatseg.per) / 100)
    
    # Getting ramp segment percent of total time inferred from the user input of flat segment percent
    ramp.seg.per <- 100 - as.numeric(input$flatseg.per)
    
    # Calculating ramp segment time
    rampseg.time2 <- amp.env2 * (ramp.seg.per / 100)
    
    # Collecting fade time from user input
    # Setting fade time to 0 to eliminate for now but leaving in as an option
    fade.time2 <- as.numeric(0) * 24
    
    # Directory input (Can't use windows format otherwise it thinks it's a return)
    direct.input1 <- input$sound.library
    
    ########### Status Bar ########
    # Create an empty dataframe to update progress
    dat <- data.frame(x = numeric(0), y = numeric(0))
    # Create a Progress object
    progress <- shiny::Progress$new()
    
    progress$set(message = "Making Whistles", value = 0)
    
    ########## Generating Signals #########
    # Putting insides of generate.batch function here for now so can monitor progress.
    # Probably will be better in the longer term to put the insides of this in a function to clean it up
    all.freq <- c(as.numeric(input$user.freq1), as.numeric(input$user.freq2), as.numeric(input$user.freq3), as.numeric(input$user.freq4))
    r <- RcppAlgos::permuteGeneral(all.freq, 3, repetition=TRUE)
    r.val <- RcppAlgos::permuteGeneral(1:4, 3, repetition=TRUE)
    
    # Number of whistles to generate
    num_whistles <- as.numeric(input$num.whistles)
    
    if (num_whistles > length(r[, 1])) {
      num_whistles <- length(r[, 1])
    }
    
    # Check if multiple checkboxes are selected
    if ((input$generate_both && (input$generate_individual || input$generate_merged)) ||
        (input$generate_individual && input$generate_merged)) {
      showNotification("Please only select one file generation option")
      return()
    }
    
    # Check if no checkboxes are selected
    if (!input$generate_both && !input$generate_individual && !input$generate_merged) {
      showNotification("Please select a file generation option")
      return()
    }
    
    
    # Generate individual files
    if (input$generate_both) {
      for (i in 1:num_whistles) {
        freq1.new <- r[i, 1]
        freq2.new <- r[i, 2]
        freq3.new <- r[i, 3]
        
        triplet.name <- paste0(
          "Trip_", as.character(r.val[i,1]),as.character(r.val[i,2]),as.character(r.val[i,3]), "_", as.character(freq1.new), "_", as.character(freq2.new), "_", as.character(freq3.new),"_", as.character(input$totdur), "ms", "_SR_", as.character(input$samprate), ".wav")
        #"Trip_", as.character(r.val[i,1]),as.character(r.val[i,2]),as.character(r.val[i,3]), "_", as.character(freq1.new), "_", as.character(freq2.new), "_", as.character(freq3.new),"_Dur", as.character(input$totdur), "ms", "_SamplRate_", as.character(input$samprate), ".wav")
  
        out <- create.triplet.signal(
          freq1 = freq1.new,
          freq2 = freq2.new,
          freq3 = freq3.new,
          dir = input$sound.library,
          triplet.name = triplet.name,
          sample.rate = as.numeric(input$samprate),
          duration = as.numeric(input$totdur),
          noise.before = as.numeric(input$noise.before),
          noise.after = as.numeric(input$noise.after),
          whistle.tapering = as.numeric(input$whistle_tapering),
          tapering.duration = as.numeric(input$tapering_duration)
        )
        
        # Remove all variables for this iteration
        rm(freq1.new, freq2.new, freq3.new, triplet.name, out)
        
        # Increment the progress bar, and update the detail text.
        progress$inc(1 / num_whistles, detail = paste("Whistle Number ", i))
      }
      
      # Get the path to the download directory
      directory_path <- input$sound.library
      
      # Merge the individual whistle files outside the loop
      merge.wav(
        input$sound.library,
        paste0(
          "AllArtificialWhistles_",
          input$user.freq1, "_", input$user.freq2, "_", input$user.freq3, "_", input$user.freq4,
          "_", as.character(input$totdur), "ms", "_SR_", as.character(input$samprate)
        )
      )
    }
    
    else if (input$generate_individual) {
      for (i in 1:num_whistles) {
        freq1.new <- r[i, 1]
        freq2.new <- r[i, 2]
        freq3.new <- r[i, 3]
        
        triplet.name <- paste0(
          "Trip_", as.character(r.val[i,1]),as.character(r.val[i,2]),as.character(r.val[i,3]), "_", as.character(freq1.new), "_", as.character(freq2.new), "_", as.character(freq3.new),"_Dur", as.character(input$totdur), "ms", "_SamplRate_", as.character(input$samprate), ".wav")
        
        out <- create.triplet.signal(
          freq1 = freq1.new,
          freq2 = freq2.new,
          freq3 = freq3.new,
          dir = input$sound.library,
          triplet.name = triplet.name,
          sample.rate = as.numeric(input$samprate),
          duration = as.numeric(input$totdur),
          noise.before = as.numeric(input$noise.before),
          noise.after = as.numeric(input$noise.after),
          whistle.tapering = as.numeric(input$whistle_tapering),
          tapering.duration = as.numeric(input$tapering_duration)
        )
        
        # Remove all variables for this iteration
        rm(freq1.new, freq2.new, freq3.new, triplet.name, out)
      }
    }
    
    else if (input$generate_merged) {
      for (i in 1:num_whistles) {
        freq1.new <- r[i, 1]
        freq2.new <- r[i, 2]
        freq3.new <- r[i, 3]
        
        triplet.name <- paste0(
          "Trip_", as.character(r.val[i,1]),as.character(r.val[i,2]),as.character(r.val[i,3]), "_", as.character(freq1.new), "_", as.character(freq2.new), "_", as.character(freq3.new),"_Dur", as.character(input$totdur), "ms", "_SamplRate_", as.character(input$samprate), ".wav")
        
        out <- create.triplet.signal(
          freq1 = freq1.new,
          freq2 = freq2.new,
          freq3 = freq3.new,
          dir = input$sound.library,
          triplet.name = triplet.name,
          sample.rate = as.numeric(input$samprate),
          duration = as.numeric(input$totdur),
          noise.before = as.numeric(input$noise.before),
          noise.after = as.numeric(input$noise.after),
          whistle.tapering = as.numeric(input$whistle_tapering),
          tapering.duration = as.numeric(input$tapering_duration)
        )
        
        # Remove all variables for this iteration
        rm(freq1.new, freq2.new, freq3.new, triplet.name, out)
        
        # Increment the progress bar, and update the detail text.
        progress$inc(1 / num_whistles, detail = paste("Whistle Number ", i))
      }
      
      # Get the path to the download directory
      directory_path <- input$sound.library
      
      # Get a list of files starting with "All" in the specified directory
      files_to_remove <- list.files(path = directory_path, pattern = "^Trip")
      
      
      # Merge the individual whistle files
      merge.wav(
        input$sound.library,
        paste0(
          "AllArtificialWhistles_",
          input$user.freq1, "_", input$user.freq2, "_", input$user.freq3, "_", input$user.freq4,
          "_Dur", as.character(input$totdur), "ms", "_SamplRate_", as.character(input$samprate)
        )
      )
      # Remove those files
      file.remove(file.path(directory_path, files_to_remove))
    }
    
    progress$close()
    
  })  # End of observeEvent(input$run, {...}
} # End of server


shinyApp(ui, server)
  