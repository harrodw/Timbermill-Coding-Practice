################################################################################
# Title: Validating BirdNET classifications with an R Shiny app
# Author: Will Harrod
# Date Created: 2025-12-23
################################################################################

# 0) Add packages ##############################################################

# Packages 
library(tidyverse)
library(seewave)
library(tuneR)
library(shiny)
library(bslib)
library(audio)

# Clear environments
rm(list = ls())

# 1) Setup Environment #########################################################

# File Paths (Change these for your device) ------------------------------------
# Name of the directory that holds your validation data 
validations_dir <- "C:\\Users\\willh\\Documents\\NCSU\\Disertation_Code\\Disertation_Coding_Practice\\Data\\"
# Name of the validation data 
validations_file <- "uf_validations.csv"
# ------------------------------------------------------------------------------

# Combine file name and path
file_path <- paste0(validations_dir, validations_file)


# Define how often you want to autosave 
save_rate <- 25

# Set working directory to the script's location
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Create the www folder if it doesn't exist
if (!dir.exists("www")) {
  dir.create("www")
}

# 2) Data Preparation ##########################################################

# Add the birds data
birds_to_valid_raw <- read.csv(file_path) 
# glimpse(birds_to_valid_raw)

# Get a list of species for the dropdown
species_list <- sort(unique(birds_to_valid_raw$Species))
# species_list

# Length of the species list 
n_species <- length(species_list)
# n_species

# Number of validations per species 
n_valid <- nrow(birds_to_valid_raw)/n_species
# n_valid

# Clean the validation data 
birds_to_valid <- birds_to_valid_raw %>% 
  group_by(Species) %>%
  mutate(Voc.ID = 1:n()) %>% 
  ungroup() %>% 
  mutate(Confidence = round(Confidence, 2)) %>% 
  select(rowid, Species, Confidence, True.Positive, Voc.ID, ARU.Name, Date, Hour, Minute, Second, File)
# glimpse(birds_to_valid)

# Define the default bird 
default_bird <- birds_to_valid %>% 
  arrange(Species) %>% 
  slice_head(n = 1) %>% 
  pull(Species)
# default_bird

# Starting snippets to vaildate
default_valid_ids <- birds_to_valid %>% 
  filter(Species == default_bird) %>% 
  pull(Voc.ID)
# default_valid_ids

# Pallette for the pie chart
pie_pal <- c(
  "Not Validated" = "gray75",
  "True Positive" = "green3",
  "False Positive" = "orangered1"
  )

# 3) Shiny UI ##################################################################

# UI call
ui <- page_fluid(
  
  # Theme and title
  theme = bs_theme(version = version_default(), bootswatch = "superhero"),
  # div(titlePanel("BirdNet Validation"), style = "text-align: center;"),
  
  # ----------------------------------------------------------------------------
  # Selection  -----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Start the first row
  fluidRow(

  # Choose a species
  column(6,
         # Cusom font for species selection
         h5("Select a Species", style = "margin-bottom: 10px;"), 
         # Begin species selection
         selectInput("species", NULL,
                     choices = species_list,
                     selected = default_bird,
                     width = "100%"
      )), # End species choice
      
      # Select whether or not to only see rows that are now validated 
      column(6,
          h5("Vocalizations Type", style = "margin-bottom: 10px;"), 
           # Begin valdiation type selection
           selectInput("unval_only", NULL,
                  choices = c("All Vocalizations", "Unvalidated Vocalizations"),
                  selected = "Show all Vocalizations",
                  width = "100%"
      )), # End validation choice
         
  # Hidden vocalization selection     
  div(style = "display: none;",
      selectInput("voc", "Vocalization Number",
                  choices = NULL,
                  selected = NULL,
                  width = "100%"
      ))  # End vocalization choice

  ), # End selection
  
  # ----------------------------------------------------------------------------
  # Display --------------------------------------------------------------------
  # ----------------------------------------------------------------------------
 
  # Start the display row
   fluidRow(
     # Plot the vocalization spectrogram
     column(8, class = "mt-1",
      plotOutput("spectro_plot")
     ), # End spectro plot
    
     # Plot the progress
     column(4, class = "mt-1",
      plotOutput("progress_plot") 
    ) # End progress plot 
  ),
  
  # Play the sound
  fluidRow(
  column(12, class = "mt-2",
  actionButton("play", "Play Sound Again", 
               icon = icon("volume-up"),
               width = "100%",
               class = "btn-primary"),
  
  # Listen to the hidden Audio Element
  tags$div(id = "audio_container")
  
  )), # End display
  
  # ----------------------------------------------------------------------------
  # Validate -------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Display the species name
  fluidRow(
    column(12, class = "mt-2",
    h5(
       textOutput("species_name"), 
       style = "text-align: center;"
    ))),
  
  # Start the third row
  fluidRow(
    
    # True positive option
    column(6, class = "mt-1",
    actionButton("true_pos", "Yes, True Positive?",
                 icon = icon("thumbs-up"),
                 width = "100%",
                 class = "btn-success"
                 )), # End true positive option

    # False Positive Option
    column(6, class = "mt-1",
    actionButton("false_pos", "No, False Positive?",
                 icon("thumbs-down"),
                 width = "100%",
                 class = "btn-danger"
                 )) # end false positive option
    
  ), # End validation section
  
  # ----------------------------------------------------------------------------
  # Navigation -----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Start the fourth row
  fluidRow(

    # Previous option
    column(6, class = "mt-2",
    actionButton("prev_voc", "Previous",
                 icon = icon("chevron-left"),
                 width = "100%",
                 class = "btn-warning"
    )), # End next option

    # Next Option
    column(6, class = "mt-2",
    actionButton("next_voc", "Next",
                 icon = icon("chevron-right"),
                 width = "100%",
                 class = "btn-warning"
                 
    )) # end next option
    
  ), # End navigation
  
  # ----------------------------------------------------------------------------
  # Download -------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Download button
  fluidRow(
    column(12, class = "mt-2",
      actionButton("download", "Download",
                   icon("download"),
                   width = "100%",
                   class = "btn-info"
      ) # End download button
    )) # End download 
  
 ) # End UI

# 4) Shiny Server ##############################################################

# Call the shiny server
server <- function(input, output, session) {
  
  # ----------------------------------------------------------------------------
  # Setup ----------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Initialize reactive values
  validations <- reactiveVal(birds_to_valid)
  save_counter <- reactiveVal(0)
  
  # Autosave Function
  perform_auto_save <- function() {
    new_count <- save_counter() + 1
    if (new_count >= save_rate) {
      write.csv(validations(), file_path, row.names = FALSE)
      showNotification("Auto-save complete", type = "message")
      save_counter(0)
    } else {
      save_counter(new_count)
    }
  }
   
  # ----------------------------------------------------------------------------
  # Selection ------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Select a species and whether or not to display all rows 
  selected_species <- reactive({
    
    # Filter to a species
    data <- validations() %>% filter(Species == input$species)
    
    # Check if the user wants to filter out validated rows
    if (input$unval_only == "Unvalidated Vocalizations") {
      data <- data %>% filter(is.na(True.Positive))
    }
    # Return the selection
    return(data)
  })
  
  # Update the number of vocalizations whenever species OR filter changes
  observeEvent(c(input$species, input$unval_only), {
    
    # Get the IDs from the filtered reactive we just made
    new_voc_list <- selected_species() %>% pull(Voc.ID)
    
    # Update the hidden selector
    updateSelectInput(session = session,
                      inputId = "voc",
                      choices = new_voc_list,
                      # Select the first available unvalidated ID automatically
                      selected = head(new_voc_list, 1))
    
  }) # End vocalization list
  
  # Update the total number of vocalizations
  total_voc <- reactive({
    validations() %>% filter(Species == input$species) %>% pull(Voc.ID) %>% max()
    }) # End total vocalization reactivity
  
  # Select the vocalization
  selected_vocalization <- reactive({
    selected_species() %>% filter(Voc.ID == input$voc)
  }) # End vocalization selection
  
  # Update the ID of the vocalization
  voc_id <- reactive({
    selected_vocalization() %>% pull(Voc.ID)
  })# End vocalization ID reactivity
  
  # ----------------------------------------------------------------------------
  # Display --------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Spectrogram plot render
  output$spectro_plot <- renderPlot({
    
    # Ensure species is selected
    req(input$species) 
    
    # Define the .wav file path 
    wav_file <- selected_vocalization() %>% pull(File)
    
    # Check if file exists to avoid crash
    if(!file.exists(wav_file)) return(NULL)
  
    # Read in the .wav if it exisits 
    wav <- readWave(wav_file)
    
    # Define the confidence score
    conf <- selected_vocalization() %>% pull(Confidence)

    # Create the spectrogram
    spectro(wav = wav,
            f = wav@samp.rate,
            scale = FALSE,
            dB = "max0",
            palette = spectro.colors,
            main = paste("Confidence:", conf, "| Vocalization:", voc_id(), "out of", total_voc()),
            cexlab = 1.3,
            cexaxis = 1.3
    ) # End spectrogram creation 
    
  }) # End spectrogram plot render
  
  # Pie chart of proportion validated 
  output$progress_plot <- renderPlot({
    
    # Ensure species is selected
    req(input$species) 
    
    # Change the species data to have descriptive titles based on validation progress
    progress_dat <- validations() %>% 
      filter(Species == input$species) %>%
      mutate(True.Positive = case_when(is.na(True.Positive) ~ "Not Validated",
                                       True.Positive == 1 ~ "True Positive",
                                       True.Positive == 0 ~ "False Positive")) %>% 
      mutate(True.Positive = factor(True.Positive, 
                                    levels = c("Not Validated", 
                                               "True Positive", 
                                               "False Positive")))
    
    # Plot 
    progress_dat %>% 
      ggplot(aes(x = "", fill = True.Positive, col = True.Positive)) +
      geom_bar(width = 1) +
      scale_fill_manual(values = pie_pal) +
      scale_color_manual(values = pie_pal) +
      coord_polar("y", start = 0) +
      labs(title = "            Validation Progress") +
      guides(fill = guide_legend(ncol = 1, nrow = 3)) +
      theme_void() +
      theme(plot.title = element_text(size = 18),
            legend.text = element_text(size = 14),
            legend.position = "bottom",
            legend.title = element_blank()) 
    
  }) # End pie chart render 
  
    # Audio render from the play button or changing vocalizations
  observeEvent(c(input$play, input$voc, input$species), {
    
    # Define and check file
    wav_file <- selected_vocalization() %>% pull(File)
    if(!file.exists(wav_file)) return(NULL)
    
    # Read and Save
    wav <- readWave(wav_file)
    writeWave(wav, "www/temp_audio.wav")
    
    # Use a cache-buster to track updates to the file
    timestamp <- as.numeric(Sys.time())
    audio_src <- paste0("temp_audio.wav?v=", timestamp)
    
    # Remove the previous player before adding a new one
    removeUI(selector = "audio#vocal_player")
    
    # 5. Insert the new player
    insertUI(
      selector = "#audio_container",
      where = "beforeEnd",
      ui = tags$audio(
        id = "vocal_player", 
        src = audio_src,
        type = "audio/wav",
        autoplay = TRUE,      
        style = "display:none;" 
      )) # End Audio player UI
    
  }, ignoreInit = TRUE) # End audio render 
  
  # ----------------------------------------------------------------------------
  # validation -----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Name of the species 
  output$species_name <- renderText(paste0("Was this sound made by: ", input$species, "?"))
  
  # True positive 
  observeEvent(input$true_pos, {
    
    # Get the current state of the data 
    current_validation <- validations()
    
    # Add a true positive 
    current_validation <- current_validation %>% 
      mutate(True.Positive = case_when(Species == input$species & Voc.ID == as.numeric(input$voc) ~ 1,
                                       TRUE ~ as.numeric(True.Positive)))
    
    # Update the data 
    validations(current_validation)
    
    # Trigger autosave 
    perform_auto_save()
    
    # Define the next vocalization selection
    next_voc <- as.numeric(input$voc) + 1
    
    # Advance to the next vocalization
    if(next_voc <= total_voc()){
      updateSelectInput(session,
                        inputId = "voc",
                        selected = next_voc)
      
    }
      
  }) # End true positive button
  
  # False Positive
  observeEvent(input$false_pos, {
    
    # Get the current state of the data 
    current_validation <- validations()
    
    # Add a false positive 
    current_validation <- current_validation %>% 
      mutate(True.Positive = case_when(Species == input$species & Voc.ID == as.numeric(input$voc) ~ 0,
                                       TRUE ~ as.numeric(True.Positive)))
    
    # Update the data 
    validations(current_validation)
    
    # Trigger autosave 
    perform_auto_save()
    
    # Define the next vocalization selection
    next_voc <- as.numeric(input$voc) + 1
    
    # Advance to the next vocalization
    if(next_voc <= total_voc()){
      updateSelectInput(session,
                        inputId = "voc",
                        selected = next_voc)
    } 
    
    # Update the save counter 
    new_count <- save_counter() + 1
    
    # Check if autosave is needed
    if (new_count >= save_rate) {
      
      # Overwrite the local CSV with the current reactive data
      write.csv(validations(), file_path, row.names = FALSE)
      
      # Notify the user so they know a save happened
      showNotification("Auto-save complete", type = "message")
      
      # Reset counter
      save_counter(0)
    } else {
      save_counter(new_count)
    }
    
  }) # End true positive but
  
  # ----------------------------------------------------------------------------
  # Navigation -----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Previous
  observeEvent(input$prev_voc, {
    
    # Define the previous vocalization selection
    previous_voc <- as.numeric(input$voc) - 1
 
    # Make sure that it can't skip lower than 1 
    if(previous_voc >= 1){
      updateSelectInput(session,
                        inputId = "voc",
                        selected = previous_voc)
    } 
  }) # End previous button
  
  # Next 
  observeEvent(input$next_voc, {
    
    # Define the next vocalization selection
    next_voc <- as.numeric(input$voc) + 1
    
    # Make sure that it can't go higher than the maximum number of vocalizations 
    if(next_voc <= total_voc()){
      updateSelectInput(session,
                        inputId = "voc",
                        selected = next_voc)
    } 
  }) # End next button
  
  # ----------------------------------------------------------------------------
  # Download -------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  # Manual Download Button
  observeEvent(input$download, {
      
      # Save the .csv
      write.csv(validations(), file_path, row.names = FALSE)
      # Notify user of the save
      showNotification("Progress Saved", type = "message")
      
    }
  )# End manual download 
  
} # End server 

# 5) Run the app ###############################################################
shinyApp(ui, server)

