################################################################################
# Title: Practicing BirdNET Classification
# Author: Will Harrod
# Date Created: 2025-10-15
################################################################################

# 1) Preparation ###############################################################

# 1.1) Add packages and data ---------------------------------------------------

# Clear environments
rm(list = ls())

# Add packages
library(tidyverse)
library(birdnetR)

# Path to the folder with all audio data 
aru_folder <- "C:\\Users\\willh\\Documents\\NCSU\\Data\\Audio\\Umstead_Farm_Data"

# View all ARU's in that folder 
aru_file_list_raw <- sort(list.dirs(aru_folder))
# Remove the parent folder
aru_file_list <- aru_file_list_raw[2:length(aru_file_list_raw)]
# View
aru_file_list

# Number of ARU's
nARUs <- length(aru_file_list)

# Object to store the classifications
classif <- tibble()

# Add in the ARU metadata 
aru_info_raw <- read.csv("C:\\Users\\willh\\Documents\\NCSU\\Data\\Audio\\Umstead_Farm_Downloads\\umstead_station_sites.csv")
# View
glimpse(aru_info_raw)

# View the names 
count(aru_info_raw, Id, Name)

# Clean the ARU metadata
aru_info <- aru_info_raw %>% 
  # Pull out the nessesary imformation
  select(Id, Name, Latitude, Longitude, Timezone) %>% 
  # arrange by aru ID
  arrange(Id) %>% 
  # Convert ID to character
  mutate(Id = as.character(Id))

# 1.2) BirdNET parameters -------------------------------------------------------

# Initialize the TensorFlow Lite model
model <- birdnetR::birdnet_model_tflite()

# Set a confidence threshhold for occupancy (for species list)
min_occ <- 0.03

# Set a minimum confidence level for the classifier
min_conf <- 0.05

# load the meta model
meta_model <- birdnet_model_meta("v2.4")

# 2) Run the BirdNET classifier #######################################

# 2.1) Start loop over ARU's  --------------------------------------------------

# Start time
start_time <- Sys.time()

# Literate over each ARU ----
for(i in 1:nARUs){
 
# Path to a single aru
audio_path <- aru_file_list[i]

# Extract the ID of that ARU
aru_id <- str_extract(audio_path, "\\d{5}")

# name of that ARu
aru_name <- aru_info %>% filter(Id == aru_id) %>% pull(Name)

# List of audio files taken by that ARU
recordings <- tibble(File.Name =list.files(audio_path)) %>% 
  # Only look at recordings from 2025 for now
  # filter(str_detect(File.Name, "2025\\d{4}_")) %>% 
  # Convert to a vector
  pull(File.Name)

# Define the coordinates for the ARU
lat <- aru_info$Latitude[i]
long <- aru_info$Longitude[i]

# 2.2) Start loop over Audio files --------------------------------------------------

# Define the number of files in that folder
nfiles <- length(recordings)

# Literate over each morning of audio from a single ARU ----
for(j in 1:nfiles){

# Path to a single audio file that I know has a bird recording 
audio_file <- recordings[j]

# Extract the date
audio_date <- audio_file %>% 
  str_extract("(\\d{8})_") %>% 
  str_remove_all("_") %>% 
  as_date()

# Extract the week
audio_week <- week(audio_date)

# 2.3) Generate a list of possible species -------------------------------------

# Predict species occurrence in Logan, Utah during the week of the audio recording
species_list_all <- predict_species_at_location_and_time(meta_model, 
                                                     latitude = lat, 
                                                     longitude = long, 
                                                      week = audio_week)

# Filter species with low occupancy probabilities
species_list <- species_list_all %>% 
  arrange(confidence) %>% 
  filter(confidence >= min_occ) 

# View species predictions
# species_list

# How many species?
# nrow(species_list)

# Run the BirdNET classifier using only out custom species list
# Using try catch to skip problematic files 
tryCatch({  
pred <- predict_species_from_audio_file(model = model, 
                                  audio_file = paste0(audio_path, "\\", audio_file), 
                                  min_confidence = min_conf,
                                  filter_species = species_list$label,
                                  keep_empty = FALSE)
  
  # Message that the model is done with one file
  message("Finished classifying birds in recording ", j, " out of ", nfiles, 
          " for ARU: ", aru_name)
  
  # View species predictions
  pred_organized <- pred %>% 
    # Make a column for time stamps in minutes 
    mutate(start.min = floor(start / 60)) %>% 
    # Make a column for time stamps in seconds affter minute
    mutate(start.sec = start - (start.min * 60)) %>% 
    # Move aroud comuns for easy viewing
    select(start.min, start.sec, common_name, confidence) %>% 
    # Add ARU ID and the Date 
    mutate(ARU.ID = aru_id, ARU.Name = aru_name, Date = audio_date)
  
  # Combine with over AUR's data 
  classif <- bind_rows(classif, pred_organized)
  
  # Save the classifications
  write.csv(classif, "Data\\uf_birdnet_classif_25.csv")
  
  # Condition if recording is problematic
}, error = function(e) {
  
  # CODE TO RUN ON ERROR
  error_message <- paste0("ERROR: Skipping file ", audio_file, 
                          " from ARU: ", aru_id,
                          ". Reason: ", conditionMessage(e))
  
  # Error message
  message(error_message)
  # Return NULL or an empty data frame so the loop can continue

}) # end tryCatch

} # End loop over files

} # End loop over ARU's

# 3) Classifier Output #########################################################

# Assign the output to a new object
classified <- classif 

# View
glimpse(classified)
print(classified, n = Inf)

# Save as a csv
write.csv(classified, "Data\\uf_birdnet_classif_25.csv")

