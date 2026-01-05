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
aru_folder <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Audio_Data"

# View all SMM ARU-specific folder 
aru_list <- list.files(aru_folder, pattern = "SMM_\\d{2}")
aru_list

# Number of ARU's
nARUs <- length(aru_list)

# Object to store the classifications
dat <- tibble()

# 1.2) BirdNET parameters -------------------------------------------------------

# Initialize the TensorFlow Lite model
model <- birdnetR::birdnet_model_tflite()

# Set a confidence threshhold for occupancy (for species list)
min_occ <- 0.03

# Set a minimum confidence level for the classifier
min_conf <- 0.3

# load the meta model
meta_model <- birdnet_model_meta("v2.4")

# 2) Test the BirdNET classifier #######################################

# 2.1) Start loop over ARU's  --------------------------------------------------

# Literate over each ARU ----
for(i in 1:nARUs){

# Define a single ARU
aru_id <- aru_list[i]

# Path to raw audio data
audio_folder <- paste0(aru_folder, "\\", aru_id)

# Generate a list of all .wav files in that folder
audio_files <- list.files(audio_folder, pattern = "SMM")

# 2.2) Start loop over Audio files --------------------------------------------------

# Define the number of files in that folder
nfiles <- length(audio_files)

# Literate over each morning of audio from a single ARU ----
for(j in 1:nfiles){

# Path to a single audio file that I know has a bird recording 
audio_file <- audio_files[j]

# Extract ARU ID
aru_id <- str_extract(audio_file, "SMM\\d{2}")
aru_id

# Extract the date
audio_date <- audio_file %>% 
  str_extract("_(\\d{8})_") %>% 
  str_remove_all("_") %>% 
  as_date()

# Extract the week
audio_week <- week(audio_date)

# Define the coordinates for the ARU
# Note: These are the coordinates for temple fork but I could change the to be a vector for all of the ARU's
lat <- 41.82
long <- -111.65

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
species_list

# How many species?
nrow(species_list)

# Run the model using only out custom species list
pred <- predict_species_from_audio_file(model = model, 
                                        audio_file = paste0(audio_folder, "\\", audio_file), 
                                        min_confidence = min_conf,
                                        filter_species = species_list$label,
                                        keep_empty = FALSE)

# View species predictions
pred_organized <- pred %>% 
  # Make a column for time stamps in minutes 
  mutate(start.min = floor(start / 60)) %>% 
  # Make a column for time stamps in seconds affter minute
  mutate(start.sec = start - (start.min * 60)) %>% 
  # Move aroud comuns for easy viewing
  select(start.min, start.sec, common_name, confidence) %>% 
  # Add ARU ID and the Date 
  mutate(ARU.ID = aru_id, Date = audio_date)

# Combine with over AUR's data 
dat <- bind_rows(dat, pred_organized)

# Message that the model is done with one file
message(paste0("Finished classifying birds in recording ", j, " out of ", nfiles, " for ARU: ", aru_id))

} # End loop over files

# Message that the model is done with one ARU
# message(paste0("Finished classifying all birds recorded by ARU ", i, " out of ", nARUs))

} # End loop over ARU's

# 3) Classifier Output #########################################################

# Assign the output to a new object
class <- dat

# View
glimpse(class)
print(class, n = Inf)

# Save as a csv
write.csv(class, "Data\\bear_river_birdnet_detections_june_2023.csv")

