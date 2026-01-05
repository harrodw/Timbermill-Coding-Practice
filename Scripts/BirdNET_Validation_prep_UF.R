################################################################################
# Title: Valadating BirdNET classifications
# Author: Will Harrod
# Date Created: 2025-10-15
################################################################################

# 1) Preparation ###############################################################

# 1.1) Add packages and data ---------------------------------------------------

# Clear environments
rm(list = ls())

# Add packages
library(tidyverse)
library(warbleR)
library(seewave)
library(tuneR)
library(birdnetR)

# File directories (Change these for your device) ------------------------------

# Name of the directory that holds your validation data 
csv_dir <- "C:\\Users\\willh\\Documents\\NCSU\\Disertation_Code\\Disertation_Coding_Practice\\Data\\"

# Path to your .flac input files 
flac_dir <- "C:\\Users\\willh\\Documents\\NCSU\\Data\\Audio\\Umstead_Farm_Data\\"

# Path to save your .wav output files 
wav_dir <- "C:\\Users\\willh\\Documents\\NCSU\\Data\\Audio\\UF_Validations\\"

# Set a path to your FLAC binary file (one file per device)
flac_exe_dir <- "C:\\Utilities\\flac-1.5.0-win\\Win64"

# File names (Change these for your device) -----------------------------------

# Name of your output validation file 
output_file <- "uf_validations.csv"

# Birdnet classification csv (output from previous script) 
birdnet_file <- "uf_birdnet_classif_2025_020.csv"

# ARU metadata
metadata_file <- "umstead_station_sites.csv"

# ------------------------------------------------------------------------------

# set a minimum confidence threshold
class_conf <- 0.15
min_conf <- 0.3

# Add the classifications data
classifications <- read.csv(paste0(csv_dir, birdnet_file)) %>% 
  # Arrange by AUR ID then by date then ny time
  arrange(ARU.ID, Date, Minute, Second) %>% 
  # Convert date to a date
  mutate(Date = ymd(Date)) %>%
  # Remove low confidence scores
  filter(Confidence >= min_conf) 
  # select(-X) 
# glimpse(classifications)

# # How many species?
# classifications %>% 
#   count(Species) %>% 
#   arrange(-n)

# Define a number of species to validate
n_species <- 21

# Make a list of species to validate
species_list <- classifications %>% 
  count(Species) %>% 
  arrange(-n) %>% 
  slice_head(n = n_species) %>% 
  pull(Species)

# Filter the whole dataset to only include those species 
classifications_common <- classifications %>% 
  filter(Species %in% species_list) %>% 
  # Calculate Week of each recording
  mutate(Week = week(Date)) 
# View
# glimpse(classifications_common)

# Add in the ARU metadata 
aru_info_raw <- read.csv(paste0(flac_dir, metadata_file))
# glimpse(aru_info_raw)

# # View the names 
# count(aru_info_raw, Id, Name)

# Clean the ARU metadata
aru_info <- aru_info_raw %>% 
  # Pull out the necessary information
  select(Id, Name, Latitude, Longitude, Timezone) %>% 
  # arrange by aru ID
  arrange(Id) %>% 
  # Convert ID to character
  mutate(Id = as.character(Id))

# 1.2) Prepare data for stratified random sampling -----------------------------
# Following equation 2 from Thompson et al. (2025)
# R_i = 1 - (C_i * F_ik)
# For soecies i during week k
# I had bad results including the predicted occupancy so I'm skipping that

# Define confidence bin breaks
conf_breaks <- c(0.4, 0.5, 1)

# Define weights for each break
break_weights <- c(0.1, 0.2, 0.7)

# Number of breaks
nbreaks <- length(conf_breaks)

# add a column for confidence breaks 
classifications_weighted <- classifications_common %>% 
  # Break weights
  mutate(Weight = case_when(Confidence <= conf_breaks[1] ~ break_weights[1],
                            Confidence > conf_breaks[1] & Confidence <= conf_breaks[2] ~ break_weights[2],
                            Confidence > conf_breaks[2] ~ break_weights[3],
                            TRUE ~ NA))
  
# View
# count(classifications_weighted, Weight)

# Define how many birds I want to validate from each confidence class per species per ARU
n_wav <- 175
n_valid <- 100

# # Histograms of the confodence scores
# classifications_weighted %>% 
#   group_by(Species) %>% 
#   slice_sample(n = n_wav, replace = FALSE, weight_by = Weight
#                ) %>%
#   ungroup() %>% 
#   ggplot(aes(x = Confidence)) +
#   geom_histogram(fill = "skyblue3") +
#   labs(
#     x = "Minimumn Subtracted Confidence Score",
#     y = "Number of Classifications",
#     main = ""
#   ) +
#   theme_classic() + 
#   facet_wrap(~Species)

################################################################################################
# 2) Select random stratified rows to validate from across difference confidence levels
# (comment out) ############################
################################################################################################

# Create directeries for each species (Comment out)
for(s in 1:length(species_list)){

  # Define a species
  species_file <- species_list[s]

  # Transform the species name into a file name
  species_file <- str_remove(species_file, "'")
  species_file <- str_replace(species_file, " ", "_")
  species_file <- str_replace(species_file, "-", "_")

  dir.create(path = paste0(wav_dir, species_file), recursive = TRUE)
}

# Define the rows to validate
class_to_valid <- classifications_weighted %>%
  # Arrange in a useful order
  arrange(ARU.ID, Date, Hour, Minute, Second) %>%
  # Add a column for whether or not each row is a true positive
  mutate(True.Positive = NA) %>%
  # Prepare date and time for the file reader
  mutate(Date = str_remove_all(string =  as.character(Date), pattern = "-")) %>%
  # Group  by sspecies and site
  group_by(Species) %>%
  # Select random classifications to validate
  slice_sample(n = n_wav, replace = FALSE, weight_by = Weight) %>%
  ungroup() %>% 
  select(-Weight)

# # Number of validations per species and ARU
# class_to_valid %>%
#   count(ARU.ID, Species) %>%
#   arrange(Species) %>%
#   print(n = Inf)
# 
# # Number of validations by species (should all be 100)
# class_to_valid %>%
#   count(Species) %>%
#   print(n = Inf)
# 
# # Total number to validate
# nrow(class_to_valid)
# 
# # Number of sites with at least one validations by species
# class_to_valid %>%
#   count(ARU.ID, Species) %>%
#   count(Species) %>%
#   print(n = Inf)
# 
# #View
# glimpse(class_to_valid)

# Loop over the files I need to validate and convert them to wav
for(i in 1:nrow(class_to_valid)){

  # pull out some information from the recording
  aru_id <- class_to_valid$ARU.ID[i]
  aru_file <- class_to_valid$File[i]
  start_sec_tmp <- class_to_valid$Second[i]
  start_sec <- ifelse(start_sec_tmp == 0, 0, start_sec_tmp - 1)
  end_sec_tmp <- start_sec + 5
  end_sec <- ifelse(end_sec_tmp >= 60, 60, end_sec_tmp)
  species_file <- class_to_valid$Species[i]

  # Transform the species name into a file name
  species_file <- str_remove(species_file, "'")
  species_file <- str_replace(species_file, " ", "_")
  species_file <- str_replace(species_file, "-", "_")

  # path to a flac file
  flac_audio_path <- paste0(flac_dir, aru_id, "\\", aru_file)

  # Convert from flac to wave
  wav2flac(
    file = flac_audio_path,
    reverse = TRUE,          # flac to wav
    overwrite = FALSE,       # duplicate the file
    exename = "flac.exe",    # name of the flac file
    path2exe = flac_exe_dir  # path to the flac file
  )

  # path to the new wav
  wav_path <- str_replace_all(flac_audio_path, ".flac", ".wav")

  # Name of the new wav file
  wav_name <- paste0(str_extract(wav_path, "\\d{8}_\\d{6}"), ".wav")

  # Read in the new
  wav <- readWave(wav_path,
                  from = start_sec,
                  to = end_sec,
                  units = "seconds")

  # Output destination for the new wav
  new_wav_path <- paste0(wav_dir, species_file, "\\", wav_name)

  # Save the new wav
  writeWave(wav, new_wav_path)

  # Link the name of the new file to the data
  class_to_valid$File[i] <- new_wav_path

}

# Downsample back to 100
class_to_valid %>%
  group_by(Species) %>%
  slice_sample(n = n_valid) %>%
  arrange(Species) %>%
  rowid_to_column() %>%
  ungroup() %>%
  # Save as a csv
  write.csv(paste0(csv_dir, output_file), 
            row.names = FALSE)

