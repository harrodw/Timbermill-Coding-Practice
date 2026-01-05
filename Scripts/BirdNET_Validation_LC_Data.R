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
library(nimble)
library(tuneR)
library(seewave)

# Add data 
birds <- read.csv("Data\\bear_river_birdnet_detections_june_2023.csv") %>% 
  select(common_name, confidence, ARU.ID, Date, start.min, start.sec) %>% 
  rename(Species = "common_name") %>% 
  mutate(timestamp = start.min * 60 + start.sec)

# View data 
slice_head(birds, n = 30)
glimpse(birds)

# How many observations of each species 
birds %>% 
  count(Species) %>% 
  arrange(-n)

# View a specific species 
birds %>% 
  filter(Species == "Red Crossbill") %>% 
  arrange(-confidence)

# Select the top species 
top_birds_list <- birds %>% 
  count(Species) %>% 
  arrange(-n) %>% 
  slice_head(n = 9) %>% 
  pull(Species)

# View top species 
top_birds_list

# Extract the top species detections from the list 
common_birds <- birds %>% 
  filter(Species %in% top_birds_list)

# View how many detections of those birds 
slice_head(common_birds, n = 30)
glimpse(common_birds)

# Define how many birds I want to validate
n_valid <- 200

##### Select random rows to validate (comment out) ############################
birds_to_validate <- common_birds %>%
  group_by(Species) %>%
  slice_sample(n = n_valid, replace = FALSE) %>%
  # Arrange in a useful order
  ungroup() %>% 
  arrange(Species, ARU.ID, Date, start.min, start.sec) %>%
  # Add a column for whether or not each row is a true positive
  mutate(True.Positive = NA) %>%
  mutate(Date = str_remove_all(string =  as.character(Date), pattern = "-")) 

# View
glimpse(birds_to_validate)

# Make validation checklists for each species (comment out)
# for(s in 1:length(top_birds_list)){
#   # Define a species
#   species <- top_birds_list[s]
#   # Remove underscores and dashes
#   species_file <- str_replace_all(species, " ", "_")
#   species_file <- str_replace_all(species_file, "_", "_")
#   species_file <- str_replace_all(species_file, "'", "")
#   # Pull out only that species
#   species_to_validate <- birds_to_validate %>% filter(Species == species)
#   # Save as a csv
#   write.csv(species_to_validate, paste0("Data\\bear_river_birdnet_detections_june_2023_", species_file, "_validated.csv"))
# }

################################################################################
# 2) Validate classifications ##################################################
################################################################################

# Define a species 
species <- "Warbling Vireo"
species_file <- str_replace_all(species, " ", "_") 
species_file <- str_replace_all(species_file, "_", "_") 
species_file <- str_replace_all(species_file, "'", "") 

# Read in the previously validated classifications for that species to see where I should start
birds_to_validate <- read.csv(paste0("Data\\bear_river_birdnet_detections_june_2023_", species_file, "_validated.csv")) %>% 
  select(-X) 
# View 
glimpse(birds_to_validate)

# See where I left off
birds_to_validate %>% 
  filter(!is.na(True.Positive)) %>% 
  arrange(-rowid) %>% 
  slice_head(n = 1) %>% 
  pull(rowid) %>% 
  print()

##### Define which classification to validate ##################################################
bird_id <- 200

# pull out some information from that bird 
aru <- birds_to_validate$ARU.ID[bird_id]
date <- birds_to_validate %>% filter(rowid == bird_id) %>% pull(Date)
timestamp_start <- birds_to_validate$timestamp[bird_id]
timestamp_end <- timestamp_start + 3
conf_score <- signif(birds_to_validate$confidence[bird_id], 2)
# View 
c(species, conf_score, aru, date, timestamp_start, timestamp_end)

# Optional audio timing buffers if the sound is not clear
buff_before <- 0
buff_after <- 0

# Define the path to the wav file of interest 
path <- paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Audio_Data\\", aru)

# Define that file
file <- list.files(path = path, pattern = paste0(aru, "_", date, "_", "\\d{6}(\\.wav|\\..*)?$"))

# Read in the wav
wav <- readWave(paste0(path, "\\", file))

# Extract the 3-second classification snippet from the wav
wav_snip <- extractWave(object = wav, from = timestamp_start - buff_before, to = timestamp_end + buff_after, xunit = "time")

# Get the sampling rate 
sample_rate <- wav_snip@samp.rate

##### Plot the spectogram ######################################################
spectro(wave = wav_snip, f = sample_rate, scale = FALSE, dB = "max0",
        main = paste0(species, "Spectogram. Confidence: ", conf_score, ". ARU: ", aru, ". Date ", ymd(date)))

##### Play the sound ###########################################################
play(wav_snip)

#### STOP!!! LISTEN!!!!! #######################################################

# Clear plotting window
graphics.off()

#### Assign either a true (1) for a false (0) to that classification ###########
birds_to_validate$True.Positive[bird_id] <- 1
# View 
birds_to_validate %>% filter(!is.na(True.Positive))

# Save the validations as an updated csv
write.csv(birds_to_validate, paste0("Data\\bear_river_birdnet_detections_june_2023_", species_file, "_validated.csv"))
##### Repeat ###################################################################
