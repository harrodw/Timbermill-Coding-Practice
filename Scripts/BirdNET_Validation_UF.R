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


# Read in the recodings to validate
class_to_valid <- read.csv(paste0("Data\\uf_audio_2025_common_birds_validated.csv")) %>% select(-X)
# View
glimpse(class_to_valid)
slice_sample(class_to_valid, n = 30)

# List of species
species_list <- unique(class_to_valid$Species)

# View the species
species_list

# Define a species
bird <- species_list[4]
bird

# Convert to a file name
species_file <- str_remove(bird, "'")
species_file <- str_replace(species_file, " ", "_")
species_file <- str_replace(species_file, "-", "_")

# Filter to a single species
class_to_valid_sp <- class_to_valid %>%  
  filter(Species == bird) 

# View
glimpse(class_to_valid_sp)
ggplot(class_to_valid_sp)+geom_histogram(aes(x = Confidence)) + xlim(min_conf, 1)

# Find the first row for that species 
min_row <- min(class_to_valid_sp$rowid)
min_row

# List of audio files to validate for that species
valid_files <- class_to_valid_sp$File

################################################################################
# 2) Validate classifications ##################################################
################################################################################

##### Define which classification to validate ##################################################
voc_id <- 16

# list the confidence score for that file
conf_score <- round(class_to_valid$Confidence[min_row - 1 + voc_id], 2)

# Define a recording to validate 
validate_audio <- readWave(valid_files[voc_id])

# Get the sampling rate 
sample_rate <- validate_audio@samp.rate

##### Plot the spectogram ######################################################
spectro(wave = validate_audio, f = sample_rate, scale = FALSE, dB = "max0",
        palette = spectro.colors,
        main = paste0(bird, " Spectogram (Confidence: ", conf_score, ")"))

##### Play the sound ###########################################################
play(validate_audio)

#### STOP!!! LISTEN!!!!! #######################################################

# Clear plotting window
graphics.off()

#### Assign either a true (1) for a false (0) to that classification ###########
#### Optional 2 for unsure and need to review later ############################
class_to_valid$True.Positive[min_row - 1 + voc_id] <- 0
# View 
class_to_valid %>% select(-File) %>%  filter(True.Positive == 0)

# Save the validations as an updated csv
write.csv(class_to_valid, "uf_audio_2025_common_birds_validated.csv")
##### Repeat ###################################################################
