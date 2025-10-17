################################################################################
# Title: Practice cleaning ARU data 
# Authoor: Will Harrod
# Date Created: 2025-10-15
################################################################################

# 1) Preparation ###############################################################

# 1.1) Add packages and data ---------------------------------------------------

# Clear environments
rm(list = ls())

# Add packages
library(tidyverse)
library(tuneR)

# Path to the folder with all audio data 
aru_folder <- "C:\\Users\\willh\\Box\\raw ARU data\\2023 WA backup data"

# View all SMM ARU-specific folder 
aru_list <- list.files(aru_folder, pattern = "SMM_\\d{2}")
aru_list

# Extract ARU ID's
aru_file_ids <- str_extract(aru_list, "SMM_\\d{2}")
aru_file_ids

# Number of ARU's
nARUs <- length(aru_list)

# Create folders for mono files
paste0("C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Audio_Data", "\\", aru_file_ids) %>% 
  walk(~ dir.create(., recursive = TRUE, showWarnings = FALSE))

# Start loop over all ARU's ----
for(i in 1:nARUs){
  # Define a single ARU
  aru <- aru_list[i]
  aru_file <- aru_file_ids[i]
  
  # Path to raw audio data
  wav_folder <- paste0(aru_folder, "\\", aru)
  
  # Generate a list of all .wav files in that folder
  wav_files_all <- list.files(wav_folder, pattern = "SMM")
  
  # 1.2) Convert wav files to mono -----------------------------------------------
  
  # Select only one hour worth of recordings 
  wav_files <- tibble(file = wav_files_all) %>% 
    # Filter for recordings from June
    filter(str_detect(file, "_202306")) %>%
    # Filter recordings from GMT05 (8am in Utah)
    filter(str_detect(file, "_05.*")) %>%
    mutate(file.size = NA) %>% 
    # convert back to a vector
    pull(file)
  wav_files
  
  # Number of files 
  nfiles <- length(wav_files)
  
  # Path to a new folder for mono files
  mono_folders_all <- "C:\\Users\\willh\\Box\\Will_Harrod_MS_Project\\Data\\Audio_Data"
  
  # Convert each stereo file to mono
  for(j in 1:nfiles){
    # Define a single .wav file
    wav_file <- wav_files[j]
    # Extract the file size
    file_size <- file.info(paste0(wav_folder, "\\", wav_file)) %>% pull(size)
    # Skip that file if it is empty 
    if(file_size == 0 | is.na(file_size)) {
      message("skipping empty file ", wav_file)
      next
    }
    # Extract that file
    audio <- readWave(paste0(wav_folder, "\\", wav_file))
    # Extract the first channel
    mono <- channel(audio, "left")
    # Write it as a mono file into a new folder
    writeWave(object = mono, 
              filename = paste0(mono_folders_all, "\\", aru_file, "\\", wav_file))
    # Message at the end of each iteration
    message(paste0("convered file ", wav_file, " from wav to mono. (File ", j, " out of ", nfiles, " for ARU: ", aru_file, ")"))
  } # end loop over all audio files within an aru
  message(paste0("Converted all files for ARU ", aru_file))
  
} # end loop over all ARU's
