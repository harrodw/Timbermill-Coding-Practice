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

# File directories (Change these for your device) ------------------------------

# Directory for the folder with all audio data 
flac_dir <- "C:\\Users\\willh\\Documents\\NCSU\\Data\\Audio\\Umstead_Farm_Data"

# File names (Change these for your device)-------------------------------------

# ARU metadata
metadata_file <- "umstead_station_sites.csv"

# heading for BirdNet output
output_heading <- "uf_birdnet_classif_2024_"

# ------------------------------------------------------------------------------

# Other settings to modify -----------------------------------------------------

# Set a confidence threshold for occupancy (for species list)
# min_occ <- 0.03

# Set a minimum confidence level for the classifier
min_conf <- 0.2

# ------------------------------------------------------------------------------

# View all ARU's in that folder 
aru_file_list_raw <- sort(list.dirs(flac_dir))
# Remove the parent folder
aru_file_list <- aru_file_list_raw[2:length(aru_file_list_raw)]
# View
aru_file_list

# Number of ARU's
nARUs <- length(aru_file_list)

# Object to store the classifications
classif <- tibble()

# Object to store a tally of recording interval
rec_ints <- tibble()

# Add in the ARU metadata 
aru_info_raw <- read.csv(paste0(flac_dir, metadata_file))
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

# load the meta model
meta_model <- birdnet_model_meta("v2.4")

# Define the coordinates for the ARU
lat <- mean(aru_info$Latitude)
long <- mean(aru_info$Longitude)

# Extract the week
audio_week <- week("2025-05-25")

# # Predict species occurrence during the average week of the audio recording
species_list_tmp <- predict_species_at_location_and_time(meta_model,
                                                         latitude = lat,
                                                         longitude = long,
                                                         week = audio_week) %>%
  arrange(confidence) 
  # Filter species with low occupancy probabilities
  # filter(confidence >= min_occ)

# View
species_list_tmp
species_list_tmp$label

# Make a modified list of forest birds 
possible_birds <- c("Limnothlypis swainsonii_Swainson's Warbler",
                    "Setophaga magnolia_Magnolia Warbler",                     
                    "Setophaga caerulescens_Black-throated Blue Warbler",      
                    "Zonotrichia albicollis_White-throated Sparrow",           
                    "Geothlypis formosa_Kentucky Warbler",                     
                    "Setophaga virens_Black-throated Green Warbler",           
                    "Catharus guttatus_Hermit Thrush",                         
                    "Accipiter cooperii_Cooper's Hawk",                        
                    "Antrostomus carolinensis_Chuck-will's-widow",             
                    "Larus delawarensis_Ring-billed Gull",                     
                    "Pheucticus ludovicianus_Rose-breasted Grosbeak",          
                    "Vireo solitarius_Blue-headed Vireo",                      
                    "Haliaeetus leucocephalus_Bald Eagle",                     
                    "Petrochelidon pyrrhonota_Cliff Swallow",                  
                    "Strix varia_Barred Owl",                                  
                    "Helmitheros vermivorum_Worm-eating Warbler",              
                    "Meleagris gallopavo_Wild Turkey",                         
                    "Icterus galbula_Baltimore Oriole",                        
                    "Quiscalus major_Boat-tailed Grackle",                     
                    "Mniotilta varia_Black-and-white Warbler",                 
                    "Parkesia motacilla_Louisiana Waterthrush",                
                    "Dryobates villosus_Hairy Woodpecker",                     
                    "Setophaga petechia_Yellow Warbler",                       
                    "Colinus virginianus_Northern Bobwhite",                   
                    "Melanerpes erythrocephalus_Red-headed Woodpecker",        
                    "Setophaga ruticilla_American Redstart",                   
                    "Columba livia_Rock Pigeon",                               
                    "Sturnella magna_Eastern Meadowlark",                      
                    "Buteo jamaicensis_Red-tailed Hawk",                       
                    "Protonotaria citrea_Prothonotary Warbler",                
                    "Vireo flavifrons_Yellow-throated Vireo",                  
                    "Stelgidopteryx serripennis_Northern Rough-winged Swallow",
                    "Anas platyrhynchos_Mallard",                              
                    "Colaptes auratus_Northern Flicker",                       
                    "Piranga olivacea_Scarlet Tanager",                        
                    "Setophaga discolor_Prairie Warbler",                      
                    "Icteria virens_Yellow-breasted Chat",                     
                    "Setophaga citrina_Hooded Warbler",                        
                    "Setophaga dominica_Yellow-throated Warbler",              
                    "Charadrius vociferus_Killdeer",                           
                    "Bombycilla cedrorum_Cedar Waxwing",                       
                    "Troglodytes aedon_House Wren",                            
                    "Buteo lineatus_Red-shouldered Hawk",                      
                    "Seiurus aurocapilla_Ovenbird",                            
                    "Sitta pusilla_Brown-headed Nuthatch",                     
                    "Icterus spurius_Orchard Oriole",                          
                    "Tachycineta bicolor_Tree Swallow",                        
                    "Progne subis_Purple Martin",                              
                    "Passer domesticus_House Sparrow",                         
                    "Pandion haliaetus_Osprey",                                
                    "Corvus ossifragus_Fish Crow",                             
                    "Setophaga americana_Northern Parula",                     
                    "Empidonax virescens_Acadian Flycatcher",                  
                    "Piranga rubra_Summer Tanager",                            
                    "Passerina caerulea_Blue Grosbeak",                        
                    "Branta canadensis_Canada Goose",                          
                    "Spizella pusilla_Field Sparrow",                          
                    "Sayornis phoebe_Eastern Phoebe",                          
                    "Coccyzus americanus_Yellow-billed Cuckoo",                
                    "Sitta carolinensis_White-breasted Nuthatch",              
                    "Dryocopus pileatus_Pileated Woodpecker",                  
                    "Hylocichla mustelina_Wood Thrush",                        
                    "Ardea herodias_Great Blue Heron",                         
                    "Tyrannus tyrannus_Eastern Kingbird",                      
                    "Setophaga pinus_Pine Warbler",                            
                    "Contopus virens_Eastern Wood-Pewee",                      
                    "Archilochus colubris_Ruby-throated Hummingbird",          
                    "Chaetura pelagica_Chimney Swift",                         
                    "Geothlypis trichas_Common Yellowthroat",                  
                    "Vireo griseus_White-eyed Vireo",                          
                    "Dumetella carolinensis_Gray Catbird",                     
                    "Melospiza melodia_Song Sparrow",                          
                    "Toxostoma rufum_Brown Thrasher",                          
                    "Haemorhous mexicanus_House Finch",                        
                    "Dryobates pubescens_Downy Woodpecker",                    
                    "Vireo olivaceus_Red-eyed Vireo",                          
                    "Sturnus vulgaris_European Starling",                      
                    "Polioptila caerulea_Blue-gray Gnatcatcher",               
                    "Myiarchus crinitus_Great Crested Flycatcher",             
                    "Molothrus ater_Brown-headed Cowbird",                     
                    "Hirundo rustica_Barn Swallow",                            
                    "Agelaius phoeniceus_Red-winged Blackbird",                
                    "Quiscalus quiscula_Common Grackle",                       
                    "Passerina cyanea_Indigo Bunting",                         
                    "Pipilo erythrophthalmus_Eastern Towhee",                  
                    "Sialia sialis_Eastern Bluebird",                          
                    "Spizella passerina_Chipping Sparrow",                     
                    "Spinus tristis_American Goldfinch",                       
                    "Melanerpes carolinus_Red-bellied Woodpecker",             
                    "Turdus migratorius_American Robin",                       
                    "Mimus polyglottos_Northern Mockingbird",                  
                    "Cyanocitta cristata_Blue Jay",                            
                    "Corvus brachyrhynchos_American Crow",                     
                    "Baeolophus bicolor_Tufted Titmouse",                      
                    "Poecile carolinensis_Carolina Chickadee",                 
                     "Thryothorus ludovicianus_Carolina Wren",                  
                    "Zenaida macroura_Mourning Dove",                          
                    "Cardinalis cardinalis_Northern Cardinal")

# Filter the species
species_list <- species_list_tmp %>% filter(label %in% possible_birds)
# View again
species_list

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
  # Only look at recordings from 2024 from May and June for now
  filter(str_detect(File.Name, "202405\\d{2}_") | str_detect(File.Name, "202406\\d{2}_")) %>%
  # Only look at three hours of reocordings 
  filter(str_detect(File.Name, "_06") | str_detect(File.Name, "_07") | str_detect(File.Name, "_08")) %>%
  # Convert to a vector
  pull(File.Name)

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

# Extract the hour
audio_hour <- audio_file %>% 
  str_extract("_\\d{6}") %>% 
  str_sub(start = 3, end = 3) %>%
  as.integer()

# Extract the minute 
audio_minute <- audio_file %>% 
  str_extract("_\\d{6}") %>%  
  str_sub(start = 5, end = 5) %>%
  as.integer()

# 2.3) Run the BirdNET classifier using only out custom species list ----------------

# Using try catch to skip problematic files 
tryCatch({  
pred <- predict_species_from_audio_file(model = model, 
                                        audio_file = paste0(audio_path, "\\", audio_file), 
                                        min_confidence = min_conf,
                                        filter_species = species_list$label,
                                          # "Setophaga citrina_Hooded Warbler",
                                        keep_empty = FALSE) 
  
  # View species predictions
  pred_organized <- pred %>% 
    # Make new columns
    mutate(Hour = audio_hour,
           Minute = audio_minute,
           Second = floor(start),
           ARU.ID = aru_id, 
           ARU.Name = aru_name,
           Species = common_name,
           Confidence = confidence,
           Date = audio_date,
           File = audio_file) %>% 
    # Move aroud comuns for easy viewing
    select(ARU.ID, ARU.Name, Date, Hour, Minute, Second, Species, Confidence, File) %>% 
    # Add ARU ID and the Date 
    mutate()
  
  # Combine with over AUR's data 
  classif <- bind_rows(classif, pred_organized)
  
  # Tally of recording interval
  rec_ints_tmp <- tibble(
    Hour = audio_hour,
    Minute = audio_minute,
    ARU.ID = aru_id, 
    ARU.Name = aru_name,
    Date = audio_date,
    File = audio_file
  )
  
  # Bind with others 
  rec_ints <- bind_rows(rec_ints, rec_ints_tmp)
  
  # Message that the model is done with one file
  message("Finished classifying birds in recording ", j, " out of ", nfiles, 
          " for ARU: ", aru_id)
  
  # Condition if recording is problematic
}, error = function(e) {
    # run ON ERROR and allows the loop to continue
    warning("Skipping file: ", audio_file, " Error: ", e$message)

}) # end tryCatch

} # End loop over files

# Save the classifications
write.csv(classif, paste0(csv_dir, 
                          output_heading, 
                          as.character(str_remove(min_conf, ".")),
                          ".csv"))

} # End loop over ARU's

# 3) Classifier Output #########################################################

# Done
message("All Recordings Classified")

# Assign the output to a new object
classified <- classif 

# View
glimpse(classified)
print(classified, n = 30)

# Save the classifications
write.csv(classified, paste0(csv_dir, 
                             output_heading, 
                             as.character(str_remove(min_conf, ".")),
                             ".csv"),
          row.names = FALSE)