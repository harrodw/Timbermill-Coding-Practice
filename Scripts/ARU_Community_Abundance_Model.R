################################################################################
# Title: Theoretical model for estimating relative abundance from ARU data 
# Author: Will Harrod
# Date Created: 2025-11-18
#
################################################################################
# Data Source: Dr. Ivana Mali
#
#
################################################################################
# This model extends code from the following sources 
#
# Doser, J. W., Finley, A. O., Weed, A. S., & Zipkin, E. F. (2021). 
#      Integrating automated acoustic vocalization data and point count surveys 
#      for estimation of bird abundance. Methods in Ecology and Evolution, 
#      12(6), 1040-1049.
#
################################################################################

# 1) Preparation ###############################################################

# 1.1) Add packages and data ---------------------------------------------------

# Clear environments
rm(list = ls())

# Add packages
library(tidyverse)
library(jagsUI)
library(MCMCvis)

# Prepare the detection data ---------------------------------------------------

# Define a species 
species_name <- "Hooded Warbler"

# # Remove underscores and dashes
species_file <- str_replace_all(species_name, " ", "_")
species_file <- str_replace_all(species_file, "_", "_")
species_file <- str_replace_all(species_file, "'", "")

# set a minimum confidence threashhold
min_conf <- 0.15

# Add the classifications data
species_dat <-  read.csv("Data\\uf_birdnet_howa_classif_25.csv") %>% 
  # Arrange by AUR ID then by date then ny time
  arrange(Date, Hour, Minute, Second, ) %>%
  # Convert date to a date
  mutate(Date = ymd(Date)) %>%
  select(-X) %>% 
  # Filter by confidence
  filter(Confidence >= min_conf) %>%
  # Add row ID's
  rowid_to_column() 
  
# View data 
glimpse(species_dat)

# Validated subset of vocalizations --------------------------------------------
# # I'm still working on actually validating these so for now I'll fake it
# 
# # Number of "validated" recordings
# n_valid <- 100
# 
# # True positive rate
# tp <- 0.8
# 
# # Number of true positives
# n_true <- floor(n_valid*tp)
# 
# # number of false positives
# n_false <- n_valid - n_true
# 
# # Simulate validated recordings 
# valid_rows <- species %>% 
#   slice_sample(n = n_valid) %>% 
#   arrange(rowid) %>% 
#   pull(rowid)
# valid_rows
# 
# # Simulate false positives
# false_pos_rows <- tibble(Row.ID = valid_rows) %>% 
#   slice_sample(n = n_false) %>% 
#   arrange(Row.ID) %>% 
#   pull(Row.ID)
# false_pos_rows
# 
# # Simulate the validations 
# validations <- species %>% 
#   # Asign several random rows to be true or false positives
#   mutate(Validated = case_when(rowid %in% valid_rows ~ 1, 
#                                !rowid %in% valid_rows ~ 0),
#          False.Pos = case_when(Validated == 1 & rowid %in% false_pos_rows ~ 1, 
#                               TRUE ~ 0),
#          ) %>% 
#   select(-rowid)
# # View 
# glimpse(validations)

# Add the validations 
validated_files <- read.csv(paste0("Data\\uf_audio_25_", species_file, "_validated.csv")) %>%
  mutate(Hour = str_extract(File, "_\\d{2}")) %>% 
  mutate(Hour = str_remove(Hour, "_")) %>% 
  mutate(Hour = as.integer(Hour),
         Date = ymd(Date)) %>% 
  select(ARU.ID, Date, Hour, Minute, Second,  True.Positive)
# View
glimpse(validated_files)

# Number of validations 
n_valid <- nrow(validated_files)
# Number of true positives
n_true <- sum(validated_files$True.Positive)
# Number of false possitives
n_false <- n_valid - n_true
# view
c(n_valid, n_true, n_false)

# Join with the species information
validations <- left_join(species_dat, validated_files, by = c("ARU.ID", "Date", "Hour", "Minute", "Second")) %>% 
  # add a column for valdiated or not
  mutate(Validated = case_when(!is.na(True.Positive) ~ 1, TRUE ~ 0)) %>% 
  mutate(True.Positive = replace_na(True.Positive, 0))
# View
glimpse(validations)


# Prepare the survey effort data -----------------------------------------------

# Add in the ARU metadata 
aru_info_tmp <- read.csv("C:\\Users\\willh\\Documents\\NCSU\\Data\\Audio\\Umstead_Farm_Downloads\\umstead_station_sites.csv")
# View
glimpse(aru_info_tmp)

# View the names 
distinct(aru_info_tmp, Id, Name)

# Clean the ARU metadata
aru_info <- aru_info_tmp %>% 
  # Pull out the nessesary imformation
  select(Id, Name, Latitude, Longitude, Timezone) %>% 
  # arrange by aru ID
  arrange(Id) %>% 
  # Convert ID to character
  mutate(Id = as.character(Id)) 

# Path to the folder with all audio data 
aru_folder <- "C:\\Users\\willh\\Documents\\NCSU\\Data\\Audio\\Umstead_Farm_Data"

# View all ARU's in that folder 
aru_file_list_raw <- sort(list.dirs(aru_folder))
aru_file_list <- aru_file_list_raw[2:length(aru_file_list_raw)]
# c(2, 3, 4, 6, 7, 8, 9, 10)

# View
aru_file_list

# Number of ARU's
nARUs <- length(aru_file_list)

# Storage object for the survey periods 
rec_ints_tmp1 <- tibble()

# Loop over the ARU data and extract the survey periods 
for(i in 1:nARUs){

  # Path to a single ARU
  audio_path <- aru_file_list[i]
  
  # Extract the ID of that ARU
  aru_id <- str_extract(audio_path, "\\d{5}")
  
  # Name of that ARU
  aru_name <- aru_info %>% filter(Id == aru_id) %>% pull(Name)

  # Calculate survey periods 
  rec_ints_tmp2 <- tibble(File.Name = list.files(audio_path)) %>% 
    # Only look at recordings from May and June 2024
    # filter(str_detect(File.Name, "202405") | str_detect(File.Name, "202406")) %>%
    # Only look at three hours of recordings 
    filter(str_detect(File.Name, "_06") | str_detect(File.Name, "_07") | str_detect(File.Name, "_08")) %>% 
    # print(n = Inf)
    # Extract when each recording took place 
    mutate(
      # ARU name 
      ARU.Name = aru_name,
      # Date
      Date = ymd(str_extract(File.Name, "2024\\d{4}")),
      # Recording Hour
      Hour = case_when(str_detect(File.Name, "_06") ~ 1,
                           str_detect(File.Name, "_07") ~ 2,
                           str_detect(File.Name, "_08") ~ 3,
                           )) %>% 
    # Select Relevant columns 
    select(ARU.Name, Date, Hour) %>%
    arrange(Date, Hour) %>% 
    distinct()
  # rec_ints_tmp2 %>%  arrange(Date, Hour) %>%  distinct(Date, Hour) %>% print(n = Inf)
  # Bind to the Hours for other ARU's
  rec_ints_tmp1 <- bind_rows(rec_ints_tmp1, rec_ints_tmp2) 
}

# Save as a new object
rec_ints <- rec_ints_tmp1 %>% 
  # Put in chronological order
  mutate(Hour = as.integer(Hour),
         # Combine date and Hour
         Recording.Interval = paste0(Date, "-H", Hour))

# View
glimpse(rec_ints)
count(rec_ints, ARU.Name)

# Minimum intervals per ARU
min_ints <- rec_ints %>% 
  count(ARU.Name) %>%
  arrange(n) %>% 
  slice_head(n = 1) %>% 
  pull(n)

# Combine all Hours with the species detentions ----------------------------
# View validations again
glimpse(validations)

# Number of vocalization by ARU and recording period
voc_count_tmp1 <- validations %>% 
  # Detections by site
  group_by(ARU.Name, Date, Hour) %>% 
  mutate(Hour = as.integer(Hour) - 5) %>% 
  reframe(ARU.Name, Date, Hour, 
          n.Vocalizations = n(), 
          n.Validated= sum(Validated), 
          n.True = sum(True.Positive),
          Confidence = mean(Confidence)
            ) %>%
  distinct()
  
# View
glimpse(voc_count_tmp1)
hist(voc_count_tmp1$Confidence)

# Fill in Hours with no detections
voc_count <- rec_ints %>% 
  # Join with Hours that had no detections
  left_join(voc_count_tmp1, by = c("ARU.Name", "Date", "Hour")) %>% 
  # Put in a good order
  arrange(Date, Hour, ARU.Name) %>% 
  # Replace NA's with 0
  mutate(n.Vocalizations = replace_na(n.Vocalizations, 0),
         n.Validated = replace_na(n.Validated, 0),
         n.True = replace_na(n.True, 0),
         Confidence = log(Confidence)
         ) %>% 
  # Define surveys with any detections
  mutate(Present = case_when(n.Vocalizations > 0 ~ 1, n.Vocalizations == 0 ~ 0)) %>%
  # Make numeric versions 
  mutate(Date.num = as.integer(as.factor(Date)),
         ARU.ID.num = as.integer(as.factor(ARU.Name))
         ) %>% 
  group_by(ARU.ID.num) %>%
  slice_head(n = min_ints) %>% 
  mutate(Recording.Interval.num = as.integer(as.factor(Recording.Interval))) %>% 
  ungroup() %>%
  # Select Relevant columns
  select(ARU.ID.num, Recording.Interval.num, Date.num, Hour, n.Vocalizations, Present, n.Validated, n.True, Confidence)
  
# View
glimpse(voc_count)
sort(voc_count$Recording.Interval.num)
sort(voc_count$Date.num)
hist(scale(voc_count$Confidence)[,1])

# View as a matrix
voc_count %>% 
  select(ARU.ID.num, Recording.Interval.num, n.Vocalizations) %>% 
  pivot_wider(names_from = Recording.Interval.num, values_from = n.Vocalizations)

# Define the number of Hours in each day
nints_daily <- length(unique(voc_count$Hour))

# 1.2) Convert data to a format jags can use ---------------------------------

# Constants --------------------------------------------------------------------

# Define R (the number of sites) and J (the maximum number of sampling Hours)
R <- max(voc_count$ARU.ID.num)
J <- max(voc_count$Recording.Interval.num)

# View
R
J

# # J ----------------------------------------------------------------------
# 
# # Define J, the number of survey periods at each site
# J <- voc_count %>% 
#   group_by(ARU.ID.num) %>% 
#   reframe(ARU.ID.num, Max.Int = length(unique(Recording.Interval.num))) %>% 
#   distinct() %>% 
#   pull(Max.Int)
# # View
# J

# v ------------------------------------------------------------------------------
# Define v, the number of positive detections by site and day, whether true or false
v <- voc_count %>% 
  select(ARU.ID.num, Recording.Interval.num, n.Vocalizations) %>% 
  arrange(ARU.ID.num, Recording.Interval.num) %>% 
  pivot_wider(names_from = Recording.Interval.num, values_from = n.Vocalizations) %>% 
  select(-ARU.ID.num) %>% 
  relocate(all_of(str_sort(everything(), numeric = TRUE))) %>% 
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0))) %>% 
  as.matrix()
# View
v
str(v)

# y ------------------------------------------------------------------------------

# Define y, a matrix of whether or not each site had any detections 
y <- voc_count %>% 
  select(ARU.ID.num, Recording.Interval.num, Present) %>% 
  arrange(Recording.Interval.num, ARU.ID.num) %>%
  pivot_wider(names_from = Recording.Interval.num, values_from = Present) %>% 
  select(-ARU.ID.num) %>%
  relocate(all_of(str_sort(everything(), numeric = TRUE))) %>% 
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0))) %>% 
  as.matrix()

# View
y
str(y)

# n ------------------------------------------------------------------------------

# Define the proportion validated 
# prop_val <- 0.5

# Define n, the number of validations for each survey window 
# n <- floor(v*prop_val)
n <- voc_count %>%
  select(ARU.ID.num, Recording.Interval.num, n.Validated) %>% 
  arrange(Recording.Interval.num, ARU.ID.num) %>%
  pivot_wider(names_from = Recording.Interval.num, values_from = n.Validated) %>% 
  select(-ARU.ID.num) %>%
  relocate(all_of(str_sort(everything(), numeric = TRUE))) %>% 
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0))) %>%
  as.matrix()
# View
n
str(n)
sum(rowSums(n))

# k ---------------------------------------------------------------------------

# Define k, the number of validated true positives by site and day
# k <- floor(n*tp_rate)
k <- voc_count %>%
  select(ARU.ID.num, Recording.Interval.num, n.True) %>% 
  arrange(Recording.Interval.num, ARU.ID.num) %>%
  pivot_wider(names_from = Recording.Interval.num, values_from = n.True) %>% 
  select(-ARU.ID.num) %>%
  relocate(all_of(str_sort(everything(), numeric = TRUE))) %>% 
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0))) %>%
  as.matrix()
# View
k 
str(k)
sum(rowSums(k))

# Compare n to k
sum(rowSums(n)) - sum(rowSums(k))

# J.a -----------------------------------------------------------------------------

# Define J.a, the number Hours with vocalizations at each site 
J.a <- voc_count %>% 
  filter(Present == 1) %>% 
  group_by(ARU.ID.num) %>% 
  reframe(n.Ints = n()) %>% 
  distinct() %>% 
  pull(n.Ints)
# View# View# View
J.a

# sites.a ------------------------------------------------------------------------
# Indexes of where acoustic data were obtained for the specific species
sites.a <- voc_count %>% 
  filter(Present == 1) %>% 
  distinct(ARU.ID.num) %>% 
  arrange(ARU.ID.num) %>% 
  pull(ARU.ID.num)
# View
sites.a

# sites.v ------------------------------------------------------------------------
# Indexes of where acoustic data were validated for the specific species
sites.v <- voc_count %>% 
  filter(n.Validated >= 1) %>% 
  distinct(ARU.ID.num) %>% 
  arrange(ARU.ID.num) %>% 
  pull(ARU.ID.num)
# View
sites.v

# J.a -----------------------------------------------------------------------------

# Define J.val, the number of Hours where vocalizations were validated at each site
J.val <- voc_count %>% 
  filter(n.Validated >= 1) %>% 
  count(ARU.ID.num) %>% 
  pull(n)

# View
J.val
sum(J.val)

# R.val ------------------------------------------------------------------------
# The number of sites where acoustic data was validated
R.val <- length(sites.v)
# View
R.val

# Check if this is the right number of sites
R.val == length(J.val)

# times -------------------------------------------------------------------------
# Construct times, an n.sites x J matrix that contains the indices of the
# specific surveys at each acoustic site that have at least one detected
# vocalization. This determines the specific components of v[i, j] that
# are used in the zero-truncated Poisson.

# Storage object for time with detections
times.a <- matrix(NA, nrow = R, ncol = max(rowSums(y)))
 
# Add the time indexing information
for (i in 1:R) {

  # Define all of periods with vocalizations for a single ARU
  ints.a <- voc_count %>% 
    filter(ARU.ID.num == i & Present == 1) %>% 
    arrange(Recording.Interval.num) %>% 
    distinct(Recording.Interval.num) %>% 
    pull(Recording.Interval.num)
  # Assign those values to the indexing variable 
  times.a[i, 1:length(ints.a)] <- ints.a
}
# View
times.a
v


# times.v --------------------------------------------------------------------
# The times when each valedation occured
# Storage object for time with detections
times.v <- matrix(NA, nrow = R.val, ncol = max(J.val))

# Add the time indexing information
for (i in 1:R.val) {

  # Define all of periods with validations for a single ARU
  ints.v <- voc_count %>% 
    filter(ARU.ID.num == i) %>%
    filter(n.Validated >= 1) %>% 
    distinct(Recording.Interval.num) %>% 
    pull(Recording.Interval.num)
  # Assign those values to the indexing variable 
  times.v[i, 1:length(ints.v)] <- ints.v
  
}
# View
times.v
J.val
n
k

# Day -------------------------------------------------------------------------

# Number of days surveyed
# n.days <- length(unique(voc_count$Date.num))

# # Vector of days
# days_vect <- voc_count %>%
#   mutate(Date.num = replace_na(Date.num, 49)) %>% 
#   distinct(Recording.Interval.num, Date.num) %>% 
#   arrange(Recording.Interval.num) %>% 
#   pull(Date.num)

# Day when each survey took place 
date <- voc_count %>% 
  mutate(Date.num = round(scale(Date.num)[,1], 2)) %>% 
  select(ARU.ID.num, Recording.Interval.num, Date.num) %>% 
  arrange(Recording.Interval.num, Date.num) %>%
  pivot_wider(names_from = Recording.Interval.num, values_from = Date.num) %>% 
  select(-ARU.ID.num) %>%
  relocate(all_of(str_sort(everything(), numeric = TRUE))) %>% 
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0))) %>%
  as.matrix()
  
  
  # matrix(rep(days_vect, R), nrow = R, ncol = J, byrow = TRUE)
 
# View
date

# Confidence level  ---------------------------------------------------------------------------

# Average confidence for all validations by site and visit
conf <- voc_count %>%
  select(ARU.ID.num, Recording.Interval.num, Confidence) %>% 
  arrange(Recording.Interval.num, ARU.ID.num) %>%
  mutate(Confidence = round(Confidence, 2)) %>% 
  pivot_wider(names_from = Recording.Interval.num, values_from = Confidence) %>% 
  select(-ARU.ID.num) %>%
  relocate(all_of(str_sort(everything(), numeric = TRUE))) %>% 
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0))) %>%
  as.matrix()
# View
conf
str(conf)

# K ----------------------------------------------------------------------------

# Define K, the latent number of true positives at each visit to each site
K.inits <- matrix(NA, nrow = R.val, ncol = max(J.val))

# Estimated true posiitve rate
tp_rate <- n_true/n_valid

# Initialize each site with the number of true positive validations that occurred there
for(i in 1:R.val){
  for(j in 1:J.val[i]){
    # Asign soething between validations and vocalizations as the initial values for K
    K.inits[i, j] <- ceiling(v[sites.v[i], times.v[i, j]] * tp_rate)
  }
}

# View
K.inits
str(K.inits)

# N ----------------------------------------------------------------------------

# Define N, the true abundance at each site
N.inits <- ceiling(apply(k, 1, max)*0.5) + 1
# View
N.inits

# ------------------------------------------------------------------------------

# 2) Run the model #############################################################

# 2.1) Model specification -----------------------------------------------------

# File path to send the outputs
mod_path <- "C:\\Users\\willh\\Documents\\NCSU\\Disertation_Code\\Bayesian_Mods\\"

# Model statement using nimble 
mod_name <- "aru_abund_mod.txt"

# Model statement
cat(file = paste0(mod_path, mod_name),
"  model{

####### Model definitions #############################
    # Indexing -------------------------------------------
    # i = sampling locations (ARU's)
    # j = 10-minute sampling Hours 
    # R = total number of sites
    # R.Val = Number of ARU sites with validations
    # J = Maximum number of Hours at any site
    # J number of sampling Hours at each site
    # J.val = Number of Hours at each site with validations
    # times.a = Times when acoustic activity occurred
    # times.v = Times when validations occurred
    # sites.a = Sites with at least one vocalization
    # sites.v = Sites with at least one validation
    # Data ------------------------------------------------
    # y = A matrix of sites by visits for whether or not each site had any detection
    # v = A matrix of sites by visits for the number of vocalizations 
    # n = Number of validations at each visit to each site
    # k = Number of validated positives at each visit to each site
    # conf = average confidence score for all validations at each visit to each site 
    # Parameters ------------------------------------------
    # omega = false positive rate 
    # Delta = Singing rate
    # phi = Overdispersion in singing rate 
    # beta0.site = Random intercept on abundance at each site
    # alpha0 = Intercept on detection probability
    # alpha.N = Effect of the number of individuals on detection probability 
    # Latent variables ------------------------------------
    # N = Latent relative abundance at each site
    # K = Latent number of true positives 
    # Q = Latent number of false positives
    # p = Probability of detecting at least one vocalization in an acoustic recording
    # tp = True positive rate  
    
    # Priors ----------------------------------------------------------------
    
    # Beta node (parameters that influence abundance) ----
    # Hyper parameter on the variation between sites 
    tau.beta0.site ~ dgamma(0.01, 0.01)
    # Random intercept on relative abundance at each site
    for(i in 1:R){
      beta0.site[i] ~ dnorm(0, tau.beta0.site) 
    }
    
    # Alpha node (parameters that influence detecting a bird) ----
    mu.alpha ~ dunif(0, 1)
    alpha0 <- log(mu.alpha / (1 - mu.alpha)) 
    alpha.N ~ dgamma(0.01, 0.01)
    
    # Gamma node (Parameters that influence vocalization rate) ----
    gamma0 ~ dnorm(0, 0.1)
    gamma.date ~ dnorm(0, 0.1)
    gamma.date2 ~ dnorm(0, 0.1)
    
    # Hyperpriors discrribing the confidence scores
    mu.nu ~ dnorm(0, 0.1)
    sd.nu ~ dgamma(0.01, 0.01)
    tau.nu <- 1/pow(sd.omega, 2)
    
    # Hyperpriors discrribing the scale between confidence scores and flase positive rate
    mu.kappa ~ dnorm(0, 0.1)
    sd.kappa ~ dgamma(0.01, 0.01)
    tau.kappa <- 1/pow(sd.omega, 2)

    
    # Loop over species  -----
    for(s in 1:){
    
    # Shape of the curve discribing classifier scores 
    log(nu[s]) ~ dnorm(mu.nu, tau.nu)
    
    # Scale parameter between slope of false positives and rate of false positives
    log(kappa[s]) ~ dnorm(mu.kappa, tau.kappa)
    
    # Rate of false positives
    omega[s] <- kappa[s] * nu[s]
    
    }
    
    # Phi node (Unmodeled overdispersion in singing rate) ----
    
    # Hyper prior for unmodeled variation in singing rate
    epsilon.phi ~ dgamma(0.01, 0.01)
    
    # Unexplained variation in singing rate
    for (i in 1:R) {
      for (j in 1:J) { 
        phi[i, j] ~ dgamma(epsilon.phi, epsilon.phi)
      } # j
    } # i
    
    # Liklihood ----------------------------------------------------------------
    
    # Individual Vocalizations --------------
    for(v in 1:n.voc){
    
       # Intercept for false positive rate for each species
       voc[v] ~ dbeta(1, 1 + nu[voc.species[v]])
    
     } # Vocalizations
    
    
    # ARU's --------------------
    for (i in 1:R) {
      
      # Linear combination expected for abundance at each point
      log(lambda.N[i]) <- beta0.site[i]
      
      # Abudnance at each point
      N[i] ~ dpois(lambda.N[i])
      
      # Linear combination for the probability of detecting at least one bird at each point 
      logit(pi[i]) <- alpha0 + alpha.N * N[i] 
      
      # Acoustic Data -------------------
      for (j in 1:J) { 
        
        # Log linear combination for the per individual rate of vocalization
        log(delta[i, j]) <- gamma0 + gamma.date * date[i, j] + gamma.date2 * pow(date[i, j], 2)
        
        # Probability of detecting at least one bird
        y[i, j] ~ dbern(pi[i])
        
        # True positive rate 
        tp[i, j] <- delta[i, j] * N[i] / (delta[i, j] * N[i] + omega)
        
        # Posterior predictive checks for Bayesian P-value for observations
        y.pred[i, j] ~ dbern(pi[i])
        resid.y[i, j] <- pow(pow(y[i, j], 0.5) - pow(pi[i], 0.5), 2)
        resid.y.pred[i, j] <- pow(pow(y.pred[i, j], 0.5) - pow(pi[i], 0.5), 2)
        
      } # j
      
      # Number of vocalizations for each visit to each site
      for (j in 1:J.a[i]) {
        
        # Log-linear combination for the number of vocalizations at site i during visit j for the zero-truncated poisson
        lambda.v[i, j] <- (delta[i, times.a[i, j]] * N[i] + omega) * phi[i, times.a[i, j]]
        
        # number of detected vocalizations for sites with at least one vocalization
        v[i, times.a[i, j]] ~ dpois(lambda.v[i, j] * y[i, times.a[i, j]])T(1, 1000)
        
        # Posterior predictive checks for Bayesian P-value for singing rate ----
        v.pred[i, j] ~ dpois(lambda.v[i, j] * y[i, times.a[i, j]])T(1, 1000)
        mu.v[i, j] <- lambda.v[i, j] / (1 - exp(-1 * lambda.v[i, j]))
        resid.v[i, j] <- pow(pow(v[i, times.a[i, j]], 0.5) - pow(mu.v[i, j], 0.5), 2)
        resid.v.pred[i, j] <- pow(pow(v.pred[i, j], 0.5) - pow(mu.v[i, j], 0.5), 2)

      } # j
    } # i
    
    # Manual validation of acoustic classifications-----------------
    for (i in 1:R.val) {
      for (j in 1:J.val[i]) {

        # Total number of true positives at site i during visit j
        K[i, j] ~ dbin(tp[sites.v[i], times.v[i, j]], v[sites.v[i], times.v[i, j]])

        # Define the number of false positives during each survey period
        Q[i, j] <- v[sites.v[i], times.v[i, j]] - K[i, j]

        # Number of manually checked vocalizations at site i and survey j
        k[sites.v[i], times.v[i, j]] ~ dhyper(K[i, j], Q[i, j], n[sites.v[i], times.v[i, j]], 1)

      } # j
    } # i
    
  # Bayesian P-value ------------------------------------------------------
   for (i in 1:R) {
    tmp.v[i] <- sum(resid.v[i, 1:J.a[i]])
    tmp.v.pred[i] <- sum(resid.v.pred[i, 1:J.a[i]])
   }

  # Check model fit
    fit.y <- sum(resid.y[sites.a, 1:J])
    fit.y.pred <- sum(resid.y.pred[sites.a, 1:J])
    fit.v <- sum(tmp.v[1:R])
    fit.v.pred <- sum(tmp.v.pred[1:R])
    bp.y <- step(fit.y.pred - fit.y)
    bp.v <- step(fit.v.pred - fit.v)
    
    # Summary stats
    mean.tp <- mean(tp[1:R, 1:J])
    
} "
    ) # End model statement

# ------------------------------------------------------------------------------
# Combine all of the data for jags  
jags_dat <- list(
  # Data 
  v = v,                 # Number of vocalizations (RxJ matrix)
  y = y,                 # Sites with at least one vocalization (RxJ matrix)
  n = n,                 # Number of validated detection (RxJ matrix)
  k = k,                 # Number of true positives by site (RxJ matrix)
  date = date,           # Date of each recording (RxJ matrix)ys
  # Site indexing constants 
  R = R,                 # Total number of sites (scalar)
  R.val = R.val,         # Number of sites where acoustic classifications were validated (scalar)
  sites.a = sites.a,     # Indexes of sites with acoustic detections (Vecor)
  sites.v = sites.v,     # Indexes of sites with validations (Vector)
  # Time Indexing constants
  J = J,             # Maximum number of Hours (Scalar)
  # J = J,                 # Total number of survey periods at each site (R vector)
  J.val = J.val,         # Number of vocalizations that were validated during each survey window (R vector)
  J.a = J.a,             # Number of Hours with any vocalizations (R vector)
  times.a = times.a,     # Indexes of survey windows with vocalizations (R x J.a matrix)
  times.v = times.v      # Times when validations occurred (R.val x J.val matrix)
)
# View
# jags_dat

# ------------------------------------------------------------------------------
# Initial Values 
inits <- function() list(
  tau.beta0.site = runif(1, 0, 1),
  beta0.site = rnorm(R, 0, 1),
  gamma0 = rnorm(1, 0, 1),
  gamma.date = rnorm(1, 0, 1),
  gamma.date2 = rnorm(1, 0, 1),
  omega = runif(1, 0, 3),
  mu.alpha = runif(1, 0, 1),
  alpha.N = runif(1, 0, 3),
  epsilon.phi = runif(0, 1),
  phi = matrix(runif(R*J, 0, 1), nrow = R, ncol = J, byrow = FALSE),
  N = N.inits,
  K = K.inits
)

# ------------------------------------------------------------------------------
# Parameters to save
params <- c(
  "beta0.site",
  "mu.alpha",
  "alpha.N",
  "epsilon.phi",
  "omega",
  "gamma0",
  "gamma.date",
  "gamma.date2",
  "N",
  "bp.y",
  "bp.v",
  "mean.tp"
)

# ------------------------------------------------------------------------------
# MCMC settings for
nc <- 3  ;  ni <- 60000  ;  nb <- 30000;  nt <- 25

# Quick check of how many samples we'll keep in the posterior
message(paste((ni - nb) / nt), " samples will be kept from the posterior")

# Call jags from R
start <- Sys.time() %>%  print()          # Start time for the sampler
aru_abund_mod <- jags(data = jags_dat, 
                      inits = inits, 
                      parameters.to.save =  params, 
                      model.file = paste0(mod_path, mod_name), 
                      n.chains = nc, 
                      n.thin = nt, 
                      n.iter = ni, 
                      n.burnin = nb)
difftime(Sys.time(), start)               # End time for the sampler

# Save model output to local drive
saveRDS(aru_abund_mod, file = paste0(mod_path, "_howa_aru_abund_model.rds"))

# View model summary
round(summary(aru_abund_mod), 2)

# Traceplots and density graphs 
MCMCtrace(object = aru_abund_mod$samples,
          params = "all",
          # excl = c(""),
          pdf = TRUE,
          open_pdf = TRUE,
          ind = TRUE,
          n.eff = TRUE,
          wd = "C:\\Users\\willh\\Documents\\NCSU\\Model_Outputs",
          filename = "model_AV_traceplot",
          type = 'both')
