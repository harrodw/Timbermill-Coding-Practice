################################################################################
# Title: Theoretical model for estimating relative abundance from ARU data 
# Author: Will Harrod
# Date Created: 2025-10-15

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

# Define a species 
species_name <- "Warbling Vireo"
species_file <- str_replace_all(species_name, " ", "_") 
species_file <- str_replace_all(species_file, "_", "_") 
species_file <- str_replace_all(species_file, "'", "") 
species_file

# Define a interval length (minutes)
survey_period <- 60

# Add all a birds data 
all_birds1 <- read.csv("Data\\bear_river_birdnet_detections_june_2023.csv") 
# Add the full dataset from git
all_birds1 <- read.csv("")

# Clean the data 
all_birds <- all_birds1 %>% 
  # Arrange by AUR ID then by date then ny time
  arrange(ARU.ID, Date, start.min, start.sec) %>% 
  # Convert date to a date
  mutate(Date = ymd(Date)) %>%
  select(-X) %>% 
  # New column for survey interval
  mutate(Interval = floor(start.min/survey_period)) %>% 
  # New column for interval by date 
  mutate(Date.Interval = paste(as.numeric(Date), Interval, sep = "-")) %>% 
  # Create numeric versions of some of the variables 
  mutate(ARU.ID.num = as.numeric(as.factor(ARU.ID)),
         Date.Interval.num = as.numeric(as.factor(Date.Interval)))
  
  
# View data 
glimpse(all_birds)

# Pull out a single species
species <- all_birds %>% 
  filter(common_name == species_name)

# The number of intervals that length in a day
nints_daily <- all_birds %>% 
  distinct(Date, Date.Interval.num) %>%
  filter(Date == min(Date)) %>% 
  group_by(Date) %>% 
  reframe(Date, nints = n()) %>% 
  distinct() %>% 
  pull(nints)
# View
nints_daily

# Define all of the ARU sites surveyed
ARUs_tbl <- all_birds %>% 
  distinct(ARU.ID, ARU.ID.num) 
# Make into a vector
ARUs <- ARUs_tbl$ARU.ID.num
# View
ARUs_tbl
ARUs

# Are all of the ARUs present in the data?
length(ARUs) == length(unique(ARUs))

# Define all of the sites surveyed
intervals_tbl <- all_birds %>% 
  distinct(Date.Interval, Date.Interval.num) %>% 
  arrange(Date.Interval.num)
# Make into a vector
intervals <- intervals_tbl$Date.Interval.num
# View
intervals_tbl
intervals

# Tibble of all sitte interval combinations
site_ints <- all_birds %>% 
  distinct(ARU.ID.num, Date.Interval.num) %>% 
  arrange(ARU.ID.num, Date.Interval.num)

# Are all of the intervals present in the data?
length(intervals) == length(unique(intervals))

# Define a number of rows to make into false positives
n_false <- 10

# Turn those rows into flase positives 
false_pos_rows <- sort(floor(runif(n_false, 1, 200)))
false_pos_rows

# Add validations from local drive
validations1 <- read.csv(paste0("Data\\bear_river_birdnet_detections_june_2023_", species_file, "_validated.csv")) 
# Add valadations from git 
validations1 <- read.csv("https://github.com/harrodw/Timbermill-Coding-Practice/blob/main/Data/bear_river_birdnet_detections_june_2023_Warbling_Vireo_validated.csv")


# Clean the validations 
validations <- validations1 %>% 
  select(-X) %>% 
  # Convert date to a date
  mutate(Date = ymd(Date)) %>% 
  # Remove unvalidated rows
  filter(!is.na(True.Positive)) %>% 
  # New column for 10 minute interval
  mutate(Interval = floor(start.min/survey_period)) %>% 
  # New column for interval by date 
  mutate(Date.Interval = paste(as.numeric(Date), Interval, sep = "-")) %>% 
  # Join the numeric ARU ID's
  left_join(ARUs_tbl, by = "ARU.ID") %>% 
  # Join the numeric intervals
  left_join(intervals_tbl, by = "Date.Interval") %>% 
  # Asign several random rows to be false positives
  mutate(True.Positive = case_when(rowid %in% false_pos_rows ~ 0, 
                                   TRUE ~ True.Positive))
  
# View
glimpse(validations)

# Make a dataframe of true positives 
true_pos <- validations %>% 
  filter(True.Positive == 1)

# How many observations?
nrow(all_birds)

# How many days?
length(unique(all_birds$Date))

# How many ARU's?
length(ARUs)

# How many intervals 
length(intervals)

# How many were validated? 
nrow(validations)

# How many are true positives?
nrow(true_pos)

# 1.2) Convert data to a format jags can use ---------------------------------

# Constants --------------------------------------------------------------------

# Define R (the number of sites) and J.max (the maximum number of sampling intervals)
R <- length(ARUs)
J.max <- length(intervals)

# View
R
J.max

# J ----------------------------------------------------------------------

# Define J, the number of survey periods at each site
J <- all_birds %>% 
  group_by(ARU.ID.num) %>% 
  reframe(ARU.ID.num, Max.Int = max(Date.Interval.num)) %>% 
  distinct() %>% 
  pull(Max.Int)
# View
J

# v ------------------------------------------------------------------------------
# Define v, the number of positive detections by site and day, whether true or false
v <- all_birds %>% 
  # Define all sampling periods
  distinct(ARU.ID.num, Date.Interval.num) %>% 
  # Join with the species specific detections 
  left_join(species, by = c("ARU.ID.num", "Date.Interval.num")) %>% 
  # Add a column for whether or not any detections occured during that period
  mutate(Detection = case_when(!is.na(common_name) ~ 1,
                               is.na(common_name) ~ 0)) %>% 
  # Calculate detections by by sampling period
  group_by(Date.Interval.num, ARU.ID.num) %>% 
  reframe(Date.Interval.num, ARU.ID.num, Count = sum(Detection)) %>% 
  distinct() %>%  
  # Arrange in a better order 
  arrange(Date.Interval.num, ARU.ID.num) %>%
  # Join with sites that had no observations
  right_join(site_ints, by = c("ARU.ID.num", "Date.Interval.num")) %>% 
  # Convert to a detection history table
  pivot_wider(names_from = Date.Interval.num, values_from = Count) %>% 
  # Populate zero-counts
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0))) %>% 
  # Put the rows in the right order
  arrange(ARU.ID.num) %>% 
  # Remove the ARU ID column
  select(-ARU.ID.num) %>% 
  # Convert to a matrix
  as.matrix()
# View
v
str(v)
# y ------------------------------------------------------------------------------

# Define y, a matrix of whether or not each site had any detections 
y <- all_birds %>% 
  # Define all sampling periods
  distinct(ARU.ID.num, Date.Interval.num) %>% 
  # Join with the species specific detections 
  left_join(species, by = c("ARU.ID.num", "Date.Interval.num")) %>% 
  # Add a column for whether or not any detections occured during that period
  mutate(Detection = case_when(!is.na(common_name) ~ 1,
                               is.na(common_name) ~ 0)) %>% 
  # Calculate detections by by sampling period
  group_by(Date.Interval.num, ARU.ID.num) %>% 
  reframe(Date.Interval.num, ARU.ID.num, Count = sum(Detection)) %>% 
  mutate(Count = case_when(Count > 0 ~ 1, Count == 0 ~ 0)) %>% 
  distinct() %>%  
  # Arrange in a better order 
  arrange(Date.Interval.num, ARU.ID.num) %>%
  # Join with sites that had no observations
  right_join(site_ints, by = c("ARU.ID.num", "Date.Interval.num")) %>% 
  # Convert to a detection history table
  pivot_wider(names_from = Date.Interval.num, values_from = Count) %>% 
  # Populate zero-counts
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0))) %>% 
  # Put the rows in the right order
  arrange(ARU.ID.num) %>% 
  # Remove the ARU ID column
  select(-ARU.ID.num) %>% 
  # Convert to a matrix
  as.matrix()

# View
y
str(y)
# n ------------------------------------------------------------------------------

# Group the number of validations by grid and survey
valids_count_int <- validations %>% 
  group_by(ARU.ID.num, Date.Interval.num) %>% 
  reframe(ARU.ID.num, Date.Interval.num, Valid.Count = n()) %>% 
  arrange(ARU.ID.num, Date.Interval.num) %>% 
  distinct()
# View
print(valids_count_int, n = nrow(valids_count_int))
valids_count_int %>% count(ARU.ID.num)

# Define n, the number of validations for each survey window 
n <- all_birds %>% 
  # Define all sampling periods for each site 
  distinct(Date.Interval.num, ARU.ID.num) %>%  
  # Join with the validations
  left_join(valids_count_int, by = c("Date.Interval.num", "ARU.ID.num")) %>% 
  # Arrange in a better order 
  arrange(Date.Interval.num, ARU.ID.num) %>% 
  # Convert to a detection history table
  pivot_wider(names_from = Date.Interval.num, values_from = Valid.Count) %>% 
  # Populate zero-counts
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0))) %>% 
  # Put the rows in the right order
  arrange(ARU.ID.num) %>% 
  # Remove the ARU ID column
  select(-ARU.ID.num) %>% 
  # Convert to a matrix
  as.matrix()
# View
n
str(n)

# k ---------------------------------------------------------------------------

# Group the number of true positives by grid and survey
pos_count <- true_pos %>% 
  group_by(ARU.ID.num, Date.Interval.num) %>% 
  reframe(ARU.ID.num, Date.Interval.num, Valid.Count = sum(True.Positive)) %>% 
  arrange(ARU.ID.num, Date.Interval.num) %>% 
  distinct() 
# View
print(pos_count, n = Inf)
count(pos_count, ARU.ID.num)

# Construct k, the number of true positives by site and day
k <- all_birds %>% 
  # Define unique survey periods
  distinct(ARU.ID.num, Date.Interval.num) %>% 
  # Add in the number of true posiitives 
  left_join(pos_count, by = c("ARU.ID.num", "Date.Interval.num")) %>% 
  # Arrange in a better order 
  arrange(Date.Interval.num, ARU.ID.num) %>% 
  # Convert to a detection history table
  pivot_wider(names_from = Date.Interval.num, values_from = Valid.Count) %>% 
  # Populate zero-counts
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0))) %>% 
  # Put the rows in the right order
  arrange(ARU.ID.num) %>% 
  # Remove the ARU ID column
  select(-ARU.ID.num) %>%
  # Convert to a matrix
  as.matrix()
# View
k
str(k)

# Compare n to k
n - k

# J.a -----------------------------------------------------------------------------

# Define all of the sites
all_sites <- distinct(all_birds, ARU.ID.num)

# Define J.a, the number intervals with vocalizations at each site 
J.a <- species %>% 
  # distinct survey periods 
  distinct(ARU.ID.num, Date.Interval.num) %>% 
  # Number of intervals with vocalizations at each site
  count(ARU.ID.num) %>% 
  # Add in the sites that may have no vocalizations
  left_join(all_sites, by = "ARU.ID.num") %>% 
  # Put in the right order
  arrange(ARU.ID.num) %>%
  # Fill in missing values 
  mutate(n = replace_na(n, 0)) %>% 
  # Vectorize that information
  pull(n)
# View# View# View
J.a

# sites.a ------------------------------------------------------------------------
# Indexes of where acoustic data were obtained for the specific species
sites.a <- species %>% 
  distinct(ARU.ID.num) %>% 
  arrange(ARU.ID.num) %>% 
  pull(ARU.ID.num)
# View
sites.a

# sites.v ------------------------------------------------------------------------
# Indexes of where acoustic data were validated for the specific species
sites.v <- validations %>% 
  distinct(ARU.ID.num) %>% 
  arrange(ARU.ID.num) %>% 
  pull(ARU.ID.num)
# View
sites.v

# J.a -----------------------------------------------------------------------------

# Define J.val, the number of intervals where vocalizations were validated at each site
J.val <- validations %>% 
  distinct(ARU.ID.num, Date.Interval.num) %>%  
  group_by(ARU.ID.num) %>% 
  reframe(ARU.ID.num, Valid.Count = n()) %>% 
  distinct() %>% 
  arrange(ARU.ID.num) %>% 
  pull(Valid.Count)
# View
J.val

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
times.a <- matrix(NA, nrow = R, ncol = J.max)
 
# Add the time indexing information
for (i in 1:R) {
  # Define all of periods with vocalizations for a single ARU
  ints.a <- species %>% 
    filter(ARU.ID.num == i) %>% 
    distinct(Date.Interval.num) %>% 
    pull(Date.Interval.num)
  # Assign those values to the indexing variable 
  times.a[i, 1:length(ints.a)] <- ints.a
}
# View
times.a

# times.v --------------------------------------------------------------------
# The times when each valedation occured
# Storage object for time with detections
times.v <- matrix(NA, nrow = R.val, ncol = max(J))

# Add the time indexing information
for (i in 1:R.val) {
  # Define all of periods with validations for a single ARU
  ints.v <- validations %>% 
    filter(ARU.ID.num == i) %>% 
    distinct(Date.Interval.num) %>% 
    pull(Date.Interval.num)
  # Assign those values to the indexing variable 
  times.v[i, 1:length(ints.v)] <- ints.v
  
}
# View
times.v

# Day -------------------------------------------------------------------------

# Number of days surveyed
n.days <- length(unique(all_birds$Date))

# Day when each survey took place 
days <- all_birds %>% 
  select(ARU.ID, Date.Interval) %>% 
  arrange(Date.Interval, ARU.ID) %>% 
  # Convert date and ARU ID to factors
  mutate(Date.Interval = as.numeric(as.factor(Date.Interval)),
         ARU.ID = as.numeric(as.factor(ARU.ID))) %>% 
  # Expand to include values with no recordings
  expand(ARU.ID, Date.Interval) %>% 
  distinct(ARU.ID, Date.Interval) %>% 
  # Define date based on the interval
  mutate(Date = ceiling(Date.Interval/nints_daily)) %>% 
  # convert to a wider table
  pivot_wider(names_from = Date.Interval, values_from = Date) %>% 
  #Remove the ARU ID Column
  select(-ARU.ID) %>% 
  # Convert to a matrix
  as.matrix()
# View
days

# K ----------------------------------------------------------------------------

# Define K, the latent number of true positives at each site
K.inits <- matrix(NA, nrow = R.val, ncol = max(J.val))

# Define an estimated proportion of true positives 
tp_rate <- 0.05 

# Initialize each site with the number of true positive validations that occurred there
for(i in 1:R.val){
  for(j in 1:J.val[i]){
    # Pull out the number of vocalizations that that site
    voc <- v[sites.v[i], times.v[i, j]]
    # Pull out the number of validations that that site
    pos <- k[sites.v[i], times.v[i, j]]
    # Define the number of unvalidated vocalizations 
    unval <- voc - val
    
    # Subtract 1 from the sites with more than 1 validation
    # if(voc == 1) {
    #   pos <- voc 
    # } else {
    #   pos <- voc - floor(fp_rate*voc)
    # } 
    
    # Asign soething between validations and vocalizations as the initial values for K
    K.inits[i, j] <- val + floor(unval*tp_rate)
  }
}

# View
K.inits
str(K.inits)

# ------------------------------------------------------------------------------

# 2) Run the model #############################################################

# 2.1) Model specification -----------------------------------------------------

# File path to send the outputs
mod_path <- "C:\\Users\\willh\\OneDrive\\Documents\\NCSU\\Disertation_Code\\Bayesian_Mods\\"

# Model statement using nimble 
mod_name <- "aru_abund_mod.txt"

# Model statement
cat(file = paste0(mod_path, mod_name),
"  model{

####### Model definitions #############################
    # Indexing -------------------------------------------
    # i = sampling locations (ARU's)
    # j = 10-minute sampling intervals 
    # R = total number of sites
    # R.Val = Number of ARU sites with validations
    # J.max = Maximum number of intervals at any site
    # J number of sampling intervals at each site
    # J.val = Number of intervals at each site with validations
    # times.a = Times when acoustic activity occurred
    # times.v = Times when validations occurred
    # sites.a = Sites with at least one vocalization
    # sites.v = Sites with at least one validation
    # Data ------------------------------------------------
    # y = A matrix of sites by visits for whether or not each site had any detection
    # v = A matrix of sites by visits for the number of vocalizations 
    # n = Number of validations at each visit to each site
    # k = Number of validated positives at each visit to each site
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
    
    # Alpha node (parameters that influence detection probability) ----
    mu.alpha ~ dunif(0, 1)
    alpha0 <- log(mu.alpha / (1 - mu.alpha)) 
    alpha.N ~ dunif(0, 1000)
    
    # Gamma node (Parameters that influence vocalization rate) ----
    
    # Hyperprior for the effect of day
    tau.gamma.day ~ dgamma(0.01, 0.01)
    
    # Effect of day on number of detection 
    for (d in 1:n.days) {
      gamma.day[d] ~ dnorm(0, tau.gamma.day)
    } # d
    
    # Estimated rate of false positives ----
    omega ~ dunif(0, 1000) 
    
    # Phi node (Unmodeled overdispersion in singing rate) ----
    
    # Hyper prior for unmodeled variation in abundance 
    a.phi ~ dunif(0, 100)
    
    # Unexplained variation in singing rate
    for (i in 1:R) {
      for (j in 1:J.max) { 
        phi[i, j] ~ dgamma(a.phi, a.phi)
      } # j
    } # i
    
    # Liklihood ----------------------------------------------------------------
    for (i in 1:R) {
      
      # Linear combination expected for abundance at each point
      log(lambda[i]) <- beta0.site[i]
      
      # Abudnance at each point
      N[i] ~ dpois(lambda[i])
      
      # Linear combination for the probability of detecting a bird at each point 
      logit(p[i]) <- alpha0 + alpha.N * N[i] 
      
      # Acoustic Data -------------------
      for (j in 1:J[i]) { 
        
        # Log linear combination for the per individual rate of vocalization
        log(delta[i, j]) <- gamma.day[days[i, j]]
        
        # Probability of detecting an individual 
        y[i, j] ~ dbern(p[i])
        
        # True positive rate 
        tp[i, j] <- delta[i, j] * N[i] / (delta[i, j] * N[i] + omega)
        
        # Posterior predictive checks for Bayesian P-value for observations
        y.pred[i, j] ~ dbern(p[i])
        resid.y[i, j] <- pow(pow(y[i, j], 0.5) - pow(p[i], 0.5), 2)
        resid.y.pred[i, j] <- pow(pow(y.pred[i, j], 0.5) - pow(p[i], 0.5), 2)
        
      } # j
      
      # Number of vocalizations for each visit to each site
      for (j in 1:J.a[i]) {
        
        # Log-linear combination for the number of vocalizations at site i during visit j for the zero-truncated poisson
        v.lin.comb[i, times.a[i, j]] <- (delta[i, times.a[i, j]] * N[i] + omega) * phi[i, times.a[i, j]]
        
        # number of detected vocalizations for sites with at least one vocalization
        v[i, times.a[i, j]] ~ dpois(v.lin.comb[i, times.a[i, j]] * y[i, times.a[i, j]])T(1, 1000)
        
        # Posterior predictive checks for Bayesian P-value for singing rate ----
        v.pred[i, j] ~ dpois(v.lin.comb[i, times.a[i, j]] * y[i, times.a[i, j]])T(1, 1000)
        
        # Expected value for the number of vocalizations
        mu.v[i, j] <- v.lin.comb[i, times.a[i, j]] / (1 - exp(-1 * v.lin.comb[i, times.a[i, j]]))
        
        # Freeman Tukey statistic for observed singing rate
        resid.v[i, j] <- pow(pow(v[i, times.a[i, j]], 0.5) - pow(mu.v[i, j], 0.5), 2)

        # Freeman Tukey statistic for simulated singing rate
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

    # # Check model fit
    fit.y <- sum(resid.y[sites.a, 1:J.max])
    fit.y.pred <- sum(resid.y.pred[sites.a, 1:J.max])
    fit.v <- sum(tmp.v[1:R])
    fit.v.pred <- sum(tmp.v.pred[1:R])
    bp.y <- step(fit.y.pred - fit.y)
    bp.v <- step(fit.v.pred - fit.v)
} "
    ) # End model statement

# ------------------------------------------------------------------------------
# Combine all of the data for jags  
jags_dat <- list(
  # Data 
  y = y,                 # Number of vocalizations (RxJ.max matrix)
  n = n,                 # Number of validated detection (RxJ.max matrix)
  # k = k,                 # Number of true positives by site (RxJ.max matrix)
  days = days,           # Date of each recording (RxJ.max matrix)
  n.days = n.days,       # Number of days
  # Site indexing constants 
  R = R,                 # Total number of sites (scalar)
  # R.val = R.val,         # Number of sites where acoustic classifications were validated (scalar)
  # sites.a = sites.a,     # Indexes of sites with acoustic detections (Vecor)
  # sites.v = sites.v,     # Indexes of sites with validations (Vector)
  # Time Indexing constants
  J.max = J.max,             # Maximum number of intervals (Scalar)
  J = J,                 # Total number of survey periods at each site (R vector)
  # J.val = J.val,         # Number of vocalizations that were validated during each survey window (R vector)
  J.a = J.a,             # Number of intervals with any vocalizations (R vector)
  times.a = times.a     # Indexes of survey windows with vocalizations (R x J.a matrix)
  # times.v = times.v      # Times when validations occurred (R.val x J.val matrix)
)
# View
jags_dat

# ------------------------------------------------------------------------------
# Initial Values 
inits <- function() list(
  tau.beta0.site = runif(1, 0, 1),
  beta0.site = rnorm(R, 0, 1),
  tau.gamma.day = runif(1, 0, 0.1),
  gamma.day = rnorm(n.days, 0, 1),
  omega = runif(1, 0, 3),
  mu.alpha = runif(1, 0, 1),
  alpha.N = runif(1, 0, 3),
  a.phi = runif(0, 3),
  phi = matrix(runif(R*J.max, 0, 1), nrow = R, ncol = J.max, byrow = FALSE),
  N = rpois(R, mean(v))
  # K = K.inits
)

# ------------------------------------------------------------------------------
# Parameters to save
params <- c(
  "beta0.site",
  "mu.alpha",
  "alpha.N",
  "gamma.day",
  "a.phi",
  "omega",
  "N"
  # "bp.y",
  # "bp.v"
)
sites.v
times.v
# ------------------------------------------------------------------------------
# MCMC settings for
nc <- 3  ;  ni <- 1500  ;  nb <- 500;  nt <- 2 

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
saveRDS(aru_abund_mod, file = paste0(mod_path, species_file, "_aru_abund_model.rds"))

# View model summary
summary(aru_abund_mod)
