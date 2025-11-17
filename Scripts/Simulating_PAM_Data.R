################################################################################
# Title: Simulating Pasive acoustic monitoring data with a subset of the classifications validated  
# Author: Will Harrod
# Date Created: 2025-10-29
################################################################################

################################################################################
# 1) Prep ######################################################################
################################################################################

# Clear environemnts
rm(list = ls())

# Add packages
library(tidyverse)

# 2) Define simulation parameters

# 2.1) Survey information ------------------------------------------------------

# Number of days 
n.days <- 36
# Number of sites
R  <- 25
# Number of intervals per day 
nints <- 6
# Number of survey periods at each site
J <- rep(nints*n.days, R)
# Maximum number of survey periods 
J.max <- max(J)
# Days when each survey took place
days <- matrix( nrow = R, ncol = J.max)

# View
n.days
R
J
J.max

# 2.2) Parameters --------------------------------------------------------------

# Random intercept by site
beta0 <- runif(R, 1, 2)
# Effect of day on singing rate per 10 minute interval
gamma.day <- -0.2
# Flase positive rate
omega <- 1
# prob (on logit scale) of detecting at least one vocalization at a site that is not occupied.
alpha0 <- 0.1
# additional prob (on logit scale) of detecting at least one vocalization for a site that is occupied
alpha.N <- 0.3

# 2.3) Latent variables --------------------------------------------------------

# expected number of individuals at each site
lambda <- exp(beta0)

# Expected singing rate 
delta 

# 3) Simulate Data 

# 3.1) Bird abundance across the landscape

# 3.2) ARU's 

