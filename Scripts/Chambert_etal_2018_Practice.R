################################################################################
# Title: Practice Modeling following the methods outlined by Chambert et al.(2018)
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

# Add data 
birds <- read.csv("Data\\bear_river_birdnet_detections_validated_june_2023.csv")

# View data 
slice_head(birds, n = 30)

# 1.2) Convert data to a format Nimble can use ---------------------------------

# 2) Run the model #############################################################

# 2.1) Model specification -----------------------------------------------------

#################################################################################################################################
### Chambert et al. (2018) Model G ###

model{
  
  ## PRIORS
  psi ~ dunif(0,1)	# psi = Pr(Occupancy)
  p10 ~ dunif(0,1)	# p10 = Pr(y = 1 | z = 0)
  p11 ~ dunif(0,1)	# p11 = Pr(y = 1 | z = 1)
  
  lam ~ dunif(0,1000)	# lambda: Parameter of the Poisson process for TRUE Detections
  ome ~ dunif(0,1000)	# omega: Parameter of the Poisson process for FALSE Detections
  
  
  ##  Likelihood 
  for (i in 1:I){
    
    z[i] ~ dbern(psi)			# Latent Variable z: Occupancy status of site 'i' -- Bernoulli process
    p[i] <- z[i]*p11 + (1-z[i])*p10 	# p: 'detection' parameter for the next Bernoulli Process: p equals p11 if z=1, p10 if z=0
    
    for(t in 1:T){
      y[i,t] ~ dbern(p[i])			# Observed Variable y -- Bernoulli process
    } # t
    
    v[i] <- sum(y[i,]) 			# Total number of occasions with at least one detection at that site -- takes value between 0 and T {0,..,T}
    w[i] <- 1-equals(v[i],0)		# Binary transformation of v[i] =>  = 1 if there was >=1 detection, 0 if no detection -- Takes values {0/1} 
    
    
    ### probsN is the probability array used for the total number of detections N[i]
    ## Define the probability that N = 0 (No detection at all)
    probsN[i,1] <- 1 - w[i]		# Pr(N=0) is directly determined by w[i]
    
    ## The next 5 lines define the probability cells corresponding to the ZERO-TRUNCATED Poisson Process used for N[i], when w=1 -- i.e., for Pr(N>0)
    for(r in 2:R){  
      probsN[i,r] <- w[i]*(  exp(-(lam*z[i]+ome))*(pow((lam*z[i]+ome),x[r])) / ( exp(logfact(x[r]))*(1-exp(-(lam*z[i]+ome))) )   )
    } # r
    probsN[i,R+1] <- 1 - sum(probsN[i,1:R])	# Pr(N>R) // R is the highest value taken by N in the data + 1
    
    ## The next 3 lines define the probabilistic node N
    NNN[i] ~ dcat(probsN[i,])	# Here, NNN takes values = {1,2,3,...}
    NN[i] <- NNN[i]-1		# Rescale to values = {0,1,2,...} to include 0
    N[i] ~ dbin(1,NN[i])		# Stocahstic Node N (DATA): Total number of detection (false and true detections combined)
    
    K[i] ~ dpois(lam*z[i])		# Stochastic node for the latent variable K (total number of True Detections)
    Q[i] <- N[i]-K[i]		# Deterministic node: Q is the total number of False Positives
    
    # HyperGeometric Process: n[i] is the number of detection VALIDATED for site 'i' ## k[i] is the number of True Positives found during that process
    k[i] ~ dhyper(K[i], Q[i], n[i], 1)
    
  } # 'i'
  
} # end 'model'


##########################################################################################################################################################################################

