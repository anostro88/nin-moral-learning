##### Load necessary libraries and set up multi-core processing for Stan #####
library(dplyr); 
library(rstan);
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(reshape2);
library(hBayesDM)
library(readxl)
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(loo)
library(pROC)
library(shinystan)
library(rstan)
library(ggplot2)

#### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ LOAD DATA ####
N = 27; # number of participants
T = 60; #10 trials for 6 blocks
TBlock = 10; # number of trials for block
Tsubj = rep(T, N); #vector containing the number of trials for each subject

# CHOICE
choice = read.csv("choice.csv", stringsAsFactors=FALSE)
choice$subjID = NULL
choice = data.matrix(choice)

# MONEY OUTCOME
outcomeM = read.csv("conflictoutcomeM.csv", stringsAsFactors=FALSE)
outcomeM$subjID = NULL
#SHOCK OUTCOME
outcomeS = read.csv("conflictoutcomeS.csv", stringsAsFactors=FALSE)
outcomeS$subjID = NULL

#### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
outcomeM[outcomeM == 2] = -1 #Low gain is labeled 2 --> Apun
outcomeM[outcomeM == 1] = 1 #High gain is labeled 1 --> Arew
outcomeM = data.matrix(outcomeM)

outcomeS[outcomeS == 2] = -1 #High shock is labeled 2 --> Bpun
outcomeS[outcomeS == 1] = 1 #Low shock is labeled 1 --> Brew
outcomeS = data.matrix(outcomeS)


##### MODEL0:   0LR            2AB   1/-1 coding #####
fit_M0 = stan(file='M0_final.stan',  
              data = list(N,T,TBlock,Tsubj, choice = choice,outcomeM = outcomeM, outcomeS = outcomeS), 
              cores = getOption("mc.cores", 1L), #setting the mc.cores option to be as many processors as the hardware and RAM allow (up to the number of chains)
              thin=1, 
              warmup = 1000, # The number of warmup iterations should not be larger than iter and the default is iter/2
              chains = 4, 
              iter = 2000, 
              control = list(adapt_delta = 0.95))

##### MODEL1wf:  2AB  1LR wf as Trade off  1/-1 coding #####
fit_M1wf = stan(file='M1wf_final.stan',  
                data = list(N,T,TBlock,Tsubj, choice = choice,outcomeM = outcomeM, outcomeS = outcomeS), 
                cores = getOption("mc.cores", 1L), #setting the mc.cores option to be as many processors as the hardware and RAM allow (up to the number of chains)
                thin=1, 
                warmup = 1000, # The number of warmup iterations should not be larger than iter and the default is iter/2
                chains = 4, 
                iter = 2000, 
                control = list(adapt_delta = 0.95))


##### MODEL2wfDec: 2AB  2LR + wf on Decision phase   1/-1 coding #####
fit_M2wfDec = stan(file='M2wf_wfonDec_old.stan',  
                   data = list(N,T,TBlock,Tsubj, choice = choice,outcomeM = outcomeM, outcomeS = outcomeS), 
                   cores = getOption("mc.cores", 1L), #setting the mc.cores option to be as many processors as the hardware and RAM allow (up to the number of chains)
                   thin=1, 
                   warmup = 1000, # The number of warmup iterations should not be larger than iter and the default is iter/2
                   chains = 4, 
                   iter = 2000, 
                   control = list(adapt_delta = 0.95))

##### MODEL2wfOut: 2AB  2LR + wf on Outcome phase  1/-1 coding #####
fit_M2wfOut = stan(file='M2wf_wfonOut_0612.stan',  
                   data = list(N,T,TBlock,Tsubj, choice = choice,outcomeM = outcomeM, outcomeS = outcomeS), 
                   cores = getOption("mc.cores", 1L), #setting the mc.cores option to be as many processors as the hardware and RAM allow (up to the number of chains)
                   thin=1, 
                   warmup = 1000, # The number of warmup iterations should not be larger than iter and the default is iter/2
                   chains = 4, 
                   iter = 2000, 
                   control = list(adapt_delta = 0.95))
