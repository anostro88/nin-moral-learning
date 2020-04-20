##### Load necessary libraries and set up multi-core processing for Stan #####
library(dplyr);
library(rstan);
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(reshape2);
library(hBayesDM)
library(readxl)
library(tidyverse)
library("rstanarm")
library("bayesplot")
library("loo")
library("pROC")
library("shinystan")
library("rstan")
library("ggplot2")

probe_folder = "fitting_models/Probe_study"
data_folder = paste0(probe_folder, "/data")
#### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ LOAD DATA ####
Sublist = read.csv(paste0(data_folder, "/sublist.csv"), stringsAsFactors=FALSE)

N = nrow(Sublist); # number of participants
T = 120; #20 trials for 6 blocks: 10 T with 2 outcomes + 10 T with one outcome only
TBlock = 20; # number of trials for block
Tsubj = rep(T, N); #vector containing the number of trials for each subject
NBlock = 6

# CHOICE
choice = read.csv(paste0(data_folder, "/choice.csv"), stringsAsFactors=FALSE)
choice$col_sub = NULL
choice = data.matrix(choice)
# MONEY OUTCOME
outcomeM = read.csv(paste0(data_folder, "/outcomeM.csv"), stringsAsFactors=FALSE)
outcomeM$col_sub = NULL
#SHOCK OUTCOME
outcomeS = read.csv(paste0(data_folder, "/outcomeS.csv"), stringsAsFactors=FALSE)
outcomeS$col_sub = NULL

# FILTER MONEY 
filter_money = read.csv(paste0(data_folder, "/filter_ismoney.csv"), stringsAsFactors = F, header = T)
filter_money$subID = NULL

# FILTER SHOCK
filter_shock = read.csv(paste0(data_folder, "/filter_isshock.csv"), stringsAsFactors = F)
filter_shock$subID = NULL

#### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 1/-1 coding for Models 3,4,7, 10, 12 ####
outcomeM[outcomeM == 2] = -1 #Low gain is labeled 2 --> Apun
outcomeM[outcomeM == 1] = 1 #High gain is labeled 1 --> Arew
outcomeM = data.matrix(outcomeM)

outcomeS[outcomeS == 2] = -1 #High shock is labeled 2 --> Bpun
outcomeS[outcomeS == 1] = 1 #Low shock is labeled 1 --> Brew
outcomeS = data.matrix(outcomeS)

filter_money$subID = NULL
filter_money = data.matrix(filter_money)

filter_shock$subID = NULL
filter_shock = data.matrix(filter_shock)

# Training data ----

training_data = list(
  N,
  T,
  TBlock,
  Tsubj,
  choice=choice,
  outcomeM=outcomeM,
  outcomeS=outcomeS,
  filter_money=filter_money,
  filter_shock=filter_shock
)

# Models ---- 
get_model = function(filename, data) {
  return(
    stan(
      file=paste0(probe_folder, '/', filename),  
      data=data, 
      cores=getOption("mc.cores", 1L), #setting the mc.cores option to be as many processors as the hardware and RAM allow (up to the number of chains)
      thin=1, 
      warmup=1000, # The number of warmup iterations should not be larger than iter and the default is iter/2
      chains=4, 
      iter=2000, 
      control=list(adapt_delta = 0.95)) 
  )
}

##### MODEL1wf: 1LR + wf       2AB   1/-1 coding #####
fit_M1wf = get_model('20T_M1wf_F-AF.stan', training_data)

##### MODEL2: 2LR        2AB   1/-1 coding #####
fit_M2 = get_model('20T_M2_F.stan', training_data)

##### MODEL2wf: 2LR + wf       2AB   1/-1 coding #####
fit_M2wf = get_model('20T_M2wf_wfDec_F-AF.stan', training_data)

##### MODEL0: 2LR + wf       2AB   1/-1 coding #####
fit_M0 = get_model('20T_M0_F-AF.stan', training_data)
