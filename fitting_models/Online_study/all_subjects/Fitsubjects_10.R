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

all_subjects_folder = "'fitting_models/Online_study/all_subjects"
data_folder = paste0(all_subjects_folder, "/data")

#### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ LOAD DATA ####
Sublist = read.csv(paste0(data_folder, "/sublist.csv"), stringsAsFactors=FALSE)
N = nrow(Sublist); # number of participants
trial= 80; # 10 T with 2 outcomes 
triald=160; #both regular and drop
TBlock = 10; # number of trials for block
TDBlock=20;
Tsubj = rep(trial, N); #vector containing the number of trials for each subject
TDsubj=rep(triald, N);
NBlock = 8


# CHOICE
choice = read.csv(paste0(data_folder, "/choice.csv"), stringsAsFactors=FALSE)
choice$col_sub = NULL
choice = data.matrix(choice)

choice_drop=read.csv(paste0(data_folder, "/choice_drop.csv"), stringsAsFactors=FALSE)
choice_drop$col_sub = NULL
choice_drop = data.matrix(choice_drop)

# MONEY OUTCOME
outcomeM = read.csv(paste0(data_folder, "/outcomeM.csv"), stringsAsFactors=FALSE)
outcomeM$col_sub = NULL
outcomeM = data.matrix(outcomeM)

outcomeM_drop= read.csv(paste0(data_folder, "/outcomeM_drop.csv"), stringsAsFactors=FALSE)
outcomeM_drop$col_sub = NULL
outcomeM_drop = data.matrix(outcomeM_drop)

#SHOCK OUTCOME
outcomeS = read.csv(paste0(data_folder, "/outcomeS.csv"), stringsAsFactors=FALSE)
outcomeS$col_sub = NULL
outcomeS= data.matrix(outcomeS)

outcomeS_drop = read.csv(paste0(data_folder, "/outcomeS_drop.csv"), stringsAsFactors=FALSE)
outcomeS_drop$col_sub = NULL
outcomeS_drop= data.matrix(outcomeS_drop)

# FILTER MONEY 
filter_money = read.csv(paste0(data_folder, "/filter_ismoney.csv"), stringsAsFactors = F, header = T)
filter_money$col_sub = NULL
filter_money = data.matrix(filter_money)

# FILTER SHOCK
filter_shock = read.csv(paste0(data_folder, "/filter_isshock.csv"), stringsAsFactors = F)
filter_shock$col_sub = NULL
filter_shock = data.matrix(filter_shock)

#1/-1 coding

outcomeM[outcomeM == 2] = -1 #Low gain is labeled 2 
outcomeM[outcomeM == 1] = 1 #High gain is labeled 1 
outcomeM = data.matrix(outcomeM)

outcomeS[outcomeS == 2] = -1 #High shock is labeled 2
outcomeS[outcomeS == 1] = 1 #Low shock is labeled 1 
outcomeS = data.matrix(outcomeS)

outcomeM_drop[outcomeM_drop == 2] = -1 #Low gain is labeled 2 
outcomeM_drop[outcomeM_drop == 1] = 1 #High gain is labeled 1 
outcomeM_drop = data.matrix(outcomeM_drop)

outcomeS_drop[outcomeS_drop == 2] = -1 #High shock is labeled 2 
outcomeS_drop[outcomeS_drop == 1] = 1 #Low shock is labeled 1 
outcomeS_drop = data.matrix(outcomeS_drop)



# Training data ----

training_data = list(
  N=N,
  trial=trial,
  triald=triald,
  TBlock=TBlock,
  TDBlock=TDBlock,
  Tsubj=Tsubj,
  TDsubj=TDsubj,
  choice=choice,
  choice_drop=choice_drop,
  outcomeM=outcomeM,
  outcomeS=outcomeS,
  outcomeM_drop=outcomeM_drop,
  outcomeS_drop=outcomeS_drop,
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

### MODEL1wf: 1LR + wf       2AB   1/-1 coding #####
fit_M1wf = get_model('20T_M1wf_F-AF_10T.stan', training_data)
save(fit_M1wf, file=paste0(probe_folder, "/fitted_models/fit_M1wf.RData"))


##### MODEL2wf OUTCOME: 2LR + wf       2AB   1/-1 coding #####
fit_M2wfOut = get_model('20T_M2wf_wfOut_F-AF_10T.stan', training_data)
save(fit_M2wfOut, file=paste0(probe_folder, "/fitted_models/fit_M2wfOut.RData"))

##### MODEL2wf DECISION: 2LR + wf       2AB   1/-1 coding #####
fit_M2wfDec = get_model('20T_M2wf_wfDec_F-AF_10T.stan', training_data)
save(fit_M2wfDec, file=paste0(probe_folder, "/fitted_models/fit_M2wfDec.RData"))

##### MODEL0: 2LR + wf       2AB   1/-1 coding #####
fit_M0 = get_model('20T_M0_F-AF.stan', training_data)
save(fit_M0, file=paste0(probe_folder, "/fitted_models/fit_M0.RData"))
