library("rstanarm")
library("bayesplot")
library("loo")
library("ggplot2")
library("reshape2")

log_lik_raw = log_lik_raw
sublist = unique(sub_id_map$unique.df_new.col_sub.)

# M1wf
log_lik_raw_m1 = log_lik_raw[,c("sub", "trial","M1wf_logLik")] 

log_lik_raw_m1$sub = rep(sublist, each = Ntrial_long)
log_lik = data.frame()
for (sub in sublist) {
  this_sub = log_lik_raw_m1[log_lik_raw_m1$sub == sub,]
  this_sub = this_sub[order(this_sub$trial),]
  this_sub$trialNr = rep(seq(1:Ntrial), times = Nblock)
  this_sub$trial = NULL
  this_sub$blockNr = rep(1:Nblock, each = Ntrial)
  log_lik = rbind(log_lik, this_sub)
}

colnames(log_lik)[1] = "subID"
log_lik_M1wf = log_lik

# M2wfOut
log_lik_raw_m2out = log_lik_raw[,c("sub", "trial", "M2wfOut_logLik")] 

log_lik_raw_m2out$sub = rep(sublist, each = Ntrial_long)
log_lik = data.frame()
for (sub in sublist) {
  this_sub = log_lik_raw_m2out[log_lik_raw_m2out$sub == sub,]
  this_sub = this_sub[order(this_sub$trial),]
  this_sub$trialNr = rep(seq(1:Ntrial), times = Nblock)
  this_sub$trial = NULL
  this_sub$blockNr = rep(1:Nblock, each = Ntrial)
  log_lik = rbind(log_lik, this_sub)
}

colnames(log_lik)[1] = "subID"
log_lik_M2wfOut = log_lik

# M2wfDec
log_lik_raw_m2dec = log_lik_raw[,c("sub", "trial", "M2wfDec_logLik")] 

log_lik_raw_m2dec$sub = rep(sublist, each = Ntrial_long)
log_lik = data.frame()
for (sub in sublist) {
  this_sub = log_lik_raw_m2dec[log_lik_raw_m2out$sub == sub,]
  this_sub = this_sub[order(this_sub$trial),]
  this_sub$trialNr = rep(seq(1:Ntrial), times = Nblock)
  this_sub$trial = NULL
  this_sub$blockNr = rep(1:Nblock, each = Ntrial)
  log_lik = rbind(log_lik, this_sub)
}

colnames(log_lik)[1] = "subID"
log_lik_M2wfDec = log_lik



tmp1 = merge(log_lik_M1wf, log_lik_M2wfOut, by = c("subID", "trialNr", "blockNr"))
df = merge(tmp1, log_lik_M2wfDec, by = c("subID", "trialNr", "blockNr"))

#---- Compute WAIC depending on the trial

WAIC_over_trials = data.frame()

for (trial in 1:Ntrial){
  this_trial_M1wf = df[df$trialNr == trial, c('subID', 'blockNr', 'M1wf_logLik')]
  this_trial_M1wf_wide = acast(this_trial_M1wf, blockNr ~ subID)
  this_trial_waic_M1wf <- waic(this_trial_M1wf_wide)
  this_waic_M1wf = this_trial_waic_M1wf[["waic"]]

  this_trial_M2wfOut = df[df$trialNr == trial, c('subID', 'blockNr', 'M2wfOut_logLik')]
  this_trial_M2wfOut_wide = acast(this_trial_M2wfOut, blockNr ~ subID)
  this_trial_waic_M2wfOut <- waic(this_trial_M2wfOut_wide)
  this_waic_M2wfOut = this_trial_waic_M2wfOut[["waic"]]
  
  this_trial_M2wfDec = df[df$trialNr == trial, c('subID', 'blockNr', 'M2wfDec_logLik')]
  this_trial_M2wfDec_wide = acast(this_trial_M2wfDec, blockNr ~ subID)
  this_trial_waic_M2wfDec <- waic(this_trial_M2wfDec_wide)
  this_waic_M2wfDec = this_trial_waic_M2wfDec[["waic"]]
  
  this_trial_waic = cbind(this_waic_M1wf,this_waic_M2wfOut, this_waic_M2wfDec)
  WAIC_over_trials = rbind(WAIC_over_trials, this_trial_waic)
}


names(WAIC_over_trials) = c("M1wf", "M2wfOut", "M2wfDec")
WAIC_over_trials$trialNr = seq(1:Ntrial)





