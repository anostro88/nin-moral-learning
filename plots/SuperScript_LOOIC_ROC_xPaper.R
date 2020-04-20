library(pROC)
library(reshape2)
library(ggplot2)
library(rstanarm)
library(bayesplot)
library(loo)

# Set path to the location of the script
rm(list=ls())

Ntrial = 20
Ntrial_long = 120
Nblock = 6
sub_id_map = read.csv("data/sublist.csv", stringsAsFactors = F)
choice = read.csv("data/data_vertical.csv", stringsAsFactors = F)
m1wf = read.csv("data/20T_M1wf_F-AF_summary.csv", stringsAsFactors = F)
m2wf_out = read.csv("data/20T_M2wf_wfOut_F-AF_summary.csv", stringsAsFactors = F)
m2wf_dec = read.csv("data/20T_M2wf_wfDec_F-AF_summary.csv", stringsAsFactors = F)

##### LogLikelihood and LOOIC #####
source("get_all_LogLik.R")
log_lik_raw = tmp2
source("compute_LOOIC_11th_trial.R")
df = LOOIC_over_trials
LOOIC_data_long <- melt(df, id="trialNr")  # convert to long format

LOOIC_plot = ggplot(data=LOOIC_data_long,aes(x=trialNr, y=value, colour=variable)) + 
  scale_color_manual(values = c("#a25079", "#033f73", "#79b0eb")) +
  xlab("trials") +
  ylab("LOOIC") +
  geom_line(size=1, linetype = 1) +
  scale_x_continuous("Trials", breaks=c(1:20))+
  theme_bw()

# + geom_smooth(se = F)+


##### WAIC #####
log_lik_raw = tmp2
source("compute_WAIC_11th_trial.R")
df = WAIC_over_trials
WAIC_data_long <- melt(df, id="trialNr")  # convert to long format

###### Ypred and ROC ####
choice$trialNr = rep(seq(1:Ntrial), nrow(choice) / Ntrial)
names(choice)[names(choice) == 'col_sub'] = 'subID'
choice = choice[order(choice$subID), ]
choice$choice = choice$choice - 1
choice$shockOutcome = NULL
choice$rewardOutcome = NULL
source("dataset3_eeg_computational_modeling/plot_tbt/get_all_y_pred.R")
df = merge(choice, y_pred_recoded, by = c("subID", "trialNr", "blockNr") )

AUC_overTrials = data.frame(trialNr = c(), m1_auc = c(), m2_auc = c())
for (t in 1:Ntrial){
  this_trial = df[df$trialNr == t, ]
  pROC_obj_m1 <- roc(this_trial$choice,this_trial$y_pred_M1)
  pROC_obj_m2Out <- roc(this_trial$choice,this_trial$y_pred_M2_outcome)
  pROC_obj_m2Dec <- roc(this_trial$choice,this_trial$y_pred_M2_decision)
  
  this_auc = data.frame(trialNr = t, 
                        m1_auc = auc(pROC_obj_m1), 
                        m2Out_auc = auc(pROC_obj_m2Out), 
                        m2Dec_auc = auc(pROC_obj_m2Dec))
  AUC_overTrials = rbind(AUC_overTrials,this_auc) 
}

df = AUC_overTrials
write.csv(df, "dataset3_eeg_computational_modeling/output/AUC_overTrials.csv", row.names = F)
ROC_data_long <- melt(df, id="trialNr")  # convert to long format

ROC_plot = ggplot(data=ROC_data_long,aes(x=trialNr, y=value, colour=variable)) + 
  scale_color_manual(values = c("#a25079", "#033f73", "#79b0eb")) +
  xlab("trials") +
  ylab("ROC-AUC") +
  geom_line(size=1, linetype = 1) +
  #+geom_smooth() +
  scale_x_continuous("Trials", breaks=c(1:20)) +
  theme_bw()


grid.arrange(LOOIC_plot, ROC_plot, ncol =1)

