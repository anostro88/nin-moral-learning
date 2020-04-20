library("rstanarm")
library("bayesplot")
library("loo")
library("pROC")
library("shinystan")
library("rstan")
library("ggplot2")

#Load the fitted model
probe_folder = "fitting_models/Probe_study"
# Select the saved model and change accordingly
load(paste0(probe_folder, "/fitted_models/fit_M1wf.RData"))
fit_name = fit_M1wf
output_name = paste0(probe_folder,"/fitted_models/", fit_name@model_name)

if (fit_name@model_name == "20T_M1wf_F-AF"){ 
  M_pars_name = c("A", "tau", "wf")
  M_mr_name = c("tbtPE", "tbtev1", "tbtev2","tbtevc")
  } else{
    M_pars_name = c("A", "B", "tau", "wf")
    M_mr_name = c("tbtPEM", "tbtPES", "tbtevM", "tbtevS", "tbtbothevM", "tbtbothevS" )
   } 

#FOR MO
#M_pars_name = c("tau")
#M_mr_name = c("tbtPE", "tbtev1", "tbtev2")


##### getting the individual and group parameters of the model (df_ind_pars, df_group_pars) ####
model_summary = summary(fit_name)
df = model_summary[["summary"]]
df_pars_raw <- df[grepl(paste(M_pars_name, collapse="|"), row.names(df)), ] # select parameters
par_pr <- grep(pattern = "_pr" ,row.names(df_pars_raw)) # select par before phi app
df_clean = df_pars_raw[-par_pr,]  # keep only par after phi app
df_mr <- df[grepl(paste(M_mr_name, collapse="|"), row.names(df)), ] # select model regressors
df_mean = data.frame(df_clean[,1]) # take only first column of df (mean values)
list_par = data.frame(gsub("\\[|\\]|[0-9]", '', row.names(df_mean)))
df_pars = cbind(df_mean, list_par)
names(df_pars) = c("mean", "pars")

##### Create df for each parameter (df_par_.../df_mu_par_... ) ####
for (i in 1:length(M_pars_name)) {
  IndPar = M_pars_name
  GroupPar = paste0("mu_", M_pars_name)
  assign(paste0("df_par_",IndPar[i],  "_", fit_name@model_name), df_pars[df_pars$pars==IndPar[i],1])
  print(assign(paste0("df_par_",GroupPar[i],  "_", fit_name@model_name), df_pars[df_pars$pars==GroupPar[i],1]))
}

##### Combine Parameters and write out ####
df_IndPars = as.data.frame(mget(ls(pattern = paste(paste0("df_par_", IndPar), collapse="|")), .GlobalEnv)) # get all the df with "selfPain" in the names as a list from the env
row.names(df_IndPars) = row.names(df_IndPars) = Sublist[,1]
write.csv(df_IndPars, paste0(output_name, "_IndPars.csv"))
df_GroupPars = as.data.frame(mget(ls(pattern = paste(paste0("df_par_", GroupPar), collapse="|")), .GlobalEnv)) # get all the df with "selfPain" in the names as a list from the env
write.csv(df_GroupPars, paste0(output_name, "_GroupPars.csv"))
write.csv(df, paste0(output_name, "_summary.csv") )


##### Display GROUP parameters ####
posterior <- as.array(fit_name)
dim(posterior)
color_scheme_set("pink") #M1: pink #M2Out:blue,  M2Dec:brightblue, M0:yellow

par_1 = c("mu_A","mu_wf")  #"mu_B"
#par_1 = c("mu_A","mu_B", "mu_wf")  #"mu_B"
par_2 = c("mu_tau") 

mcmc_areas(posterior,  
           pars = par_1, 
           prob = 0.8, # 80% intervals
           prob_outer = 0.99, # 90%
           point_est = "median")

pars_name_all = row.names(df_mean) 
pars_name = pars_name_all[!is.element(pars_name_all, GroupPar)]
pars = list()

for (par in M_pars_name) {
  pars[[par]] = pars_name[grep(par, pars_name)]
}

color_scheme_set("pink")
mcmc_areas(posterior,  
           pars = pars$A, #change here the parameters to look at
           prob = 0.8, # 80% intervals
           prob_outer = 0.9, # 90%
           point_est = "median")

##### FUNCTIONS ####
# Function for Calculating ROC curve for true_y vs y_pred
ROC_PPC = function(fit) {
  y_pred <- rstan::extract(fit, "y_pred") # extract y_pred
  y_pred_mean <<- apply(y_pred[["y_pred"]],c(2,3), mean) # average of MCMC samples ---> dim(y_pred_mean) 27 (subjects) x 60 (trials)
  # true data for each subject
  true_y <<- choice
  # Calculate the ROC curve
  category = true_y -1
  prediction = y_pred_mean -1
  # Calculate the ROC curve
  roc_obj <- roc(category, prediction)
  auc(roc_obj)
  print(auc(roc_obj))
}  
#### ROC- Posterior Predictive Checks ####
ROC_M  = ROC_PPC(fit_name) 
  
#### LOOIC ####
log_lik = extract_log_lik(fit_name, parameter_name = "log_lik_sub", merge_chains = TRUE) #log_lik
loo <- loo(log_lik)
print(loo) 



 