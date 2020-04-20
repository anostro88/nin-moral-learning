#@@@@@@@@@@@@ Called by SuperScript_Pref_tiers_y_pred.R @@@@@@@@@@@@@@@@@@@@@

# M1wf
m1wf = m1wf
y_pred = data.frame(m1wf[grep("y_pred", m1wf$X), c(1:2)])

y_pred_sep = data.frame()

for (name in y_pred$X){
  temp = sapply(strsplit(name,"\\["), `[`, 2)
  temp = sapply(strsplit(temp,"\\]"), `[`, 1)
  temp = strsplit(temp,",")[[1]]
  this_y_pre = t(data.frame(temp))
  this_y_pre_mean = cbind(this_y_pre, name)
  y_pred_sep = rbind(y_pred_sep, this_y_pre_mean)
}

y_pred_fin = cbind(y_pred, y_pred_sep)
y_pred_fin$X = NULL

names(y_pred_fin) = c("y_pred", "sub", "trial")
y_pred_m1wf = y_pred_fin[c("sub", "trial","y_pred")]



# M2wf_OUT
m2wf_out = m2wf_out
y_pred = data.frame(m2wf_out[grep("y_pred", m2wf_out$X), c(1:2)])

y_pred_sep = data.frame()

for (name in y_pred$X){
  temp = sapply(strsplit(name,"\\["), `[`, 2)
  temp = sapply(strsplit(temp,"\\]"), `[`, 1)
  temp = strsplit(temp,",")[[1]]
  this_y_pre = t(data.frame(temp))
  this_y_pre_mean = cbind(this_y_pre, name)
  y_pred_sep = rbind(y_pred_sep, this_y_pre_mean)
}

y_pred_fin = cbind(y_pred, y_pred_sep)
y_pred_fin$X = NULL

names(y_pred_fin) = c("y_pred_out", "sub", "trial")
y_pred_m2wf_out = y_pred_fin[c("sub", "trial","y_pred_out")]

# M2wf_DEC
m2wf_dec = m2wf_dec
y_pred = data.frame(m2wf_dec[grep("y_pred", m2wf_dec$X), c(1:2)])

y_pred_sep = data.frame()

for (name in y_pred$X){
  temp = sapply(strsplit(name,"\\["), `[`, 2)
  temp = sapply(strsplit(temp,"\\]"), `[`, 1)
  temp = strsplit(temp,",")[[1]]
  this_y_pre = t(data.frame(temp))
  this_y_pre_mean = cbind(this_y_pre, name)
  y_pred_sep = rbind(y_pred_sep, this_y_pre_mean)
}

y_pred_fin = cbind(y_pred, y_pred_sep)
y_pred_fin$X = NULL

names(y_pred_fin) = c("y_pred_dec", "sub", "trial")
y_pred_m2wf_dec = y_pred_fin[c("sub", "trial","y_pred_dec")]


#-------MERGE THEM ALL

tmp1 = merge(y_pred_m1wf, y_pred_m2wf_out, by=c("sub", "trial"))
tmp2 = merge(tmp1, y_pred_m2wf_dec, by = c("sub", "trial"))
names(tmp2) = c("sub","trial","M1wf_y_pred","M2wf_y_pred_out", "M2wf_y_pred_dec")
tmp2$sub <- as.numeric(tmp2$sub)
tmp2$trial <- as.numeric(tmp2$trial)
# preprocess the y pred
ypred = tmp2
names(ypred)[1]= "subID"
Nsub = nrow(sub_id_map)

for (sub in 1:Nsub){
  ypred[ypred$subID == sub, "subID"] = sub_id_map[sub, names(sub_id_map)[1]]
}

y_pred_recoded = ypred[order(ypred$subID, ypred$trial), ]
y_pred_recoded$trialNr = rep(c(1:Ntrial), nrow(y_pred_recoded) / Ntrial)
y_pred_recoded$blockNr = rep(c(1:Nblock), each = Ntrial)


y_pred_recoded = y_pred_recoded[, c("subID", "blockNr", "trialNr", "M1wf_y_pred", "M2wf_y_pred_out", "M2wf_y_pred_dec")]
y_pred_recoded$M1wf_y_pred = y_pred_recoded$M1wf_y_pred - 1 
y_pred_recoded$M2wf_y_pred_out = y_pred_recoded$M2wf_y_pred_out - 1 
y_pred_recoded$M2wf_y_pred_dec = y_pred_recoded$M2wf_y_pred_dec - 1 

names(y_pred_recoded) = c("subID", "blockNr", "trialNr", "y_pred_M1", "y_pred_M2_outcome", "y_pred_M2_decision")
