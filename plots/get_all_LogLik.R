# M1wf
m1wf = m1wf

log_lik = data.frame(m1wf[grep("log_lik\\[", m1wf$X), c(1:2)])

log_lik_sep = data.frame()

for (name in log_lik$X){
  temp = sapply(strsplit(name,"\\["), `[`, 2)
  temp = sapply(strsplit(temp,"\\]"), `[`, 1)
  temp = strsplit(temp,",")[[1]]
  this_log_lik = t(data.frame(temp))
  this_log_lik_mean = cbind(this_log_lik, name)
  log_lik_sep = rbind(log_lik_sep, this_log_lik_mean)
}

log_lik_fin = cbind(log_lik, log_lik_sep)
log_lik_fin$X = NULL

names(log_lik_fin) = c("log_lik", "sub", "trial")
log_lik_m1wf = log_lik_fin[c("sub", "trial","log_lik")]

# M2wf_OUT
m2wf_out = m2wf_out
log_lik = data.frame(m2wf_out[grep("log_lik\\[", m2wf_out$X), c(1:2)])

log_lik_sep = data.frame()

for (name in log_lik$X){
  temp = sapply(strsplit(name,"\\["), `[`, 2)
  temp = sapply(strsplit(temp,"\\]"), `[`, 1)
  temp = strsplit(temp,",")[[1]]
  this_log_lik = t(data.frame(temp))
  this_log_lik_mean = cbind(this_log_lik, name)
  log_lik_sep = rbind(log_lik_sep, this_log_lik_mean)
}

log_lik_fin = cbind(log_lik, log_lik_sep)
log_lik_fin$X = NULL

names(log_lik_fin) = c("log_lik_M2Out", "sub", "trial")
log_lik_M2Out = log_lik_fin[c("sub", "trial","log_lik_M2Out")]



# M2wf_Dec
m2wf_dec = m2wf_dec
log_lik = data.frame(m2wf_dec[grep("log_lik\\[", m2wf_dec$X), c(1:2)])

log_lik_sep = data.frame()

for (name in log_lik$X){
  temp = sapply(strsplit(name,"\\["), `[`, 2)
  temp = sapply(strsplit(temp,"\\]"), `[`, 1)
  temp = strsplit(temp,",")[[1]]
  this_log_lik = t(data.frame(temp))
  this_log_lik_mean = cbind(this_log_lik, name)
  log_lik_sep = rbind(log_lik_sep, this_log_lik_mean)
}

log_lik_fin = cbind(log_lik, log_lik_sep)
log_lik_fin$X = NULL

names(log_lik_fin) = c("log_lik_M2Dec", "sub", "trial")
log_lik_M2Dec = log_lik_fin[c("sub", "trial","log_lik_M2Dec")]

#----- Merge M1 and M2

tmp1 = merge(log_lik_m1wf, log_lik_M2Out, by=c("sub", "trial"))
tmp2 = merge(tmp1, log_lik_M2Dec, by=c("sub", "trial"))
names(tmp2) = c("sub","trial","M1wf_logLik","M2wfOut_logLik", "M2wfDec_logLik")