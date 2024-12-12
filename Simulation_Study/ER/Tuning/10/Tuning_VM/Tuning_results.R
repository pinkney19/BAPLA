# Load libraries ----------------------------------------------------------

library(purrr)
library(pracma)

setwd("~/Downloads/BAPLA/Simulation_Study")
source("Functions_BAPLA.R")


# Chain Graph ----------------------------------------------

p = 10; 
beta <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/beta_10.RDS")
lambdas = logspace(log10(10),log10(300),100)


# Load tuning results from STORM ----------------------------------------------
out_50_1000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM/out_50_1000.RDS")
out_50_2000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM/out_50_2000.RDS")
out_50_5000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM/out_50_5000.RDS")
out_50_10000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM/out_50_10000.RDS")

out_zero_1000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM/out_zero_1000.RDS")
out_zero_2000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM/out_zero_2000.RDS")
out_zero_5000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM/out_zero_5000.RDS")
out_zero_10000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM/out_zero_10000.RDS")




# eBIC --------------------------------------------------------------------

# load tuning data files

data_50_tuning_1000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Tuning_data/data_50_tuning_1000.RDS")
data_50_tuning_2000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Tuning_data/data_50_tuning_2000.RDS")
data_50_tuning_5000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Tuning_data/data_50_tuning_5000.RDS")
data_50_tuning_10000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Tuning_data/data_50_tuning_10000.RDS")


data_zero_tuning_1000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Tuning_data/data_zero_tuning_1000.RDS")
data_zero_tuning_2000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Tuning_data/data_zero_tuning_2000.RDS")
data_zero_tuning_5000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Tuning_data/data_zero_tuning_5000.RDS")
data_zero_tuning_10000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Tuning_data/data_zero_tuning_10000.RDS")

# Format Results ----------------------------------------------------------

#N_samp = Number of training samples
N_samp = 10;  m=10;
library(boot) #inv.logit
# select data of interest - i.e. data relating to each value of lambda
get_ebic_vals = function(out_storm, N_samp, t_end, m,lambdas, data, unit_id, gamma, p, threshold){
  av_BIC = NULL;av_AIC=NULL;av_eBIC=NULL;av_nn=NULL;av_nll=NULL;av_nll2 = NULL;
  any = rep(0, length(lambdas)); all = rep(0, length(lambdas))
  for(i in 1:length(lambdas)){
    #print(i)
    b = N_samp*i
    if(i==1){
      res = out_storm[1:b]
    }
    if(i!=1){
      res = out_storm[((b-N_samp)+1):b]
    }
    
    # get estimates of beta
    beta_hats = map(res, 1)
    # select coefficient vector for specified unit
    
    beta_vec = lapply(beta_hats, function(x) x[unit_id, ])
    
    # set values below threshold to zero
    beta_vec = lapply(beta_vec, function(x) ifelse(x < threshold, 0, x))
    
    # get intercept estimates
    beta_0 = map(res,2)
    # select those for specified unit
    beta_0 = unlist(map(beta_0, unit_id))
    
    # get f estimates
    c_hat = map(res, 4)
    # select for specified unit
    c_hat = map(c_hat, unit_id)
    phi <- bs(seq(1, t_end), df=m, intercept = F)
    f = lapply(c_hat, function(x) phi%*%as.matrix(x))
    
    # or 
    
    
    # calculate BIC for the 10 training samples
    BIC_vec = NULL; AIC_vec = NULL; eBIC_vec = NULL; nll2=NULL; nll=NULL; num_non_zeroes=NULL;check = NULL
    
    for(j in 1:length(beta_hats)){#length(beta_hats)){
      
      # select data of interest to the unit_id
      y = data[[j]][unit_id,][2:(t_end+1)] 
      X1 = data[[j]][,(1:t_end)] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
      X1 = t(X1)
      
      
      term1 = y * (beta_0[j] + X1 %*% beta_vec[[j]] + f[[j]])
      term2 = log(1 + exp(beta_0[j] + X1 %*% beta_vec[[j]] + f[[j]])) 
      nll[j] = sum(term1-term2) 
      
      p_prob = inv.logit(beta_0[j] + X1 %*% beta_vec[[j]] + f[[j]])
      nll2[j] = sum( y*(log(p_prob)) + ( (1-y)*(log(1-p_prob)) ))
      
      num_non_zeroes[j] = sum(beta_vec[[j]] != 0)
      pen_term = (num_non_zeroes[j]+1+(m))*log(length(y))
      eBIC_term = 2*gamma*log(p)* (num_non_zeroes[j]+1+(m))
      
      BIC_vec[j] = -2*(nll[j]) + pen_term #+ eBIC_term
      eBIC_vec[j] = BIC_vec[j] + eBIC_term
      AIC_vec[j] = -2*(nll[j]) + (2*(num_non_zeroes[j]+1+(m)))
      
      check[j] = pen_term > (2*(num_non_zeroes[j]+1+(m)))
    }
    # check if any are zeroes
    if(any(num_non_zeroes==0)){any[i] = 1}
    # check if all zeroes
    if(all(num_non_zeroes==0)){all[i] = 1}
    # get average BIC across the training samples
    av_BIC[i] = mean(BIC_vec)
    av_AIC[i] = mean(AIC_vec)
    av_eBIC[i] = mean(eBIC_vec)
    av_nn[i] = mean(num_non_zeroes)
    av_nll[i] = mean(nll)
    av_nll2[i] = mean(nll2)
  }
  return(list(BIC = av_BIC, AIC = av_AIC, eBIC = av_eBIC,  nn=av_nn, nll=av_nll, nll2 = av_nll2, any = any, all = all ))
}
library(splines)

res2 = get_ebic_vals(out_zero_10000, N_samp, t_end = 9999, m,lambdas, data_zero_tuning_10000,2, 0.5, 10, threshold = 1e-5)

par(mfrow=c(1,3), mar = c(5,4,4,2))
plot(lambdas, res2$BIC, type='l', main="BIC")
abline(v =lambdas[which.min(res2$BIC)], col="red")
plot(lambdas, res2$AIC, type='l', main="AIC")
abline(v =lambdas[which.min(res2$AIC)], col="red")
plot(lambdas, res2$eBIC, type='l', main="eBIC")
abline(v =lambdas[which.min(res2$eBIC)], col="red")

# For all units -----------------------------------------------------------
get_lambdas = function(out, N_samp, t_end, m, lambdas, data, p, title){
  lambdas_BIC = NULL; lambdas_eBIC = NULL; lambdas_AIC = NULL;
  out_AIC = NULL; out_BIC = NULL; out_eBIC = NULL;
  for(i in 1:p){
    print(i)
    res = get_ebic_vals(out, N_samp, t_end, m,lambdas, data,i, 0.5, p, threshold = 1e-5)
    idx = which(res$any==1)[1] #extract index of lambda for which all are zero
    #if(is.na(idx)){idx = length(lambdas)}
    #if (is.integer(idx) && length(idx) == 0){idx = length(lambdas)} # if not all betas are zero 
    lambdas_AIC[i] = lambdas[which.min(res$AIC)]
    lambdas_BIC[i] = lambdas[which.min(res$BIC)]
    lambdas_eBIC[i] = lambdas[which.min(res$eBIC)]
  }
  df = data.frame(cbind(lambdas_AIC, lambdas_BIC, lambdas_eBIC))
  names(df)=c("AIC", "BIC", "eBIC")
  boxplot(df, ylab=expression(lambda), main=title)
  return(df)
}
library(splines)
library(boot)
par(mfrow = c(1,4))
f1000 = get_lambdas(out_50_1000, N_samp, 999, m, lambdas, data_50_tuning_1000, p, "50 - 1000")
f2000 = get_lambdas(out_50_2000, N_samp, 1999, m, lambdas, data_50_tuning_2000, p, "50 - 2000")
f5000 = get_lambdas(out_50_5000, N_samp, 4999, m, lambdas, data_50_tuning_5000, p, "50 - 5000")
f10000 = get_lambdas(out_50_10000, N_samp, 9999, m, lambdas, data_50_tuning_10000, p, "50 - 10000")

z1000 = get_lambdas(out_zero_1000, N_samp, 999, m, lambdas, data_zero_tuning_1000, p, "zero - 1000")
z2000 = get_lambdas(out_zero_2000, N_samp, 1999, m, lambdas, data_zero_tuning_2000, p, "zero - 2000")
z5000 = get_lambdas(out_zero_5000, N_samp, 4999, m, lambdas, data_zero_tuning_5000, p, "zero - 5000")
z10000 = get_lambdas(out_zero_10000, N_samp, 9999, m, lambdas, data_zero_tuning_10000, p, "zero - 10000")



setwd("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM")
saveRDS(f1000$BIC, "lams_50_1000.RDS")
saveRDS(f2000$BIC, "lams_50_2000.RDS")
saveRDS(f5000$BIC, "lams_50_5000.RDS")
saveRDS(f10000$BIC, "lams_50_10000.RDS")

saveRDS(z1000$BIC, "lams_zero_1000.RDS")
saveRDS(z2000$BIC, "lams_zero_2000.RDS")
saveRDS(z5000$BIC, "lams_zero_5000.RDS")
saveRDS(z10000$BIC, "lams_zero_10000.RDS")

