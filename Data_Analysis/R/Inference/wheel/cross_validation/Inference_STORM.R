# queue jobs on STORM -----------------------------------------------------

l <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm

# Left Trials -------------------------------------------------------------


library(doParallel)
library(doRNG)
library(glmnet)
# need to loop over each unit (i.e. dimension) 
unit_seq = seq(1,56)
cl <- makeCluster(44) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
res_vis_left <- foreach(k=unit_seq, .packages = c("splines","LaplacesDemon", "purrr", "glmnet")) %dopar%{
  
  #setwd("~/Downloads/BAPLA/Data_Analysis/R")
  source("Functions_data.R")
  
  # load data
  
  data_left <- readRDS("left.RDS")
  left_params <- readRDS("left_params.RDS")
  
  beta_hat= left_params$beta_hat
  beta_0 = unlist(left_params$beta_0_alt)
  f_hat = left_params$fs
  
  # Initialize parameters
  p = 56; alpha=0.05;
  
  unit_idx = k
  
  # Get desparsified estimates and CIs
  res = despars_lasso_trial_cv(unit_idx, data_left, p, beta_0, beta_hat, f_hat, alpha)
  
  # return values of interests
  
  return(list(beta_deb = res$beta_deb, sigma = res$sigma, lower = res$ci_lower, 
              upper = res$ci_upper, theta_lasso = res$theta_lasso2, rm_trials = res$rm_trials )) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 
print(end-start)
#setwd("~/Downloads/BAPLA/Data_Analysis/R/Inference")
saveRDS(res_vis_left, "vis_left_desparsified.RDS")


# Right Trials -------------------------------------------------------------


library(doParallel)
library(doRNG)
library(glmnet)
# need to loop over each unit (i.e. dimension) 
unit_seq = seq(1,56)
cl <- makeCluster(44) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
res_vis_right <- foreach(k=unit_seq, .packages = c("splines","LaplacesDemon", "purrr", "glmnet")) %dopar%{
  
  #setwd("~/Downloads/BAPLA/Data_Analysis/R")
  source("Functions_data.R")
  
  # load data
  
  data_right <- readRDS("right.RDS")
  right_params <- readRDS("right_params.RDS")
  
  beta_hat= right_params$beta_hat
  beta_0 = unlist(right_params$beta_0_alt)
  f_hat = right_params$fs
  
  # Initialize parameters
  p = 56; alpha=0.05;
  
  unit_idx = k
  
  # Get desparsified estimates and CIs
  res = despars_lasso_trial_cv(unit_idx, data_right, p, beta_0, beta_hat, f_hat, alpha)
  
  # return values of interests
  
  return(list(beta_deb = res$beta_deb, sigma = res$sigma, lower = res$ci_lower, upper = res$ci_upper, 
              theta_lasso = res$theta_lasso2, rm_trials = res$rm_trials)) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 
print(end-start)
#setwd("~/Downloads/BAPLA/Data_Analysis/R/Inference")
saveRDS(res_vis_right, "vis_right_desparsified.RDS")


