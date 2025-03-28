# De-sparsified Lasso Procedure -----------------------------------------------

setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")

# load data

data_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/binned/left.RDS")
left_params <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/left_params.RDS")

beta_hat= left_params$beta_hat
beta_0 = unlist(left_params$beta_0_alt)
f_hat = left_params$fs

# Initialize parameters
p = 48; alpha=0.05;

# Get desparsified estimates and CIs
res = list();

for(i in 1:p){
  print(i)
  set.seed(123) #reproducibility
  res[[i]] = despars_lasso_trial(i, data_left, p, beta_0, beta_hat, f_hat, alpha)
}
setwd("~/Downloads/BAPLA/Data_Analysis/R/Inference")
saveRDS(res, "res_left.RDS")



# right -------------------------------------------------------------------

data_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/binned/right.RDS")
right_params <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/right_params.RDS")

beta_hat= right_params$beta_hat
beta_0 = unlist(right_params$beta_0_alt)
f_hat = right_params$fs

# Initialize parameters
p = 48; alpha=0.05;

# Get desparsified estimates and CIs
res = list();

for(i in 1:p){
  print(i)
  set.seed(1234) #reproducibility
  res[[i]] = despars_lasso_trial(i, data_right, p, beta_0, beta_hat, f_hat, alpha)
}
setwd("~/Downloads/BAPLA/Data_Analysis/R/Inference")
saveRDS(res, "res_right.RDS")






# queue jobs on STORM -----------------------------------------------------

#l <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm

# Left Trials -------------------------------------------------------------


library(doParallel)
library(doRNG)
library(glmnet)
# need to loop over each unit (i.e. dimension) 
unit_seq = seq(1,48)
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
res_left <- foreach(k=unit_seq, .packages = c("splines","LaplacesDemon", "purrr", "glmnet")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Data_Analysis/R")
  source("Functions_data.R")
  
  # load data
  
  data_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/binned/left.RDS")
  left_params <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/left_params.RDS")
  
  beta_hat= left_params$beta_hat
  beta_0 = unlist(left_params$beta_0_alt)
  f_hat = left_params$fs
  
  # Initialize parameters
  p = 48; alpha=0.05;
  
  unit_idx = k
  
  # Get desparsified estimates and CIs
  res = despars_lasso_trial(unit_idx, data_left, p, beta_0, beta_hat, f_hat, alpha)
  
  # return values of interests
  
  return(list(beta_deb = res$beta_deb, sigma = res$sigma, lower = res$ci_lower, upper = res$ci_upper, theta_lasso = res$theta_lasso2)) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 
print(end-start)
setwd("~/Downloads/BAPLA/Data_Analysis/R/Inference")
saveRDS(res_left, "out_left_desparsified.RDS")


# Right Trials -------------------------------------------------------------


library(doParallel)
library(doRNG)
library(glmnet)
# need to loop over each unit (i.e. dimension) 
unit_seq = seq(1,48)
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
res_right <- foreach(k=unit_seq, .packages = c("splines","LaplacesDemon", "purrr", "glmnet")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Data_Analysis/R")
  source("Functions_data.R")
  
  # load data
  
  data_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/binned/right.RDS")
  right_params <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/right_params.RDS")
  
  beta_hat= right_params$beta_hat
  beta_0 = unlist(right_params$beta_0_alt)
  f_hat = right_params$fs
  
  # Initialize parameters
  p = 48; alpha=0.05;
  
  unit_idx = k
  
  # Get desparsified estimates and CIs
  res = despars_lasso_trial(unit_idx, data_right, p, beta_0, beta_hat, f_hat, alpha)
  
  # return values of interests
  
  return(list(beta_deb = res$beta_deb, sigma = res$sigma, lower = res$ci_lower, upper = res$ci_upper, theta_lasso = res$theta_lasso2)) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 
print(end-start)
setwd("~/Downloads/BAPLA/Data_Analysis/R/Inference")
saveRDS(res_right, "out_right_desparsified.RDS")


