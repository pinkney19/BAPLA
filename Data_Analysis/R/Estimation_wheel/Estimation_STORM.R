# queue jobs on STORM -----------------------------------------------------
l <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm


# Load Data ---------------------------------------------------------------

left_data <- readRDS("left.RDS")
right_data <- readRDS("right.RDS")


# Estimation --------------------------------------------------------------


# Left trials (first half) -------------------------------------------------------------
if (l==1){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  library(purrr)
  lambdas = logspace(log10(10),log10(300),100)
  units = seq(1, 56)
  grid = expand.grid( lams = lambdas, neuron = units)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k=list_of_pairs, .packages = c("purrr", "splines", 
                                            "LaplacesDemon")) %dopar%{
                                              #setwd("~/Downloads/BAPLA/Real_Data")
                                              source("Functions_data.R")
                                              
                                              
                                              unit_id = as.numeric(k[2])
                                              lam = as.numeric(k[1])
                                              
                                              m = 6
                                              
                                              # Estimate coefficients with own function
                                              res = bapla_single_unit_trial(left_data, unit_id, 1000, 1e-5, rep(lam,56), m)
                                              beta_hat = res$beta
                                              beta_0_hat = res$beta0
                                              f_hat = res$f
                                              c_hat = res$c
                                              beta_alt = res$beta_0_alt
                                              iter=res$iter
                                              
                                              
                                              return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat, beta_alt = beta_alt, iter =iter)) 
                                              
                                            }
  
  end <-Sys.time() 
  stopCluster(cl) 
  print(end-start) 
  #setwd("~/Downloads/BAPLA/Real_Data/R")
  saveRDS(out, "est_left.RDS")
}


# right -------------------------------------------------------------------
if (l==2){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(log10(10),log10(300),100)
  units = seq(1, 56)
  grid = expand.grid( lams = lambdas, neuron = units)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k=list_of_pairs, .packages = c("purrr", "splines", 
                                                "LaplacesDemon")) %dopar%{
                                                  #setwd("~/Downloads/BAPLA/Real_Data")
                                                  source("Functions_data.R")
                                                  
                                                  
                                                  unit_id = as.numeric(k[2])
                                                  lam = as.numeric(k[1])
                                                  
                                                  m = 6
                                                  
                                                  # Estimate coefficients with own function
                                                  res = bapla_single_unit_trial(right_data, unit_id, 1000, 1e-5, rep(lam,56), m)
                                                  beta_hat = res$beta
                                                  beta_0_hat = res$beta0
                                                  f_hat = res$f
                                                  c_hat = res$c
                                                  beta_alt = res$beta_0_alt
                                                  iter = res$iter
                                                  
                                                  return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat, beta_alt = beta_alt, iter = iter)) 
                                                  
                                                }
  
  end <-Sys.time() 
  stopCluster(cl) 
  print(end-start) 
  #setwd("~/Downloads/BAPLA/Real_Data/R")
  saveRDS(out, "est_right.RDS")
}
  

