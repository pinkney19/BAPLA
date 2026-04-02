# queue jobs on STORM -----------------------------------------------------
l <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm

# Simulations for m = 35 ---------------------------------------------------
if (l==1){
  m = 35
  
  # n = 1000 ----------------------------------------------------------------
  
  library(doParallel)
  library(doRNG) 
  setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Sims_data")
  data_zero = readRDS("data_50_1000.RDS")
  cl <- makeCluster(44) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                            "LaplacesDemon")) %dopar%{
                                              #setwd("~/Downloads/BAPLA/Synthetic_Experiments")
                                              
                                              source("BAPLA_Functions.R")
                                              
                                              # Initialize parameters
                                              
                                              p = 10; N=1000;
                                              max_iter = 1000; tol=1e-5; 
                                              
                                              
                                              m = 35
                                              
                                              #setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/tuning/smaller_lams/lambdas")
                                              lams_zero = readRDS(paste0("lams_", m, "_",N, ".RDS"))
                                              
                                              # Estimate coefficients with own function
                                              s = Sys.time()
                                              res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                              e = Sys.time()
                                              counter = e-s
                                              beta_hat = res$beta_hat
                                              beta_0_hat = res$beta0
                                              f_hat = res$f
                                              c_hat = res$c_hat
                                              beta_alt = res$beta_alt
                                              iter = res$iter
                                              r = res$r
                                              div = res$div
                                              c_r = res$c_r
                                              b_r = res$b_r
                                              
                                              return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, 
                                                          c_hat = c_hat, beta_alt = beta_alt, time = counter, iter = iter, 
                                                          r = r, div=div, c_r=c_r, b_r=b_r)) 
                                              
                                            }
  
  end <-Sys.time() 
  stopCluster(cl) 
  time = (end-start)
  print(time)
  saveRDS(time, paste0("m_",m, "time_1000.RDS"))
  saveRDS(out, paste0("m_",m,"out_1000.RDS"))
}

# n = 2000
if (l==2){
  m = 35
  library(doParallel)
  library(doRNG)
  
  data_zero = readRDS("data_50_2000.RDS")
  cl <- makeCluster(44) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                            "LaplacesDemon")) %dopar%{
                                            
                                              source("BAPLA_Functions.R")
                                              
                                              # Initialize parameters
                                              
                                              p = 10; N=2000;
                                              max_iter = 1000; tol=1e-5; 
                                              m = 35
                                            
                                              lams_zero = readRDS(paste0("lams_", m, "_",N, ".RDS"))
                                              
                                              # Estimate coefficients with own function
                                              s = Sys.time()
                                              res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                              e = Sys.time()
                                              counter = e-s
                                              beta_hat = res$beta_hat
                                              beta_0_hat = res$beta0
                                              f_hat = res$f
                                              c_hat = res$c_hat
                                              beta_alt = res$beta_alt
                                              iter = res$iter
                                              r = res$r
                                              div = res$div
                                              c_r = res$c_r
                                              b_r = res$b_r
                                              
                                              return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, 
                                                          c_hat = c_hat, beta_alt = beta_alt, time = counter, iter = iter, 
                                                          r = r, div=div, c_r=c_r, b_r=b_r)) 
                                            }
  
  end <-Sys.time() 
  stopCluster(cl) 
  time = (end-start)
  print(time)
  saveRDS(time, paste0("m_",m, "time_2000.RDS"))
  saveRDS(out, paste0("m_",m,"out_2000.RDS"))
}


if (l==3){
  #n = 5000
  m = 35
  library(doParallel)
  library(doRNG)
  
  data_zero = readRDS("data_50_5000.RDS")
  cl <- makeCluster(44) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                            "LaplacesDemon")) %dopar%{
                                             
                                              source("BAPLA_Functions.R")
                                              
                                              # Initialize parameters
                                              
                                              p = 10; N=5000;
                                              max_iter = 1000; tol=1e-5; 
                                              m = 35
                                            
                                              lams_zero = readRDS(paste0("lams_", m, "_",N, ".RDS"))
                                              
                                              # Estimate coefficients with own function
                                              s = Sys.time()
                                              res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                              e = Sys.time()
                                              counter = e-s
                                              beta_hat = res$beta_hat
                                              beta_0_hat = res$beta0
                                              f_hat = res$f
                                              c_hat = res$c_hat
                                              beta_alt = res$beta_alt
                                              iter = res$iter
                                              r = res$r
                                              div = res$div
                                              c_r = res$c_r
                                              b_r = res$b_r
                                              
                                              return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, 
                                                          c_hat = c_hat, beta_alt = beta_alt, time = counter, iter = iter, 
                                                          r = r, div=div, c_r=c_r, b_r=b_r)) 
                                            }
  
  end <-Sys.time() 
  stopCluster(cl) 
  time = (end-start)
  print(time)
  saveRDS(time, paste0("m_",m, "time_5000.RDS"))
  saveRDS(out, paste0("m_",m,"out_5000.RDS"))
}  

#n = 10000
if (l==4){
  library(doParallel)
  library(doRNG)
  m = 35
  data_zero = readRDS("data_50_10000.RDS")
  cl <- makeCluster(44) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                            "LaplacesDemon")) %dopar%{
                                             
                                              source("BAPLA_Functions.R")
                                              
                                              # Initialize parameters
                                              
                                              p = 10; N=10000;
                                              max_iter = 1000; tol=1e-5; 
                                              m = 35
                                             
                                              lams_zero = readRDS(paste0("lams_", m, "_",N, ".RDS"))
                                              
                                              # Estimate coefficients with own function
                                              s = Sys.time()
                                              res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                              e = Sys.time()
                                              counter = e-s
                                              beta_hat = res$beta_hat
                                              beta_0_hat = res$beta0
                                              f_hat = res$f
                                              c_hat = res$c_hat
                                              beta_alt = res$beta_alt
                                              iter = res$iter
                                              r = res$r
                                              div = res$div
                                              c_r = res$c_r
                                              b_r = res$b_r
                                              
                                              return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, 
                                                          c_hat = c_hat, beta_alt = beta_alt, time = counter, iter = iter, 
                                                          r = r, div=div, c_r=c_r, b_r=b_r)) 
                                            }
  
  end <-Sys.time() 
  stopCluster(cl) 
  time = (end-start)
  print(time)
  saveRDS(time, paste0("m_",m, "time_10000.RDS"))
  saveRDS(out, paste0("m_",m,"out_10000.RDS"))
}  
