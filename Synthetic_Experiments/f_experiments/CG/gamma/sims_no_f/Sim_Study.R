
# n = 1000 ----------------------------------------------------------------

library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/Simulate_Data/Sims_data")
data_zero = readRDS("data_50_1000.RDS")
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            setwd("~/Downloads/BAPLA/Synthetic_Experiments")
                                            source("BAPLA_Functions.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 10; N=1000;
                                            max_iter = 1000; tol=1e-5; 
                                            
                                            
                                            setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/tuning_no_f/lambdas")
                                            lams_zero = readRDS(paste0("lams_",N, ".RDS"))
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit_no_f(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                          
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
time = (end-start)
print(time)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/sims_no_f/times")
saveRDS(time, "time_1000.RDS")
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/sims_no_f/results")
saveRDS(out, "out_1000.RDS")


# n = 2000
library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/Simulate_Data/Sims_data")
data_zero = readRDS("data_50_2000.RDS")
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            setwd("~/Downloads/BAPLA/Synthetic_Experiments")
                                            source("BAPLA_Functions.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 10; N=2000;
                                            max_iter = 1000; tol=1e-5; 
                                            
                                            setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/tuning_no_f/lambdas")
                                            lams_zero = readRDS(paste0("lams_",N, ".RDS"))
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit_no_f(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                           
                                           
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
time = (end-start)
print(time)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/sims_no_f/times")
saveRDS(time,  "time_2000.RDS")
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/sims_no_f/results")
saveRDS(out, "out_2000.RDS")


#n = 5000

library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/Simulate_Data/Sims_data")
data_zero = readRDS("data_50_5000.RDS")
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            setwd("~/Downloads/BAPLA/Synthetic_Experiments")
                                            source("BAPLA_Functions.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 10; N=5000;
                                            max_iter = 1000; tol=1e-5; 
                                            
                                            setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/tuning_no_f/lambdas")
                                            lams_zero = readRDS(paste0("lams_", N, ".RDS"))
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit_no_f(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
time = (end-start)
print(time)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/sims_no_f/times")
saveRDS(time, "time_5000.RDS")
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/sims_no_f/results")
saveRDS(out, "out_5000.RDS")



#n = 10000

library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/Simulate_Data/Sims_data")
data_zero = readRDS("data_50_10000.RDS")
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            setwd("~/Downloads/BAPLA/Synthetic_Experiments")
                                            source("BAPLA_Functions.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 10; N=10000;
                                            max_iter = 1000; tol=1e-5; 
                                            
                                            setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/tuning_no_f/lambdas")
                                            lams_zero = readRDS(paste0("lams_",N, ".RDS"))
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit_no_f(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
time = (end-start)
print(time)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/sims_no_f/times")
saveRDS(time, "time_10000.RDS")
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/sims_no_f/results")
saveRDS(out, "out_10000.RDS")

