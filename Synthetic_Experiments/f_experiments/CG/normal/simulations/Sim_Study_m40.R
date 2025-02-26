# Simulations for m = 40 ---------------------------------------------------

m = 40

# n = 1000 ----------------------------------------------------------------

library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Sims_data")
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
                                            
                                            m = 40
                                            
                                            setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/tuning/smaller_lams/lambdas")
                                            lams_zero = readRDS(paste0("lams_", m, "_",N, ".RDS"))
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat, beta_alt = beta_alt)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
time = (end-start)
print(time)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/times")
saveRDS(time, paste0("m_",m, "time_1000.RDS"))
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/results")
saveRDS(out, paste0("m_",m,"out_1000.RDS"))


# n = 2000
library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Sims_data")
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
                                            m = 40
                                            setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/tuning/smaller_lams/lambdas")
                                            lams_zero = readRDS(paste0("lams_", m, "_",N, ".RDS"))
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat,beta_alt = beta_alt)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
time = (end-start)
print(time)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/times")
saveRDS(time, paste0("m_",m, "time_2000.RDS"))
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/results")
saveRDS(out, paste0("m_",m,"out_2000.RDS"))


#n = 5000

library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Sims_data")
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
                                            m = 40
                                            setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/tuning/smaller_lams/lambdas")
                                            lams_zero = readRDS(paste0("lams_", m, "_",N, ".RDS"))
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat,beta_alt = beta_alt)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
time = (end-start)
print(time)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/times")
saveRDS(time, paste0("m_",m, "time_5000.RDS"))
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/results")
saveRDS(out, paste0("m_",m,"out_5000.RDS"))



#n = 10000

library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Sims_data")
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
                                            m = 40
                                            setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/tuning/smaller_lams/lambdas")
                                            lams_zero = readRDS(paste0("lams_", m, "_",N, ".RDS"))
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat,beta_alt = beta_alt)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
time = (end-start)
print(time)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/times")
saveRDS(time, paste0("m_",m, "time_10000.RDS"))
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/results")
saveRDS(out, paste0("m_",m,"out_10000.RDS"))

