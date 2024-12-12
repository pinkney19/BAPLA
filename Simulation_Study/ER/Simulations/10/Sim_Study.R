
# beta_0=-2.6 / old naming of 50/50 ----------------------------------------

# n = 1000 ----------------------------------------------------------------

library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Sims_data")
data_zero = readRDS("data_50_1000.RDS")
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("Functions_BAPLA.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 10; N=1000;
                                            max_iter = 1000; tol=1e-5; 
                                            setwd("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM")
                                            lams_zero <- readRDS("lams_50_1000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulations/10")
saveRDS(out, "out_50_1000.RDS")


# n = 2000
library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Sims_data")
data_zero = readRDS("data_50_2000.RDS")
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("Functions_BAPLA.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 10; N=2000;
                                            max_iter = 1000; tol=1e-5; 
                                            setwd("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM")
                                            lams_zero <- readRDS("lams_50_2000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start)
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulations/10")
saveRDS(out, "out_50_2000.RDS")


#n = 5000

library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Sims_data")
data_zero = readRDS("data_50_5000.RDS")
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("Functions_BAPLA.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 10; N=5000;
                                            max_iter = 1000; tol=1e-5; 
                                            setwd("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM")
                                            lams_zero <- readRDS("lams_50_5000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulations/10")
saveRDS(out, "out_50_5000.RDS")



#n = 10000

library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Sims_data")
data_zero = readRDS("data_50_10000.RDS")
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("Functions_BAPLA.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 10; N=10000;
                                            max_iter = 1000; tol=1e-5; 
                                            setwd("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM")
                                            lams_zero <- readRDS("lams_50_10000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulations/10")
saveRDS(out, "out_50_10000.RDS")


# beta_0=-3.6 (old zero-inflated) -----------------------------------------

# n = 1000
library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Sims_data")
data_zero = readRDS("data_zero_1000.RDS")
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("Functions_BAPLA.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 10; N=1000;
                                            max_iter = 1000; tol=1e-5; 
                                            setwd("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM")
                                            lams_zero <- readRDS("lams_zero_1000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulations/10")
saveRDS(out, "out_zero_1000.RDS")

#n = 2000
library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Sims_data")
data_zero = readRDS("data_zero_2000.RDS")
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("Functions_BAPLA.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 10; N=2000;
                                            max_iter = 1000; tol=1e-5; 
                                            setwd("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM")
                                            lams_zero <- readRDS("lams_zero_2000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulations/10")
saveRDS(out, "out_zero_2000.RDS")

#n = 5000
library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Sims_data")
data_zero = readRDS("data_zero_5000.RDS")
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("Functions_BAPLA.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 10; N=5000;
                                            max_iter = 1000; tol=1e-5; 
                                            setwd("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM")
                                            lams_zero <- readRDS("lams_zero_5000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulations/10")
saveRDS(out, "out_zero_5000.RDS")


#n = 10,000
library(doParallel)
library(doRNG)
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulate_Data/10/Sims_data")
data_zero = readRDS("data_zero_10000.RDS")
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("Functions_BAPLA.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 10; N=10000;
                                            max_iter = 1000; tol=1e-5; 
                                            setwd("~/Downloads/BAPLA/Simulation_Study/ER/Tuning/10/Tuning_VM")
                                            lams_zero <- readRDS("lams_zero_10000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
setwd("~/Downloads/BAPLA/Simulation_Study/ER/Simulations/10")
saveRDS(out, "out_zero_10000.RDS")
