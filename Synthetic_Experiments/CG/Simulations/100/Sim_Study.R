# queue jobs on STORM -----------------------------------------------------

l <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm

# beta_0= 0.1 --- 50/50 ----------------------------------------

# n = 1000 ----------------------------------------------------------------
if (l==1){
library(doParallel)
library(doRNG)
#setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulate_Data/50/Sims_data")
data_zero = readRDS("data_50_1000.RDS")
cl <- makeCluster(40) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            #setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("BAPLA_Functions.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 100; N=1000;
                                            max_iter = 1000; tol=1e-5; 
                                            #setwd("~/Downloads/BAPLA/Simulation_Study/CG/Tuning/50")
                                            lams_zero <- readRDS("lams_50_1000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat, beta_alt = beta_alt)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
#setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50")
saveRDS(out, "out_50_1000.RDS")
}

# n = 2000
if (l==2){
library(doParallel)
library(doRNG)
##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulate_Data/50/Sims_data")
data_zero = readRDS("data_50_2000.RDS")
cl <- makeCluster(40) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            ##setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("BAPLA_Functions.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 100; N=2000;
                                            max_iter = 1000; tol=1e-5; 
                                            ##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Tuning/50")
                                            lams_zero <- readRDS("lams_50_2000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat,beta_alt = beta_alt)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start)
##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50")
saveRDS(out, "out_50_2000.RDS")
}

#n = 5000
if (l==3){
library(doParallel)
library(doRNG)
##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulate_Data/50/Sims_data")
data_zero = readRDS("data_50_5000.RDS")
cl <- makeCluster(40) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            ##setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("BAPLA_Functions.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 100; N=5000;
                                            max_iter = 1000; tol=1e-5; 
                                            ##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Tuning/50")
                                            lams_zero <- readRDS("lams_50_5000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat,beta_alt = beta_alt)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50")
saveRDS(out, "out_50_5000.RDS")
}


#n = 10000
if (l==4){
library(doParallel)
library(doRNG)
##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulate_Data/50/Sims_data")
data_zero = readRDS("data_50_10000.RDS")
cl <- makeCluster(40) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            ##setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("BAPLA_Functions.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 100; N=10000;
                                            max_iter = 1000; tol=1e-5; 
                                            ##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Tuning/50")
                                            lams_zero <- readRDS("lams_50_10000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat,beta_alt = beta_alt)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50")
saveRDS(out, "out_50_10000.RDS")
}

# beta_0= -2.6 (old zero inflated) ----------------------------------------

#n = 1000
if (l==5){
library(doParallel)
library(doRNG)
##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulate_Data/50/Sims_data")
data_zero = readRDS("data_zero_1000.RDS")
cl <- makeCluster(40) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            ##setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("BAPLA_Functions.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 100; N=1000;
                                            max_iter = 1000; tol=1e-5; 
                                            ##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Tuning/50")
                                            lams_zero <- readRDS("lams_zero_1000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat,beta_alt = beta_alt)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50")
saveRDS(out, "out_zero_1000.RDS")
}

# n = 2000
if (l==6){
library(doParallel)
library(doRNG)
##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulate_Data/50/Sims_data")
data_zero = readRDS("data_zero_2000.RDS")
cl <- makeCluster(40) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            ##setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("BAPLA_Functions.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 100; N=2000;
                                            max_iter = 1000; tol=1e-5; 
                                            ##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Tuning/50")
                                            lams_zero <- readRDS("lams_zero_2000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat,beta_alt = beta_alt)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50")
saveRDS(out, "out_zero_2000.RDS")
}

# n = 5000
if (l==7){
library(doParallel)
library(doRNG)
##setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulate_Data/50/Sims_data")
data_zero = readRDS("data_zero_5000.RDS")
cl <- makeCluster(40) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            ##setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("BAPLA_Functions.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 100; N=5000;
                                            max_iter = 1000; tol=1e-5; 
                                            #setwd("~/Downloads/BAPLA/Simulation_Study/CG/Tuning/50")
                                            lams_zero <- readRDS("lams_zero_5000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat,beta_alt = beta_alt)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
#setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50")
saveRDS(out, "out_zero_5000.RDS")
}

# n = 10,000
if (l==8){
library(doParallel)
library(doRNG)
#setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulate_Data/50/Sims_data")
data_zero = readRDS("data_zero_10000.RDS")
cl <- makeCluster(40) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            #setwd("~/Downloads/BAPLA/Simulation_Study")
                                            source("BAPLA_Functions.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 100; N=10000;
                                            max_iter = 1000; tol=1e-5; 
                                            #setwd("~/Downloads/BAPLA/Simulation_Study/CG/Tuning/50")
                                            lams_zero <- readRDS("lams_zero_10000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat,beta_alt = beta_alt)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
#setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50")
saveRDS(out, "out_zero_10000.RDS")
}