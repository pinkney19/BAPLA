# Queue jobs on STORM 

l <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm

# n = 1000 ----------------------------------------------------------------
if (l==1){
library(doParallel)
library(doRNG)
#setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Sims_data")
data_zero = readRDS("data_50_1000.RDS")
# only do for 10 replications
data_zero = data_zero[1:10]
cl <- makeCluster(40) 
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
                                            #setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/10")
                                            lams_zero <- readRDS("lams_50_1000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            s = Sys.time()
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            e = Sys.time()
                                            counter = e-s
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            iter = res$iter
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, 
                                                        c_hat = c_hat, beta_alt = beta_alt, time = counter, iter = iter)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
#setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/10")
saveRDS(out, "out_1000.RDS")
}


# n = 2000
if (l==2){
library(doParallel)
library(doRNG)
#setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Sims_data")
data_zero = readRDS("data_50_2000.RDS")
data_zero  = data_zero[1:10]
cl <- makeCluster(40) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            #setwd("~/Downloads/BAPLA/Synthetic_Experiments")
                                            source("BAPLA_Functions.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 10; N=2000;
                                            max_iter = 1000; tol=1e-5; 
                                            #setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/10")
                                            lams_zero <- readRDS("lams_50_2000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            s = Sys.time()
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            e = Sys.time()
                                            counter = e-s
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            iter = res$iter
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, 
                                                        c_hat = c_hat,beta_alt = beta_alt, time = counter, iter = iter)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start)
#setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/10")
saveRDS(out, "out_2000.RDS")
}

#n = 5000
if (l==3){
library(doParallel)
library(doRNG)
#setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Sims_data")
data_zero = readRDS("data_50_5000.RDS")
data_zero = data_zero[1:10]
cl <- makeCluster(40) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            #setwd("~/Downloads/BAPLA/Synthetic_Experiments")
                                            source("BAPLA_Functions.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 10; N=5000;
                                            max_iter = 1000; tol=1e-5; 
                                            #setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/10")
                                            lams_zero <- readRDS("lams_50_5000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            s = Sys.time()
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            e = Sys.time()
                                            counter = e-s
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            iter = res$iter
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, 
                                                        c_hat = c_hat,beta_alt = beta_alt, time = counter, iter = iter)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
#setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/10")
saveRDS(out, "out_5000.RDS")
}


#n = 10000
if (l==4){
library(doParallel)
library(doRNG)
#setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Sims_data")
data_zero = readRDS("data_50_10000.RDS")
data_zero = data_zero[1:10]
cl <- makeCluster(40) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=data_zero, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            #setwd("~/Downloads/BAPLA/Synthetic_Experiments")
                                            source("BAPLA_Functions.R")
                                            
                                            # Initialize parameters
                                            
                                            p = 10; N=10000;
                                            max_iter = 1000; tol=1e-5; 
                                            #setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/10")
                                            lams_zero <- readRDS("lams_50_10000.RDS")
                                            m = 10
                                            
                                            # Estimate coefficients with own function
                                            s = Sys.time()
                                            res = Estimate_coefficients_logit(k, p, max_iter, tol, lams_zero, m)
                                            e  = Sys.time()
                                            counter = e-s
                                            beta_hat = res$beta_hat
                                            beta_0_hat = res$beta0
                                            f_hat = res$f_hat
                                            c_hat = res$c_hat
                                            beta_alt = res$beta_alt
                                            iter = res$iter
                                            
                                            return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, 
                                                        c_hat = c_hat,beta_alt = beta_alt, time= counter, iter = iter)) 
                                            
                                          }

end <-Sys.time() 
stopCluster(cl) 
print(end-start) 
#setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/10")
saveRDS(out, "out_10000.RDS")
}

