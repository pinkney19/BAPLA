# queue jobs on STORM -----------------------------------------------------
l <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm

# Load Functions ----------------------------------------------------------

# Load Data ---------------------------------------------------------------

left_data <- readRDS("~/Downloads/BAPLA/Real_Data/Data/estimation/left.RDS")
right_data <- readRDS("~/Downloads/BAPLA/Real_Data/Data/estimation/right.RDS")


# Estimation --------------------------------------------------------------

source("Functions_data.R")
# library(splines)
# res = bapla_single_unit_trial(right_data, 8, 50, 1e-5, rep(10,74), 6)
# plot(seq(-0.2,0.4, length.out=  length(res$f)) , res$f, ylab = "Firing rate", xlab = "Time (s)")


# parallelised fitting ----------------------------------------------------


# Left trials (first half) -------------------------------------------------------------

library(doParallel)
library(doRNG)
library(pracma) #for logspace
library(purrr)
lambdas = logspace(log10(1),log10(150),100)
units = seq(1, 74)
grid = expand.grid( lams = lambdas[1:50], neuron = units)
list_of_pairs <- split(grid, seq(nrow(grid)))
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=list_of_pairs, .packages = c("purrr", "splines", 
                                          "LaplacesDemon")) %dopar%{
                                            setwd("~/Downloads/BAPLA/Real_Data")
                                            source("Functions_data.R")
                                            
                                            
                                            unit_id = as.numeric(k[2])
                                            lam = as.numeric(k[1])
                                            
                                            m = 6
                                            
                                            # Estimate coefficients with own function
                                            res = bapla_single_unit_trial(left_data, unit_id, 1000, 1e-5, rep(lam,74), m)
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
setwd("~/Downloads/BAPLA/Real_Data/R")
saveRDS(out, "est_left1.RDS")


# second half -------------------------------------------------------------

# Left trials (first half) -------------------------------------------------------------

library(doParallel)
library(doRNG)
library(pracma) #for logspace
lambdas = logspace(log10(1),log10(150),100)
units = seq(1, 74)
grid = expand.grid( lams = lambdas[51:100], neuron = units)
list_of_pairs <- split(grid, seq(nrow(grid)))
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=list_of_pairs, .packages = c("purrr", "splines", 
                                              "LaplacesDemon")) %dopar%{
                                                setwd("~/Downloads/BAPLA/Real_Data")
                                                source("Functions_data.R")
                                                
                                                
                                                unit_id = as.numeric(k[2])
                                                lam = as.numeric(k[1])
                                                
                                                m = 6
                                                
                                                # Estimate coefficients with own function
                                                res = bapla_single_unit_trial(left_data, unit_id, 1000, 1e-5, rep(lam,74), m)
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
setwd("~/Downloads/BAPLA/Real_Data/R")
saveRDS(out, "est_left2.RDS")





# right -------------------------------------------------------------------

library(doParallel)
library(doRNG)
library(pracma) #for logspace
lambdas = logspace(log10(1),log10(150),100)
units = seq(1, 74)
grid = expand.grid( lams = lambdas[1:50], neuron = units)
list_of_pairs <- split(grid, seq(nrow(grid)))
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=list_of_pairs, .packages = c("purrr", "splines", 
                                              "LaplacesDemon")) %dopar%{
                                                setwd("~/Downloads/BAPLA/Real_Data")
                                                source("Functions_data.R")
                                                
                                                
                                                unit_id = as.numeric(k[2])
                                                lam = as.numeric(k[1])
                                                
                                                m = 6
                                                
                                                # Estimate coefficients with own function
                                                res = bapla_single_unit_trial(right_data, unit_id, 1000, 1e-5, rep(lam,74), m)
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
setwd("~/Downloads/BAPLA/Real_Data/R")
saveRDS(out, "est_right1.RDS")



# second half -------------------------------------------------------------

library(doParallel)
library(doRNG)
library(pracma) #for logspace
lambdas = logspace(log10(1),log10(150),100)
units = seq(1, 74)
grid = expand.grid( lams = lambdas[51:100], neuron = units)
list_of_pairs <- split(grid, seq(nrow(grid)))
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
out <- foreach(k=list_of_pairs, .packages = c("purrr", "splines", 
                                              "LaplacesDemon")) %dopar%{
                                                setwd("~/Downloads/BAPLA/Real_Data")
                                                source("Functions_data.R")
                                                
                                                
                                                unit_id = as.numeric(k[2])
                                                lam = as.numeric(k[1])
                                                
                                                m = 6
                                                
                                                # Estimate coefficients with own function
                                                res = bapla_single_unit_trial(right_data, unit_id, 1000, 1e-5, rep(lam,74), m)
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
setwd("~/Downloads/BAPLA/Real_Data/R")
saveRDS(out, "est_right2.RDS")







#beta_hat = map(out, 1)



# #sim_data <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/Simulate_Data/10/Tuning_data/data_50_tuning_1000.RDS")
# beta_0 = NULL; f = list(); f = rep(list(f), 74); 
# beta = matrix(NA, nrow = 74, ncol = 74);
# iter = NULL;
# for(i in 1:74){
#   print(i)
#   res = bapla_single_unit_trial(left_data, i, 1000, 1e-5, rep(30,74), 6)
#   beta[i,] = res$beta
#   f[[i]] = res$f
#   beta_0[i] = res$beta0
#   iter[i] = res$iter
# }
