
# beta_0 = 0.1 (50/50) ---------------------------------------------------

# n = 1000

# load libraries
library(purrr)
library(splines)
# load results from stage 1
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/10")
out <- readRDS("out_50_1000.RDS")
# load data used to get stage 1 results
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulate_Data/10/Sims_data")
data <- readRDS("data_50_1000.RDS")
beta_hats = map(out, 1) #interaction matrix

# Parallelised refitting --------------------------------------------------

library(doParallel)
library(doRNG)
library(pracma) #for logspace
list_of_pairs <- Map(list, data, beta_hats)
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
refit <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments")
  source("BAPLA_Functions.R")
  
  # Initialize parameters
  
  p = 10; 
  max_iter = 1000; tol=1e-5; 
  m = 10
  
  res = Estimate_coefficients_refit(k[[1]], p, max_iter, tol, m, k[[2]])
  
  beta_hat = res$beta_hat
  beta_0_hat = res$beta0
  f_hat = res$f_hat
  c_hat = res$c_hat
  
  
  return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 
print(end-start)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Refit/10")
saveRDS(refit, "refit_50_1000.RDS")

 
# n = 2000

# load libraries
library(purrr)
library(splines)
# load results from stage 1
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/10")
out <- readRDS("out_50_2000.RDS")
# load data used to get stage 1 results
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulate_Data/10/Sims_data")
data <- readRDS("data_50_2000.RDS")
beta_hats = map(out, 1)

# Parallelised refitting --------------------------------------------------

library(doParallel)
library(doRNG)
library(pracma) #for logspace
list_of_pairs <- Map(list, data, beta_hats)
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
refit <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments")
  source("BAPLA_Functions.R")
  
  # Initialize parameters
  
  p = 10; 
  max_iter = 1000; tol=1e-5; 
  m = 10
  
  res = Estimate_coefficients_refit(k[[1]], p, max_iter, tol, m, k[[2]])
  
  beta_hat = res$beta_hat
  beta_0_hat = res$beta0
  f_hat = res$f_hat
  c_hat = res$c_hat
  
  
  return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 
print(end-start)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Refit/10")
saveRDS(refit, "refit_50_2000.RDS")


# n = 5000
# load libraries
library(purrr)
library(splines)
# load results from stage 1
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/10")
out <- readRDS("out_50_5000.RDS")
# load data used to get stage 1 results
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulate_Data/10/Sims_data")
data <- readRDS("data_50_5000.RDS")
beta_hats = map(out, 1)

# Parallelised refitting --------------------------------------------------

library(doParallel)
library(doRNG)
library(pracma) #for logspace
list_of_pairs <- Map(list, data, beta_hats)
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
refit <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments")
  source("BAPLA_Functions.R")
  
  # Initialize parameters
  
  p = 10; 
  max_iter = 1000; tol=1e-5; 
  m = 10
  
  res = Estimate_coefficients_refit(k[[1]], p, max_iter, tol, m, k[[2]])
  
  beta_hat = res$beta_hat
  beta_0_hat = res$beta0
  f_hat = res$f_hat
  c_hat = res$c_hat
  
  
  return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 
print(end-start)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Refit/10")
saveRDS(refit, "refit_50_5000.RDS")


# n = 10,000

# load libraries
library(purrr)
library(splines)
# load results from stage 1
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/10")
out <- readRDS("out_50_10000.RDS")
# load data used to get stage 1 results
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulate_Data/10/Sims_data")
data <- readRDS("data_50_10000.RDS")
beta_hats = map(out, 1)

# Parallelised refitting --------------------------------------------------

library(doParallel)
library(doRNG)
library(pracma) #for logspace
list_of_pairs <- Map(list, data, beta_hats)
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
refit <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments")
  source("BAPLA_Functions.R")
  
  # Initialize parameters
  
  p = 10; 
  max_iter = 1000; tol=1e-5; 
  m = 10
  
  res = Estimate_coefficients_refit(k[[1]], p, max_iter, tol, m, k[[2]])
  
  beta_hat = res$beta_hat
  beta_0_hat = res$beta0
  f_hat = res$f_hat
  c_hat = res$c_hat
  
  
  return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 
print(end-start)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Refit/10")
saveRDS(refit, "refit_50_10000.RDS")


# beta_0 = -3.6 (old zero-inflated model) --------------------------------------------------

# n = 1000
# load libraries
library(purrr)
library(splines)
# load results from stage 1
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/10")
out <- readRDS("out_zero_1000.RDS")
# load data used to get stage 1 results
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulate_Data/10/Sims_data")
data <- readRDS("data_zero_1000.RDS")
beta_hats = map(out, 1)

# Parallelised refitting --------------------------------------------------

library(doParallel)
library(doRNG)
library(pracma) #for logspace
list_of_pairs <- Map(list, data, beta_hats)
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
refit <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments")
  source("BAPLA_Functions.R")
  
  # Initialize parameters
  
  p = 10; 
  max_iter = 1000; tol=1e-5; 
  m = 10
  
  res = Estimate_coefficients_refit(k[[1]], p, max_iter, tol, m, k[[2]])
  
  beta_hat = res$beta_hat
  beta_0_hat = res$beta0
  f_hat = res$f_hat
  c_hat = res$c_hat
  
  
  return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 
print(end-start)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Refit/10")
saveRDS(refit, "refit_zero_1000.RDS")



# n = 2000
# load libraries
library(purrr)
library(splines)
# load results from stage 1
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/10")
out <- readRDS("out_zero_2000.RDS")
# load data used to get stage 1 results
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulate_Data/10/Sims_data")
data <- readRDS("data_zero_2000.RDS")
beta_hats = map(out, 1)

# Parallelised refitting --------------------------------------------------

library(doParallel)
library(doRNG)
library(pracma) #for logspace
list_of_pairs <- Map(list, data, beta_hats)
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
refit <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments")
  source("BAPLA_Functions.R")
  
  # Initialize parameters
  
  p = 10; 
  max_iter = 1000; tol=1e-5; 
  m = 10
  
  res = Estimate_coefficients_refit(k[[1]], p, max_iter, tol, m, k[[2]])
  
  beta_hat = res$beta_hat
  beta_0_hat = res$beta0
  f_hat = res$f_hat
  c_hat = res$c_hat
  
  
  return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 
print(end-start)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Refit/10")
saveRDS(refit, "refit_zero_2000.RDS")


# n = 5000

# load libraries
library(purrr)
library(splines)
# load results from stage 1
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/10")
out <- readRDS("out_zero_5000.RDS")
# load data used to get stage 1 results
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulate_Data/10/Sims_data")
data <- readRDS("data_zero_5000.RDS")
beta_hats = map(out, 1)

# Parallelised refitting --------------------------------------------------

library(doParallel)
library(doRNG)
library(pracma) #for logspace
list_of_pairs <- Map(list, data, beta_hats)
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
refit <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments")
  source("BAPLA_Functions.R")
  
  # Initialize parameters
  
  p = 10; 
  max_iter = 1000; tol=1e-5; 
  m = 10
  
  res = Estimate_coefficients_refit(k[[1]], p, max_iter, tol, m, k[[2]])
  
  beta_hat = res$beta_hat
  beta_0_hat = res$beta0
  f_hat = res$f_hat
  c_hat = res$c_hat
  
  
  return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 
print(end-start)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Refit/10")
saveRDS(refit, "refit_zero_5000.RDS")


# n = 10,000

# load libraries
library(purrr)
library(splines)
# load results from stage 1
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/10")
out <- readRDS("out_zero_10000.RDS")
# load data used to get stage 1 results
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulate_Data/10/Sims_data")
data <- readRDS("data_zero_10000.RDS")
beta_hats = map(out, 1)

# Parallelised refitting --------------------------------------------------

library(doParallel)
library(doRNG)
library(pracma) #for logspace
list_of_pairs <- Map(list, data, beta_hats)
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
refit <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments")
  source("BAPLA_Functions.R")
  
  # Initialize parameters
  
  p = 10; 
  max_iter = 1000; tol=1e-5; 
  m = 10
  
  res = Estimate_coefficients_refit(k[[1]], p, max_iter, tol, m, k[[2]])
  
  beta_hat = res$beta_hat
  beta_0_hat = res$beta0
  f_hat = res$f_hat
  c_hat = res$c_hat
  
  
  return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat)) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 
print(end-start)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Refit/10")
saveRDS(refit, "refit_zero_10000.RDS")
