
# Load Required Libraries -------------------------------------------------

library(doParallel)
library(doRNG)


# Set number of replicates ------------------------------------------------

N_reps = seq(1, 10)


# p = 50 ------------------------------------------------------------------


# Zero-Inflated Model (beta_0 = -2.6) -----------------------------------------------------

# n = 1000 time points

cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 456)
data_zero_1000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments")
  source("BAPLA_Functions.R")
  
  # Initialize parameters
  p = 50; N=1000;
  
  beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_50.RDS")
  beta_0 = rep(-2.6,p)
  
  # Simulate the data
  data = Simulate_data_add_norm(p, N, beta, beta_0, cut_off=0, mu=0.5, sigma = 0.05)
  
  return(data) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 

# n = 2000 time points


cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 456)
data_zero_2000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments")
  source("BAPLA_Functions.R")
  
  # Initialize parameters
  p = 50; N=2000;
  
   beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_50.RDS")
  beta_0 = rep(-2.6,p)
  
  # Simulate the data
  data = Simulate_data_add_norm(p, N, beta, beta_0, cut_off=0, mu=0.5, sigma = 0.05)
  
  return(data) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 



# n = 5000 time points

cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 456)
data_zero_5000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
                                                       
                                                       setwd("~/Downloads/BAPLA/Synthetic_Experiments")
                                                       source("BAPLA_Functions.R")
                                                       
                                                       # Initialize parameters
                                                       p = 50; N=5000;
                                                       
                                                        beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_50.RDS")
                                                       beta_0 = rep(-2.6,p)
                                                       
                                                       # Simulate the data
                                                       data = Simulate_data_add_norm(p, N, beta, beta_0, cut_off=0, mu=0.5, sigma = 0.05)
                                                       
                                                       return(data) 
                                                       
                                                     }

end <-Sys.time() 
stopCluster(cl) 
end-start 


# n = 10000 time points

cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 456)
data_zero_10000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments")
  source("BAPLA_Functions.R")
  
  # Initialize parameters
  p = 50; N=10000;
  
   beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_50.RDS")
  beta_0 = rep(-2.6,p)
  
  # Simulate the data
  data = Simulate_data_add_norm(p, N, beta, beta_0, cut_off=0, mu=0.5, sigma = 0.05)
  
  return(data) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 


# 50/50 Model -----------------------------------------------------

# n = 1000 time points
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 456)
data_50_1000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments")
  source("BAPLA_Functions.R")
  
  # Initialize parameters
  p = 50; N=1000
   beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_50.RDS")
  beta_0 = rep(0.1,p)
  
  # Simulate the data
  data = Simulate_data_add_norm(p, N, beta, beta_0,cut_off=0, mu=0.5, sigma = 0.05)
  
  return(data) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 

# n = 2000 time points
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 456)
data_50_2000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments")
  source("BAPLA_Functions.R")
  
  # Initialize parameters
  p = 50; N=2000
   beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_50.RDS")
  beta_0 = rep(0.1,p)
  
  # Simulate the data
  data = Simulate_data_add_norm(p, N, beta, beta_0, cut_off=0, mu=0.5, sigma = 0.05)
  return(data) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 

# n = 5000 time points
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 456)
data_50_5000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments")
  source("BAPLA_Functions.R")
  
  # Initialize parameters
  p = 50; N=5000
   beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_50.RDS")
  beta_0 = rep(0.1,p)
  
  # Simulate the data
  data = Simulate_data_add_norm(p, N, beta, beta_0,cut_off=0, mu=0.5, sigma = 0.05)
  
  return(data) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 



# n = 10000 time points
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 456)
data_50_10000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments")
  source("BAPLA_Functions.R")
  
  # Initialize parameters
  p = 50; N=10000
   beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_50.RDS")
  beta_0 = rep(0.1,p)
  
  # Simulate the data
  data = Simulate_data_add_norm(p, N, beta, beta_0, cut_off=0, mu=0.5, sigma = 0.05)
  
  return(data) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 

setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/50/Tuning_data")
saveRDS(data_zero_1000, "data_zero_tuning_1000.RDS")
saveRDS(data_zero_2000, "data_zero_tuning_2000.RDS")
saveRDS(data_zero_5000, "data_zero_tuning_5000.RDS")
saveRDS(data_zero_10000, "data_zero_tuning_10000.RDS")

saveRDS(data_50_1000, "data_50_tuning_1000.RDS")
saveRDS(data_50_2000, "data_50_tuning_2000.RDS")
saveRDS(data_50_5000, "data_50_tuning_5000.RDS")
saveRDS(data_50_10000, "data_50_tuning_10000.RDS")


