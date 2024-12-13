
# Load Required Libraries -------------------------------------------------

library(doParallel)
library(doRNG)


# Set number of replicates ------------------------------------------------

N_reps = seq(1, 100)


# p = 50 ------------------------------------------------------------------


# Zero-Inflated Model (beta_0 = -4.6) -----------------------------------------------------

# n = 1000 time points

cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
data_zero_1000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Simulation_Study")
  source("Functions_BAPLA.R")
  
  # Initialize parameters
  p = 50; N=1500;
  
  beta <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/beta_50.RDS")
  beta_0 = rep(-3.6,p)
  
  # Simulate the data
  data = Simulate_data_add_gamma_norm(p, N, beta, beta_0, cut_off=500, shape = 3 , scale=(N+500)/10, const=(N-500),  mu=(N+500)/2, sigma=(N+500)/10)
  
  return(data) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 

# n = 2000 time points


cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
data_zero_2000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Simulation_Study")
  source("Functions_BAPLA.R")
  
  # Initialize parameters
  p = 50; N=2500;
  
   beta <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/beta_50.RDS")
  beta_0 = rep(-3.6,p)
  
  # Simulate the data
  data = Simulate_data_add_gamma_norm(p, N, beta, beta_0, cut_off=500, shape = 3 , scale=(N+500)/10, const=(N-500),  mu=(N+500)/2, sigma=(N+500)/10)
  
  return(data) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 



# n = 5000 time points

cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
data_zero_5000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
                                                       
                                                       setwd("~/Downloads/BAPLA/Simulation_Study")
                                                       source("Functions_BAPLA.R")
                                                       
                                                       # Initialize parameters
                                                       p = 50; N=5500;
                                                       
                                                        beta <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/beta_50.RDS")
                                                       beta_0 = rep(-3.6,p)
                                                       
                                                       # Simulate the data
                                                       data = Simulate_data_add_gamma_norm(p, N, beta, beta_0, cut_off=500, shape = 3 , scale=(N+500)/10, const=(N-500),  mu=(N+500)/2, sigma=(N+500)/10)
                                                       
                                                       return(data) 
                                                       
                                                     }

end <-Sys.time() 
stopCluster(cl) 
end-start 


# n = 10000 time points

cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
data_zero_10000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Simulation_Study")
  source("Functions_BAPLA.R")
  
  # Initialize parameters
  p = 50; N=10500;
  
   beta <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/beta_50.RDS")
  beta_0 = rep(-3.6,p)
  
  # Simulate the data
  data = Simulate_data_add_gamma_norm(p, N, beta, beta_0, cut_off=500, shape = 3 , scale=(N+500)/10, const=(N-500),  mu=(N+500)/2, sigma=(N+500)/10)
  
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
registerDoRNG(seed = 123)
data_50_1000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Simulation_Study")
  source("Functions_BAPLA.R")
  
  # Initialize parameters
  p = 50; N=1500
   beta <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/beta_50.RDS")
  beta_0 = rep(-2.6,p)
  
  # Simulate the data
  data = Simulate_data_add_gamma_norm(p, N, beta, beta_0, cut_off=500, shape = 3 , scale=(N+500)/10, const=(N-500),  mu=(N+500)/2, sigma=(N+500)/10)
  
  return(data) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 

# n = 2000 time points
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
data_50_2000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Simulation_Study")
  source("Functions_BAPLA.R")
  
  # Initialize parameters
  p = 50; N=2500
   beta <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/beta_50.RDS")
  beta_0 = rep(-2.6,p)
  
  # Simulate the data
  data = Simulate_data_add_gamma_norm(p, N, beta, beta_0, cut_off=500, shape = 3 , scale=(N+500)/10, const=(N-500),  mu=(N+500)/2, sigma=(N+500)/10)
  return(data) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 

# n = 5000 time points
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
data_50_5000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Simulation_Study")
  source("Functions_BAPLA.R")
  
  # Initialize parameters
  p = 50; N=5500
   beta <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/beta_50.RDS")
  beta_0 = rep(-2.6,p)
  
  # Simulate the data
  data = Simulate_data_add_gamma_norm(p, N, beta, beta_0, cut_off=500, shape = 3 , scale=(N+500)/10, const=(N-500),  mu=(N+500)/2, sigma=(N+500)/10)
  
  return(data) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 



# n = 10000 time points
cl <- makeCluster(4) 
start <- Sys.time()
registerDoParallel(cl)
registerDoRNG(seed = 123)
data_50_10000 <- foreach(k=N_reps, .packages = c("LaplacesDemon")) %dopar%{
  
  setwd("~/Downloads/BAPLA/Simulation_Study")
  source("Functions_BAPLA.R")
  
  # Initialize parameters
  p = 50; N=10500
   beta <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/beta_50.RDS")
  beta_0 = rep(-2.6,p)
  
  # Simulate the data
  data = Simulate_data_add_gamma_norm(p, N, beta, beta_0, cut_off=500, shape = 3 , scale=(N+500)/10, const=(N-500),  mu=(N+500)/2, sigma=(N+500)/10)
  
  return(data) 
  
}

end <-Sys.time() 
stopCluster(cl) 
end-start 

setwd("~/Downloads/BAPLA/Simulation_Study/SB/Simulate_Data/50/Sims_data")
saveRDS(data_zero_1000, "data_zero_1000.RDS")
saveRDS(data_zero_2000, "data_zero_2000.RDS")
saveRDS(data_zero_5000, "data_zero_5000.RDS")
saveRDS(data_zero_10000, "data_zero_10000.RDS")

saveRDS(data_50_1000, "data_50_1000.RDS")
saveRDS(data_50_2000, "data_50_2000.RDS")
saveRDS(data_50_5000, "data_50_5000.RDS")
saveRDS(data_50_10000, "data_50_10000.RDS")


