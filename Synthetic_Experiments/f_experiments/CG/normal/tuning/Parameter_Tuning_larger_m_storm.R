# queue jobs on STORM -----------------------------------------------------
l <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm

# 50/50 -------------------------------------------------------------------

# n = 1000
if (l==1){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  
  m_seq = c(25,30,35,40)
  data_seq = seq(1, 10)
  lambdas = logspace(log10(1),log10(150),100)
  grid = expand.grid(sample = data_seq, lams = lambdas,  m = m_seq)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  
  cl <- makeCluster(44) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
    
    #setwd("~/Downloads/BAPLA/Synthetic_Experiments")
    source("BAPLA_Functions.R")
    
    # load data
    #setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Tuning_data")
    data_50 <- readRDS("data_50_tuning_1000.RDS")
    
    # Initialize parameters
    
    p = 10; N=1000;
    max_iter = 1000; tol=1e-5; 
    l = as.numeric(k[2])
    lams = rep(l,p)
    m = as.numeric(k[3])
    
    data_idx = as.numeric(k[1])
    data = data_50[[data_idx]]
    
    # Estimate coefficients with own function
    res = Estimate_coefficients_logit(data, p, max_iter, tol, lams, m)
    beta_hat = res$beta_hat
    beta_0_hat = res$beta0
    f_hat = res$f
    c_hat = res$c_hat
    beta_alt = res$beta_alt
    
    return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat, beta_alt = beta_alt)) 
    
  }
  
  end <-Sys.time() 
  stopCluster(cl) 
  end-start 
  print(end-start)
  #setwd("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1")
  saveRDS(out, "out_50_1000.RDS")
}  

# n = 2000
if (l==2){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  
  m_seq = c(25,30,35,40)
  data_seq = seq(1, 10)
  lambdas = logspace(log10(1),log10(150),100)
  grid = expand.grid(sample = data_seq, lams = lambdas,  m = m_seq)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  
  cl <- makeCluster(44) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
    
    #setwd("~/Downloads/BAPLA/Synthetic_Experiments")
    source("BAPLA_Functions.R")
    
    # load data
    #setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Tuning_data")
    data_50 <- readRDS("data_50_tuning_2000.RDS")
    
    # Initialize parameters
    
    p = 10; N=2000;
    max_iter = 1000; tol=1e-5; 
    l = as.numeric(k[2])
    lams = rep(l,p)
    m = as.numeric(k[3])
    
    data_idx = as.numeric(k[1])
    data = data_50[[data_idx]]
    
    # Estimate coefficients with own function
    res = Estimate_coefficients_logit(data, p, max_iter, tol, lams, m)
    beta_hat = res$beta_hat
    beta_0_hat = res$beta0
    f_hat = res$f
    c_hat = res$c_hat
    beta_alt = res$beta_alt
    
    return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat, beta_alt = beta_alt)) 
    
  }

  end <-Sys.time() 
  stopCluster(cl) 
  end-start 
  print(end-start)
  #setwd("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1")
  saveRDS(out, "out_50_2000.RDS")
}


# n = 5000

if (l==3){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  
  m_seq = c(25,30,35,40)
  data_seq = seq(1, 10)
  lambdas = logspace(log10(1),log10(150),100)
  grid = expand.grid(sample = data_seq, lams = lambdas,  m = m_seq)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  
  cl <- makeCluster(44) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
    
    #setwd("~/Downloads/BAPLA/Synthetic_Experiments")
    source("BAPLA_Functions.R")
    
    # load data
    #setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Tuning_data")
    data_50 <- readRDS("data_50_tuning_5000.RDS")
    
    # Initialize parameters
    
    p = 10; N=5000;
    max_iter = 1000; tol=1e-5; 
    l = as.numeric(k[2])
    lams = rep(l,p)
    m = as.numeric(k[3])
    
    data_idx = as.numeric(k[1])
    data = data_50[[data_idx]]
    
    # Estimate coefficients with own function
    res = Estimate_coefficients_logit(data, p, max_iter, tol, lams, m)
    beta_hat = res$beta_hat
    beta_0_hat = res$beta0
    f_hat = res$f
    c_hat = res$c_hat
    beta_alt = res$beta_alt
    
    return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat, beta_alt = beta_alt)) 
    
  }
  
  end <-Sys.time() 
  stopCluster(cl) 
  end-start 
  print(end-start)
  #setwd("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1")
  saveRDS(out, "out_50_5000.RDS")
}

# n = 10000
if (l==4){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  
  
  m_seq = c(25,30,35,40)
  data_seq = seq(1, 10)
  lambdas = logspace(log10(1),log10(150),100)
  grid = expand.grid(sample = data_seq, lams = lambdas,  m = m_seq)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  
  cl <- makeCluster(44) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
    
    #setwd("~/Downloads/BAPLA/Synthetic_Experiments")
    source("BAPLA_Functions.R")
    
    # load data
    #setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Tuning_data")
    data_50 <- readRDS("data_50_tuning_10000.RDS")
    
    # Initialize parameters
    
    p = 10; N=10000;
    max_iter = 1000; tol=1e-5; 
    l = as.numeric(k[2])
    lams = rep(l,p)
    m = as.numeric(k[3])
    
    data_idx = as.numeric(k[1])
    data = data_50[[data_idx]]
    
    # Estimate coefficients with own function
    res = Estimate_coefficients_logit(data, p, max_iter, tol, lams, m)
    beta_hat = res$beta_hat
    beta_0_hat = res$beta0
    f_hat = res$f
    c_hat = res$c_hat
    beta_alt = res$beta_alt
    
    return(list(beta_hat = beta_hat, beta_0_hat=beta_0_hat, f_hat = f_hat, c_hat = c_hat, beta_alt = beta_alt)) 
    
  }
  
  end <-Sys.time() 
  stopCluster(cl) 
  end-start 
  print(end-start)
  #setwd("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1")
  saveRDS(out, "out_50_10000.RDS")
}