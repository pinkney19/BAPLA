# queue jobs on STORM -----------------------------------------------------

l <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm


# zero-inflated -----------------------------------------------------------

# n = 1000

if (l==1){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(log10(10),log10(300),100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
    
    
    source("Functions_BAPLA.R")
    
    # load data
    data_zero <- readRDS("data_zero_tuning_1000.RDS")
    
    # Initialize parameters
    
    p = 10; N=1000;
    max_iter = 1000; tol=1e-5; 
    l = as.numeric(k[2])
    lams = rep(l,p)
    m = 10
    
    data_idx = as.numeric(k[1])
    data = data_zero[[data_idx]]
    
    # Estimate coefficients with own function
    res = Estimate_coefficients_logit(data, p, max_iter, tol, lams, m)
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
  saveRDS(out, "out_zero_1000.RDS")
}

# n = 2000

if (l==2){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(log10(10),log10(300),100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
    
    
    source("Functions_BAPLA.R")
    
    # load data
    data_zero <- readRDS("data_zero_tuning_2000.RDS")
    
    # Initialize parameters
    
    p = 10; N=2000;
    max_iter = 1000; tol=1e-5; 
    l = as.numeric(k[2])
    lams = rep(l,p)
    m = 10
    
    data_idx = as.numeric(k[1])
    data = data_zero[[data_idx]]
    
    # Estimate coefficients with own function
    res = Estimate_coefficients_logit(data, p, max_iter, tol, lams, m)
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
  saveRDS(out, "out_zero_2000.RDS")
}


# n = 5000 


if (l==3){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(log10(10),log10(300),100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
                                                            
          
          source("Functions_BAPLA.R")
          
          # load data
          data_zero <- readRDS("data_zero_tuning_5000.RDS")
         
          # Initialize parameters
          
          p = 10; N=5000;
          max_iter = 1000; tol=1e-5; 
          l = as.numeric(k[2])
          lams = rep(l,p)
          m = 10
         
          data_idx = as.numeric(k[1])
          data = data_zero[[data_idx]]
          
          # Estimate coefficients with own function
          res = Estimate_coefficients_logit(data, p, max_iter, tol, lams, m)
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
  saveRDS(out, "out_zero_5000.RDS")
}


# n = 10000 

if (l==4){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(log10(10),log10(300),100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
    
    
    source("Functions_BAPLA.R")
    
    # load data
    data_zero <- readRDS("data_zero_tuning_10000.RDS")
    
    # Initialize parameters
    
    p = 10; N=10000;
    max_iter = 1000; tol=1e-5; 
    l = as.numeric(k[2])
    lams = rep(l,p)
    m = 10
    
    data_idx = as.numeric(k[1])
    data = data_zero[[data_idx]]
    
    # Estimate coefficients with own function
    res = Estimate_coefficients_logit(data, p, max_iter, tol, lams, m)
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
  saveRDS(out, "out_zero_10000.RDS")
}


# 50/50 -------------------------------------------------------------------

# n = 1000

if (l==5){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(log10(10),log10(300),100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
    
    
    source("Functions_BAPLA.R")
    
    # load data
    data_50 <- readRDS("data_50_tuning_1000.RDS")
    
    # Initialize parameters
    
    p = 10; N=1000;
    max_iter = 1000; tol=1e-5; 
    l = as.numeric(k[2])
    lams = rep(l,p)
    m = 10
    
    data_idx = as.numeric(k[1])
    data = data_50[[data_idx]]
    
    # Estimate coefficients with own function
    res = Estimate_coefficients_logit(data, p, max_iter, tol, lams, m)
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
  saveRDS(out, "out_50_1000.RDS")
}

# n = 2000
if (l==6){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(log10(10),log10(300),100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
    
    
    source("Functions_BAPLA.R")
    
    # load data
    data_50 <- readRDS("data_50_tuning_2000.RDS")
    
    # Initialize parameters
    
    p = 10; N=2000;
    max_iter = 1000; tol=1e-5; 
    l = as.numeric(k[2])
    lams = rep(l,p)
    m = 10
    
    data_idx = as.numeric(k[1])
    data = data_50[[data_idx]]
    
    # Estimate coefficients with own function
    res = Estimate_coefficients_logit(data, p, max_iter, tol, lams, m)
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
  saveRDS(out, "out_50_2000.RDS")
}

# n = 5000

if (l==7){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(log10(10),log10(300),100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
    
    
    source("Functions_BAPLA.R")
    
    # load data
    data_50 <- readRDS("data_50_tuning_5000.RDS")
    
    # Initialize parameters
    
    p = 10; N=5000;
    max_iter = 1000; tol=1e-5; 
    l = as.numeric(k[2])
    lams = rep(l,p)
    m = 10
    
    data_idx = as.numeric(k[1])
    data = data_50[[data_idx]]
    
    # Estimate coefficients with own function
    res = Estimate_coefficients_logit(data, p, max_iter, tol, lams, m)
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
  saveRDS(out, "out_50_5000.RDS")
}

# n = 10000
if (l==8){
  library(doParallel)
  library(doRNG)
  library(pracma) #for logspace
  lambdas = logspace(log10(10),log10(300),100)
  data_seq = seq(1, 10)
  grid = expand.grid(sample = data_seq, lams = lambdas)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  out <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon")) %dopar%{
    
    
    source("Functions_BAPLA.R")
    
    # load data
    data_50 <- readRDS("data_50_tuning_10000.RDS")
    
    # Initialize parameters
    
    p = 10; N=10000;
    max_iter = 1000; tol=1e-5; 
    l = as.numeric(k[2])
    lams = rep(l,p)
    m = 10
    
    data_idx = as.numeric(k[1])
    data = data_50[[data_idx]]
    
    # Estimate coefficients with own function
    res = Estimate_coefficients_logit(data, p, max_iter, tol, lams, m)
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
  saveRDS(out, "out_50_10000.RDS")
}
