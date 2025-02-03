# De-sparsified Lasso Procedure -----------------------------------------------


# queue jobs on STORM -----------------------------------------------------

l <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm


# 50/50 -------------------------------------------------------------------

#n = 1000
if (l==1){
  library(doParallel)
  library(doRNG)
  # need to loop over each unit (i.e. dimension) and for each Monte Carlo sample
  n_samp = seq(1,100)
  unit_seq = seq(1,10)
  grid = expand.grid(sample = n_samp, unit=unit_seq)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  res_50 <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon", "purrr")) %dopar%{
  
   
    source("BAPLA_Functions.R")
    
    # load data
    data_50 <- readRDS("data_50_1000.RDS")
    out_50 <- readRDS("refit_50_1000.RDS") #use first stage estimates)
   
    # Initialize parameters
    p = 10; alpha=0.05;
    sample_idx = as.numeric(k[1])
    data = data_50[[sample_idx]]
    
    unit_idx = as.numeric(k[2])

    # parameters estimates for sample of interest
    #f_hat = as.vector(f_hat[[sample_idx]])
    out = out_50[[sample_idx]]
    beta_hat = out$beta_hat
    beta_0 = out$beta_0_hat
    c = out$c_hat
    t_end = dim(data)[2] - 1
    phi <- bs(seq(1, t_end), df=10, intercept = F) 
    f_hat = lapply(c, function(x) phi%*%as.matrix(x))
    # Get desparsified estimates and CIs
    res = despars_lasso(unit_idx, data, p, beta_0, beta_hat, f_hat, alpha)
  
    # return values of interests
    
    return(list(beta_deb = res$beta_deb, sigma = res$sigma, lower = res$ci_lower, upper = res$ci_upper, theta_lasso = res$theta_lasso)) 
    
  }
  
  end <-Sys.time() 
  stopCluster(cl) 
  end-start 
  print(end-start)
  saveRDS(res_50, "out_50_1000_desparsified.RDS")
  
}
#n = 2000

if (l==2){
  library(doParallel)
  library(doRNG)
  # need to loop over each unit (i.e. dimension) and for each Monte Carlo sample
  n_samp = seq(1,100)
  unit_seq = seq(1,10)
  grid = expand.grid(sample = n_samp, unit=unit_seq)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  res_50 <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon", "purrr")) %dopar%{
    
    
    source("BAPLA_Functions.R")
    
    # load data
    data_50 <- readRDS("data_50_2000.RDS")
    out_50 <- readRDS("refit_50_2000.RDS") #use first stage estimates)
    
    # Initialize parameters
    p = 10; alpha=0.05;
    sample_idx = as.numeric(k[1])
    data = data_50[[sample_idx]]
    
    unit_idx = as.numeric(k[2])
   
    
    # parameters estimates for sample of interest
    #f_hat = as.vector(f_hat[[sample_idx]])
    out = out_50[[sample_idx]]
    beta_hat = out$beta_hat
    beta_0 = out$beta_0_hat
    c = out$c_hat
    t_end = dim(data)[2] - 1
    phi <- bs(seq(1, t_end), df=10, intercept = F)
    f_hat = lapply(c, function(x) phi%*%as.matrix(x))
    
    # Get desparsified estimates and CIs
    res = despars_lasso(unit_idx, data, p, beta_0, beta_hat, f_hat, alpha)
    
    # return values of interests
    
    return(list(beta_deb = res$beta_deb, sigma = res$sigma, lower = res$ci_lower, upper = res$ci_upper, theta_lasso = res$theta_lasso)) 
    
  }
  
  end <-Sys.time() 
  stopCluster(cl) 
  end-start 
  print(end-start)
  saveRDS(res_50, "out_50_2000_desparsified.RDS")
  
}

# n = 5000

if (l==3){
  library(doParallel)
  library(doRNG)
  # need to loop over each unit (i.e. dimension) and for each Monte Carlo sample
  n_samp = seq(1,100)
  unit_seq = seq(1,10)
  grid = expand.grid(sample = n_samp, unit=unit_seq)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  res_50 <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon", "purrr")) %dopar%{
    
    
    source("BAPLA_Functions.R")
    
    # load data
    data_50 <- readRDS("data_50_5000.RDS")
    out_50 <- readRDS("refit_50_5000.RDS") #use first stage estimates)
    
    # Initialize parameters
    p = 10; alpha=0.05;
    sample_idx = as.numeric(k[1])
    data = data_50[[sample_idx]]
    
    unit_idx = as.numeric(k[2])
   
    
    # parameters estimates for sample of interest
    #f_hat = as.vector(f_hat[[sample_idx]])
    out = out_50[[sample_idx]]
    beta_hat = out$beta_hat
    beta_0 = out$beta_0_hat
    c = out$c_hat
    t_end = dim(data)[2] - 1
    phi <- bs(seq(1, t_end), df=10, intercept = F) 
    f_hat = lapply(c, function(x) phi%*%as.matrix(x))
    
    # Get desparsified estimates and CIs
    res = despars_lasso(unit_idx, data, p, beta_0, beta_hat, f_hat, alpha)
    
    # return values of interests
    
    return(list(beta_deb = res$beta_deb, sigma = res$sigma, lower = res$ci_lower, upper = res$ci_upper, theta_lasso = res$theta_lasso)) 
    
  }
  
  end <-Sys.time() 
  stopCluster(cl) 
  end-start 
  print(end-start)
  saveRDS(res_50, "out_50_5000_desparsified.RDS")
  
}

#n = 10000

if (l==4){
  library(doParallel)
  library(doRNG)
  # need to loop over each unit (i.e. dimension) and for each Monte Carlo sample
  n_samp = seq(1,100)
  unit_seq = seq(1,10)
  grid = expand.grid(sample = n_samp, unit=unit_seq)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  res_50 <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon", "purrr")) %dopar%{
    
    
    source("BAPLA_Functions.R")
    
    # load data
    data_50 <- readRDS("data_50_10000.RDS")
    out_50 <- readRDS("refit_50_10000.RDS") #use first stage estimates)
    
    # Initialize parameters
    p = 10; alpha=0.05;
    sample_idx = as.numeric(k[1])
    data = data_50[[sample_idx]]
    
    unit_idx = as.numeric(k[2])
  
    
    # parameters estimates for sample of interest
    #f_hat = as.vector(f_hat[[sample_idx]])
    out = out_50[[sample_idx]]
    beta_hat = out$beta_hat
    beta_0 = out$beta_0_hat
    c = out$c_hat
    t_end = dim(data)[2] - 1
    phi <- bs(seq(1, t_end), df=10, intercept = F) 
    f_hat = lapply(c, function(x) phi%*%as.matrix(x))
    
    # Get desparsified estimates and CIs
    res = despars_lasso(unit_idx, data, p, beta_0, beta_hat, f_hat, alpha)
    
    # return values of interests
    
    return(list(beta_deb = res$beta_deb, sigma = res$sigma, lower = res$ci_lower, upper = res$ci_upper, theta_lasso = res$theta_lasso)) 
    
  }
  
  end <-Sys.time() 
  stopCluster(cl) 
  end-start 
  print(end-start)
  saveRDS(res_50, "out_50_10000_desparsified.RDS")
  
}


# zero-inflated -----------------------------------------------------------
# n  = 1000
if (l==5){
  library(doParallel)
  library(doRNG)
  # need to loop over each unit (i.e. dimension) and for each Monte Carlo sample
  n_samp = seq(1,100)
  unit_seq = seq(1,10)
  grid = expand.grid(sample = n_samp, unit=unit_seq)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  res_50 <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon", "purrr")) %dopar%{
    
    
    source("BAPLA_Functions.R")
    
    # load data
    data_zero <- readRDS("data_zero_1000.RDS")
    out_zero <- readRDS("refit_zero_1000.RDS") #use first stage estimates)
    
    # Initialize parameters
    p = 10; alpha=0.05;
    sample_idx = as.numeric(k[1])
    data = data_zero[[sample_idx]]
    
    unit_idx = as.numeric(k[2])
    
    # parameters estimates for sample of interest
    #f_hat = as.vector(f_hat[[sample_idx]])
    out = out_zero[[sample_idx]]
    beta_hat = out$beta_hat
    beta_0 = out$beta_0_hat
    c = out$c_hat
    t_end = dim(data)[2] - 1
    phi <- bs(seq(1, t_end), df=10, intercept = F) 
    f_hat = lapply(c, function(x) phi%*%as.matrix(x))
    
    # Get desparsified estimates and CIs
    res = despars_lasso(unit_idx, data, p, beta_0, beta_hat, f_hat, alpha)
    
    # return values of interests
    
    return(list(beta_deb = res$beta_deb, sigma = res$sigma, lower = res$ci_lower, upper = res$ci_upper, theta_lasso = res$theta_lasso)) 
    
  }
  
  end <-Sys.time() 
  stopCluster(cl) 
  end-start 
  print(end-start)
  saveRDS(res_50, "out_zero_1000_desparsified.RDS")
  
}

# n  = 2000
if (l==6){
  library(doParallel)
  library(doRNG)
  # need to loop over each unit (i.e. dimension) and for each Monte Carlo sample
  n_samp = seq(1,100)
  unit_seq = seq(1,10)
  grid = expand.grid(sample = n_samp, unit=unit_seq)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  res_50 <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon", "purrr")) %dopar%{
    
    
    source("BAPLA_Functions.R")
    
    # load data
    data_zero <- readRDS("data_zero_2000.RDS")
    out_zero <- readRDS("refit_zero_2000.RDS") #use first stage estimates)
    
    # Initialize parameters
    p = 10; alpha=0.05;
    sample_idx = as.numeric(k[1])
    data = data_zero[[sample_idx]]
    
    unit_idx = as.numeric(k[2])
    
    # parameters estimates for sample of interest
    #f_hat = as.vector(f_hat[[sample_idx]])
    out = out_zero[[sample_idx]]
    beta_hat = out$beta_hat
    beta_0 = out$beta_0_hat
    c = out$c_hat
    t_end = dim(data)[2] - 1
    phi <- bs(seq(1, t_end), df=10, intercept = F) 
    f_hat = lapply(c, function(x) phi%*%as.matrix(x))
    
    # Get desparsified estimates and CIs
    res = despars_lasso(unit_idx, data, p, beta_0, beta_hat, f_hat, alpha)
    
    # return values of interests
    
    return(list(beta_deb = res$beta_deb, sigma = res$sigma, lower = res$ci_lower, upper = res$ci_upper, theta_lasso = res$theta_lasso)) 
    
  }
  
  end <-Sys.time() 
  stopCluster(cl) 
  end-start 
  print(end-start)
  saveRDS(res_50, "out_zero_2000_desparsified.RDS")
  
}

# n  = 5000
if (l==7){
  library(doParallel)
  library(doRNG)
  # need to loop over each unit (i.e. dimension) and for each Monte Carlo sample
  n_samp = seq(1,100)
  unit_seq = seq(1,10)
  grid = expand.grid(sample = n_samp, unit=unit_seq)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  res_50 <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon", "purrr")) %dopar%{
    
    
    source("BAPLA_Functions.R")
    
    # load data
    data_zero <- readRDS("data_zero_5000.RDS")
    out_zero <- readRDS("refit_zero_5000.RDS") #use first stage estimates)
    
    # Initialize parameters
    p = 10; alpha=0.05;
    sample_idx = as.numeric(k[1])
    data = data_zero[[sample_idx]]
    
    unit_idx = as.numeric(k[2])
    
    # parameters estimates for sample of interest
    #f_hat = as.vector(f_hat[[sample_idx]])
    out = out_zero[[sample_idx]]
    beta_hat = out$beta_hat
    beta_0 = out$beta_0_hat
    c = out$c_hat
    t_end = dim(data)[2] - 1
    phi <- bs(seq(1, t_end), df=10, intercept = F) 
    f_hat = lapply(c, function(x) phi%*%as.matrix(x))
    
    # Get desparsified estimates and CIs
    res = despars_lasso(unit_idx, data, p, beta_0, beta_hat, f_hat, alpha)
    
    # return values of interests
    
    return(list(beta_deb = res$beta_deb, sigma = res$sigma, lower = res$ci_lower, upper = res$ci_upper, theta_lasso = res$theta_lasso)) 
    
  }
  
  end <-Sys.time() 
  stopCluster(cl) 
  end-start 
  print(end-start)
  saveRDS(res_50, "out_zero_5000_desparsified.RDS")
  
}

# n  = 10000
if (l==8){
  library(doParallel)
  library(doRNG)
  # need to loop over each unit (i.e. dimension) and for each Monte Carlo sample
  n_samp = seq(1,100)
  unit_seq = seq(1,10)
  grid = expand.grid(sample = n_samp, unit=unit_seq)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(40) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  res_50 <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon", "purrr")) %dopar%{
    
    
    source("BAPLA_Functions.R")
    
    # load data
    data_zero <- readRDS("data_zero_10000.RDS")
    out_zero <- readRDS("refit_zero_10000.RDS") #use first stage estimates)
    
    # Initialize parameters
    p = 10; alpha=0.05;
    sample_idx = as.numeric(k[1])
    data = data_zero[[sample_idx]]
    
    unit_idx = as.numeric(k[2])
    
    # parameters estimates for sample of interest
    #f_hat = as.vector(f_hat[[sample_idx]])
    out = out_zero[[sample_idx]]
    beta_hat = out$beta_hat
    beta_0 = out$beta_0_hat
    c = out$c_hat
    t_end = dim(data)[2] - 1
    phi <- bs(seq(1, t_end), df=10, intercept = F) 
    f_hat = lapply(c, function(x) phi%*%as.matrix(x))
    
    # Get desparsified estimates and CIs
    res = despars_lasso(unit_idx, data, p, beta_0, beta_hat, f_hat, alpha)
    
    # return values of interests
    
    return(list(beta_deb = res$beta_deb, sigma = res$sigma, lower = res$ci_lower, upper = res$ci_upper, theta_lasso = res$theta_lasso)) 
    
  }
  
  end <-Sys.time() 
  stopCluster(cl) 
  end-start 
  print(end-start)
  saveRDS(res_50, "out_zero_10000_desparsified.RDS")
  
}