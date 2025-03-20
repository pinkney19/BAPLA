# De-sparsified Lasso Procedure -----------------------------------------------


# queue jobs on STORM -----------------------------------------------------

l <- as.numeric(commandArgs(trailingOnly=TRUE)[1]) #needed to queue jobs on storm


# Left trials

if (l==1){
  library(doParallel)
  library(doRNG)
  # need to loop over each unit (i.e. dimension) and for each trial
  n_samp = seq(1,60)
  unit_seq = seq(1,48)
  grid = expand.grid(sample = n_samp, unit=unit_seq)
  list_of_pairs <- split(grid, seq(nrow(grid)))
  cl <- makeCluster(20) 
  start <- Sys.time()
  registerDoParallel(cl)
  registerDoRNG(seed = 123)
  res_50 <- foreach(k = list_of_pairs, .packages = c("splines","LaplacesDemon", "purrr")) %dopar%{
    
    
    source("Functions_data.R")
    
    # load data
    #setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Sims_data")
    data_50 <- readRDS("left.RDS")
    #setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/10")
    out_50 <- readRDS("out_50_1000.RDS") #use first stage estimates)
    
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
    phi2 = matrix(NA, nrow = nrow(phi), ncol=ncol(phi))
    phi_bar = NULL;
    for(i in 1:ncol(phi)){
      phi2[,i] = phi[,i] - mean(phi[,i])
      phi_bar[i] = mean(phi[,i])
    }
    f_hat = lapply(c, function(x) phi2%*%as.matrix(x))
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