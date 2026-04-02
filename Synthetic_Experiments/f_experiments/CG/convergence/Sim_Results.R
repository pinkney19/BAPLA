# Load libraries ----------------------------------------------------------

library(purrr)
library(ggplot2)
library(matrixStats)
library(pROC)
library(splines)

setwd("~/Downloads/BAPLA/Synthetic_Experiments")
source("BAPLA_Functions.R")




# Parameter values --------------------------------------------------------

p = 10; 
beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_10.RDS")
library(purrr)

# Function -------------------------------------------------

get_divergence_diagnostics = function(out){
  #' Function to obtain divergence diagnostics
  div = map(out, 9); all_div = list();
  for(i in 1:length(div)){
    all_div[[i]] = sapply(div[[i]], sum)
  }
  num_div = sapply(all_div, sum)
  iter_idx = which(sapply(all_div, sum)!=0)
  return(list(num_div = num_div, sample_idx = iter_idx))
}

save_results = function(p, m , beta){

  out_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/convergence/results/m_",m,"out_1000.RDS"))
  out_2000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/convergence/results/m_",m,"out_2000.RDS"))
  out_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/convergence/results/m_",m,"out_5000.RDS"))
  out_10000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/convergence/results/m_",m,"out_10000.RDS"))
  
  out_list = list(out_1000, out_2000, out_5000, out_10000)
  
  # divergence diagnostics
  d1000 = get_divergence_diagnostics(out_1000)
  d2000 = get_divergence_diagnostics(out_2000)
  d5000 = get_divergence_diagnostics(out_5000)
  d10000 = get_divergence_diagnostics(out_10000)
  
  d = list(d1000$sample_idx, d2000$sample_idx, d5000$sample_idx, d10000$sample_idx)
  n = c(1000, 2000, 5000, 10000);
  RMSEs = NULL; ses =NULL; aurocs = NULL; aurocs_se = NULL;
  mses_50 = NULL; std_50 = NULL; f_mses_50 = NULL; f_std_50 = NULL;
  num_diverged = numeric(length(out_list)); avg_time = NULL; avg_iter=NULL;
  for(j in 1:length(d)){
    if(length(d[[j]])==0){
      
      res = metrics_new(out_list[[j]], beta, 1e-5)
      RMSEs[j] = res$av_mse; ses[j] = res$se;
      aurocs[j] = res$av_AUC; aurocs_se = res$auc_se;
      
      # Intercept estimates -----------------------------------------------------
      mses_50[j] = est_intercept(out_list[[j]], p, 0.1)$mse
      std_50[j] = est_intercept(out_list[[j]], p, 0.1)$err
      # F estimates -------------------------------------------------------------
      f_mses_50[j] = err_normal_f(out_list[[j]], p, n[j], m)$mse
      f_std_50[j] = err_normal_f(out_list[[j]], p,n[j],m)$err
      
      # iterations to convergence
      conv_iter = map(out_list[[j]], 7)
      avg_iter[j] = mean(Reduce("+", conv_iter)/length(conv_iter))
      
      # run-time/computational cost
      times = map(out_list[[j]], 6)
      avg_time[j] = mean(unlist(times))
      
      
    }
    
    if(length(d[[j]]!=0)){
      num_diverged[j] = length(d[[j]])
      # use only samples which did not diverge
      out = out_list[[j]][(-d[[j]])]
      # calculate results as normal
      res = metrics_new(out, beta, 1e-5)
      RMSEs[j] = res$av_mse; ses[j] = res$se;
      aurocs[j] = res$av_AUC; aurocs_se = res$auc_se;
      # Intercept estimates -----------------------------------------------------
      mses_50[j] = est_intercept(out, p, 0.1)$mse
      std_50[j] = est_intercept(out, p, 0.1)$err
      # F estimates -------------------------------------------------------------
      f_mses_50[j] = err_normal_f(out, p, n[j], m)$mse
      f_std_50[j] = err_normal_f(out, p,n[j],m)$err

      # iterations/timings ------------------------------------------------------
      # iterations to convergence
      conv_iter = map(out, 7)
      avg_iter[j] = mean(Reduce("+", conv_iter)/length(conv_iter))
      
      # run-time/computational cost
      times = map(out, 6)
      avg_time[j] = mean(unlist(times)) 
      
    }
  }
    
    
    setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/convergence/formatted_res")
  
    saveRDS(RMSEs, paste0("rmse_",m,"m.RDS"))
    saveRDS(ses, paste0("ses_",m,"m.RDS"))
    saveRDS(aurocs, paste0("AUC_",m, "m.RDS"))
    saveRDS(aurocs_se, paste0("AUC_se",m,"m.RDS"))
    saveRDS(num_diverged, paste0("div_",m,"m.RDS" ))
    
    # intercept
    saveRDS(mses_50, paste0("int_mses_",m,"m.RDS"))
    saveRDS(std_50, paste0("int_ses_",m ,"m.RDS"))
    # f
    saveRDS(f_mses_50, paste0("f_mses_",m,"m.RDS"))
    saveRDS(f_std_50, paste0("f_ses_", m, "m.RDS"))
    # iterations/timings
    saveRDS(avg_iter, paste0("avg_iter", m, "m.RDS"))
    saveRDS(avg_time, paste0("avg_time", m, "m.RDS"))
  return(num_diverged)
}
save_results(p, 3, beta)
save_results(p, 5, beta)
save_results(p, 10, beta)
save_results(p, 15, beta)
save_results(p, 20, beta)
save_results(p, 25, beta)
save_results(p, 30, beta)
save_results(p, 35, beta)
save_results(p, 40, beta)
