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
  
  
  # divergence diagnostics
  d1000 = get_divergence_diagnostics(out_1000)
  d2000 = get_divergence_diagnostics(out_2000)
  d5000 = get_divergence_diagnostics(out_5000)
  d10000 = get_divergence_diagnostics(out_10000)
  
  d = c(d1000$sample_idx, d2000$sample_idx, d5000$sample_idx, d10000$sample_idx)
  
  if(length(d)==0){
    
    res_1000 =metrics_new(out_1000, beta, 1e-5)
    res_2000 =metrics_new(out_2000, beta,1e-5)
    res_5000 =metrics_new(out_5000, beta,1e-5)
    res_10000 =metrics_new(out_10000, beta,1e-5)
    
    
    RMSEs = c(res_1000$av_mse, res_2000$av_mse,res_5000$av_mse, res_10000$av_mse)
    ses = c(res_1000$se, res_2000$se,res_5000$se, res_10000$se)
    aurocs = c(res_1000$av_AUC, res_2000$av_AUC,res_5000$av_AUC, res_10000$av_AUC)
    aurocs_se = c(res_1000$auc_se, res_2000$auc_se,res_5000$auc_se, res_10000$auc_se)
  }
  
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/convergence/formatted_res")
  
  saveRDS(RMSEs, paste0("rmse_",m,"m.RDS"))
  saveRDS(ses, paste0("ses_",m,"m.RDS"))
  saveRDS(aurocs, paste0("AUC_",m, "m.RDS"))
  saveRDS(aurocs_se, paste0("AUC_se",m,"m.RDS"))
  
}
save_results(3, 3, beta)
save_results(5, 3, beta)
save_results(10, 3, beta)
save_results(15, 3, beta)
save_results(20, 3, beta)
save_results(25, 3, beta)
  # Intercept estimates -----------------------------------------------------
  
  # Errors
  
  mses_50 = c(est_intercept(out_1000, p, 0.1)$mse, est_intercept(out_2000, p, 0.1)$mse, 
              est_intercept(out_5000, p, 0.1)$mse, est_intercept(out_10000, p, 0.1)$mse)
  mses_50
  std_50 = c(est_intercept(out_1000, p, 0.1)$err, est_intercept(out_2000, p, 0.1)$err, 
             est_intercept(out_5000, p, 0.1)$err, est_intercept(out_10000, p, 0.1)$err)
  std_50
  
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res")
  
  saveRDS(mses_50, paste0("int_mses_",m,"m.RDS"))
  saveRDS(std_50, paste0("int_ses_",m ,"m.RDS"))
  
  
  # F estimates -------------------------------------------------------------
  

  f_mses_50 = c(err_normal_f(out_1000, p, 1000, m)$mse, err_normal_f(out_2000, p, 2000,m)$mse, 
                err_normal_f(out_5000, p, 5000,m)$mse, err_normal_f(out_10000, p, 10000,m)$mse)
  f_mses_50
  f_std_50 = c(err_normal_f(out_1000, p,1000,m)$err, err_normal_f(out_2000, p, 2000,m)$err, 
               err_normal_f(out_5000, p, 5000,m)$err, err_normal_f(out_10000, p, 10000,m)$err)
  f_std_50
  
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res")
  
  saveRDS(f_mses_50, paste0("f_mses_",m,"m.RDS"))
  saveRDS(f_std_50, paste0("f_ses_", m, "m.RDS"))

}

save_results(10, 3, beta)
save_results(10, 5, beta)
save_results(10, 10, beta)
save_results(10, 15, beta)
save_results(10, 20, beta)

save_results(10, 25, beta)
save_results(10, 30, beta)
save_results(10, 35, beta)
save_results(10, 40, beta)


# Save all ----------------------------------------------------------------

save_all_res = function(p, m , beta){
  
  out_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/results/m_",m,"out_1000.RDS"))
  out_2000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/results/m_",m,"out_2000.RDS"))
  out_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/results/m_",m,"out_5000.RDS"))
  out_10000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/results/m_",m,"out_10000.RDS"))
  
  
  
  
  res_1000 =metrics_new(out_1000, beta, 1e-5)
  res_2000 =metrics_new(out_2000, beta,1e-5)
  res_5000 =metrics_new(out_5000, beta,1e-5)
  res_10000 =metrics_new(out_10000, beta,1e-5)
  
  
  RMSEs = list(res_1000$all, res_2000$all,res_5000$all, res_10000$all)

  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res")
  
  saveRDS(RMSEs, paste0("rmse_all",m,"m.RDS"))
  

  
  
  # F estimates -------------------------------------------------------------
  
  
  f_mses_50 = list(err_normal_f(out_1000, p, 1000, m)$all, err_normal_f(out_2000, p, 2000,m)$all, 
                err_normal_f(out_5000, p, 5000,m)$all, err_normal_f(out_10000, p, 10000,m)$all)
  f_mses_50

  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res")
  
  saveRDS(f_mses_50, paste0("f_mses_all",m,"m.RDS"))
  
}


save_all_res(10, 3, beta)
save_all_res(10, 5, beta)
save_all_res(10, 10, beta)
save_all_res(10, 15, beta)
save_all_res(10, 20, beta)

save_all_res(10, 25, beta)
save_all_res(10, 30, beta)
save_all_res(10, 35, beta)
save_all_res(10, 40, beta)
