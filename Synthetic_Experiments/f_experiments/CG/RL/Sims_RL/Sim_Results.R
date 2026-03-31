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


# Function -------------------------------------------------



out_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_RL/results/out_1000.RDS")
out_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_RL/results/out_2000.RDS")
out_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_RL/results/out_5000.RDS")
out_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_RL/results/out_10000.RDS")
library(pracma)
res_1000 =metrics_new(out_1000, beta, 1e-5)
res_2000 =metrics_new(out_2000, beta,1e-5)
res_5000 =metrics_new(out_5000, beta,1e-5)
res_10000 =metrics_new(out_10000, beta,1e-5)


RMSEs = c(res_1000$av_mse, res_2000$av_mse,res_5000$av_mse, res_10000$av_mse)
ses = c(res_1000$se, res_2000$se,res_5000$se, res_10000$se)
aurocs = c(res_1000$av_AUC, res_2000$av_AUC,res_5000$av_AUC, res_10000$av_AUC)
aurocs_se = c(res_1000$auc_se, res_2000$auc_se,res_5000$auc_se, res_10000$auc_se)


setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_RL/formatted_res")

saveRDS(RMSEs, "rmse.RDS")
saveRDS(ses, "ses.RDS")
saveRDS(aurocs,"AUC.RDS")
saveRDS(aurocs_se, "AUC_se.RDS")
  
  

# Intercept estimates -----------------------------------------------------

# Errors

mses_50 = c(est_intercept(out_1000, p, 0.1)$mse, est_intercept(out_2000, p, 0.1)$mse, 
            est_intercept(out_5000, p, 0.1)$mse, est_intercept(out_10000, p, 0.1)$mse)
mses_50
std_50 = c(est_intercept(out_1000, p, 0.1)$err, est_intercept(out_2000, p, 0.1)$err, 
           est_intercept(out_5000, p, 0.1)$err, est_intercept(out_10000, p, 0.1)$err)
std_50


setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_RL/formatted_res")

saveRDS(mses_50, "int_mses.RDS")
saveRDS(std_50, "int_ses.RDS")



  
out_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_RL/results/out_1000.RDS")
out_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_RL/results/out_2000.RDS")
out_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_RL/results/out_5000.RDS")
out_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_RL/results/out_10000.RDS")




res_1000 =metrics_new(out_1000, beta, 1e-5)
res_2000 =metrics_new(out_2000, beta,1e-5)
res_5000 =metrics_new(out_5000, beta,1e-5)
res_10000 =metrics_new(out_10000, beta,1e-5)
  
  
RMSEs = list(res_1000$all, res_2000$all,res_5000$all, res_10000$all)
RMSEs
  
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_RL/formatted_res")

saveRDS(RMSEs, "rmse_all.RDS")
  

