# Load libraries ----------------------------------------------------------

library(purrr)
setwd("~/Downloads/BAPLA/Synthetic_Experiments")
source("BAPLA_Functions.R")




# Parameter values --------------------------------------------------------

p = 10; 
beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_10.RDS")


# m = 3 -------------------------------------------------------------------

m = 10;

# load simulation results -------------------------------------------------

out_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_BAPLA/results/m_",m,"out_1000.RDS"))
out_2000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_BAPLA/results/m_",m,"out_2000.RDS"))
out_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_BAPLA/results/m_",m,"out_5000.RDS"))
out_10000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_BAPLA/results/m_",m,"out_10000.RDS"))

time_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_BAPLA/results/time_1000.RDS"))
time_2000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_BAPLA/results/time_2000.RDS"))
time_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_BAPLA/results/time_5000.RDS"))
time_10000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_BAPLA/results/time_10000.RDS"))


library(pROC)
res_1000 =metrics_new(out_1000, beta, 1e-5)
res_2000 =metrics_new(out_2000, beta,1e-5)
res_5000 =metrics_new(out_5000, beta,1e-5)
res_10000 =metrics_new(out_10000, beta,1e-5)


RMSEs = c(res_1000$av_mse, res_2000$av_mse,res_5000$av_mse, res_10000$av_mse)
ses = c(res_1000$se, res_2000$se,res_5000$se, res_10000$se)
aurocs = c(res_1000$av_AUC, res_2000$av_AUC,res_5000$av_AUC, res_10000$av_AUC)
aurocs_se = c(res_1000$auc_se, res_2000$auc_se,res_5000$auc_se, res_10000$auc_se)


setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_BAPLA/formatted_res")

saveRDS(RMSEs, paste0("rmse_",m,"m.RDS"))
saveRDS(ses, paste0("ses_",m,"m.RDS"))
saveRDS(aurocs, paste0("AUC_",m, "m.RDS"))
saveRDS(aurocs_se, paste0("AUC_se",m,"m.RDS"))



# Intercept estimates -----------------------------------------------------

# Errors

mses_50 = c(est_intercept(out_1000, 10, 0.1)$mse, est_intercept(out_2000, 10, 0.1)$mse, 
            est_intercept(out_5000, 10, 0.1)$mse, est_intercept(out_10000, 10, 0.1)$mse)
mses_50
std_50 = c(est_intercept(out_1000, 10, 0.1)$err, est_intercept(out_2000, 10, 0.1)$err, 
           est_intercept(out_5000, 10, 0.1)$err, est_intercept(out_10000, 10, 0.1)$err)
std_50


setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_BAPLA/formatted_res")

saveRDS(mses_50, paste0("int_mses_",m,"m.RDS"))
saveRDS(std_50, paste0("int_ses_",m ,"m.RDS"))


# plots

# library(ggplot2)
# # plots for High-firing model 
# p1 = plot_intercept_estimates2(out_1000, -1, 1, "n = 1000", 0.1, 10)
# p2 = plot_intercept_estimates2(out_2000, -1, 1, "n =  2000", 0.1, 10)
# p3 = plot_intercept_estimates2(out_5000, -1, 1, "n = 5000", 0.1, 10)
# p4 = plot_intercept_estimates2(out_10000, -1, 1, "n = 10000", 0.1, 10)
# library(ggpubr)
# ggarrange(p1,p2,p3,p4, ncol=4, common.legend =T)



# F estimates -------------------------------------------------------------


# get F estimates  --------------------------------------------------------------
library(splines)


f_hat_50_1000 = get_f(out_1000, m, 999, unit_id = 1)
f_hat_50_2000 = get_f(out_2000, m, 1999, unit_id = 1)
f_hat_50_5000 = get_f(out_5000, m, 4999, unit_id = 1)
f_hat_50_10000 = get_f(out_10000, m, 9999, unit_id = 1)


library(ggplot2)
library(matrixStats)


 f1 = plot_f_cis_RL(f_hat_50_1000, 1, "n = 1000",1000)
 f2 = plot_f_cis_RL(f_hat_50_2000, 1, "n = 2000",2000)
 f3 = plot_f_cis_RL(f_hat_50_5000, 1, "n = 5000",5000)
 f4 = plot_f_cis_RL(f_hat_50_10000, 1, "n = 10000",10000)

 # 
 library(ggpubr)
 print(ggarrange(f1,f2,f3,f4, ncol=4, common.legend =T))




#Errors

# errors - NEED TO UPDATE TO REFLECT NO F STRUCTURE
#f_mses_50 = c(err_gamma_f(out_1000, 10, 1000, m)$mse, err_gamma_f(out_2000, 10, 2000,m)$mse, 
#              err_gamma_f(out_5000, 10, 5000,m)$mse, err_gamma_f(out_10000, 10, 10000,m)$mse)
#f_mses_50
#f_std_50 = c(err_gamma_f(out_1000, 10,1000,m)$err, err_gamma_f(out_2000, 10, 2000,m)$err, 
#             err_gamma_f(out_5000, 10, 5000,m)$err, err_gamma_f(out_10000, 10, 10000,m)$err)
#f_std_50


#setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/RL/Sims_BAPLA/formatted_res")#

#saveRDS(f_mses_50, paste0("f_mses_",m,"m.RDS"))
#saveRDS(f_std_50, paste0("f_ses_", m, "m.RDS"))


