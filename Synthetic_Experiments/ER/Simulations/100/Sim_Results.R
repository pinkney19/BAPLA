# Load libraries ----------------------------------------------------------

library(purrr)
setwd("~/Downloads/BAPLA/Synthetic_Experiments")
source("BAPLA_Functions.R")




# Parameter values --------------------------------------------------------

p = 100; 
beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/ER/beta_100.RDS")



# load simulation results -------------------------------------------------
out_50_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/100/out_50_1000.RDS")
out_50_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/100/out_50_2000.RDS")
out_50_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/100/out_50_5000.RDS")
out_50_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/100/out_50_10000.RDS")

out_zero_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/100/out_zero_1000.RDS")
out_zero_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/100/out_zero_2000.RDS")
out_zero_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/100/out_zero_5000.RDS")
out_zero_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/100/out_zero_10000.RDS")

# new way -----------------------------------------------------------------
library(pROC)
library(pracma)
res_50_1000 =metrics_new(out_50_1000, beta, 1e-5)
res_50_2000 =metrics_new(out_50_2000, beta,1e-5)
res_50_5000 =metrics_new(out_50_5000, beta,1e-5)
res_50_10000 =metrics_new(out_50_10000, beta,1e-5)

res_zero_1000 =metrics_new(out_zero_1000, beta, 1e-5)
res_zero_2000 =metrics_new(out_zero_2000, beta,1e-5)
res_zero_5000 =metrics_new(out_zero_5000, beta,1e-5)
res_zero_10000 =metrics_new(out_zero_10000, beta,1e-5)

RMSEs_50 = c(res_50_1000$av_mse, res_50_2000$av_mse,res_50_5000$av_mse, res_50_10000$av_mse)
ses_50 = c(res_50_1000$se, res_50_2000$se,res_50_5000$se, res_50_10000$se)
aurocs_50 = c(res_50_1000$av_AUC, res_50_2000$av_AUC,res_50_5000$av_AUC, res_50_10000$av_AUC)
aurocs_se_50 = c(res_50_1000$auc_se, res_50_2000$auc_se,res_50_5000$auc_se, res_50_10000$auc_se)

RMSEs_zero = c(res_zero_1000$av_mse, res_zero_2000$av_mse,res_zero_5000$av_mse, res_zero_10000$av_mse)
ses_zero = c(res_zero_1000$se, res_zero_2000$se,res_zero_5000$se, res_zero_10000$se)
aurocs_zero = c(res_zero_1000$av_AUC, res_zero_2000$av_AUC,res_zero_5000$av_AUC, res_zero_10000$av_AUC)
aurocs_se_zero = c(res_zero_1000$auc_se, res_zero_2000$auc_se,res_zero_5000$auc_se, res_zero_10000$auc_se)



setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/100")
saveRDS(RMSEs_50, "rmse_50.RDS")
saveRDS(ses_50,"ses_50.RDS")
saveRDS(aurocs_50, "AUC_50.RDS")
saveRDS(aurocs_se_50, "AUC_se_50.RDS")

saveRDS(RMSEs_zero, "rmse_zero.RDS")
saveRDS(ses_zero,"ses_zero.RDS")
saveRDS(aurocs_zero, "AUC_zero.RDS")
saveRDS(aurocs_se_zero, "AUC_se_zero.RDS")




# Intercept estimates -----------------------------------------------------

# Errors


mses_50 = c(est_intercept(out_50_1000, p, 0.1)$mse, est_intercept(out_50_2000, p, 0.1)$mse, 
            est_intercept(out_50_5000, p, 0.1)$mse, est_intercept(out_50_10000, p, 0.1)$mse)
mses_50
std_50 = c(est_intercept(out_50_1000, p, 0.1)$err, est_intercept(out_50_2000, p, 0.1)$err, 
           est_intercept(out_50_5000, p, 0.1)$err, est_intercept(out_50_10000, p, 0.1)$err)
std_50


mses_zero = c(est_intercept(out_zero_1000, p, -2.6)$mse, est_intercept(out_zero_2000, p, -2.6)$mse, 
              est_intercept(out_zero_5000, p, -2.6)$mse, est_intercept(out_zero_10000, p, -2.6)$mse)
mses_zero
std_zero = c(est_intercept(out_zero_1000, p, -2.6)$err, est_intercept(out_50_2000, p, -2.6)$err, 
             est_intercept(out_zero_5000, p, -2.6)$err, est_intercept(out_zero_10000, p, -2.6)$err)
std_zero

setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/100")
saveRDS(mses_50, "int_mses_50.RDS")
saveRDS(std_50,"int_ses_50.RDS")

saveRDS(mses_zero, "int_mses_zero.RDS")
saveRDS(std_zero,"int_ses_zero.RDS")

# Plots
library(ggplot2)
# plots for High-firing model 
p1 = plot_intercept_estimates2(out_50_1000, -3, 3, "n = 1000", 0.1,p)
p2 = plot_intercept_estimates2(out_50_2000, -3, 3, "n =  2000", 0.1,p)
p3 = plot_intercept_estimates2(out_50_5000, -3, 3, "n = 5000", 0.1,p)
p4 = plot_intercept_estimates2(out_50_10000, -3, 3, "n = 10000", 0.1,p)
library(ggpubr)
ggarrange(p1,p2,p3,p4, ncol=4, common.legend =T)

# -2.6 

p1 = plot_intercept_estimates2(out_zero_1000, -5, 0, "n = 1000", -2.6,p)
p2 = plot_intercept_estimates2(out_zero_2000, -5, 0, "n =  2000", -2.6,p)
p3 = plot_intercept_estimates2(out_zero_5000, -5, 0, "n = 5000", -2.6,p)
p4 = plot_intercept_estimates2(out_zero_10000, -5, 0, "n = 10000", -2.6,p)
library(ggpubr)
ggarrange(p1,p2,p3,p4, ncol=4, common.legend =T)


# F estimates -------------------------------------------------------------


library(splines)

f_hat_50_1000 = get_f(out_50_1000, 10, 999, unit_id = 1)
f_hat_50_2000 = get_f(out_50_2000, 10, 1999, unit_id = 1)
f_hat_50_5000 = get_f(out_50_5000, 10, 4999, unit_id = 1)
f_hat_50_10000 = get_f(out_50_10000, 10, 9999, unit_id = 1)





library(ggplot2)
library(matrixStats)



f1 = plot_f_cis_gamma(f_hat_50_1000, 1, "n = 1000",1000)
f2 = plot_f_cis_gamma(f_hat_50_2000, 1, "n = 2000",2000)
f3 = plot_f_cis_gamma(f_hat_50_5000, 1, "n = 5000",5000)
f4 = plot_f_cis_gamma(f_hat_50_10000, 1, "n = 10000",10000)

ggarrange(f1,f2,f3,f4, ncol=4, common.legend =T)

f_hat_zero_1000 = get_f(out_zero_1000, 10, 999, unit_id = 1)
f_hat_zero_2000 = get_f(out_zero_2000, 10, 1999, unit_id = 1)
f_hat_zero_5000 = get_f(out_zero_5000, 10, 4999, unit_id = 1)
f_hat_zero_10000 = get_f(out_zero_10000, 10, 9999, unit_id = 1)

f1 = plot_f_cis_gamma(f_hat_zero_1000, 1, "n = 1000",1000)
f2 = plot_f_cis_gamma(f_hat_zero_2000, 1, "n = 2000",2000)
f3 = plot_f_cis_gamma(f_hat_zero_5000, 1, "n = 5000",5000)
f4 = plot_f_cis_gamma(f_hat_zero_10000, 1, "n = 10000",10000)

ggarrange(f1,f2,f3,f4, ncol=4, common.legend =T)


# Errors

m = 10;

f_mses_50 = c(err_gamma_f(out_50_1000, p, 1000, m)$mse, err_gamma_f(out_50_2000, p, 2000,m)$mse, 
              err_gamma_f(out_50_5000, p, 5000,m)$mse, err_gamma_f(out_50_10000, p, 10000,m)$mse)
f_mses_50
f_std_50 = c(err_gamma_f(out_50_1000, p,1000,m)$err, err_gamma_f(out_50_2000, p, 2000,m)$err, 
             err_gamma_f(out_50_5000, p, 5000,m)$err, err_gamma_f(out_50_10000, p, 10000,m)$err)
f_std_50


f_mses_zero = c(err_gamma_f(out_zero_1000, p, 1000,m)$mse, err_gamma_f(out_zero_2000, p, 2000,m)$mse, 
                err_gamma_f(out_zero_5000, p, 5000,m)$mse, err_gamma_f(out_zero_10000, p, 10000,m)$mse)
f_mses_zero
f_std_zero = c(err_gamma_f(out_zero_1000, p, 1000,m)$err, err_gamma_f(out_50_2000, p, 2000,m)$err, 
               err_gamma_f(out_zero_5000, p, 5000,m)$err, err_gamma_f(out_zero_10000, p, 10000,m)$err)
f_std_zero


setwd("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulations/100")
saveRDS(f_mses_50, "f_mses_50.RDS")
saveRDS(f_std_50,"f_ses_50.RDS")

saveRDS(f_mses_zero, "f_mses_zero.RDS")
saveRDS(f_std_zero,"f_ses_zero.RDS")




