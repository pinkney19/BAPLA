
# libraries ---------------------------------------------------------------

library(purrr)
library(splines)

setwd("~/Downloads/BAPLA/Synthetic_Experiments")
source("BAPLA_Functions.R")

refit_50_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Refit/50/refit_50_1000.RDS")
refit_50_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Refit/50/refit_50_2000.RDS")
refit_50_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Refit/50/refit_50_5000.RDS")
refit_50_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Refit/50/refit_50_10000.RDS")

refit_zero_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Refit/50/refit_zero_1000.RDS")
refit_zero_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Refit/50/refit_zero_2000.RDS")
refit_zero_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Refit/50/refit_zero_5000.RDS")
refit_zero_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Refit/50/refit_zero_10000.RDS")

# truth -------------------------------------------------------------------

p = 50; 
beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_50.RDS")


# new way -----------------------------------------------------------------

res_50_1000 =metrics_new(refit_50_1000, beta, 1e-5)
res_50_2000 =metrics_new(refit_50_2000, beta,1e-5)
res_50_5000 =metrics_new(refit_50_5000, beta,1e-5)
res_50_10000 =metrics_new(refit_50_10000, beta,1e-5)

RMSEs_50 = c(res_50_1000$av_mse, res_50_2000$av_mse,res_50_5000$av_mse, res_50_10000$av_mse)
ses_50 = c(res_50_1000$se, res_50_2000$se,res_50_5000$se, res_50_10000$se)



res_zero_1000 =metrics_new(refit_zero_1000, beta,1e-5)
res_zero_2000 =metrics_new(refit_zero_2000, beta,1e-5)
res_zero_5000 =metrics_new(refit_zero_5000, beta,1e-5)
res_zero_10000 =metrics_new(refit_zero_10000, beta,1e-5)

RMSEs_zero = c(res_zero_1000$av_mse, res_zero_2000$av_mse,res_zero_5000$av_mse, res_zero_10000$av_mse)
ses_zero = c(res_zero_1000$se, res_zero_2000$se,res_zero_5000$se, res_zero_10000$se)



setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Refit/50")
saveRDS(RMSEs_50, "rmse_50.RDS")
saveRDS(ses_50,"ses_50.RDS")
saveRDS(RMSEs_zero, "rmse_zero.RDS")
saveRDS(ses_zero,"ses_zero.RDS")


# Intercept ---------------------------------------------------------------




library(ggplot2)
# plots for High-firing model 
p1 = plot_intercept_estimates2(refit_50_1000, -1, 1, "n = 1000", 0.1,50)
p2 = plot_intercept_estimates2(refit_50_2000, -1, 1, "n =  2000", 0.1,50)
p3 = plot_intercept_estimates2(refit_50_5000, -1, 1, "n = 5000", 0.1,50)
p4 = plot_intercept_estimates2(refit_50_10000, -1, 1, "n = 10000", 0.1,50)
library(ggpubr)
ggarrange(p1,p2,p3,p4, ncol=4, common.legend =T)



p5 = plot_intercept_estimates2(refit_zero_1000, -5, -1, "zero - 1000", -2.6, 50)
p6 = plot_intercept_estimates2(refit_zero_2000, -5, -1, "zero - 2000", -2.6,50)
p7 = plot_intercept_estimates2(refit_zero_5000, -5, -1, "zero - 5000", -2.6,50)
p8 = plot_intercept_estimates2(refit_zero_10000, -5, -1, "zero - 10000", -2.6,50)
ggarrange(p5,p6,p7,p8, ncol=4, common.legend =T)


mses_50 = c(est_intercept(refit_50_1000, 50, 0.1)$mse, est_intercept(refit_50_2000, 50, 0.1)$mse, 
            est_intercept(refit_50_5000, 50, 0.1)$mse, est_intercept(refit_50_10000, 50, 0.1)$mse)
mses_50
std_50 = c(est_intercept(refit_50_1000, 50, 0.1)$err, est_intercept(refit_50_2000, 50, 0.1)$err, 
           est_intercept(refit_50_5000, 50, 0.1)$err, est_intercept(refit_50_10000, 50, 0.1)$err)
std_50


mses_zero = c(est_intercept(refit_zero_1000, 50, -2.6)$mse, est_intercept(refit_zero_2000, 50, -2.6)$mse, 
              est_intercept(refit_zero_5000, 50, -2.6)$mse, est_intercept(refit_zero_10000, 50, -2.6)$mse)
mses_zero
std_zero = c(est_intercept(refit_zero_1000, 50, -2.6)$err, est_intercept(refit_50_2000, 50, -2.6)$err, 
             est_intercept(refit_zero_5000, 50, -2.6)$err, est_intercept(refit_zero_10000, 50, -2.6)$err)
std_zero

setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Refit/50")
saveRDS(mses_50, "int_mses_50.RDS")
saveRDS(std_50,"int_ses_50.RDS")

saveRDS(mses_zero, "int_mses_zero.RDS")
saveRDS(std_zero,"int_ses_zero.RDS")


# F estimates -------------------------------------------------------------

f_hat_50_1000 = get_f(refit_50_1000, 10, 999, unit_id = 1)
f_hat_50_2000 = get_f(refit_50_2000, 10, 1999, unit_id = 1)
f_hat_50_5000 = get_f(refit_50_5000, 10, 4999, unit_id = 1)
f_hat_50_10000 = get_f(refit_50_10000, 10, 9999, unit_id = 1)

f_hat_zero_1000 = get_f(refit_zero_1000, 10, 999, unit_id = 1)
f_hat_zero_2000 = get_f(refit_zero_2000, 10, 1999, unit_id = 1)
f_hat_zero_5000 = get_f(refit_zero_5000, 10, 4999, unit_id = 1)
f_hat_zero_10000 = get_f(refit_zero_10000, 10, 9999, unit_id = 1)



library(ggplot2)
library(matrixStats)


f1 = plot_f_cis_normal(f_hat_50_1000, 1, "n = 1000",1000)
f2 = plot_f_cis_normal(f_hat_50_2000, 1, "n = 2000",2000)
f3 = plot_f_cis_normal(f_hat_50_5000, 1, "n = 5000",5000)
f4 = plot_f_cis_normal(f_hat_50_10000, 1, "n = 10000",10000)

ggarrange(f1,f2,f3,f4, ncol=4, common.legend =T)


f5 = plot_f_cis_normal(f_hat_zero_1000, 1, "n = 1000",1000)
f6 = plot_f_cis_normal(f_hat_zero_2000, 1, "n = 2000",2000)
f7 = plot_f_cis_normal(f_hat_zero_5000, 1, "n = 5000",5000)
f8 = plot_f_cis_normal(f_hat_zero_10000, 1, "n = 10000",10000)

ggarrange(f5,f6,f7,f8, ncol=4, common.legend =T)



# errors 
f_mses_50 = c(err_normal_f(refit_50_1000, 10, 1000)$mse, err_normal_f(refit_50_2000, 10, 2000)$mse, 
              err_normal_f(refit_50_5000, 10, 5000)$mse, err_normal_f(refit_50_10000, 10, 10000)$mse)
f_mses_50
f_std_50 = c(err_normal_f(refit_50_1000, 10,1000)$err, err_normal_f(refit_50_2000, 10, 2000)$err, 
             err_normal_f(refit_50_5000, 10, 5000)$err, err_normal_f(refit_50_10000, 10, 10000)$err)
f_std_50


f_mses_zero = c(err_normal_f(refit_zero_1000, 10, 1000)$mse, err_normal_f(refit_zero_2000, 10, 2000)$mse, 
                err_normal_f(refit_zero_5000, 10, 5000)$mse, err_normal_f(refit_zero_10000, 10, 10000)$mse)
f_mses_zero
f_std_zero = c(err_normal_f(refit_zero_1000, 10, 1000)$err, err_normal_f(refit_50_2000, 10, 2000)$err, 
               err_normal_f(refit_zero_5000, 10, 5000)$err, err_normal_f(refit_zero_10000, 10, 10000)$err)
f_std_zero

setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Refit/50")
saveRDS(f_mses_50, "f_mses_50.RDS")
saveRDS(f_std_50,"f_ses_50.RDS")

saveRDS(f_mses_zero, "f_mses_zero.RDS")
saveRDS(f_std_zero,"f_ses_zero.RDS")
