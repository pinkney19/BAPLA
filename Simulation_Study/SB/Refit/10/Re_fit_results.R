
# libraries ---------------------------------------------------------------

library(purrr)
library(splines)

setwd("~/Downloads/BAPLA/Simulation_Study")
source("Functions_BAPLA.R")

refit_50_1000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/Refit/10/refit_50_1000.RDS")
refit_50_2000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/Refit/10/refit_50_2000.RDS")
refit_50_5000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/Refit/10/refit_50_5000.RDS")
refit_50_10000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/Refit/10/refit_50_10000.RDS")

refit_zero_1000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/Refit/10/refit_zero_1000.RDS")
refit_zero_2000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/Refit/10/refit_zero_2000.RDS")
refit_zero_5000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/Refit/10/refit_zero_5000.RDS")
refit_zero_10000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/Refit/10/refit_zero_10000.RDS")

# truth -------------------------------------------------------------------

p = 10; 
beta <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/beta_10.RDS")


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


setwd("~/Downloads/BAPLA/Simulation_Study/SB/Refit/10")
saveRDS(RMSEs_50, "rmse_50.RDS")
saveRDS(ses_50,"ses_50.RDS")


saveRDS(RMSEs_zero, "rmse_zero.RDS")
saveRDS(ses_zero,"ses_zero.RDS")






# Intercept estimates -----------------------------------------------------

beta_0_boxplots2 = function(beta_0_res, p, title, true_beta_0, a, b){
  #'  Function to plot intercept estimates for each unit across monte carlo samples
  #'
  #' @param beta_0_res nested list of intercept estimates from monte carlo sim study
  #' @param p dimension
  #' @param title title of plot
  #' @param true_beta_0 true intercept in model specification
  #' @export  
  beta0_hat = list(); beta0_hat = rep(list(beta0_hat), p)
  for(j in 1:p){
    beta0_hat[[j]] = unlist(map(beta_0_res, j))
  }
  
  df <- do.call(data.frame, beta0_hat)
  names(df) = seq(1,p)
  
  
  # test with ggplot
  df_long <- reshape2::melt(df, id.vars = NULL,variable.name = "Variable", value.name = "Value")
  
  # Plot using ggplot2
  ggplot(df_long, aes(x = as.factor(Variable), y = Value)) +
    geom_boxplot(fill = "skyblue") +
    geom_hline(yintercept = true_beta_0, color = "navy", linetype = "dashed", size = 1) +
    labs(title = title, x = expression(p), y = expression(hat(beta[0]))) +
    ylim(a, b) +
    theme_minimal()
  
}


plot_intercept_estimates2 = function(out, a, b, title, true_beta_0){
  beta_0_hats = map(out, 2)
  beta_0_boxplots2(beta_0_hats, 10, title, true_beta_0, a, b)
}

library(ggplot2)

plot_intercept_estimates2(refit_zero_1000, -3, -1, "zero - 1000", -3.6)
plot_intercept_estimates2(refit_zero_2000, -3, -1, "zero - 2000", -3.6)
plot_intercept_estimates2(refit_zero_5000, -3, -1, "zero - 5000", -3.6)
plot_intercept_estimates2(refit_zero_10000, -3, -1, "zero - 10000", -3.6)


plot_intercept_estimates2(refit_50_1000, -2, 4, "50 - 1000", 0.1)
plot_intercept_estimates2(refit_50_2000, -2, 4, "50 - 2000", 0.1)
plot_intercept_estimates2(refit_50_5000, -2, 4, "50 - 5000", 0.1)
plot_intercept_estimates2(refit_50_10000, -2, 4, "50 - 10000", 0.1)




