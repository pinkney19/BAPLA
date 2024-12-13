# Load libraries ----------------------------------------------------------

library(purrr)
setwd("~/Downloads/BAPLA/Simulation_Study")
source("Functions_BAPLA.R")




# Parameter values --------------------------------------------------------

p = 50; 
beta <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/beta_50.RDS")



# load simulation results -------------------------------------------------
out_50_1000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50/out_50_1000.RDS")
out_50_2000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50/out_50_2000.RDS")
out_50_5000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50/out_50_5000.RDS")
out_50_10000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50/out_50_10000.RDS")

out_zero_1000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50/out_zero_1000.RDS")
out_zero_2000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50/out_zero_2000.RDS")
out_zero_5000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50/out_zero_5000.RDS")
out_zero_10000 <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50/out_zero_10000.RDS")

# new way -----------------------------------------------------------------
library(pROC)
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



setwd("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/50")
saveRDS(RMSEs_50, "rmse_50.RDS")
saveRDS(ses_50,"ses_50.RDS")
saveRDS(aurocs_50, "AUC_50.RDS")
saveRDS(aurocs_se_50, "AUC_se_50.RDS")

saveRDS(RMSEs_zero, "rmse_zero.RDS")
saveRDS(ses_zero,"ses_zero.RDS")
saveRDS(aurocs_zero, "AUC_zero.RDS")
saveRDS(aurocs_se_zero, "AUC_se_zero.RDS")




# old way -----------------------------------------------------------------


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
  beta_0_boxplots2(beta_0_hats, p = 50, title, true_beta_0, a, b)
}

library(ggplot2)

p1 = plot_intercept_estimates2(out_50_1000, -10, 0, "50 - 1000", -2.6)
p2 = plot_intercept_estimates2(out_50_2000, -10, 0, "50 - 2000", -2.6)
p3 = plot_intercept_estimates2(out_50_5000, -10, 0, "50 - 5000", -2.6)
p4 = plot_intercept_estimates2(out_50_10000, -10, 0, "50 - 10000", -2.6)
library(ggpubr)
ggarrange(p1,p2,p3,p4, ncol=4, common.legend =T)


# F estimates -------------------------------------------------------------


# check from github
list_to_matrix = function(f){
  mat_f = matrix(NA, ncol=length(f[[1]]), nrow = length(f))
  for(i in 1:100){
    mat_f[i,]=f[[i]]
  }
  return(mat_f)
}


# get F estimates  --------------------------------------------------------------
library(splines)
get_f = function(out,m, t_end, unit_id){
  #' Function to obtain f estimate with non-centred basis functions but estimated coefficients
  #'
  #' @param out RDS file from STORM
  #' @param m number of basis functions
  #' @param t_end dimension of previous values
  #' @param unit_id index for considered dimension
  #' @export  
  phi <- bs(seq(1, t_end), df=m, intercept = F) 
  c_hats = map(out, 4)
  c_hats_id = map(c_hats, unit_id)
  f = lapply(c_hats_id, function(x) phi%*%as.matrix(x))
  
  return(f)
}


f_hat_50_1000 = get_f(out_50_1000, 10, 999, unit_id = 1)
f_hat_50_2000 = get_f(out_50_2000, 10, 1999, unit_id = 1)
f_hat_50_5000 = get_f(out_50_5000, 10, 4999, unit_id = 1)
f_hat_50_10000 = get_f(out_50_10000, 10, 9999, unit_id = 1)



plot_f_cis = function(f, unit_id, title,N, const){
  
  #f1 = map(f,unit_id )
  check = list_to_matrix(f)
  library(matrixStats)
  plot( colQuantiles(check, probs=0.5), ylim=c(-2,6), type='l', xlab="t", 
        ylab=expression(hat(f)(t)), main = title)
  lines(colQuantiles(check, probs =0.975), col="red")
  lines( colQuantiles(check, probs=0.025), col="red")
  
  
  lines(const*dnorm(seq(1,(N-1)), mean = (N)/2, sd=(N+1000)/10), col="blue", lwd =2, lty = "dashed")
  legend("topright", c("truth", "95% CIs", "Avg"), col=c("blue", "red", "black"), 
         lty = c(2,1,1), lwd = c(2,1,1), cex = 0.6)
  
  #return(check)
  
}
par(mfrow=c(1,4), mar = c(5.5, 4.5, 4.5, 2.5))
plot_f_cis(f_hat_50_1000, 1, "50 - 1000",1000, 1000)
plot_f_cis(f_hat_50_2000, 1, "50 - 2000",2000, 2000)
plot_f_cis(f_hat_50_5000, 1, "50 - 5000",5000, 5000)
plot_f_cis(f_hat_50_10000, 1, "50 - 10000",10000, 10000)

