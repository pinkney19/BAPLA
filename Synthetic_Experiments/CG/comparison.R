
# Intercept Errors --------------------------------------------------------

library(purrr)
library(splines)
library(pracma)
library(ggplot2)
library(ggplot2)

setwd("~/Downloads/BAPLA/Synthetic_Experiments")
source("BAPLA_Functions.R")

plot_intercept_errs = function(model_type, p, zero_inf, title){
  #' Function to plot comparison of intercept errors for stage 1 vs stage 2
  #' @param model_type (string) "CG", "ER" or "SB"
  #' @param p (scalar)
  #' @param zero_inf (T/F) indicating if zero-inflated model or not
  #' @param title (string) title for plot
  #' 
  
  if(zero_inf==F){
    # means 50/50 output required
    refit_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_50_1000.RDS"))
    refit_2000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_50_2000.RDS"))
    refit_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_50_5000.RDS"))
    refit_10000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_50_10000.RDS"))
    
    out_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_50_1000.RDS"))
    out_2000 <-  readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_50_2000.RDS"))
    out_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_50_5000.RDS"))
    out_10000 <-  readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_50_10000.RDS"))
    
    sim = cbind(est_intercept(out_1000, p, 0.1)$all, est_intercept(out_2000, p, 0.1)$all,
                est_intercept(out_5000, p, 0.1)$all,est_intercept(out_10000, p, 0.1)$all)
    
    refit =cbind(est_intercept(refit_1000, p, 0.1)$all,  est_intercept(refit_2000, p, 0.1)$all,
                 est_intercept(refit_5000, p, 0.1)$all, est_intercept(refit_10000, p, 0.1)$all)
  }
  
  if(zero_inf==T){
    # means zero-inflated output required
    refit_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_zero_1000.RDS"))
    refit_2000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_zero_2000.RDS"))
    refit_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_zero_5000.RDS"))
    refit_10000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_zero_10000.RDS"))
    
    out_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_zero_1000.RDS"))
    out_2000 <-  readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_zero_2000.RDS"))
    out_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_zero_5000.RDS"))
    out_10000 <-  readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_zero_10000.RDS"))
    
    sim = cbind(est_intercept(out_1000, p, -2.6)$all, est_intercept(out_2000, p, -2.6)$all,
                est_intercept(out_5000, p, -2.6)$all,est_intercept(out_10000, p, -2.6)$all)
    
    refit =cbind(est_intercept(refit_1000, p, -2.6)$all,  est_intercept(refit_2000, p, -2.6)$all,
                 est_intercept(refit_5000, p, -2.6)$all, est_intercept(refit_10000, p, -2.6)$all)
  }

 

  # Convert matrices to data frames and add a column identifier
  sim_df <- as.data.frame(sim)
  names(sim_df) = c("1000", "2000", "5000", "10,000")
  sim_df$type <- 'Stage 1'
  sim_df$id <- 1:nrow(sim_df)
  
  refit_df <- as.data.frame(refit)
  names(refit_df) = c("1000", "2000", "5000", "10,000")
  refit_df$type <- 'Stage 2'
  refit_df$id <- 1:nrow(refit_df)
  
  # Combine the data frames
  data <- rbind(sim_df, refit_df)
  
  # Melt the data frame to long format
  data_long <- melt(data, id.vars = c('type', 'id'))
  
  # Create the plot
  ggplot(data_long, aes(x = variable, y = value, fill = type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("Stage 1" = "red", "Stage 2" = "blue")) +
    ggtitle(title) +
    labs(x = "n", y = "Average MSE across p") +  # Change x and y labels 
    theme_minimal()
  
}

p1 = plot_intercept_errs("CG", 10, F, "50/50 (p=10)")
p2 = plot_intercept_errs("CG", 10, T, "ZI (p=10)")
p3 = plot_intercept_errs("CG", 50, F, "50/50 (p=50)")
p4 = plot_intercept_errs("CG", 50, T, "ZI (p=50)")

library(ggpubr)
plot = ggarrange(p1,p2,p3,p4, ncol=2, nrow = 2, common.legend = T)
print(annotate_figure(plot, "Chain Graph"))

# ER GRaph 
p1 = plot_intercept_errs("ER", 10, F, "50/50 (p=10)")
p2 = plot_intercept_errs("ER", 10, T, "ZI (p=10)")
p3 = plot_intercept_errs("ER", 50, F, "50/50 (p=50)")
p4 = plot_intercept_errs("ER", 50, T, "ZI (p=50)")

library(ggpubr)
plot = ggarrange(p1,p2,p3,p4, ncol=2, nrow = 2, common.legend = T)
print(annotate_figure(plot, "ER Graph"))


# SB model

p1 = plot_intercept_errs("SB", 10, F, "50/50 (p=10)")
p2 = plot_intercept_errs("SB", 10, T, "ZI (p=10)")
p3 = plot_intercept_errs("SB", 50, F, "50/50 (p=50)")
p4 = plot_intercept_errs("SB", 50, T, "ZI (p=50)")

library(ggpubr)
plot = ggarrange(p1,p2,p3,p4, ncol=2, nrow = 2, common.legend = T)
print(annotate_figure(plot, "SB Model"))


# lambdas -----------------------------------------------------------------

lams_50_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/10/lams_50_1000.RDS")
lams_50_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/10/lams_50_2000.RDS")
lams_50_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/10/lams_50_5000.RDS")
lams_50_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/10/lams_50_10000.RDS")

lams_zero_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/10/lams_zero_1000.RDS")
lams_zero_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/10/lams_zero_2000.RDS")
lams_zero_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/10/lams_zero_5000.RDS")
lams_zero_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/10/lams_zero_10000.RDS")

lams_50 = cbind(lams_50_1000, lams_50_2000, lams_50_5000, lams_50_10000)

lams_zero = cbind(lams_zero_1000, lams_zero_2000, lams_zero_5000, lams_zero_10000)

df_50 <- as.data.frame(lams_50)
names(df_50) = c("1000", "2000", "5000", "10,000")
df_50$type <- '50/50'
df_50$id <- 1:nrow(df_50)

df_zero <- as.data.frame(lams_zero)
names(df_zero) = c("1000", "2000", "5000", "10,000")
df_zero$type <- 'Zero-Inflated'
df_zero$id <- 1:nrow(df_zero)

# Combine the data frames
data <- rbind(df_50, df_zero)

# Melt the data frame to long format
data_long <- melt(data, id.vars = c('type', 'id'))

# Create the plot
ggplot(data_long, aes(x = variable, y = value, fill = type)) +
  geom_boxplot() +
  scale_fill_manual(values = c("50/50" = "lightgreen", "Zero-Inflated" = "lightpink")) +
  ggtitle("Regularisation Parameter") +
  labs(x = "n", y = "Average lambda") +  # Change x and y labels 
  theme_minimal()



# f errors ----------------------------------------------------------------

plot_f_errs = function(model_type, p, zero_inf, title){
  #' Function to plot comparison of intercept errors for stage 1 vs stage 2
  #' @param model_type (string) "CG", "ER" or "SB"
  #' @param p (scalar)
  #' @param zero_inf (T/F) indicating if zero-inflated model or not
  #' @param title (string) title for plot
  #' 
  
  if(zero_inf==F){
    # means 50/50 output required
    refit_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_50_1000.RDS"))
    refit_2000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_50_2000.RDS"))
    refit_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_50_5000.RDS"))
    refit_10000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_50_10000.RDS"))
    
    out_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_50_1000.RDS"))
    out_2000 <-  readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_50_2000.RDS"))
    out_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_50_5000.RDS"))
    out_10000 <-  readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_50_10000.RDS"))
    
    if(model_type=="CG"){
      sim = cbind(err_normal_f(out_1000, p, 1000, m=10)$all, err_normal_f(out_2000, p, 2000, m=10)$all,
                  err_normal_f(out_5000, p, 5000, m=10)$all,err_normal_f(out_10000, p, 10000,m =10)$all)
      
      refit =cbind(err_normal_f(refit_1000, p, 1000, m=10)$all,  err_normal_f(refit_2000, p, 2000,m =10)$all,
                   err_normal_f(refit_5000, p, 5000, m=10)$all, err_normal_f(refit_10000, p, 10000, m=10)$all)
    }
    if(model_type=="ER"){
      sim = cbind(err_gamma_f(out_1000, p, 1000, m=10)$all, err_gamma_f(out_2000, p, 2000, m=10)$all,
                  err_gamma_f(out_5000, p, 5000, m=10)$all,err_gamma_f(out_10000, p, 10000,m =10)$all)
      
      refit =cbind(err_gamma_f(refit_1000, p, 1000, m=10)$all,  err_gamma_f(refit_2000, p, 2000,m =10)$all,
                   err_gamma_f(refit_5000, p, 5000, m=10)$all, err_gamma_f(refit_10000, p, 10000, m=10)$all)
    }
    if(model_type=="SB"){
      sim = cbind(err_normal_gamma_f(out_1000, p, 1000, m=10)$all, err_normal_gamma_f(out_2000, p, 2000, m=10)$all,
                  err_normal_gamma_f(out_5000, p, 5000, m=10)$all,err_normal_gamma_f(out_10000, p, 10000,m =10)$all)
      
      refit =cbind(err_normal_gamma_f(refit_1000, p, 1000, m=10)$all,  err_normal_gamma_f(refit_2000, p, 2000,m =10)$all,
                   err_normal_gamma_f(refit_5000, p, 5000, m=10)$all, err_normal_gamma_f(refit_10000, p, 10000, m=10)$all)
    }
  }
  
  if(zero_inf==T){
    # means zero-inflated output required
    refit_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_zero_1000.RDS"))
    refit_2000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_zero_2000.RDS"))
    refit_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_zero_5000.RDS"))
    refit_10000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_zero_10000.RDS"))
    
    out_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_zero_1000.RDS"))
    out_2000 <-  readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_zero_2000.RDS"))
    out_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_zero_5000.RDS"))
    out_10000 <-  readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_zero_10000.RDS"))
    
    if(model_type=="CG"){
      sim = cbind(err_normal_f(out_1000, p, 1000, m=10)$all, err_normal_f(out_2000, p, 2000, m=10)$all,
                  err_normal_f(out_5000, p, 5000, m=10)$all,err_normal_f(out_10000, p, 10000,m =10)$all)
      
      refit =cbind(err_normal_f(refit_1000, p, 1000, m=10)$all,  err_normal_f(refit_2000, p, 2000,m =10)$all,
                   err_normal_f(refit_5000, p, 5000, m=10)$all, err_normal_f(refit_10000, p, 10000, m=10)$all)
    }
    if(model_type=="ER"){
      sim = cbind(err_gamma_f(out_1000, p, 1000, m=10)$all, err_gamma_f(out_2000, p, 2000, m=10)$all,
                  err_gamma_f(out_5000, p, 5000, m=10)$all,err_gamma_f(out_10000, p, 10000,m =10)$all)
      
      refit =cbind(err_gamma_f(refit_1000, p, 1000, m=10)$all,  err_gamma_f(refit_2000, p, 2000,m =10)$all,
                   err_gamma_f(refit_5000, p, 5000, m=10)$all, err_gamma_f(refit_10000, p, 10000, m=10)$all)
    }
    if(model_type=="SB"){
      sim = cbind(err_normal_gamma_f(out_1000, p, 1000, m=10)$all, err_normal_gamma_f(out_2000, p, 2000, m=10)$all,
                  err_normal_gamma_f(out_5000, p, 5000, m=10)$all,err_normal_gamma_f(out_10000, p, 10000,m =10)$all)
      
      refit =cbind(err_normal_gamma_f(refit_1000, p, 1000, m=10)$all,  err_normal_gamma_f(refit_2000, p, 2000,m =10)$all,
                   err_normal_gamma_f(refit_5000, p, 5000, m=10)$all, err_normal_gamma_f(refit_10000, p, 10000, m=10)$all)
    }
  }
  
  
  
  # Convert matrices to data frames and add a column identifier
  sim_df <- as.data.frame(sim)
  names(sim_df) = c("1000", "2000", "5000", "10,000")
  sim_df$type <- 'Stage 1'
  sim_df$id <- 1:nrow(sim_df)
  
  refit_df <- as.data.frame(refit)
  names(refit_df) = c("1000", "2000", "5000", "10,000")
  refit_df$type <- 'Stage 2'
  refit_df$id <- 1:nrow(refit_df)
  
  # Combine the data frames
  data <- rbind(sim_df, refit_df)
  
  # Melt the data frame to long format
  data_long <- melt(data, id.vars = c('type', 'id'))
  
  # Create the plot
  ggplot(data_long, aes(x = variable, y = value, fill = type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("Stage 1" = "red", "Stage 2" = "blue")) +
    ggtitle(title) +
    labs(x = "n", y = "Average MSE across p") +  # Change x and y labels 
    theme_minimal()
  
}

p1 = plot_f_errs("CG", 10, F, "50/50 (p=10)")
p2 = plot_f_errs("CG", 10, T, "ZI (p=10)")
p3 = plot_f_errs("CG", 50, F, "50/50 (p=50)")
p4 = plot_f_errs("CG", 50, T, "ZI (p=50)")

library(ggpubr)
plot = ggarrange(p1,p2,p3,p4, ncol=2, nrow = 2, common.legend = T)
print(annotate_figure(plot, "Chain Graph"))

# ER GRaph 
p1 = plot_f_errs("ER", 10, F, "50/50 (p=10)")
p2 = plot_f_errs("ER", 10, T, "ZI (p=10)")
p3 = plot_f_errs("ER", 50, F, "50/50 (p=50)")
p4 = plot_f_errs("ER", 50, T, "ZI (p=50)")

library(ggpubr)
plot = ggarrange(p1,p2,p3,p4, ncol=2, nrow = 2, common.legend = T)
print(annotate_figure(plot, "ER Graph"))


# SB model

p1 = plot_f_errs("SB", 10, F, "50/50 (p=10)")
p2 = plot_f_errs("SB", 10, T, "ZI (p=10)")
p3 = plot_f_errs("SB", 50, F, "50/50 (p=50)")
p4 = plot_f_errs("SB", 50, T, "ZI (p=50)")

library(ggpubr)
plot = ggarrange(p1,p2,p3,p4, ncol=2, nrow = 2, common.legend = T)
print(annotate_figure(plot, "SB Model"))



# Interaction Matrix ------------------------------------------------------

plot_interaction_errs = function(model_type, p, zero_inf, title){
  #' Function to plot comparison of intercept errors for stage 1 vs stage 2
  #' @param model_type (string) "CG", "ER" or "SB"
  #' @param p (scalar)
  #' @param zero_inf (T/F) indicating if zero-inflated model or not
  #' @param title (string) title for plot
  #' 
  
  if(zero_inf==F){
    # means 50/50 output required
    refit_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_50_1000.RDS"))
    refit_2000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_50_2000.RDS"))
    refit_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_50_5000.RDS"))
    refit_10000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_50_10000.RDS"))
    
    out_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_50_1000.RDS"))
    out_2000 <-  readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_50_2000.RDS"))
    out_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_50_5000.RDS"))
    out_10000 <-  readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_50_10000.RDS"))
    
  
    beta <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/beta_",p,".RDS"))
    sim = cbind(metrics_new(out_1000, beta, 1e-5)$all,metrics_new(out_2000, beta, 1e-5)$all,
                metrics_new(out_5000, beta, 1e-5)$all,metrics_new(out_10000, beta, 1e-5)$all)
    
    refit =cbind(metrics_new(refit_1000, beta, 1e-5)$all,metrics_new(refit_2000, beta, 1e-5)$all,
                 metrics_new(refit_5000, beta, 1e-5)$all,metrics_new(refit_10000, beta, 1e-5)$all)
   
  }
  
  if(zero_inf==T){
    # means zero-inflated output required
    refit_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_zero_1000.RDS"))
    refit_2000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_zero_2000.RDS"))
    refit_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_zero_5000.RDS"))
    refit_10000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Refit/",p,"/refit_zero_10000.RDS"))
    
    out_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_zero_1000.RDS"))
    out_2000 <-  readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_zero_2000.RDS"))
    out_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_zero_5000.RDS"))
    out_10000 <-  readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/", model_type, "/Simulations/",p,"/out_zero_10000.RDS"))
    
    beta <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/beta_",p,".RDS"))
    sim = cbind(metrics_new(out_1000, beta, 1e-5)$all,metrics_new(out_2000, beta, 1e-5)$all,
                metrics_new(out_5000, beta, 1e-5)$all,metrics_new(out_10000, beta, 1e-5)$all)
    
    refit =cbind(metrics_new(refit_1000, beta, 1e-5)$all,metrics_new(refit_2000, beta, 1e-5)$all,
                 metrics_new(refit_5000, beta, 1e-5)$all,metrics_new(refit_10000, beta, 1e-5)$all)
  }
  
  
  
  # Convert matrices to data frames and add a column identifier
  sim_df <- as.data.frame(sim)
  names(sim_df) = c("1000", "2000", "5000", "10,000")
  sim_df$type <- 'Stage 1'
  sim_df$id <- 1:nrow(sim_df)
  
  refit_df <- as.data.frame(refit)
  names(refit_df) = c("1000", "2000", "5000", "10,000")
  refit_df$type <- 'Stage 2'
  refit_df$id <- 1:nrow(refit_df)
  
  # Combine the data frames
  data <- rbind(sim_df, refit_df)
  
  # Melt the data frame to long format
  data_long <- melt(data, id.vars = c('type', 'id'))
  
  # Create the plot
  ggplot(data_long, aes(x = variable, y = value, fill = type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("Stage 1" = "red", "Stage 2" = "blue")) +
    ggtitle(title) +
    labs(x = "n", y = "Average MSE across p") +  # Change x and y labels 
    theme_minimal()
  
}

p1 = plot_interaction_errs("CG", 10, F, "50/50 (p=10)")
p2 = plot_interaction_errs("CG", 10, T, "ZI (p=10)")
p3 = plot_interaction_errs("CG", 50, F, "50/50 (p=50)")
p4 = plot_interaction_errs("CG", 50, T, "ZI (p=50)")

library(ggpubr)
plot = ggarrange(p1,p2,p3,p4, ncol=2, nrow = 2, common.legend = T)
print(annotate_figure(plot, "Chain Graph"))

# ER 

p1 = plot_interaction_errs("ER", 10, F, "50/50 (p=10)")
p2 = plot_interaction_errs("ER", 10, T, "ZI (p=10)")
p3 = plot_interaction_errs("ER", 50, F, "50/50 (p=50)")
p4 = plot_interaction_errs("ER", 50, T, "ZI (p=50)")

library(ggpubr)
plot = ggarrange(p1,p2,p3,p4, ncol=2, nrow = 2, common.legend = T)
print(annotate_figure(plot, "ER Graph"))

# SB


p1 = plot_interaction_errs("SB", 10, F, "50/50 (p=10)")
p2 = plot_interaction_errs("SB", 10, T, "ZI (p=10)")
p3 = plot_interaction_errs("SB", 50, F, "50/50 (p=50)")
p4 = plot_interaction_errs("SB", 50, T, "ZI (p=50)")

library(ggpubr)
plot = ggarrange(p1,p2,p3,p4, ncol=2, nrow = 2, common.legend = T)
print(annotate_figure(plot, "SB Model"))
