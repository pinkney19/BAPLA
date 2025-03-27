setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
# Read in files -----------------------------------------------------------
est_left1 <- readRDS("~/luna/BAPLA/Data_Analysis/est_left1.RDS")
est_left2 <- readRDS("~/luna/BAPLA/Data_Analysis/est_left2.RDS")
est_right1 <- readRDS("~/luna/BAPLA/Data_Analysis/est_right1.RDS")
est_right2 <- readRDS("~/luna/BAPLA/Data_Analysis/est_right2.RDS")
# combine files -----------------------------------------------------------

est_left <- c(est_left1, est_left2)
est_right = c(est_right1, est_right2) 


# BIC ---------------------------------------------------------------------
library(pracma)
lambdas = logspace(log10(10),log10(300),100)
units = seq(1, 48)
grid = expand.grid( lams = lambdas, neuron = units)
list_of_pairs <- split(grid, seq(nrow(grid)))

left_data <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/binned/left.RDS")
right_data <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/binned/right.RDS")


# output ------------------------------------------------------------------

library(purrr)


# select results of interest ----------------------------------------------
region_ids <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/region_ids.RDS")
map_regions = c(CA1 = 1, DG =2, GPe = 3, MB = 4, MRN = 5, POL = 6, SCm = 7, SCsg = 8 , VISrl = 9)
mapped = map_regions[region_ids]


select_results = function(lambdas, unit_indices, out_storm){
  #' Function to select regularisation parameter based on BIC results
  #' 
  out = list();
  betas = NULL; betas = rep(list(betas), length(lambdas)); betas = rep(list(betas), length(unit_indices))
  beta_0 = NULL; beta_0 = rep(list(beta_0), length(lambdas)); beta_0 = rep(list(beta_0), length(unit_indices))
  f = NULL; f = rep(list(f), length(lambdas)); f = rep(list(f), length(unit_indices))
  
  for(j in 1:length(unit_indices)){
    print(unit_indices[j])
    b = length(lambdas)*unit_indices[j]
    if(unit_indices[j]==1){
      # select results from storm
      out  = out_storm[1:b]
    }
    if(unit_indices[j]!=1){
      # select results from storm
      out = out_storm[((b-length(lambdas))+1):b]
    }
    betas[[j]] = map(out, 1)
    beta_0[[j]] = map(out, 5) # use corrected for basis functions
    f[[j]] = map(out, 3)
    # make betas matrix not vector
    #betas = lapply(betas, as.matrix)
  }
  return(list(betas = betas, f=f, beta_0 = beta_0))
}


unit_indices = which(mapped==4)

res = select_results(lambdas, unit_indices, est_left)


betas = res$betas
beta_0s  = res$beta_0
beta_0s = lapply(beta_0s, function(x) unlist(x, recursive = F))
fs = res$f


check = list();
unit_indices = unname(unit_indices)
id = 5
for(i in 1:length(lambdas)){
  print(i)
  check[[i]] = calc_log_lik(i, unit_indices[5], left_data, beta_0s[[5]],
                            betas[[5]], fs[[5]])
}

unlist(map(check, 2))





left_res = choose_lambda(trial_data = left_data, p = 48, lambdas, est_left, m=4)
right_res = choose_lambda(trial_data = right_data, p = 48, lambdas, est_right, m=4)

# save results
setwd("~/Downloads/BAPLA/Data_Analysis/R/Estimation/results")
saveRDS(left_res, "left_res.RDS")
saveRDS(right_res, "right_res.RDS")
# 
# left_df = data.frame(lams = left_res$lam_star, region=region_id, label = seq(1,78))
# right_df = data.frame(lams = right_res$lam_star, region=region_id, label = seq(1,78))
# 
# 
# library(ggplot2)
# left_lams = ggplot(data = left_df, aes(x = label, y = lams, color = region))+
#       geom_point() +
#       labs(x = "Neuron ID", y = expression(lambda)) +
#       theme_minimal()+
#    ggtitle("Left Trials")
#  
# right_lams = ggplot(data = right_df, aes(x = label, y = lams, color = region))+
#    geom_point() +
#    labs(x = "Neuron ID", y = expression(lambda)) +
#    theme_minimal()+
#    ggtitle("Right Trials") 
#  
# library(ggpubr)
# plt = ggarrange(left_lams, right_lams, ncol=2, common.legend = T, legend = "right")
# print(plt)
# 
# # Extract data relevant to chosen lambda ----------------------------------
# # check = est_right[101:200]
# # c2 = check[[right_res$lam_id[2]]]
# # c2$beta_hat
# # 
# 
# setwd("~/Downloads/BAPLA/Real_Data/Data/estimation/basis4/results")
# saveRDS(right_res, "right_res.RDS")
# saveRDS(left_res, "left_res.RDS")
# 
# lam_id = right_res$lam_id
# out_star = list();
# p = 78
# for(j in 1:p){
#   print(j)
#   b = length(lambdas)*j
#   if(j==1){
#     # select results from storm
#     out  = est_right[1:b]
#   }
#   if(j!=1){
#     # select results from storm
#     out = est_right[((b-length(lambdas))+1):b]
#   }
#   out_star[[j]] = out[[lam_id[j]]]
# }
# 
# betas = map(out_star,1)
# beta_hat = matrix(NA, 78, 78)
# for(i in 1:length(betas)){
#   beta_hat[i,] = betas[[i]]
# }
# image(beta_hat)
# 
# 
# beta2 = beta_hat
# beta2[beta2<0] = -1
# beta2[beta2>0] = 1
# image(beta2)
# 
# # Convert matrix to data frame
# image_df <- as.data.frame(as.table(beta2))
# 
# # Plot the image using ggplot2
# ggplot(image_df, aes(Var1, Var2, fill = Freq)) +
#   geom_raster() +
#   scale_fill_gradient(low = "white", high = "blue") +
#   theme_minimal() +
#   labs(x = "X-axis", y = "Y-axis", fill = "Intensity")
# 
#  
# # out_right = right_res$out
# # beta_hat = matrix(NA, 78,78)
# # betas = map(out_right, 1)
# # for(i in 1:78){
# #   beta_hat[i,] = betas[[i]]
# # }
# 
# # # plot lambda values by region (?)
# # 
# # setwd("~/Downloads/BAPLA/Real_Data/R/chosen_lams")
# # saveRDS(lam_star, "lam_star.RDS")
# # saveRDS(lam_id, "lam_id.RDS")
# # 
# 
# 
# 
# # betas = map(est1,1)
# # beta_hat = matrix(NA, nrow = length(betas), ncol=length(betas))
# # for(i in 1:length(betas)){
# #   beta_hat[i,] = betas[[i]]
# # }
# # image(beta_hat)
# # 
# # beta1 = beta_hat
# # beta1[beta1>0]=1
# # beta1[beta1<0] = -1
# # 
# # image(beta1)
# # 
# # beta0s = map(est1,2)
# # beta0s = unlist(beta0s)
# # fs = map(est1, 3)
# # 
# # f_hat = list();
# # for(i in 1:length(fs)){
# #   f_hat[[i]] = fs[[i]] + beta0s[i]
# # }
# # 
# # par(mfrow = c(1,3))
# # plot(seq(-0.2,0.4,length.out = length(fs[[1]])), f_hat[[1]], type='l', ylab = "Firing Rate", xlab = "time (s)", main = "CA1")
# # plot(seq(-0.2,0.4,length.out = length(fs[[2]])), f_hat[[2]], type='l', ylab = "Firing Rate", xlab = "time (s)", main = "DG")
# # plot(seq(-0.2,0.4,length.out = length(fs[[38]])),  f_hat[[38]], type='l', ylab = "Firing Rate", xlab = "time (s)", main = "POL")
# # 
# # 
# # 
# # par(mfrow = c(1,3))
# # plot(seq(-0.2,0.4,length.out = length(fs[[1]])), (abs(min(f_hat[[1]])) +f_hat[[1]])/(abs(beta0s[1])), type='l', ylab = "Firing Rate", xlab = "time (s)", main = "CA1")
# # plot(seq(-0.2,0.4,length.out = length(fs[[2]])), (abs(min(f_hat[[2]])) + f_hat[[2]])/abs(beta0s[2]), type='l', ylab = "Firing Rate", xlab = "time (s)", main = "DG")
# # plot(seq(-0.2,0.4,length.out = length(fs[[38]])), (abs(min(f_hat[[38]])) + f_hat[[38]])/abs(beta0s[38]), type='l', ylab = "Firing Rate", xlab = "time (s)", main = "POL")
# # 
# # 
# # 
# # beta0s = map(est1,2)
# # unlist(beta0s)
