
# Read in files -----------------------------------------------------------
# basis m = 6
# est_left1 <- readRDS("~/luna/BAPLA/Real_Data/est_left1.RDS")
# est_left2 <- readRDS("~/luna/BAPLA/Real_Data/est_left2.RDS")
# est_right1 <- readRDS("~/luna/BAPLA/Real_Data/est_right1.RDS")
# est_right2 <- readRDS("~/luna/BAPLA/Real_Data/est_right2.RDS")

# basis m = 4

est_left1 <- readRDS("~/luna/BAPLA/Real_Data/new_DA/est_left1.RDS")
est_left2 <- readRDS("~/luna/BAPLA/Real_Data/new_DA/est_left2.RDS")
est_right1 <- readRDS("~/luna/BAPLA/Real_Data/new_DA/est_right1.RDS")
est_right2 <- readRDS("~/luna/BAPLA/Real_Data/new_DA/est_right2.RDS")
# combine files -----------------------------------------------------------

est_left <- c(est_left1, est_left2)
est_right = c(est_right1, est_right2) 


# BIC ---------------------------------------------------------------------
library(pracma)
lambdas = logspace(log10(0.5),log10(400),100)
#lambdas2 = logspace(log10(150),log10(300),100)
#lambdas = c(lambdas1, lambdas2)
units = seq(1, 78)
grid = expand.grid( lams = lambdas, neuron = units)
list_of_pairs <- split(grid, seq(nrow(grid)))


left_data <- readRDS("~/Downloads/BAPLA/Real_Data/Data/estimation/left.RDS")
right_data <- readRDS("~/Downloads/BAPLA/Real_Data/Data/estimation/right.RDS")


# Functions ---------------------------------------------------------------



BIC_function= function(lam_idx, unit_idx, gamma, trial_data, beta_0, betas, f, m){
  #' Function to get BIC for real data
  #' @param lam_idx index of lambda value
  #' @param unit_idx index of the unit/neuron
  #' @param gamma hyperparameter for eBIC
  #' @param trial_data data from neuroscience experiment

  ######################### obtain data for each trial ######################## 
  trial_X = list(); trial_X = rep(list(trial_X), length(trial_data))
  nll = NULL;
  for(l in 1:length(trial_data)){
    # select data of interest to the unit_id (and trial of interest)
    data = trial_data[[l]]
    y = data[unit_idx,][2:length(data[unit_idx,])] # use
    X1 = data[,(1: (length(data[unit_idx,])-1) )] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
    X1 = t(X1)
    p = dim(data)[1]
    
  #########################################
    
    term1 = y * (beta_0[[lam_idx]] + X1 %*% betas[[lam_idx]] + f[[lam_idx]])
    term2 = log(1 + exp(beta_0[[lam_idx]] + X1 %*% betas[[lam_idx]] + f[[lam_idx]])) 
    nll[l] = sum(term1-term2) 
  }
    
    lik = sum(nll)
  
    num_non_zeroes = sum(betas[[lam_idx]] != 0)
    n_star = 600 * 60 #36060
    pen_term = (num_non_zeroes+1+(m))*log(n_star)
    eBIC_term = 2*gamma*log(p)* (num_non_zeroes+1+(m))
    
    BIC = -2*lik + pen_term #+ eBIC_term
    eBIC = BIC + eBIC_term
    AIC = -2*lik + (2*(num_non_zeroes+1+(m)))

    return(list(BIC = BIC, AIC = AIC, eBIC = eBIC))
}


choose_lambda = function(trial_data, p, lambdas, out_storm){

  BIC = NULL; BIC = rep(list(BIC), p)
  lam_star = NULL; lam_id = NULL;
  out = list();
  for(j in 1:p){
    print(j)
    b = length(lambdas)*j
    if(j==1){
      # select results from storm
      out  = out_storm[1:b]
    }
    if(j!=1){
      # select results from storm
      out = out_storm[((b-length(lambdas))+1):b]
    }
    betas = map(out, 1)
    beta_0 = map(out, 5) # use corrected for basis functions
    f = map(out, 3)
    # make betas matrix not vector
    betas = lapply(betas, as.matrix)
    
    for(i in 1:length(lambdas)){
      BIC[[j]][i] = BIC_function(i, j, 0.5, trial_data, beta_0, betas, f, m=6)$BIC
    }
    lam_id[j] = which.min(BIC[[j]])
    lam_star[j] = lambdas[which.min(BIC[[j]])]
    
    
  }
  return(list(lam_star = lam_star, lam_id = lam_id))
}


# output ------------------------------------------------------------------

library(purrr)
left_res = choose_lambda(trial_data = left_data, p = 78, lambdas, est_left)
right_res = choose_lambda(trial_data = right_data, p = 78, lambdas, est_right)

# plot(seq(1,78),lam_star, xlab = "Neuron ID", ylab = expression(lambda))

region_id <- readRDS("~/Downloads/BAPLA/Real_Data/Data/region_id.RDS")

left_df = data.frame(lams = left_res$lam_star, region=region_id, label = seq(1,78))
right_df = data.frame(lams = right_res$lam_star, region=region_id, label = seq(1,78))


library(ggplot2)
left_lams = ggplot(data = left_df, aes(x = label, y = lams, color = region))+
      geom_point() +
      labs(x = "Neuron ID", y = expression(lambda)) +
      theme_minimal()+
   ggtitle("Left Trials")
 
right_lams = ggplot(data = right_df, aes(x = label, y = lams, color = region))+
   geom_point() +
   labs(x = "Neuron ID", y = expression(lambda)) +
   theme_minimal()+
   ggtitle("Right Trials") 
 
library(ggpubr)
plt = ggarrange(left_lams, right_lams, ncol=2, common.legend = T, legend = "right")
print(plt)

# Extract data relevant to chosen lambda ----------------------------------
# check = est_right[101:200]
# c2 = check[[right_res$lam_id[2]]]
# c2$beta_hat
# 

setwd("~/Downloads/BAPLA/Real_Data/R/new_DA/results")
saveRDS(right_res, "right_res.RDS")
saveRDS(left_res, "left_res.RDS")

lam_id = right_res$lam_id
out_star = list();
p = 78
for(j in 1:p){
  print(j)
  b = length(lambdas)*j
  if(j==1){
    # select results from storm
    out  = est_right[1:b]
  }
  if(j!=1){
    # select results from storm
    out = est_right[((b-length(lambdas))+1):b]
  }
  out_star[[j]] = out[[lam_id[j]]]
}

betas = map(out_star,1)
beta_hat = matrix(NA, 78, 78)
for(i in 1:length(betas)){
  beta_hat[i,] = betas[[i]]
}
image(beta_hat)


beta2 = beta_hat
beta2[beta2<0] = -1
beta2[beta2>0] = 1
image(beta2)

# Convert matrix to data frame
image_df <- as.data.frame(as.table(beta2))

# Plot the image using ggplot2
ggplot(image_df, aes(Var1, Var2, fill = Freq)) +
  geom_raster() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(x = "X-axis", y = "Y-axis", fill = "Intensity")

 
# out_right = right_res$out
# beta_hat = matrix(NA, 78,78)
# betas = map(out_right, 1)
# for(i in 1:78){
#   beta_hat[i,] = betas[[i]]
# }

# # plot lambda values by region (?)
# 
# setwd("~/Downloads/BAPLA/Real_Data/R/chosen_lams")
# saveRDS(lam_star, "lam_star.RDS")
# saveRDS(lam_id, "lam_id.RDS")
# 



# betas = map(est1,1)
# beta_hat = matrix(NA, nrow = length(betas), ncol=length(betas))
# for(i in 1:length(betas)){
#   beta_hat[i,] = betas[[i]]
# }
# image(beta_hat)
# 
# beta1 = beta_hat
# beta1[beta1>0]=1
# beta1[beta1<0] = -1
# 
# image(beta1)
# 
# beta0s = map(est1,2)
# beta0s = unlist(beta0s)
# fs = map(est1, 3)
# 
# f_hat = list();
# for(i in 1:length(fs)){
#   f_hat[[i]] = fs[[i]] + beta0s[i]
# }
# 
# par(mfrow = c(1,3))
# plot(seq(-0.2,0.4,length.out = length(fs[[1]])), f_hat[[1]], type='l', ylab = "Firing Rate", xlab = "time (s)", main = "CA1")
# plot(seq(-0.2,0.4,length.out = length(fs[[2]])), f_hat[[2]], type='l', ylab = "Firing Rate", xlab = "time (s)", main = "DG")
# plot(seq(-0.2,0.4,length.out = length(fs[[38]])),  f_hat[[38]], type='l', ylab = "Firing Rate", xlab = "time (s)", main = "POL")
# 
# 
# 
# par(mfrow = c(1,3))
# plot(seq(-0.2,0.4,length.out = length(fs[[1]])), (abs(min(f_hat[[1]])) +f_hat[[1]])/(abs(beta0s[1])), type='l', ylab = "Firing Rate", xlab = "time (s)", main = "CA1")
# plot(seq(-0.2,0.4,length.out = length(fs[[2]])), (abs(min(f_hat[[2]])) + f_hat[[2]])/abs(beta0s[2]), type='l', ylab = "Firing Rate", xlab = "time (s)", main = "DG")
# plot(seq(-0.2,0.4,length.out = length(fs[[38]])), (abs(min(f_hat[[38]])) + f_hat[[38]])/abs(beta0s[38]), type='l', ylab = "Firing Rate", xlab = "time (s)", main = "POL")
# 
# 
# 
# beta0s = map(est1,2)
# unlist(beta0s)
