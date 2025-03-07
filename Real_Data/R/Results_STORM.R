
# Read in files -----------------------------------------------------------
est_left1 <- readRDS("~/luna/BAPLA/Real_Data/est_left1.RDS")
est_left2 <- readRDS("~/luna/BAPLA/Real_Data/est_left2.RDS")
est_right1 <- readRDS("~/luna/BAPLA/Real_Data/est_right1.RDS")
est_right2 <- readRDS("~/luna/BAPLA/Real_Data/est_right2.RDS")


# combine files -----------------------------------------------------------

est_left <- c(est_left1, est_left2)
est_right = c(est_right1, est_right2)


# BIC ---------------------------------------------------------------------

lambdas = logspace(log10(1),log10(150),100)
units = seq(1, 74)
grid = expand.grid( lams = lambdas, neuron = units)
list_of_pairs <- split(grid, seq(nrow(grid)))

# do for unit 1, left trials

left1_out  = est_left[1:100]
betas = map(left1_out, 1)
beta_0 = map(left1_out, 5) # use corrected for basis functions
beta_0_2 = map(left1_out,2)
f = map(left1_out, 3)
iter = map(left1_out, 6)

left_data <- readRDS("~/Downloads/BAPLA/Real_Data/Data/estimation/left.RDS")
# for now
trial_data = left_data # need to update when doing right later...

idx = 1 # considering unit 1 here!
# make betas matrix not vector
betas = lapply(betas, as.matrix)

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


# do for left trials ------------------------------------------------------

left_data <- readRDS("~/Downloads/BAPLA/Real_Data/Data/estimation/left.RDS")
# for now
trial_data = left_data # need to update when doing right later...

# all units
p = 74
BIC = NULL; BIC = rep(list(BIC), p)
lam_star = NULL; lam_id = NULL;
for(j in 1:p){
  print(j)
  b = 100*j
  if(j==1){
    # select results from storm
    out  = est_left[1:b]
  }
  if(j!=1){
    # select results from storm
    out = est_left[((b-100)+1):b]
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

plot(seq(1,74),lam_star, xlab = "Neuron ID", ylab = expression(lambda))

setwd("~/Downloads/BAPLA/Real_Data/R/chosen_lams")
saveRDS(lam_star, "lam_star.RDS")
saveRDS(lam_id, "lam_id.RDS")




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
