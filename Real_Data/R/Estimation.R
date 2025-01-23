
# Load Functions ----------------------------------------------------------

setwd("~/Downloads/BAPLA/Real_Data")
source("Functions_data.R")
library(purrr)

# Load Data ---------------------------------------------------------------

left_binned <- readRDS("~/Downloads/BAPLA/Real_Data/Data/left_binned.RDS")
right_binned <- readRDS("~/Downloads/BAPLA/Real_Data/Data/right_binned.RDS")
regions = names(left_binned)

# Exploratory Checks ------------------------------------------------------

par(mfrow=c(3,4))
for(i in 1:length(regions)){
  plot(seq(-0.2,0.4,by = 0.001),left_binned[[i]][[1]][[1]], type='l',main = regions[i], xlab = "time(secs)", ylab = "")
  
}

out_left = format_data(left_binned)
data_left = out_left$data # 106 trials
ids = out_left$ids



out_right = format_data(right_binned)
data_right = out_right$data # 73 trials
id_right = out_right$ids

library(splines)
res = bapla_single_unit_trial(data_left, 1, 50, 1e-5, rep(10,36), 10)

#sim_data <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/Simulate_Data/10/Tuning_data/data_50_tuning_1000.RDS")
beta_0 = NULL; f = list(); f = rep(list(f), 106); beta = list(); beta = rep(list(beta), 106);iter = NULL;
for(i in 1:106){
  print(i)
  res = bapla_single_unit_trial(data_left, 1, 1000, 1e-5, rep(10,84), 10)
  beta[[i]] = res$beta
  f[[i]] = res$f
  beta_0[i] = res$beta0
  iter[i] = res$iter
}
