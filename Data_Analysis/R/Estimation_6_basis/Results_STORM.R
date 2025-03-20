setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
# Read in files -----------------------------------------------------------
est_left1 <- readRDS("~/luna/BAPLA/Data_Analysis/6_basis/est_left1.RDS")
est_left2 <- readRDS("~/luna/BAPLA/Data_Analysis/6_basis/est_left2.RDS")
est_right1 <- readRDS("~/luna/BAPLA/Data_Analysis/6_basis/est_right1.RDS")
est_right2 <- readRDS("~/luna/BAPLA/Data_Analysis/6_basis/est_right2.RDS")
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
left_res = choose_lambda(trial_data = left_data, p = 48, lambdas, est_left)
right_res = choose_lambda(trial_data = right_data, p = 48, lambdas, est_right)

# save results
setwd("~/Downloads/BAPLA/Data_Analysis/R/Estimation_6_basis/results")
saveRDS(left_res, "left_res.RDS")
saveRDS(right_res, "right_res.RDS")
