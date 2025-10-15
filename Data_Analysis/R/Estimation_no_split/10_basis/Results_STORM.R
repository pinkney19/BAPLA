setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
# Read in files -----------------------------------------------------------

est_left <- readRDS("~/luna/BAPLA/Data_Analysis/vis/10_basis/est_left.RDS")
est_right <- readRDS("~/luna/BAPLA/Data_Analysis/vis/10_basis/est_right.RDS")



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
left_res = choose_lambda(trial_data = left_data, p = 48, lambdas, est_left, m=10)
right_res = choose_lambda(trial_data = right_data, p = 48, lambdas, est_right, m = 10)

# save results
setwd("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/10_basis/results")
saveRDS(left_res, "left_res.RDS")
saveRDS(right_res, "right_res.RDS")
