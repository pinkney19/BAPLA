setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
# Read in files -----------------------------------------------------------

est_left <- readRDS("~/luna/BAPLA/Data_Analysis/wheel/est_left_no_f.RDS")
est_right <- readRDS("~/luna/BAPLA/Data_Analysis/wheel/est_right_no_f.RDS")



# BIC ---------------------------------------------------------------------
library(pracma)
lambdas = logspace(log10(10),log10(300),100)
units = seq(1, 56)
grid = expand.grid( lams = lambdas, neuron = units)
list_of_pairs <- split(grid, seq(nrow(grid)))

left_data <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/left.RDS")
right_data <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/right.RDS")


# output ------------------------------------------------------------------

library(purrr)
left_res = choose_lambda_no_f(trial_data = left_data, p = 56, lambdas, est_left)
right_res = choose_lambda_no_f(trial_data = right_data, p = 56, lambdas, est_right)

# save results
setwd("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/no_f/results")
saveRDS(left_res, "left_res.RDS")
saveRDS(right_res, "right_res.RDS")
