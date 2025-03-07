setwd("~/Downloads/BAPLA/Synthetic_Experiments")
source("BAPLA_Functions.R")
library(splines)
data_50_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Sims_data/data_50_1000.RDS")

data = data_50_1000[[1]]

lams = readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/10/lams_50_1000.RDS")
m = 10
res = Estimate_coefficients_logit(data, 10, 1000, 1e-5, lams, m)
res$beta_hat
