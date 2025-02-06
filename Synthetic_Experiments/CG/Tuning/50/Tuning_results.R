# Load libraries ----------------------------------------------------------

library(purrr)
library(pracma)

setwd("~/Downloads/BAPLA/Synthetic_Experiments")
source("BAPLA_Functions.R")


# Chain Graph ----------------------------------------------

p = 50; 
beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_50.RDS")
lambdas = logspace(log10(10),log10(300),100)


# Load tuning results from STORM ----------------------------------------------
out_50_1000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Tuning/50/out_50_1000.RDS")
out_50_2000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Tuning/50/out_50_2000.RDS")
out_50_5000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Tuning/50/out_50_5000.RDS")
out_50_10000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Tuning/50/out_50_10000.RDS")

out_zero_1000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Tuning/50/out_zero_1000.RDS")
out_zero_2000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Tuning/50/out_zero_2000.RDS")
out_zero_5000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Tuning/50/out_zero_5000.RDS")
out_zero_10000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Tuning/50/out_zero_10000.RDS")




# eBIC --------------------------------------------------------------------

# load tuning data files

data_50_tuning_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/50/Tuning_data/data_50_tuning_1000.RDS")
data_50_tuning_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/50/Tuning_data/data_50_tuning_2000.RDS")
data_50_tuning_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/50/Tuning_data/data_50_tuning_5000.RDS")
data_50_tuning_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/50/Tuning_data/data_50_tuning_10000.RDS")

data_zero_tuning_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/50/Tuning_data/data_zero_tuning_1000.RDS")
data_zero_tuning_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/50/Tuning_data/data_zero_tuning_2000.RDS")
data_zero_tuning_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/50/Tuning_data/data_zero_tuning_5000.RDS")
data_zero_tuning_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/50/Tuning_data/data_zero_tuning_10000.RDS")


# Format Results ----------------------------------------------------------

#N_samp = Number of training samples
N_samp = 10;  m=10;
library(boot) #inv.logit

library(splines)

res2 = get_bic_vals(out_zero_1000, N_samp, t_end = 999, m,lambdas, data_zero_tuning_1000,2, 0.5, 10, threshold = 1e-5)

par(mfrow=c(1,3), mar = c(5,4,4,2))
plot(lambdas, res2$BIC, type='l', main="BIC")
abline(v =lambdas[which.min(res2$BIC)], col="red")
plot(lambdas, res2$AIC, type='l', main="AIC")
abline(v =lambdas[which.min(res2$AIC)], col="red")
plot(lambdas, res2$eBIC, type='l', main="eBIC")
abline(v =lambdas[which.min(res2$eBIC)], col="red")

# For all units -----------------------------------------------------------

library(splines)
library(boot)
par(mfrow = c(1,4))
f1000 = get_lambdas(out_50_1000, N_samp, 999, m, lambdas, data_50_tuning_1000, p, "50 - 1000")
f2000 = get_lambdas(out_50_2000, N_samp, 1999, m, lambdas, data_50_tuning_2000, p, "50 - 2000")
f5000 = get_lambdas(out_50_5000, N_samp, 4999, m, lambdas, data_50_tuning_5000, p, "50 - 5000")
f10000 = get_lambdas(out_50_10000, N_samp, 9999, m, lambdas, data_50_tuning_10000, p, "50 - 10000")

z1000 = get_lambdas(out_zero_1000, N_samp, 999, m, lambdas, data_zero_tuning_1000, p, "zero - 1000")
z2000 = get_lambdas(out_zero_2000, N_samp, 1999, m, lambdas, data_zero_tuning_2000, p, "zero - 2000")
z5000 = get_lambdas(out_zero_5000, N_samp, 4999, m, lambdas, data_zero_tuning_5000, p, "zero - 5000")
z10000 = get_lambdas(out_zero_10000, N_samp, 9999, m, lambdas, data_zero_tuning_10000, p, "zero - 10000")




setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/50")
saveRDS(f1000$BIC, "lams_50_1000.RDS")
saveRDS(f2000$BIC, "lams_50_2000.RDS")
saveRDS(f5000$BIC, "lams_50_5000.RDS")
saveRDS(f10000$BIC, "lams_50_10000.RDS")

saveRDS(z1000$BIC, "lams_zero_1000.RDS")
saveRDS(z2000$BIC, "lams_zero_2000.RDS")
saveRDS(z5000$BIC, "lams_zero_5000.RDS")
saveRDS(z10000$BIC, "lams_zero_10000.RDS")

