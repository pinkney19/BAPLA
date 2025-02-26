library(purrr)
library(pracma)

setwd("~/Downloads/BAPLA/Synthetic_Experiments")
source("BAPLA_Functions.R")


# BIC --------------------------------------------------------------------

# load tuning data files
data_50_tuning_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/Simulate_Data/Tuning_data/data_50_tuning_1000.RDS")
data_50_tuning_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/Simulate_Data/Tuning_data/data_50_tuning_2000.RDS")
data_50_tuning_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/Simulate_Data/Tuning_data/data_50_tuning_5000.RDS")
data_50_tuning_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/Simulate_Data/Tuning_data/data_50_tuning_10000.RDS")


# Format Results ----------------------------------------------------------

N_samp = 10; #Number of training samples  
library(boot) #inv.logit
library(splines)

out_1000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_gamma/beta_0.1/tuning_no_f/out_50_1000.RDS")
out_2000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_gamma/beta_0.1/tuning_no_f/out_50_2000.RDS")
out_5000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_gamma/beta_0.1/tuning_no_f/out_50_5000.RDS")
out_10000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_gamma/beta_0.1/tuning_no_f/out_50_10000.RDS")


lambdas = logspace(log10(1),log10(150),100)
m = 0;
N_samp= 10;
par(mfrow = c(1,4))
p = 10
f1000 = get_lambdas_no_f(out_1000, N_samp, 999, m, lambdas, data_50_tuning_1000, p, "n = 1000")
f2000 = get_lambdas_no_f(out_2000, N_samp, 1999, m, lambdas, data_50_tuning_2000, p, "n = 2000")
f5000 = get_lambdas_no_f(out_5000, N_samp, 4999, m, lambdas, data_50_tuning_5000, p,  "n = 5000")
f10000 = get_lambdas_no_f(out_10000, N_samp, 9999, m, lambdas, data_50_tuning_10000, p, "n = 10000")

setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/tuning_no_f/lambdas")
saveRDS(f1000$BIC, paste0("lams_1000.RDS"))
saveRDS(f2000$BIC, paste0("lams_2000.RDS"))
saveRDS(f5000$BIC, paste0("lams_5000.RDS"))
saveRDS(f10000$BIC,paste0("lams_10000.RDS"))


