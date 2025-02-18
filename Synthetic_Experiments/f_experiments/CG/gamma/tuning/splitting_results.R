# Load libraries ----------------------------------------------------------

library(purrr)
library(pracma)

setwd("~/Downloads/BAPLA/Synthetic_Experiments")
source("BAPLA_Functions.R")


# Chain Graph ----------------------------------------------

p = 10; 
beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_10.RDS")
m_seq = c(3,5,10,15,20)
data_seq = seq(1, 10)
lambdas = logspace(log10(10),log10(300),100)
grid = expand.grid(sample = data_seq, lams = lambdas,  m = m_seq)
list_of_pairs <- split(grid, seq(nrow(grid)))

# Load tuning results from STORM ----------------------------------------------
out_50_1000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1/tuning/out_50_1000.RDS")
out_50_2000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1/tuning/out_50_2000.RDS")
out_50_5000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1/tuning/out_50_5000.RDS")
out_50_10000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1/tuning/out_50_10000.RDS")


# Save for separate ms ----------------------------------------------------

ms = unname(unlist(map(list_of_pairs,3)))
# Split ms into 5 lists of length 1000
split_ms <- split(ms, ceiling(seq_along(ms) / 1000))


# do same for out files
out_1000 = split(out_50_1000, ceiling(seq_along(out_50_1000) / 1000 ) )
out_2000 = split(out_50_2000, ceiling(seq_along(out_50_2000) / 1000 ) )
out_5000 = split(out_50_5000, ceiling(seq_along(out_50_5000) / 1000 ) )
out_10000 = split(out_50_10000, ceiling(seq_along(out_50_10000) / 1000 ) )


# Function to save output files for each m --------------------------------

save_m_out = function(out, n, m_seq){
  for(i in 1:length(out)){
    setwd(paste0("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1/tuning/out/",m_seq[i]))
    saveRDS(out[[i]], paste0("out_", n, ".RDS"))
  }
}

save_m_out(out_10000, 10000, m_seq)

# # eBIC --------------------------------------------------------------------
# 
# # load tuning data files
# 
# data_50_tuning_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Tuning_data/data_50_tuning_1000.RDS")
# data_50_tuning_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Tuning_data/data_50_tuning_2000.RDS")
# data_50_tuning_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Tuning_data/data_50_tuning_5000.RDS")
# data_50_tuning_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Tuning_data/data_50_tuning_10000.RDS")
# 
# 
# # Format Results ----------------------------------------------------------
# 
# #N_samp = Number of training samples
# N_samp = 10;  m=10;
# library(boot) #inv.logit
# library(splines)
# 
# res2 = get_bic_vals(out_zero_1000, N_samp, t_end = 999, m,lambdas, data_zero_tuning_1000,2, 0.5, 10, threshold = 1e-5)
# 
# plot(lambdas, res2$BIC, type='l', main="BIC")
# abline(v =lambdas[which.min(res2$BIC)], col="red")
# 
# 
# 
# library(splines)
# library(boot)
# par(mfrow = c(1,4))
# f1000 = get_lambdas(out_50_1000, N_samp, 999, m, lambdas, data_50_tuning_1000, p, "50 - 1000")
# f2000 = get_lambdas(out_50_2000, N_samp, 1999, m, lambdas, data_50_tuning_2000, p, "50 - 2000")
# f5000 = get_lambdas(out_50_5000, N_samp, 4999, m, lambdas, data_50_tuning_5000, p, "50 - 5000")
# f10000 = get_lambdas(out_50_10000, N_samp, 9999, m, lambdas, data_50_tuning_10000, p, "50 - 10000")
# 
# z1000 = get_lambdas(out_zero_1000, N_samp, 999, m, lambdas, data_zero_tuning_1000, p, "zero - 1000")
# z2000 = get_lambdas(out_zero_2000, N_samp, 1999, m, lambdas, data_zero_tuning_2000, p, "zero - 2000")
# z5000 = get_lambdas(out_zero_5000, N_samp, 4999, m, lambdas, data_zero_tuning_5000, p, "zero - 5000")
# z10000 = get_lambdas(out_zero_10000, N_samp, 9999, m, lambdas, data_zero_tuning_10000, p, "zero - 10000")
# 
# 
# 
# 
# setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/10")
# saveRDS(f1000$BIC, "lams_50_1000.RDS")
# saveRDS(f2000$BIC, "lams_50_2000.RDS")
# saveRDS(f5000$BIC, "lams_50_5000.RDS")
# saveRDS(f10000$BIC, "lams_50_10000.RDS")
# 
# saveRDS(z1000$BIC, "lams_zero_1000.RDS")
# saveRDS(z2000$BIC, "lams_zero_2000.RDS")
# saveRDS(z5000$BIC, "lams_zero_5000.RDS")
# saveRDS(z10000$BIC, "lams_zero_10000.RDS")
# 
