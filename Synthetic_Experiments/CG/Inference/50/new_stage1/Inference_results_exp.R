# Libraries 

library(purrr) # map
setwd("~/Downloads/BAPLA/Synthetic_Experiments")
source("BAPLA_Functions.R")


# Ground truth and structure of data --------------------------------------

n_samp = seq(1,100)
p = 50; 

true_beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_50.RDS")

# Stage 1 Results ---------------------------------------------------------

# Read 50/50 data from STORM 
out_50_1000_desparsified <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Inference/50/invert_stage1/out_50_1000_desparsified.RDS")
out_50_2000_desparsified <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Inference/50/invert_stage1/out_50_2000_desparsified.RDS")
out_50_5000_desparsified <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Inference/50/invert_stage1/out_50_5000_desparsified.RDS")
out_50_10000_desparsified <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Inference/50/invert_stage1/out_50_10000_desparsified.RDS")


# Results for 50/50 data
out50 = list(out_50_1000_desparsified, out_50_2000_desparsified, out_50_5000_desparsified, out_50_1000_desparsified)
res_50_s1 = Format_results(true_beta, out50, n_samp, p, "stage 1 ")

# zero results 
out_zero_1000_desparsified <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Inference/50/invert_stage1/out_zero_1000_desparsified.RDS")
out_zero_2000_desparsified <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Inference/50/invert_stage1/out_zero_2000_desparsified.RDS")
out_zero_5000_desparsified <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Inference/50/invert_stage1/out_zero_5000_desparsified.RDS")
out_zero_10000_desparsified <- readRDS("~/luna/BAPLA/Synthetic_Experiments/CG/Inference/50/invert_stage1/out_zero_10000_desparsified.RDS")


outzero = list(out_zero_1000_desparsified, out_zero_2000_desparsified, out_zero_5000_desparsified, out_zero_10000_desparsified)

res_zero_s1 = Format_results(true_beta, outzero, n_samp, p, "stage 1")

setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Inference/50/new_stage1")



saveRDS(res_50_s1$avg, "res_50_s1.RDS")
saveRDS(res_zero_s1$avg, "res_zero_s1.RDS")

saveRDS(res_50_s1$std, "res_50_s1_ses.RDS")
saveRDS(res_zero_s1$std, "res_zero_s1_ses.RDS")



