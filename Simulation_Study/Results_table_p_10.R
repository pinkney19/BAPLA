
# Results table p = 10 ----------------------------------------------------


# Chain graph--------------------------------------------------------------

# p = 10
rmse_s1_50 <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/10/rmse_50.RDS")
rmse_s1_zero <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/10/rmse_zero.RDS")


AUC_50 <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/10/AUC_50.RDS")
AUC_zero <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/Simulations/10/AUC_zero.RDS")

a_10_p = cbind(rmse_s1_50,  rmse_s1_zero,  AUC_50, AUC_zero)
round(a_10_p,2)

CG = round(a_10_p, 2)
CG

# ER graph --------------------------------------------------------------

# p = 10
rmse_s1_50 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Simulations/10/rmse_50.RDS")
rmse_s1_zero <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Simulations/10/rmse_zero.RDS")


AUC_50 <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Simulations/10/AUC_50.RDS")
AUC_zero <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/Simulations/10/AUC_zero.RDS")

b_10_p = cbind(rmse_s1_50,  rmse_s1_zero,  AUC_50, AUC_zero)
round(b_10_p,2)

ER = round(b_10_p, 2)
ER

# SB graph --------------------------------------------------------------

# p = 10
rmse_s1_50 <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/Simulations/10/rmse_50.RDS")
rmse_s1_zero <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/Simulations/10/rmse_zero.RDS")


AUC_50 <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/Simulations/10/AUC_50.RDS")
AUC_zero <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/Simulations/10/AUC_zero.RDS")

c_10_p = cbind(rmse_s1_50,  rmse_s1_zero,  AUC_50, AUC_zero)
round(c_10_p,2)

SB = round(c_10_p, 2)
SB


tbl = round(rbind(CG, SB, ER), 2)
tbl = as.data.frame(tbl)

n=c(1000,2000, 5000, 10000)
n= rep(n,3)
p = c(10,10,10)
p = rep(p, 4)

library(dplyr)

tbl <- tbl %>% mutate(n = n)
# reorder 
tbl <- tbl %>% select(n, everything())

tbl <- tbl %>% mutate(p = p)
# reorder 
tbl <- tbl %>% select(p, everything())
tbl
library(xtable)
xtable(tbl)



# standard errors ---------------------------------------------------------

# Model a
# p = 10

se_s1_50 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Simulations/10/ses_50.RDS")
se_s2_50 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Refit/10/ses_50.RDS")

se_s1_zero <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Simulations/10/ses_zero.RDS")
se_s2_zero <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Refit/10/ses_zero.RDS")

se_AUC_50 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Simulations/10/AUC_se_50.RDS")
se_AUC_zero <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Simulations/10/AUC_se_zero.RDS")

a_10_se = cbind(se_s1_50, se_s2_50, se_s1_zero, se_s2_zero, se_AUC_50, se_AUC_zero)
round(a_10_se,2)

# p =50
se_s1_50 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Simulations/50/ses_50.RDS")
se_s2_50 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Refit/50/ses_50.RDS")

se_s1_zero <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Simulations/50/ses_zero.RDS")
se_s2_zero <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Refit/50/ses_zero.RDS")

se_AUC_50 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Simulations/50/AUC_se_50.RDS")
se_AUC_zero <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Simulations/50/AUC_se_zero.RDS")

a_50_se = cbind(se_s1_50, se_s2_50, se_s1_zero, se_s2_zero, se_AUC_50, se_AUC_zero)
round(a_50_se,2)

Model_A = round(rbind(a_10_se, a_50_se),2)
Model_A


# Model b
# Model a
# p = 10

se_s1_50 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Simulations/10/ses_50.RDS")
se_s2_50 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Refit/10/ses_50.RDS")

se_s1_zero <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Simulations/10/ses_zero.RDS")
se_s2_zero <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Refit/10/ses_zero.RDS")

se_AUC_50 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Simulations/10/AUC_se_50.RDS")
se_AUC_zero <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Simulations/10/AUC_se_zero.RDS")

a_10_se = cbind(se_s1_50, se_s2_50, se_s1_zero, se_s2_zero, se_AUC_50, se_AUC_zero)
round(a_10_se,2)

# p =50
se_s1_50 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Simulations/50/ses_50.RDS")
se_s2_50 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Refit/50/ses_50.RDS")

se_s1_zero <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Simulations/50/ses_zero.RDS")
se_s2_zero <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Refit/50/ses_zero.RDS")

se_AUC_50 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Simulations/50/AUC_se_50.RDS")
se_AUC_zero <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Simulations/50/AUC_se_zero.RDS")

a_50_se = cbind(se_s1_50, se_s2_50, se_s1_zero, se_s2_zero, se_AUC_50, se_AUC_zero)
round(a_50_se,2)

Model_B = round(rbind(a_10_se, a_50_se),2)
Model_B



# Inference Results -------------------------------------------------------


# Model A -----------------------------------------------------------------
res_50_s1 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Inference/10/res_50_s1.RDS")
res_50_s2 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Inference/10/res_50_s2.RDS")

res_zero_s1 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Inference/10/res_zero_s1.RDS")
res_zero_s2 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Inference/10/res_zero_s2.RDS")


round(res_50_s1,2)


s = cbind(res_50_s1[,(1)], res_50_s2[,(1)], res_zero_s1[,(1)], res_zero_s2[,(1)])
sc = cbind(res_50_s1[,(2)], res_50_s2[,(2)], res_zero_s1[,(2)], res_zero_s2[,(2)])

len_s = cbind(res_50_s1[,(3)], res_50_s2[,(3)], res_zero_s1[,(3)], res_zero_s2[,(3)])
len_sc = cbind(res_50_s1[,(4)], res_50_s2[,(4)], res_zero_s1[,(4)], res_zero_s2[,(4)])


a_10_cov = cbind(s, sc) 
a_10_length = cbind(len_s, len_sc)

# p = 50

res_50_s1 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Inference/50/res_50_s1.RDS")
res_50_s2 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Inference/50/res_50_s2.RDS")

res_zero_s1 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Inference/50/res_zero_s1.RDS")
res_zero_s2 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_A/Inference/50/res_zero_s2.RDS")


round(res_50_s1,2)


s = cbind(res_50_s1[,(1)], res_50_s2[,(1)], res_zero_s1[,(1)], res_zero_s2[,(1)])
sc = cbind(res_50_s1[,(2)], res_50_s2[,(2)], res_zero_s1[,(2)], res_zero_s2[,(2)])

len_s = cbind(res_50_s1[,(3)], res_50_s2[,(3)], res_zero_s1[,(3)], res_zero_s2[,(3)])
len_sc = cbind(res_50_s1[,(4)], res_50_s2[,(4)], res_zero_s1[,(4)], res_zero_s2[,(4)])


a_50_cov = cbind(s, sc)
a_50_length = cbind(len_s, len_sc)

Model_a_cov = round(rbind(a_10_cov, a_50_cov),2)
Model_a_lengths = round(rbind(a_10_length, a_50_length),2)


# Model B -----------------------------------------------------------------
res_50_s1 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Inference/10/res_50_s1.RDS")
res_50_s2 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Inference/10/res_50_s2.RDS")

res_zero_s1 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Inference/10/res_zero_s1.RDS")
res_zero_s2 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Inference/10/res_zero_s2.RDS")


round(res_50_s1,2)


s = cbind(res_50_s1[,(1)], res_50_s2[,(1)], res_zero_s1[,(1)], res_zero_s2[,(1)])
sc = cbind(res_50_s1[,(2)], res_50_s2[,(2)], res_zero_s1[,(2)], res_zero_s2[,(2)])

len_s = cbind(res_50_s1[,(3)], res_50_s2[,(3)], res_zero_s1[,(3)], res_zero_s2[,(3)])
len_sc = cbind(res_50_s1[,(4)], res_50_s2[,(4)], res_zero_s1[,(4)], res_zero_s2[,(4)])


a_10_cov = cbind(s, sc)
a_10_length = cbind(len_s, len_sc)

# p = 50

res_50_s1 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Inference/50/res_50_s1.RDS")
res_50_s2 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Inference/50/res_50_s2.RDS")

res_zero_s1 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Inference/50/res_zero_s1.RDS")
res_zero_s2 <- readRDS("~/Downloads/GAPLA_FC/Synthetic_Experiments/Model_B2/Inference/50/res_zero_s2.RDS")


round(res_50_s1,2)


s = cbind(res_50_s1[,(1)], res_50_s2[,(1)], res_zero_s1[,(1)], res_zero_s2[,(1)])
sc = cbind(res_50_s1[,(2)], res_50_s2[,(2)], res_zero_s1[,(2)], res_zero_s2[,(2)])

len_s = cbind(res_50_s1[,(3)], res_50_s2[,(3)], res_zero_s1[,(3)], res_zero_s2[,(3)])
len_sc = cbind(res_50_s1[,(4)], res_50_s2[,(4)], res_zero_s1[,(4)], res_zero_s2[,(4)])


a_50_cov = cbind(s, sc)
a_50_length = cbind(len_s, len_sc)

Model_B_cov = round(rbind(a_10_cov, a_50_cov),2)
Model_B_lengths = round(rbind(a_10_length, a_50_length),2)

tbl2_covs = rbind(Model_a_cov, Model_B_cov)
tbl2_covs = as.data.frame(tbl2_covs)

n=c(1000,2000, 5000, 10000)
n= rep(n,4)
p = c(10,10,10,10,50,50, 50, 50)
p = rep(p, 2)

library(dplyr)

tbl2_covs <- tbl2_covs %>% mutate(n = n)
# reorder 
tbl2_covs <- tbl2_covs %>% select(n, everything())

tbl2_covs <- tbl2_covs %>% mutate(p = p)
# reorder 
tbl2_covs <- tbl2_covs %>% select(p, everything())
tbl2_covs
library(xtable)
xtable(tbl2_covs)


# table for length of the confidence intervals ----------------------------


