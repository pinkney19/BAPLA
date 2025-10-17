
# Results table p = 10 ----------------------------------------------------

# Function to get results table
get_results = function(p, model_type, inf){
  # Estimation
  rmse_s1_50 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/rmse_50.RDS"))
  rmse_s1_zero <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/rmse_zero.RDS"))
  
  # mses for intercept
  int_mse_50 = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/int_mses_50.RDS"))
  int_mse_zero = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/int_mses_zero.RDS"))
  
  # mses for f
  f_mse_50 = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/f_mses_50.RDS"))
  f_mse_zero = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/f_mses_zero.RDS"))
  
  # AUROC
  AUC_50 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/AUC_50.RDS"))
  AUC_zero <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/AUC_zero.RDS"))
  
  # Inference
  if(inf==T){
    if(p!=100){
    res_50_s1 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Inference/",p,"/new_stage1/res_50_s1.RDS"))
    res_zero_s1 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Inference/",p,"/new_stage1/res_zero_s1.RDS"))
    }
    if(p==100){
      res_50_s1 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Inference/",p,"/res_50_s1.RDS"))
      res_zero_s1 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Inference/",p,"/res_zero_s1.RDS"))
    }
  }
  
  if(inf==F){
    res_50_s1 = matrix(NA, 4, 4)
    res_zero_s1 = matrix(NA, 4, 4)
  }
  
  out_50 = cbind(as.numeric(rmse_s1_50), as.numeric(int_mse_50), as.numeric(f_mse_50), as.numeric(AUC_50), 
                 as.numeric(res_50_s1[,1]), as.numeric(res_50_s1[,2]), as.numeric(res_50_s1[,3]), as.numeric(res_50_s1[,4]))
 
  
  out_zero = cbind(as.numeric(rmse_s1_zero), as.numeric(int_mse_zero), as.numeric(f_mse_zero), as.numeric(AUC_zero), as.numeric(res_zero_s1[,1]), 
                   as.numeric(res_zero_s1[,2]), as.numeric(res_zero_s1[,3]), as.numeric(res_zero_s1[,4]))
 
  
  out = round(rbind(out_50, out_zero),2)
  return(out)
  
}

get_ses = function(p, model_type, inf){
  # standard errors for interaction matrix
  ses_50 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/ses_50.RDS"))
  ses_zero <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/ses_zero.RDS"))
  
  # standard errors for intercept
  int_ses_50 = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/int_ses_50.RDS"))
  int_ses_zero = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/int_ses_zero.RDS"))
  
  # se for f
  f_ses_50 = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/f_ses_50.RDS"))
  f_ses_zero = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/f_ses_zero.RDS"))
  
  # se AUROC
  AUC_ses_50 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/AUC_se_50.RDS"))
  AUC_ses_zero <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Simulations/",p,"/AUC_se_zero.RDS"))
  
  # Inference
  if(inf==T){
    if(p!=100){
      res_50_s1 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Inference/",p,"/new_stage1/res_50_s1_ses.RDS"))
      res_zero_s1 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Inference/",p,"/new_stage1/res_zero_s1_ses.RDS"))
    }
    if(p==100){
      res_50_s1 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Inference/",p,"/res_50_s1_ses.RDS"))
      res_zero_s1 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/",model_type,"/Inference/",p,"/res_zero_s1_ses.RDS"))
    }
      }
  
  if(inf==F){
    res_50_s1 = matrix(NA, 4, 4)
    res_zero_s1 = matrix(NA, 4, 4)
  }
  
  out_50 = cbind(as.numeric(ses_50), as.numeric(int_ses_50), as.numeric(f_ses_50), as.numeric(AUC_ses_50), 
                 as.numeric(res_50_s1[,1]), as.numeric(res_50_s1[,2]), as.numeric(res_50_s1[,3]), as.numeric(res_50_s1[,4]))
  
  
  out_zero = cbind(as.numeric(ses_zero), as.numeric(int_ses_zero), as.numeric(f_ses_zero), as.numeric(AUC_ses_zero), as.numeric(res_zero_s1[,1]), 
                   as.numeric(res_zero_s1[,2]), as.numeric(res_zero_s1[,3]), as.numeric(res_zero_s1[,4]))
  
  
  out = round(rbind(out_50, out_zero),2)
  return(out)
  
}


CG_10 = get_results(10, "CG", inf = T)
CG_50 = get_results(50, "CG", inf = T)
CG_100 = get_results(100, "CG", T)


ER_10 = get_results(10, "ER", inf = T)
ER_50 = get_results(50, "ER", inf = T)
ER_100 = get_results(100,"ER", T)

SB_10 = get_results(10, "SB", inf=T)
SB_50 = get_results(50, "SB", inf=T)
SB_100 = get_results(100,"SB", T)


table = rbind(CG_10, CG_50, ER_10, ER_50, SB_10, SB_50)


tbl = as.data.frame(table)



n=c(1000,2000, 5000, 10000)
n= rep(n,12)
beta_0 = c(rep(0.1,4), rep(-2.6,4))
beta_0 = rep(beta_0, 6) 
d = c(rep(10,8), rep(50,8))
d = rep(d, 3)
library(dplyr)

tbl <- tbl %>% mutate(n = n)
# reorder 
tbl <- tbl %>% select(n, everything())

tbl <- tbl %>% mutate(beta_0 = beta_0)
# reorder 
tbl <- tbl %>% select(beta_0, everything())

tbl <- tbl %>% mutate(d = d)
# reorder 
tbl <- tbl %>% select(d, everything())
tbl
library(xtable)
xtable(tbl)



# standard errors ---------------------------------------------------------
CG_10 = get_ses(10, "CG", inf = T)
CG_50 = get_ses(50, "CG", inf = T)
CG_100 = get_ses(100, "CG", T)

ER_10 = get_ses(10, "ER", inf = T)
ER_50 = get_ses(50, "ER", inf = T)
ER_100 = get_ses(100,"ER", T)

SB_10 = get_ses(10, "SB", inf=T)
SB_50 = get_ses(50, "SB", inf=T)
SB_100 = get_ses(100, "SB", T)

table = rbind(CG_10, CG_50, ER_10, ER_50, SB_10, SB_50)


tbl = as.data.frame(table)

n=c(1000,2000, 5000, 10000)
n= rep(n,12)
beta_0 = c(rep(0.1,4), rep(-2.6,4))
beta_0 = rep(beta_0, 6) 
d = c(rep(10,8), rep(50,8))

d = rep(d, 3)
library(dplyr)

tbl <- tbl %>% mutate(n = n)
# reorder 
tbl <- tbl %>% select(n, everything())

tbl <- tbl %>% mutate(beta_0 = beta_0)
# reorder 
tbl <- tbl %>% select(beta_0, everything())

tbl <- tbl %>% mutate(d = d)
# reorder 
tbl <- tbl %>% select(d, everything())
tbl
library(xtable)
xtable(tbl)









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


