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




m_seq = c(25,30,35,40)

#for(i in 1:length(m_seq)){
  
for(i in 1:length(m_seq)){  
  
  print(paste("No basis functions is ", m_seq[i])) 
  
  out_1000 <- readRDS(paste0("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_gamma/beta_0.1/tuning_bigm/out/",m_seq[i],"/out_1000.RDS"))
  out_2000 <- readRDS(paste0("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_gamma/beta_0.1/tuning_bigm/out/",m_seq[i],"/out_2000.RDS"))
  out_5000 <- readRDS(paste0("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_gamma/beta_0.1/tuning_bigm/out/",m_seq[i],"/out_5000.RDS"))
  out_10000 <- readRDS(paste0("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_gamma/beta_0.1/tuning_bigm/out/",m_seq[i],"/out_10000.RDS"))
  
  m = m_seq[i]
  lambdas = logspace(log10(1),log10(150),100)
  
  N_samp= 10;
  par(mfrow = c(1,4))
  p = 10
  f1000 = get_lambdas(out_1000, N_samp, 999, m, lambdas, data_50_tuning_1000, p, paste("m =", m_seq[i], "and n = 1000"))
  f2000 = get_lambdas(out_2000, N_samp, 1999, m, lambdas, data_50_tuning_2000, p, paste("m =", m_seq[i], "and n = 2000"))
  f5000 = get_lambdas(out_5000, N_samp, 4999, m, lambdas, data_50_tuning_5000, p, paste("m =", m_seq[i], "and n = 5000"))
  f10000 = get_lambdas(out_10000, N_samp, 9999, m, lambdas, data_50_tuning_10000, p, paste("m =", m_seq[i], "and n = 10000"))
  
  setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/tuning_bigm/lambdas")
  saveRDS(f1000$BIC, paste0("lams_",m_seq[i],"_1000.RDS"))
  saveRDS(f2000$BIC, paste0("lams_",m_seq[i],"_2000.RDS"))
  saveRDS(f5000$BIC, paste0("lams_",m_seq[i],"_5000.RDS"))
  saveRDS(f10000$BIC,paste0("lams_",m_seq[i],"_10000.RDS"))

}

