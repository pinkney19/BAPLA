# Load libraries ----------------------------------------------------------

library(purrr)
setwd("~/Downloads/BAPLA/Synthetic_Experiments")
source("BAPLA_Functions.R")




# Parameter values --------------------------------------------------------

p = 10; 
beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_10.RDS")


# m = 3 -------------------------------------------------------------------

m = 3;

# load simulation results -------------------------------------------------

out_1000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/results/m_",m,"out_1000.RDS"))
out_2000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/results/m_",m,"out_2000.RDS"))
out_5000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/results/m_",m,"out_5000.RDS"))
out_10000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/results/m_",m,"out_10000.RDS"))



# get F estimates  --------------------------------------------------------------
library(splines)


f_hat_50_1000 = get_f(out_1000, m, 999, unit_id = 1)
f_hat_50_2000 = get_f(out_2000, m, 1999, unit_id = 1)
f_hat_50_5000 = get_f(out_5000, m, 4999, unit_id = 1)
f_hat_50_10000 = get_f(out_10000, m, 9999, unit_id = 1)


library(ggplot2)
library(matrixStats)


f1 = plot_f_cis_normal(f_hat_50_1000, 1, "n = 1000",1000)
f2 = plot_f_cis_normal(f_hat_50_2000, 1, "n = 2000",2000)
f3 = plot_f_cis_normal(f_hat_50_5000, 1, "n = 5000",5000)
f4 = plot_f_cis_normal(f_hat_50_10000, 1, "n = 10000",10000)
# 
 plot = (ggarrange(f1,f2,f3,f4, ncol=4, common.legend =T))
print(plot)

 setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/f_plots")
 ggsave(paste0("plot_f_m", m, ".pdf"), plot = plot, width = 8, height =3, units = "in")


