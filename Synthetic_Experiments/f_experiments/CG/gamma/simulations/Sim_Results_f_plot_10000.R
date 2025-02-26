# Load libraries ----------------------------------------------------------

library(purrr)
setwd("~/Downloads/BAPLA/Synthetic_Experiments")
source("BAPLA_Functions.R")




# Parameter values --------------------------------------------------------

p = 10; 
beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_10.RDS")


# m = 3 -------------------------------------------------------------------

m_seq = c(3, 5, 10, 15, 20);

library(splines)
library(ggplot2)
library(matrixStats)

# load simulation results -------------------------------------------------
plot_list = list();
for(i in 1:length(m_seq)){
  m = m_seq[i]
  out_10000 <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/results/Nas_removed/m_",m,"out_10000.RDS"))

  
  f_hat_50_10000 = get_f(out_10000, m, 9999, unit_id = 1)

 
  plot_list[[i]] = plot_f_cis_gamma(f_hat_50_10000, 1, paste0("m = ",m) ,10000)

}
library(ggpubr)

plot = ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]], ncol=5, common.legend = T)
plot


plot2 = ggarrange( plot_list[[3]], plot_list[[5]], ncol=2, common.legend = T)
plot2

setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/f_plots")
ggsave("ex_m_10000_gamma.pdf", plot = plot2, width = 10, height =3, units = "in")


