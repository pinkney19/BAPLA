plot_errs_interaction_gamma <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/plots/plot_errs_interaction_gamma.RDS")
plot_errs_gamma_f <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/plots/plot_errs_gamma_f.RDS")

library(ggpubr)
p1 = ggarrange(plot_errs_interaction_gamma, plot_errs_gamma_f, common.legend = T)
p1
plot_errs_interaction_normal <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/plots/plot_errs_interaction_normal.RDS")
plot_errs_normal_f <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/plots/plot_errs_normal_f.RDS")

p2 = ggarrange(plot_errs_interaction_normal, plot_errs_normal_f, common.legend = T)


p1
p2
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/plots")
ggsave("normal_plots.pdf", plot = p2, width = 4, height =3, units = "in")
ggsave("gamma_plot.pdf", plot = p1, width = 4, height =3, units = "in")
