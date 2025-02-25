AUC_3m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/AUC_3m.RDS")
AUC_5m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/AUC_5m.RDS")
AUC_10m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/AUC_10m.RDS")
AUC_15m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/AUC_15m.RDS")
AUC_20m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/AUC_20m.RDS")
AUC <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/sims_no_f/formatted_res/AUC.RDS")

# AUROC plot

df =data.frame(n = rep(c(1000,2000,5000,10000), 6), data = c(AUC, AUC_3m, AUC_5m, AUC_10m, AUC_15m, AUC_20m), 
               m = factor(rep(c(0,3,5,10,15,20), each = 4)))
p1 = ggplot(df, aes(x = n, y = data, color = m)) +
  geom_line() +
  ylim(0, 1) +
  labs(y = "Average AUROC", x = expression(n), title = "") +
  theme_minimal()
p1

# RMSE plot
rmse <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/sims_no_f/formatted_res/rmse.RDS")
rmse_3m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/rmse_3m.RDS")
rmse_5m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/rmse_5m.RDS")
rmse_10m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/rmse_10m.RDS")
rmse_15m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/rmse_15m.RDS")
rmse_20m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/rmse_20m.RDS")

df2 = data.frame(n = rep(c(1000,2000,5000,10000), 6), data = c(rmse, rmse_3m, rmse_5m, rmse_10m, rmse_15m, rmse_20m), 
                 m = factor(rep(c(0,3,5,10,15,20), each = 4)))
p2 = ggplot(df2, aes(x = n, y = data, color = m)) +
  geom_line() +
  ylim(0, 1) +
  labs(y = "Average RMSE", x = expression(n), title = "") +
  theme_minimal()
p2

ggarrange(p1,p2,ncol=2, common.legend = T, legend = "right")
