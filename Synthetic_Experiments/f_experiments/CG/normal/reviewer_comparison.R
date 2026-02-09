
# no f

AUC <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/sims_no_f/formatted_res/AUC.RDS")
rmse <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/sims_no_f/formatted_res/rmse.RDS")
int_mse <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/sims_no_f/formatted_res/int_mses.RDS")


# with f
AUC_50 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/10/AUC_50.RDS")
rmse_50 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/10/rmse_50.RDS")
int_mse_50 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/10/int_mses_50.RDS")


# plots
n = c(1000,2000,5000,10000)
par(mfrow =c(1,2))
plot(n, (rmse_50),xlab="n", ylab = "RMSE", type="l", ylim=c(0,.8))
lines(n,(rmse), col="blue")

plot(n, AUC_50, xlab="n", ylab="AUROC", ylim=c(0, 1), type='l')
points(n, AUC, col="blue", type='l')
library(xtable)
xtable(cbind(rmse, rmse_50, AUC, AUC_50))
