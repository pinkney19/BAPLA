
# RMSE
rmse_5m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/rmse_5m.RDS")
rmse_10m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/rmse_10m.RDS")
rmse_20m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/rmse_20m.RDS")
rmse_40m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/rmse_40m.RDS")

# mse intercept
int_mses_5m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/int_mses_5m.RDS")
int_mses_10m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/int_mses_10m.RDS")
int_mses_20m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/int_mses_20m.RDS")
int_mses_40m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/int_mses_40m.RDS")

# fmse
f_mses_5m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/f_mses_5m.RDS")
f_mses_10m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/f_mses_10m.RDS")
f_mses_20m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/f_mses_20m.RDS")
f_mses_40m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/f_mses_40m.RDS")

# AUC
AUC_5m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/AUC_5m.RDS")
AUC_10m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/AUC_10m.RDS")
AUC_20m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/AUC_20m.RDS")
AUC_40m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/AUC_40m.RDS")


# comp time

m_5time_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/times/m_5time_10000.RDS")
m_10time_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/times/m_10time_10000.RDS")
m_20time_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/times/m_20time_10000.RDS")
m_40time_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/times/m_40time_10000.RDS")

# results for n = 10,000
id = 4
rmses = rbind(rmse_5m[id], rmse_10m[id], rmse_20m[id], rmse_40m[id])
int_mses = rbind(int_mses_5m[id], int_mses_10m[id], int_mses_20m[id], int_mses_40m[id])
f_mses = rbind(f_mses_5m[id],f_mses_10m[id], f_mses_20m[id], f_mses_40m[id] )
AUCs = rbind(AUC_5m[id], AUC_10m[id], AUC_20m[id], AUC_40m[id])

times = rbind(0.54373633333,as.numeric(m_10time_10000), as.numeric(m_20time_10000), as.numeric(m_40time_10000))

df = round(data.frame(rmses, int_mses, f_mses, AUCs, times),2)

library(xtable)
xtable(df)



# Gamma -------------------------------------------------------------------

# RMSE
rmse_5m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/rmse_5m.RDS")
rmse_10m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/rmse_10m.RDS")
rmse_20m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/rmse_20m.RDS")
rmse_40m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/rmse_40m.RDS")

# mse intercept
int_mses_5m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/int_mses_5m.RDS")
int_mses_10m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/int_mses_10m.RDS")
int_mses_20m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/int_mses_20m.RDS")
int_mses_40m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/int_mses_40m.RDS")

# fmse
f_mses_5m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/f_mses_5m.RDS")
f_mses_10m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/f_mses_10m.RDS")
f_mses_20m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/f_mses_20m.RDS")
f_mses_40m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/f_mses_40m.RDS")

# AUC
AUC_5m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/AUC_5m.RDS")
AUC_10m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/AUC_10m.RDS")
AUC_20m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/AUC_20m.RDS")
AUC_40m <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/AUC_40m.RDS")


# comp time

m_5time_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/times/m_5time_10000.RDS")
m_10time_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/times/m_10time_10000.RDS")
m_20time_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/times/m_20time_10000.RDS")
m_40time_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/times/m_40time_10000.RDS")

# results for n = 10,000
id = 4
rmses = rbind(rmse_5m[id], rmse_10m[id], rmse_20m[id], rmse_40m[id])
int_mses = rbind(int_mses_5m[id], int_mses_10m[id], int_mses_20m[id], int_mses_40m[id])
f_mses = rbind(f_mses_5m[id],f_mses_10m[id], f_mses_20m[id], f_mses_40m[id] )
AUCs = rbind(AUC_5m[id], AUC_10m[id], AUC_20m[id], AUC_40m[id])

times = rbind(0.54373633333,as.numeric(m_10time_10000), as.numeric(m_20time_10000), as.numeric(m_40time_10000))

df = round(data.frame(rmses, int_mses, f_mses, AUCs),2) #, times),2)

library(xtable)
xtable(df)
