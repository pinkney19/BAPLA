setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
# Load 60 Trials data -----------------------------------------------------
left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/left.RDS")
right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/right.RDS")


# Load 30 Trials data -----------------------------------------------------
left2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/30_Trials/left2.RDS")
right2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/30_Trials/right2.RDS")



# Average No. Points  -----------------------------------------------------

par(mfrow = c(1,2))
plot_avg_points(left, right, "60 Trials")
plot_avg_points(left2, right2, "30 Trials")

