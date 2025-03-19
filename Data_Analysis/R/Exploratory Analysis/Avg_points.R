
# Load 60 Trials data -----------------------------------------------------
left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/left.RDS")
right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/right.RDS")


# Load 30 Trials data -----------------------------------------------------
left2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/30_Trials/left2.RDS")
right2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/30_Trials/right2.RDS")



# Average No. Points  -----------------------------------------------------

plot_avg_points = function(left_data, right_data, title){
  pp_left = lapply(left_data, rowSums)
  av_left = Reduce("+",pp_left)/length(pp_left)
  plot(seq(1, length(av_left)), av_left, col="blue", ylab = "Avg Points per Trial", xlab = "Unit", ylim = c(0,60), main = title)
  
  pp_right = lapply(right_data, rowSums)
  av_right = Reduce("+",pp_right)/length(pp_right)
  points(seq(1, length(av_right)), av_right, col="red")
  abline(h = 10, lty = 2)
  legend("topright", c("left", "right"), col=c("blue", "red"), pch = c(1,1))
  return(list(av_lft = av_left, av_rt = av_right))
}
par(mfrow = c(1,2))
plot_avg_points(left, right, "60 Trials")
plot_avg_points(left2, right2, "30 Trials")

