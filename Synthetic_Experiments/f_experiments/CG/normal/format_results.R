get_results_2 = function(m){
  # Estimation
  rmse <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/rmse_",m,"m.RDS"))
  
  
  # mses for intercept
  int_mse <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/int_mses_",m,"m.RDS"))
  
  # mses for f
  f_mse = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/f_mses_",m,"m.RDS"))
  
  # AUROC
  AUC <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/AUC_",m,"m.RDS"))
  
  
  
  out = cbind(as.numeric(rmse), as.numeric(int_mse), as.numeric(f_mse), as.numeric(AUC))
  out = as.data.frame(out)
  names(out) = c("rmse", "int_mse", "f_mse", "AUC")
  return(out)
  
}

m3 = round(get_results_2(3), 2)
m5 = round(get_results_2(5) ,2)
m10 = round(get_results_2(10) ,2)
m15 = round(get_results_2(15), 2)
m20 = round(get_results_2(20), 2)


m_seq = c(3,5,10,15,20)
n = c(1000,2000,5000,10000)
x_beta = NULL; x_beta = list(rep(x_beta), length(n)); x_f = x_beta
for(i in 1:length(n)){
  x_beta[[i]] = c(m3$rmse[[i]], m5$rmse[[i]], m10$rmse[[i]],m15$rmse[[i]], m20$rmse[[i]])
  x_f[[i]] = c(m3$f_mse[[i]], m5$f_mse[[i]], m10$f_mse[[i]],m15$f_mse[[i]], m20$f_mse[[i]])
}

# plot(m_seq, x_beta[[1]], ylim =c(0,0.3), type='l', ylab = "Average RMSE", xlab = "m", main = expression(normal))
# for(i in 2:length(n)){
#   lines(m_seq, x_beta[[i]], col=i)
# }


library(ggplot2)

# Assuming m_seq and x_beta are already defined
data <- data.frame(m = rep(m_seq, times = length(x_beta)),
                   RMSE = unlist(x_beta),
                   n = factor(rep(n, each = length(m_seq))))

p1 = ggplot(data, aes(x = m, y = RMSE, color = n)) +
  geom_line() +
  ylim(0, 1) +
  geom_vline(xintercept = 10, color = "black", linetype = "dashed") +
  labs(y = "Average RMSE", x = expression(m), title = "") +
  theme_minimal()


data2 <- data.frame(m = rep(m_seq, times = length(x_beta)),
                    MSE_f = unlist(x_f),
                    n = factor(rep(n, each = length(m_seq))))

p2 = ggplot(data2, aes(x = m, y = MSE_f, color = n)) +
  geom_line() +
  ylim(0, 5) +
  geom_vline(xintercept = 10, color = "black", linetype = "dashed") +
  labs(y = "Average MSE", x = expression(m), title = expression(hat(f)[i](t))) +
  theme_minimal()

p1
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments")
ggsave("plot_errs_interaction_normal.pdf", plot = p1, width = 4, height =3, units = "in")

ggsave("plot_errs_normal_f.pdf", plot = p2, width = 4, height =3, units = "in")


library(ggpubr)
plot = ggarrange(p1, p2, ncol=2, common.legend = T)
plot


setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments")
ggsave("plot_errs_normal_f.pdf", plot = plot, width = 8, height =3, units = "in")


# Get boxplot plots -------------------------------------------------------


get_results_3 = function(m){
  # Estimation
  rmse <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/rmse_all",m,"m.RDS"))
  
  # mses for f
  f_mse = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/simulations/formatted_res/f_mses_all",m,"m.RDS"))
  
  return(list(rmse = rmse, f_mse = f_mse))
  
}


all3 = get_results_3(3)
all5 = get_results_3(5)
all10 = get_results_3(10)
all15 = get_results_3(15)
all20 = get_results_3(20)

df = data.frame(); df = rep(list(df), length(n))
for(i in 1:length(n)){
  df[[i]] =  cbind(all3$rmse[[i]], all5$rmse[[i]], all10$rmse[[i]], all15$rmse[[i]], all20$rmse[[i]])
}

i  = 1 
df =  cbind(all3$rmse[[i]], all5$rmse[[i]], all10$rmse[[i]], all15$rmse[[i]], all20$rmse[[i]])
boxplot(df)

# Import necessary libraries
library(ggplot2)
library(reshape2)

# Create a data frame using cbind
i <- 1
df <- data.frame(
  all3 = all3$rmse[[i]],
  all5 = all5$rmse[[i]],
  all10 = all10$rmse[[i]],
  all15 = all15$rmse[[i]],
  all20 = all20$rmse[[i]]
)

# Convert the data frame to a long format
df_long <- melt(df, variable.name = "Group", value.name = "RMSE")

# Use ggplot to create a boxplot
p = ggplot(df_long, aes(x = Group, y = RMSE)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of RMSE", x = "Group", y = "RMSE")

p2 = p + 


boxplot(df[[1]], ylim = c(0,0.35))
boxplot(df[[2]], col="red", add = T)
boxplot(df[[3]], col="green", add = T)
boxplot(df[[4]], col="blue", add = T)


# ggplot2 -----------------------------------------------------------------

