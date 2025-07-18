get_results_2 = function(m){
  # Estimation
  rmse <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/rmse_",m,"m.RDS"))
  
  
  # mses for intercept
  int_mse <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/int_mses_",m,"m.RDS"))
  
  # mses for f
  f_mse = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/f_mses_",m,"m.RDS"))
  
  # AUROC
  AUC <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/AUC_",m,"m.RDS"))
  
  
  
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


m25 = round(get_results_2(25) ,2)
m30 = round(get_results_2(30) ,2)
m35 = round(get_results_2(35), 2)
m40 = round(get_results_2(40), 2)

# no f
rmse0 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/sims_no_f/formatted_res/rmse.RDS")

m_seq = c(3,5,10,15,20, 25, 30, 35, 40)
n = c(1000,2000,5000,10000)
x_beta = NULL; x_beta = list(rep(x_beta), length(n)); x_f = x_beta
for(i in 1:length(n)){
  x_beta[[i]] = c(rmse0[[i]],m3$rmse[[i]], m5$rmse[[i]], m10$rmse[[i]],m15$rmse[[i]], m20$rmse[[i]],
                  m25$rmse[[i]], m30$rmse[[i]],m35$rmse[[i]], m40$rmse[[i]])
  x_f[[i]] = c(m3$f_mse[[i]], m5$f_mse[[i]], m10$f_mse[[i]],m15$f_mse[[i]], m20$f_mse[[i]],
               m25$f_mse[[i]], m30$f_mse[[i]],m35$f_mse[[i]], m40$f_mse[[i]])
}

# plot(m_seq, x_beta[[1]], ylim =c(0,0.3), type='l', ylab = "Average RMSE", xlab = "m", main = expression(Gamma))
# for(i in 2:length(n)){
#   lines(m_seq, x_beta[[i]], col=i)
# }




library(ggplot2)

m_seq2 = c(0, 3,5,10,15,20, 25, 30, 35, 40)

# Assuming m_seq and x_beta are already defined
data <- data.frame(m = rep(m_seq2, times = length(x_beta)),
                   RMSE = unlist(x_beta),
                   n = factor(rep(n, each = length(m_seq2))))

p1 = ggplot(data, aes(x = m, y = RMSE, color = n)) +
  geom_line() +
  ylim(0,0.85) +
  geom_vline(xintercept = 10, color = "black", linetype = "dashed") +
  labs(y = "Average RMSE", x = expression(m), title = "") +
  theme(axis.title.x = element_text(size = 12)) +
  theme_minimal()

p1

setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/plots")
saveRDS(p1, "plot_errs_interaction_gamma.RDS")


#ggsave("plot_errs_interaction_gamma.pdf", plot = p1, width = 4, height =3, units = "in")

data2 <- data.frame(m = rep(m_seq, times = length(x_beta)),
                    MSE_f = unlist(x_f),
                    n = factor(rep(n, each = length(m_seq))))

p2 = ggplot(data2, aes(x = m, y = MSE_f, color = n)) +
  geom_line() +
  ylim(0, 9) +
  geom_vline(xintercept = 10, color = "black", linetype = "dashed") +
  labs(y = "Average MSE", x = expression(m), title = ) +
  theme_minimal()

p2
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/plots")

saveRDS(p2, "plot_errs_gamma_f.RDS")

#ggsave("plot_errs_gamma_f.pdf", plot = p2, width = 4, height =3, units = "in")


# consistency plot
m_star = 10;
id = which(m_seq == m_star)
mse_f_star = map(x_f, 3)

df3 = data.frame(data = unlist(mse_f_star), n = n)

p3 = ggplot(df3, aes(x=n, y = data)) +
    geom_line() + 
    labs(y = "MSE",x = expression(n)) + 
  theme_minimal()
p3 

library(ggpubr)
plot = ggarrange(p1, p2, ncol=2, common.legend = T)
plot

setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments")
ggsave("plot_errs_f.pdf", plot = plot, width = 8, height =3, units = "in")


# Get boxplot plots -------------------------------------------------------


get_results_3 = function(m){
  # Estimation
  rmse <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/rmse_all",m,"m.RDS"))
  
  # mses for f
  f_mse = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/Nas_removed/f_mses_all",m,"m.RDS"))
  
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
df_long <- melt(df2, variable.name = "Group", value.name = "RMSE")

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

