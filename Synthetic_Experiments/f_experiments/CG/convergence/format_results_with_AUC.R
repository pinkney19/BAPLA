get_results_2 = function(m){
  # Estimation
  rmse <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/convergence/formatted_res/rmse_",m,"m.RDS"))
  
  
  # mses for intercept
  int_mse <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/convergence/formatted_res/int_mses_",m,"m.RDS"))
  
  # mses for f
  f_mse = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/convergence/formatted_res/f_mses_",m,"m.RDS"))
  
  # AUROC
  AUC <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/convergence/formatted_res/AUC_",m,"m.RDS"))
  
  # divergent diagnostics 
  div = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/convergence/formatted_res/div_", m, "m.RDS"))
  # iterations
  avg_iter =  readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/convergence/formatted_res/avg_iter", m, "m.RDS"))
  out = cbind(as.numeric(rmse), as.numeric(int_mse), as.numeric(f_mse), as.numeric(AUC), as.numeric(div), as.numeric(avg_iter))
  out = as.data.frame(out)
  names(out) = c("rmse", "int_mse", "f_mse", "AUC", "div", "iter")
  return(out)
  
}

m3 = round(get_results_2(3), 4)
m5 = round(get_results_2(5) ,4)
m10 = round(get_results_2(10) ,4)
m15 = round(get_results_2(15), 4)
m20 = round(get_results_2(20), 4)
m25 = round(get_results_2(25) ,4)
m30 = round(get_results_2(30) ,4)
m35 = round(get_results_2(35), 4)
m40 = round(get_results_2(40), 4)


df_iter = data.frame(iterations = c(m3$iter, m5$iter, m10$iter, m15$iter, m20$iter, m25$iter, 
                                    m30$iter, m35$iter, m40$iter), 
                     m = c(rep("3", length(m3$iter)), rep("5", length(m5$iter)), rep("10",length(m10$iter)),
                           rep("15", length(m15$iter)), rep("20", length(m20$iter)), rep("25",length(m25$iter)), 
                           rep("30", length(m30$iter)), rep("35", length(m35$iter)), rep("40",length(m40$iter))) ,
                     n = rep(c(1000,2000,5000,10000), 9))



p_iter = ggplot(df_iter, aes(x = n, y = iterations, group = m, color = m))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  ylab("Average No. of Iterations to convergence")+xlab("n")+
  theme(legend.position="top", legend.text = element_text(size = 12), 
        axis.text = element_text(size = 11))+
  guides(colour = guide_legend(nrow =2))
p_iter
rmse0 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/sims_no_f/formatted_res/rmse.RDS")
AUC0 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/sims_no_f/formatted_res/AUC.RDS")
m_seq = c(3,5,10,15,20, 25, 30, 35, 40)
n = c(1000,2000,5000,10000)
x_beta = NULL; x_beta = list(rep(x_beta), length(n)); x_f = x_beta; x_AUC = x_beta;
for(i in 1:length(n)){
  x_beta[[i]] = c(rmse0[[i]], m3$rmse[[i]], m5$rmse[[i]], m10$rmse[[i]],m15$rmse[[i]], m20$rmse[[i]],
                  m25$rmse[[i]], m30$rmse[[i]],m35$rmse[[i]], m40$rmse[[i]])
  x_f[[i]] = c(m3$f_mse[[i]], m5$f_mse[[i]], m10$f_mse[[i]],m15$f_mse[[i]], m20$f_mse[[i]],
               m25$f_mse[[i]], m30$f_mse[[i]],m35$f_mse[[i]], m40$f_mse[[i]])
  x_AUC[[i]] = c(AUC0[[i]], m3$AUC[[i]], m5$AUC[[i]], m10$AUC[[i]],m15$AUC[[i]], m20$AUC[[i]],
                 m25$AUC[[i]], m30$AUC[[i]],m35$AUC[[i]], m40$AUC[[i]])
}

# plot(m_seq, x_beta[[1]], ylim =c(0,0.3), type='l', ylab = "Average RMSE", xlab = "m", main = expression(normal))
# for(i in 2:length(n)){
#   lines(m_seq, x_beta[[i]], col=i)
# }


library(ggplot2)

m_seq2 = c(0, 3,5,10,15,20, 25, 30, 35, 40)


data <- data.frame(m = rep(m_seq2, times = length(x_beta)),
                   RMSE = unlist(x_beta),
                   n = factor(rep(n, each = length(m_seq2))))

p1 = ggplot(data, aes(x = m, y = RMSE, color = n)) +
  geom_line() +
  ylim(0, 0.85) +
  geom_vline(xintercept = 10, color = "black", linetype = "dashed") +
  labs(y = "Average RMSE", x = expression(m), title = "") +
  theme_minimal()
p1

data2 <- data.frame(m = rep(m_seq, times = length(x_beta)),
                    MSE_f = unlist(x_f),
                    n = factor(rep(n, each = length(m_seq))))

p2 = ggplot(data2, aes(x = m, y = MSE_f, color = n)) +
  geom_line() +
  ylim(0, 9) +
  geom_vline(xintercept = 10, color = "black", linetype = "dashed") +
  labs(y = "Average MSE", x = expression(m), title = "") +
  theme_minimal()


p2

## plot for AUC

data3<- data.frame(m = rep(m_seq2, times = length(x_beta)),
                   AUC = unlist(x_AUC),
                   n = factor(rep(n, each = length(m_seq2))))
p3 = ggplot(data3, aes(x = m, y = AUC, color = n)) +
  geom_line() +
  ylim(0, 1) +
  geom_vline(xintercept = 10, color = "black", linetype = "dashed") +
  labs(y = "Average AUROC", x = expression(m), title = "") +
  theme_minimal()
p3

setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/convergence/plots")
ggsave("p_iter.pdf", plot = p_iter, width = 6, height =4, units = "in")
#saveRDS(p1, "plot_errs_interaction_normal.RDS")
#saveRDS(p2, "plot_errs_normal_f.RDS")
#saveRDS(p3, "plot_errs_AUC.RDS")

ggsave("plot_errs_interaction_normal.pdf", plot = p1, width = 4, height =3, units = "in")
ggsave("plot_errs_normal_f.pdf", plot = p2, width = 4, height =3, units = "in")
ggsave("plot_errs_AUC_normal.pdf", plot = p3, width =4, height = 3, units ="in" )


library(ggpubr)


all_plots = ggarrange(p1, p3, p2, common.legend = T, legend="top", ncol=3)
all_plots

ggsave("all_plots_normal.pdf", plot=  all_plots, width =8, height=3, units ="in")

plot = ggarrange(p1, p2, ncol=2, common.legend = T)
plot



setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments")
ggsave("plot_errs_normal_f.pdf", plot = plot, width = 8, height =3, units = "in")


# Get boxplot plots -------------------------------------------------------


get_results_3 = function(m){
  # Estimation
  rmse <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/convergence/formatted_res/rmse_all",m,"m.RDS"))
  
  # mses for f
  f_mse = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/convergence/formatted_res/f_mses_all",m,"m.RDS"))
  
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

