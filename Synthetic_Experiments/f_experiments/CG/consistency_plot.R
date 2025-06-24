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

m_seq = c(3,5,10,15,20, 25, 30, 35, 40)
n = c(1000,2000,5000,10000)
x_beta = NULL; x_beta = list(rep(x_beta), length(n)); x_f = x_beta
for(i in 1:length(n)){
  x_beta[[i]] = c(m3$rmse[[i]], m5$rmse[[i]], m10$rmse[[i]],m15$rmse[[i]], m20$rmse[[i]],
                  m25$rmse[[i]], m30$rmse[[i]],m35$rmse[[i]], m40$rmse[[i]])
  x_f[[i]] = c(m3$f_mse[[i]], m5$f_mse[[i]], m10$f_mse[[i]],m15$f_mse[[i]], m20$f_mse[[i]],
               m25$f_mse[[i]], m30$f_mse[[i]],m35$f_mse[[i]], m40$f_mse[[i]])
}



# consistency plot
m_star = 10;
id = which(m_seq == m_star)
mse_f_star_gamma = map(x_f, id)

df3 = data.frame(data = unlist(mse_f_star_gamma), n = n)

p3 = ggplot(df3, aes(x=n, y = data)) +
    geom_line() + 
    labs(y = "MSE",x = expression(n)) + 
  theme_minimal()
p3 


# normal ------------------------------------------------------------------

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

m25 = round(get_results_2(25) ,2)
m30 = round(get_results_2(30) ,2)
m35 = round(get_results_2(35), 2)
m40 = round(get_results_2(40), 2)


m_seq = c(3,5,10,15,20, 25, 30, 35, 40)
n = c(1000,2000,5000,10000)
x_beta = NULL; x_beta = list(rep(x_beta), length(n)); x_f = x_beta
for(i in 1:length(n)){
  x_beta[[i]] = c(m3$rmse[[i]], m5$rmse[[i]], m10$rmse[[i]],m15$rmse[[i]], m20$rmse[[i]],
                  m25$rmse[[i]], m30$rmse[[i]],m35$rmse[[i]], m40$rmse[[i]])
  x_f[[i]] = c(m3$f_mse[[i]], m5$f_mse[[i]], m10$f_mse[[i]],m15$f_mse[[i]], m20$f_mse[[i]],
               m25$f_mse[[i]], m30$f_mse[[i]],m35$f_mse[[i]], m40$f_mse[[i]])
}


m_star = 20;
id = which(m_seq == m_star)
mse_f_star_norm = unlist(map(x_f, id))


df4 = data.frame(data = mse_f_star_norm, n = n)
df4
ggplot(df4, aes(x = n, y = data)) +
  geom_line()+
  labs(y = "MSE",x = expression(n)) + 
  theme_minimal() 
  

df = data.frame(data = c(unlist(mse_f_star_gamma), mse_f_star_norm), n = rep(n, 2), 
                type = factor(c(rep("gamma", 4), rep("normal",4))) )

# p1 = ggplot(data = df, aes(x = n, y = data, color = type)) +
#   geom_line()+
#   ylim(0, 3.2)+
#   xlim(1000, 10000) +
#   labs(x = expression(n), y = "MSE") +
#   theme_minimal() + 
#   #theme(legend.position = "top") +
#   theme(legend.title = element_blank())+
#   scale_color_manual(values=c("blue", "red"))+
#   theme(legend.position=c(0.8,0.75))

p1 <- ggplot(data = df, aes(x = n, y = data, linetype = type)) +
  geom_line() +
  ylim(0, 3.2) +
  xlim(1000, 10000) +
  labs(x = expression(n), y = "MSE") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme(legend.position = c(0.8, 0.75))



p1 
setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG")
ggsave("consistency_plot_bw.pdf", plot = p1, width = 3.5, height =3.5, units = "in")
