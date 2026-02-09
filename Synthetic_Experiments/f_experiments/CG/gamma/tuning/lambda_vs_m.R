
# checks for lambda vs m --------------------------------------------------

m_seq = c(3,5,10,15,20,25,30, 35, 40);
n_seq = c(1000,5000,10000);

cols = c("deeppink", "lightgreen", "deepskyblue", "orange", "coral", "darkgreen", "blue", "violet","brown1", "darkorange2")


plot_lambdas = function(n, m_seq, p=10, gamma_type){
  lams=NULL; lams=rep(list(lams), p);
  for(j in 1:p){
    for(i in 1:length(m_seq)){
    if(gamma_type){
      lams[[j]][i] <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/tuning/lambdas/lams_",
                                   m_seq[i],"_", n, ".RDS"))[j]
    }
      if(gamma_type==F){
        lams[[j]][i] <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/normal/tuning/smaller_lams/lambdas/lams_",
                                       m_seq[i],"_", n, ".RDS"))[j]
      }
    }
  }
  m_lams = list();
  for(k in 1:length(m_seq)){
    m_lams[[k]] = unlist(map(lams, k))
  }
  names(m_lams) = m_seq
  lams_m = stack(m_lams)
  colnames(lams_m) = c("lambda", "m")
  df = as.data.frame(lams_m)
  plt = ggplot(df, aes(x = m, y = lambda))+
    geom_boxplot()+
    theme_minimal()+
    ylab(expression(lambda))
    #ggtitle(paste("n = ", n))
  return(plt)
}
  
#par(mfrow = c(1,3))
plt1 = plot_lambdas(1000, m_seq,p=10,  T)
plt2 = plot_lambdas(5000, m_seq,p=10,  T)
plt3 = plot_lambdas(10000, m_seq,p=10,  T)

setwd("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/tuning")
ggsave("lambda_v_m_10000.pdf", plt3, width = 3, height =2, units = "in")
library(ggpubr)
plt = ggarrange(plt1, plt2, plt3, ncol=1)






