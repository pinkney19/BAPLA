library(purrr)
library(pracma)

setwd("~/Downloads/BAPLA/Synthetic_Experiments")
source("BAPLA_Functions.R")


# BIC --------------------------------------------------------------------




# Format Results ----------------------------------------------------------

N_samp = 10; #Number of training samples  
library(boot) #inv.logit
library(splines)



# m = 3 -------------------------------------------------------------------
plot_BIC = function(n){

  m_seq = c(3,5,10,15,20)
  
  #for(i in 1:length(m_seq)){
  BIC = list(); AIC = list(); eBIC = list();
  for(i in 1:length(m_seq)){  
    
    print(paste("No basis functions is ", m_seq[i])) 
    
    out <- readRDS(paste0("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1/tuning/out/",m_seq[i],"/out_", n, ".RDS"))
    data <-  readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Tuning_data/data_50_tuning_", n, ".RDS"))
    
    m = m_seq[i]
    lambdas = logspace(log10(10),log10(300),100)
    
    N_samp= 10;
    
    p = 10
    res_1000 = get_bic_vals(out, N_samp, n-1, m, lambdas, data, 
                            5, gamma=0.5,  p, threshold=1e-5)
    
    BIC[[i]] = res_1000$BIC
    AIC[[i]] = res_1000$AIC
    eBIC[[i]] = res_1000$eBIC
  
  }
  ymin = log(min(unlist(lapply(BIC, min))))
  ymax = log(max(unlist(lapply(BIC, max))))
  plot(lambdas, log(BIC[[1]]), type='l', main = paste("n = ", n), ylim = c(7.05, 7.22), #c(ymin-0.01, ymax+0.01), 
       ylab = expression(log(BIC)), xlab = expression(lambda))
  lines(lambdas, log(BIC[[2]]), col="deeppink")
  lines(lambdas, log(BIC[[3]]), col="navy")
  lines(lambdas, log(BIC[[4]]), col="orange")
  lines(lambdas, log(BIC[[5]]), col="darkgreen")
  legend("bottomright", legend = c(3, 5, 10, 15, 20), lty = c(1,1,1,1, 1), 
         col=c("black", "deeppink", "navy", "orange", "darkgreen"), horiz = T, cex = 0.5)
  return(BIC)
}




# plot BIC - bigger m -----------------------------------------------------

plot_BIC2 = function(n){
  
  m_seq = c(25,30,35,40)
  
  #for(i in 1:length(m_seq)){
  BIC = list(); AIC = list(); eBIC = list();
  for(i in 1:length(m_seq)){  
    
    print(paste("No basis functions is ", m_seq[i])) 
    
    out <- readRDS(paste0("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1/tuning_bigm/out/",m_seq[i],"/out_", n, ".RDS"))
    data <-  readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Tuning_data/data_50_tuning_", n, ".RDS"))
    
    m = m_seq[i]
    lambdas = logspace(log10(1),log10(150),100)
    
    N_samp= 10;
    
    p = 10
    res_1000 = get_bic_vals(out, N_samp, n-1, m, lambdas, data, 
                            5, gamma=0.5,  p, threshold=1e-5)
    
    BIC[[i]] = res_1000$BIC
    AIC[[i]] = res_1000$AIC
    eBIC[[i]] = res_1000$eBIC
    
  }
  ymin = log(min(unlist(lapply(BIC, min))))
  ymax = log(max(unlist(lapply(BIC, max))))
  plot(lambdas, log(BIC[[1]]), type='l', main = paste("n = ", n), ylim = c(7.05, 7.22), #ymin-0.01, ymax+0.01), 
       ylab = expression(log(BIC)), xlab = expression(lambda), col="purple")
  lines(lambdas, log(BIC[[2]]), col="coral")
  lines(lambdas, log(BIC[[3]]), col="deepskyblue")
  lines(lambdas, log(BIC[[4]]), col="lightgreen")
  legend("bottomright", legend = c(25, 30, 35, 40), lty = c(1,1,1,1), cex=0.5,
         col=c("purple", "coral", "deepskyblue", "lightgreen"), horiz = T)
  
  print(lambdas[unlist(lapply(BIC, which.min))])
  return(BIC)
}

par(mfrow = c(1,1))
b1 = plot_BIC(n=1000)
b2 = plot_BIC2(n=1000)

lams1 = logspace(log10(10),log10(300),100)
lams2 = logspace(log10(1), log10(150), 100)

lams1_new = lams1[lams1<=150]
lams2_new = lams2[lams2>=10]

par(mfrow = c(1,1))
plot(lams1_new, log(b1[[1]][1:length(lams1_new)]), ylim = c(7.05, 7.22), ylab=expression(log(BIC)), xlab=expression(lambda), type="l")
lines(lams1_new, log(b1[[2]][1:length(lams1_new)]), col= "deeppink")
lines(lams1_new, log(b1[[3]][1:length(lams1_new)]), col= "navy")
lines(lams1_new, log(b1[[4]][1:length(lams1_new)]), col= "orange")
lines(lams1_new, log(b1[[5]][1:length(lams1_new)]), col= "darkgreen")
lines(lams2_new, log(b2[[1]][(length(lams2)-length(lams2_new)+1):length(lams2)]), col="purple")
lines(lams2_new, log(b2[[2]][(length(lams2)-length(lams2_new)+1):length(lams2)]), col="coral")
lines(lams2_new, log(b2[[3]][(length(lams2)-length(lams2_new)+1):length(lams2)]), col="deepskyblue")
lines(lams2_new, log(b2[[4]][(length(lams2)-length(lams2_new)+1):length(lams2)]), col="lightgreen")
title(main = "n = 1000")
legend("bottomright", )
