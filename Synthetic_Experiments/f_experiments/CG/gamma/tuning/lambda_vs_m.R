
# checks for lambda vs m --------------------------------------------------

m_seq = c(3,5,10,15,20,25,30, 35, 40);
n_seq = c(1000,5000,10000);

cols = c("deeppink", "lightgreen", "deepskyblue", "orange", "coral", "darkgreen", "blue", "violet","brown1", "darkorange2")


plot_lambdas = function(n, m_seq, p=10){
  lams=NULL; lams=rep(list(lams), p);
  for(j in 1:p){
    for(i in 1:length(m_seq)){
    lams[[j]][i] <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/tuning/lambdas/lams_",
                                   m_seq[i],"_", n, ".RDS"))[j]
    }
  }
  upper = max(unlist(lams)); lower = min(unlist(lams));
  plot(m_seq, lams[[1]], xlab=expression(m), ylab=expression(lambda), ylim=c(lower-1, upper+1), col=cols[1], type='l')
  for(k in 2:p){
    lines(m_seq, lams[[k]], col=cols[k])
  }
}

plot_lambdas(1000, m_seq)
