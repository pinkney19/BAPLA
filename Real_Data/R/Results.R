est1 <- readRDS("~/Downloads/BAPLA/Real_Data/R/est1.RDS")


betas = map(est1,1)
beta_hat = matrix(NA, nrow = length(betas), ncol=length(betas))
for(i in 1:length(betas)){
  beta_hat[i,] = betas[[i]]
}
image(beta_hat)

beta1 = beta_hat
beta1[beta1>0]=1
beta1[beta1<0] = -1

image(beta1)

beta0s = map(est1,2)
beta0s = unlist(beta0s)
fs = map(est1, 3)

f_hat = list();
for(i in 1:length(fs)){
  f_hat[[i]] = fs[[i]] + beta0s[i]
}

par(mfrow = c(1,3))
plot(seq(-0.2,0.4,length.out = length(fs[[1]])), f_hat[[1]], type='l', ylab = "Firing Rate", xlab = "time (s)", main = "CA1")
plot(seq(-0.2,0.4,length.out = length(fs[[2]])), f_hat[[2]], type='l', ylab = "Firing Rate", xlab = "time (s)", main = "DG")
plot(seq(-0.2,0.4,length.out = length(fs[[38]])),  f_hat[[38]], type='l', ylab = "Firing Rate", xlab = "time (s)", main = "POL")



par(mfrow = c(1,3))
plot(seq(-0.2,0.4,length.out = length(fs[[1]])), (abs(min(f_hat[[1]])) +f_hat[[1]])/(abs(beta0s[1])), type='l', ylab = "Firing Rate", xlab = "time (s)", main = "CA1")
plot(seq(-0.2,0.4,length.out = length(fs[[2]])), (abs(min(f_hat[[2]])) + f_hat[[2]])/abs(beta0s[2]), type='l', ylab = "Firing Rate", xlab = "time (s)", main = "DG")
plot(seq(-0.2,0.4,length.out = length(fs[[38]])), (abs(min(f_hat[[38]])) + f_hat[[38]])/abs(beta0s[38]), type='l', ylab = "Firing Rate", xlab = "time (s)", main = "POL")



beta0s = map(est1,2)
unlist(beta0s)
