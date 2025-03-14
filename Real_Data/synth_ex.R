CG_data <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Tuning_data/data_50_tuning_1000.RDS")
n_samp = length(CG_data)
unit1 = list();
for(i in 1:n_samp){
  unit1[[i]] = CG_data[[i]][1,] 
}
par(mfrow = c(1,1))
spikes = list();
spikes[[1]] = which(unit1[[1]]==1)
plot(spikes[[1]], rep(1, length(spikes[[1]])), ylim =c (0,11), 
     ylab = "Trial", xlab = "Time (ms)", col= i)
for(i in 2:length(unit1)){
  spikes[[i]] = which(unit1[[i]]==1) #get locations of spikes
  points(spikes[[i]], rep(i, length(spikes[[i]])), col=i)
}

setwd("~/Downloads/BAPLA/Real_Data")
source("Functions_data.R")

spikes2 = lapply(spikes, function(x) x- 400)
out1 = plot_psth(spikes2, 1, 400 , 600, "")
out2 = plot_psth(spikes2, 10, 400, 600, "")
out3 = plot_psth(spikes2, 20, 400, 600, "")



# Question: can we recover this f shape based on our estimation procedure in the real data setting?
lams_50_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Tuning/10/lams_50_1000.RDS")
lambda = lams_50_1000[1]
library(splines)
res1 = bapla_single_unit_trial(CG_data, 1, 1000, 1e-5, lams_50_1000, 15)
par(mfrow = c(1,4))
plot(seq(0,1,length.out = length(res1$f)), res1$f, type='l', ylab = expression(hat(f)[i](t)), xlab = expression(t), main = "Estimated f", ylim=c(-2,5))
#plot(out1$hist, type='l', ylab = "PSTH1", xlab = "Time (ms)")
plot(seq(0, 1, length.out = length(out1$hist)), out1$hist, type='l', ylab = "FR", xlab = expression(t), main = "PSTH - 1")
plot(seq(0, 1, length.out = length(out2$hist)), out2$hist, type='l', ylab = "FR", xlab = expression(t), main = "PSTH - 10")
plot(seq(0, 1, length.out = length(out3$hist)), (out3$hist), type='l', ylab = "FR", xlab = expression(t), 
     main = "PSTH -20", ylim = c(-2,5))
lines(seq(0,1,length.out = length(res1$f)), res1$f, type='l', ylab = expression(hat(f)[i](t)), col="blue", lty = 2, lwd = "dashed")
#lines(seq(1,length(res1$f)), res1$f/10, type='l', ylab = expression(hat(f)[i](t)), xlab = "Time (ms)", col="red", lwd = 2, lty="dashed")

# do for all units
beta_hat = matrix(NA, 10,10); f = list(); beta0 = NULL;
for(j in 1:10){
  print(j)
  out = bapla_single_unit_trial(CG_data, j, 1000, 1e-5, lams_50_1000, 20)
  beta_hat[j,] = out$beta
  f[[j]] = out$f
  beta0[j] = out$beta0
}

par(mfrow = c(1,1))
plot(seq(0,1,length.out = length(f[[1]])), f[[1]], type='l', ylab = expression(hat(f)[i](t)), xlab = "Time (ms)", main = "Estimated f", ylim = c(-2,10))
for(i in 2:length(f)){
  lines(seq(0,1,length.out =length(f[[i]])), f[[i]], type='l', ylab = expression(hat(f)[i](t)), xlab = "Time (ms)", main = "Estimated f", col=i)
}
N = 1000
xs <- dnorm(seq(0, 1, length.out=N-1), mean = 0.5, sd =  0.05)-1
lines(seq(0,1,length.out = length(xs)),xs, col="blue", lwd = 2 ,lty = "dashed")
legend("topright", "truth", col="blue", lty =2, lwd = 2)
