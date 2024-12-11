
# Libraries  --------------------------------------------------------------

library(igraph)
setwd("~/Downloads/BAPLA/Simulation_Study")
source("Functions_BAPLA.R")
# p = 10 ------------------------------------------------------------------


# Erdos Renyi Graph  

set.seed(123)
#beta_ER = erdos.renyi.game(10, 0.35, directed = T)
beta_ER = erdos.renyi.game(10, 0.34, directed = T)
par(mfrow=c(1,2),mar = c(0.1,0.1,0.1,0.1))
plot(beta_ER)
beta_mat_ER = as.matrix(beta_ER)
sum(beta_mat_ER)



# Stochastic Block Model 
set.seed(123)
pm <- cbind( c(.75, .05), c(.05, .75) )
beta_SB <- sample_sbm(10, pref.matrix=pm, block.sizes=c(5,5), directed = T)
plot(beta_SB)
beta_SB_mat = as.matrix(beta_SB)
sum(beta_SB_mat)

# p = 50 ------------------------------------------------------------------

set.seed(123)
beta_ER_50 = erdos.renyi.game(50, 0.09, directed = T)
par(mar = c(0.1,0.1,0.1,0.1))
plot(beta_ER_50)
beta_mat_ER_50 = as.matrix(beta_ER_50)
sum(beta_mat_ER_50)

set.seed(123)
pm50 = diag(0.75, nrow = 10, ncol=10)
pm50[pm50==0]=0.02
beta_SB_50 <- sample_sbm(50, pref.matrix=pm50, block.sizes=c(5,5,5,5,5,5,5,5,5,5), directed = T)
plot(beta_SB_50)
beta_SB_50_mat = as.matrix(beta_SB_50)
sum(beta_SB_50_mat)

# image plots -------------------------------------------------------------
par(mfrow = c(1,2),mar=c(5,4,4,2))

image(as.matrix(as.matrix(beta_SB)))
image(as.matrix(as.matrix(beta_SB_50)))
mtext("Stochastic Block Model", side = 3, line = -2, outer = T)

image(as.matrix(beta_mat_ER))
image(as.matrix(beta_mat_ER_50))
mtext("Ergos Renyi Model", side = 3, line = -2, outer = T)


# Add in weights ----------------------------------------------------------

set.seed(123)
ER_10 = add_weights(beta_mat_ER, 0.3)
set.seed(123)
ER_50 = add_weights(beta_mat_ER_50, 0.3)
set.seed(123)
SB_10 = add_weights(as.matrix(as.matrix(beta_SB)), 0.3)
set.seed(123)
SB_50 = add_weights(as.matrix(as.matrix(beta_SB_50)), 0.3)

par(mfrow=c(1,2), mar = c(5,4,4,2))
image(as.matrix(as.matrix(SB_10)))
image(as.matrix(as.matrix(SB_50)))
mtext("Stochastic Block Model", side = 3, line = -2, outer = T)

image(as.matrix(ER_10))
image(as.matrix(ER_50))
mtext("Ergos Renyi Model", side = 3, line = -2, outer = T)


# Add in chain graph  -----------------------------------------------------

CG_10 = chain_graph(10, 0.3) 
CG_50 = chain_graph(50, 0.3) 

# plot graphs -------------------------------------------------------------
library(igraph)

par(mfrow=c(1,3),mar = c(0.1,0.1,0.1,0.1))
plot_graph(CG_10, 0.5, 10)
plot_graph(SB_10, 0.5, 10)
plot_graph(ER_10, 0.5, 10)

par(mfrow=c(1,3),mar = c(0.1,0.1,0.1,0.1))
plot_graph(CG_50, 0.5, 10)
plot_graph(SB_50, 0.5, 10)
plot_graph(ER_50, 0.5, 10)


# Image plots -------------------------------------------------------------
par(mfrow=c(1,3), mar = c(5,4,4,2))
image(CG_10, main = "Chain Graph")
image(SB_10, main = "Stochastic Block")
image(as.matrix(ER_10), main = "Erdos Renyi")

image(CG_50, main = "Chain Graph")
image(SB_50, main = "Stochastic Block")
image(as.matrix(ER_50), main= "Erdos Renyi")


# Save interaction matrices -----------------------------------------------

setwd("~/Downloads/BAPLA/Simulation_Study/CG")
saveRDS(CG_10, "beta_10.RDS")
saveRDS(CG_50, "beta_50.RDS")

setwd("~/Downloads/BAPLA/Simulation_Study/ER")
saveRDS(as.matrix(ER_10), "beta_10.RDS")
saveRDS(as.matrix(ER_50), "beta_50.RDS")

setwd("~/Downloads/BAPLA/Simulation_Study/SB")
saveRDS(as.matrix(SB_10), "beta_10.RDS")
saveRDS(as.matrix(SB_50), "beta_50.RDS")
