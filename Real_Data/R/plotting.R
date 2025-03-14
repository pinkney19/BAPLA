setwd("~/Downloads/BAPLA/Real_Data")
source("Functions_data.R")
# Read in files -----------------------------------------------------------
est_left1 <- readRDS("~/luna/BAPLA/Real_Data/less_basis/est_left1.RDS")
est_left2 <- readRDS("~/luna/BAPLA/Real_Data/less_basis/est_left2.RDS")
est_right1 <- readRDS("~/luna/BAPLA/Real_Data/less_basis/est_right1.RDS")
est_right2 <- readRDS("~/luna/BAPLA/Real_Data/less_basis/est_right2.RDS")


# combine files -----------------------------------------------------------

est_left <- c(est_left1, est_left2)
est_right = c(est_right1, est_right2) 


# Read in results files ---------------------------------------------------
left_res <- readRDS("~/Downloads/BAPLA/Real_Data/Data/estimation/basis4/results/left_res.RDS")
right_res <- readRDS("~/Downloads/BAPLA/Real_Data/Data/estimation/basis4/results/right_res.RDS")

# left_res <- readRDS("~/Downloads/BAPLA/Real_Data/Data/estimation/results/left_res.RDS")
# right_res <- readRDS("~/Downloads/BAPLA/Real_Data/Data/estimation/results/right_res.RDS")


# regularisation plots ----------------------------------------------------

region_id <- readRDS("~/Downloads/BAPLA/Real_Data/Data/region_id.RDS")

left_df = data.frame(lams = left_res$lam_star, region=region_id, label = seq(1,78))
right_df = data.frame(lams = right_res$lam_star, region=region_id, label = seq(1,78))


library(ggplot2)
left_lams = ggplot(data = left_df, aes(x = label, y = lams, color = region))+
  geom_point() +
  labs(x = "Neuron ID", y = expression(lambda)) +
  theme_minimal()+
  ggtitle("Left Trials")

right_lams = ggplot(data = right_df, aes(x = label, y = lams, color = region))+
  geom_point() +
  labs(x = "Neuron ID", y = expression(lambda)) +
  theme_minimal()+
  ggtitle("Right Trials") 

library(ggpubr)
plt = ggarrange(left_lams, right_lams, ncol=2, common.legend = T, legend = "right")
print(plt)



# networks ----------------------------------------------------------------

plot_network = function(res, out_storm, p, region_col){
  lam_id = res$lam_id
  out_star = list(); 
  for(j in 1:p){
    print(j)
    b = length(lambdas)*j
    if(j==1){
      # select results from storm
      out  = out_storm[1:b]
    }
    if(j!=1){
      # select results from storm
      out = out_storm[((b-length(lambdas))+1):b]
    }
    out_star[[j]] = out[[lam_id[j]]]
  }
  
  betas = map(out_star,1)
  beta_hat = matrix(NA, 78, 78)
  for(i in 1:length(betas)){
    beta_hat[i,] = betas[[i]]
  }
  
  fs = map(out_star, 3)
  beta_0 = map(out_star, 5)
  plot_graph(beta_hat, 0.0002, 8,region_col)
  return(list(beta= beta_hat, f=fs, beta0 = beta_0))
}

# Create a named vector that maps each string to a numeric value
mapping <- c(CA1 = 1, DG = 2, GPe = 3, MB = 4,
             MRN = 5, POL = 6, POST=7, SCm = 8,
             SCsg = 9, VISl = 10,VISrl = 11 )

# Use the match function to replace the strings with their corresponding numeric values
num_region <- mapping[region_id]

lambdas = logspace(log10(0.5),log10(400),100)
library(igraph)
par(mfrow =c (1,2))
br = plot_network(right_res, est_right, 78, num_region)
bl = plot_network(left_res, est_left, 78, num_region)




# fplots ------------------------------------------------------------------

# Example with those selected in paper

# Find the indices of the first occurrence of each unique entry in region_id vector
ex_ids = sapply(unique(region_id), function(x) which(region_id == x)[1])
ex_ids

left_df$lams[ex_ids]


#ex_ids[5] = 39 #avoid larger lambda case
time = seq(-0.2, 0.4, length.out= length(bl$f[[1]]))
par(mfrow = c(3,4))
for(i in 1:length(ex_ids)){
  idx = ex_ids[i]
  plot(time, (bl$f[[idx]]), type='l', ylab = "Firing Rate", xlab = "Time (secs)", main = names(ex_ids)[i])
}
