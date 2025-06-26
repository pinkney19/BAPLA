setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")

# chosen lambdas ----------------------------------------------------------

region_id <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/region_ids.RDS")
left_res <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/no_f/results/left_res.RDS")
right_res <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/no_f/results/right_res.RDS")

left_df = data.frame(lams = left_res$lam_star, region=region_id, label = seq(1,length(region_id)))
right_df = data.frame(lams = right_res$lam_star, region=region_id, label = seq(1,length(region_id)))


library(ggplot2)
left_lams = ggplot(data = left_df, aes(x = label, y = lams, color = region))+
  geom_point() +
  labs(x = "Neuron ID", y = expression(lambda)) +
  theme_minimal()+
  ggtitle("Left Trials") +
  ylim(10,300)+
  scale_color_manual(values = c("#F8766D", "darkorange",  "olivedrab", 
                                "#00BA38",  "#00C1A7",  "deepskyblue1", 
                                 "dodgerblue3",  "mediumorchid1", "brown2"))

right_lams = ggplot(data = right_df, aes(x = label, y = lams, color = region))+
  geom_point() +
  labs(x = "Neuron ID", y = expression(lambda)) +
  theme_minimal()+
  ggtitle("Right Trials") + 
  ylim(10,300)+
  scale_color_manual(values = c("#F8766D", "darkorange",  "olivedrab", 
                                         "#00BA38",  "#00C1A7",  "deepskyblue1", 
                                         "dodgerblue3",  "mediumorchid1", "brown2"))

library(ggpubr)
plt = ggarrange(left_lams, right_lams, ncol=2, common.legend = T, legend = "right")
print(plt)


# plot BICs ---------------------------------------------------------------
reg_id = c(1,2,4,9, 27, 35, 48, 52, 53, 55, 56)
#CA = 1
ids = list();
for(i in 1:length(reg_id)){
  ids[[i]] = seq(reg_id[i], reg_id[(i+1)]-1)
}

ids[[11]] = 56
left_BIC = left_res$BIC
right_BIC = right_res$BIC

plot_bic = function(region_no, BIC_data, title){
  
  idx = ids[[region_no]]
  
  data = BIC_data[idx]
  
  lambdas = logspace(log10(10),log10(300),100)
  l_max = max(unlist(lapply(data,max)))
  l_min = min(unlist(lapply(data,min)))
  plot(lambdas, data[[1]], type='l', ylim = c(l_min, l_max), ylab = "BIC", xlab = expression(lambda), main = title)
  lam_id = which.min(data[[1]])
  points(lambdas[lam_id], data[[1]][lam_id], col="red")
  if(length(data)>1){
    for(i in 2:length(data)){
      lines(lambdas, data[[i]])
      lam_id = which.min(data[[i]])
      points(lambdas[lam_id], data[[i]][lam_id], col="red")
    }
  }
}

regs = unique(region_id)

par(mar = c(5.1, 4.1,4.1,2.1), mfrow = c(4,3))
for(i in 1:length(regs)){
  plot_bic(i, left_BIC, regs[i])
}
par(mar = c(5.1, 4.1,4.1,2.1), mfrow = c(4,3))
for(i in 1:length(regs)){
  plot_bic(i, right_BIC, regs[i])
}


# load results -------------------------------------------------------------

est_left <- readRDS("~/luna/BAPLA/Data_Analysis/wheel/est_left_no_f.RDS")
est_right <- readRDS("~/luna/BAPLA/Data_Analysis/wheel/est_right_no_f.RDS")

# Results -----------------------------------------------------------------
get_estimates = function(lambdas, est_res, lam_id){
  #' Function to obtain parameter estimates for chosen value of lambda
  #' @param lambdas (numeric) vector of lambda values
  #' @param est_res (list) STORM output for estimates of all lambda values and all dimensions  

  out_star = list();
  p = 56
  for(j in 1:p){
    print(j)
    b = length(lambdas)*j
    if(j==1){
      # select results from storm
      out  = est_res[1:b]
    }
    if(j!=1){
      # select results from storm
      out = est_res[((b-length(lambdas))+1):b]
    }
    out_star[[j]] = out[[lam_id[j]]]
  }
  
  betas = map(out_star,1)
  beta_hat = matrix(NA, p, p)
  for(i in 1:length(betas)){
    beta_hat[i,] = betas[[i]]
  }
  
  fs = map(out_star, 3)
  iter = map(out_star, 6)
  beta_0_alt = map(out_star, 5)
  beta_0 = map(out_star, 2)
  
  return(list(fs = fs, beta_hat = beta_hat, iter = iter, beta_0 = beta_0, beta_0_alt = beta_0_alt))
  
}
library(pracma)
library(purrr)
lambdas = logspace(log10(10),log10(300),100)

left_res$lam_id
out_left = get_estimates(lambdas, est_left, left_res$lam_id)
out_right = get_estimates(lambdas, est_right, right_res$lam_id)


# save results
setwd("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/no_f/results")
saveRDS(out_left, "left_params.RDS")
saveRDS(out_right, "right_params.RDS")


par(mfrow = c(1,2), mar = c(5,4,4,5))
image(out_left$beta_hat, main = "left trials")
image(out_right$beta_hat, main = "right trials")

mapping <- c(CA = 1, DG = 2, GPe = 3, MB = 4,
             MRN = 5, POL = 6,  SCm = 7,
             SCsg = 8,VISl = 9, VISp = 10, VISrl = 11 )
cols = c("#F8766D", "darkorange",  "olivedrab", "#00BA38",  "#00C1A7",  
                  "deepskyblue1", "dodgerblue3",  "mediumorchid1", "lightpink1", "deeppink", "brown2")
map_cols = c(CA = cols[1], DG =cols[2], GPe = cols[3], MB = cols[4], MRN = cols[5], POL = cols[6], SCm = cols[7], 
             SCsg = cols[8] , VISl = cols[9], VISp = cols[10], VISrl = cols[11])

# Use the match function to replace the strings with their corresponding numeric values
num_region <- mapping[region_id]
col_region = map_cols[region_id]

col_region[1] = "#F8766D"
library(igraph)
par(mar = c(1,1,1,1))
plot_graph(out_left$beta_hat, 0.3, 10, col_region)
plot_graph(out_right$beta_hat, 0.5, 10, col_region)

# convert edges to +/- 1

convert_mat = function(mat){
  mat[mat>0] = 1
  mat[mat<0] = -1
  return(mat)
}


beta_right = convert_mat(out_right$beta_hat)
beta_left = convert_mat(out_left$beta_hat)

image(beta_left, main = "left")
image(beta_right, main = "right")

regs[1] = "CA1"
par(mar = c(0.2,0.02,.2,.02), mfrow=c(1,2))
plot_graph(beta_left, 0.3, 10, col_region)
#legend("topleft", legend = regs, col = cols,   pch = 19, bty = "n")
plot_graph(beta_right,0.3, 10, col_region)
legend("right", legend = regs, col = cols,   
       pch = 19, bty = "n", cex = 1.5)



# Beta_0 ------------------------------------------------------------------

intercept_left = out_left$beta_0
intercept_right = out_right$beta_0
#beta0_left = sapply(intercept_left, function(x) 1/(1+exp(-x))*1000 )[reg_id]
#beta0_right = sapply(intercept_right, function(x) 1/(1+exp(-x))*1000 )[reg_id]


