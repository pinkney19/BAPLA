
library(purrr)
left_params <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/results/left_params.RDS")
c_hats = left_params$c
fs = left_params$fs
beta_0 = left_params$beta_0_alt

c_mat = matrix(data = NA, nrow = 56, ncol=6)
c_mat_std = matrix(data=NA, nrow=56, ncol=6)
for(i in 1:length(c_hats)){
  c_mat[i, ] = c_hats[[i]]
  c_mat_std[i,] = (c_mat[i,] - min(c_mat[i,]) ) /  (max(c_mat[i,]) - min(c_mat[i,]))
}
# par(mfrow=c(1,2))
# image(t(c_mat))
# image(t(c_mat_std))
#standardise entries in c_mat
set.seed(123)
res_clust=kmeans(c_mat_std, centers = 2)
res_clust$cluster


id1 = which(res_clust$cluster==1) # stationary (assume)
id2 = which(res_clust$cluster==2) # non-stationary

length(id1)
length(id2)


# check against non-stationary IDs from PELT
id_non_stationary <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/combined_plots/f_v_no_f/id_non_stationary.RDS")#
id_stationary <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/combined_plots/f_v_no_f/id_stationary.RDS")

100 * ( sum((id1 %in% id_stationary))/ length(id1) ) # 67% agree
100* ( sum((id2 %in% id_non_stationary)) / length(id2) ) # 42% agree

# to check
#check_ids = id_stationary[stat_as_non_stat_ids]

# try with fs -------------------------------------------------------------


f_mat = matrix(data = NA, nrow = 56, ncol=600)
f_mat_std = matrix(data=NA, nrow=56, ncol=600)
fr = matrix(data = NA, nrow = 56, ncol=600)
for(i in 1:dim(f_mat)[1]){
  f_mat[i, ] = fs[[i]] + beta_0[[i]]
  fr[i,] = ( exp(f_mat[i,]) / (1+exp(f_mat[i,])) ) * 1000
  f_mat_std[i,] = (f_mat[i,] - min(f_mat[i,]) ) /  (max(f_mat[i,]) - min(f_mat[i,]))
}


stat_fs = fr[(id1),]
non_stat_fs = fr[(id2),]
par(mfrow=c(2,2))
plot(colMeans(stat_fs), ylim = c(0,100), main="clustering stationary")
plot(colMeans(non_stat_fs), ylim=c(0,100), main="clustering non-stationary")


# PELT
stat_fs2 = fr[(id_stationary),]
plot(colMeans(stat_fs2), ylim=c(0,100), main="PELT stationary")
non_stat_fs2 = fr[(id_non_stationary),]
plot(colMeans(non_stat_fs2), ylim=c(0,100), main="PELT non-stationary")

plot(f_mat_std[35,])
plot(f_mat_std[45,])

plot(f_mat_std[52,])


par(mfrow = c(1,2))
plot(seq(1,600), fr[id2[1],], type="n", ylim = c(0,200), main="k-means non-stationary")
for(i in 1:length(id2)){
  lines(seq(1,600), fr[id2[i],], type="l", col=i)
}

#stationary
plot(seq(1,600), fr[id1[1],], type="n", ylim = c(0,200), main="k-means `stationary`")
for(i in 1:length(id1)){
  lines(seq(1,600), fr[id1[i],], type="l", col=i)
}


# from pelt
par(mfrow = c(1,2))
plot(seq(1,600), fr[id_non_stationary[1],], type="n", ylim = c(0,200), main="PELT non-stationary")
for(i in 1:length(id_non_stationary)){
  lines(seq(1,600), fr[id_non_stationary[i],], type="l", col=i)
}

#stationary
plot(seq(1,600), fr[id_stationary[1],], type="n", ylim = c(0,200), main="PELT `stationary`")
for(i in 1:length(id_stationary)){
  lines(seq(1,600), fr[id_stationary[i],], type="l", col=i)
}



# identify the "non-stationary ones (as identified by PELT)
idx_stat = id_stationary[id_stationary %in% id1==T]
common_ids = match(idx_stat, id1)
plot(seq(1,600), f_mat[common_ids[1], ], type="n", ylim=c(-9,2))
for(i in 1:length(common_ids)){
  lines(seq(1,600), f_mat[common_ids[i],], col="blue")
}



image(f_mat)
image(f_mat_std)
par(mfrow = c(1,1))
plot(f_mat_std[1,], type="n", ylim=c(0,1))
for(i in 1:dim(f_mat_std)[1]){
  lines(seq(0,600, length.out = length(f_mat_std[i,])),f_mat_std[i,], col=i)
}

res_clust_f=kmeans(f_mat, centers = 2)
res_clust_f$cluster

id_f1 = which(res_clust_f$cluster==1) # non-stationary (assume)
id_f2 = which(res_clust_f$cluster==2) # stationary

length(id_f1)
length(id_f2)

100 * ( sum((id_f1 %in% id_stationary))/ length(id_f1) ) # 66% agree
100* ( sum((id_f2 %in% id_non_stationary)) / length(id_f2) ) # 70% agree



plot_fs = function(check_ids){
  
  i = 1
  #par(mfrow = c(1,1))
  plot(seq(0,599), 
       (exp(f_mat[check_ids[i],])/(1+exp(f_mat[check_ids[i],])) *1000), type="n",
       ylim= c(0,300), xlab = "Time", ylab ="Firing rate")
  for(i in 1:length(check_ids)){
    lines(seq(0,599), 
          (exp(f_mat[check_ids[i],])/(1+exp(f_mat[check_ids[i],])) *1000) )
  }
}

par(mfrow =c(1,2))
plot_fs(id1)
plot_fs(id2)

plot_fs(id_stationary)
plot_fs(id_non_stationary)

# Time series clustering
c