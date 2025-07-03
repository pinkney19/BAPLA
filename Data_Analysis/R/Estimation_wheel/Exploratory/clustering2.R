setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")

library(zoo)
library(dtwclust)

units_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/wheel/raw/left.RDS")

# Define the time window (e.g., -0.2s to 0.4s around the stimulus)
time_window <- c(-0.2, 0.4)  # seconds

# Define bin size for PSTH (e.g., 0.01 seconds)
bin_size = 0.001

# Create bins based on the defined time window and bin size
time_bins <- seq(time_window[1], time_window[2], by = bin_size)

firing_rate = matrix(data = NA, nrow = length(units_left), ncol = length(time_bins)-1)
smooth_fr = matrix(data = NA, nrow = length(units_left), ncol = length(time_bins)-1)
for(i in 1:length(units_left)){
  firing_rate[i,] = firing_rate_func(units_left[[i]], time_bins, bin_size)
  smooth_fr[i,] = rollmean(firing_rate[i,], k = 5, fill = NA)
}

xs = time_bins[-length(time_bins)]
plot(xs, firing_rate[i,], type="n", ylim = c(0,300), main = "Firing Rates")


par(mfrow = c(4,3))
for(i in 1:dim(firing_rate)[1]){
  plot(xs, smooth_fr[i,], type='l', ylim = c(0,300))
}

# first and last two columns are full of NAs due to smoothing
# remove these
mat = smooth_fr
smooth_fr2 = mat[, -(c(1, 2, ncol(mat)-1, ncol(mat)))]

# use PELT to identify changepoints in firing rate data - 
# i.e. non-stationary vs stationary
cpts2 =list();
for(i in 1:dim(smooth_fr2)[1]){
  cpts2[[i]] = cpts(cpt.var(firing_rate[i,], method = "PELT"))
}

non_stat_ids = which(lengths(cpts2)!=0)
stat_ids = which(lengths(cpts2)==0)

par(mfrow = c(4,3))
for(i in 1:dim(firing_rate)[1]){
  if(i %in% non_stat_ids){
    plot(xs, firing_rate[i,], type='l')
  }
}

for(i in 1:dim(firing_rate)[1]){
  if(i %in% stat_ids){
    plot(xs, firing_rate[i,], type='l', col="blue")
  }
}


# save IDs of the neurons
setwd("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/combined_plots/f_v_no_f")
saveRDS(stat_ids, "id_stationary.RDS")
saveRDS(non_stat_ids, "id_non_stationary.RDS")
