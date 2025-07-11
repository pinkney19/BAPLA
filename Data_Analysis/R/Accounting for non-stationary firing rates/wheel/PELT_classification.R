setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")

library(changepoint)

units_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/wheel/raw/left.RDS")
units_right <-readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/wheel/raw/right.RDS")

# Define the time window (e.g., -0.2s to 0.4s around the stimulus)
time_window <- c(-0.2, 0.4)  # seconds

# Define bin size (e.g. 1ms)
bin_size = 0.001

# Create bins based on the defined time window and bin size
time_bins <- seq(time_window[1], time_window[2], by = bin_size)

get_firing_rate_matrix = function(units, time_bins){
  # calculate firing rate
  firing_rate = matrix(data = NA, nrow = length(units), ncol = length(time_bins)-1)
  smooth_fr = matrix(data = NA, nrow = length(units), ncol = length(time_bins)-1)
  # place holder for change points
  cpts = list();
  for(i in 1:length(units)){
    firing_rate[i,] = firing_rate_func(units[[i]], time_bins, bin_size)
    smooth_fr[i,] = rollmean(firing_rate[i,], k = 5, fill = NA)
    cpts[[i]] = cpts(cpt.var(firing_rate[i,], method = "PELT"))
  }
  
  # get IDs of statioary and non-stationary neurons
  non_stat_ids = which(lengths(cpts)!=0) # i.e. which neurons have `changepoints`
  stat_ids = which(lengths(cpts)==0)
  
  return(list(firing_rate = firing_rate, smoothed_fr = smooth_fr, cpts = cpts,
              stat_ids = stat_ids, non_stat_ids = non_stat_ids))
}

out_left = get_firing_rate_matrix(units_left, time_bins)
out_right = get_firing_rate_matrix(units_right, time_bins)

# plot example
plot_example_with_cpts = function(unit_id, time_bins, out_left, out_right){
  par(mfrow = c(1,2))
  xs = time_bins[-length(time_bins)]
  plot(xs, out_left$firing_rate[unit_id,], type="l", ylim = c(0,300), 
       main = paste("Unit", unit_id, " - Left Trials"), ylab = "Firing rate (Hz)", xlab = "Time (ms)")
  abline(v = c(time_bins[out_left$cpts[[unit_id]]]), col="red", lty = "dashed", lwd = 2)
  plot(xs, out_right$firing_rate[unit_id,], type="l", ylim = c(0,300), 
       main = paste("Unit", unit_id, " - Right Trials"), 
       ylab = "Firing rate (Hz)", xlab = "Time (ms)")
  abline(v = c(time_bins[out_right$cpts[[unit_id]]]), col="red", lty = "dashed", lwd = 2)
}

# non-stationary example
plot_example_with_cpts(35, time_bins, out_left, out_right)
# stationary example
plot_example_with_cpts(17, time_bins, out_left, out_right)


left_stat_ids = out_left$stat_ids
left_non_stat_ids = out_left$non_stat_ids

right_stat_ids = out_right$stat_ids
right_non_stat_ids = out_right$non_stat_ids



# Save IDs of the neurons -------------------------------------------------

# left trials
setwd("~/Downloads/BAPLA/Data_Analysis/R/Accounting for non-stationary firing rates/wheel/left")
saveRDS(left_stat_ids, "id_stationary.RDS")
saveRDS(left_non_stat_ids, "id_non_stationary.RDS")

# right trials 
setwd("~/Downloads/BAPLA/Data_Analysis/R/Accounting for non-stationary firing rates/wheel/right")
saveRDS(right_stat_ids, "id_stationary.RDS")
saveRDS(right_non_stat_ids, "id_non_stationary.RDS")


