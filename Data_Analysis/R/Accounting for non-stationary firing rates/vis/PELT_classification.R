setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")

library(changepoint)

units_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/raw/left.RDS")
units_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/raw/right.RDS")

# Define the time window (e.g., -0.2s to 0.4s around the stimulus)
time_window <- c(-0.2, 0.4)  # seconds

# Define bin size (e.g. 1ms)
bin_size = 0.001

# Create bins based on the defined time window and bin size
time_bins <- seq(time_window[1], time_window[2], by = bin_size)



out_left = get_firing_rate_matrix(units_left, time_bins)
out_right = get_firing_rate_matrix(units_right, time_bins)

# plot examples

# non-stationary example
plot_example_with_cpts(47, time_bins, out_left, out_right)
# stationary example
plot_example_with_cpts(17, time_bins, out_left, out_right)


left_stat_ids = out_left$stat_ids
left_non_stat_ids = out_left$non_stat_ids

right_stat_ids = out_right$stat_ids
right_non_stat_ids = out_right$non_stat_ids



# Save IDs of the neurons -------------------------------------------------

# left trials
setwd("~/Downloads/BAPLA/Data_Analysis/R/Accounting for non-stationary firing rates/vis/left")
saveRDS(left_stat_ids, "id_stationary.RDS")
saveRDS(left_non_stat_ids, "id_non_stationary.RDS")

# right trials 
setwd("~/Downloads/BAPLA/Data_Analysis/R/Accounting for non-stationary firing rates/vis/right")
saveRDS(right_stat_ids, "id_stationary.RDS")
saveRDS(right_non_stat_ids, "id_non_stationary.RDS")


