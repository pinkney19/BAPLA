setwd("~/Downloads/BAPLA/Real_Data")
source("Functions_data.R")
# Load data ---------------------------------------------------------------

left_trial_spikes <- readRDS("~/Downloads/BAPLA/Real_Data/Data/left_trial_spikes.RDS")
right_trial_spikes <- readRDS("~/Downloads/BAPLA/Real_Data/Data/right_trial_spikes.RDS")


# Remove regions with no spikes
left_data =  Filter(function(x) length(x) > 0, left_trial_spikes)
right_data =  Filter(function(x) length(x) > 0, right_trial_spikes)
regions <- names(left_trial_spikes)

xs = seq(-0.2, 0.4, by = 0.001) #delta = 1ms


left_binned = bin_all(left_data)
right_binned = bin_all(right_data)

setwd("~/Downloads/BAPLA/Real_Data/Data")
saveRDS(left_binned, "left_binned.RDS")
saveRDS(right_binned, "right_binned.RDS")
