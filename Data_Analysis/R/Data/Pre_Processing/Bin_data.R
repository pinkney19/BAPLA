# Load Functions ----------------------------------------------------------

setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")


# Load data ---------------------------------------------------------------

left_trial_spikes <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/left_trial_spikes.RDS")
right_trial_spikes <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/right_trial_spikes.RDS")


regions <- names(left_trial_spikes)


# Bin Data ----------------------------------------------------------------

xs = seq(-0.2, 0.4, by = 0.001) #delta = 1ms


left_binned = bin_all(left_data)
right_binned = bin_all(right_data)

setwd("~/Downloads/BAPLA/Data_Analysis/R/Data")
saveRDS(left_binned, "left_binned.RDS")
saveRDS(right_binned, "right_binned.RDS")
