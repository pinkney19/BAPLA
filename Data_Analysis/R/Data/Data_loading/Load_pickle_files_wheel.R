
# Data Analysis ------------------------------------------------------


# Install and load reticulate package
#install.packages("reticulate")
library(reticulate)

# Import pickle module from Python
pickle <- import("pickle")
setwd("~/Downloads/BAPLA/Data_Analysis/Python/wheel_data")

# Load the .pickle files
source_python("pickle_reader.py")
spike_times_all_regions <- read_pickle_file("spike_times_all_neurons_wheel.pickle")
left_times_all_regions <- read_pickle_file("left_times_all_neurons_wheel.pickle")
right_times_all_regions <- read_pickle_file("right_times_all_neurons_wheel.pickle")

regions = unique(names(spike_times_all_regions))

setwd("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing_wheel")
saveRDS(spike_times_all_regions, "all_spikes.RDS")
saveRDS(left_times_all_regions, "left_trial_spikes.RDS")
saveRDS(right_times_all_regions, "right_trial_spikes.RDS")
