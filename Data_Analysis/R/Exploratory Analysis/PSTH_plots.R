setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")


# Load IDs of neurons used in analysis ------------------------------------

left_id <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/neuron_ids/60_trials/left_id_filtered_spikes.RDS")
right_id <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/neuron_ids/60_trials/right_id_filtered_spikes.RDS")


# Load spiking data -------------------------------------------------------


left_trial_spikes <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/left_trial_spikes.RDS")
left_data  = unlist(left_trial_spikes, recursive = F)
right_trial_spikes <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/right_trial_spikes.RDS")
right_data = unlist(right_trial_spikes, recursive = F)

# Select Units used in Analysis -------------------------------------------

units_left = left_data[left_id]
units_right = right_data[right_id]


# Check match of IDs ------------------------------------------------------

names(units_left) == names(left_id)
names(units_right) == names(right_id)

library(tm)
regions = removeNumbers(names(units_left))
reg_id = c(1,2,5,7, 27, 30, 43, 46, 48)


# Plot PSTH with ALL trials -----------------------------------------------

setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")

library(ggplot2)

plts = list();
for(i in 1:length(reg_id)){
  plts[[i]] = plot_right_left_psth(units_right[[reg_id[i]]], units_left[[reg_id[i]]], 0.01, 0.2, 0.4, regions[reg_id[i]])
}

#plts = map(plot_list, 1)
ggarrange(plts[[1]],plts[[2]], plts[[3]],
          plts[[4]], plts[[5]], plts[[6]], 
          plts[[7]], plts[[8]], plts[[9]], ncol = 3, nrow = 3, common.legend = T)



# Plot PSTH with SELECTED trials ------------------------------------------

# get rid of trials with no spikes


units_right2 = remove_zeros(units_right)
units_left2 = remove_zeros(units_left)

trials_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/neuron_ids/60_trials/trial_ids/trials_left.RDS")
trials_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/neuron_ids/60_trials/trial_ids/trials_right.RDS")



units_right3 = remove_low_trials(units_right2, trials_right)
units_left3 = remove_low_trials(units_left2, trials_left)


# plot psths --------------------------------------------------------------


library(ggplot2)

plts2 = list();
for(i in 1:length(reg_id)){
  plts2[[i]] = plot_right_left_psth(units_right3[[reg_id[i]]], units_left3[[reg_id[i]]], 0.01, 0.2, 0.4, regions[reg_id[i]])
}

#plts = map(plot_list, 1)
ggarrange(plts2[[1]],plts2[[2]], plts2[[3]],
          plts2[[4]], plts2[[5]], plts2[[6]], 
          plts2[[7]], plts2[[8]], plts2[[9]], ncol = 3, nrow = 3, common.legend = T)

# check no. points

n_left = lapply(units_left3, lengths)
n_right = lapply(units_right3, lengths)

av_l = sapply(n_left, mean)
av_r = sapply(n_right, mean)

par(mfrow = c(1,1))
plot(seq(1, length(av_l)), av_l, col="red")
points(seq(1, length(av_r)),av_r, col="blue")


# save data ---------------------------------------------------------------
setwd("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/raw")
saveRDS(units_left3, "left.RDS")
saveRDS(units_right3, "right.RDS")
