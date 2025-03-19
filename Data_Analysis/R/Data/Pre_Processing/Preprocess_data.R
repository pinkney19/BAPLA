# Load Functions ----------------------------------------------------------

setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
library(purrr)

# Load data ---------------------------------------------------------------

setwd("~/Downloads/BAPLA/Data_Analysis/R/Data")
right_binned <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/right_binned.RDS")
left_binned <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/left_binned.RDS")
regions = names(left_binned)


# Remove trials with no spikes --------------------------------------------


data_left = remove_zero_trials(left_binned)
data_right = remove_zero_trials(right_binned)

no_trials_left = lapply(data_left, lengths)
no_trials_right = lapply(data_right, lengths)

cl = round(unlist(lapply(no_trials_left, mean)))
cr =round(unlist(lapply(no_trials_right, mean)))
par(mfrow = c(1,2))
hist(cl, main = "Avg Left Trials in each Region")
hist(cr, main = "Avg Right Trials in each Region")



# Filter units based on number of trials ----------------------------------


# Check which neurons have at least 60 "Active" trials -------------------------
# i.e. neurons with activity (spiking) in at least 60 trials

id_right = which(unlist(no_trials_right)>60)
setwd("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/neuron_ids/60_trials")
saveRDS(id_right, "right_id_filtered_trials.RDS")

dr = unlist(data_right, recursive = F)
dr = dr[id_right]

id_left = which(unlist(no_trials_left)>60)
dl = unlist(data_left, recursive = F)
dl = dl[id_left]

# keep same neurons in both left and right trials - want to be able to compare

keep_ids = which(names(dl)%in% names(dr))

id_left2 = id_left[keep_ids]
setwd("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/neuron_ids/60_trials")
saveRDS(id_left2, "left_id_filtered_trials.RDS")

dl = dl[keep_ids]



length(dr) == length(dl) # 218 neurons
names(dl) == names(dr)


library(tm)

regions_dl = removeNumbers(names(dl))
regions_dr = removeNumbers(names(dr))

regions_dl == regions_dr
regions_dl[regions_dl == "CA"] = "CA1"
regions_dr[regions_dr == "CA"] = "CA1"

regions_dl == regions_dr

# Filter units based on how active they are, i.e., how many spikes? ------------



# Make data same number of trials 
dr_out = cut_data(dr, 60)
dr_new = dr_out$new_data
right_kept = dr_out$trials_kept
dr_num_spikes = dr_out$num_spikes
dl_out = cut_data(dl, 60)
dl_new = dl_out$new_data
dl_num_spikes = dl_out$num_spikes
left_kept = dl_out$trials_kept

av_dl = sapply(dl_num_spikes, mean)
av_dr = sapply(dr_num_spikes, mean)
av_dr == av_dl

par(mfrow = c(1,1))

plot(seq(1, 218), sapply(dr_num_spikes, mean), col="red", ylab = "Avg. Spikes", xlab = "Unit ID",
     main = "Average No. Spikes per trial", type='l', ylim = c(0,65))
lines(seq(1, 218), sapply(dl_num_spikes, mean), col="blue")
legend("topright", c("Left", "Right"), col=c("blue", "red"), lty = c(1,1))
abline(h = 10, lty = 2)
idx_r = which(av_dr>10); idx_l = which(av_dl>10)

# save IDs of neurons which match fitlering conditions - i.e., at least 60 trials and at least 10 spikes
id_right2 = id_right[idx_r]

# save IDS of trials for which the neurons matching the filtering conditions refer to
setwd("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/neuron_ids/60_trials/trial_ids")
trials_right = right_kept[idx_r]
saveRDS(trials_right, "trials_right.RDS")


setwd("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/neuron_ids/60_trials")
saveRDS(id_right2, "right_id_filtered_spikes.RDS")

common_ids = idx_l[which(idx_l %in% idx_r)]
common_ids

left3 = id_left2[common_ids]
names(left3) == names(id_right2) # check same ids

setwd("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/neuron_ids/60_trials")
saveRDS(left3, "left_id_filtered_spikes.RDS")

trials_left = left_kept[common_ids]
setwd("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/neuron_ids/60_trials/trial_ids")
saveRDS(trials_left, "trials_left.RDS")

points(common_ids, sapply(dl_num_spikes, mean)[common_ids] ) # add to plot


length(common_ids)

unique(regions_dr[common_ids])

setwd("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing")
saveRDS(common_ids, "60_trials_idx.RDS")
# save data which satisfy criteria ----------------------------------------

left_data_to_use = dl_new[common_ids]
right_data_to_use = dr_new[common_ids]

#setwd("~/Downloads/BAPLA/Real_Data/Data/kept_trials")
#saveRDS(right_kept, "idx_right.RDS") # save trials used for right analysis.
#saveRDS(left_kept, "idx_left.RDS") # save trials used for left analysis.



# get average number of spikes per trial for each unit --------------------


av_left = avg_spike_per_trial(left_data_to_use)
av_right = avg_spike_per_trial(right_data_to_use)

par(mfrow = c(1,2))
plot(seq(1, length(av_right)), av_right, col="red",  ylim = c(0,60), xlab = "Unit ID", ylab = "Avg points per trial")
points(seq(1, length(av_left)), av_left, col="blue")
abline(h = 10, lty =2)
hist(av_left, main = "", col="blue", xlab = "Avg points per trial")
hist(av_right, add = T, col="red")

# format data -------------------------------------------------------------


# Means we keep 48 units in our analysis (good for us since synthetic experiments were for d = 50!)

dl_form = format_data(left_data_to_use)
dr_form = format_data(right_data_to_use)

# save data
setwd("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation")
saveRDS(dl_form, "left.RDS")
saveRDS(dr_form, "right.RDS")

