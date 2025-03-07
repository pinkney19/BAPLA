
# Load Functions ----------------------------------------------------------

setwd("~/Downloads/BAPLA/Real_Data")
source("Functions_data.R")
library(purrr)

# Load Data ---------------------------------------------------------------

left_binned <- readRDS("~/Downloads/BAPLA/Real_Data/Data/left_binned.RDS")
right_binned <- readRDS("~/Downloads/BAPLA/Real_Data/Data/right_binned.RDS")
regions = names(left_binned)

# Exploratory Checks ------------------------------------------------------

par(mfrow=c(3,4))
for(i in 1:length(regions)){
  plot(seq(-0.2,0.4,by = 0.001),left_binned[[i]][[1]][[1]], type='l',main = regions[i], xlab = "time(secs)", ylab = "")
  
}

# Remove trials with no spikes --------------------------------------------


data_left = remove_zero_trials(left_binned)

data_right = remove_zero_trials(right_binned)

no_trials_left = lapply(data_left, lengths)
no_trials_right = lapply(data_right, lengths)

round(unlist(lapply(no_trials_left, mean)))
round(unlist(lapply(no_trials_right, mean)))

df = rbind(round(unlist(lapply(no_trials_left, mean))), round(unlist(lapply(no_trials_right, mean))))
# table for paper
library(xtable)
xtable(df)

setwd("~/Downloads/BAPLA/Real_Data/Data/processed")
saveRDS(data_left, "data_left.RDS")
saveRDS(data_right, "data_right.RDS")



# make same number of trials ----------------------------------------------

id_right = which(unlist(no_trials_right)>60)
dr = unlist(data_right, recursive = F)
dr = dr[id_right]

id_left = which(unlist(no_trials_left)>60)
dl = unlist(data_left, recursive = F)
dl = dl[id_left]


keep_ids = which(names(dl)%in% names(dr))

dl = dl[keep_ids]



length(dr) == length(dl) # 78 neurons
names(dl) == names(dr)


library(tm)

regions_dl = removeNumbers(names(dl))
regions_dr = removeNumbers(names(dr))

regions_dl == regions_dr
regions_dl[1] = regions_dr[1] = "CA1"

# Make data same number of trials 

dr_new = cut_data(dr, 60)
dl_new = cut_data(dl, 60)

names(dl_new) = regions_dl
names(dr_new) = regions_dr

# get average number of spikes per trial for each unit --------------------
avg_spike_per_trial = function(data){
  #' Function to calculate average number of spiking events per experimental trial
  #' for each neuron/unit
  #' @param data (list) nested list of trial times for each unit
  av_spike = NULL;
  for(i in 1:length(data)){
    av_spike[i] = mean(unlist(lapply(data[[i]], sum)))
  }
  return(av_spike)
}

av_left = avg_spike_per_trial(dl_new)
av_right = avg_spike_per_trial(dr_new)

par(mfrow = c(1,2))
hist(av_left)
hist(av_right)

# format data -------------------------------------------------------------

dl_form = format_data(dl_new)
dr_form = format_data(dr_new)

# save data
setwd("~/Downloads/BAPLA/Real_Data/Data/estimation")
saveRDS(dl_form, "left.RDS")
saveRDS(dr_form, "right.RDS")
# 
# library(splines)
# res = bapla_single_unit_trial(data_left, 1, 50, 1e-5, rep(10,36), 10)
# 
# #sim_data <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/Simulate_Data/10/Tuning_data/data_50_tuning_1000.RDS")
# beta_0 = NULL; f = list(); f = rep(list(f), 106); beta = list(); beta = rep(list(beta), 106);iter = NULL;
# for(i in 1:106){
#   print(i)
#   res = bapla_single_unit_trial(data_left, 1, 1000, 1e-5, rep(10,84), 10)
#   beta[[i]] = res$beta
#   f[[i]] = res$f
#   beta_0[i] = res$beta0
#   iter[i] = res$iter
# }
