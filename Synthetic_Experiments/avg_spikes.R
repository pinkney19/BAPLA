
# Checks for simulated data -----------------------------------------------

# CG
# old 50/50 model -> beta_0 = -2.6 => 70Hz firing rate


data_50_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulate_Data/10/Sims_data/data_50_1000.RDS")
data_50_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulate_Data/10/Sims_data/data_50_2000.RDS")
data_50_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulate_Data/10/Sims_data/data_50_5000.RDS")
data_50_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/ER/Simulate_Data/10/Sims_data/data_50_10000.RDS")


data_check_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulate_Data/10/Sims_data/data_check_1000.RDS")

avg_spikes = function(data, unit){
  #' Function to check average number of spikes for a given unit/neuron over the total number of samples
  av1 = NULL;
  for(i in 1:length(data)){
    av1[i] = mean(data[[i]][unit,])
  }
  return(mean(av1)*100)
}

avg_spikes(data_check_1000, 1)
avg_spikes(data_50_2000, 1)
avg_spikes(data_50_5000, 1)
avg_spikes(data_50_10000, 1)



