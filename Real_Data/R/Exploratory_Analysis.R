
# Exploratory Analysis ----------------------------------------------------
all_spikes <- readRDS("~/Downloads/BAPLA/Real_Data/Data/all_spikes.RDS")
right_trial_spikes <- readRDS("~/Downloads/BAPLA/Real_Data/Data/right_trial_spikes.RDS")
left_trial_spikes <- readRDS("~/Downloads/BAPLA/Real_Data/Data/left_trial_spikes.RDS")

regions <- names(all_spikes)

# Plot PSTHs for example neurons from each region
plot_histograms = function(spike_data){
  par(mfrow=c(3,4))
  for(i in 1:length(regions)){
    if(i!=3){
      if(length(spike_data[[i]])!=0){
        hist(unlist(spike_data[[i]][[1]]), freq = F,  main = regions[i], xlab = expression(t))
        abline(v = 0, col="red")
      }
    }
    if(i==3){
      hist(unlist(spike_data[[i]][[2]]), freq = F,  main = regions[i], xlab = expression(t))
      abline(v = 0, col="red")
    }
  }
}

plot_histograms(all_spikes)
plot_histograms(right_trial_spikes)
plot_histograms(left_trial_spikes)


#ggplot version 
library(ggplot2)
library(dplyr)

plot_histograms <- function(spike_data, regions, title) {
  # Prepare the data
  data <- data.frame()
  for (i in 1:length(regions)) {
    if (i != 3) {
      if (length(spike_data[[i]]) != 0) {
        temp_data <- data.frame(region = regions[i], time = unlist(spike_data[[i]][[1]]))
        data <- rbind(data, temp_data)
      }
    } else {
      temp_data <- data.frame(region = regions[i], time = unlist(spike_data[[i]][[2]]))
      data <- rbind(data, temp_data)
    }
  }
  
  # Create the plot
  plot <- ggplot(data, aes(x = time)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", alpha = 0.7) +
    geom_vline(xintercept = 0, color = "red") +
    facet_wrap(~ region, scales = "free") +
    labs(x = expression(t), y = "Density", title = title) +
    theme_minimal()
  
  print(plot)
}
plot_histograms(all_spikes, regions, "All Trials")
plot_histograms(right_trial_spikes, regions, "Right Trials")
plot_histograms(left_trial_spikes, regions, "Left Trials")



# Singular neuron psth  ---------------------------------------------------

library(ggplot2)

plot_psth <- function(spike_times, bin_size, pre_time, post_time, title) {
  # Calculate the time window
  time_window <- seq(-pre_time, post_time, by = bin_size)
  
  # Initialize histogram
  hist <- numeric(length(time_window) - 1)
  
  # Loop through each stimulus onset
  for (i in 1:length(spike_times)) {
   
    hist <- hist + hist(spike_times[[i]], breaks = time_window, plot = FALSE)$counts
  }
  
  # Normalize histogram
  hist <- (hist / length(spike_times)) / bin_size
  
  # Prepare data for plotting
  data <- data.frame(time = time_window[-length(time_window)], firing_rate = hist)
  
  # Plot PSTH
  plot <- ggplot(data, aes(x = time, y = firing_rate)) +
    geom_bar(stat = "identity", width = bin_size, fill = "deepskyblue", alpha = 0.7) +
    geom_vline(xintercept = 0, color = "grey50") +
    labs(x = "Time (s)", y = "Firing Rate (Hz)", title = title) +
    theme_minimal()
  
  print(plot)
}

plot_examples = function(spike_data, bin_size){
  plot_list = list();
  for(i in 1:length(regions)){
    print(i)
    if (length(spike_data[[i]]) != 0) {
      if(i!=3){
        plot_list[[i]] = plot_psth(spike_data[[i]][[1]], bin_size, 0.2,0.4, regions[i])
      }
    }
      if(i==3){
        plot_list[[i]] = plot_psth(spike_data[[i]][[2]], bin_size, 0.2,0.4, regions[i])
      }
    
  }
  return(plot_list)
}


plot_list_all = plot_examples(all_spikes, 0.01)
plot_list_right = plot_examples(right_trial_spikes, 0.01)
plot_list_left = plot_examples(left_trial_spikes, 0.01)

library(ggpubr)
plot_all <- ggarrange(plot_list_all[[1]], plot_list_all[[2]], plot_list_all[[3]],plot_list_all[[4]],plot_list_all[[5]],
          plot_list_all[[6]],plot_list_all[[7]],plot_list_all[[8]],plot_list_all[[9]],plot_list_all[[10]],
          plot_list_all[[12]], ncol=4, nrow=3)

annotate_figure(plot_all, top = text_grob("All Trials", face = "bold", size = 14))

plot_right <- ggarrange(plot_list_right[[1]], plot_list_right[[2]], plot_list_right[[3]],plot_list_right[[4]],plot_list_right[[5]],
          plot_list_right[[6]],plot_list_right[[7]],plot_list_right[[8]],plot_list_right[[9]],plot_list_right[[10]],
          plot_list_right[[12]], ncol=4, nrow=3)
annotate_figure(plot_right, top = text_grob("Right Trials", face = "bold", size = 14))

plot_left <- ggarrange(plot_list_left[[1]], plot_list_left[[2]], plot_list_left[[3]],plot_list_left[[4]],plot_list_left[[5]],
          plot_list_left[[6]],plot_list_left[[7]],plot_list_left[[8]],plot_list_left[[9]],plot_list_left[[10]],
          plot_list_left[[12]], ncol=4, nrow=3)
annotate_figure(plot_left, top = text_grob("Left Trials", face = "bold", size = 14))


# -------------------------------------------------------------------------

plot_right_left_psth <- function(right_times, left_times, bin_size, pre_time, post_time, title) {
  # Calculate the time window
  time_window <- seq(-pre_time, post_time, by = bin_size)
  
  # Initialize histogram
  hist <- numeric(length(time_window) - 1)
  
  for (i in 1:length(right_times)) {
    
    hist <- hist + hist(right_times[[i]], breaks = time_window, plot = FALSE)$counts
  }
  
  # Normalize histogram
  hist <- (hist / length(right_times)) / bin_size
  # Prepare data for plotting
  data <- data.frame(time = time_window[-length(time_window)], firing_rate = hist)
  
  # do same for left times
  hist2 <- numeric(length(time_window) - 1)
  
  # Loop through each left trial
  for (i in 1:length(left_times)) {
    hist2 <- hist2 + hist(left_times[[i]], breaks = time_window, plot = FALSE)$counts
  }
  
  # Normalize histogram
  hist2 <- (hist2 / length(left_times)) / bin_size
  
  # Prepare data for plotting
  data2 <- data.frame(time = time_window[-length(time_window)], firing_rate = hist2)
  
  # Combine data into one data frame
  data$Trials <- 'Right'
  data2$Trials <- 'Left'
  combined_data <- rbind(data, data2)
  
  # Plot PSTH
  plot = ggplot(combined_data, aes(x = time, y = firing_rate, fill = Trials)) +
    geom_bar(stat = "identity", position = "identity", alpha = 0.8)+
    geom_vline(xintercept = 0, color = "grey50") +
    scale_fill_manual(values = c("blue", "red")) +
    labs(x = "Time (s)", y = "Firing Rate (Hz)", title = title) +
    theme_minimal()
  
  return(plot)
}

plot_both = function(right_trial_spikes, left_trial_spikes, bin_size){
  plot_list = list();
  for(i in 1:length(regions)){
    print(i)
    if (length(right_trial_spikes[[i]]) != 0) {
      if(i!=3){
        plot_list[[i]] = plot_right_left_psth(right_trial_spikes[[i]][[1]], left_trial_spikes[[i]][[1]],bin_size, 0.2,0.4, regions[i])
      }
    }
    if(i==3){
      plot_list[[i]] = plot_right_left_psth(right_trial_spikes[[i]][[2]], left_trial_spikes[[i]][[2]],bin_size, 0.2,0.4, regions[i])
    }
    
  }
  return(plot_list)
}
plot_lr = plot_both(right_trial_spikes, left_trial_spikes, 0.01)

library(ggpubr)
plots_lr <- ggarrange(plot_lr[[1]], plot_lr[[2]], plot_lr[[3]],plot_lr[[4]],plot_lr[[5]],
                      plot_lr[[6]],plot_lr[[7]],plot_lr[[8]],plot_lr[[9]],plot_lr[[10]],
                      plot_lr[[12]], ncol=4, nrow=3, common.legend = T)
plots_lr
