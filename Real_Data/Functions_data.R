
# 1. Plot single unit PSTH ------------------------------------------------

plot_psth <- function(spike_times, bin_size, pre_time, post_time, title) {
  #'  Function to plot normalised PSTH for single unit
  #'
  #' @param spike_times 
  #' @param bin_size 
  #' @param pre_time
  #' @param post_time
  #' @param title title of plot
  #' @export  
  
  
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
  
  return(plot)
}

# 2. Plot multiple PSTH plots ---------------------------------------------

plot_examples = function(spike_data, bin_size){
  #'  Function to plot example psth plots for multiple units
  #'
  #' @param spike_data 
  #' @param bin_size
  #' @export  
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


# 3. Plot PSTH of left and Right ------------------------------------------

plot_right_left_psth <- function(right_times, left_times, bin_size, pre_time, post_time, title) {
  #'  Function to plot normalised PSTH for left and right trials
  #'
  #' @param right_times
  #' @param left_times
  #' @param bin_size 
  #' @param pre_time
  #' @param post_time
  #' @param title title of plot
  #' @export  
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

plot_lr_examples = function(right_trial_spikes, left_trial_spikes, bin_size){
  #'  Function to plot examples of normalised PSTH for left and right trials
  #'
  #' @param right_trial_spikes
  #' @param left_trial_spikes
  #' @param bin_size 
  #' @param pre_time
  #' @param post_time
  #' @param title title of plot
  #' @export  
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