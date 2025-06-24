setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
# inital raster and psth plot ---------------------------------------------

left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/raw/left.RDS")

plot_psth_one(left[[47]], left[[47]], 0.01, 0.2, 0.4, "", F)


spike_times = left[[47]]
# # Create an empty plot
# par(mfrow = c(1,1))
# plot(NULL, xlim = c(-0.2, 0.4), ylim = c(0, length(spike_times) + 0.5),
#      xlab = "Time (s)", ylab = "Trial", main = "", yaxt = "n")
# 
# # Add trial numbers to the y-axis
# axis(2, at = 1:length(spike_times), labels = 1:length(spike_times))
# 
# # Plot spikes for each trial
# for (i in seq_along(spike_times)) {
#   trial_spikes <- spike_times[[i]]
#   segments(x0 = trial_spikes, y0 = i - 0.4, x1 = trial_spikes, y1 = i + 0.4)
# }


# Convert the list into a data frame
spike_df <- do.call(rbind, lapply(seq_along(spike_times), function(trial) {
  data.frame(trial = trial, time = spike_times[[trial]])
}))

# Create the raster plot
ggplot(spike_df, aes(x = time, y = trial)) +
  geom_segment(aes(xend = time, yend = trial + 0.4), size = 0.5) +
  labs(x = "Time (s)", y = "Trial", title = "") +
  geom_vline(xintercept = 0, color = "deepskyblue") +
  theme_minimal()
