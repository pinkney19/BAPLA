setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
# inital raster and psth plot ---------------------------------------------


left2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/wheel/raw/left.RDS")
right2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/wheel/raw/right.RDS")

check_f = function(times, pre_time, post_time, bin_size){
  # Define the time window (e.g., -0.2s to 0.4s around the stimulus)
  time_window <- c(-pre_time, post_time)  # seconds
  # Define bin size for PSTH (e.g., 0.01 seconds)
  # Create bins based on the defined time window and bin size
  time_bins <- seq(time_window[1], time_window[2], by = bin_size)
  firing_rate = firing_rate_func(times, time_bins, bin_size)
  data <- data.frame(time = time_bins[-length(time_bins)], firing_rate = firing_rate)
  return(list(dat = data, x = time_window))
}

library(MASS)
out1 = check_f(left2[[35]], 0.2, 0.4, 0.001)
out2 = check_f(left2[[52]], 0.2, 0.4, 0.001)
par(mfrow = c(1,2))

par(mfrow = c(3,3))
for(i in 1:56){
  out = check_f(left2[[i]], 0.2, 0.4, 0.001)
  plot(out$dat$time, out$dat$firing_rate, main = i, type='l', ylim = c(0,300))
}

plot(out1$dat$time, out1$dat$firing_rate, main = "on", type='l', ylim = c(0,300))
plot(out2$dat$time, out2$dat$firing_rate, main = "off", type='l',ylim = c(0,300))



plot(h2, main = "off",ylim = c(0,0.015))
out$x

plot(dat$time, dat$firing_rate, type='l')
k = kde2d(dat$time, dat$firing_rate, n = 600)
plot(k$x, k$y, col="blue")


p3 = plot_right_left_psth(right2[[52]], left2[[52]], 0.01, 0.2, 0.4, "SCsg")
p4 = plot_right_left_psth(right2[[35]], left2[[35]], 0.01, 0.2, 0.4, "POL")

plot_psth_one(left2[[52]], left2[[52]], 0.01, 0.2, 0.4, "", F)

plot_psth_one(right2[[35]], right2[[35]], 0.01, 0.2, 0.4, "", F)

plot_stim = ggarrange(p1, p2, ncol = 2, common.legend = T)
plot_wheel = ggarrange(p3, p4, ncol = 2, common.legend = T)
setwd("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis")
ggsave("plot_stim2.pdf", plot = plot_stim, width = 4.5, height =3.5, units = "in")
ggsave("plot_wheel2.pdf", plot = plot_wheel, width = 4.5, height =3.5, units = "in")


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
