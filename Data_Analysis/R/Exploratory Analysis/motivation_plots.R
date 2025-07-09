setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
# inital raster and psth plot ---------------------------------------------

left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/raw/left.RDS")
right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/raw/right.RDS")

p1 = plot_right_left_psth(right[[47]], left[[47]], 0.01, 0.2, 0.4, "SCsg")
p2 = plot_right_left_psth(right[[30]], left[[30]], 0.01, 0.2, 0.4, "POL")

left2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/wheel/raw/left.RDS")
right2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/wheel/raw/right.RDS")

p3 = plot_right_left_psth(right2[[52]], left2[[52]], 0.01, 0.2, 0.4, "SCsg")
p4 = plot_right_left_psth(right2[[35]], left2[[35]], 0.01, 0.2, 0.4, "POL")

plot_psth_one(left[[47]], left[[47]], 0.01, 0.2, 0.4, "", F)

plot_psth_one(right[[47]], right[[47]], 0.01, 0.2, 0.4, "", F)

plot_stim = ggarrange(p1, p2, ncol = 2, common.legend = T)
plot_wheel = ggarrange(p3, p4, ncol = 2, common.legend = T)
setwd("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis")
ggsave("plot_stim2.pdf", plot = plot_stim, width = 4.5, height =3.5, units = "in")
ggsave("plot_wheel2.pdf", plot = plot_wheel, width = 4.5, height =3.5, units = "in")


spike_times = right[[47]]
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
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size=12))
