# Exploratory Analysis ----------------------------------------------------


# load data

all_spikes <- readRDS("~/Downloads/BAPLA/Real_Data/Data/all_spikes.RDS")
right_trial_spikes <- readRDS("~/Downloads/BAPLA/Real_Data/Data/right_trial_spikes.RDS")
left_trial_spikes <- readRDS("~/Downloads/BAPLA/Real_Data/Data/left_trial_spikes.RDS")

regions <- names(all_spikes)


# load trials used

idx_left <- readRDS("~/Downloads/BAPLA/Real_Data/Data/kept_trials/idx_left.RDS")
idx_right <- readRDS("~/Downloads/BAPLA/Real_Data/Data/kept_trials/idx_right.RDS")
idx_unit <- readRDS("~/Downloads/BAPLA/Real_Data/Data/kept_trials/idx_unit.RDS")


# Remove unused neurons ---------------------------------------------------

right_spikes = unlist(right_trial_spikes, recursive = F)
right_spikes = right_spikes[idx_unit]
left_spikes = unlist(left_trial_spikes, recursive = F)
left_spikes = left_spikes[idx_unit]


# keep only trials used in the estimation procedure -----------------------

for(i in 1:length(right_spikes)){
  right_spikes[[i]] = right_spikes[[i]][idx_right[[i]]]
  left_spikes[[i]] = left_spikes[[i]][idx_left[[i]]]
}
region_id <- readRDS("~/Downloads/BAPLA/Real_Data/Data/region_id.RDS")
names(right_spikes) = region_id
names(left_spikes) = region_id


# check number spikes -----------------------------------------------------

no_spikes_right = list(); no_spikes_left = list();
for(i in 1:length(right_spikes)){
  no_spikes_right[[i]] = lengths(right_spikes[[i]])
  no_spikes_left[[i]] = lengths(left_spikes[[i]])
}

avg_spike_right = round(unlist(lapply(no_spikes_right, mean)))
avg_spike_left = round(unlist(lapply(no_spikes_left, mean)))

df = data.frame(avg_right = avg_spike_right, avg_left = avg_spike_left, region = region_id, label = seq(1,78))

r = ggplot(data = df, aes(x = label, y = avg_right, color = region)) +
  geom_point() +
  theme_minimal()

l = ggplot(data = df, aes(x = label, y = avg_left, color = region)) +
  geom_point() +
  theme_minimal()

idx1 = which(df$avg_left>10)
idx2 = which(df$avg_right>10)

both_idx = intersect(idx1, idx2)

rs = right_spikes[both_idx]
ls = left_spikes[both_idx]

r2 = region_id[both_idx]

names(rs) = r2
names(ls) = r2

ggarrange(l,r,ncol=2, common.legend = T, legend = "right")

# plots -------------------------------------------------------------------
setwd("~/Downloads/BAPLA/Real_Data")
source("Functions_data.R")
plot_list_right = plot_examples(rs, 0.05 , unique(r2))
plot_right <- ggarrange(plot_list_right[[1]]$plot, plot_list_right[[2]]$plot, plot_list_right[[3]]$plot,plot_list_right[[4]]$plot,plot_list_right[[5]]$plot,
                        plot_list_right[[6]]$plot,plot_list_right[[7]]$plot,plot_list_right[[8]]$plot, ncol=3, nrow=3)
annotate_figure(plot_right, top = text_grob("Right Trials", face = "bold", size = 14))


plot_list_left = plot_examples(ls, 0.1, unique(r2))

plot_left <- ggarrange(plot_list_left[[1]]$plot, plot_list_left[[2]]$plot, plot_list_left[[3]]$plot,
                       plot_list_left[[4]]$plot,plot_list_left[[5]]$plot,plot_list_left[[6]]$plot,
                       plot_list_left[[7]]$plot,plot_list_left[[8]]$plot, ncol=3, nrow=3)
annotate_figure(plot_left, top = text_grob("Left Trials", face = "bold", size = 14))
# -------------------------------------------------------------------------
setwd("~/Downloads/BAPLA/Real_Data")
source("Functions_data.R")


# Plots -------------------------------------------------------------------
library(ggplot2)
plot_list_all = plot_examples(all_spikes, 0.01)
plot_list_right = plot_examples(right_trial_spikes, 0.01)
plot_list_left = plot_examples(left_trial_spikes, 0.01)



library(ggpubr)
plot_all <- ggarrange(plot_list_all[[1]], plot_list_all[[2]], plot_list_all[[3]],plot_list_all[[4]],plot_list_all[[5]],
                      plot_list_all[[6]],plot_list_all[[7]],plot_list_all[[8]],plot_list_all[[9]],plot_list_all[[10]],
                      plot_list_all[[12]], ncol=4, nrow=3)

annotate_figure(plot_all, top = text_grob("All Trials", face = "bold", size = 14))

plot_right <- ggarrange(plot_list_right[[1]]$plot, plot_list_right[[2]]$plot, plot_list_right[[3]]$plot,plot_list_right[[4]]$plot,plot_list_right[[5]]$plot,
                        plot_list_right[[6]]$plot,plot_list_right[[7]]$plot,plot_list_right[[8]]$plot,plot_list_right[[9]]$plot,plot_list_right[[10]]$plot,
                        plot_list_right[[12]]$plot, ncol=4, nrow=3)
annotate_figure(plot_right, top = text_grob("Right Trials", face = "bold", size = 14))

plot_left <- ggarrange(plot_list_left[[1]]$plot, plot_list_left[[2]]$plot, plot_list_left[[3]]$plot,
                       plot_list_left[[4]]$plot,plot_list_left[[5]]$plot,plot_list_left[[6]]$plot,
                       plot_list_left[[7]]$plot,plot_list_left[[8]]$plot,plot_list_left[[9]]$plot
                       ,plot_list_left[[10]]$plot,plot_list_left[[12]]$plot, ncol=4, nrow=3)
annotate_figure(plot_left, top = text_grob("Left Trials", face = "bold", size = 14))


# Separate left and right -------------------------------------------------

plot_lr = plot_lr_examples(right_trial_spikes, left_trial_spikes, 0.01)

library(ggpubr)
plots_lr <- ggarrange(plot_lr[[1]], plot_lr[[2]], plot_lr[[3]],plot_lr[[4]],plot_lr[[5]],
                      plot_lr[[6]],plot_lr[[7]],plot_lr[[8]],plot_lr[[9]],plot_lr[[10]],
                      plot_lr[[12]], ncol=4, nrow=3, common.legend = T)
plots_lr


ggarrange(plot_lr[[1]], plot_lr[[2]], plot_lr[[6]], ncol=3, common.legend = T)


