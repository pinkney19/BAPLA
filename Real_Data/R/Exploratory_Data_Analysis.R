# Exploratory Analysis ----------------------------------------------------

all_spikes <- readRDS("~/Downloads/BAPLA/Real_Data/Data/all_spikes.RDS")
right_trial_spikes <- readRDS("~/Downloads/BAPLA/Real_Data/Data/right_trial_spikes.RDS")
left_trial_spikes <- readRDS("~/Downloads/BAPLA/Real_Data/Data/left_trial_spikes.RDS")

regions <- names(all_spikes)


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

plot_right <- ggarrange(plot_list_right[[1]], plot_list_right[[2]], plot_list_right[[3]],plot_list_right[[4]],plot_list_right[[5]],
                        plot_list_right[[6]],plot_list_right[[7]],plot_list_right[[8]],plot_list_right[[9]],plot_list_right[[10]],
                        plot_list_right[[12]], ncol=4, nrow=3)
annotate_figure(plot_right, top = text_grob("Right Trials", face = "bold", size = 14))

plot_left <- ggarrange(plot_list_left[[1]], plot_list_left[[2]], plot_list_left[[3]],plot_list_left[[4]],plot_list_left[[5]],
                       plot_list_left[[6]],plot_list_left[[7]],plot_list_left[[8]],plot_list_left[[9]],plot_list_left[[10]],
                       plot_list_left[[12]], ncol=4, nrow=3)
annotate_figure(plot_left, top = text_grob("Left Trials", face = "bold", size = 14))


# Separate left and right -------------------------------------------------

plot_lr = plot_lr_examples(right_trial_spikes, left_trial_spikes, 0.01)

library(ggpubr)
plots_lr <- ggarrange(plot_lr[[1]], plot_lr[[2]], plot_lr[[3]],plot_lr[[4]],plot_lr[[5]],
                      plot_lr[[6]],plot_lr[[7]],plot_lr[[8]],plot_lr[[9]],plot_lr[[10]],
                      plot_lr[[12]], ncol=4, nrow=3, common.legend = T)
plots_lr
