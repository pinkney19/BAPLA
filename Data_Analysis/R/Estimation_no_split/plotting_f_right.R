setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")

units_right3 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/raw/right.RDS")
units_left3 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/raw/right.RDS")
regions <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/region_ids.RDS")

library(ggplot2)
reg_id = c(1,2,5,7, 27, 30, 43, 46, 48)

plts2 = list();
for(i in 1:length(reg_id)){
  plts2[[i]] = plot_right_left_psth(units_right3[[reg_id[i]]], units_left3[[reg_id[i]]], 0.01, 0.2, 0.4, regions[reg_id[i]])
}

#plts = map(plot_list, 1)

plts2[[1]]

# read in f estimates
f_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/f_right.RDS")
#
plot_with_f = function(id){
plt = plts2[[id]] + 
  geom_line(data = data.frame(x = seq(-0.2, 0.4, length.out = length(f_left[[1]])), 
                              y = f_left[[id]]), 
            aes(x = x, y = y), 
            inherit.aes = FALSE,  # Prevent inheriting aesthetics from the base plot
            color = "black")
  return(plt)
}

plot_f = list();
for(i in 1:length(reg_id)){
  plot_f[[i]] = plot_with_f(i)
}

ggarrange(plot_f[[1]],plot_f[[2]], plot_f[[3]],
          plot_f[[4]], plot_f[[5]], plot_f[[6]], 
          plot_f[[7]], plot_f[[8]], plot_f[[9]], ncol = 3, nrow = 3, legend = F)


