setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")

units_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/wheel/raw/left.RDS")
units_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/wheel/raw/right.RDS")
regions <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/region_ids.RDS")

library(ggplot2)
#reg_id = c(52, 35) # SCsg and POL neurons in example/motivation plots

id_non_stationary <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/combined_plots/f_v_no_f/id_non_stationary.RDS")
region_id <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/wheel/region_ids.RDS")

#names(reg_id) = c("SCsg", "POL")
plts2 = list();

for(i in 1:length(id_non_stationary)){
  plts2[[i]] = plot_right_left_psth(units_right[[id_non_stationary[i]]], units_left[[id_non_stationary[i]]], 
                                    0.01, 0.2, 0.4, region_id[i])
}

#ggarrange(plts2[[1]], plts2[[2]], common.legend = T)

#plts = map(plot_list, 1)

#plts2[[1]]

# read in f estimates
#f_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/f_left.RDS")

all_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/results/left_params.RDS")

f_left = all_left$fs #list(all_left$fs[[reg_id[1]]], all_left$fs[[reg_id[2]]])
beta0_left = all_left$beta_0#list(all_left$beta_0_alt[[reg_id[1]]], all_left$beta_0_alt[[reg_id[2]]])

calc_firing_rate = function(f, intercept){
  fir_rate = list();
  for(i in 1:length(f)){
    fir_rate[[i]] = f[[i]] + intercept[[i]] 
  }  
  rate = lapply(fir_rate, function(x) exp(x)/(1+exp(x))*1000 )
  return(rate)
}

f_left = f_left[id_non_stationary]
beta0_left = beta0_left[id_non_stationary]

fr = calc_firing_rate(f_left, beta0_left)

plot_list = list();
for(i in 1:length(fr)){
  plot_list[[i]] =  plts2[[i]] + geom_line(data = data.frame(x = seq(-0.2,0.4,length.out = length(fr[[i]])) ,
                                y = fr[[i]]), 
              aes(x = x, y = y), 
              inherit.aes = F,
              color = "black")
}

p1 = plts2[[1]] +
  geom_line(data = data.frame(x = seq(-0.2,0.4,length.out = length(fr[[1]])) ,
                              y = fr[[1]]), 
            aes(x = x, y = y), 
            inherit.aes = F,
            color = "black")

p2 = plts2[[2]] +
  geom_line(data = data.frame(x = seq(-0.2,0.4,length.out = length(fr[[2]])) ,
                              y = fr[[2]]), 
            aes(x = x, y = y), 
            inherit.aes = F,
            color = "black")
                

plot_wheel = ggarrange(p1,p2, ncol=2,common.legend = T)#
plot_wheel









setwd("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/plots")
ggsave("plot_wheel_f_left.pdf", plot = plot_wheel, width = 4.5, height =3.5, units = "in")

