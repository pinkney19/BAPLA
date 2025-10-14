setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")

units_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/wheel/raw/left.RDS")
units_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/wheel/raw/right.RDS")
regions <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/region_ids.RDS")

library(ggplot2)
#reg_id = c(52, 35) # SCsg and POL neurons in example/motivation plots

id_non_stationary <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/combined_plots/f_v_no_f/id_non_stationary.RDS")
id_stationary <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/combined_plots/f_v_no_f/id_stationary.RDS")
region_id <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/wheel/region_ids.RDS")

#names(reg_id) = c("SCsg", "POL")
plts2 = list();

for(i in 1:length(id_non_stationary)){
  plts2[[i]] = plot_right_left_psth(units_right[[id_non_stationary[i]]], units_left[[id_non_stationary[i]]], 
                                    0.01, 0.2, 0.4, region_id[id_non_stationary[i]])
}

plt_stat = plot_right_left_psth(units_right[[id_stationary[31]]], units_left[[id_stationary[31]]], 
                                0.01, 0.2, 0.4, region_id[id_stationary[31]])
#ggarrange(plts2[[1]], plts2[[2]], common.legend = T)

#plts = map(plot_list, 1)

#plts2[[1]]

# read in f estimates
#f_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/f_left.RDS")

all_left_3 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/3_basis/results/left_params.RDS")

f_left = all_left_3$fs #list(all_left$fs[[reg_id[1]]], all_left$fs[[reg_id[2]]])
beta0_left = all_left_3$beta_0#list(all_left$beta_0_alt[[reg_id[1]]], all_left$beta_0_alt[[reg_id[2]]])

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
#plot_list = list();
#for(i in 1:length(fr)){
#  plot_list[[i]] =  plts2[[i]] + geom_line(data = data.frame(x = seq(-0.2,0.4,length.out = length(fr[[i]])) ,
 #                               y = fr[[i]]), 
  #            aes(x = x, y = y), 
   #           inherit.aes = F,
    #          color = "black")
#}
f_left = all_left_3$fs #list(all_left$fs[[reg_id[1]]], all_left$fs[[reg_id[2]]])
beta0_left = all_left_3$beta_0#list(all_left$beta_0_alt[[reg_id[1]]], all_left$beta_0_alt[[reg_id[2]]])

f_left_stat = f_left[id_stationary]
beta0_left = beta0_left[id_stationary]
fr_stat = calc_firing_rate(f_left_stat, beta0_left)

p1 = plt_stat +
  geom_line(data = data.frame(x = seq(-0.2,0.4,length.out = length(fr_stat[[31]])) ,
                              y = fr_stat[[31]]), 
            aes(x = x, y = y), 
            inherit.aes = F,
            color = "black")

p2 = plts2[[13]] +
  geom_line(data = data.frame(x = seq(-0.2,0.4,length.out = length(fr[[13]])) ,
                              y = fr[[13]]), 
            aes(x = x, y = y), 
            inherit.aes = F,
            color = "black")+theme(legend.position = "none")
                
p2

plot_wheel = ggarrange(p1,p2, ncol=2,common.legend = T)#
plot_wheel


p2


setwd("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/3_basis/results")
ggsave("FR_POL_3_basis.pdf", plot= p2, width = 4, height = 4, units="in")



setwd("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/plots")
ggsave("plot_wheel_f_left.pdf", plot = plot_wheel, width = 4.5, height =3.5, units = "in")




# Different neurons -------------------------------------------------------

# 52 and 35
p3 = plts2[[13]] +
  geom_line(data = data.frame(x = seq(-0.2,0.4,length.out = length(fr[[13]])) ,
                              y = fr[[13]]), 
            aes(x = x, y = y), 
            inherit.aes = F,
            color = "black")

p4 = plts2[[35]] +
  geom_line(data = data.frame(x = seq(-0.2,0.4,length.out = length(fr[[35]])) ,
                              y = fr[[35]]), 
            aes(x = x, y = y), 
            inherit.aes = F,
            color = "black")

plot_wheel2 = ggarrange(p3,p4, ncol=2,common.legend = T)#
plot_wheel2
