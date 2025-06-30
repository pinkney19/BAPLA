setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")

units_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/wheel/raw/left.RDS")
units_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/wheel/raw/right.RDS")
regions <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/region_ids.RDS")

library(ggplot2)
reg_id = c(52, 35) # SCsg and POL neurons in example/motivation plots
names(reg_id) = c("SCsg", "POL")
plts2 = list();

for(i in 1:length(reg_id)){
  plts2[[i]] = plot_right_left_psth(units_right[[reg_id[i]]], units_left[[reg_id[i]]], 0.01, 0.2, 0.4, names(reg_id)[i])
}

ggarrange(plts2[[1]], plts2[[2]], common.legend = T)

#plts = map(plot_list, 1)

#plts2[[1]]

# read in f estimates
#f_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/f_left.RDS")

all_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/results/left_params.RDS")

f_left = all_left$fs
beta0_left = all_left$beta_0_alt

par(mfrow = c(1,1))


f_left = list(all_left$fs[[reg_id[1]]], all_left$fs[[reg_id[2]]])
beta0_left = list(all_left$beta_0_alt[[reg_id[1]]], all_left$beta_0_alt[[reg_id[2]]])

calc_firing_rate = function(f, intercept){
  fir_rate = list();
  for(i in 1:length(f)){
    fir_rate[[i]] = f[[i]] + intercept[[i]] 
  }  
  rate = lapply(fir_rate, function(x) exp(x)/(1+exp(x))*1000 )
  return(rate)
}

fr = list();rate = list();
for(i in 1:length(f_left)){
  fr[[i]] = f_left[[i]] +  beta0_left[[i]]
  rate[[i]] = exp(fr[[i]]) / (1+exp(fr[[i]]))  * 1000
}

plot(x =seq(-0.2, 0.4, length.out = length(rate[[1]])), 
     y = seq(-5, 3, length.out = length(rate[[1]])), type ="n",
     xlab = "Time", ylab = "Firing rate")

for(i in 1:length(rate)){
  lines(x =seq(-0.2, 0.4, length.out = length(rate[[1]])),
        y = f_left[[i]], col=i)
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
                

plot_wheel = ggarrange(p1,p2, ncol=2,common.legend = T)
setwd("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/plots")
ggsave("plot_wheel_f_left.pdf", plot = plot_wheel, width = 4.5, height =3.5, units = "in")

