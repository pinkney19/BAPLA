setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")


units_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/raw/left.RDS")
units_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Estimation/60_Trials/raw/right.RDS")


library(ggplot2)
reg_id = c(47, 30) # SCsg and POL neurons in example/motivation plots
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

all_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/left_params.RDS")

f_left = list(all_left$fs[[46]], all_left$fs[[reg_id[2]]])
beta0_left = list(all_left$beta_0_alt[[46]], all_left$beta_0_alt[[reg_id[2]]])

calc_firing_rate = function(f, intercept){
  fir_rate = list();
  for(i in 1:length(f)){
    fir_rate[[i]] = f[[i]] + intercept[[i]] 
  }  
  rate = lapply(fir_rate, function(x) exp(x)/(1+exp(x))*1000 )
  return(rate)
}


fr = calc_firing_rate(f_left, beta0_left)

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
                

plot_stim = ggarrange(p1,p2, ncol=2,common.legend = T)
plot_stim
setwd("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/plots")
ggsave("plot_stim_f_left.pdf", plot = plot_stim, width = 4.5, height =3.5, units = "in")

