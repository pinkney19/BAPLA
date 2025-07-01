id_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing_wheel/60_trials_idx.RDS")

id_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/60_trials_idx.RDS")
setwd("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis")





all_ids = unique(c(id_wheel, id_vis))
saveRDS(all_ids, "neuron_ids_all.RDS")
