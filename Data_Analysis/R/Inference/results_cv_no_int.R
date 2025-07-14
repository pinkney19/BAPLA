setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
# Load point estimates -------------------------------------------------------------

# visual dataset
left_params <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/left_params.RDS")
right_params <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/right_params.RDS")
beta_hat_vis_left= left_params$beta_hat
beta_hat_vis_right = right_params$beta_hat

# wheel dataset
left_params2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/results/left_params.RDS")
right_params2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/results/right_params.RDS")
beta_hat_wheel_left= left_params2$beta_hat
beta_hat_wheel_right = right_params2$beta_hat

# load new results --------------------------------------------------------
vis_left_desparsified2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/cv_results/no_intercept/vis_left_desparsified.RDS")
vis_right_desparsified2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/cv_results/no_intercept/vis_right_desparsified.RDS")

wheel_left_desparsified2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/cv_results/no_intercept/wheel_left_desparsified.RDS")
wheel_right_desparsified2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/cv_results/no_intercept/wheel_right_desparsified.RDS")



# results -----------------------------------------------------------------



out_vis_left = inference_results_new(vis_left_desparsified2, beta_hat_vis_left)
out_vis_right = inference_results_new(vis_right_desparsified2, beta_hat_vis_right)

out_wheel_left = inference_results_new(wheel_left_desparsified2, beta_hat_wheel_left)
out_wheel_right = inference_results_new(wheel_right_desparsified2, beta_hat_wheel_right)

out_vis_left$ids
out_vis_right$ids


out_wheel_left$ids
out_wheel_right$ids


# number deleted edges ----------------------------------------------------

length(out_vis_left$ids)
length(out_vis_right$ids)

length(out_wheel_left$ids)
length(out_wheel_right$ids)


# save results for plotting purposes --------------------------------------
# put edges to zero first
beta_hat_vis_left[out_vis_left$ids] = 0
beta_hat_vis_right[out_vis_right$ids] = 0

beta_hat_wheel_left[out_wheel_left$ids] = 0
beta_hat_wheel_right[out_wheel_right$ids] = 0

setwd("~/Downloads/BAPLA/Data_Analysis/R/Inference/cv_results")
saveRDS(beta_hat_vis_left, "beta_vis_left.RDS")
saveRDS(beta_hat_vis_right, "beta_vis_right.RDS")
saveRDS(beta_hat_wheel_left, "beta_wheel_left.RDS")
saveRDS(beta_hat_wheel_right, "beta_wheel_right.RDS")

# combine estimates for plotting purposes

id_all <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/neuron_ids_all.RDS")
sorted_ids = sort(id_all) 
id_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing_wheel/60_trials_idx.RDS")
id_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/60_trials_idx.RDS")


res_vis_left = combine_estimates(beta_hat_vis_left, sorted_ids, p_est = 48, id_vec = id_vis)
res_wheel_left = combine_estimates(beta_hat_wheel_left, sorted_ids, p_est = 56, id_vec = id_wheel)

res_vis_right =  combine_estimates(beta_hat_vis_right, sorted_ids, p_est = 48, id_vec = id_vis)
res_wheel_right = combine_estimates(beta_hat_wheel_right, sorted_ids, p_est = 56, id_vec = id_wheel)

setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
library(igraph)


# load combined "first stage" results ----------------------------------------------

vis_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/vis_left.RDS")
vis_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/vis_right.RDS")
wheel_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/wheel_left.RDS")
wheel_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/wheel_right.RDS")


# other necessary parameters ----------------------------------------------

col_region <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/col_region.RDS")
fixed_layout <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/combined_plots/fr/layout.RDS")
# neurons to shade in visual plots
x_vis = which(sorted_ids %in% id_vis==F)
# neurons to shade in wheel plots
x_wheel = which(sorted_ids %in% id_wheel==F)

par(mfrow = c(1,2), mar=c(0,0,0,0))
plot_graph_inference(vis_left, res_vis_left, 0.5, 7, col_region, fixed_layout, x_vis)
legend("topleft", legend = names(map_cols), col = cols,   
       pch = 19, bty = "n", cex = 1.2)
plot_graph_inference(vis_right, res_vis_right, 0.5, 7, col_region, fixed_layout, x_vis)

plot_graph_inference(wheel_left, res_wheel_left, 0.5, 7, col_region, fixed_layout, x_wheel)
plot_graph_inference(wheel_right, res_wheel_right, 0.5, 7, col_region, fixed_layout, x_wheel)
