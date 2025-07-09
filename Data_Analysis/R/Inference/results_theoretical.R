
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


# Load desparsified results -----------------------------------------------
vis_left_desparsified <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/vis/theoretical_option/vis_left_desparsified.RDS")
vis_right_desparsified <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/vis/theoretical_option/vis_right_desparsified.RDS")

wheel_left_desparsified <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/wheel/theoretical/wheel_left_desparsified.RDS")
wheel_right_desparsified <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/wheel/theoretical/wheel_right_desparsified.RDS")

# Functions ---------------------------------------------------------------


inference_results = function(res, beta_hat){
  non_zero_indices = list(); mat = matrix(NA, nrow=dim(beta_hat)[1], ncol=dim(beta_hat)[1]);
  for(i in 1:length(res)){
    check_0 = ifelse(res[[i]]$lower < 0 & res[[i]]$upper>0, T, F)
    non_zero_indices[[i]] = which(check_0==F)
    if(length(non_zero_indices[[i]])!=0){
      # set non zero entries to 1
      mat[i, non_zero_indices[[i]]] = 1
    }
  }
  # put into adjacency matrix format
  beta_hat[beta_hat!=0]=1
  # set NAs to zero
  mat[is.na(mat)]=0
  out = mat == beta_hat
  ids = which(out == F) # edges to be  greyed out in final plots
  id_mat = arrayInd(ids, dim(beta_hat))
  return(list(ids=ids, mat=mat, beta_hat = beta_hat, n_inds= non_zero_indices, id_mat = id_mat))
}
  

# results -----------------------------------------------------------------



out_vis_left = inference_results(vis_left_desparsified, beta_hat_vis_left)
out_vis_right = inference_results(vis_right_desparsified, beta_hat_vis_right)

out_wheel_left = inference_results(wheel_left_desparsified, beta_hat_wheel_left)
out_wheel_right = inference_results(wheel_right_desparsified, beta_hat_wheel_right)

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

setwd("~/Downloads/BAPLA/Data_Analysis/R/Inference/theoretical_results")
saveRDS(beta_hat_vis_left, "beta_vis_left.RDS")
saveRDS(beta_hat_vis_right, "beta_vis_right.RDS")
saveRDS(beta_hat_wheel_left, "beta_wheel_left.RDS")
saveRDS(beta_hat_wheel_right, "beta_wheel_right.RDS")

par(mfrow = c(1,2))
plot_graph_inference(res_vis_left, res_vis_left_inf, 0.5, 7, col_region, fixed_layout, x_vis)
legend("topleft", legend = names(map_cols), col = cols,   
       pch = 19, bty = "n", cex = 1.2)
plot_graph_inference(res_vis_right, res_vis_right_inf, 0.5, 7, col_region, fixed_layout, x_vis)

plot_graph_inference(res_wheel_left, res_wheel_left_inf, 0.5, 7, col_region, fixed_layout, x_wheel)
plot_graph_inference(res_wheel_right, res_wheel_right_inf, 0.5, 7, col_region, fixed_layout, x_wheel)
