#res_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/res_left.RDS")

vis_left_desparsified <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/vis/storm_results/vis_left_desparsified.RDS")
left_params <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/left_params.RDS")
beta_hat_vis_left= left_params$beta_hat

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
  return(list(ids=ids, mat=mat, beta_hat = beta_hat, n_inds= non_zero_indices))
}
  


out1 = inference_results(vis_left_desparsified, beta_hat_vis_left)

length(out1$ids) # number spurious edges
out1$ids
# vis_right_desparsified <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/vis/storm_results/vis_right_desparsified.RDS")
# right_params <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/right_params.RDS")
# 
# beta_hat2 = right_params$beta_hat
# 
# inference_results(vis_right_desparsified, beta_hat2)
# non_zero_indices = list(); mat = matrix(NA, nrow=48, ncol=48);
# for(i in 1:length(res_left)){
#   check_0 = ifelse(res_left[[i]]$ci_lower < 0 & res_left[[i]]$ci_upper>0, T, F)
#   non_zero_indices[[i]] = which(check_0==F)
#   if(length(non_zero_indices[[i]])!=0){
#     # set non zero entries to 1
#     mat[i, non_zero_indices[[i]]] = 1
#   }
# }
# check = beta_hat
# check[check!=0]=1
# 
# 
# mat[is.na(mat)]=0
# out = mat == check
# which(out == F) # edges to be  gregre

wheel_left_desparsified <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/wheel/storm_results/wheel_left_desparsified.RDS")
left_params <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/results/left_params.RDS")
beta_hat= left_params$beta_hat
out2 = inference_results(wheel_left_desparsified, beta_hat)
length(out2$ids) # 8
image(out2$mat)
image(out2$beta_hat)
out2$ids



# theoretical lambda ------------------------------------------------------

vis_left_desparsified2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/vis/storm_results/old/vis_left_desparsified.RDS")
out3 = inference_results(vis_left_desparsified2, beta_hat_vis_left)
out3$ids

# right
right_params <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/right_params.RDS")
beta_hat_vis_right= right_params$beta_hat
vis_right_desparsified2 <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/vis/storm_results/old/vis_right_desparsified.RDS")
out4 = inference_results(vis_right_desparsified2, beta_hat_vis_right)
out4$ids
