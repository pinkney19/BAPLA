res_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/res_left.RDS")
left_params <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/left_params.RDS")
beta_hat= left_params$beta_hat

inference_results = function(res, beta_hat){
  non_zero_indices = list(); mat = matrix(NA, nrow=48, ncol=48);
  for(i in 1:length(res)){
    check_0 = ifelse(res[[i]]$ci_lower < 0 & res[[i]]$ci_upper>0, T, F)
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
  return(ids)
}
  
  
inference_results(res_left, beta_hat)

right_params <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/right_params.RDS")


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
