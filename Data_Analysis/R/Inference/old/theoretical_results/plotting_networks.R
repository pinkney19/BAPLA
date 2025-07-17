
# Plot networks with all neurons  -----------------------------------------


# Load data
region_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/wheel/region_ids.RDS")
region_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/region_ids.RDS")

id_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing_wheel/60_trials_idx.RDS")
id_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/60_trials_idx.RDS")

# combine the data
df = data.frame(neuron_id = c(id_wheel, id_vis), region = c(region_wheel, region_vis))
# order by neuron number
df_ordered = df[order(df$neuron_id), ]
# remove rows with duplicate neurons
final_df = df_ordered[!duplicated(df_ordered$neuron_id), ]


id_all <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/neuron_ids_all.RDS")

# sanity check
sort(id_all) == final_df$neuron_id

region_all = final_df$region

# Load connectivity estimates
left_params_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/results/left_params.RDS")
right_params_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/results/right_params.RDS")

left_params_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/left_params.RDS")
right_params_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/right_params.RDS")

beta_left_wheel = left_params_wheel$beta_hat
beta_right_wheel = right_params_wheel$beta_hat

beta_left_vis = left_params_vis$beta_hat
beta_right_vis = right_params_vis$beta_hat

sorted_ids = sort(id_all)

combine_estimates = function(est, sorted_ids, p_est, id_vec){
  #' Function to combine estimates from visual stimulus and wheel experiments
  #' @param est Estimate of the interaction matrix of dimension p_est x p_est
  #' @param sorted_ids neuron ids used in both visual and wheel experiments
  #' @param p_est dimension of the estimated interaction matrix
  #' @param id_vec vector of neuron ids corresponding to the estimated interaction matrix "est"
  
  # set up empty matrix
  network_combined = matrix(data = 0, nrow = length(sorted_ids), ncol=length(sorted_ids))
  
  x = matrix(data = 0, nrow = length(sorted_ids), ncol=length(sorted_ids))
  x[1:p_est, 1:p_est] = est
  rownames(x) = 1:82
  
  cols_in = 1:p_est
  
  flag = sorted_ids %in% id_vec
  
  idx = which(flag == T)
  # target columns for output matrix
  cols_out = idx
  
  for(i in 1:length(sorted_ids)){
    # identify row of matrix corresponding to this neuron in the original estimate 
    # of the interaction matrix
    name = which(id_vec == sorted_ids[i])
    # if neuron is in current estimate of interaction matrix (est)
    if(sorted_ids[i] %in% id_vec){
      row_data = x[name, cols_in]
      network_combined[i,cols_out] = row_data
    }
  }
  
  return(network_combined)
}




res_vis_left = combine_estimates(beta_left_vis, sorted_ids, p_est = 48, id_vec = id_vis)
res_wheel_left = combine_estimates(beta_left_wheel, sorted_ids, p_est = 56, id_vec = id_wheel)

res_vis_right =  combine_estimates(beta_right_vis, sorted_ids, p_est = 48, id_vec = id_vis)
res_wheel_right = combine_estimates(beta_right_wheel, sorted_ids, p_est = 56, id_vec = id_wheel)


# Load inference estimates and combine ------------------------------------

beta_vis_left_inf <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/theoretical_results/beta_vis_left.RDS")
beta_vis_right_inf <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/theoretical_results/beta_vis_right.RDS")
beta_wheel_left_inf <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/theoretical_results/beta_wheel_left.RDS")
beta_wheel_right_inf <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Inference/theoretical_results/beta_wheel_right.RDS")

# combine
res_vis_left_inf = combine_estimates(beta_vis_left_inf, sorted_ids, p_est = 48, id_vec = id_vis)
res_wheel_left_inf = combine_estimates(beta_wheel_left_inf, sorted_ids, p_est = 56, id_vec = id_wheel)

res_vis_right_inf =  combine_estimates(beta_vis_right_inf, sorted_ids, p_est = 48, id_vec = id_vis)
res_wheel_right_inf = combine_estimates(beta_wheel_right_inf, sorted_ids, p_est = 56, id_vec = id_wheel)


# -------------------------------------------------------------------------


mapping <- c(CA1 = 1, DG = 2, GPe = 3, MB = 4,
             MRN = 5, POL = 6,  SCm = 7,
             SCsg = 8,VISl = 9, VISp = 10, VISrl = 11 )
cols = c("#F8766D", "darkorange",  "olivedrab", "#00BA38",  "#00C1A7",  
                  "deepskyblue1", "dodgerblue3",  "mediumorchid1", "lightpink1", "deeppink", "brown2")
                  
map_cols = c(CA1 = cols[1], DG =cols[2], GPe = cols[3], MB = cols[4], MRN = cols[5], POL = cols[6], SCm = cols[7], 
             SCsg = cols[8] , VISl = cols[9], VISp = cols[10], VISrl = cols[11])

# Use the match function to replace the strings with their corresponding numeric values
final_df$region[1:2]
num_region <- mapping[final_df$region]
col_region = map_cols[final_df$region]


# plots -------------------------------------------------------------------

setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
library(igraph)

# convert
convert_mat = function(mat){
  mat[mat>0]=1; mat[mat<0]=2;
  return(mat)
}

res_wheel_left = convert_mat(res_wheel_left)
res_wheel_right = convert_mat(res_wheel_right)
res_vis_left = convert_mat(res_vis_left)
res_vis_right = convert_mat(res_vis_right)


res_wheel_left_inf = convert_mat(res_wheel_left_inf)
res_wheel_right_inf = convert_mat(res_wheel_right_inf)
res_vis_left_inf = convert_mat(res_vis_left_inf)
res_vis_right_inf = convert_mat(res_vis_right_inf)


# neurons to shade in visual plots
x_vis = which(final_df$neuron_id %in% id_vis==F)
# neurons to shade in wheel plots
x_wheel = which(final_df$neuron_id %in% id_wheel==F)

fixed_layout <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/combined_plots/fr/layout.RDS")

par(mfrow=c(1,2), mar = c(0,0,0,0))
set.seed(123)
plot_graph_new(res_vis_left, 0.5, 7, col_region, fixed_layout, x_wheel)
legend("topleft", legend = names(map_cols), col = cols,   
       pch = 19, bty = "n", cex = 1.2)
set.seed(123)
plot_graph_new(res_vis_left_inf, 0.5, 7, col_region, fixed_layout, x_wheel)

