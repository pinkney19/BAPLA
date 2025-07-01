
# Plot networks with all neurons  -----------------------------------------


# Load data
region_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/wheel/region_ids.RDS")
region_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/region_ids.RDS")

id_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing_wheel/60_trials_idx.RDS")
id_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/60_trials_idx.RDS")


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
  # populate with original matrix
  network_combined[1:p_est, 1:p_est] = est
  # re-order the rows to align with sorted_id ordering
  # set-up empty matrix to store results
  network_combined_ordered = matrix(data = 0, nrow = length(sorted_ids), ncol=length(sorted_ids))
  idx = NULL;
  for(i in 1:length(sorted_ids)){
    # if neuron is in current estimate of interaction matrix (est)
    if(sorted_ids[i] %in% id_vec){
      # identify row of matrix corresponding to this neuron in the original estimate of the interaction matrix
      idx[i] = which(id_vec == sorted_ids[i])
      # populate combined network with "old" estimated interaction matrix
      network_combined_ordered[i,] = network_combined[idx[i],]
    }
  }
  return(list(mat = network_combined_ordered, idx = idx))
}
  

res_vis_left = combine_estimates(beta_left_vis, sorted_ids, p_est = 48, id_vec = id_vis)
res_wheel_left = combine_estimates(beta_left_wheel, sorted_ids, p_est = 56, id_vec = id_wheel)

res_vis_right =  combine_estimates(beta_right_vis, sorted_ids, p_est = 48, id_vec = id_vis)
res_wheel_right = combine_estimates(beta_right_wheel, sorted_ids, p_est = 56, id_vec = id_wheel)

sanity_check = function(idx, network){
  to_check = is.na(idx)
  tc = which(to_check==T); count=NULL;
  for(i in 1:length(tc)){
    count[i] = sum(network[tc[i],])
  }
  return(sum(count))
}


sanity_check(res_vis_left$idx, res_vis_left$mat)
sanity_check(res_vis_right$idx, res_vis_right$mat)
sanity_check(res_wheel_left$idx, res_wheel_left$mat)
sanity_check(res_wheel_right$idx, res_wheel_right$mat)


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

setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
library(igraph)
par(mar = c(0,0,0,0), mfrow = c(1,2), xpd = NA)
plot_graph(res_vis_left$mat, 0.3, 7, col_region)
plot_graph(res_vis_right$mat, 0.3,7,col_region)

plot_graph(res_wheel_left$mat, 0.3, 7, col_region)
plot_graph(res_wheel_right$mat, 0.3,7,col_region)




mat1 = res_wheel_left$mat
mat2 = res_wheel_right$mat
mat3 = res_vis_left$mat
mat4 = res_vis_right$mat

mat1[mat1>0] = 1; mat1[mat1<0]=2;
mat2[mat2>0] = 1; mat2[mat2<0]=2;
mat3[mat3>0] = 1; mat3[mat3<0]=2;
mat4[mat4>0] = 1; mat4[mat4<0]=2;

plot_graph_new(mat1, 0.3, 7, col_region)
plot_graph_new(mat2, 0.3, 7, col_region)


plot_graph_new(mat3, 0.3, 7, col_region)
plot_graph_new(mat4, 0.3, 7, col_region)
# for(i in 1:length(sorted_ids)){
#   # only populate new matrix if the neuron is in the visual data/results
#   if(sorted_ids[i] %in% id_vis){
#     # get "location"/matrix row of this neuron in the old network
#     idx = which(id_vis == sorted_ids[i])
#     # populate new network with "old" network data
#     network_left_vis[i,] = beta_left_vis[idx,]
#   }
#   
# }
