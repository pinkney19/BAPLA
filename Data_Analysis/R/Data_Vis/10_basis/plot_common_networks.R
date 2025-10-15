
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
left_params_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/10_basis/results/left_params.RDS")
right_params_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/10_basis/results/right_params.RDS")

left_params_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/10_basis/results/left_params.RDS")
right_params_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/10_basis/results/right_params.RDS")

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
  
  
  
#   # populate with original matrix
#   network_combined[1:p_est, 1:p_est] = est
#   # re-order the rows to align with sorted_id ordering
#   # set-up empty matrix to store results
#   network_combined_ordered = matrix(data = 0, nrow = length(sorted_ids), ncol=length(sorted_ids))
#   idx = NULL;
#   for(i in 1:length(sorted_ids)){
#     # if neuron is in current estimate of interaction matrix (est)
#     if(sorted_ids[i] %in% id_vec){
#       # identify row of matrix corresponding to this neuron in the original estimate of the interaction matrix
#       idx[i] = which(id_vec == sorted_ids[i])
#       # populate combined network with "old" estimated interaction matrix
#       network_combined_ordered[i,] = network_combined[idx[i],]
#     }
#   }
#   return(list(mat = network_combined_ordered, idx = idx))
# }
#   

res_vis_left = combine_estimates(beta_left_vis, sorted_ids, p_est = 48, id_vec = id_vis)
res_wheel_left = combine_estimates(beta_left_wheel, sorted_ids, p_est = 56, id_vec = id_wheel)

res_vis_right =  combine_estimates(beta_right_vis, sorted_ids, p_est = 48, id_vec = id_vis)
res_wheel_right = combine_estimates(beta_right_wheel, sorted_ids, p_est = 56, id_vec = id_wheel)

# sanity_check = function(idx, network){
#   to_check = is.na(idx)
#   tc = which(to_check==T); count=NULL;
#   for(i in 1:length(tc)){
#     count[i] = sum(network[tc[i],])
#   }
#   return(sum(count))
# }
# 
# 
# sanity_check(res_vis_left$idx, res_vis_left$mat)
# sanity_check(res_vis_right$idx, res_vis_right$mat)
# sanity_check(res_wheel_left$idx, res_wheel_left$mat)
# sanity_check(res_wheel_right$idx, res_wheel_right$mat)


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
setwd("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats")
saveRDS(col_region, "col_region.RDS")


setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
library(igraph)
par(mar = c(1,1,1,1), mfrow = c(1,2), xpd = NA)
plot_graph(res_vis_left, 0.3, 7, col_region)
plot_graph(res_vis_right, 0.3,7,col_region)

plot_graph(res_wheel_left, 0.3, 7, col_region)
plot_graph(res_wheel_right, 0.3,7,col_region)




mat1 = res_wheel_left
mat2 = res_wheel_right
mat3 = res_vis_left
mat4 = res_vis_right

mat1[mat1>0] = 1; mat1[mat1<0]=2;
mat2[mat2>0] = 1; mat2[mat2<0]=2;
mat3[mat3>0] = 1; mat3[mat3<0]=2;
mat4[mat4>0] = 1; mat4[mat4<0]=2;

# save data for summary stats
setwd("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/10_basis")
saveRDS(mat1, "wheel_left.RDS")
saveRDS(mat2, "wheel_right.RDS")
saveRDS(mat3, "vis_left.RDS")
saveRDS(mat4, "vis_right.RDS")


# neurons to shade in visual plots
x_vis = which(final_df$neuron_id %in% id_vis==F)
# neurons to shade in wheel plots
x_wheel = which(final_df$neuron_id %in% id_wheel==F)


# layout
#set.seed(123)
#fixed_layout = get_fixed_layout_all(mat1, col_region, 7)
#setwd("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/combined_plots/fr/3_basis")
#saveRDS(fixed_layout, "layout.RDS")
fixed_layout <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/combined_plots/fr/layout.RDS")
par(mfrow=c(1,2), mar = c(0,0,0,0))
set.seed(123)
#legend("top", legend = names(map_cols), col = cols,   
 #      pch = 19, bty = "n", cex = 1.5)

plot_graph_new(mat1, 0.5, 7, col_region, fixed_layout, x_wheel)
legend("topleft", legend = names(map_cols), col = cols,   
       pch = 19, bty = "n", cex = 1.2)
set.seed(123)
plot_graph_new(mat2, 0.5, 7, col_region, fixed_layout, x_wheel)
#legend("topleft", legend = names(map_cols), col = cols,   
       #pch = 19, bty = "n", cex = 0.7)

#fixed_layout = get_fixed_layout_all(mat3, col_region, 7)
set.seed(123)
plot_graph_new(mat3, 0.5, 7, col_region, fixed_layout, x_vis)
legend("topleft", legend = names(map_cols), col = cols,   
       pch = 19, bty = "n", cex = 1.2)


set.seed(123)
plot_graph_new(mat4, 0.5, 7, col_region,fixed_layout, x_vis)


# checks
sanity_check= function(x_id, res){
  count=NULL;
  for(i in 1:length(x_id)){
    count[i] = sum(res[x_id[i],])
  }
  return(count)
}

sanity_check(x_wheel, res_wheel_left)
sanity_check(x_wheel, res_wheel_left)
sanity_check(x_vis, res_vis_left)
sanity_check(x_vis, res_vis_left)

# check non-zero entries are due to overlap:
df$neuron_id[which(sanity_check(x_vis, res_vis_left)!=0)] %in% id_vis
df$neuron_id[which(sanity_check(x_wheel, res_wheel_left)!=0)] %in% id_wheel



# sumary stats
no_edgs = function(mat){
  inhib = length(mat[mat==2]) # inhibitory edges  
  excit = length(mat[mat==1]) # excitatory edges
  return(list(inhib= inhib, ex = excit))
}

no_edgs(mat1)
no_edgs(mat2)
no_edgs(mat3)
no_edgs(mat4)

#setwd("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/combined_plots/f_v_no_f")
#saveRDS(mat1, "wheel_left_f.RDS")

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


set.seed(123)
p1 = plot_graph_new_ggplot(mat1, arrow_size = 0.1, node_size = 6, col_region)
set.seed(123)
p2 = plot_graph_new_ggplot(mat2, arrow_size = 0.1, node_size = 6, col_region)
set.seed(123)
p3 = plot_graph_new_ggplot(mat3, arrow_size = 0.1, node_size = 6, col_region)
set.seed(123)
p4 = plot_graph_new_ggplot(mat4, arrow_size = 0.1, node_size = 6, col_region)


plot_wheel = ggarrange(p1,p2, ncol=2, common.legend = T, legend="top")
plot_wheel
plot_vis = ggarrange(p3,p4, ncol=2, common.legend = T, legend="left")
plot_vis


ggarrange(p1,p2,p3,p4,ncol=4, common.legend = T)



