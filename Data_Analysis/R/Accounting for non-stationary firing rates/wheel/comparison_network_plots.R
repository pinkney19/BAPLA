setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
library(igraph)

# Load required data ------------------------------------------------------


# Load data
region_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/wheel/region_ids.RDS")
region_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/region_ids.RDS")
id_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing_wheel/60_trials_idx.RDS")
id_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing/60_trials_idx.RDS")



# Load connectivity estimates

# left
left_params_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/no_f/results/left_params.RDS")
left_params_f <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/results/left_params.RDS")
# right
right_params_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/no_f/results/right_params.RDS")
right_params_f <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/results/right_params.RDS")

# visual 
#left 
left_params_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/no_f/results/left_params.RDS")
right_params_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/no_f/results/right_params.RDS")

left_params_f_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/left_params.RDS")
right_params_f_vis <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/right_params.RDS")


# get location of stationary and non-stationary neurons

id_non_stat_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Accounting for non-stationary firing rates/wheel/left/id_non_stationary.RDS")
id_non_stat_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Accounting for non-stationary firing rates/wheel/right/id_non_stationary.RDS")

id_stat_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Accounting for non-stationary firing rates/wheel/left/id_stationary.RDS")
id_stat_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Accounting for non-stationary firing rates/wheel/right/id_stationary.RDS")

# load combined connectivity estimates
wheel_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/glm/wheel_left.RDS")
wheel_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/glm/wheel_right.RDS")
vis_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/vis_left.RDS")
vis_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/vis_right.RDS")


# layout for full graph
full_layout <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/combined_plots/fr/layout.RDS")
# colours fo full graph
col_region_all <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/col_region.RDS")


# IDs of all neurons
id_all <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/neuron_ids_all.RDS")
sorted_ids = sort(id_all)

# start -------------------------------------------------------------------

plot_comparison_f_no_f = function(dataset_type, trial_type, interaction_matrix, comparison_matrix, 
                                  sorted_ids, id_vis, id_wheel ,col_region_all, combined_estimate){
  
  #' description
  #' @param dataset_type ("wheel" or "vis")
  #' @param trial_type ("left" or "right")
  #' @param df (dataframe) with neuron IDs and regions 
  #' @param interaction matrix d x d interaction matrix (usually glm interaction matrix)
  #' @param comparison_matrix d x d comparison marix (usually BAPLA interaction matrix)
  #' @param sorted_ids 1 x 82 numeric vector of neuron IDs
  #' @param id_vis 1 x 48 numeric vector of neurons in stimulus onset data
  #' @param id_wheel 1 x 56 numeric vector of neurons in wheel movement dataset
  #' @param col_region_all vector of colours for whole network plot
  #' 
  #' 

  df = data.frame(neuron_id = c(get(paste0("id_",dataset_type))), region = c(get(paste0("region_",dataset_type))))
  
  # identify statinoary and non-stationary neurons
  
  stat_neurons = get(paste0("id_",dataset_type))[get(paste0("id_stat_", trial_type))]
  non_stat_neurons = get(paste0("id_",dataset_type))[get(paste0("id_non_stat_", trial_type))]

  for (i in 1:dim(interaction_matrix)[1]) {
    if (df$neuron_id[i] %in% stat_neurons) {
      df$type[i] <- "S" # stationary
    } else {
      df$type[i] <- "NS" # non-stationary
    }
  }

  mapping <- c(S = 1, NS = 2 )
  cols = c( "black", "white")
                    
  map_cols = c(S = cols[1], NS =cols[2])
  
  num_region <- mapping[df$type]
  col_region = map_cols[df$type]
  
  
  mat1 = interaction_matrix
  mat2 = comparison_matrix
  
  
  mat1[mat1>0] = 1; mat1[mat1<0]=2; # convert for plotting purposes
  mat2[mat2>0] = 1; mat2[mat2<0]=2;

  # get fixed layout for bipartite graphs
  set.seed(123)
  fixed_layout = get_fixed_layout_bi(mat1, col_region, F, 7)

  # plot full network
  # neurons to shade in visual plots
  x_vis = which(sorted_ids %in% id_vis==F)
  # neurons to shade in wheel plots
  x_wheel = which(sorted_ids %in% id_wheel==F)
  
  
  
  par(mfrow = c(1,3), mar = c(0,0,0,0))
  set.seed(123)
  plot_graph_new(combined_estimate, 0.5, 7, col_region_all, full_layout, get(paste0("x_",dataset_type)))
  legend("topleft", legend = unique(names(col_region_all)), 
         col = unique((col_region_all)),   
         pch = 19, bty = "n", cex = 1.2)
  plot_bipartite_compar(mat1, mat2, 0.3, 7, col_region, fixed_layout)
  plot_bipartite_compar(mat2, mat1, 0.3, 7, col_region, fixed_layout)
  
}

plot_comparison_f_no_f("wheel", "left", interaction_matrix = left_params_wheel$beta_hat, 
                       comparison_matrix = left_params_f$beta_hat,sorted_ids, id_vis, id_wheel,
                       col_region_all, combined_estimate = wheel_left)


plot_comparison_f_no_f("wheel", "right", interaction_matrix = right_params_wheel$beta_hat, 
                       comparison_matrix = right_params_f$beta_hat,sorted_ids, id_vis, id_wheel,
                       col_region_all, combined_estimate = wheel_right)


plot_comparison_f_no_f("vis", "left", interaction_matrix = left_params_vis$beta_hat, 
                       comparison_matrix = left_params_f_vis$beta_hat ,sorted_ids, id_vis, id_wheel,
                       col_region_all, combined_estimate = vis_left)

plot_comparison_f_no_f("vis", "right", interaction_matrix = right_params_vis$beta_hat, 
                       comparison_matrix = right_params_f_vis$beta_hat ,sorted_ids, id_vis, id_wheel,
                       col_region_all, combined_estimate = vis_right)





# Check edges -------------------------------------------------------------

# 
# count_within_edges(mat1, unique(names(col_region)), col_region)
# count_within_edges(mat2, unique(names(col_region)), col_region)
# 
# 
# w1  = round((sum(count_within_edges(mat1, unique(names(col_region)), col_region)) / 80 ) *100 ,2)
# w2 = round((sum(count_within_edges(mat2, unique(names(col_region)), col_region)) / 38 ) * 100, 2)
# 
# df2 = data.frame(within =c(w1,w2))
# 
# df2$between = c(100-w1, 100-w2)
# df2
# 
# 
# # checks ------------------------------------------------------------------
# 
# plot_bipartite_compar_check <- function(beta_matrix, comparison_matrix, arrow_size, node_size, region_col, fixed_layout) {
#   # Create a graph from beta_matrix
#   graph <- graph_from_adjacency_matrix(100 * beta_matrix, mode = "directed", weighted = TRUE, diag = FALSE)
#   E(graph)$color <- ifelse(E(graph)$weight > 0, "black", "black")
#   E(graph)$lty <- ifelse(E(graph)$weight < 200 & E(graph)$weight != 0, "solid", "dashed")
#   
#   # Assign node types based on region_col
#   V(graph)$type <- ifelse(region_col == "black", "black", "white")
#   
#   # Create a graph from comparison_matrix
#   comparison_graph <- graph_from_adjacency_matrix(100 * comparison_matrix, mode = "directed", weighted = TRUE, diag = FALSE)
#   
#   # Identify edges in beta_matrix but not in comparison_matrix
#   edges_beta <- as_edgelist(graph)
#   edges_comparison <- as_edgelist(comparison_graph)
#   unique_edges <- edges_beta[!apply(edges_beta, 1, function(edge) {
#     any(apply(edges_comparison, 1, function(comp_edge) all(comp_edge == edge)))
#   }), ]
#   
#   # Filter edges between black and white nodes
#   edges_bw <- unique_edges[apply(unique_edges, 1, function(edge) {
#     (V(graph)[edge[1]]$type == "black" && V(graph)[edge[2]]$type == "white") ||
#       (V(graph)[edge[1]]$type == "white" && V(graph)[edge[2]]$type == "black")
#   }), ]
#   
#   # Count edges and determine their types
#   bw_edges_count <- nrow(edges_bw)
#   edge_types <- ifelse(E(graph)[edges_bw]$weight < 200 & E(graph)[edges_bw]$weight != 0, "solid", "dashed")
#   
#   print(paste("Number of edges between black and white nodes:", bw_edges_count))
#   print(paste("Edge types:", paste(edge_types, collapse = ", ")))
#   
#   # Plot the graph
#   V(graph)$color <- region_col
#   V(graph)$label.color <- "black"
#     V(graph)$size <- node_size
#     plot(graph, edge.arrow.size = arrow_size, vertex.label = NA, edge.curved = 0.2, layout = fixed_layout)
#     return(edge_types)
# }
# 
# 
# plot_bipartite_compar_check(mat1, mat2, 0.3, 7, col_region, fixed_layout)
