
# Plot networks with all neurons  -----------------------------------------


# Load data
region_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/wheel/region_ids.RDS")

id_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data/Pre_Processing_wheel/60_trials_idx.RDS")

# combine the data
df = data.frame(neuron_id = c(id_wheel), region = c(region_wheel))
# order by neuron number
df_ordered = df[order(df$neuron_id), ]
# remove rows with duplicate neurons
final_df = df_ordered[!duplicated(df_ordered$neuron_id), ]


region_all = final_df$region

# Load connectivity estimates
left_params_wheel <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/no_f/results/left_params.RDS")

beta_left_wheel = left_params_wheel$beta_hat

# get location of stationary and non-stationary neurons
id_non_stat <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/combined_plots/f_v_no_f/id_non_stationary.RDS")
id_stat <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/combined_plots/f_v_no_f/id_stationary.RDS")

stat_neurons = id_wheel[id_stat]
non_stat_neurons = id_wheel[id_non_stat]

for (i in 1:56) {
  if (df$neuron_id[i] %in% stat_neurons) {
    df$type[i] <- "S"
  } else {
    df$type[i] <- "NS"
  }
}

mapping <- c(S = 1, NS = 2 )
cols = c( "black", "white")
                  
map_cols = c(S = cols[1], NS =cols[2])

num_region <- mapping[df$type]
col_region = map_cols[df$type]

setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
library(igraph)





mat1 = beta_left_wheel


mat1[mat1>0] = 1; mat1[mat1<0]=2;


# par(mfrow=c(1,1), mar = c(0,0,0,0))
# set.seed(123)
# #legend("top", legend = names(map_cols), col = cols,
#  #      pch = 19, bty = "n", cex = 1.5)
# 
# plot_graph_new(mat1, 0.3, 7, col_region)
# legend("topleft", legend = names(map_cols), col = cols,
#        pch = 19, bty = "n", cex = 0.7)
# 
# par(mfrow = c(1,2))
# 
# set.seed(123)
# # plot only non-stationary edges
# plot_graph_MB_POL(mat1, arrow_size = 0.5, node_size = 5,
#                                      region_col = col_region, keep_regions = c("NS"))


# add in with f estimate

left_params_f <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/results/left_params.RDS")

mat2 = left_params_f$beta_hat
mat2[mat2>0] = 1; mat2[mat2<0]=2;
set.seed(123)
# plot_graph_MB_POL(mat2, arrow_size = 0.5, node_size = 5,
#                   region_col = col_region, keep_regions = c("NS"))
# 
# # compare full graphs
# 
# set.seed(123)
# #legend("top", legend = names(map_cols), col = cols,   
# #      pch = 19, bty = "n", cex = 1.5)
# par(mfrow = c(1,2))
# plot_graph_new(mat1, 0.3, 7, col_region)
# legend("topleft", legend = names(map_cols), col = cols,   
#        pch = 19, bty = "n", cex = 0.7)
# set.seed(123)
# plot_graph_new(mat2, 0.3, 7, col_region)


# set layout --------------------------------------------------------------


# get_fixed_layout_orig = function(beta_matrix,region_col ){
#   ref_graph <- graph_from_adjacency_matrix(100*beta_matrix, 
#                                            mode = "directed", weighted = T, diag = F)
#   
#   V(ref_graph)$color <- region_col
#   region_col = col_region
#   # Set the layout based on region
#   n_clusters = length(region_col)
#   #match colours to groups
#   group_map = match(V(ref_graph)$color, region_col)
#   
#   
#   # Prepare layout list per cluster
#   layouts <- list()
#   #offsets <- expand.grid(x= seq(0, n_clusters)*0.6, y=0)  # spread vertically (adjust as needed)
#   
#   # Define 2D positions for each cluster (grid or radial layout)
#   angle <- seq(0, 2 * pi, length.out = n_clusters + 1)[-1]  # spread in a circle
#   radius <- 25  # distance between cluster centers
#   offsets <- data.frame(x = cos(angle) * radius, y = sin(angle) * radius)
#   
#   
#   for (i in 1:n_clusters) {
#     nodes_in_group <- which(group_map == i)
#     subg <- induced_subgraph(ref_graph, nodes_in_group)
#     l <- layout_in_circle(subg)#,  niter = 2000)
#     
#     # Normalize to avoid overlap within cluster
#     #l <- norm_coords(l, xmin = -5, xmax = 5, ymin = -5, ymax = 5)
#     
#     
#     # Offset the circle layout so clusters don't overlap
#     offset <- as.numeric(offsets[i, ])
#     l <- sweep(l, 2, offset, "+")
#     
#     layouts[[i]] <- list(nodes = nodes_in_group, layout = l)
#   }
#   
#   # Combine all layouts into one full layout
#   full_layout <- matrix(NA, nrow = vcount(ref_graph), ncol = 2)
#   for (i in 1:n_clusters) {
#     full_layout[layouts[[i]]$nodes, ] <- layouts[[i]]$layout
#   }
#   
#   return(full_layout)
# }
# 
# fixed_layout_orig = get_fixed_layout_orig(mat1, col_region)
# 
# plot_graph_new(mat1, 0.3, 7, col_region, fixed_layout_orig)
# 
# # sumary stats
# no_edgs = function(mat){
#   inhib = length(mat[mat==2]) # inhibitory edges  
#   excit = length(mat[mat==1]) # excitatory edges
#   return(list(inhib= inhib, ex = excit))
# }
# 
# no_edgs(mat1)
# no_edgs(mat2)
# no_edgs(mat3)
# no_edgs(mat4)

get_fixed_layout = function(beta_matrix, region_col, split_nodes, node_size){
  graph <- graph_from_adjacency_matrix(100*beta_matrix, mode = "directed", weighted = T, diag = F)
  # Set edge colors based on the sign of the matrix values
  E(graph)$color <- ifelse(E(graph)$weight > 0, "black", "black")
  
  # Set edge type based on sign of the matrix values
  E(graph)$lty <- ifelse(E(graph)$weight <200 & E(graph)$weight!=0 , "solid", "dashed")
  
  
  # Set vertex color to white
  V(graph)$color <- region_col
  # Set vertex label color to black
  V(graph)$label.color <- "black"
    V(graph)$size <- node_size
    
    V(graph)$type = unname(region_col)
    
    V(graph)$type = ifelse(V(graph)$type=="black", 1, 0)
    
    if(split_nodes == T){
      types = V(graph)$type 
      # split into groups
      group0 = which(types==1)
      group1 = which(types == 0)
      
      # For each group, split into two halves
      group0_split <- split(group0, rep(1:2, length.out = length(group0)))
      group1_split <- split(group1, rep(1:2, length.out = length(group1)))
      
      # Define positions
      layout <- matrix(NA, nrow = vcount(graph), ncol = 2)
      
      # Assign x positions evenly spaced within each row
      layout[group0_split[[1]], 1] <- seq(1, length(group0_split[[1]]))
      layout[group0_split[[2]], 1] <- seq(1, length(group0_split[[2]]))
      layout[group1_split[[1]], 1] <- seq(1, length(group1_split[[1]]))
      layout[group1_split[[2]], 1] <- seq(1, length(group1_split[[2]]))
      
      # Assign y positions for rows
      layout[group0_split[[1]], 2] <- 0     # bottom row 1
      layout[group0_split[[2]], 2] <- 1     # bottom row 2
      layout[group1_split[[1]], 2] <- 3     # top row 1
      layout[group1_split[[2]], 2] <- 4     # top row 2
    }
    
    if(split_nodes==F){
      # layout as normal "bipartite"
      layout = layout_as_bipartite(graph)
    }
    return(layout)
}
set.seed(123)
fixed_layout = get_fixed_layout(mat1, col_region, F, 7)
plot_bipartite = function(beta_matrix, arrow_size, node_size, region_col, fixed_layout){
  # Create a graph from the matrix
  graph <- graph_from_adjacency_matrix(100*beta_matrix, mode = "directed", weighted = T, diag = F)
  # Set edge colors based on the sign of the matrix values
  E(graph)$color <- ifelse(E(graph)$weight > 0, "black", "black")
  
  # Set edge type based on sign of the matrix values
  E(graph)$lty <- ifelse(E(graph)$weight <200 & E(graph)$weight!=0 , "solid", "dashed")
  
  
  # Set vertex color to white
  V(graph)$color <- region_col
  # Set vertex label color to black
  V(graph)$label.color <- "black"
  V(graph)$size <- node_size
  
  V(graph)$type = unname(region_col)
  
  V(graph)$type = ifelse(V(graph)$type=="black", 1, 0)
  

    
  edge_count <- ecount(graph)
  print(paste("Number of edges in the graph:", edge_count))
  
  V(graph)$color = unname(region_col)
  
  
  plot(graph, edge.arrow.size = arrow_size,
       vertex.label = NA,  edge.curved=0.2, layout = fixed_layout, 
       vertex.label.cex = 0.7)
  }
# par(mfrow = c(1,2), mar=c(0,0,0,0))  
# plot_bipartite(mat1, 0.3, 7, col_region, fixed_layout)
# plot_bipartite(mat2, 0.3, 7, col_region, fixed_layout)
# 
# 
# set.seed(123)
# fixed_layout = get_fixed_layout(mat1, col_region, T, 7)
# plot_bipartite(mat1, 0.3, 7, col_region, fixed_layout)
# plot_bipartite(mat2, 0.3, 7, col_region, fixed_layout)



# Check edges -------------------------------------------------------------
plot_bipartite_compar = function(beta_matrix, comparison_matrix,  arrow_size, node_size, region_col, fixed_layout){
  # Create a graph from the matrix
  graph <- graph_from_adjacency_matrix(100*beta_matrix, mode = "directed", weighted = T, diag = F)
  # Set edge colors based on the sign of the matrix values
  E(graph)$color <- ifelse(E(graph)$weight > 0, "black", "black")
  
  # Set edge type based on sign of the matrix values
  E(graph)$lty <- ifelse(E(graph)$weight <200 & E(graph)$weight!=0 , "solid", "dashed")
  
  
  # Create a graph from the comparison_matrix
  comparison_graph <- graph_from_adjacency_matrix(100 * comparison_matrix, mode = "directed", weighted = TRUE, diag = FALSE)
  
  # Identify edges in both graphs
  edges_beta <- as_edgelist(graph)
  edges_comparison <- as_edgelist(comparison_graph)
  
  # Count matching edges
  matching_edges_count <- sum(apply(edges_beta, 1, function(edge) {
    any(apply(edges_comparison, 1, function(comp_edge) all(comp_edge == edge)))
  }))
  
  # Set edge colors: blue for edges in both graphs, black otherwise
  E(graph)$color <- apply(edges_beta, 1, function(edge) {
    if (any(apply(edges_comparison, 1, function(comp_edge) all(comp_edge == edge)))) {
    "blue2"
    } else {
      "black"
    }
  })
  
  
  
  # Set vertex color to white
  V(graph)$color <- region_col
  # Set vertex label color to black
  V(graph)$label.color <- "black"
    V(graph)$size <- node_size
    
    V(graph)$type = unname(region_col)
    
    V(graph)$type = ifelse(V(graph)$type=="black", 1, 0)
    
    
    
    edge_count <- ecount(graph)
    print(paste("Number of edges in the graph:", edge_count))
    print(paste("Number of matching edges:", matching_edges_count))
    
    V(graph)$color = unname(region_col)
    
    
    plot(graph, edge.arrow.size = arrow_size,
         vertex.label = NA,  edge.curved=0.2, layout = fixed_layout, 
         vertex.label.cex = 0.7)
}


plot_bipartite_compar(mat1, mat2, 0.3, 7, col_region, fixed_layout)
plot_bipartite_compar(mat2, mat1, 0.3, 7, col_region, fixed_layout)

count_within_edges(mat1, unique(names(col_region)), col_region)
count_within_edges(mat2, unique(names(col_region)), col_region)


w1  = round((sum(count_within_edges(mat1, unique(names(col_region)), col_region)) / 80 ) *100 ,2)
w2 = round((sum(count_within_edges(mat2, unique(names(col_region)), col_region)) / 38 ) * 100, 2)

df2 = data.frame(within =c(w1,w2))

df2$between = c(100-w1, 100-w2)
df2


# checks ------------------------------------------------------------------

plot_bipartite_compar_check <- function(beta_matrix, comparison_matrix, arrow_size, node_size, region_col, fixed_layout) {
  # Create a graph from beta_matrix
  graph <- graph_from_adjacency_matrix(100 * beta_matrix, mode = "directed", weighted = TRUE, diag = FALSE)
  E(graph)$color <- ifelse(E(graph)$weight > 0, "black", "black")
  E(graph)$lty <- ifelse(E(graph)$weight < 200 & E(graph)$weight != 0, "solid", "dashed")
  
  # Assign node types based on region_col
  V(graph)$type <- ifelse(region_col == "black", "black", "white")
  
  # Create a graph from comparison_matrix
  comparison_graph <- graph_from_adjacency_matrix(100 * comparison_matrix, mode = "directed", weighted = TRUE, diag = FALSE)
  
  # Identify edges in beta_matrix but not in comparison_matrix
  edges_beta <- as_edgelist(graph)
  edges_comparison <- as_edgelist(comparison_graph)
  unique_edges <- edges_beta[!apply(edges_beta, 1, function(edge) {
    any(apply(edges_comparison, 1, function(comp_edge) all(comp_edge == edge)))
  }), ]
  
  # Filter edges between black and white nodes
  edges_bw <- unique_edges[apply(unique_edges, 1, function(edge) {
    (V(graph)[edge[1]]$type == "black" && V(graph)[edge[2]]$type == "white") ||
      (V(graph)[edge[1]]$type == "white" && V(graph)[edge[2]]$type == "black")
  }), ]
  
  # Count edges and determine their types
  bw_edges_count <- nrow(edges_bw)
  edge_types <- ifelse(E(graph)[edges_bw]$weight < 200 & E(graph)[edges_bw]$weight != 0, "solid", "dashed")
  
  print(paste("Number of edges between black and white nodes:", bw_edges_count))
  print(paste("Edge types:", paste(edge_types, collapse = ", ")))
  
  # Plot the graph
  V(graph)$color <- region_col
  V(graph)$label.color <- "black"
    V(graph)$size <- node_size
    plot(graph, edge.arrow.size = arrow_size, vertex.label = NA, edge.curved = 0.2, layout = fixed_layout)
    return(edge_types)
}


plot_bipartite_compar_check(mat1, mat2, 0.3, 7, col_region, fixed_layout)
