
# Summary_statistics ------------------------------------------------------

# Data

vis_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/vis_left.RDS")
vis_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/vis_right.RDS")
wheel_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/wheel_left.RDS")
wheel_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/wheel_right.RDS")


# functions ---------------------------------------------------------------

plot_sub_graph = function(beta_matrix, arrow_size, node_size, region_col, keep_regions = c("MB", "POL")) {
  #' Plot interaction matrix for specific brain regions only (e.g., MB and POL)
  #'
  #' @param beta_matrix p x p interaction matrix
  #' @param arrow_size arrow size in igraph plot
  #' @param node_size node size in igraph plot
  #' @param region_col vector of region names for each node
  #' @param keep_regions character vector of regions to retain (e.g., c("MB", "POL"))
  #' @export
  
  # Create full graph
  graph <- graph_from_adjacency_matrix(100 * beta_matrix, mode = "directed", weighted = TRUE, diag = FALSE)
  
  # Set edge and node properties
  V(graph)$region <- region_col
  V(graph)$color <- region_col
  V(graph)$label.color <- "black"
    V(graph)$size <- node_size
    
    E(graph)$color <- ifelse(E(graph)$weight > 0, "black", "black")
    E(graph)$lty <- ifelse(E(graph)$weight < 200 & E(graph)$weight != 0, "solid", "dashed")
    
    # Filter: keep only nodes in MB or POL
    keep_nodes <- which(names(region_col) %in% keep_regions)
    subgraph <- induced_subgraph(graph, keep_nodes)
    
    edge_count <- ecount(subgraph)
    print(paste("Number of edges in the subgraph:", edge_count))
    
    
    # Group by region
    region_labels <- V(subgraph)$region
    region_types <- unique(region_labels)
    n_regions <- length(region_types)
    
    # Position clusters
    angle <- seq(0, 2 * pi, length.out = n_regions + 1)[-1]
    radius <- 25
    offsets <- data.frame(x = cos(angle) * radius, y = sin(angle) * radius)
    
    
    
    layouts <- list()
    for (i in 1:n_regions) {
      region <- region_types[i]
      node_ids <- which(region_labels == region)
      subg_region <- induced_subgraph(subgraph, node_ids)
      l <- layout_with_fr(subg_region, niter = 1000)
      l <- sweep(l, 2, as.numeric(offsets[i, ]), "+")
      layouts[[i]] <- list(nodes = node_ids, layout = l)
    }
    
    # Merge layouts
    full_layout <- matrix(NA, nrow = vcount(subgraph), ncol = 2)
    for (i in 1:n_regions) {
      full_layout[layouts[[i]]$nodes, ] <- layouts[[i]]$layout
    }
    
    
    # Plot only selected regions
    plot(subgraph,
         edge.arrow.size = arrow_size,
         vertex.label = NA,
         edge.curved = 0.2,
         layout = full_layout)
}
no_edgs = function(mat){
  inhib = length(mat[mat==2]) # inhibitory edges  
  excit = length(mat[mat==1]) # excitatory edges
  
  p_in = round((inhib/(inhib+excit))*100, 2)
  p_ex = round((excit/(inhib+excit))*100, 2)
  return(list(inhib= p_in, ex = p_ex))
}


r1 = no_edgs(vis_left); r2 = no_edgs(vis_right)
r3 = no_edgs(wheel_left); r4 = no_edgs(wheel_right)

df = data.frame(inhibitory = c(r1$inhib, r2$inhib, r3$inhib, r4$inhib),
                excitatory = c(r1$ex, r2$ex, r3$ex, r4$ex))


regions = c()

par(mfrow = c(1,1))
plot_graph_MB_POL(mat3, arrow_size = 0.5, node_size = 2,
               region_col = col_region, keep_regions = c("MB"))
