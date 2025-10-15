
# Summary_statistics ------------------------------------------------------

# Data

vis_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/3_basis/vis_left.RDS")
vis_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/3_basis/vis_right.RDS")
wheel_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/3_basis/wheel_left.RDS")
wheel_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/3_basis/wheel_right.RDS")


# functions ---------------------------------------------------------------

count_edges = function(beta_matrix, arrow_size, node_size, region_col, keep_regions) {
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
    return(edge_count)
}
no_edgs = function(mat){
  inhib = length(mat[mat==2]) # inhibitory edges  
  excit = length(mat[mat==1]) # excitatory edges
  
  p_in = round((inhib/(inhib+excit))*100, 2)
  p_ex = round((excit/(inhib+excit))*100, 2)
  return(list(inhib= p_in, ex = p_ex, total = inhib+excit))
}
count_within_edges = function(mat, regions, col_region){
  edges = NULL;
  for(i in 1:length(regions)){
    edges[i] = count_edges(mat, arrow_size = 0.5, node_size = 2,
                           region_col = col_region, keep_regions = c(regions[i]))
  }
  names(edges) = regions
  return(edges)
}

col_region <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/col_region.RDS")


# results BAPLA -----------------------------------------------------------

r1 = no_edgs(vis_left); r2 = no_edgs(vis_right)
r3 = no_edgs(wheel_left); r4 = no_edgs(wheel_right)

df = data.frame(inhibitory = c(r1$inhib, r2$inhib, r3$inhib, r4$inhib),
                excitatory = c(r1$ex, r2$ex, r3$ex, r4$ex))
t(df)

regions = c("CA1", "DG" , "GPe", "MB", 
            "MRN", "POL",  "SCm",
            "SCsg","VISl", "VISrl", "VISp" )

w1  = round((sum(count_within_edges(vis_left, regions, col_region)) / r1$total ) *100 ,2)
w2 = round((sum(count_within_edges(vis_right, regions, col_region)) / r2$total ) * 100, 2)
w3 = round((sum(count_within_edges(wheel_left, regions, col_region)) / r3$total ) * 100, 2)
w4 = round((sum(count_within_edges(wheel_right, regions, col_region)) / r4$total ) * 100, 2)

df$within = c(w1, w2, w3, w4)
df$between = c(100-w1, 100-w2, 100-w3, 100-w4)

library(xtable)
xtable(t(df))




# glm results -------------------------------------------------------------

vis_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/glm/vis_left.RDS")
vis_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/glm/vis_right.RDS")
wheel_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/glm/wheel_left.RDS")
wheel_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/glm/wheel_right.RDS")

r1 = no_edgs(vis_left); r2 = no_edgs(vis_right)
r3 = no_edgs(wheel_left); r4 = no_edgs(wheel_right)

df = data.frame(inhibitory = c(r1$inhib, r2$inhib, r3$inhib, r4$inhib),
                excitatory = c(r1$ex, r2$ex, r3$ex, r4$ex))
t(df)

regions = c("CA1", "DG" , "GPe", "MB", 
            "MRN", "POL",  "SCm",
            "SCsg","VISl", "VISrl", "VISp" )

w1  = round((sum(count_within_edges(vis_left, regions, col_region)) / r1$total ) *100 ,2)
w2 = round((sum(count_within_edges(vis_right, regions, col_region)) / r2$total ) * 100, 2)
w3 = round((sum(count_within_edges(wheel_left, regions, col_region)) / r3$total ) * 100, 2)
w4 = round((sum(count_within_edges(wheel_right, regions, col_region)) / r4$total ) * 100, 2)

df$within = c(w1, w2, w3, w4)
df$between = c(100-w1, 100-w2, 100-w3, 100-w4)

library(xtable)
xtable(t(df))
