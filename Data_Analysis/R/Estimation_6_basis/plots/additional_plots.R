left_params <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/left_params.RDS")
right_params <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results/right_params.RDS")


# f plots -----------------------------------------------------------------

f_left = left_params$fs
f_right = right_params$fs


intercept_left = left_params$beta_0
intercept_right = right_params$beta_0

calc_firing_rate = function(f, intercept){
  fir_rate = list();
  for(i in 1:length(f)){
    fir_rate[[i]] = f[[i]] + intercept[[i]] 
  }  
  rate = lapply(fir_rate, function(x) exp(x)/(1+exp(x))*1000 )
  return(rate)
}

fire_right = calc_firing_rate(f_right, intercept_right)
fire_left = calc_firing_rate(f_left, intercept_left)

setwd("~/Downloads/BAPLA/Data_Analysis/R/Estimation_no_split/results")
saveRDS(fire_right, "all_right.RDS")
saveRDS(fire_left, "all_left.Rds")


par(mfrow = c(1,2))
plot(seq(0,600, length.out = 100), seq(0,150, length.out = 100), type="n") 

for(i in 7:26){
  lines(fire_left[[i]], col=i)
}


length(left_params$beta_hat[left_params$beta_hat!=0])
length(right_params$beta_hat[right_params$beta_hat!=0])


left_beta_hat = left_params$beta_hat
adj_left = left_beta_hat
adj_left[adj_left!=0]=1
image(adj_left)

region_ids <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Exploratory Analysis/region_ids.RDS")


group_nodes = as.numeric(factor(region_ids, levels = unique(region_ids)))


# Split into a list based on the values
groups <- split(seq_along(group_nodes), group_nodes)


# Create and plot graphs for each group
par(mfrow = c(1, 3))  # Set up plotting area for 3 graphs
for (i in seq_along(groups)) {
  # Extract submatrix for the group
  group_nodes <- groups[[i]]
  submatrix <- adj_matrix[group_nodes, group_nodes]
  
  # Create igraph object for intra-group edges
  g <- graph_from_adjacency_matrix(submatrix, mode = "undirected", diag = FALSE)
  
  # Add inter-group edges
  if (i < length(groups)) {
    next_group_nodes <- groups[[i + 1]]
    inter_edges <- expand.grid(group_nodes, next_group_nodes)
    inter_edges <- as.matrix(inter_edges[sample(nrow(inter_edges), size = 2), ])  # Randomly select 2 edges
    g <- add_edges(g, t(inter_edges))
  }
  
  # Plot the graph
  plot(g, main = paste("Graph for Group", i), vertex.label = group_nodes)
}















# Create igraph object for intra-group edges
g <- graph_from_adjacency_matrix(submatrix, mode = "undirected", diag = FALSE)

# Add inter-group edges
if (i < length(groups)) {
  next_group_nodes <- groups[[i + 1]]
  inter_edges <- expand.grid(group_nodes, next_group_nodes)
  inter_edges <- as.matrix(inter_edges[sample(nrow(inter_edges), size = 2), ])  # Randomly select 2 edges
  g <- add_edges(g, t(inter_edges))
}



f_left = left_params$fs
f_right = right_params$fs


beta0_left = left_params$beta_0#_alt
beta0_right = right_params$beta_0_alt
