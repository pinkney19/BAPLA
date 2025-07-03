
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
cols = c( "deepskyblue1", "deeppink")
                  
map_cols = c(S = cols[1], NS =cols[2])

num_region <- mapping[df$type]
col_region = map_cols[df$type]

setwd("~/Downloads/BAPLA/Data_Analysis/R")
source("Functions_data.R")
library(igraph)





mat1 = beta_left_wheel


mat1[mat1>0] = 1; mat1[mat1<0]=2;


par(mfrow=c(1,1), mar = c(0,0,0,0))
set.seed(123)
#legend("top", legend = names(map_cols), col = cols,
 #      pch = 19, bty = "n", cex = 1.5)

plot_graph_new(mat1, 0.3, 7, col_region)
legend("topleft", legend = names(map_cols), col = cols,
       pch = 19, bty = "n", cex = 0.7)

par(mfrow = c(1,2))

set.seed(123)
# plot only non-stationary edges
plot_graph_MB_POL(mat1, arrow_size = 0.5, node_size = 5,
                                     region_col = col_region, keep_regions = c("NS"))


# add in with f estimate

left_params_f <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Estimation_wheel/results/left_params.RDS")

mat2 = left_params_f$beta_hat
mat2[mat2>0] = 1; mat2[mat2<0]=2;
set.seed(123)
plot_graph_MB_POL(mat2, arrow_size = 0.5, node_size = 5,
                  region_col = col_region, keep_regions = c("NS"))

# compare full graphs

set.seed(123)
#legend("top", legend = names(map_cols), col = cols,   
#      pch = 19, bty = "n", cex = 1.5)
par(mfrow = c(1,2))
plot_graph_new(mat1, 0.3, 7, col_region)
legend("topleft", legend = names(map_cols), col = cols,   
       pch = 19, bty = "n", cex = 0.7)
set.seed(123)
plot_graph_new(mat2, 0.3, 7, col_region)


# set layout --------------------------------------------------------------

set.seed(1234)

ref_graph <- graph_from_adjacency_matrix(100*mat1, 
                                         mode = "directed", weighted = T, diag = F)

V(ref_graph)$color <- region_col
region_col = col_region
# Set the layout based on region
n_clusters = length(region_col)
#match colours to groups
group_map = match(V(ref_graph)$color, region_col)


# Prepare layout list per cluster
layouts <- list()
#offsets <- expand.grid(x= seq(0, n_clusters)*0.6, y=0)  # spread vertically (adjust as needed)

# Define 2D positions for each cluster (grid or radial layout)
angle <- seq(0, 2 * pi, length.out = n_clusters + 1)[-1]  # spread in a circle
radius <- 25  # distance between cluster centers
offsets <- data.frame(x = cos(angle) * radius, y = sin(angle) * radius)


for (i in 1:n_clusters) {
  nodes_in_group <- which(group_map == i)
  subg <- induced_subgraph(ref_graph, nodes_in_group)
  l <- layout_in_circle(subg)#,  niter = 2000)
  
  # Normalize to avoid overlap within cluster
  #l <- norm_coords(l, xmin = -5, xmax = 5, ymin = -5, ymax = 5)
  
  
  # Offset the circle layout so clusters don't overlap
  offset <- as.numeric(offsets[i, ])
  l <- sweep(l, 2, offset, "+")
  
  layouts[[i]] <- list(nodes = nodes_in_group, layout = l)
}

# Combine all layouts into one full layout
full_layout <- matrix(NA, nrow = vcount(ref_graph), ncol = 2)
for (i in 1:n_clusters) {
  full_layout[layouts[[i]]$nodes, ] <- layouts[[i]]$layout
}

full_layout


plot_graph_new(mat2, 0.3, 7, col_region, full_layout)

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

