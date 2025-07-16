get_node_degree = function(matrix){
  # Exclude self-loops (diagonal)
  diag(matrix) <- 0
  # Degree 
  # matrix != 0 -> creates true/false for each row
  degrees <- rowSums(matrix != 0)
  return(max(degrees))
}
beta_CG_10 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_10.RDS")
beta_CG_50 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_50.RDS")

beta_ER_10 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/ER/beta_10.RDS")
beta_ER_50 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/ER/beta_50.RDS")

beta_SB_10 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/SB/beta_10.RDS")
beta_SB_50 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/SB/beta_50.RDS")

get_node_degree(beta_CG_10)
get_node_degree(beta_CG_50)
get_node_degree(beta_ER_10)
get_node_degree(beta_ER_50)
get_node_degree(beta_SB_10)
get_node_degree(beta_SB_50)


vis_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/vis_left.RDS")
vis_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/vis_right.RDS")
wheel_left <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/wheel_left.RDS")
wheel_right <- readRDS("~/Downloads/BAPLA/Data_Analysis/R/Data_Vis/summary_stats/BAPLA/wheel_right.RDS")
get_node_degree(vis_left) # 4
get_node_degree(vis_right) # 4
get_node_degree(wheel_left) # 5
get_node_degree(wheel_right) # 8
