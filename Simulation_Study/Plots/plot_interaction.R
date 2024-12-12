setwd("~/Downloads/BAPLA/Simulation_Study")
source("Functions_BAPLA.R")

beta_CG <- readRDS("~/Downloads/BAPLA/Simulation_Study/CG/beta_10.RDS")
beta_ER <- readRDS("~/Downloads/BAPLA/Simulation_Study/ER/beta_10.RDS")
beta_SB <- readRDS("~/Downloads/BAPLA/Simulation_Study/SB/beta_10.RDS")

library(igraph)
par(mar=c(0.1,0.1,0.1,0.1))
set.seed(123)
plot_graph(beta_CG, 0.5, 10)
#set.seed(123)
plot_graph(beta_ER, 0.5, 10)
#set.seed(123)
plot_graph(beta_SB, 0.5, 10)

# degree
max_non_zero_entries <- function(matrix) {
  max_count <- 0
  for (i in 1:nrow(matrix)) {
    non_zero_count <- sum(matrix[i, ] != 0)
    if (non_zero_count > max_count) {
      max_count <- non_zero_count
    }
  }
  return(max_count)
}

max_non_zero_entries(beta_CG)
max_non_zero_entries(beta_ER)
max_non_zero_entries(beta_SB)



# Example usage:
# matrix <- matrix(c(1, 0, 3, 0, 0, 6, 0, 8, 0), nrow=3, byrow=TRUE)
# max_non_zero_entries(matrix)