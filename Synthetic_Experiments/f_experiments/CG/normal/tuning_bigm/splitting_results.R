# Load libraries ----------------------------------------------------------

library(purrr)
library(pracma)

setwd("~/Downloads/BAPLA/Synthetic_Experiments")
source("BAPLA_Functions.R")


# Chain Graph ----------------------------------------------

p = 10; 
beta <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/beta_10.RDS")
m_seq = c(25,30,35,40)
data_seq = seq(1, 10)
lambdas = logspace(log10(10),log10(300),100)
grid = expand.grid(sample = data_seq, lams = lambdas,  m = m_seq)
list_of_pairs <- split(grid, seq(nrow(grid)))

# Load tuning results from STORM ----------------------------------------------
out_50_1000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1/tuning_bigm/out_50_1000.RDS")
out_50_2000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1/tuning_bigm/out_50_2000.RDS")
out_50_5000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1/tuning_bigm/out_50_5000.RDS")
out_50_10000 <- readRDS("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1/tuning_bigm/out_50_10000.RDS")


# Save for separate ms ----------------------------------------------------

#ms = unname(unlist(map(list_of_pairs,3)))
# Split ms into 5 lists of length 1000
#split_ms <- split(ms, ceiling(seq_along(ms) / 1000))


# do same for out files
out_1000 = split(out_50_1000, ceiling(seq_along(out_50_1000) / 1000 ) )
out_2000 = split(out_50_2000, ceiling(seq_along(out_50_2000) / 1000 ) )
out_5000 = split(out_50_5000, ceiling(seq_along(out_50_5000) / 1000 ) )
out_10000 = split(out_50_10000, ceiling(seq_along(out_50_10000) / 1000 ) )


# Function to save output files for each m --------------------------------

save_m_out = function(out, n, m_seq){
  for(i in 1:length(out)){
    setwd(paste0("~/luna/BAPLA/Synthetic_Experiments/f_experiments/CG_normal/beta_0.1/tuning_bigm/out/",m_seq[i]))
    saveRDS(out[[i]], paste0("out_", n, ".RDS"))
  }
}

save_m_out(out_1000, 1000, m_seq)
save_m_out(out_2000, 2000, m_seq)
save_m_out(out_5000, 5000, m_seq)
save_m_out(out_10000, 10000, m_seq)

