
# functions requried ------------------------------------------------------

get_results = function(all_res){
  avg_time = NULL;avg_iter = list();
  for(i in 1:length(all_res)){
    res = all_res[[i]]
    times = map(res, 6)
    avg_time[i] = mean(unlist(times)) # 0.69 seconds
    iter = map(res, 7)
    avg_iter[[i]] = Reduce("+", iter)/length(iter)
  }
  iter_av = sapply(avg_iter, mean)
  return(list(iter = iter_av, time = avg_time))
}



# p = 10 ------------------------------------------------------------------

out_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/10/out_1000.RDS")
out_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/10/out_2000.RDS")
out_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/10/out_5000.RDS")
out_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/10/out_10000.RDS")

all_res_10p = list(out_1000, out_2000, out_5000, out_10000)

res_10 = get_results(all_res_10p)


# p = 50 ------------------------------------------------------------------

out_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/50/out_1000.RDS")
out_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/50/out_2000.RDS")
out_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/50/out_5000.RDS")
out_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/50/out_10000.RDS")

all_res_50p = list(out_1000, out_2000, out_5000, out_10000)

res_50 = get_results(all_res_50p)
