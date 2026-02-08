
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
times_10 = res_10$time
times_10
# p = 50 ------------------------------------------------------------------

out_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/50/out_1000.RDS")
out_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/50/out_2000.RDS")
out_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/50/out_5000.RDS")
out_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/50/out_10000.RDS")

# all seconds apart from n= 10000 which is in minutes

all_res_50p = list(out_1000, out_2000, out_5000, out_10000)

res_50 = get_results(all_res_50p)

times_50 = res_50$time
times_50[4] = times_50[4]*60
times_50
# p = 100 ------------------------------------------------------------------

out_1000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/100/out_1000.RDS")
out_2000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/100/out_2000.RDS")
out_5000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/100/out_5000.RDS")
out_10000 <- readRDS("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results/100/out_10000.RDS")

# all seconds apart from n= 10000 which is in minutes

all_res_100p = list(out_1000, out_2000, out_5000, out_10000)

res_100 = get_results(all_res_100p)
# n= 1000 is seconds, rest is minutes> convert time into seconds?
times_100 = res_100$time
times_100[2:4] = times_100[2:4]*60



# plots -------------------------------------------------------------------
# Time vs n
df = data.frame(log_times = c(log(times_10), log(times_50), log(times_100)), 
                times = c((times_10), (times_50), (times_100)),
                p = c(rep("10", length(times_10)), rep("50", length(times_50)), rep("100",length(times_100))), 
                col = c(rep("p=10", length(times_10)), rep("p=50", length(times_50)), rep("p=100", length(times_100)) ),
                n = rep(c(1000,2000,5000,10000), 3))
library(ggplot2)
p1 = ggplot(df, aes(x = n, y = times, group = p, color = p))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  ylab("Runtime (seconds)")+xlab("n")+
  theme(legend.position="top")
p2 = ggplot(df, aes(x = n, y = log_times, group = p, color = p))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  ylab("log(Runtime)")+xlab("n")+
  theme(legend.position="top")
library(ggpubr)
ggarrange(p1,p2,ncol=2, common.legend = T)


# Time vs p

