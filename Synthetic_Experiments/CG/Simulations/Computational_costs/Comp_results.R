library(purrr)
library(ggplot2)
library(ggpubr)
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
# n= 1000 is seconds, rest is minutes> convert time into seconds
times_100 = res_100$time
times_100[2:4] = times_100[2:4]*60
times_100

rbind(times_10, times_50, times_100)
# plots -------------------------------------------------------------------
# Time vs n
df = data.frame(log_times = c(log(times_10), log(times_50), log(times_100)), 
                times = c((times_10), (times_50), (times_100)),
                d = c(rep("10", length(times_10)), rep("50", length(times_50)), rep("100",length(times_100))), 
                col = c(rep("p=10", length(times_10)), rep("p=50", length(times_50)), rep("p=100", length(times_100)) ),
                n = rep(c(1000,2000,5000,10000), 3))

# add column with time in minues
df$times_min = df$times/60

library(ggplot2)
p1 = ggplot(df, aes(x = n, y = times, group = d, color = d))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  ylab("Runtime (seconds)")+xlab("n")+
  theme(legend.position="top")

p1_mins = ggplot(df, aes(x = n, y = times_min, group = d, color = d))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  ylab("Runtime (mins)")+xlab("n")+
  theme(legend.position="top")
p1_mins
p2 = ggplot(df, aes(x = n, y = log_times, group = d, color = d))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  ylab("log(Runtime)")+xlab("n")+
  theme(legend.position="top")


library(ggpubr)
both_p = ggarrange(p1,p2,ncol=2, common.legend = T)
both_p
# iterations

df_iter = data.frame(iterations = c(res_10$iter, res_50$iter, res_100$iter), 
                     d = c(rep("10", length(res_10$iter)), rep("50", length(res_50$iter)), rep("100",length(res_100$iter))),
                     n = rep(c(1000,2000,5000,10000), 3))

p3 = ggplot(df_iter, aes(x = n, y = iterations, group = d, color = d))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  ylab("Average No. of Iterations to convergence")+xlab("n")+
  theme(legend.position="top")
time_iter = ggarrange(p2,p3, ncol=2, common.legend = T)
setwd("~/Downloads/BAPLA/Synthetic_Experiments/CG/Simulations/Computational_costs/results")
ggsave("comp_cost_plot.pdf", plot = time_iter, width = 8, height = 4, units = "in" )
ggsave("log_runtime.pdf", p2, width = 4, height = 4, units ="in")
ggsave("convergence.pdf", p3, width = 4, height = 4, units = "in")
ggsave("runtime_mins.pdf", p1_mins, width = 4, height = 4, units = "in")


# time vs d ?
times = list(res_10$time, res_50$time, res_100$time)
new_times = list();
for(i in 1:lengths(times)[1]){
  new_times[[i]] = unlist(map(times, i))
}

new_times[[2]][3] = new_times[[2]][3]*60
new_times[[3]][3] = new_times[[3]][3]*60
new_times[[4]][2:3] = new_times[[4]][2:3]*60
new_times


df_p = data.frame(log_times = c(log(new_times[[1]]), log(new_times[[2]]), 
                                log(new_times[[3]]), log(new_times[[4]])), 
                n = c(rep("1000", 3), rep("2000", 3), rep("5000",3), rep("10000", 3)), 
                d = rep(c(10,50,100), 4))
p4 = ggplot(df_p, aes(x = d, y = log_times, group = n, color = n))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  ylab("log(Runtime)")+xlab("d")+
  theme(legend.position="top")
ggsave("log_runtime_d.pdf", p4, width = 4, height = 4, units ="in")

# add plot with time in minutes

new_times_min = lapply(new_times, function(x) x/60)

df = data.frame(log_times = c(log(times_10), log(times_50), log(times_100)), 
                times = c((times_10), (times_50), (times_100)),
                d = c(rep(10, length(times_10)), rep(50, length(times_50)), rep(100,length(times_100))), 
                col = c(rep("p=10", length(times_10)), rep("p=50", length(times_50)), rep("p=100", length(times_100)) ),
                n = rep(c("1000","2000","5000","10000"), 3))
p5 = ggplot(df, aes(x = d, y = log_times, group = n, color = n))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  ylab("log(Runtime)")+xlab("d")+
  theme(legend.position="top")

p5


df_iter2 = data.frame(iterations = c(res_10$iter, res_50$iter, res_100$iter), 
                     d = c(rep(10, length(res_10$iter)), rep(50, length(res_50$iter)),
                           rep(100,length(res_100$iter))),
                     n = rep(c("1000","2000","5000","10000"), 3))
p6 = ggplot(df_iter2, aes(x = d, y = iterations, group = n, color = n))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  ylab("Average No. of Iterations to convergence")+xlab("d")+
  theme(legend.position="top")
ggsave("convergence_d.pdf", p6, width = 4, height = 4, units ="in")
