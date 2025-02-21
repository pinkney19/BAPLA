get_results_2 = function(m){
  # Estimation
  rmse <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/rmse_",m,"m.RDS"))
  
  
  # mses for intercept
  int_mse <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/int_mses_",m,"m.RDS"))
  
  # mses for f
  f_mse = readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/f_mses_",m,"m.RDS"))
  
  # AUROC
  AUC <- readRDS(paste0("~/Downloads/BAPLA/Synthetic_Experiments/f_experiments/CG/gamma/simulations/formatted_res/AUC_",m,"m.RDS"))
  
  
  
  out = cbind(as.numeric(rmse), as.numeric(int_mse), as.numeric(f_mse), as.numeric(AUC))
  out = as.data.frame(out)
  names(out) = c("rmse", "int_mse", "f_mse", "AUC")
  return(out)
  
}

round(get_results_2(3), 2)
round(get_results_2(5) ,2)
round(get_results_2(10) ,2)
round(get_results_2(15), 2)
round(get_results_2(20), 2)
