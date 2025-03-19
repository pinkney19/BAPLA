
# 1. Plot single unit PSTH ------------------------------------------------

plot_psth <- function(spike_times, bin_size, pre_time, post_time, title) {
  #'  Function to plot normalised PSTH for single unit
  #'
  #' @param spike_times 
  #' @param bin_size 
  #' @param pre_time
  #' @param post_time
  #' @param title title of plot
  #' @export  
  
  
  # Calculate the time window
  time_window <- seq(-pre_time, post_time, by = bin_size)
  
  # Initialize histogram
  hist <- numeric(length(time_window) - 1)
  
  # Loop through each stimulus onset
  for (i in 1:length(spike_times)) {
    
    hist <- hist + hist(spike_times[[i]], breaks = time_window, plot = FALSE)$counts
  }
  
  # Normalize histogram
  hist <- (hist) / length(spike_times) / bin_size
  #hist <- (hist) / bin_size
  # integrate to zero
  #hist = hist - mean(hist)
  
  # Prepare data for plotting
  data <- data.frame(time = time_window[-length(time_window)], firing_rate = hist)
  
  # Plot PSTH
  plot <- ggplot(data, aes(x = time, y = firing_rate)) +
    geom_bar(stat = "identity", width = bin_size, fill = "deepskyblue", alpha = 0.7) +
    geom_vline(xintercept = 0, color = "grey50") +
    labs(x = "Time (s)", y = "Firing Rate (Hz)", title = title) +
    #ylim(0, 2)
    theme_minimal()
  
  return(list(plot=plot, hist = hist))
}

# 2. Plot multiple PSTH plots ---------------------------------------------

plot_examples = function(spike_data, bin_size, regions){
  #'  Function to plot example psth plots for multiple units
  #'
  #' @param spike_data 
  #' @param bin_size
  #' @export  
  plot_list = list();
  for(i in 1:length(regions)){
    print(i)
    plot_list[[i]] = plot_psth(spike_data[[i]][[1]], bin_size, 0.2,0.4, regions[i])
  }
  return(plot_list)
}


# 3. Plot PSTH of left and Right ------------------------------------------

plot_right_left_psth <- function(right_times, left_times, bin_size, pre_time, post_time, title) {
  #'  Function to plot normalised PSTH for left and right trials
  #'
  #' @param right_times
  #' @param left_times
  #' @param bin_size 
  #' @param pre_time
  #' @param post_time
  #' @param title title of plot
  #' @export  
  # Calculate the time window
  time_window <- seq(-pre_time, post_time, by = bin_size)
  
  # Initialize histogram
  hist <- numeric(length(time_window) - 1)
  
  for (i in 1:length(right_times)) {
    
    hist <- hist + hist(right_times[[i]], breaks = time_window, plot = FALSE)$counts
  }
  
  # Normalize histogram
  hist <- (hist / length(right_times)) / bin_size
  # make sure integrates to 0?
  #hist = hist - mean(hist)
  # Prepare data for plotting
  data <- data.frame(time = time_window[-length(time_window)], firing_rate = hist)
  
  # do same for left times
  hist2 <- numeric(length(time_window) - 1)
  
  # Loop through each left trial
  for (i in 1:length(left_times)) {
    hist2 <- hist2 + hist(left_times[[i]], breaks = time_window, plot = FALSE)$counts
  }
  
  # Normalize histogram
  hist2 <- (hist2 / length(left_times)) / bin_size
  #hist2 = hist2 - mean(hist2)
  # Prepare data for plotting
  data2 <- data.frame(time = time_window[-length(time_window)], firing_rate = hist2)
  
  # Combine data into one data frame
  data$Trials <- 'Right'
  data2$Trials <- 'Left'
  combined_data <- rbind(data, data2)
  
  # Plot PSTH
  plot = ggplot(combined_data, aes(x = time, y = firing_rate, fill = Trials)) +
    geom_bar(stat = "identity", position = "identity", alpha = 0.8)+
    geom_vline(xintercept = 0, color = "grey50") +
    scale_fill_manual(values = c("blue", "red")) +
    labs(x = "Time (s)", y = "Firing Rate (Hz)", title = title) +
    theme_minimal()
  
  return(plot)
}

# 4. Plot multiple PSTH plots for left and right examples -----------------


plot_lr_examples = function(right_trial_spikes, left_trial_spikes, bin_size){
  #'  Function to plot examples of normalised PSTH for left and right trials
  #'
  #' @param right_trial_spikes
  #' @param left_trial_spikes
  #' @param bin_size 
  #' @param pre_time
  #' @param post_time
  #' @param title title of plot
  #' @export  
  plot_list = list();
  for(i in 1:length(regions)){
    print(i)
    if (length(right_trial_spikes[[i]]) != 0) {
      if(i!=3){
        plot_list[[i]] = plot_right_left_psth(right_trial_spikes[[i]][[1]], left_trial_spikes[[i]][[1]],bin_size, 0.2,0.4, regions[i])
      }
    }
    if(i==3){
      plot_list[[i]] = plot_right_left_psth(right_trial_spikes[[i]][[2]], left_trial_spikes[[i]][[2]],bin_size, 0.2,0.4, regions[i])
    }
    
  }
  return(plot_list)
}
# 5. Bin spikes for single unit -------------------------------------------

bin_data = function(data, xs){
  # Function to binarise the spike counts 
  
  # Initialize binarized data vector with zeros
  binarized_data <- rep(0, length(xs))
  # Iterate through each data point
  for (spike in data) {
    # Find the interval in the xs sequence where the spike happens
    for (i in 1:(length(xs) - 1)) {
      if (xs[i] <= spike && spike < xs[i + 1]) {
        binarized_data[i] <- 1
      }
    }
  }
  return(binarized_data)
}

# 6. bin all spikes -------------------------------------------------------

bin_all = function(spike_data){
  binned  = spike_data;
  for(i in 1:length(spike_data)){
    for(j in 1:length(spike_data[[i]])){
      binned[[i]][[j]] = lapply(spike_data[[i]][[j]], function(x) bin_data(x, xs))
    }
  }
  return(binned)
}



# 7. Remove trials with no spikes -----------------------------------------

remove_zero_trials = function(binned_data){
  #' Function to remove trials where neuron doesn't spike
  #' @param binned_data 
  #' @export 
  #' 
  #' 
  # Get rid of trials with no spikes
  spike_count = list(); spike_count = rep(list(spike_count), length(binned_data));
  idx = list();
  for(i in 1:length(binned_data)){
    dummy_list = binned_data[[i]]
    for(j in 1:length(dummy_list)){
      # count number of spikes observed in each trial for each unit
      spike_count[[i]][[j]] = unlist(lapply(dummy_list[[j]], sum))
    }
    # identify trials for which the unit/neuron does not spike (i.e. no spiking events in this trial)
    idx[[i]] = lapply(spike_count[[i]], function(x) which(x==0))
    # remove "non-spiking" trials from dataset
    for(k in 1:length(dummy_list)){
      # if there is a "no-spiking trial" observed
      if(length(idx[[i]][[k]])!=0){
        # remove these trials from the dataset
        binned_data[[i]][[k]] = binned_data[[i]][[k]][-idx[[i]][[k]]]
      }
    }
  }
  return(binned_data)
}

# 7(old). Format data to same as simulation studies ----------------------------

format_data = function(data_binned){
   #' Function to put data in same format as simulated data
   #' @param data_binned binned data .RDS file
   #all = unlist(data_binned, recursive = F)
   neuron_id = names(data_binned)
   all_data = unname(data_binned)


   data_mat_list = matrix(NA, nrow = length(neuron_id), ncol = lengths(all_data[[1]])[1]);
   data_mat_list = rep(list(data_mat_list), length(all_data[[1]]))

   for(i in 1:length(all_data[[1]])){ # for i in 1: # trials
     dummy_var =  map(all_data,i) # extract trial i data for each neuron
     data_mat_list[[i]] = do.call(rbind, dummy_var) # put into matrix format so same as simulated data
   }


   # check if any units have zero spikes in some of the trials
   spike_count = lapply(data_mat_list, rowSums) # get number of spikes observed in each unit per trial
   zero_idx = lapply(spike_count, function(x) which(x==0))
   zero_units = unique(unlist(zero_idx))


   # new_data = all_data[-zero_units]
   # 
   # for(i in 1:length(new_data[[1]])){
   #   dummy_var =  map(new_data,i) # extract first trial data for each neuron
   #   data_mat_list[[i]] = do.call(rbind, dummy_var) # put into matrix format so same as simulated data
   # }


   #return(list(data = data_mat_list, ids = neuron_id[-zero_units]))
   return(data_mat_list)
 }

# 7. BAPLA single unit ----------------------------------------------------

bapla_single_unit_trial = function(trial_data, idx, max_iter, tol, lams, m){
  #'  Function to estimate coefficients of the BAPLA for a given dimension and regularization parameter
  #'
  #' @param data data source 
  #' @param idx (scalar) to indicate for which data stream we want parameter estimates
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param lams Regularisation parameter
  #' @param m number of basis coefficients for splines
  #' @export  
  
  ######################### obtain data for each trial ######################## 
  trial_X = list(); trial_X = rep(list(trial_X), length(trial_data))
  for(l in 1:length(trial_data)){
    data = trial_data[[l]]
    y = data[idx,][2:length(data[idx,])] # use
    X1 = data[,(1: (length(data[idx,])-1) )] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
    X1 = t(X1)
    p = dim(data)[1]
    
    # standardize the data
    x_bar = colMeans(X1) # get mean of each column
    # X = X1
    X = matrix(NA, ncol=ncol(X1), nrow=nrow(X1))
    for (i in 1:p){
      X[,i] = (X1[,i] - x_bar[i])/sd(X1[,i])
    }
    
    trial_X[[l]] = X
    
  }
  ############## Initialisations #################
  # set regularisation parameter
  lambda = lams[idx]
  
  # # set initial parameters 
  beta_0 = 0
  beta = rep(0,p)
  
  c <- rep(0, m)
  
  phi <- bs(seq(1, length(y)), df=m, intercept = F) 
  phi2 = matrix(NA, nrow = nrow(phi), ncol=ncol(phi))
  phi_bar = NULL;
  for(i in 1:ncol(phi)){
    phi2[,i] = phi[,i] - mean(phi[,i])
    phi_bar[i] = mean(phi[,i])
  }
  
  f <- phi2%*%as.matrix(c)
  
  ############## Cyclical coordinate descent #############
  
  for(iter in 1:max_iter) {
    #print(iter)
    # current parameter estimates
    beta_tilde <- beta
    beta0_tilde <- beta_0
    f_tilde <- f
    c_tilde <- c
    
    ls_coef = list(); ls_coef = rep(list(ls_coef), length(trial_data))
    denom = ls_coef;
    for(k in 1:length(trial_data)){
      
      X = trial_X[[k]]
      
      # compute p_tilde
      p_tilde = ( 1 + exp(-beta0_tilde - X %*% beta_tilde - f_tilde) )^-1
      # compute w
      w = p_tilde * (1-p_tilde)
      
      # compute z
      z = beta0_tilde + X %*% beta_tilde + f_tilde + ( (y-p_tilde)/ w )
      
      # Beta matrix update
      for(j in 1:p) {
        
        # Compute partial residual
        # i.e. fitted value excluding contribution from x_{ij}
        
        y_tilde <- beta_0 + X %*% beta - X[,j] * beta[j] + f_tilde
        
        # compute least-squares coefficient when fitting this partial residual to x_{ij}
        # remember to use z instead of y as the "working response"
        
        ls_coef[[k]][j] <- crossprod(w*X[,j], z - (y_tilde))
        
        denom[[k]][j] <- sum(w*(X[,j]^2))
        
        # Update beta coefficient
        #beta[j] <- ( sign(ls_coef  ) * pmax(0, abs(ls_coef ) - lambda) ) / sum(w*(X[,j]^2)) #soft thresholding step
        
        
      }
      
      
    }
    
    # update beta coefficient
    for(j2 in 1:p){
      ls_coef_trial = sum(unlist(map(ls_coef,j2)))
      denom_trial = sum(unlist(denom,j2))
      beta[j2] = ( sign(ls_coef_trial) * pmax(0, abs(ls_coef_trial ) - lambda) / denom_trial ) 
    }
  
    # Intercept update 
    num = NULL; denom = NULL;
    for (l2 in 1:length(trial_data)){
      X = trial_X[[l2]]
      
      # compute p_tilde
      p_tilde = ( 1 + exp(-beta0_tilde - X %*% beta - f_tilde) )^-1
      # compute w
      w = p_tilde * (1-p_tilde)
      # compute z
      z = beta0_tilde + X %*% beta + f_tilde + ( (y-p_tilde)/ w )
      
      num[l2] <-as.numeric(crossprod(w, z- (X%*%beta) - f_tilde ))
      denom[l2] <- sum(w)
    }
      
    beta_0 <- sum(num)/sum(denom)
    
    # Update spline coefficients assuming known form for f
    
    
    # spline coefficients update 
    c2_up = list(); c2_up = rep(list(c2_up), length(trial_data));
    c2_d = list(); c2_d = rep(list(c2_d), length(trial_data));
    c2 = list(); c2 = rep(list(c2), length(trial_data))
    for(k in 1:m){  
      for (l3 in 1:length(trial_data)){
        X = trial_X[[l3]]
        
        # compute p_tilde
        p_tilde = ( 1 + exp(-beta_0 - X %*% beta - f_tilde) )^-1
        # compute w
        w = p_tilde * (1-p_tilde)
        # compute z
        z = beta_0 + X %*% beta + f_tilde + ( (y-p_tilde)/ w )
        
        # step 1: calculate/update the c_k*phi_k(t) with previously updated c_k
        t = list(); t = rep(list(t), length(c));
        for(k2 in 1:length(c)){
          t[[k2]] <- c[k2]*phi2[,k2]
        }
        
        
        # step 2: update nect c_k (i.e. each c_k updated in turn)
        
        # update ck
        cj_sum <- Reduce("+", t[-k])
        c2_up[[l3]][k] <-  crossprod(w*phi2[,k], z- beta_0 - (X%*%beta) - cj_sum) 
        c2_d[[l3]][k] <- sum(w*(phi2[,k])^2)
        #c2[[l3]][1] <- c2_up[[l3]][1] /  c2_d[[l3]][1]
      }
    
    u1 = unlist(map(c2_up, k))
    d1 = unlist(map(c2_d, k))
    # update c vector with new value of c_k
    c[k] = sum(u1)/sum(d1)
    
    #t[[k]] = c[k] * phi2[,k]
    }
    
     
    f = phi2 %*% as.matrix(c)
    
    f = as.vector(f)
    
    #if there is NAs in the beta estimate - break AND set beta = beta_tilde
    #need to set fs and betas to previous as well
    if(sum(is.na(beta))!=0){
      beta = beta_tilde
      beta_0 = beta0_tilde
      f = f_tilde
      c = c_tilde
      break
    }
    
    # Checks for convergence
    if(sum(is.na(beta))==0){ #if no na's in the beta estimate
      if( max((beta_tilde-beta)^2)<tol^2) break
    }
    # Checks for convergence
    #if( all(!is.na(beta)) & all(!is.na(beta_0)) & all(!is.na(c)) & sqrt(sum((beta - beta_tilde)^2)) < tol & sqrt(sum((beta_0 - beta0_tilde)^2)) <tol  & sqrt(sum((c - c_tilde)^2)) < tol) break
    #if( max((beta_tilde-beta)^2)<tol^2) break
  }
  
  #DE-STANDARDISE THE COEFFICIENTS
  sum_term = NULL;
  for(j in 1:p){
    beta[j] = beta[j]/sd(X1[,j])
    sum_term[j] = (x_bar[j]*beta[j])
  }
  beta_0 = beta_0 - sum(sum_term)
  
  # Account for centered basis functions
  beta_0_alt = beta_0 - sum(c*phi_bar)
  
  
  # Print final coefficients
  return(list(beta0=beta_0, beta=beta, f=f, c=c, iter = iter, beta_0_alt = beta_0_alt))
}



# 7.b ---------------------------------------------------------------------

#' bapla_single_unit_trial2 = function(trial_data, idx, max_iter, tol, lams, m){
#'   #'  Function to estimate coefficients of the BAPLA for a given dimension and regularization parameter
#'   #'
#'   #' @param data data source 
#'   #' @param idx (scalar) to indicate for which data stream we want parameter estimates
#'   #' @param max_iter Maximum number of iterations
#'   #' @param tol tolereance level for the coordinate descent algorithm
#'   #' @param lams Regularisation parameter
#'   #' @param m number of basis coefficients for splines
#'   #' @export  
#'   
#'   ######################### obtain data for each trial ######################## 
#'   trial_X = list(); trial_X = rep(list(trial_X), length(trial_data))
#'   for(l in 1:length(trial_data)){
#'     data = trial_data[[l]]
#'     y = data[idx,][2:length(data[idx,])] # use
#'     X1 = data[,(1: (length(data[idx,])-1) )] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
#'     X1 = t(X1)
#'     p = dim(data)[1]
#'     
#'     # standardize the data
#'     x_bar = colMeans(X1) # get mean of each column
#'     # X = X1
#'     X = matrix(NA, ncol=ncol(X1), nrow=nrow(X1))
#'     for (i in 1:p){
#'       X[,i] = (X1[,i] - x_bar[i])/sd(X1[,i])
#'     }
#'     
#'     trial_X[[l]] = X
#'     
#'   }
#'   ############## Initialisations #################
#'   # set regularisation parameter
#'   lambda = lams[idx]
#'   
#'   # # set initial parameters 
#'   beta_0 = 0
#'   beta = rep(0,p)
#'   
#'   c <- rep(0, m)
#'   
#'   phi <- bs(seq(1, length(y)), df=m, intercept = F) 
#'   phi2 = matrix(NA, nrow = nrow(phi), ncol=ncol(phi))
#'   phi_bar = NULL;
#'   for(i in 1:ncol(phi)){
#'     phi2[,i] = phi[,i] - mean(phi[,i])
#'     phi_bar[i] = mean(phi[,i])
#'   }
#'   
#'   f <- phi2%*%as.matrix(c)
#'   
#'   ############## Cyclical coordinate descent #############
#'   
#'   for(iter in 1:max_iter) {
#'     print(iter)
#'     # current parameter estimates
#'     beta_tilde <- beta
#'     beta0_tilde <- beta_0
#'     f_tilde <- f
#'     c_tilde <- c
#'     
#'     ls_coef = list(); ls_coef = rep(list(ls_coef), length(trial_data))
#'     denom = ls_coef;
#'     for(l in 1:length(trial_data)){
#'       
#'       X = trial_X[[l]]
#'       
#'       # compute p_tilde
#'       p_tilde = ( 1 + exp(-beta0_tilde - X %*% beta_tilde - f_tilde) )^-1
#'       # compute w
#'       w = p_tilde * (1-p_tilde)
#'       
#'       # compute z
#'       z = beta0_tilde + X %*% beta_tilde + f + ( (y-p_tilde)/ w )
#'       
#'       # Beta matrix update
#'       for(j in 1:p) {
#'         
#'         # Compute partial residual
#'         # i.e. fitted value excluding contribution from x_{ij}
#'         
#'         y_tilde <- beta_0 + X %*% beta - X[,j] * beta[j] + f
#'         
#'         # compute least-squares coefficient when fitting this partial residual to x_{ij}
#'         # remember to use z instead of y as the "working response"
#'         
#'         ls_coef[[l]][j] <- crossprod(w*X[,j], z - (y_tilde))
#'         
#'         denom[[l]][j] <- sum(w*(X[,j]^2))
#'         
#'         # Update beta coefficient
#'         #beta[j] <- ( sign(ls_coef  ) * pmax(0, abs(ls_coef ) - lambda) ) / sum(w*(X[,j]^2)) #soft thresholding step
#'         
#'         
#'       }
#'       
#'       
#'     }
#'     
#'     # update beta coefficient
#'     for(j in 1:p){
#'       ls_coef_trial = sum(unlist(map(ls_coef,j)))
#'       denom_trial = sum(unlist(denom,j))
#'       beta[j] = ( sign(ls_coef_trial) * pmax(0, abs(ls_coef_trial ) - lambda) / denom_trial ) 
#'     }
#'     
#'     # Intercept update 
#'     num = NULL; denom = NULL;
#'     for (l in 1:length(trial_data)){
#'       X = trial_X[[l]]
#'       
#'       # compute p_tilde
#'       p_tilde = ( 1 + exp(-beta0_tilde - X %*% beta_tilde - f_tilde) )^-1
#'       # compute w
#'       w = p_tilde * (1-p_tilde)
#'       # compute z
#'       z = beta0_tilde + X %*% beta_tilde + f + ( (y-p_tilde)/ w )
#'       
#'       num[l] <-as.numeric(crossprod(w, z- (X%*%beta) - f ))
#'       denom[l] <- sum(w)
#'     }
#'     
#'     beta_0 <- sum(num)/sum(denom)
#'     
#'     # Update spline coefficients assuming known form for f
#'     
#'     
#'     # spline coefficients update 
#'     c2 = list(); c2 = rep(list(c2), length(trial_data));
#'     for (l in 1:length(trial_data)){
#'       X = trial_X[[l]]
#'       
#'       # compute p_tilde
#'       p_tilde = ( 1 + exp(-beta0_tilde - X %*% beta_tilde - f_tilde) )^-1
#'       # compute w
#'       w = p_tilde * (1-p_tilde)
#'       # compute z
#'       z = beta0_tilde + X %*% beta_tilde + f + ( (y-p_tilde)/ w )
#'       
#'       # step 1: calculate the c_k*phi_k(t)
#'       t = list(); t = rep(list(t), length(c));
#'       for(k in 1:length(c)){
#'         t[[k]] <- c[k]*phi2[,k]
#'       }
#'       
#'       # # step 2: update each c_k in turn
#'       for(k in 1:length(c)){
#'         cj_sum <- Reduce("+", t[-k])
#'         c2[[l]][k] <-  crossprod(w*phi2[,k], z- beta_0 - (X%*%beta) - cj_sum) / sum(w*(phi2[,k])^2)
#'         t[[k]] <- as.numeric(c2[[l]][k])*phi2[,k] #update with new c_k
#'       }
#'     }
#'     
#'     for(i in 1:length(trial_data)){
#'       c2[[i]] = unlist(c2[[i]])
#'     }
#'     c = Reduce("+",c2)
#'     
#'     f = phi2 %*% as.matrix(c)
#'     
#'     f = as.vector(f)
#'     
#'     #if there is NAs in the beta estimate - break AND set beta = beta_tilde
#'     #need to set fs and betas to previous as well
#'     if(sum(is.na(beta))!=0){
#'       beta = beta_tilde
#'       beta_0 = beta0_tilde
#'       f = f_tilde
#'       c = c_tilde
#'       break
#'     }
#'     
#'     # Checks for convergence
#'     if(sum(is.na(beta))==0){ #if no na's in the beta estimate
#'       if( max((beta_tilde-beta)^2)<tol^2) break
#'     }
#'     # Checks for convergence
#'     #if( all(!is.na(beta)) & all(!is.na(beta_0)) & all(!is.na(c)) & sqrt(sum((beta - beta_tilde)^2)) < tol & sqrt(sum((beta_0 - beta0_tilde)^2)) <tol  & sqrt(sum((c - c_tilde)^2)) < tol) break
#'     #if( max((beta_tilde-beta)^2)<tol^2) break
#'   }
#'   
#'   #DE-STANDARDISE THE COEFFICIENTS
#'   sum_term = NULL;
#'   for(j in 1:p){
#'     beta[j] = beta[j]/sd(X1[,j])
#'     sum_term[j] = (x_bar[j]*beta[j])
#'   }
#'   beta_0 = beta_0 - sum(sum_term)
#'   
#'   # Account for centered basis functions
#'   beta_0_alt = beta_0 - sum(c*phi_bar)
#'   
#'   
#'   # Print final coefficients
#'   return(list(beta0=beta_0, beta=beta, f=f, c=c, iter = iter, beta_0_alt = beta_0_alt))
#' }

# 8. plot igraph ----------------------------------------------------------

plot_graph = function(beta_matrix, arrow_size, node_size, region_col){
  #' Function to plot the interaction matrix as an igraph plot
  #'
  #' @param beta_matrix p x p interaction matrix
  #' @param arrow_size size of arrow to be specified in igraph plot
  #' @param node_size size of node to be specified in igraph plot
  #' @export
  # Create a graph from the matrix
  graph <- graph_from_adjacency_matrix(100*beta_matrix, mode = "directed", weighted = T, diag = F)
  # Set edge colors based on the sign of the matrix values
  E(graph)$color <- ifelse(E(graph)$weight > 0, "red", "blue")
  # Set vertex color to white
  V(graph)$color <- region_col
    # Set vertex label color to black
  V(graph)$label.color <- "black"
    V(graph)$size <- node_size
    # Plot the graph
    #plot(graph, edge.arrow.size = 0.5, vertex.label = NA, edge.width = 2, edge.curved = 0.2)
    #set.seed(123)
    # Set the layout to circular
    layout <- layout_in_circle(graph)
    plot(graph, edge.arrow.size = arrow_size,
         vertex.label = seq(1,dim(beta_matrix)[1]), 
         edge.width = 0.1, edge.curved=0.2, layout = layout)
}


# 9. Cut data to certain number of trials ---------------------------------

cut_data = function(data, num_trials){
  #' Function to cut data to specific number of trials
  #' @param data (list) data to cut/modify 
  #' @param num_trials (scalar) number of trials to keep
  
  new_data = list(); kept_trials = list();
  num_spikes = list();
  for(i in 1:length(data)){
    
    # Calculate the number of spikes in each trial
    sums <- sapply(data[[i]], sum)
    
    # Get the order of trials based on the number of spikes (smallest to largest)
    order_indices <- order(sums)
    
    # Record the indices of the trials used in our analysis
    kept_trials[[i]] = rev(order_indices)[1:num_trials]
    
    # Reorder the trials data based on the number of spikes
    sorted_data <- data[[i]][order_indices]
    
    # Reverse order as want highest to lowest
    sorted_data <- rev(sorted_data)
    
    new_data[[i]] = sorted_data[1:num_trials]
    # Record number of spikes in each trial
    num_spikes[[i]] =  sapply(new_data[[i]], sum)
  }
  return(list(new_data = new_data, trials_kept = kept_trials, num_spikes = num_spikes))
}
