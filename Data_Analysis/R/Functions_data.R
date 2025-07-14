firing_rate_func = function(spike_train_data, time_bins, bin_size){
  # Initialize a vector to store spike counts for each bin
  spike_counts <- matrix(0, nrow = length(spike_train_data), ncol = length(time_bins) - 1)
  
  # Count spikes in each bin for each trial
  for (i in 1:length(spike_train_data)) {
    trial_spikes <- spike_train_data[[i]]
    for (j in 1:(length(time_bins) - 1)) {
      # Count spikes in the j-th bin
      spike_counts[i, j] <- sum(trial_spikes >= time_bins[j] & trial_spikes < time_bins[j + 1])
    }
  }
  # Calculate the average spike count per bin across trials
  avg_spike_counts <- colMeans(spike_counts)
  
 
  # Convert spike count to firing rate (Hz = spikes per second)
  firing_rate1 <- avg_spike_counts  /   bin_size
  return(firing_rate1)
}


# alternative PSTH --------------------------------------------------------
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
  
 
  
  
  # Define the time window (e.g., -0.2s to 0.4s around the stimulus)
  time_window <- c(-pre_time, post_time)  # seconds
  
  # Define bin size for PSTH (e.g., 0.01 seconds)
  
  
  # Create bins based on the defined time window and bin size
  time_bins <- seq(time_window[1], time_window[2], by = bin_size)
  
  
  firing_rate1 = firing_rate_func(right_times, time_bins, bin_size)
  firing_rate2 = firing_rate_func(left_times, time_bins, bin_size)

  data <- data.frame(time = time_bins[-length(time_bins)], firing_rate = firing_rate1)
  

  data2 <- data.frame(time = time_bins[-length(time_bins)], firing_rate = firing_rate2)
  
  # Combine data into one data frame
  data$Trials <- 'Right'
  data2$Trials <- 'Left'
  combined_data <- rbind(data, data2)
  
  # Plot PSTH
  plot = ggplot(combined_data, aes(x = time, y = firing_rate, fill = Trials)) +
    geom_bar(stat = "identity", position = "identity", alpha = 0.5)+
    geom_vline(xintercept = 0, color = "grey50") +
    #scale_fill_manual(values = c("blue", "red")) +
    scale_fill_manual(values = c("blue", "red")) +
    labs(x = "Time (s)", y = "Firing Rate (Hz)", title = title) +
    theme_minimal()+
    ylim(0, 220) 
  
  return(plot)
}



# plot only left or only right --------------------------------------------

plot_psth_one <- function(right_times, left_times, bin_size, pre_time, post_time, title, right_flag) {
  #'  Function to plot normalised PSTH for left and right trials
  #'
  #' @param right_times
  #' @param left_times
  #' @param bin_size 
  #' @param pre_time
  #' @param post_time
  #' @param title title of plot
  #' @export  
  
  
  
  
  # Define the time window (e.g., -0.2s to 0.4s around the stimulus)
  time_window <- c(-pre_time, post_time)  # seconds
  
  # Define bin size for PSTH (e.g., 0.01 seconds)
  
  
  # Create bins based on the defined time window and bin size
  time_bins <- seq(time_window[1], time_window[2], by = bin_size)
  
  
  firing_rate1 = firing_rate_func(right_times, time_bins, bin_size)
  firing_rate2 = firing_rate_func(left_times, time_bins, bin_size)
  
  data <- data.frame(time = time_bins[-length(time_bins)], firing_rate = firing_rate1)
  
  
  data2 <- data.frame(time = time_bins[-length(time_bins)], firing_rate = firing_rate2)
  
  # Combine data into one data frame
  data$Trials <- 'Right'
  data2$Trials <- 'Left'
  combined_data <- rbind(data, data2)
  
  # Plot PSTH
  if(right_flag == T){colour = "black"}
  if(right_flag == F){colour = "black"}
  plot = ggplot(combined_data, aes(x = time, y = firing_rate, fill = Trials)) +
    geom_bar(stat = "identity", position = "identity", alpha = 0.5)+
    geom_vline(xintercept = 0, color = "deepskyblue") +
    scale_fill_manual(values = c(colour, colour)) +
    labs(x = "Time (s)", y = "Firing Rate (Hz)", title = title) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_text(size = 14),
          axis.text = element_text(size=12))
  
  return(plot)
}



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
    geom_bar(stat = "identity", position = "identity", alpha = 0.5)+
    geom_vline(xintercept = 0, color = "grey50") +
    scale_fill_manual(values = c("blue", "red")) +
    labs(x = "Time (s)", y = "Firing Rate (Hz)", title = title) +
    theme_minimal()+
    ylim(0,220)
  
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
  E(graph)$color <- ifelse(E(graph)$weight > 0, "black", "black")
  
  # Set edge type based on sign of the matrix values
  E(graph)$lty <- ifelse(E(graph)$weight >0, "solid", "dashed")
  
  
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
         vertex.label = seq(1,dim(beta_matrix)[1]), edge.curved=0.2, layout = layout)
}



# 8b ----------------------------------------------------------------------

plot_graph_new = function(beta_matrix, arrow_size, node_size, region_col, full_layout, highlight_rows){
  #' Function to plot the interaction matrix as an igraph plot
  #'
  #' @param beta_matrix p x p interaction matrix
  #' @param arrow_size size of arrow to be specified in igraph plot
  #' @param node_size size of node to be specified in igraph plot
  #' @param region_col colours for specific regions
  #' @param full_layout fixed layout for igraph plot
  #' @param highlight_rows nodes not used in the estimation step - highlight in grey
  #' @export
  
  

  # Create a graph from the matrix
  graph <- graph_from_adjacency_matrix(100*beta_matrix, mode = "directed", weighted = T, diag = F)
  # Set edge colors based on the sign of the matrix values
  E(graph)$color <- ifelse(E(graph)$weight > 0, "black", "black")
  
  # Set edge type based on sign of the matrix values
  E(graph)$lty <- ifelse(E(graph)$weight <200 & E(graph)$weight!=0 , "solid", "dashed")
  
  
  # Set vertex color to white
  V(graph)$color <- region_col
  
  # shade edges not used in estimation in a different colour
  V(graph)$color[highlight_rows] <- "grey"  # Shade highlighted nodes in grey
    
  # Count edges between nodes of each color
  edge_color_counts <- table(sapply(as_edgelist(graph), function(edge) {
    paste(V(graph)$color[edge[1]], V(graph)$color[edge[2]], sep = "-")
  }))
  
  # Print edge count and edge color counts
  edge_count <- ecount(graph)
  print(paste("Number of edges in the graph:", edge_count))
  print("Edge counts by color group:")
  print(edge_color_counts)
  
    # Set vertex label color to black
    V(graph)$label.color <- "black"
    V(graph)$size <- node_size
  
    

    #layout <- layout_in_circle(graph)
    plot(graph, edge.arrow.size = arrow_size,
         vertex.label = NA,  edge.curved=0.2, layout = full_layout, 
         vertex.label.cex = 0.7)
}


# 8d. inference graph -----------------------------------------------------

plot_graph_inference = function(beta_matrix, comparison_matrix, arrow_size, node_size, region_col, full_layout, highlight_rows){
  #' Function to plot the interaction matrix as an igraph plot
  #'
  #' @param beta_matrix p x p interaction matrix
  #' @param arrow_size size of arrow to be specified in igraph plot
  #' @param node_size size of node to be specified in igraph plot
  #' @param region_col colours for specific regions
  #' @param full_layout fixed layout for igraph plot
  #' @param highlight_rows nodes not used in the estimation step - highlight in grey
  #' @export
  
  
  
  # Create a graph from the matrix
  graph <- graph_from_adjacency_matrix(100*beta_matrix, mode = "directed", weighted = T, diag = F)
  # Set edge colors based on the sign of the matrix values
  E(graph)$color <- ifelse(E(graph)$weight > 0, "black", "black")
  
  # Set edge type based on sign of the matrix values
  E(graph)$lty <- ifelse(E(graph)$weight <200 & E(graph)$weight!=0 , "solid", "dashed")
  
  # Create a graph from the comparison_matrix
  comparison_graph <- graph_from_adjacency_matrix(100 * comparison_matrix, mode = "directed", weighted = TRUE, diag = FALSE)
  
  # Identify edges in both graphs
  edges_beta <- as_edgelist(graph)
  edges_comparison <- as_edgelist(comparison_graph)
  
  # Count matching edges
  matching_edges_count <- sum(apply(edges_beta, 1, function(edge) {
    any(apply(edges_comparison, 1, function(comp_edge) all(comp_edge == edge)))
  }))
  
  # Set edge colors: red for edges in both graphs, black otherwise
  E(graph)$color <- apply(edges_beta, 1, function(edge) {
    if (any(apply(edges_comparison, 1, function(comp_edge) all(comp_edge == edge)))) {
      "black"
    } else {
      "darkgrey"
    }
  })
  
  # Set vertex color to white
  V(graph)$color <- region_col
  
  # shade edges not used in estimation in a different colour
  V(graph)$color[highlight_rows] <- "white"  # Shade highlighted nodes in grey
    
  # Count edges between nodes of each color
  edge_color_counts <- table(sapply(as_edgelist(graph), function(edge) {
    paste(V(graph)$color[edge[1]], V(graph)$color[edge[2]], sep = "-")
  }))
  
  # Print edge count and edge color counts
  edge_count <- ecount(graph)
  print(paste("Number of edges in the graph:", edge_count))
  print("Edge counts by color group:")
  print(edge_color_counts)
  
  # Set vertex label color to black
  V(graph)$label.color <- "black"
    V(graph)$size <- node_size
    
    
    
    #layout <- layout_in_circle(graph)
    plot(graph, edge.arrow.size = arrow_size,
         vertex.label = NA,  edge.curved=0.2, layout = full_layout, 
         vertex.label.cex = 0.7)
}


# 8c - fixed layout for igraph --------------------------------------------

get_fixed_layout_all = function(beta_matrix, region_col, node_size){
  #' Function to fix layout of igraph
  #' @param beta_matrix = d x d interaction matrix
  #' @param region_col A character vector which assigns color for each node.
  #' @param node_size (numeric) size of nodes in igraph
  graph <- graph_from_adjacency_matrix(100*beta_matrix, mode = "directed", weighted = T, diag = F)
  # Set edge colors based on the sign of the matrix values
  E(graph)$color <- ifelse(E(graph)$weight > 0, "black", "black")
  
  # Set edge type based on sign of the matrix values
  E(graph)$lty <- ifelse(E(graph)$weight <200 & E(graph)$weight!=0 , "solid", "dashed")
  
  
  # Set vertex color to white
  V(graph)$color <- region_col
  
  
  # shade edges not used in estimation in a different colour
  #V(graph)$color[highlight_rows] <- "grey"  # Shade highlighted nodes in grey
    
  # re-order based on colour
  # order = c(
  #   which(V(graph)$color == "grey"),
  #   which(V(graph)$color != "grey")
  # )
  # 
  
  
  # Set vertex label color to black
  V(graph)$label.color <- "black"
    V(graph)$size <- node_size
    
    
    
    # Set the layout based on region
    n_clusters = length(region_col)
    #match colours to groups
    group_map = match(V(graph)$color, region_col)
    
    
    # Prepare layout list per cluster
    layouts <- list()
    #offsets <- expand.grid(x= seq(0, n_clusters)*0.6, y=0)  # spread vertically (adjust as needed)
    
    # Define 2D positions for each cluster (grid or radial layout)
    angle <- seq(0, 2 * pi, length.out = n_clusters + 1)[-1]  # spread in a circle
    radius <- 25  # distance between cluster centers
    offsets <- data.frame(x = cos(angle) * radius, y = sin(angle) * radius)
    
    
    for (i in 1:n_clusters) {
      nodes_in_group <- which(group_map == i)
      subg <- induced_subgraph(graph, nodes_in_group)
      l <- layout_with_fr(subg,  niter = 2000)
      
      # Normalize to avoid overlap within cluster
      #l <- norm_coords(l, xmin = -5, xmax = 5, ymin = -5, ymax = 5)
      
      
      # Offset the circle layout so clusters don't overlap
      offset <- as.numeric(offsets[i, ])
      l <- sweep(l, 2, offset, "+")
      
      layouts[[i]] <- list(nodes = nodes_in_group, layout = l)
    }
    
    # Combine all layouts into one full layout
    full_layout <- matrix(NA, nrow = vcount(graph), ncol = 2)
    for (i in 1:n_clusters) {
      full_layout[layouts[[i]]$nodes, ] <- layouts[[i]]$layout
    }
    
    
    return(full_layout)
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


# 10. Function to count avg spikes per trial ------------------------------

avg_spike_per_trial = function(data){
  #' Function to calculate average number of spiking events per experimental trial
  #' for each neuron/unit
  #' @param data (list) nested list of trial times for each unit
  av_spike = NULL;
  for(i in 1:length(data)){
    av_spike[i] = mean(unlist(lapply(data[[i]], sum)))
  }
  return(av_spike)
}


# 11. Function to plot avg points -----------------------------------------


plot_avg_points = function(left_data, right_data, title){
  #' Function to plot average number of points per trial
  #' @param left_data (list)  
  #' @param right_data (list)
  #' @param title (string) title of plot
  pp_left = lapply(left_data, rowSums)
  av_left = Reduce("+",pp_left)/length(pp_left)
  plot(seq(1, length(av_left)), av_left, col="blue", ylab = "Avg Points per Trial", xlab = "Unit", ylim = c(0,60), main = title)
  
  pp_right = lapply(right_data, rowSums)
  av_right = Reduce("+",pp_right)/length(pp_right)
  points(seq(1, length(av_right)), av_right, col="red")
  abline(h = 10, lty = 2)
  legend("topright", c("left", "right"), col=c("blue", "red"), pch = c(1,1))
  return(list(av_lft = av_left, av_rt = av_right))
}


# 12. Function to remove zero trials from raw data ------------------------

remove_zeros = function(unit_data){
  #' Function to remove trials with no spikes from raw data
  #' unit_data (list) list of raw spikes times (i.e. no binned)
  new_data = list();
  for(i in 1:length(unit_data)){
    no_spikes = lengths(unit_data[[i]]) # get number of spikes in each trial
    zero_ids = which(no_spikes == 0) # get IDs of trials with no spikes
    if(length(zero_ids)>0){ # if there are trials with no spikes -> remove them
      new_data[[i]] = unit_data[[i]][-zero_ids]
    }
    if(length(zero_ids)==0){ # If all trials have spikes, keep them all 
      new_data[[i]] = unit_data[[i]]
    }
  }
  return(new_data)
}


# 13. Function to remove low firing trials from raw data ------------------

remove_low_trials = function(unit_data, trial_ids){
  #' Function to remove low firing trials from raw data
  #' @param unit_data (list) list of raw spike times
  #' @param trial_ids (list) list of trial ids to keep in analysis
  new_data = list();
  for(i in 1:length(unit_data)){
    # select most active trials
    new_data[[i]] = unit_data[[i]][trial_ids[[i]]]
  }
  return(new_data)
}


# 14. BIC Function --------------------------------------------------------

BIC_function= function(lam_idx, unit_idx, gamma, trial_data, beta_0, betas, f, m){
  #' Function to get BIC for real data
  #' @param lam_idx index of lambda value
  #' @param unit_idx index of the unit/neuron
  #' @param gamma hyperparameter for eBIC
  #' @param trial_data data from neuroscience experiment
  
  ######################### obtain data for each trial ######################## 
  trial_X = list(); trial_X = rep(list(trial_X), length(trial_data))
  nll = NULL;
  for(l in 1:length(trial_data)){
    # select data of interest to the unit_id (and trial of interest)
    data = trial_data[[l]]
    y = data[unit_idx,][2:length(data[unit_idx,])] # use
    X1 = data[,(1: (length(data[unit_idx,])-1) )] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
    X1 = t(X1)
    p = dim(data)[1]
    
    #########################################
    
    term1 = y * (beta_0[[lam_idx]] + X1 %*% betas[[lam_idx]] + f[[lam_idx]])
    term2 = log(1 + exp(beta_0[[lam_idx]] + X1 %*% betas[[lam_idx]] + f[[lam_idx]])) 
    nll[l] = sum(term1-term2) 
  }
  
  lik = sum(nll)
  
  num_non_zeroes = sum(betas[[lam_idx]] != 0)
  n_star = 600 * 60 #36060
  pen_term = (num_non_zeroes+1+(m))*log(n_star)
  eBIC_term = 2*gamma*log(p)* (num_non_zeroes+1+(m))
  
  BIC = -2*lik + pen_term #+ eBIC_term
  eBIC = BIC + eBIC_term
  AIC = -2*lik + (2*(num_non_zeroes+1+(m)))
  
  return(list(BIC = BIC, AIC = AIC, eBIC = eBIC))
}


# 14.b function to calculate log-likelihood for common lambda per region -------------------------

calc_log_lik = function(lam_idx, unit_idx, trial_data, beta_0, betas, f, m){
  #' Function to get BIC for real data
  #' @param lam_idx index of lambda value
  #' @param unit_idx index of the unit/neuron
  #' @param gamma hyperparameter for eBIC
  #' @param trial_data data from neuroscience experiment
  
  ######################### obtain data for each trial ######################## 
  trial_X = list(); trial_X = rep(list(trial_X), length(trial_data))
  ll = NULL;
  for(l in 1:length(trial_data)){
    print(l)
    # select data of interest to the unit_id (and trial of interest)
    data = trial_data[[l]]
    y = data[unit_idx,][2:length(data[unit_idx,])] # use
    X1 = data[,(1: (length(data[unit_idx,])-1) )] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
    X1 = t(X1)
    p = dim(data)[1]
    
    #########################################
    
    term1 = y * (beta_0[[lam_idx]] + X1 %*% betas[[lam_idx]] + f[[lam_idx]])
    term2 = log(1 + exp(beta_0[[lam_idx]] + X1 %*% betas[[lam_idx]] + f[[lam_idx]])) 
    ll[l] = sum(term1-term2) 
  }
  
  lik = sum(ll)
  
  num_non_zeroes = sum(betas[[lam_idx]] != 0)
  
  return(list(lik=lik, k=num_non_zeroes))
}

BIC_common_lambda = function(lam_idx, beta_list, lik, beta0_list, f_list, m){ 
  #' Function to calcaulte BIC for common lambda per region
  #' @param lam_idx index of lambda value
  #' @param beta_list nested list of interaction matrices for the units of interest
  #' @param lik list of likelihood valus for the units of interest
  #' @param beta0_list list of intercept values for the untis of interest
  #' @param f_list list of estimated f's for units of interest
  #' @param m number of basis functions used in the estimation
  #' 
  
  
  
  num_non_zeroes = sum(betas[[lam_idx]] != 0)
  n_star = 600 * 60 #36060
  pen_term = (num_non_zeroes+1+(m))*log(n_star)
  eBIC_term = 2*gamma*log(p)* (num_non_zeroes+1+(m))
  
  BIC = -2*lik + pen_term #+ eBIC_term
  eBIC = BIC + eBIC_term
  AIC = -2*lik + (2*(num_non_zeroes+1+(m)))
  
  return(list(BIC = BIC, AIC = AIC, eBIC = eBIC))
}


# 15. Regularisation parameter selection --------------------------------------

choose_lambda = function(trial_data, p, lambdas, out_storm, m){
  #' Function to select regularisation parameter based on BIC results
  #' 
  BIC = NULL; BIC = rep(list(BIC), p)
  lam_star = NULL; lam_id = NULL;
  out = list();
  for(j in 1:p){
    print(j)
    b = length(lambdas)*j
    if(j==1){
      # select results from storm
      out  = out_storm[1:b]
    }
    if(j!=1){
      # select results from storm
      out = out_storm[((b-length(lambdas))+1):b]
    }
    betas = map(out, 1)
    beta_0 = map(out, 5) # use corrected for basis functions
    f = map(out, 3)
    # make betas matrix not vector
    betas = lapply(betas, as.matrix)
    
    for(i in 1:length(lambdas)){
      BIC[[j]][i] = BIC_function(i, j, 0.5, trial_data, beta_0, betas, f, m)$BIC
    }
    lam_id[j] = which.min(BIC[[j]])
    lam_star[j] = lambdas[which.min(BIC[[j]])]
    
    
  }
  return(list(lam_star = lam_star, lam_id = lam_id, BIC = BIC))
}


# 16. Desparsifed lasso  --------------------------------------------------

despars_lasso_trial = function(idx, trial_data, p, beta_0, beta_hat, f_hat, alpha){
  #' Function to obtain debiased lasso estimates and confidence intervals
  #'
  #' @param idx index of unit of interest, i.e., idx in 1,dots,p
  #' @param trial_data list of trial times, each entry in list is matrix (dxn)
  #' @param p no of dimensions
  #' @param beta_0 vector of intercept estimates 
  #' @param beta_hat estimate of interaction matrix
  #' @param f_hat estimate of nonparametric function
  #' @export  
  
  
  first_deriv = list(); 
  sigma_hat = matrix(NA, p,p);  
  sigma_hat = rep(list(sigma_hat), length(trial_data));
  sigma_hat_check = matrix(NA, p,p);  
  sigma_hat_check = rep(list(sigma_hat_check), length(trial_data));
  theta_lasso = matrix(NA, p,p);
  theta_lasso = rep(list(theta_lasso, length(trial_data)))
  
  W = matrix(NA, p,p);  
  W = rep(list(W), length(trial_data));
  for(i in 1:length(trial_data)){
    print(i)
    data = trial_data[[i]]
    y = data[idx,][2:length(data[idx,])] 
    X1 = data[,(1: (length(data[idx,])-1) )] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
    X1 = t(X1)
    p = dim(data)[1]
    
    
    p_t = (1+exp(-beta_0[idx] - X1 %*% beta_hat[idx,] - f_hat[[idx]]))^(-1)
    first_deriv[[i]] = as.numeric( - ( t(y-p_t) %*% X1 )  ) # / length(y) ## (1 x p)
    vec_pt = as.vector(p_t)
    
    for(l in 1:p){
      for(k in 1:p){
        sigma_hat_check[[i]][l,k] = sum( (vec_pt*(1-vec_pt))*X1[,l]*X1[,k] )
      }
    }
    
    D = diag(vec_pt * (1-vec_pt))
    sigma_hat[[i]] = ( t(X1) %*% D %*% X1 )  #/length(y) #p x p
    
    W[[i]] = sqrt(D) %*% X1
    
  
  # n_eff = length(trial_data)*600
  # first_pn = Reduce("+", first_deriv) / n_eff
  # second_pn1 = Reduce("+", sigma_hat_check)/n_eff
  # second_pn2 = Reduce("+",sigma_hat)/n_eff
  # 
  # winner = lapply(W, function(x) t(x)%*%x)
  # 
  # checks
  # W = sqrt(D) %*% X1
  #check = (t(W) %*% W )/length(y)
  #round(check,8) == round(sigma_hat,8)
  
  # use W as input to standard glmnet
  #library(glmnet)
  # nodewise lasso 
  C = diag(p); tau = NULL; tau2=NULL
  for (j in 1:p){
    #print(j)
    w_minus_j = W[[i]][,-j]
    w_j = W[[i]][,j]
   
    #  # use glmnet for regression problem for each j
    # tryCatch({
    #   gamma_j.fit = cv.glmnet(x=w_minus_j, y = w_j, family="gaussian", alpha=1, intercept=F, standardize = T)
    #   # if succeeds -> exit loop
    #   retry = F
    # }, error = function(e){
    #   #print error message
    #   cat("An error occurred:", e$message, "\n")
    # },
    # finally =  {gamma_j.fit = cv.glmnet(x=w_minus_j, y = w_j, family="gaussian", alpha=1, intercept=F, standardize = T)
    # })
    
    # address issue with glmnet
    lambda = NULL; gamma_j.fit = NULL;
     tryCatch({
    gamma_j.fit = cv.glmnet(x=w_minus_j, y = w_j, family="gaussian", alpha=1, intercept=F, standardize = T)
    }, error = function(e){
      #print error message
      cat("An error occurred:", e$message, "\n")
       # if fails , manually set value of lambda
       lambda <<- sqrt(log(p)/600)
      })
    # Use glmnet with appropraite lambda - either theoretical or cross-validated
    if (!is.null(gamma_j.fit)) {
      gamma_j <- glmnet(x = w_minus_j, y = w_j,  family = "gaussian", alpha = 1,intercept = FALSE, standardize = TRUE,lambda = gamma_j.fit$lambda.min)
    } else {
      gamma_j <- glmnet(x = w_minus_j, y = w_j, family = "gaussian", alpha = 1, intercept = FALSE, standardize = TRUE,lambda = lambda)
    }
    
    #gamma_j = glmnet(x=w_minus_j, y = w_j, family="gaussian", alpha=1, intercept=F, standardize = T, lambda = gamma_j.fit$lambda.min)
    gamma_j = as.vector(gamma_j$beta)
    # populate "phi" matrix
    C[j,-j] = (-1)*t(gamma_j) # i.e. update all but diagonal entry
    tau[j] = as.numeric ( ( t(w_j-w_minus_j%*%as.matrix(gamma_j)) %*% as.matrix(w_j) )) # /length(y) )
    tau2[j] = as.numeric( sigma_hat[[i]][j,j] - sigma_hat[[i]][j,-j] %*% as.matrix(gamma_j) )
  }
  theta_lasso[[i]] = diag(1/tau) %*% C
  }
  
  n_eff = length(trial_data)*600
  first_pn = Reduce("+", first_deriv) / n_eff
  second_pn1 = Reduce("+", sigma_hat_check)/n_eff
  second_pn2 = Reduce("+",sigma_hat)/n_eff
  
  theta_lasso2 = Reduce("+", theta_lasso)/n_eff
  
  beta_deb = beta_hat[idx,] - theta_lasso2%*%as.matrix(first_pn)
  beta_deb = as.numeric(beta_deb)
  
  sigma = sqrt(diag(theta_lasso2 %*% second_pn1 %*% t(theta_lasso2)))
  
  ci_lower = beta_deb - qnorm(1-alpha/2)*sigma/sqrt(length(n_eff))
  ci_upper = beta_deb + qnorm(1-alpha/2)*sigma/sqrt(length(n_eff))
  
  
  return(list(beta_deb = beta_deb, sigma = sigma, ci_lower = ci_lower, ci_upper = ci_upper, theta_lasso2 = theta_lasso2))
}



# 17. Bapla single unit trial with no f -----------------------------------

bapla_single_unit_trial_no_f = function(trial_data, idx, max_iter, tol, lams){
  #'  Function to estimate coefficients of the BAPLA for a given dimension and regularization parameter
  #'
  #' @param data data source 
  #' @param idx (scalar) to indicate for which data stream we want parameter estimates
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param lams Regularisation parameter
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
  
  
  
  ############## Cyclical coordinate descent #############
  
  for(iter in 1:max_iter) {
    #print(iter)
    # current parameter estimates
    beta_tilde <- beta
    beta0_tilde <- beta_0
    
    
    ls_coef = list(); ls_coef = rep(list(ls_coef), length(trial_data))
    denom = ls_coef;
    for(k in 1:length(trial_data)){
      
      X = trial_X[[k]]
      
      # compute p_tilde
      p_tilde = ( 1 + exp(-beta0_tilde - X %*% beta_tilde ) )^-1
      # compute w
      w = p_tilde * (1-p_tilde)
      
      # compute z
      z = beta0_tilde + X %*% beta_tilde + ( (y-p_tilde)/ w )
      
      # Beta matrix update
      for(j in 1:p) {
        
        # Compute partial residual
        # i.e. fitted value excluding contribution from x_{ij}
        
        y_tilde <- beta_0 + X %*% beta - X[,j] * beta[j] 
        
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
      p_tilde = ( 1 + exp(-beta0_tilde - X %*% beta ) )^-1
      # compute w
      w = p_tilde * (1-p_tilde)
      # compute z
      z = beta0_tilde + X %*% beta  + ( (y-p_tilde)/ w )
      
      num[l2] <-as.numeric(crossprod(w, z- (X%*%beta)  ))
      denom[l2] <- sum(w)
    }
    
    beta_0 <- sum(num)/sum(denom)
    
  
    
    #if there is NAs in the beta estimate - break AND set beta = beta_tilde
    #need to set fs and betas to previous as well
    if(sum(is.na(beta))!=0){
      beta = beta_tilde
      beta_0 = beta0_tilde
      break
    }
    
    # Checks for convergence
    if(sum(is.na(beta))==0){ #if no na's in the beta estimate
      if( max((beta_tilde-beta)^2)<tol^2) break
    }
   
  }
  
  #DE-STANDARDISE THE COEFFICIENTS
  sum_term = NULL;
  for(j in 1:p){
    beta[j] = beta[j]/sd(X1[,j])
    sum_term[j] = (x_bar[j]*beta[j])
  }
  beta_0 = beta_0 - sum(sum_term)
  
  
  # Print final coefficients
  return(list(beta0=beta_0, beta=beta, iter = iter))
}



# 18. BIC function no f -------------------------------------------------------

BIC_function_no_f= function(lam_idx, unit_idx, gamma, trial_data, beta_0, betas){
  #' Function to get BIC for real data
  #' @param lam_idx index of lambda value
  #' @param unit_idx index of the unit/neuron
  #' @param gamma hyperparameter for eBIC
  #' @param trial_data data from neuroscience experiment
  
  ######################### obtain data for each trial ######################## 
  trial_X = list(); trial_X = rep(list(trial_X), length(trial_data))
  nll = NULL;
  for(l in 1:length(trial_data)){
    # select data of interest to the unit_id (and trial of interest)
    data = trial_data[[l]]
    y = data[unit_idx,][2:length(data[unit_idx,])] # use
    X1 = data[,(1: (length(data[unit_idx,])-1) )] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
    X1 = t(X1)
    p = dim(data)[1]
    
    #########################################
    
    term1 = y * (beta_0[[lam_idx]] + X1 %*% betas[[lam_idx]] )
    term2 = log(1 + exp(beta_0[[lam_idx]] + X1 %*% betas[[lam_idx]] ) ) 
    nll[l] = sum(term1-term2) 
  }
  
  lik = sum(nll)
  
  num_non_zeroes = sum(betas[[lam_idx]] != 0)
  n_star = 600 * 60 #36060
  pen_term = (num_non_zeroes+1)*log(n_star)
  eBIC_term = 2*gamma*log(p)* (num_non_zeroes+1)
  
  BIC = -2*lik + pen_term #+ eBIC_term
  eBIC = BIC + eBIC_term
  AIC = -2*lik + (2*(num_non_zeroes+1))
  
  return(list(BIC = BIC, AIC = AIC, eBIC = eBIC))
}


# 19. Regularisation parameter selection no f --------------------------------------

choose_lambda_no_f = function(trial_data, p, lambdas, out_storm){
  #' Function to select regularisation parameter based on BIC results (with no f)
  #' 
  BIC = NULL; BIC = rep(list(BIC), p)
  lam_star = NULL; lam_id = NULL;
  out = list();
  for(j in 1:p){
    print(j)
    b = length(lambdas)*j
    if(j==1){
      # select results from storm
      out  = out_storm[1:b]
    }
    if(j!=1){
      # select results from storm
      out = out_storm[((b-length(lambdas))+1):b]
    }
    betas = map(out, 1)
    beta_0 = map(out, 2) 
    # make betas matrix not vector
    betas = lapply(betas, as.matrix)
    
    for(i in 1:length(lambdas)){
      BIC[[j]][i] = BIC_function_no_f(i, j, 0.5, trial_data, beta_0, betas)$BIC
    }
    lam_id[j] = which.min(BIC[[j]])
    lam_star[j] = lambdas[which.min(BIC[[j]])]
    
    
  }
  return(list(lam_star = lam_star, lam_id = lam_id, BIC = BIC))
}




# 20. Desparsified lasso for trial data with BIC --------------------------


despars_lasso_trial_bic = function(idx, trial_data, p, beta_0, beta_hat, f_hat, alpha){
  #' Function to obtain debiased lasso estimates and confidence intervals
  #'
  #' @param idx index of unit of interest, i.e., idx in 1,dots,p
  #' @param trial_data list of trial times, each entry in list is matrix (dxn)
  #' @param p no of dimensions
  #' @param beta_0 vector of intercept estimates 
  #' @param beta_hat estimate of interaction matrix
  #' @param f_hat estimate of nonparametric function
  #' @export  
  
  
  first_deriv = list(); 
  sigma_hat = matrix(NA, p,p);  
  sigma_hat = rep(list(sigma_hat), length(trial_data));
  sigma_hat_check = matrix(NA, p,p);  
  sigma_hat_check = rep(list(sigma_hat_check), length(trial_data));
  theta_lasso = matrix(NA, p,p);
  theta_lasso = rep(list(theta_lasso, length(trial_data)))
  removed_trial_ids = list();
  
  W = matrix(NA, p,p);  
  W = rep(list(W), length(trial_data));
  for(i in 1:length(trial_data)){
    print(i)
    data = trial_data[[i]]
    y = data[idx,][2:length(data[idx,])] 
    X1 = data[,(1: (length(data[idx,])-1) )] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
    X1 = t(X1)
    p = dim(data)[1]
    
    
    p_t = (1+exp(-beta_0[idx] - X1 %*% beta_hat[idx,] - f_hat[[idx]]))^(-1)
    first_deriv[[i]] = as.numeric( - ( t(y-p_t) %*% X1 )  ) # / length(y) ## (1 x p)
    vec_pt = as.vector(p_t)
    
    for(l in 1:p){
      for(k in 1:p){
        sigma_hat_check[[i]][l,k] = sum( (vec_pt*(1-vec_pt))*X1[,l]*X1[,k] )
      }
    }
    
    D = diag(vec_pt * (1-vec_pt))
    sigma_hat[[i]] = ( t(X1) %*% D %*% X1 )  #/length(y) #p x p
    
    W[[i]] = sqrt(D) %*% X1
    
    
    
    # use W as input to standard glmnet
    #library(glmnet)
    # nodewise lasso 
    C = diag(p); tau = NULL; tau2=NULL
    for (j in 1:p){
      print(j)
      w_minus_j = W[[i]][,-j]
      w_j = W[[i]][,j]
      # use glmnet for regression problem for each j
      #gamma_j.fit = cv.glmnet(x=w_minus_j, y = w_j, family="gaussian", alpha=1, intercept=F, standardize = T)
      
      fit = glmnet(x=w_minus_j, y = w_j, family="gaussian", alpha=1, intercept=F, 
                   standardize = T)
      
      #if()
        
      n = 600;
      
      # Compute BIC safely
      bic_values <- sapply(1:length(fit$lambda), function(i) {
        lambda_val <- fit$lambda[i]
        
        # handling NA error
        preds <- tryCatch({
          predict(fit, newx = w_minus_j, s = lambda_val)
        }, error = function(e) {
          warning(paste("Prediction failed at lambda index", i, ":", e$message))
          return(rep(NA, n))
        })
        
        if (anyNA(preds)) {
          return(NA)  # Skip this lambda if prediction failed
        }
        
        # Compute log-likelihood and BIC
        mse <- mean((w_j - preds)^2)
        log_likelihood <- -0.5 * n * log(mse)
        k <- sum(fit$beta[, i] != 0)
        bic <- -2 * log_likelihood + k * log(n)
        return(bic)
      })
      
      # check: if no issue with this particular trial then continue:
      if (all(is.na(bic_values)) ) {
        # exit loop
        print(paste("There is a problem with estimation at trial", i, "- Neuron", j))
        break
      }
        #stop("All BIC values are NA — model fitting or prediction failed.")
      #}
      
      # Select lambda with minimum BIC
      best_lambda_index <- which.min(bic_values)
      bic_lam <- fit$lambda[best_lambda_index]
      
      gamma_j = glmnet(x=w_minus_j, y = w_j, family="gaussian", alpha=1, intercept=F, 
                       standardize = T, lambda = bic_lam)
      
      gamma_j = as.vector(gamma_j$beta)
      # populate "phi" matrix
      C[j,-j] = (-1)*t(gamma_j) # i.e. update all but diagonal entry
      tau[j] = as.numeric ( ( t(w_j-w_minus_j%*%as.matrix(gamma_j)) %*% as.matrix(w_j) )) # /length(y) )
      tau2[j] = as.numeric( sigma_hat[[i]][j,j] - sigma_hat[[i]][j,-j] %*% as.matrix(gamma_j) )
      }
    if(length(tau2)==p){
      theta_lasso[[i]] = diag(1/tau) %*% C
    } else{
      theta_lasso[[i]]=NA}
  }
  
  removed_trial_ids = which(is.na(theta_lasso))
  
  # remove trials which had NAs
  theta_lasso = theta_lasso[!is.na(theta_lasso)]
  # update effective sample size to removed trials which were removed
  n_eff = length(theta_lasso)*600
  first_pn = Reduce("+", first_deriv) / n_eff
  second_pn1 = Reduce("+", sigma_hat_check)/n_eff
  second_pn2 = Reduce("+",sigma_hat)/n_eff
  
  
  theta_lasso2 = Reduce("+", theta_lasso)/n_eff
  
  beta_deb = beta_hat[idx,] - theta_lasso2%*%as.matrix(first_pn)
  beta_deb = as.numeric(beta_deb)
  
  sigma = sqrt(diag(theta_lasso2 %*% second_pn1 %*% t(theta_lasso2)))
  
  ci_lower = beta_deb - qnorm(1-alpha/2)*sigma/sqrt(length(n_eff))
  ci_upper = beta_deb + qnorm(1-alpha/2)*sigma/sqrt(length(n_eff))
  
  
  return(list(beta_deb = beta_deb, sigma = sigma, ci_lower = ci_lower, 
              ci_upper = ci_upper, theta_lasso2 = theta_lasso2, rm_trials = removed_trial_ids))
}


# 21. Combine interaction matrix estimates for plotting purposes ----------

combine_estimates = function(est, sorted_ids, p_est, id_vec){
  #' Function to combine estimates from visual stimulus and wheel experiments
  #' @param est Estimate of the interaction matrix of dimension p_est x p_est
  #' @param sorted_ids neuron ids used in both visual and wheel experiments
  #' @param p_est dimension of the estimated interaction matrix
  #' @param id_vec vector of neuron ids corresponding to the estimated interaction matrix "est"
  
  # set up empty matrix
  network_combined = matrix(data = 0, nrow = length(sorted_ids), ncol=length(sorted_ids))
  
  x = matrix(data = 0, nrow = length(sorted_ids), ncol=length(sorted_ids))
  x[1:p_est, 1:p_est] = est
  rownames(x) = 1:82
  
  cols_in = 1:p_est
  
  flag = sorted_ids %in% id_vec
  
  idx = which(flag == T)
  # target columns for output matrix
  cols_out = idx
  
  for(i in 1:length(sorted_ids)){
    # identify row of matrix corresponding to this neuron in the original estimate 
    # of the interaction matrix
    name = which(id_vec == sorted_ids[i])
    # if neuron is in current estimate of interaction matrix (est)
    if(sorted_ids[i] %in% id_vec){
      row_data = x[name, cols_in]
      network_combined[i,cols_out] = row_data
    }
  }
  
  return(network_combined)
}


# 22. Results for inference procedure -------------------------------------

inference_results = function(res, beta_hat){
  #' Function to format results from inference procedure
  #' @param res list of results (of length p) from STORM containing debiased estimate and upper/lower CI values 
  #' @param beta_hat estimate of the interaction matrix obtained from the regularised estimator 
  non_zero_indices = list(); mat = matrix(NA, nrow=dim(beta_hat)[1], ncol=dim(beta_hat)[1]);
  for(i in 1:length(res)){
    check_0 = ifelse(res[[i]]$lower < 0 & res[[i]]$upper>0, T, F)
    non_zero_indices[[i]] = which(check_0==F)
    if(length(non_zero_indices[[i]])!=0){
      # set non zero entries to 1
      mat[i, non_zero_indices[[i]]] = 1
    }
  }
  # put into adjacency matrix format
  beta_hat[beta_hat!=0]=1
  # set NAs to zero
  mat[is.na(mat)]=0
  out = mat == beta_hat
  ids = which(out == F) # edges to be  greyed out in final plots
  id_mat = arrayInd(ids, dim(beta_hat))
  return(list(ids=ids, mat=mat, beta_hat = beta_hat, n_inds= non_zero_indices, id_mat = id_mat))
}



count_edges = function(beta_matrix, arrow_size, node_size, region_col, keep_regions) {
  #' Plot interaction matrix for specific brain regions only (e.g., MB and POL)
  #'
  #' @param beta_matrix p x p interaction matrix
  #' @param arrow_size arrow size in igraph plot
  #' @param node_size node size in igraph plot
  #' @param region_col vector of region names for each node
  #' @param keep_regions character vector of regions to retain (e.g., c("MB", "POL"))
  #' @export
  
  # Create full graph
  graph <- graph_from_adjacency_matrix(100 * beta_matrix, mode = "directed", weighted = TRUE, diag = FALSE)
  
  # Set edge and node properties
  V(graph)$region <- region_col
  V(graph)$color <- region_col
  V(graph)$label.color <- "black"
    V(graph)$size <- node_size
    
    E(graph)$color <- ifelse(E(graph)$weight > 0, "black", "black")
    E(graph)$lty <- ifelse(E(graph)$weight < 200 & E(graph)$weight != 0, "solid", "dashed")
    
    # Filter: keep only nodes in MB or POL
    keep_nodes <- which(names(region_col) %in% keep_regions)
    subgraph <- induced_subgraph(graph, keep_nodes)
    
    edge_count <- ecount(subgraph)
    return(edge_count)
}
no_edgs = function(mat){
  inhib = length(mat[mat==2]) # inhibitory edges  
  excit = length(mat[mat==1]) # excitatory edges
  
  p_in = round((inhib/(inhib+excit))*100, 2)
  p_ex = round((excit/(inhib+excit))*100, 2)
  return(list(inhib= p_in, ex = p_ex, total = inhib+excit))
}
count_within_edges = function(mat, regions, col_region){
  edges = NULL;
  for(i in 1:length(regions)){
    edges[i] = count_edges(mat, arrow_size = 0.5, node_size = 2,
                           region_col = col_region, keep_regions = c(regions[i]))
  }
  names(edges) = regions
  return(edges)
}



# 23. Function to get firing rates and changepoints for raw data ----------


get_firing_rate_matrix = function(units, time_bins){
  #' Function to calculate firing rate (in Hz) from raw spikes
  #' @param units (list) nested list of spike times for each neuron. Each neuron has 60 trials
  #' @param time_bins (numeric) bins for downsampling spike times
  #' #' @return A list with the following components:
  #' \describe{
  #'   \item{firing_rate}{Matrix of firing rates of dimension p x n - each row gives firing rate for particular neuron}
  #'   \item{cpts}{List of detected change points in the firing rate for each neuron.}
  #'   \item{stat_ids}{Indices of `stationary' neurons}
  #'   \item{non_stat_ids}{Indices of `non-stationary' neurons.}
  #' }
  
  # calculate firing rate
  firing_rate = matrix(data = NA, nrow = length(units), ncol = length(time_bins)-1)
  smooth_fr = matrix(data = NA, nrow = length(units), ncol = length(time_bins)-1)
  # place holder for change points
  cpts = list();
  for(i in 1:length(units)){
    firing_rate[i,] = firing_rate_func(units[[i]], time_bins, bin_size)
    smooth_fr[i,] = rollmean(firing_rate[i,], k = 5, fill = NA)
    cpts[[i]] = cpts(cpt.var(firing_rate[i,], method = "PELT"))
  }
  
  # get IDs of statioary and non-stationary neurons
  non_stat_ids = which(lengths(cpts)!=0) # i.e. which neurons have `changepoints`
  stat_ids = which(lengths(cpts)==0)
  
  return(list(firing_rate = firing_rate, cpts = cpts,
              stat_ids = stat_ids, non_stat_ids = non_stat_ids))
}


# 24. Function to plot firing rates and changepoint of example neu --------

plot_example_with_cpts = function(unit_id, time_bins, out_left, out_right){
  #' Function to plot firing rates and change points of example neurons
  #' @param unit_id (numeric) ID of neuron to plot
  #' @param time_bins (numeric) bins for downsampling spike times
  #' @param out_left (list) result from get_firing_rate_matrix function for left trials.
  #' @param out_right (list) result from get_firing_rate_matrix function for right trials.
  par(mfrow = c(1,2))
  xs = time_bins[-length(time_bins)]
  plot(xs, out_left$firing_rate[unit_id,], type="l", ylim = c(0,150), 
       main = paste("Unit", unit_id, " - Left Trials"), ylab = "Firing rate (Hz)", xlab = "Time (ms)")
  abline(v = c(time_bins[out_left$cpts[[unit_id]]]), col="red", lty = "dashed", lwd = 2)
  plot(xs, out_right$firing_rate[unit_id,], type="l", ylim = c(0,150), 
       main = paste("Unit", unit_id, " - Right Trials"), 
       ylab = "Firing rate (Hz)", xlab = "Time (ms)")
  abline(v = c(time_bins[out_right$cpts[[unit_id]]]), col="red", lty = "dashed", lwd = 2)
}


# 25. Function to get fixed layout for bipartite graph --------------------

get_fixed_layout_bi = function(beta_matrix, region_col, split_nodes=F, node_size){
  #' Function to get fixed layout for bipartite graph
  #' @param beta_matrix (numeric) d x d interaction matrix to be plotted
  #' @param region_col (character) vector of colours associated with each neuron ("black" or "white)
  #' @param split_nodes (logical) Default is False. If true, will split nodes across two lines
  #' @param node_size (numeric) size of nodes in igraph plot (usually 7-10 appropriate)
  graph <- graph_from_adjacency_matrix(100*beta_matrix, mode = "directed", weighted = T, diag = F)
  # Set edge colors based on the sign of the matrix values
  E(graph)$color <- ifelse(E(graph)$weight > 0, "black", "black")
  
  # Set edge type based on sign of the matrix values
  E(graph)$lty <- ifelse(E(graph)$weight <200 & E(graph)$weight!=0 , "solid", "dashed")
  
  
  # Set vertex color to white
  V(graph)$color <- region_col
  # Set vertex label color to black
  V(graph)$label.color <- "black"
    V(graph)$size <- node_size
    
    V(graph)$type = unname(region_col)
    
    V(graph)$type = ifelse(V(graph)$type=="black", 1, 0)
    
    if(split_nodes == T){
      types = V(graph)$type 
      # split into groups
      group0 = which(types==1)
      group1 = which(types == 0)
      
      # For each group, split into two halves
      group0_split <- split(group0, rep(1:2, length.out = length(group0)))
      group1_split <- split(group1, rep(1:2, length.out = length(group1)))
      
      # Define positions
      layout <- matrix(NA, nrow = vcount(graph), ncol = 2)
      
      # Assign x positions evenly spaced within each row
      layout[group0_split[[1]], 1] <- seq(1, length(group0_split[[1]]))
      layout[group0_split[[2]], 1] <- seq(1, length(group0_split[[2]]))
      layout[group1_split[[1]], 1] <- seq(1, length(group1_split[[1]]))
      layout[group1_split[[2]], 1] <- seq(1, length(group1_split[[2]]))
      
      # Assign y positions for rows
      layout[group0_split[[1]], 2] <- 0     # bottom row 1
      layout[group0_split[[2]], 2] <- 1     # bottom row 2
      layout[group1_split[[1]], 2] <- 3     # top row 1
      layout[group1_split[[2]], 2] <- 4     # top row 2
    }
    
    if(split_nodes==F){
      # layout as normal "bipartite"
      layout = layout_as_bipartite(graph)
    }
    return(layout)
}


# 26. Function to plot bipartite graph ------------------------------------

plot_bipartite = function(beta_matrix, arrow_size, node_size, region_col, fixed_layout){
  # Create a graph from the matrix
  graph <- graph_from_adjacency_matrix(100*beta_matrix, mode = "directed", weighted = T, diag = F)
  # Set edge colors based on the sign of the matrix values
  E(graph)$color <- ifelse(E(graph)$weight > 0, "black", "black")
  
  # Set edge type based on sign of the matrix values
  E(graph)$lty <- ifelse(E(graph)$weight <200 & E(graph)$weight!=0 , "solid", "dashed")
  
  
  # Set vertex color to white
  V(graph)$color <- region_col
  # Set vertex label color to black
  V(graph)$label.color <- "black"
    V(graph)$size <- node_size
    
    V(graph)$type = unname(region_col)
    
    V(graph)$type = ifelse(V(graph)$type=="black", 1, 0)
    
    
    
    edge_count <- ecount(graph)
    print(paste("Number of edges in the graph:", edge_count))
    
    V(graph)$color = unname(region_col)
    
    
    plot(graph, edge.arrow.size = arrow_size,
         vertex.label = NA,  edge.curved=0.2, layout = fixed_layout, 
         vertex.label.cex = 0.7)
}


# 27. Function to plot bipartite graph with comparison edges colou --------
plot_bipartite_compar = function(beta_matrix, comparison_matrix,  arrow_size, node_size, region_col, fixed_layout){
  
  #' Function to plot bipartite graph with comparison matrix. Edges common to both matrices will be coloured in blue.
  #' @param beta_matrix d x d interaction matrix (e.g. glm estimate)
  #' @param comparison_matrix d x d comparison matrix (e.g. BAPLA esitmate)
  
  # Create a graph from the matrix
  graph <- graph_from_adjacency_matrix(100*beta_matrix, mode = "directed", weighted = T, diag = F)
  # Set edge colors based on the sign of the matrix values
  E(graph)$color <- ifelse(E(graph)$weight > 0, "black", "black")
  
  # Set edge type based on sign of the matrix values
  E(graph)$lty <- ifelse(E(graph)$weight <200 & E(graph)$weight!=0 , "solid", "dashed")
  
  
  # Create a graph from the comparison_matrix
  comparison_graph <- graph_from_adjacency_matrix(100 * comparison_matrix, mode = "directed", weighted = TRUE, diag = FALSE)
  
  # Identify edges in both graphs
  edges_beta <- as_edgelist(graph)
  edges_comparison <- as_edgelist(comparison_graph)
  
  # Count matching edges
  matching_edges_count <- sum(apply(edges_beta, 1, function(edge) {
    any(apply(edges_comparison, 1, function(comp_edge) all(comp_edge == edge)))
  }))
  
  # Set edge colors: blue for edges in both graphs, black otherwise
  E(graph)$color <- apply(edges_beta, 1, function(edge) {
    if (any(apply(edges_comparison, 1, function(comp_edge) all(comp_edge == edge)))) {
      "blue2"
    } else {
      "black"
    }
  })
  
  
  
  # Set vertex color to white
  V(graph)$color <- region_col
  # Set vertex label color to black
  V(graph)$label.color <- "black"
    V(graph)$size <- node_size
    
    V(graph)$type = unname(region_col)
    
    V(graph)$type = ifelse(V(graph)$type=="black", 1, 0)
    
    
    
    edge_count <- ecount(graph)
    print(paste("Number of edges in the graph:", edge_count))
    print(paste("Number of matching edges:", matching_edges_count))
    
    V(graph)$color = unname(region_col)
    
    
    plot(graph, edge.arrow.size = arrow_size,
         vertex.label = NA,  edge.curved=0.2, layout = fixed_layout, 
         vertex.label.cex = 0.7)
}



# 28. Desparsified lasso with cross-validation lambda selection -----------

despars_lasso_trial_cv = function(idx, trial_data, p, beta_0, beta_hat, f_hat, alpha){
  #' Function to obtain debiased lasso estimates and confidence intervals
  #'
  #' @param idx index of unit of interest, i.e., idx in 1,dots,p
  #' @param trial_data list of trial times, each entry in list is matrix (dxn)
  #' @param p no of dimensions
  #' @param beta_0 vector of intercept estimates 
  #' @param beta_hat estimate of interaction matrix
  #' @param f_hat estimate of nonparametric function
  #' @export  
  
  
  first_deriv = list(); 
  sigma_hat = matrix(NA, p,p);  
  sigma_hat = rep(list(sigma_hat), length(trial_data));
  sigma_hat_check = matrix(NA, p,p);  
  sigma_hat_check = rep(list(sigma_hat_check), length(trial_data));
  theta_lasso = matrix(NA, p,p);
  theta_lasso = rep(list(theta_lasso, length(trial_data)))
  
  removed_trial_ids = list();
  
  W = matrix(NA,p,p);  
  W = rep(list(W), length(trial_data));
  for(i in 1:length(trial_data)){
    print(i)
    data = trial_data[[i]]
    y = data[idx,][2:length(data[idx,])] 
    X1 = data[,(1: (length(data[idx,])-1) )] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
    X1 = t(X1)
    p = dim(data)[1]
    
    
    p_t = (1+exp(-beta_0[idx] - X1 %*% beta_hat[idx,] - f_hat[[idx]]))^(-1)
    first_deriv[[i]] = as.numeric( - ( t(y-p_t) %*% X1 )  ) # / length(y) ## (1 x p)
    vec_pt = as.vector(p_t)
    
    for(l in 1:p){
      for(k in 1:p){
        sigma_hat_check[[i]][l,k] = sum( (vec_pt*(1-vec_pt))*X1[,l]*X1[,k] )
      }
    }
    
    D = diag(vec_pt * (1-vec_pt))
    sigma_hat[[i]] = ( t(X1) %*% D %*% X1 )  #/length(y) #p x p
    
    W[[i]] = sqrt(D) %*% X1
    
    
    # use W as input to standard glmnet
    #library(glmnet)
    # nodewise lasso 
    C = diag(p); tau = NULL; tau2=NULL
    for (j in 1:p){
      #print(j)
      w_minus_j = W[[i]][,-j]
      # centre:
      #w_minus_j_centred = scale(w_minus_j, center=T, scale = F)
      w_j = W[[i]][,j]

      
      # address issue with glmnet
      lambda = NULL; gamma_j.fit = NULL;
      tryCatch({
        gamma_j.fit = cv.glmnet(x=w_minus_j, y = w_j, family="gaussian", alpha=1, intercept = F)
      }, error = function(e){
        #print error message
        cat("An error occurred:", e$message, "\n")
        # if fails , manually set value of lambda
        # lambda <<- sqrt(log(p)/600)
      })
      # Use glmnet with appropraite lambda - either theoretical or cross-validated
      if (!is.null(gamma_j.fit)) {
        gamma_j <- glmnet(x = w_minus_j, y = w_j,  family = "gaussian", alpha = 1,lambda = gamma_j.fit$lambda.min, intercept = F)
      
      #gamma_j = glmnet(x=w_minus_j, y = w_j, family="gaussian", alpha=1, intercept=F, standardize = T, lambda = gamma_j.fit$lambda.min)
      gamma_j = as.vector(gamma_j$beta)
      # populate "phi" matrix
      C[j,-j] = (-1)*t(gamma_j) # i.e. update all but diagonal entry
      tau[j] = as.numeric ( ( t(w_j-w_minus_j%*%as.matrix(gamma_j)) %*% as.matrix(w_j) )) # /length(y) )
      tau2[j] = as.numeric( sigma_hat[[i]][j,j] - sigma_hat[[i]][j,-j] %*% as.matrix(gamma_j) )
      } else{tau[j]=NA}
      
    }
    # if failed for any p - remove trial
    if(any(is.na(tau)) == F){
      theta_lasso[[i]] = diag(1/tau) %*% C
    } else{theta_lasso[[i]] = NA}
  }
  
  removed_trial_ids = which(is.na(theta_lasso))
  
  # remove trials which had NAs
  theta_lasso = theta_lasso[!is.na(theta_lasso)]
  # update effective sample size to removed trials which were removed
  n_eff = length(theta_lasso)*600
  
  first_pn = Reduce("+", first_deriv) / n_eff
  second_pn1 = Reduce("+", sigma_hat_check)/n_eff
  second_pn2 = Reduce("+",sigma_hat)/n_eff
  
  theta_lasso2 = Reduce("+", theta_lasso)/n_eff
  
  beta_deb = beta_hat[idx,] - theta_lasso2%*%as.matrix(first_pn)
  beta_deb = as.numeric(beta_deb)
  
  sigma = sqrt(diag(theta_lasso2 %*% second_pn1 %*% t(theta_lasso2)))
  
  ci_lower = beta_deb - qnorm(1-alpha/2)*sigma/sqrt(length(n_eff))
  ci_upper = beta_deb + qnorm(1-alpha/2)*sigma/sqrt(length(n_eff))
  
  
  return(list(beta_deb = beta_deb, sigma = sigma, ci_lower = ci_lower, 
              ci_upper = ci_upper, theta_lasso2 = theta_lasso2, rm_trials = removed_trial_ids))
}



# updated inference results -----------------------------------------------

inference_results_new = function(res, beta_hat){
  #' Function to format results from inference procedure
  #' @param res list of results (of length p) from STORM containing debiased estimate and upper/lower CI values 
  #' @param beta_hat estimate of the interaction matrix obtained from the regularised estimator 
  non_zero_indices = list(); mat = matrix(NA, nrow=dim(beta_hat)[1], ncol=dim(beta_hat)[1]);
  no_trials_removed = NULL; id_removed_trials = list();
  for(i in 1:length(res)){
    check_0 = ifelse(res[[i]]$lower < 0 & res[[i]]$upper>0, T, F)
    non_zero_indices[[i]] = which(check_0==F)
    if(length(non_zero_indices[[i]])!=0){
      # set non zero entries to 1
      mat[i, non_zero_indices[[i]]] = 1
    }
    
    no_trials_removed[i] = length(res[[i]]$rm_trials)
    id_removed_trials[[i]] = res[[i]]$rm_trials
    
  }
  # put into adjacency matrix format
  beta_hat[beta_hat!=0]=1
  # set NAs to zero
  mat[is.na(mat)]=0
  out = mat == beta_hat
  ids = which(out == F) # edges to be  greyed out in final plots
  id_mat = arrayInd(ids, dim(beta_hat))
  return(list(ids=ids, mat=mat, beta_hat = beta_hat, n_inds= non_zero_indices, id_mat = id_mat,
              id_removed_trials = id_removed_trials, no_trials_removed = no_trials_removed))
}

