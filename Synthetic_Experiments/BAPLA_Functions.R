
# Functions for BAPLA Paper -----------------------------------------------


# 1. Add weights to graphs generated from igraph --------------------------

add_weights = function(mat, rho){
  #' Function to add weights to simulated graph structure
  #'
  #' @param mat p x p matrix with entries either 0 or 1
  #' @param rho (scalar) to replace '1's in interaction matrix
  #' @export
  
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      mat[i,j] =   mat[i,j]*sample(c(-rho,rho),1)
    }
  }
  return(mat)
}


# 2. Generate chain graph strucutre ---------------------------------------

chain_graph = function(p, rho){
  #' Function to generate the interaction matrix for the chain graph model
  #'
  #' @param p dimension of the model/number of nodes on the graph
  #' @param rho (scalar) to replace '1's in interaction matrix
  #' @export
  
  mat = diag(nrow = p, ncol=p)
  
  # Fill the entries above the diagonal with positive values
  n = dim(mat)[1]
  for (i in 1:(n-1)) {
    mat[i, i+1] <- rho
  }
  
  # Fill the entries below the diagonal with negative values
  for (i in 2:n) {
    mat[i, i-1] <- -rho
  }
  return(mat)
}


# 3. Plot interaction matrix as igraph ------------------------------------

plot_graph = function(beta_matrix, arrow_size, node_size){
  #' Function to plot the interaction matrix as an igraph plot
  #'
  #' @param beta_matrix p x p interaction matrix
  #' @param arrow_size size of arrow to be specified in igraph plot
  #' @param node_size size of node to be specified in igraph plot
  #' @export
  #
  #
  beta_matrix = t(beta_matrix)
  # Create a graph from the matrix
  graph <- graph_from_adjacency_matrix(100*beta_matrix, mode = "directed", weighted = T, diag = F)
  # Set edge colors based on the sign of the matrix values
  #E(graph)$color <- ifelse(E(graph)$weight > 0, "red", "blue")
  # Set vertex color to white
  # Set edge colors based on the sign of the matrix values
  E(graph)$color <- ifelse(E(graph)$weight > 0, "black", "black")
  
  # Set edge type based on sign of the matrix values
  E(graph)$lty <- ifelse(E(graph)$weight >0, "solid", "dashed")
  
  
  V(graph)$color <- "white"
    # Set vertex label color to black
  V(graph)$label.color <- "black"
    V(graph)$size <- node_size
    # Plot the graph
    #plot(graph, edge.arrow.size = 0.5, vertex.label = NA, edge.width = 2, edge.curved = 0.2)
    #set.seed(123)
    # Set the layout to circular
    layout <- layout_in_circle(graph)
    plot(graph, edge.arrow.size = arrow_size, vertex.label = seq(1,dim(beta_matrix)[1]), edge.width = 2, edge.curved=0.2)
}



# 4a. Simulate data with normal f ------------------------------------------

Simulate_data_add_norm = function(p, N, beta, beta_0, cut_off, mu, sigma){
  
  #' Simualte binary time series according to BAPLA model with f as a normal.
  #'
  #' @param p Number of dimensions
  #' @param N Number of time points
  #' @param beta p x p linear coefficient matrix  
  #' @param beta_0 1 x p vector of intercept terms
  #' @param cut_off (scalar). Number of samples to discard as "burn in" period. 
  #' @param mu (scalar) mean for normal f
  #' @param sigma (scalar) sd for normal f
  #' @export  
  
  # Set up empty matrix to store data in
  y = matrix(data=NA, nrow=p, ncol=N)
  # Each column represents data from each "unit" at a given time point
  for(i in 1:p){
    # generate y_0 for each unit using independent bernoullis'
    # store output in first columns of Y matrix
    y[i,1] = rbinom(1, 1, invlogit(beta_0[i])) 
  }
  # simulate data based on model parameters
  # Loop over time points
  for(j in 2:N){
    # Loop over dimensions
    for(i in 1:p){
      # extract vector of previous values (i.e. y_{t-1} values)
      y_minus = as.matrix(y[,j-1])
      y[i,j] = rbinom(1, 1 , prob = invlogit(beta_0[i]+t(beta[i,])%*%y_minus + (dnorm(j/N, mean = mu, sd=sigma)-1))) 
    }
  }
  
  # get rid of first 100 samples => "burn in"
  #new_y = y[, -(1:cut_off)]
  return(y)
}



# 4b. Simulate data with gamma f -------------------------------------------

Simulate_data_add_gamma = function(p, N, beta, beta_0, cut_off, shape, scale){
  
  #' Simulate binary time series according to BAPLA model with f as a gamma
  #'
  #' @param p Number of dimensions
  #' @param N Number of time points
  #' @param beta p x p linear coefficient matrix  
  #' @param beta_0 1 x p vector of intercept terms
  #' @param cut_off (scalar). Number of samples to discard as "burn in" period. 
  #' @param shape (scalar) shape parameter for gamma f
  #' @param scale (scalar) scale parameter for gamma f
  #' @param const scalaing for normal f
  #' @export  
  
  # Set up empty matrix to store data in
  y = matrix(data=NA, nrow=p, ncol=N)
  # Each column represents data from each "unit" at a given time point
  for(i in 1:p){
    # generate y_0 for each unit using independent bernoullis'
    # store output in first columns of Y matrix
    y[i,1] = rbinom(1, 1, invlogit(beta_0[i])) 
  }
  # simulate data based on model parameters
  # Loop over time points
  for(j in 2:N){
    # Loop over dimensions
    for(i in 1:p){
      # extract vector of previous values (i.e. y_{t-1} values)
      y_minus = as.matrix(y[,j-1])
      y[i,j] = rbinom(1, 1 , prob = invlogit(beta_0[i]+t(beta[i,])%*%y_minus + (dgamma(j/N, shape = shape, scale = scale)-1))) 
    }
  }
  
  # get rid of first 100 samples => "burn in"
  #new_y = y[, -(1:cut_off)]
  return(y)
}

# 4c. Simulate data with gamma and normal f -------------------------------


Simulate_data_add_gamma_norm = function(p, N, beta, beta_0, cut_off, shape, scale, mu, sigma){
  
  #' binary time series simulation Function.
  #'
  #' @param p Number of dimensions
  #' @param N Number of time points
  #' @param beta p x p linear coefficient matrix  
  #' @param beta_0 1 x p vector of intercept terms
  #' @param cut_off (scalar). Number of samples to discard as "burn in" period. 
  #' @export  
  
  # Set up empty matrix to store data in
  y = matrix(data=NA, nrow=p, ncol=N)
  # Each column represents data from each "unit" at a given time point
  for(i in 1:p){
    # generate y_0 for each unit using independent bernoullis'
    # store output in first columns of Y matrix
    y[i,1] = rbinom(1, 1, invlogit(beta_0[i])) 
  }
  # simulate data based on model parameters
  # Loop over time points
  for(j in 2:N){
    # Loop over dimensions
    for(i in 1:p){
      # extract vector of previous values (i.e. y_{t-1} values)
      y_minus = as.matrix(y[,j-1])
      if(i<=p/2){
        # first half add gamma
        y[i,j] = rbinom(1, 1 , prob = invlogit(beta_0[i]+t(beta[i,])%*%y_minus + (dgamma(j/N, shape = shape, scale = scale)-1))) 
      }
      if(i>p/2){
        # second half add normal
        y[i,j] = rbinom(1, 1 , prob = invlogit(beta_0[i]+t(beta[i,])%*%y_minus + (dnorm(j/N, mean = mu, sd=sigma)-1)))
      }
    }
  }
  
  # get rid of first 100 samples => "burn in"
  #new_y = y[, -(1:500)]
  return(y)
}


# Simulate data RL --------------------------------------------------------
# 4d. simulate data with no f
Simulate_data_RL = function(p, N, beta, beta_0, cut_off){
  
  #' Simualte binary time series according to BAPLA model with f as a normal.
  #'
  #' @param p Number of dimensions
  #' @param N Number of time points
  #' @param beta p x p linear coefficient matrix  
  #' @param beta_0 1 x p vector of intercept terms
  #' @param cut_off (scalar). Number of samples to discard as "burn in" period. 
  #' @param mu (scalar) mean for normal f
  #' @param sigma (scalar) sd for normal f
  #' @export  
  
  # Set up empty matrix to store data in
  y = matrix(data=NA, nrow=p, ncol=N)
  # Each column represents data from each "unit" at a given time point
  for(i in 1:p){
    # generate y_0 for each unit using independent bernoullis'
    # store output in first columns of Y matrix
    y[i,1] = rbinom(1, 1, invlogit(beta_0[i])) 
  }
  # simulate data based on model parameters
  # Loop over time points
  for(j in 2:N){
    # Loop over dimensions
    for(i in 1:p){
      # extract vector of previous values (i.e. y_{t-1} values)
      y_minus = as.matrix(y[,j-1])
      y[i,j] = rbinom(1, 1 , prob = invlogit(beta_0[i]+t(beta[i,])%*%y_minus))  
    }
  }
  
  # get rid of first 100 samples => "burn in"
  #new_y = y[, -(1:cut_off)]
  return(y)
}

# 5. Estimation for all units --------------------------------------

Estimate_coefficients_logit <- function(data_source, p, max_iter, tol, lams, m){
  #'  Function to estimate coefficients of the BAPLA model for all dimensions and regularization parameter
  #'
  #' @param data_source data
  #' @param p dimension
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param lams Regularisation parameter
  #' @param m number of basis coefficients for splines
  #' @export  
  p = dim(data_source)[1]
  #print(p)
  beta0_hat=NULL; beta_hat = matrix(NA, p ,p); f = list(); f_hat = rep(list(f), p); 
  c_hat = list(); c_hat = rep(list(c_hat), p); iter = NULL; beta0_alt = NULL;
  r=list();c_r = list(); b_r=list();
  for (i in 1:p){
    print(i)
    res = Estimate_coefficients_with_rate(data_source, i, max_iter, tol, lams, m)
    beta0_hat[i] = res$beta0
    beta_hat[i,] = res$beta
    f_hat[[i]] = res$f
    c_hat[[i]] = res$c
    iter[i] = res$iter
    beta0_alt[i] = res$beta_alt
    r[[i]] = res$r
    c_r[[i]] = res$c_r
    b_r[[i]] = res$b_r
  }
  
  return(list(beta0_hat=beta0_hat, beta_hat=beta_hat, f = f_hat, c_hat = c_hat, 
              iter = iter, beta_alt = beta0_alt, r=r, c_r=c_r, b_r = b_r))
}
# 6. Estimation for single unit ---------------------------------------------
Estimate_coefficients_with_rate = function(data, idx, max_iter, tol, lams, m){
  #'  Function to estimate coefficients of the BAPLA for a given dimension and regularization parameter
  #'
  #' @param data data source 
  #' @param idx (scalar) to indicate for which data stream we want parameter estimates
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param lams Regularisation parameter
  #' @param m number of basis coefficients for splines
  #' @export  
  
  # obtain data
  
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
  
  # set regularisation parameter
  lambda = lams[idx]
  
  # # set initial parameters 
  beta_0 = 0
  beta = rep(0,p)
  
  c <- rep(0, m)
  
  phi <- bs(seq(0,1, length.out=length(y)), df=m, intercept = F) # to check?
  phi2 = matrix(NA, nrow = nrow(phi), ncol=ncol(phi))
  phi_bar = NULL;
  for(i in 1:ncol(phi)){
    phi2[,i] = phi[,i] - mean(phi[,i])
    phi_bar[i] = mean(phi[,i])
  }
  
  f <- phi2%*%as.matrix(c)
  
  
  # Perform cyclical coordinate descent
  r = NULL; c_r = NULL; b_r = NULL;
  for(iter in 1:max_iter) {
    #print(iter)
    # current parameter estimates
    beta_tilde <- beta
    beta0_tilde <- beta_0
    f_tilde <- f
    c_tilde <- c
    
    # Get working response and weights for IRLS
    # i.e. ~ Equations (16) and (17) of glmnet paper
    
    # compute p_tilde
    p_tilde = ( 1 + exp(-beta0_tilde - X %*% beta_tilde - f_tilde) )^-1
    # compute w
    w = p_tilde * (1-p_tilde)
    
    # compute z
    z = beta0_tilde + X %*% beta_tilde + f + ( (y-p_tilde)/ w )
    
    # Beta matrix update
    for(j in 1:p) {
      
      # Compute partial residual
      # i.e. fitted value excluding contribution from x_{ij}
      
      y_tilde <- beta_0 + X %*% beta - X[,j] * beta[j] + f
      
      # compute least-squares coefficient when fitting this partial residual to x_{ij}
      # remember to use z instead of y as the "working response"
      
      ls_coef <- crossprod(w*X[,j], z - (y_tilde))
      
      # Update beta coefficient
      beta[j] <- ( sign(ls_coef  ) * pmax(0, abs(ls_coef ) - lambda) ) / sum(w*(X[,j]^2)) #soft thresholding step
      
      
    }
    
    # Intercept update - 
    beta_0 <- crossprod(w, z- (X%*%beta) - f ) / sum(w)
    beta_0 <- as.numeric(beta_0)
    
    
    
    # Update spline coefficients assuming known form for f
    
    # step 1: calculate the c_k*phi_k(t)
    t = list(); t = rep(list(t), length(c));
    for(k in 1:length(c)){
      t[[k]] <- c[k]*phi2[,k]
    }
    
    
    # # step 2: update each c_k in turn
    for(k in 1:length(c)){
      cj_sum <- Reduce("+", t[-k])
      c[k] <-  crossprod(w*phi2[,k], z- beta_0 - (X%*%beta) - cj_sum) / sum(w*(phi2[,k])^2)
      t[[k]] <- c[k]*phi2[,k] #update with new c_k
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
      print(paste("investigate issue at iteration", iter))
      break
    }

    # Checks for convergence
    if(sum(is.na(beta))==0){ #if no na's in the beta estimate
      r[iter] = max((beta_tilde-beta)^2)
      c_r[iter] = max(abs(c_tilde-c))
      b_r[iter] = max(abs(beta0_tilde-beta_0))
      if( r[iter]<tol^2 ) break#& c_r[iter]<tol & b_r[iter]<tol) break
    }
    
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
  return(list(beta0=beta_0, beta=beta, f=f, c=c, iter = iter, beta_alt = beta_0_alt, r=r,
              c_r = c_r, b_r = b_r))
}

# 7. Coverage for confidence intervals ------------------------------------

coverage = function(active, true_beta, out_storm, n_samp, unit_id){
  
  # select results of interest
  b = length(n_samp)*unit_id #i.e. for unit_id = 1 b = 100
  if(unit_id==1){
    res = out_storm[1:b] #i.e. samples 1-100 for unit 1
  }
  if(unit_id!=1){
    res = out_storm[((b-length(n_samp))+1):b]
  }
  
  true_beta = true_beta[unit_id,]
  if(active==T){
    s = which(true_beta!=0 & true_beta!=1) # active set - excludes diagonal entries
  }
  if(active==F){
    s_c = which(true_beta==0)
    s = s_c
  }
  
  lower = map(res, 3)
  upper = map(res, 4)  
  
  if(sum(s)!=0){
    truth = true_beta[s]
    s_count = rep(0,length(truth)); s_count = rep(list(s_count), length(n_samp))
    #zero_count = rep(0,length(truth)); zero_count = rep(list(zero_count), length(n_samp))
    length_ci = rep(0, length(truth)); length_ci = rep(list(length_ci), length(n_samp))
    for(i in 1:length(n_samp)){
      lower_vals = lower[[i]][s]
      upper_vals = upper[[i]][s]
      for(j in 1:length(truth)){
        if(truth[j]>=lower_vals[j] & truth[j]<=upper_vals[j]){s_count[[i]][j] = s_count[[i]][j] + 1}
        # check if zero in CIs
       # if(0>=lower_vals[j] & 0<=upper_vals[j]){zero_count[[i]][j] = zero_count[[i]][j] + 1}
      }
      # get lengths of CI
      length_ci[[i]] = upper_vals-lower_vals
    }
    totals = unlist(lapply(s_count, sum))
    avg_cov = sum(totals)/(length(n_samp)*length(s))
    
    #totals_zero = unlist(lapply(zero_count, sum))
    #avg_zeroes = sum(totals_zero)/(length(n_samp)*length(s))
    
    # lengths
    t_length = unlist(lapply(length_ci, sum))
    av_length = sum(t_length)/ (length(n_samp)*length(s))
  }
  # if no values in the active set
  if(sum(s)==0){
    avg_cov = av_length = NA
  }
  
  return(list(cov = avg_cov, len = av_length  ))
}

# 8. Average coverage function --------------------------------------------

get_avg_cov = function(true_beta, out, n_samp, p){
  s_cov = NULL; sc_cov = NULL;
  s_len = NULL; sc_len = NULL;
  zero_s = NULL; zero_sc = NULL;
  for(i in 1:p){
    s_cov[i] = coverage(T, true_beta, out, n_samp, i)$cov                
    sc_cov[i] = coverage(F, true_beta, out, n_samp, i)$cov
    s_len[i] = coverage(T, true_beta, out, n_samp, i)$len  
    sc_len[i] = coverage(F, true_beta, out, n_samp, i)$len  
    #zero_s[i] = coverage(T, true_beta, out, n_samp, i)$zero
    #zero_sc[i] = coverage(F, true_beta, out, n_samp, i)$zero
  }
  

  
  return(list(s_cov_mean = mean(s_cov, na.rm=T), sc_cov_mean = mean(sc_cov, na.rm=T), s_all = s_cov, 
              sc_all = sc_cov, av_len_s = mean(s_len, na.rm=T), av_len_sc = mean(sc_len, na.rm=T),
              s_cov_std = std_err(s_cov[!is.na(s_cov)]), sc_cov_std = std_err(sc_cov[!is.na(sc_cov)]), std_len_s = std_err(s_len[!is.na(s_len)]),
              std_len_sc = std_err(sc_len[!is.na(sc_len)])))#, 
              #zero_s_mean = mean(zero_s, na.rm=T), zero_sc_mean = mean(zero_sc, na.rm=T)))
}



# 10. Format inference results --------------------------------------------

Format_results = function(true_beta, out, n_samp, p,model_type){
  
  res_50_1000 = get_avg_cov(true_beta, out[[1]], n_samp, p)
  res_50_2000 = get_avg_cov(true_beta, out[[2]], n_samp, p)
  res_50_5000 = get_avg_cov(true_beta, out[[3]], n_samp, p)
  res_50_10000 = get_avg_cov(true_beta, out[[4]], n_samp, p)
  
  
  df_50_1000 = data.frame(s = res_50_1000$s_all, sc=res_50_1000$sc_all)
  df_50_2000 = data.frame(s = res_50_2000$s_all, sc=res_50_2000$sc_all)
  df_50_5000 = data.frame(s = res_50_5000$s_all, sc=res_50_5000$sc_all)
  df_50_10000 = data.frame(s = res_50_10000$s_all, sc=res_50_10000$sc_all)
  
  
  par(mfrow=c(1,4))
  boxplot(df_50_1000, main = paste0(model_type, " - 1000"), ylim =c(0.65, 0.99))
  boxplot(df_50_2000, main = paste0(model_type, " - 2000"), ylim =c(0.65, 0.99))
  boxplot(df_50_5000, main = paste0(model_type, " - 5000"), ylim =c(0.65, 0.99))
  boxplot(df_50_10000, main = paste0(model_type, " - 10000"), ylim =c(0.65, 0.99))
  
  # average
  out_1000 = c(res_50_1000$s_cov_mean, res_50_1000$sc_cov_mean, res_50_1000$av_len_s, res_50_1000$av_len_sc)
  out_2000 = c(res_50_2000$s_cov_mean, res_50_2000$sc_cov_mean, res_50_2000$av_len_s, res_50_2000$av_len_sc)
  out_5000 = c(res_50_5000$s_cov_mean, res_50_5000$sc_cov_mean, res_50_5000$av_len_s, res_50_5000$av_len_sc)
  out_10000 = c(res_50_10000$s_cov_mean, res_50_10000$sc_cov_mean, res_50_10000$av_len_s, res_50_10000$av_len_sc)
  
  #std errors 
  out2_1000 = c(res_50_1000$s_cov_std, res_50_1000$sc_cov_std, res_50_1000$std_len_s, res_50_1000$std_len_sc)
  out2_2000 = c(res_50_2000$s_cov_std, res_50_2000$sc_cov_std, res_50_2000$std_len_s, res_50_2000$std_len_sc)
  out2_5000 = c(res_50_5000$s_cov_std, res_50_5000$sc_cov_std, res_50_5000$std_len_s, res_50_5000$std_len_sc)
  out2_10000 = c(res_50_10000$s_cov_std, res_50_10000$sc_cov_std, res_50_10000$std_len_s, res_50_10000$std_len_sc)
  
  out2 = rbind(out_1000, out_2000, out_5000, out_10000)
  
  out3 = rbind(out2_1000, out2_2000, out2_5000, out2_10000)
  colnames(out2) = c("Avcov_s", "Avcov_sc", "Avlen_s", "Avlen_sc")
  colnames(out3) = c("std_s", "std_sc", "std_len_s", "std_len_sc")
  return(list(avg = out2, std = out3))
}
# 11. Function to obtain performance measures for interaction matr --------

metrics_new = function(res, gt, threshold){
  
  beta_list = map(res, 1)
  # put entries less than threshold to zero
  lapply(beta_list, function(x) ifelse(abs(x) < threshold, 0, x))

  
  av_mse = mean( unlist(lapply(beta_list, function(x) performance_measures(x, gt)$l2)) )
  all_mse = unlist(lapply(beta_list, function(x) performance_measures(x, gt)$l2))
  # standard errors
  se = std_err(unlist(lapply(beta_list, function(x) performance_measures(x, gt)$l2)) )
  
  F1 = mean(unlist(lapply(beta_list, function(x) performance_measures(x, gt)$F1)))
  F1_se = std_err(unlist(lapply(beta_list, function(x) performance_measures(x, gt)$F1)))
  
  # vectorise matrices for AUROC
  true_beta = as.vector(gt)
  est_beta_vec = lapply(beta_list, as.vector)
  
  # make into 0 1 vectors
  
  true_beta[true_beta!=0]=1
  scores=list();
  
  for(i in 1:length(est_beta_vec)){
    scores[[i]] = abs(est_beta_vec[[i]])
    
  }
  
  roc_obj = lapply(scores, function(x) roc(true_beta, x))
  auc_list = lapply(roc_obj, auc)
  
  auc_obj = mean(unlist(auc_list))
  se_auc = std_err(unlist(auc_list))
  
  return(list(F1 = F1, av_mse = av_mse, av_AUC = auc_obj, se = se, f1_se = F1_se, auc_se = se_auc, all = all_mse)) 
}



# 12. Performance metrics -------------------------------------------------

performance_measures = function(estim, truth){
  
  # Usage - Function to obtain performance measures for Monte carlo estimates
  #        
  #
  # Input Parameters:
  #
  # estim - p x p matrix containing ISDM estimates
  # truth - true ISDM matrix (Hawkes)
  # ind   - indicator for PC or theta matrix. If true => PC matrix so get TPR and FPR
  #
  # Output:
  # TPR   - True positive rate
  # FPR   - False positive rate
  # l2    - l2 norm
  
  
  l2= NULL; d= list()
  
  d = estim - truth
  
  
  #l2 = sqrt(sum(abs(d)^2))
  l2 = ( sum(abs(d)^2) )/ (sum(abs(truth)^2))
  
  frob_norm = norm(d,"F")
  
  #Biniarise truth and estimated ISDM - i.e. set nonzero and non-diagonal elements to 1 so can compare to ground truth
  truth[truth!=1 &truth!=0] = 1
  estim[estim!=1 &estim!=0] = 1
  
  n.stream = dim(truth)[1]
  
  TN = sum(truth==0) #true number of missing edges 
  TP = (sum(truth==1)) #no. true edges 
  
  
  TPR = FPR = FP = FN =NULL;
  
  #estimated - truth
  #positive value -> edge found in estimated graph when there should NOT be -> False Positive
  #negative value -> edge not found in estimated graph when there SHOULD be -> False Negative
  diff = estim - truth
  
  FP = sum(diff>0) #false positives 
  FN = sum(diff<0) #false negatives
  
  #TPR and FPR
  FPR = FP/(FP+TN)
  TPR= 1-(FN/TP)  
  
  #F1 - score
  F1 = TP/(TP+0.5*(FP+FN))
  
  
  return(list(TPR = TPR, FPR = FPR, l2=l2, F1=F1, frob_norm = frob_norm))
  
}


# 13. Plot intercept estimates --------------------------------------------

beta_0_boxplots2 = function(beta_0_res, p, title, true_beta_0, a, b){
  #'  Function to plot intercept estimates for each unit across monte carlo samples
  #'
  #' @param beta_0_res nested list of intercept estimates from monte carlo sim study
  #' @param p dimension
  #' @param title title of plot
  #' @param true_beta_0 true intercept in model specification
  #' @export  
  beta0_hat = list(); beta0_hat = rep(list(beta0_hat), p)
  for(j in 1:p){
    beta0_hat[[j]] = unlist(map(beta_0_res, j))
  }
  
  df <- do.call(data.frame, beta0_hat)
  names(df) = seq(1,p)
  
  
  # test with ggplot
  df_long <- reshape2::melt(df, id.vars = NULL,variable.name = "Variable", value.name = "Value")
  
  # Plot using ggplot2
  ggplot(df_long, aes(x = as.factor(Variable), y = Value)) +
    geom_boxplot(fill = "white") +
    geom_hline(yintercept = true_beta_0, color = "blue", linetype = "dashed", size = 1) +
    labs(title = title, x = expression(d), y = expression(hat(beta[0]))) +
    ylim(a, b) +
    theme_minimal()
  
}


# 13.b plot intercept estimates -------------------------------------------


plot_intercept_estimates2 = function(out, a, b, title, true_beta_0,p){
  beta_0_hats = map(out, 2)
  beta_0_boxplots2(beta_0_hats, p, title, true_beta_0, a, b)
}


# 14. Function to convert list to matrix ----------------------------------

list_to_matrix = function(f){
  mat_f = matrix(NA, ncol=length(f[[1]]), nrow = length(f))
  for(i in 1:100){
    mat_f[i,]=f[[i]]
  }
  return(mat_f)
}


# 15. Function to reconstruct f estimate ----------------------------------

get_f = function(out,m, t_end, unit_id){
  #' Function to obtain f estimate with non-centred basis functions but estimated coefficients
  #'
  #' @param out RDS file from STORM
  #' @param m number of basis functions
  #' @param t_end dimension of previous values
  #' @param unit_id index for considered dimension
  #' @export  
  phi <- bs(seq(0,1, length.out = t_end), df=m, intercept = F) 
  phi2 = matrix(NA, nrow = nrow(phi), ncol=ncol(phi))
  phi_bar = NULL;
  for(i in 1:ncol(phi)){
    phi2[,i] = phi[,i] - mean(phi[,i])
    phi_bar[i] = mean(phi[,i])
  }
  c_hats = map(out, 4)
  c_hats_id = map(c_hats, unit_id)
  
  
  
  f = lapply(c_hats_id, function(x) phi2%*%as.matrix(x))

  
  return(f)
}

# 16. plots for normal f --------------------------------------------------

plot_f_cis_normal <- function(f, unit_id, title, N) {
  check <- list_to_matrix(f)
  median_vals <- colQuantiles(check, probs = 0.5)
  upper_vals <- colQuantiles(check, probs = 0.975)
  lower_vals <- colQuantiles(check, probs = 0.025)
  
  
  xs <- dnorm(seq(0, 1, length.out=N-1), mean = 0.5, sd =  0.05)-1
  
  data <- data.frame(
    t = 1:length(median_vals),
    median = median_vals,
    upper = upper_vals,
    lower = lower_vals,
    xs = xs
  )
  
  ggplot(data, aes(x = t/N)) +
    geom_line(aes(y = median), color = "black") +
    #geom_line(aes(y = upper), color = "red") +
    #geom_line(aes(y = lower), color = "red") +
    #geom_line(aes(y = xs), color = "blue", linetype = "dashed", size = 1) +
    geom_line(aes(y = upper), color = "black", linetype = "dashed") +
    geom_line(aes(y = lower), color = "black", linetype="dashed") +
    geom_line(aes(y = xs), color = "deepskyblue", linetype = "dashed", size = 1) +
    labs(x = "t", y = expression(hat(f)(t)), title = title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(limits = c(-5, 10)) +
    theme(legend.position = "topright") +
    guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid", "dashed"), size = c(1, 1, 1))))
}


# 17. Plots for gamma f ---------------------------------------------------

plot_f_cis_gamma <- function(f, unit_id, title, N) {
  check <- list_to_matrix(f)
  median_vals <- colQuantiles(check, probs = 0.5)
  upper_vals <- colQuantiles(check, probs = 0.975)
  lower_vals <- colQuantiles(check, probs = 0.025)
  
  
  xs <- dgamma(seq(0, 1, length.out=N-1), shape = 2, scale =  0.05)-1
  
  data <- data.frame(
    t = 1:length(median_vals),
    median = median_vals,
    upper = upper_vals,
    lower = lower_vals,
    xs = xs
  )
  
  ggplot(data, aes(x = t/N)) +
    geom_line(aes(y = median), color = "black") +
    # geom_line(aes(y = upper), color = "red") +
    # geom_line(aes(y = lower), color = "red") +
    # geom_line(aes(y = xs), color = "blue", linetype = "dashed", size = 1) +
    geom_line(aes(y = upper), color = "black", linetype = "dashed") +
    geom_line(aes(y = lower), color = "black", linetype="dashed") +
    geom_line(aes(y = xs), color = "deepskyblue", linetype = "dashed", size = 1) +
    labs(x = "t", y = expression(hat(f)(t)), title = title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(limits = c(-5, 12)) +
    theme(legend.position = "topright") +
    guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid", "dashed"), size = c(1, 1, 1))))
}


# plots for zero f --------------------------------------------------------
plot_f_cis_RL <- function(f, unit_id, title, N) {
  check <- list_to_matrix(f)
  median_vals <- colQuantiles(check, probs = 0.5)
  upper_vals <- colQuantiles(check, probs = 0.975)
  lower_vals <- colQuantiles(check, probs = 0.025)
  
  
  xs <- rep(0, length.out=N-1)
  
  data <- data.frame(
    t = 1:length(median_vals),
    median = median_vals,
    upper = upper_vals,
    lower = lower_vals,
    xs = xs
  )
  
  ggplot(data, aes(x = t/N)) +
    geom_line(aes(y = median), color = "black") +
    # geom_line(aes(y = upper), color = "red") +
    # geom_line(aes(y = lower), color = "red") +
    # geom_line(aes(y = xs), color = "blue", linetype = "dashed", size = 1) +
    geom_line(aes(y = upper), color = "black", linetype = "dashed") +
    geom_line(aes(y = lower), color = "black", linetype="dashed") +
    geom_line(aes(y = xs), color = "deepskyblue", linetype = "dashed", size = 1) +
    labs(x = "t", y = expression(hat(f)(t)), title = title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(limits = c(-2, 2)) +
    theme(legend.position = "topright") +
    guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid", "dashed"), size = c(1, 1, 1))))
}



# 17. Function to do refitting only on support of the regularised  estimate (single unit) --------


re_fit_supp = function(data, idx, max_iter, tol, m, reg_beta){
  #'  Function to estimate coefficients of the GAPLA model with fixed support of the regularised estimate
  #'
  #' @param idx (scalar) to indicate for which data stream we want parameter estimates
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param m number of basis coefficients for splines
  #' @param reg_beta row of regularised interaction matrix obtained in the first step i.e \in R^(1xp)
  #' @export  
  
  # obtain data
  
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
  
  # set regularisation parameter
  #lambda = lams[idx]
  
  # set initial parameters 
  beta_0 = 0 
  
  beta = rep(0,p)
  # get coefficient for standardised data
  
  c <- rep(0, m) 
  
  phi <- bs(seq(0,1, length.out=length(y)), df=m, intercept = F) 
  phi2 = matrix(NA, nrow = nrow(phi), ncol=ncol(phi))
  phi_bar = NULL;
  # center the basis functions
  for(i in 1:ncol(phi)){
    phi2[,i] = phi[,i] - mean(phi[,i])
    phi_bar[i] = mean(phi[,i])
  }
  
  f <- phi2%*%as.matrix(c)
  
  
  # Perform cyclical coordinate descent
  for(iter in 1:max_iter) {
    
    # current parameter estimates
    beta_tilde <- beta
    beta0_tilde <- beta_0
    f_tilde <- f
    c_tilde <- c
    
    # Get working response and weights for IRLS
    # i.e. ~ Equations (16) and (17) of glmnet paper
    
    # compute p_tilde
    p_tilde = ( 1 + exp(-beta_0 - X %*% beta - f) )^-1
    # compute w
    w = p_tilde * (1-p_tilde)
    # compute z
    z = beta_0 + X %*% beta + f + ( (y-p_tilde)/ w )
    
    # Beta matrix update
    for(j in 1:p) {
      # only update coefficients with positive support
      if(reg_beta[j]!=0){
        # Compute partial residual
        # i.e. fitted value excluding contribution from x_{ij}
        
        y_tilde <- beta_0 + X %*% beta - X[,j] * beta[j] + f
        
        # compute least-squares coefficient when fitting this partial residual to x_{ij}
        # remember to use z instead of y as the "working response"
        
        ls_coef <- crossprod(w*X[,j], z - (y_tilde))
        
        # Update beta coefficient
        beta[j] <- ls_coef / sum(w*(X[,j]^2)) 
        
      }
    }
    
    # Intercept update - 
    beta_0 <- crossprod(w, z- (X%*%beta) - f ) / sum(w)
    beta_0 <- as.numeric(beta_0)
    
    
    # Update spline coefficients assuming known form for f
    
    # step 1: calculate the c_k*phi_k(t)
    t = list(); t = rep(list(t), length(c));
    for(k in 1:length(c)){
      t[[k]] <- c[k]*phi2[,k]
    }
    
    
    # # step 2: update each c_k in turn
    for(k in 1:length(c)){
      cj_sum <- Reduce("+", t[-k])
      c[k] <-  crossprod(w*phi2[,k], z- beta_0 - (X%*%beta) - cj_sum) / sum(w*(phi2[,k])^2)
      t[[k]] <- c[k]*phi2[,k] #update with new c_k
    }
    
    f = phi2 %*% as.matrix(c)
    
    f = as.vector(f)
    
    if( max((beta_tilde-beta)^2)<tol^2) break
   
  }
  
  # DE-STANDARDISE THE COEFFICIENTS
  sum_term = NULL;
  for(j in 1:p){
    beta[j] = beta[j]/sd(X1[,j])
    sum_term[j] = (x_bar[j]*beta[j])
  }
  beta_0 = beta_0 - sum(sum_term)
  
  # Account for centered basis functions
  beta_alt = beta_0 - sum(c*phi_bar)
  
  
  
  # Print final coefficients
  return(list(beta0=beta_0, beta=beta, f=f, c=c, beta_alt = beta_alt))
}

# 18. Refit on support of all units ---------------------------------------

Estimate_coefficients_refit <- function(data_source, p, max_iter, tol, m, reg_beta){
  #'  Function to estimate coefficients of the refitted GAPLA model for all dimensions 
  #'
  #' @param data_srouce data of interest
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param p number of dimenions
  #' @param reg_beta regularised estimte of interaction matrix from first step \in R^{p x p}
  #' @param m number of basis coefficients for splines
  #' @export  
  p = dim(data_source)[1]
  print(p)
  beta0=NULL; beta = matrix(NA, p ,p); f = list(); f = rep(list(f), p); 
  c = list(); c = rep(list(c), p);beta_alt = NULL;
  for (i in 1:p){
    print(i)
    res = re_fit_supp(data_source, i, max_iter, tol, m, reg_beta[i,])
    beta0[i] = res$beta0
    beta[i,] = res$beta
    f[[i]] = res$f_orig
    c[[i]] = res$c
    beta_alt[i] = res$beta_alt
  }
  
  return(list(beta0=beta0, beta_hat=beta, f_hat = f, c_hat = c, beta_alt = beta_alt))
}

# 19. errors for estimated intercept term ---------------------------------

est_intercept = function(out,p, truth){
  # get intercept estimate
  ints = map(out, 2) #list of 100
  mse = NULL; se = NULL;
  for(i in 1:p){
    beta_0s = unlist(map(ints, i)) 
    mse[i] = mean( (beta_0s-truth)^2 )
    se[i] = std_err( (beta_0s-truth)^2 )
  }
  return(list(mse = mean(mse), err = mean(se), all = mse))
}


# perahps not needed !

# 20. Function to extract BIC values for single unit ----------------------

get_bic_vals = function(out_storm, N_samp, t_end, m,lambdas, data, unit_id, gamma, p, threshold){
  av_BIC = NULL;av_AIC=NULL;av_eBIC=NULL;av_nn=NULL;av_nll=NULL;av_nll2 = NULL;
  any = rep(0, length(lambdas)); all = rep(0, length(lambdas))
  for(i in 1:length(lambdas)){
    #print(i)
    b = N_samp*i
    if(i==1){
      res = out_storm[1:b]
    }
    if(i!=1){
      res = out_storm[((b-N_samp)+1):b]
    }
    
    # get estimates of beta
    beta_hats = map(res, 1)
    # select coefficient vector for specified unit
    
    beta_vec = lapply(beta_hats, function(x) x[unit_id, ])
    
    # set values below threshold to zero
    beta_vec = lapply(beta_vec, function(x) ifelse(x < threshold, 0, x))
    # set NaNs to zero
    beta_vec = lapply(beta_vec, function(x) ifelse(is.na(x), 0, x))
    
    # get intercept estimates
    beta_0 = map(res,2)
    # select those for specified unit
    beta_0 = unlist(map(beta_0, unit_id))
    
    # get f estimates
    c_hat = map(res, 4)
    # select for specified unit
    c_hat = map(c_hat, unit_id)
    phi <- bs(seq(0,1, length.out=t_end), df=m, intercept = F) # to check?
    phi2 = matrix(NA, nrow = nrow(phi), ncol=ncol(phi))
    phi_bar = NULL;
    for(k in 1:ncol(phi)){
      phi2[,k] = phi[,k] - mean(phi[,k])
      phi_bar[k] = mean(phi[,k])
    }
    f = lapply(c_hat, function(x) phi2%*%as.matrix(x))
    
    # calculate BIC for the 10 training samples
    BIC_vec = NULL; AIC_vec = NULL; eBIC_vec = NULL; nll2=NULL; nll=NULL; num_non_zeroes=NULL;check = NULL
    
    for(j in 1:length(beta_hats)){#length(beta_hats)){
      
      # select data of interest to the unit_id
      y = data[[j]][unit_id,][2:(t_end+1)] 
      X1 = data[[j]][,(1:t_end)] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
      X1 = t(X1)
      
      
      term1 = y * (beta_0[j] + X1 %*% beta_vec[[j]] + f[[j]])
      term2 = log(1 + exp(beta_0[j] + X1 %*% beta_vec[[j]] + f[[j]])) 
      nll[j] = sum(term1-term2) 
      
      p_prob = inv.logit(beta_0[j] + X1 %*% beta_vec[[j]] + f[[j]])
      nll2[j] = sum( y*(log(p_prob)) + ( (1-y)*(log(1-p_prob)) ))
      
     
      num_non_zeroes[j] = sum(beta_vec[[j]] != 0)
      pen_term = (num_non_zeroes[j]+1+(m))*log(length(y))
      eBIC_term = 2*gamma*log(p)* (num_non_zeroes[j]+1+(m))
      
      BIC_vec[j] = -2*(nll[j]) + pen_term #+ eBIC_term
      eBIC_vec[j] = BIC_vec[j] + eBIC_term
      AIC_vec[j] = -2*(nll[j]) + (2*(num_non_zeroes[j]+1+(m)))
      
      check[j] = pen_term > (2*(num_non_zeroes[j]+1+(m)))
    }
    # check if any are zeroes
    if(any(num_non_zeroes==0)){any[i] = 1}
    # check if all zeroes
    if(all(num_non_zeroes==0)){all[i] = 1}
    # get average BIC across the training samples
    av_BIC[i] = mean(BIC_vec)
    av_AIC[i] = mean(AIC_vec)
    av_eBIC[i] = mean(eBIC_vec)
    av_nn[i] = mean(num_non_zeroes)
    av_nll[i] = mean(nll)
    av_nll2[i] = mean(nll2)
  }
  return(list(BIC = av_BIC, AIC = av_AIC, eBIC = av_eBIC,  nn=av_nn, nll=av_nll, nll2 = av_nll2, any = any, all = all ))
}


# 21. Function to choose lambda value for all units -----------------------

get_lambdas = function(out, N_samp, t_end, m, lambdas, data, p, title){
  lambdas_BIC = NULL; lambdas_eBIC = NULL; lambdas_AIC = NULL;
  out_AIC = NULL; out_BIC = NULL; out_eBIC = NULL;
  for(i in 1:p){
    print(i)
    res = get_bic_vals(out, N_samp, t_end, m,lambdas, data,i, 0.5, p, threshold = 1e-5)
    idx = which(res$any==1)[1] #extract index of lambda for which all are zero
    lambdas_AIC[i] = lambdas[which.min(res$AIC)]
    lambdas_BIC[i] = lambdas[which.min(res$BIC)]
    lambdas_eBIC[i] = lambdas[which.min(res$eBIC)]
  }
  df = data.frame(cbind(lambdas_AIC, lambdas_BIC, lambdas_eBIC))
  names(df)=c("AIC", "BIC", "eBIC")
  boxplot(df, ylab=expression(lambda), main=title)
  return(df)
}


# 22. Errors for gamma f --------------------------------------------------

err_gamma_f = function(out,p, N, m){
  mse = NULL; se = NULL;
  for(i in 1:p){
    truth = dgamma(seq(0,1,length.out = N-1), shape = 2, scale = 0.05)-1
    # get intercept estimate for specific dimensions
    f_hat = get_f(out, m, N-1, i)
    f_hat = lapply(f_hat, as.numeric)
    mse[i] = mean(unlist(lapply(f_hat, function(x) mean( (x - truth)^2))))
    se[i] = mean(unlist(lapply(f_hat, function(x) std_err( (x - truth)^2))))
  }
  return(list(mse = mean(mse), err = mean(se), all = mse))
}


# 23. Errors for normal f -------------------------------------------------

err_normal_f = function(out,p, N, m){
  mse = NULL; se = NULL; #all = NULL; all = rep(list(all), p)
  for(i in 1:p){
    truth = dnorm(seq(0,1,length.out = N-1), mean = 0.5, sd = 0.05)-1
    # get intercept estimate for specific dimensions
    f_hat = get_f(out, m, N-1, i)
    f_hat = lapply(f_hat, as.numeric)
    mse[i] = mean(unlist(lapply(f_hat, function(x) mean( (x - truth)^2))))
    se[i] = mean(unlist(lapply(f_hat, function(x) std_err( (x - truth)^2))))
  }
  return(list(mse = mean(mse), err = mean(se), all = mse))
}


# 24. Errors split for normal and gamma -----------------------------------

err_normal_gamma_f = function(out,p, N,m){
  mse = NULL; se = NULL; #all = NULL; all = rep(list(all), p)
  for(i in 1:p){
    if(i<=p/2){
      # first half add gamma
      truth = dgamma(seq(0,1,length.out = N-1), shape = 2, scale = 0.05)-1
    }
    
    if(i>p/2){
      #second half normal
      truth = dnorm(seq(0,1,length.out = N-1), mean = 0.5, sd = 0.05)-1
    }
    # get intercept estimate for specific dimensions
    f_hat = get_f(out, m, N-1, i)
    f_hat = lapply(f_hat, as.numeric)
    mse[i] = mean(unlist(lapply(f_hat, function(x) mean( (x - truth)^2))))
    se[i] = mean(unlist(lapply(f_hat, function(x) std_err( (x - truth)^2))))
  }
  return(list(mse = mean(mse), err = mean(se), all = mse))
}

# 11. Estimate coeff with known beta --------------------------------------

Estimate_coefficients_true_beta <- function(data_source, p, max_iter, tol, true_beta, m){
  #'  Function to estimate coefficients of the BAPLA model for all dimensions and regularization parameter
  #'
  #' @param data_source data
  #' @param p dimension
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param lams Regularisation parameter
  #' @param m number of basis coefficients for splines
  #' @export  
  p = dim(data_source)[1]
  #print(p)
  beta0_hat=NULL; beta_hat = matrix(NA, p ,p); f = list(); f_hat = rep(list(f), p); 
  c_hat = list(); c_hat = rep(list(c_hat), p); iter = NULL; beta0_hat2 = NULL;
  for (i in 1:p){
    print(i)
    beta = true_beta[i,] # get row of true matrix
    res = Estimate_coefficients_beta(data_source, i, max_iter, tol, beta, m)
    beta0_hat[i] = res$beta0
    beta0_hat2[i] = res$beta_alt
    beta_hat[i,] = res$beta #should be same as in true beta
    f_hat[[i]] = res$f
    c_hat[[i]] = res$c
    iter[i] = res$iter
  }
  
  return(list(beta0_hat=beta0_hat, beta_hat=beta_hat, f = f_hat, c_hat = c_hat, iter = iter, beta_alt = beta0_hat2))
}


# 12.Estimate coefficients with known beta (single) --------------------------

Estimate_coefficients_beta = function(data, idx, max_iter, tol, beta,  m){
  #'  Function to estimate coefficients of the BAPLA for a given dimension and regularization parameter
  #'
  #' @param data data source 
  #' @param idx (scalar) to indicate for which data stream we want parameter estimates
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param beta true beta
  #' @param m number of basis coefficients for splines
  #' @export  
  
  # obtain data
  
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
  
  # set regularisation parameter
  
  # # set initial parameters 
  beta_0 = 0
  
  
  c <- rep(0, m)
  
  phi <- bs(seq(1, length(y)), df=m, intercept = F) 
  phi2 = matrix(NA, nrow = nrow(phi), ncol=ncol(phi))
  phi_bar = NULL;
  for(i in 1:ncol(phi)){
    phi2[,i] = phi[,i] - mean(phi[,i])
    phi_bar[i] = mean(phi[,i])
  }
  
  f <- phi2%*%as.matrix(c)
  
  
  # Perform cyclical coordinate descent
  for(iter in 1:max_iter) {
    #print(iter)
    # current parameter estimates
    beta_tilde <- beta
    beta0_tilde <- beta_0
    f_tilde <- f
    c_tilde <- c
    
    # Get working response and weights for IRLS
    # i.e. ~ Equations (16) and (17) of glmnet paper
    
    # compute p_tilde
    p_tilde = ( 1 + exp(-beta0_tilde - X %*% beta_tilde - f_tilde) )^-1
    # compute w
    w = p_tilde * (1-p_tilde)
    
    # compute z
    z = beta0_tilde + X %*% beta_tilde + f + ( (y-p_tilde)/ w )
    
    
    # Intercept update - 
    #beta_0 <- crossprod(w, z- (X%*%beta) - f ) / sum(w)
    beta_0 <- mean(z)
    beta_0 <- as.numeric(beta_0)
    
    
    
    # Update spline coefficients assuming known form for f
    
    # step 1: calculate the c_k*phi_k(t)
    t = list(); t = rep(list(t), length(c));
    for(k in 1:length(c)){
      t[[k]] <- c[k]*phi2[,k]
    }
    
    
    # # step 2: update each c_k in turn
    for(k in 1:length(c)){
      cj_sum <- Reduce("+", t[-k])
      c[k] <-  crossprod(w*phi2[,k], z- beta_0 - (X%*%beta) - cj_sum) / sum(w*(phi2[,k])^2)
      t[[k]] <- c[k]*phi2[,k] #update with new c_k
    }
    
    f = phi2 %*% as.matrix(c)
    
    f = as.vector(f)
    
    # Checks for convergence
    
    if( max((beta0_tilde-beta_0)^2)<tol^2) break
  }
  
  #DE-STANDARDISE THE COEFFICIENTS
  # sum_term = NULL;
  # for(j in 1:p){
  #   beta[j] = beta[j]/sd(X1[,j])
  #   sum_term[j] = (x_bar[j]*beta[j])
  # }
  # beta_0 = beta_0 - sum(sum_term)
  
  # Account for centered basis functions
  beta_alt = beta_0 - sum(c*phi_bar)
  
  
  # Print final coefficients
  return(list(beta0=beta_0, beta=beta, f=f, c=c, iter = iter, beta_alt = beta_alt))
}






# 13. Estimate coefficients without f -------------------------------------

Estimate_coefficients_no_rate = function(data, idx, max_iter, tol, lams, m){
  #'  Function to estimate coefficients of the BAPLA for a given dimension and regularization parameter
  #'
  #' @param data data source 
  #' @param idx (scalar) to indicate for which data stream we want parameter estimates
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param lams Regularisation parameter
  #' @param m number of basis coefficients for splines
  #' @export  
  
  # obtain data
  
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
  
  # set regularisation parameter
  lambda = lams[idx]
  
  # # set initial parameters 
  beta_0 = 0
  beta = rep(0,p)
  
  
  
  # Perform cyclical coordinate descent
  for(iter in 1:max_iter) {
    #print(iter)
    # current parameter estimates
    beta_tilde <- beta
    beta0_tilde <- beta_0
    
    # Get working response and weights for IRLS
    # i.e. ~ Equations (16) and (17) of glmnet paper
    
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
      
      ls_coef <- crossprod(w*X[,j], z - (y_tilde))
      
      # Update beta coefficient
      beta[j] <- ( sign(ls_coef  ) * pmax(0, abs(ls_coef ) - lambda) ) / sum(w*(X[,j]^2)) #soft thresholding step
      
      
    }
    
    # Intercept update - 
    beta_0 <- crossprod(w, z- (X%*%beta)  ) / sum(w)
    beta_0 <- as.numeric(beta_0)
    
    
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


# 14. Estimate coefficients without f (all units) -------------------------

Estimate_coefficients_logit_no_f <- function(data_source, p, max_iter, tol, lams, m){
  #'  Function to estimate coefficients of the BAPLA model for all dimensions and regularization parameter
  #'
  #' @param data_source data
  #' @param p dimension
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param lams Regularisation parameter
  #' @param m number of basis coefficients for splines
  #' @export  
  p = dim(data_source)[1]
  #print(p)
  beta0_hat=NULL; beta_hat = matrix(NA, p ,p)
  iter = NULL; beta0_alt = NULL;
  for (i in 1:p){
    print(i)
    res = Estimate_coefficients_no_rate(data_source, i, max_iter, tol, lams, m)
    beta0_hat[i] = res$beta0
    beta_hat[i,] = res$beta
    iter[i] = res$iter
  }
  
  return(list(beta0_hat=beta0_hat, beta_hat=beta_hat, iter = iter))
}


# 27. Get bic vals without f ----------------------------------------------

get_bic_vals_no_f = function(out_storm, N_samp, t_end, m,lambdas, data, unit_id, gamma, p, threshold){
  av_BIC = NULL;av_AIC=NULL;av_eBIC=NULL;av_nn=NULL;av_nll=NULL;av_nll2 = NULL;
  any = rep(0, length(lambdas)); all = rep(0, length(lambdas))
  for(i in 1:length(lambdas)){
    #print(i)
    b = N_samp*i
    if(i==1){
      res = out_storm[1:b]
    }
    if(i!=1){
      res = out_storm[((b-N_samp)+1):b]
    }
    
    # get estimates of beta
    beta_hats = map(res, 1)
    # select coefficient vector for specified unit
    
    beta_vec = lapply(beta_hats, function(x) x[unit_id, ])
    
    # set values below threshold to zero
    beta_vec = lapply(beta_vec, function(x) ifelse(x < threshold, 0, x))
    # set NaNs to zero
    beta_vec = lapply(beta_vec, function(x) ifelse(is.na(x), 0, x))
    
    # get intercept estimates
    beta_0 = map(res,2)
    # select those for specified unit
    beta_0 = unlist(map(beta_0, unit_id))
    
    # calculate BIC for the 10 training samples
    BIC_vec = NULL; AIC_vec = NULL; eBIC_vec = NULL; nll2=NULL; nll=NULL; num_non_zeroes=NULL;check = NULL
    
    for(j in 1:length(beta_hats)){#length(beta_hats)){
      
      # select data of interest to the unit_id
      y = data[[j]][unit_id,][2:(t_end+1)] 
      X1 = data[[j]][,(1:t_end)] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
      X1 = t(X1)
      
      
      term1 = y * (beta_0[j] + X1 %*% beta_vec[[j]] )
      term2 = log(1 + exp(beta_0[j] + X1 %*% beta_vec[[j]] ) )
      nll[j] = sum(term1-term2) 
      
      p_prob = inv.logit(beta_0[j] + X1 %*% beta_vec[[j]] )
      nll2[j] = sum( y*(log(p_prob)) + ( (1-y)*(log(1-p_prob)) ))
      
      
      num_non_zeroes[j] = sum(beta_vec[[j]] != 0)
      pen_term = (num_non_zeroes[j]+1+(m))*log(length(y))
      eBIC_term = 2*gamma*log(p)* (num_non_zeroes[j]+1+(m))
      
      BIC_vec[j] = -2*(nll[j]) + pen_term #+ eBIC_term
      eBIC_vec[j] = BIC_vec[j] + eBIC_term
      AIC_vec[j] = -2*(nll[j]) + (2*(num_non_zeroes[j]+1+(m)))
      
      check[j] = pen_term > (2*(num_non_zeroes[j]+1+(m)))
    }
    # check if any are zeroes
    if(any(num_non_zeroes==0)){any[i] = 1}
    # check if all zeroes
    if(all(num_non_zeroes==0)){all[i] = 1}
    # get average BIC across the training samples
    av_BIC[i] = mean(BIC_vec)
    av_AIC[i] = mean(AIC_vec)
    av_eBIC[i] = mean(eBIC_vec)
    av_nn[i] = mean(num_non_zeroes)
    av_nll[i] = mean(nll)
    av_nll2[i] = mean(nll2)
  }
  return(list(BIC = av_BIC, AIC = av_AIC, eBIC = av_eBIC,  nn=av_nn, nll=av_nll, nll2 = av_nll2, any = any, all = all ))
}


# 28. get lambdas no f ----------------------------------------------------

get_lambdas_no_f = function(out, N_samp, t_end, m, lambdas, data, p, title){
  lambdas_BIC = NULL; lambdas_eBIC = NULL; lambdas_AIC = NULL;
  out_AIC = NULL; out_BIC = NULL; out_eBIC = NULL;
  for(i in 1:p){
    print(i)
    res = get_bic_vals_no_f(out, N_samp, t_end, m,lambdas, data,i, 0.5, p, threshold = 1e-5)
    idx = which(res$any==1)[1] #extract index of lambda for which all are zero
    lambdas_AIC[i] = lambdas[which.min(res$AIC)]
    lambdas_BIC[i] = lambdas[which.min(res$BIC)]
    lambdas_eBIC[i] = lambdas[which.min(res$eBIC)]
  }
  df = data.frame(cbind(lambdas_AIC, lambdas_BIC, lambdas_eBIC))
  names(df)=c("AIC", "BIC", "eBIC")
  boxplot(df, ylab=expression(lambda), main=title)
  return(df)
}

# Old functions -----------------------------------------------------------




# 12. Plot lambda values --------------------------------------------------

lambda_boxplots = function(lams, p, title){
  #'  Function to plot lambda values for each unit chosen across monte carlo samples
  #'
  #' @param lams nested list of regularisation parameter estimates from monte carlo sim study
  #' @param p dimension
  #' @param title title of plot
  #' @export  
  beta0_hat = list(); beta0_hat = rep(list(beta0_hat), p)
  for(j in 1:p){
    beta0_hat[[j]] = unlist(map(lams, j))
  }
  
  df <- do.call(data.frame, beta0_hat)
  names(df) = seq(1,p)
  boxplot(df, main = title, ylab = expression(lambda), ylim=c(0, 0.002))
  
  return(colMeans(df))
  
}



# 17. Function for debiased lasso -----------------------------------------

despars_lasso = function(idx, data, p, beta_0, beta_hat, f_hat, alpha){
  #' Function to obtain debiased lasso estimates and confidence intervals
  #'
  #' @param idx index of unit of interest, i.e., idx in 1,dots,p
  #' @param data sample of data used in simulation study
  #' @param p no of dimensions
  #' @param beta_0 vector of intercept estimates 
  #' @param beta_hat estimate of interaction matrix
  #' @param f_hat estimate of nonparametric function
  #' @export  
  
  y = data[idx,][2:length(data[idx,])] # use
  X1 = data[,(1: (length(data[idx,])-1) )] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
  X1 = t(X1)
  p = dim(data)[1]
  
  
  p_t = (1+exp(-beta_0[idx] - X1 %*% beta_hat[idx,] - f_hat[[idx]]))^(-1)
  
  
  first_deriv = - ( t(y-p_t) %*% X1 ) / length(y) ## (1 x p)
  vec_pt = as.vector(p_t)
  D = diag(vec_pt * (1-vec_pt))
  sigma_hat = ( t(X1) %*% D %*% X1 ) /length(y) #p x p
  
  # checks
  W = sqrt(D) %*% X1
  #check = (t(W) %*% W )/length(y)
  #round(check,8) == round(sigma_hat,8)
  
  # use W as input to standard glmnet
  library(glmnet)
  # nodewise lasso 
  C = diag(p); tau = NULL; tau2=NULL
  for (j in 1:p){
    w_minus_j = W[,-j]
    w_j = W[,j]
    # use glmnet for regression problem for each j
    gamma_j.fit = cv.glmnet(x=w_minus_j, y = w_j, family="gaussian", alpha=1, intercept=F, standardize = T)
    gamma_j = glmnet(x=w_minus_j, y = w_j, family="gaussian", alpha=1, intercept=F, standardize = T, lambda = gamma_j.fit$lambda.min)
    gamma_j = as.vector(gamma_j$beta)
    C[j,-j] = (-1)*t(gamma_j)
    tau[j] = as.numeric ( ( t(w_j-w_minus_j%*%as.matrix(gamma_j)) %*% as.matrix(w_j) ) /length(y) )
    tau2[j] = as.numeric( sigma_hat[j,j] - sigma_hat[j,-j] %*% as.matrix(gamma_j) )
  }
  theta_lasso = diag(1/tau) %*% C
  beta_deb = beta_hat[idx,] - theta_lasso%*%t(first_deriv)
  beta_deb = as.numeric(beta_deb)
  
  sigma = sqrt(diag(theta_lasso %*% sigma_hat %*% t(theta_lasso)))
  
  ci_lower = beta_deb - qnorm(1-alpha/2)*sigma/sqrt(length(y))
  ci_upper = beta_deb + qnorm(1-alpha/2)*sigma/sqrt(length(y))
  
  
  return(list(beta_deb = beta_deb, sigma = sigma, ci_lower = ci_lower, ci_upper = ci_upper, theta_lasso = theta_lasso))
}

# 18. Function to do re-fitting with the de-biased lasso ------------------

re_fit = function(data, idx, max_iter, tol, m, beta_deb){
  #'  Function to estimate coefficients of the GAPLA model with fixed de-biased betas 
  #'
  #' @param idx (scalar) to indicate for which data stream we want parameter estimates
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param m number of basis coefficients for splines
  #' @param beta_deb debiased beta estimate in R^(1xp)
  #' @export  
  
  # obtain data
  
  y = data[idx,][2:9000] # use  8999 values
  X1 = data[,(1:8999)] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
  X1 = t(X1)
  p = dim(data)[1]
  
  # standardize the data
  x_bar = colMeans(X1) # get mean of each column
  # X = X1
  X = matrix(NA, ncol=ncol(X1), nrow=nrow(X1))
  for (i in 1:p){
    X[,i] = (X1[,i] - x_bar[i])/sd(X1[,i])
  }

  # set regularisation parameter
  #lambda = lams[idx]
  
  # set initial parameters 
  beta_0 = 0 
  
  beta = beta_deb
  # get coefficient for standardised data
  
  
  for(i in 1:p){
    beta[i] = beta[i] * sd(X1[,i])
  }
  
  c <- rep(0, m) 
  
  phi <- bs(seq(1, length(y)), df=m, intercept = F) 
  phi2 = matrix(NA, nrow = nrow(phi), ncol=ncol(phi))
  phi_bar = NULL;
  # center the basis functions
  for(i in 1:ncol(phi)){
    phi2[,i] = phi[,i] - mean(phi[,i])
    phi_bar[i] = mean(phi[,i])
  }
  
  f <- phi2%*%as.matrix(c)
  
  
  # Perform cyclical coordinate descent
  for(iter in 1:max_iter) {
    
    # current parameter estimates
    beta_tilde <- beta
    beta0_tilde <- beta_0
    f_tilde <- f
    c_tilde <- c
    
    # Get working response and weights for IRLS
    # i.e. ~ Equations (16) and (17) of glmnet paper
    
    # compute p_tilde
    p_tilde = ( 1 + exp(-beta_0 - X %*% beta - f) )^-1
    # compute w
    w = p_tilde * (1-p_tilde)
    # compute z
    z = beta_0 + X %*% beta + f + ( (y-p_tilde)/ w )
    
    # # Beta matrix update
    # for(j in 1:p) {
    #   
    #   # Compute partial residual
    #   # i.e. fitted value excluding contribution from x_{ij}
    #   
    #   y_tilde <- beta_0 + X %*% beta - X[,j] * beta[j] + f
    #   
    #   # compute least-squares coefficient when fitting this partial residual to x_{ij}
    #   # remember to use z instead of y as the "working response"
    #   
    #   ls_coef <- crossprod(w*X[,j], z - (y_tilde))
    #   
    #   # Update beta coefficient
    #   beta[j] <- ( sign(ls_coef  ) * pmax(0, abs(ls_coef ) - lambda) ) / sum(w*(X[,j]^2)) #soft thresholding step
    #   
    # }
    
    # Intercept update - 
    beta_0 <- crossprod(w, z- (X%*%beta) - f ) / sum(w)
    beta_0 <- as.numeric(beta_0)
    
    
    # Update spline coefficients assuming known form for f
    
    # step 1: calculate the c_k*phi_k(t)
    t = list(); t = rep(list(t), length(c));
    for(k in 1:length(c)){
      t[[k]] <- c[k]*phi2[,k]
    }
    
    
    # # step 2: update each c_k in turn
    for(k in 1:length(c)){
      cj_sum <- Reduce("+", t[-k])
      c[k] <-  crossprod(w*phi2[,k], z- beta_0 - (X%*%beta) - cj_sum) / sum(w*(phi2[,k])^2)
      t[[k]] <- c[k]*phi2[,k] #update with new c_k
    }
    
    f = phi2 %*% as.matrix(c)
    
    f = as.vector(f)
    
    # Check for convergence
    #if( sqrt(sum((beta - beta_tilde)^2)) < tol & sqrt(sum((beta_0 - beta0_tilde)^2)) < tol  & sqrt(sum((c - c_tilde)^2)) < tol) break
  }
  
  # DE-STANDARDISE THE COEFFICIENTS
  sum_term = NULL;
  for(j in 1:p){
    beta[j] = beta[j]/sd(X1[,j])
    sum_term[j] = (x_bar[j]*beta[j])
  }
  beta_0 = beta_0 - sum(sum_term)
  
  # Account for centered basis functions
  beta_0 = beta_0 - sum(c*phi_bar)
  
  # estimate original f
  f_orig = phi %*% as.matrix(c)
  
  # Print final coefficients
  return(list(beta0=beta_0, beta=beta, f=f, c=c, f_orig = f_orig))
}


# 21. Function to extract debiased estimates from storm output ------------

get_debiased_est = function(out_storm, p, N_samp){
  # extract debiased lasso estimates for all dimensions
  beta_deb = list(); beta_deb = rep(list(beta_deb), 100); beta_deb = rep(list(beta_deb), p);
  for(i in 1:p){
    b = N_samp*i
    if(i==1){
      res = out_storm[1:b] #i.e. samples 1-10 for lambda = 0.1
    }
    if(i!=1){
      res = out_storm[((b-N_samp)+1):b]
    }
    # betas for each sample
    beta_deb[[i]] = map(res, 1)
  }
  return(beta_deb)
}


# 22. New desparsified lasso  ---------------------------------------------

despars_lasso_new = function(idx, data, p, beta_0, beta_hat, f_hat, alpha){
  #' Function to obtain debiased lasso estimates and confidence intervals
  #'
  #' @param idx index of unit of interest, i.e., idx in 1,dots,p
  #' @param data sample of data used in simulation study
  #' @param p no of dimensions
  #' @param beta_0 vector of intercept estimates 
  #' @param beta_hat estimate of interaction matrix
  #' @param f_hat estimate of nonparametric function
  #' @export  
  
  y = data[idx,][2:length(data[idx,])] # use
  X1 = data[,(1: (length(data[idx,])-1) )] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
  X1 = t(X1)
  p = dim(data)[1]
  
  
  p_t = (1+exp(-beta_0[idx] - X1 %*% beta_hat[idx,] - f_hat[[idx]]))^(-1)
  
  
  first_deriv = - ( t(y-p_t) %*% X1 ) / length(y) ## (1 x p)
  vec_pt = as.vector(p_t)
  D = diag(vec_pt * (1-vec_pt))
  sigma_hat = ( t(X1) %*% D %*% X1 ) /length(y) #p x p
  
  # checks
  W = sqrt(D) %*% X1
  #check = (t(W) %*% W )/length(y)
  #round(check,8) == round(sigma_hat,8)
  
  # use W as input to standard glmnet
  library(glmnet)
  # nodewise lasso 
  C = diag(p); tau = NULL; tau2=NULL
  for (j in 1:p){
    w_minus_j = W[,-j]
    w_j = W[,j]
    # use glmnet for regression problem for each j
    gamma_j.fit = cv.glmnet(x=w_minus_j, y = w_j, family="gaussian", alpha=1)
    gamma_j = glmnet(x=w_minus_j, y = w_j, family="gaussian", alpha=1, lambda = gamma_j.fit$lambda.min)
    gamma_j = as.vector(gamma_j$beta)
    C[j,-j] = (-1)*t(gamma_j)
    tau[j] = as.numeric ( ( t(w_j-w_minus_j%*%as.matrix(gamma_j)) %*% as.matrix(w_j) ) /length(y) )
    tau2[j] = as.numeric( sigma_hat[j,j] - sigma_hat[j,-j] %*% as.matrix(gamma_j) )
  }
  theta_lasso = diag(1/tau) %*% C
  beta_deb = beta_hat[idx,] - theta_lasso%*%t(first_deriv)
  beta_deb = as.numeric(beta_deb)
  
  sigma = sqrt(diag(theta_lasso %*% sigma_hat %*% t(theta_lasso)))
  
  ci_lower = beta_deb - qnorm(1-alpha/2)*sigma/sqrt(length(y))
  ci_upper = beta_deb + qnorm(1-alpha/2)*sigma/sqrt(length(y))
  
  
  return(list(beta_deb = beta_deb, sigma = sigma, ci_lower = ci_lower, ci_upper = ci_upper, theta_lasso = theta_lasso))
}



# invert matrix -----------------------------------------------------------

despars_lasso_invert = function(idx, data, p, beta_0, beta_hat, f_hat, alpha){
  #' Function to obtain debiased lasso estimates and confidence intervals
  #'
  #' @param idx index of unit of interest, i.e., idx in 1,dots,p
  #' @param data sample of data used in simulation study
  #' @param p no of dimensions
  #' @param beta_0 vector of intercept estimates 
  #' @param beta_hat estimate of interaction matrix
  #' @param f_hat estimate of nonparametric function
  #' @export  
  
  y = data[idx,][2:length(data[idx,])] # use
  X1 = data[,(1: (length(data[idx,])-1) )] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
  X1 = t(X1)
  p = dim(data)[1]
  
  
  p_t = (1+exp(-beta_0[idx] - X1 %*% beta_hat[idx,] - f_hat[[idx]]))^(-1)
  
  
  first_deriv = - ( t(y-p_t) %*% X1 ) / length(y) ## (1 x p)
  vec_pt = as.vector(p_t)
  D = diag(vec_pt * (1-vec_pt))
  sigma_hat = ( t(X1) %*% D %*% X1 ) /length(y) #p x p
  
  # checks
  W = sqrt(D) %*% X1
  #check = (t(W) %*% W )/length(y)
  #round(check,8) == round(sigma_hat,8)
  
  # use W as input to standard glmnet
  # library(glmnet)
  # # nodewise lasso 
  # C = diag(p); tau = NULL; tau2=NULL
  # for (j in 1:p){
  #   w_minus_j = W[,-j]
  #   w_j = W[,j]
  #   # use glmnet for regression problem for each j
  #   gamma_j.fit = cv.glmnet(x=w_minus_j, y = w_j, family="gaussian", alpha=1)
  #   gamma_j = glmnet(x=w_minus_j, y = w_j, family="gaussian", alpha=1, lambda = gamma_j.fit$lambda.min)
  #   gamma_j = as.vector(gamma_j$beta)
  #   C[j,-j] = (-1)*t(gamma_j)
  #   tau[j] = as.numeric ( ( t(w_j-w_minus_j%*%as.matrix(gamma_j)) %*% as.matrix(w_j) ) /length(y) )
  #   tau2[j] = as.numeric( sigma_hat[j,j] - sigma_hat[j,-j] %*% as.matrix(gamma_j) )
  # }
  theta_lasso = solve(sigma_hat)#diag(1/tau) %*% C
  beta_deb = beta_hat[idx,] - theta_lasso%*%t(first_deriv)
  beta_deb = as.numeric(beta_deb)
  
  sigma = sqrt(diag(theta_lasso %*% sigma_hat %*% t(theta_lasso)))
  
  ci_lower = beta_deb - qnorm(1-alpha/2)*sigma/sqrt(length(y))
  ci_upper = beta_deb + qnorm(1-alpha/2)*sigma/sqrt(length(y))
  
  
  return(list(beta_deb = beta_deb, sigma = sigma, ci_lower = ci_lower, ci_upper = ci_upper, theta_lasso = theta_lasso))
}



