
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
  # Create a graph from the matrix
  graph <- graph_from_adjacency_matrix(100*beta_matrix, mode = "directed", weighted = T, diag = F)
  # Set edge colors based on the sign of the matrix values
  E(graph)$color <- ifelse(E(graph)$weight > 0, "red", "blue")
  # Set vertex color to white
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

Simulate_data_add_norm = function(p, N, beta, beta_0, cut_off, mu, sigma, const){
  
  #' Simualte binary time series according to BAPLA model with f as a normal.
  #'
  #' @param p Number of dimensions
  #' @param N Number of time points
  #' @param beta p x p linear coefficient matrix  
  #' @param beta_0 1 x p vector of intercept terms
  #' @param cut_off (scalar). Number of samples to discard as "burn in" period. 
  #' @param mu (scalar) mean for normal f
  #' @param sigma (scalar) sd for normal f
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
      y[i,j] = rbinom(1, 1 , prob = invlogit(beta_0[i]+t(beta[i,])%*%y_minus + const*dnorm(j, mean = mu, sd=sigma))) 
      #y[i,j] = rbinom(1, 1 , prob = invlogit(beta_0[i]+t(y_minus) %*%(beta[i,]) + const*dnorm(j, mean = mu, sd=sigma)))
    }
  }
  
  # get rid of first 100 samples => "burn in"
  new_y = y[, -(1:500)]
  return(new_y)
}



# 4b. Simulate data with gamma f -------------------------------------------

Simulate_data_add_gamma = function(p, N, beta, beta_0, cut_off, shape, scale, const){
  
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
      y[i,j] = rbinom(1, 1 , prob = invlogit(beta_0[i]+t(beta[i,])%*%y_minus + const*dgamma(j, shape = shape, scale = scale))) 
    }
  }
  
  # get rid of first 100 samples => "burn in"
  new_y = y[, -(1:500)]
  return(new_y)
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
  c_hat = list(); c_hat = rep(list(c_hat), p); iter = NULL;
  for (i in 1:p){
    print(i)
    res = Estimate_coefficients_with_rate(data_source, i, max_iter, tol, lams, m)
    beta0_hat[i] = res$beta0
    beta_hat[i,] = res$beta
    f_hat[[i]] = res$f
    c_hat[[i]] = res$c
    iter[i] = res$iter
  }
  
  return(list(beta0_hat=beta0_hat, beta_hat=beta_hat, f = f_hat, c_hat = c_hat, iter = iter))
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
    
    # Checks for convergence
    #if( all(!is.na(beta)) & all(!is.na(beta_0)) & all(!is.na(c)) & sqrt(sum((beta - beta_tilde)^2)) < tol & sqrt(sum((beta_0 - beta0_tilde)^2)) <tol  & sqrt(sum((c - c_tilde)^2)) < tol) break
    if( max((beta_tilde-beta)^2)<tol^2) break
  }
  
  #DE-STANDARDISE THE COEFFICIENTS
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
  return(list(beta0=beta_0, beta=beta, f=f, c=c, f_orig = f_orig, iter = iter))
}
# Old functions -----------------------------------------------------------


# 1. Simulate binary time series data from logistic model -----------------

Simulate_data = function(p, N, beta, beta_0, cut_off){
  
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
      y[i,j] = rbinom(1, 1 , invlogit(beta_0[i]+t(beta[i,])%*%y_minus)) 
    }
  }
  
  # get rid of first 100 samples => "burn in"
  new_y = y[, -(1:500)]
  return(new_y)
}




# 3. Estimate coefficients with glmnet ------------------------------------

glmnet_function = function(data){
  #' Function to fit/estimate coefficients of glm models with glmnet 
  #'
  #' @param data simulated data of dimension
  #' @export  
  p = dim(data)[1]
  est_beta1 = est_beta2=matrix(data=NA, p,p); est_beta0_1se=est_beta0_min = NULL;
  lam_min = lam_1se = NULL;
  for(i in 1:p){
    y_data = data[i,][2:ncol(data)] # use  8999 values
    x_data = data[,(1:(ncol(data)-1))] #i.e. y_{t-1}
    
    cv.fit = cv.glmnet(t(x_data), y_data, family = "binomial",  alpha=1)
    
    # save lambda values
    lam_min[i] = cv.fit$lambda.min
    lam_1se[i] = cv.fit$lambda.1se
    
    model.fit_1se = glmnet(t(x_data), y_data, family ="binomial", alpha=1, lambda=cv.fit$lambda.1se)
    model.fit_min = glmnet(t(x_data), y_data, family ="binomial", alpha=1, lambda=cv.fit$lambda.min)
    est_beta1[i,] = as.numeric(model.fit_1se$beta)
    est_beta2[i,] = as.numeric(model.fit_min$beta)
    
    est_beta0_1se[i] = model.fit_1se$a0
    est_beta0_min[i] = model.fit_min$a0
  }
  return(list(beta_min = est_beta2, beta0 = est_beta0_min, lams = lam_min))  
}
  
 

# 4. Own implementation of coordinate descent -----------------------------


Estimate_coefficients = function(data, idx, max_iter, tol, lams){
  #'  Function to estimate coefficients of the GLM for a given dimension and regularization parameter
  #'
  #' @param idx (scalar) to indicate for which data stream we want parameter estimates
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param lams Regularisation parameter
  #' @export  
  
  # obtain data
  
  y = data[idx,][2:9000] # use  8999 values
  X = data[,(1:8999)] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
  
  
  
  
  # tranpose data so it is in standard format of 
  X = t(X)
  
  # Center the data
  x_bar = colMeans(X) # get mean of each column
  for (i in 1:p){
    X[,i] = X[,i] - x_bar[i]
  }
  
  # set regularisation parameter
  lambda = lams[idx]
  
  # set initial parameters randomly
  beta_0 =0 
  beta = rep(0,p) 
  
  # Perform cyclical coordinate descent
  for(iter in 1:max_iter) {
    beta_old <- beta
    beta0_old <- beta_0
    for(j in 1:p) {
      
      # Equations (16) and (17) of glmnet paper
      
      # compute p_tilde
      p_tilde = ( 1 + exp(-beta_0 - X %*% beta) )^-1
      # compute w
      w = p_tilde * (1-p_tilde)
      # compute z
      z = beta_0 + X %*% beta + ( (y-p_tilde)/ w )
      
      # Compute partial residual
      # i.e. fitted value excluding contribution from x_{ij}
      y_tilde <- beta_0 + X %*% beta - X[,j] * beta[j]
      # compute least-squares coefficient when fitting this partial residual to x_{ij}
      # remember to use z instead of y as the "working response"
      
      ls_coef <- crossprod(w*X[,j], z - (y_tilde))
      
      # Update beta coefficient
      beta[j] <- ( sign(ls_coef) * max(0, abs(ls_coef) - lambda) ) / sum(w*(X[,j]^2)) #soft thresholding step
      
    }
    # update beta_0
    beta_0 <- crossprod(w, z- (X%*%beta) ) / sum(w)
    beta_0 <- as.numeric(beta_0)
    beta_0 <- mean(y)
    
    
    
    # Check for convergence
    if(sqrt(sum((beta - beta_old)^2)) < tol & sqrt(sum((beta_0 - beta0_old)^2)) < tol) break
  }
  
  # Print final coefficients
  return(list(beta0=beta_0, beta=beta))
}
  

# 6. own implementation of glmnet -----------------------------------------

glm_own = function(data, p, max_iter, tol, lams){
  #'  Function to replicate glmnet (i.e. own implementation)
  #'
  #' @param p (scalar) to indicate for which data stream we want parameter estimates
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param lams Regularisation parameter

  #' @export  
  beta0=NULL; beta = matrix(NA, p ,p)
  
  
    for (i in 1:p){
      print(i)
      res = Estimate_coefficients(data, i, max_iter, tol, lams)
      beta0[i] = res$beta0
      beta[i,] = res$beta
      f=NULL;
    }

  
  # if(rate == T){
  #   for (i in 1:p){
  #     print(i)
  #     res = Estimate_coefficients_with_rate(i, max_iter, tol, lams)
  #     beta0[i] = res$beta0
  #     beta[i,] = res$beta
  #     
  #   }
  # }
  return(list(beta0 = beta0, beta=beta))
}
  




# 7. Performance Measures (updated from old code) ---------------------------------

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











# 10. plot boxplots of intercept estimates --------------------------------

beta_0_boxplots = function(beta_0_res, p, title, true_beta_0, a, b){
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
  boxplot(df, main = title, ylab = expression(hat(beta[0])), ylim = c(a,b), col="skyblue")
  abline(h=true_beta_0, col="navy", lty ="dashed", lwd = 2)
  #return(df)
}





# 11.a New metric Function ------------------------------------------------


metrics_new = function(res, gt, threshold){
  
  beta_list = map(res, 1)
  # put entries less than threshold to zero
  lapply(beta_list, function(x) ifelse(abs(x) < threshold, 0, x))
  av_mse = mean( unlist(lapply(beta_list, function(x) performance_measures(x, gt)$l2)) )
  # standard errors
  se = std_err(unlist(lapply(beta_list, function(x) performance_measures(x, gt)$l2)) )
  
  F1 = mean(unlist(lapply(beta_list, function(x) performance_measures(x, gt)$F1)))
  F1_se = std_err(unlist(lapply(beta_list, function(x) performance_measures(x, gt)$F1)))
  
  # split results for excitatory neurons and inhibitory neurons
  # ground truth
  true_beta = as.vector(gt)
  beta2 = true_beta
  beta2[beta2<0]=-1 # change negative entries to -1
  beta2[beta2>0] = 1 # change positive entries to +1
  
  idx_ex = which(beta2>=0) #indices of excitatory and zero edges
  idx_ind = which(beta2<=0) #indices of excitatory and zero edges
  
  # estimated 
  est_beta_vec = lapply(beta_list, as.vector)
  est_beta2 = est_beta_vec
  est_inhibitory_interactions = list(); est_inhibitory_interactions = rep(list(est_inhibitory_interactions), length(est_beta2));
  est_excitatory_interactions = est_inhibitory_interactions;
  for(i in 1:length(est_beta2)){
    est_beta2[[i]][est_beta2[[i]]<0]=-1 # change negative entries to -1
    est_beta2[[i]][est_beta2[[i]]>0]=1 # change positive entries to +1
    
    est_inhibitory_interactions[[i]] = est_beta2[[i]][idx_ind]
    est_excitatory_interactions[[i]] = est_beta2[[i]][idx_ex]
    
    est_inhibitory_interactions[[i]][est_inhibitory_interactions[[i]]>0] = 0 # set falsely detected excitatory edges to 0
    est_excitatory_interactions[[i]][est_excitatory_interactions[[i]]<0] = 0 # set falsely detected inhibitory edges to 0
    
  }
  
  
  true_inhibitory_interactions = beta2[beta2<=0]
  true_excitatory_interactions = beta2[beta2>=0]
  
  if(length(unique(true_inhibitory_interactions))>1){
    # only check if inhibitory interactions are present
    # auroc for excit/inhib interactions
    roc_obj_in = lapply(est_inhibitory_interactions, function(x){roc(true_inhibitory_interactions, x, direction="<")})
    auc_list_in = lapply(roc_obj_in, auc)
    auc_obj_in = mean(unlist(auc_list_in))
    se_auc_in = std_err(unlist(auc_list_in))
  }
  
  if(length(unique(true_inhibitory_interactions))==1){
    auc_obj_in = NA
    se_auc_in = NA
  }
  
  roc_obj_ex = lapply(est_excitatory_interactions, function(x){roc(true_excitatory_interactions, x, direction="<")})
  auc_list_ex = lapply(roc_obj_ex, auc)
  
  auc_obj_ex = mean(unlist(auc_list_ex))
  se_auc_ex = std_err(unlist(auc_list_ex))
  
  
  # vectorise matrices for AUROC
  true_beta = as.vector(gt)
  est_beta_vec = lapply(beta_list, as.vector)
  
  # make into 0 1 vectors
  true_beta[true_beta!=0]=1
  for(i in 1:length(est_beta_vec)){
    est_beta_vec[[i]][est_beta_vec[[i]]!=0]=1
    
  }
  
  # Check if the response has more than two levels
  if (length(unique(true_beta)) > 2) {
    
    roc_obj = lapply(est_beta_vec, function(x){multiclass.roc(true_beta, x, direction="<")})
    auc_list = lapply(roc_obj, auc)
  } else {
    # Use roc for binary classification
    roc_obj = lapply(est_beta_vec, function(x){roc(true_beta, x, direction="<")})
    auc_list = lapply(roc_obj, auc)
  }
  
  auc_obj = mean(unlist(auc_list))
  se_auc = std_err(unlist(auc_list))
  
  return(list(F1 = F1, av_mse = av_mse, av_AUC = auc_obj, se = se, f1_se = F1_se, auc_se = se_auc, auc_in = auc_obj_in, auc_ex = auc_obj_ex))
}




# 11. Function to obtain performance metrics from simulation study --------

metric_function = function(out,p, true_beta, true_beta_0, title){
  #'  Function to obtain performance metrics from simulation study
  #'
  #' @param out .RDS file from simualtion study script
  #' @param p dimension
  #' @param true_beta true interaction matrix
  #' @param true_beta_0 true intercept in model specification
  #' @param title title of beta-0 bixplots
  #' @export  
  
  beta_hats = map(out, 1)
  metrics = lapply(beta_hats, function(x) performance_measures(x, truth=true_beta))
  
  l2 = mean(unlist(map(metrics,3)))
  f1 = mean(unlist(map(metrics, 4)))
  
  l2_all = (unlist(map(metrics,3)))

  
  return(list(l2=l2, f1=f1, l2_all = l2_all))
}


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




# 13. Plot output of f estimates from stim study --------------------------

f_plots = function(f_hat, title){
  #'  Function to plot estimates of f_i(t)
  #'
  #' @param f_hat list of estimates for each unit/i
  #' @param title title of plot
  #' @export  
  f_hat_list = list(); av_f_hat = rep(list(f_hat_list), p)
  for(i in 1:p){
    c = map(f_hat, i)
    av_f_hat[[i]] = Reduce("+", c)/length(c)
  }
  
  
  plot(av_f_hat[[1]], xlab = expression(t), ylab = expression(hat(f)(t)), main = title, type='l')
  for(i in 2:p){
    lines(av_f_hat[[i]], col=i)
  }
  
  const = 10000
  N=10000;
  lines(const*dnorm(seq(500,8999), mean = N/2, sd=N/10), col="blue", lwd =2, lty = "dashed")
}


# 14. Get uncentered estimates of f  --------------------------------------

get_f = function(out,m, t_end, unit_id){
  #' Function to obtain f estimate with non-centred basis functions but estimated coefficients
  #'
  #' @param out RDS file from STORM
  #' @param m number of basis functions
  #' @param t_end dimension of previous values
  #' @param unit_id index for considered dimension
  #' @export  
  phi <- bs(seq(1, t_end), df=m, intercept = F) 
  c_hats = map(out, 4)
  c_hats_id = map(c_hats, unit_id)
  f = lapply(c_hats_id, function(x) phi%*%as.matrix(x))
  
  return(f)
}


# 15. Function to convert list to matrix ----------------------------------

list_to_matrix = function(f){
  mat_f = matrix(NA, ncol=length(f[[1]]), nrow = length(f))
  for(i in 1:100){
    mat_f[i,]=f[[i]]
  }
  return(mat_f)
}


# 16. Function to extract f estimates from storm results ------------------

get_f = function(out,m, t_end, unit_id){
  #' Function to obtain f estimate with non-centred basis functions but estimated coefficients
  #'
  #' @param out RDS file from STORM
  #' @param m number of basis functions
  #' @param t_end dimension of previous values
  #' @param unit_id index for considered dimension
  #' @export  
  phi <- bs(seq(1, t_end), df=m, intercept = F) 
  c_hats = map(out, 4)
  c_hats_id = map(c_hats, unit_id)
  f = lapply(c_hats_id, function(x) phi%*%as.matrix(x))
  
  return(f)
}



# 17. Function for debiased lasso -----------------------------------------

debiased_lasso = function(idx, data, p, beta_0, beta_hat, f_hat, alpha){
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
  
  
  first_deriv = - ( t(y-p_t) %*% X1 ) / length(y) ##need to divide by n - double check - think do according to van de geer?? (1 x p)
  vec_pt = as.vector(p_t)
  D = diag(vec_pt * (1-vec_pt))
  sigma_hat = ( t(X1) %*% D %*% X1 ) /length(y) #p x p
  
  # checks
  W = sqrt(D) %*% X1
  #check = (t(W) %*% W )/length(y)
  #round(check,8) == round(sigma_hat,8)
  
  # use W as input to standard glmnet??
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


# 19. Plotting for f estimates --------------------------------------------

plot_f_cis = function(f, unit_id, title,a){
  
  #f1 = map(f,unit_id )
  check = list_to_matrix(f)
  library(matrixStats)
  plot(a+ colQuantiles(check, probs=0.5), ylim=c(-1,6), type='l', xlab="t", ylab=expression(hat(f)(t)), main = title)
  lines(a+colQuantiles(check, probs =0.975), col="red")
  lines( a+colQuantiles(check, probs=0.025), col="red")
  const = 10000
  N=10000;
  lines(const*dnorm(seq(500,8999), mean = N/2, sd=N/10), col="blue", lwd =2, lty = "dashed")
  legend("topright", c("truth", "95% CIs", "Avg"), col=c("blue", "red", "black"), lty = c(2,1,1), lwd = c(2,1,1))
  
  #return(check)
  
}


# 20. Convert list to matrix ----------------------------------------------

list_to_matrix = function(f){
  mat_f = matrix(NA, ncol=length(f[[1]]), nrow = length(f))
  for(i in 1:100){
    mat_f[i,]=f[[i]]
  }
  return(mat_f)
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

# 22. Function to do refitting only on support of the regularised  estimate (single unit) --------

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


# 23. Refit on support of all units ---------------------------------------

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
  c = list(); c = rep(list(c), p);
  for (i in 1:p){
    print(i)
    res = re_fit_supp(data_source, i, max_iter, tol, m, reg_beta[i,])
    beta0[i] = res$beta0
    beta[i,] = res$beta
    f[[i]] = res$f_orig
    c[[i]] = res$c
  }
  
  return(list(beta0=beta0, beta_hat=beta, f_hat = f, c_hat = c))
}

# 24. updated get f function ----------------------------------------------

get_f_new = function(refit, unit_id){
  # updated function for f_orig results 
  f_est = map(refit,3)
  f_est = map(f_est, unit_id)
  return(f_est)
}

# 25. Ridge Estimator (single unit) -----------------------------------------------------

Estimate_coefficients_ridge = function(data, idx, max_iter, tol, lams, m){
  #'  Function to estimate coefficients of the GLM for a given dimension and regularization parameter
  #'
  #' @param idx (scalar) to indicate for which data stream we want parameter estimates
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param lams Regularisation parameter
  #' @param m number of basis coefficients for splines
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
  lambda = lams[idx]
  
  # set initial parameters 
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
      
      # Compute partial residual
      # i.e. fitted value excluding contribution from x_{ij}
      
      y_tilde <- beta_0 + X %*% beta - X[,j] * beta[j] + f
      
      # compute least-squares coefficient when fitting this partial residual to x_{ij}
      # remember to use z instead of y as the "working response"
      
      ls_coef <- crossprod(w*X[,j], z - (y_tilde))
      
      # Update beta coefficient
      beta[j] <- ls_coef / ( sum(w*(X[,j]^2)) + lambda) 
      
      
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
    
    # Check for convergence
    #if( sqrt(sum((beta - beta_tilde)^2)) < tol & sqrt(sum((beta_0 - beta0_tilde)^2)) < tol  & sqrt(sum((c - c_tilde)^2)) < tol) break
  }
  
  #DE-STANDARDISE THE COEFFICIENTS
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


# 26. Ridge estimator (all units) -----------------------------------------

Estimate_coefficients_ridge_all <- function(data_source, p, max_iter, tol, lams, m){
  #'  Function to estimate coefficients of the GLM for all dimensions and regularization parameter
  #'
  #' @param idx (scalar) to indicate for which data stream we want parameter estimates
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param lams Regularisation parameter
  #' @param m number of basis coefficients for splines
  #' @export  
  p = dim(data_source)[1]
  print(p)
  beta0=NULL; beta = matrix(NA, p ,p); f = list(); f = rep(list(f), p); 
  c = list(); c = rep(list(c), p);
  for (i in 1:p){
    print(i)
    res = Estimate_coefficients_ridge(data_source, i, 1000, 1e-8, lams, m)
    beta0[i] = res$beta0
    beta[i,] = res$beta
    f[[i]] = res$f_orig
    c[[i]] = res$c
  }
  
  return(list(beta0=beta0, beta_hat=beta, f_hat = f, c_hat = c))
}

# 27. IRLS algorithm for single unit --------------------------------------

IRLS_fit = function(data, idx, max_iter, tol, m){
  #'  Function to estimate coefficients of the GAPLA model with fixed support of the regularised estimate
  #'
  #' @param idx (scalar) to indicate for which data stream we want parameter estimates
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param m number of basis coefficients for splines
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
  
  beta = rep(0,p)
  # get coefficient for standardised data
  
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
    
    # Beta matrix update
    for(j in 1:p) {
      
      # Compute partial residual
      # i.e. fitted value excluding contribution from x_{ij}
      
      y_tilde <- beta_0 + X %*% beta - X[,j] * beta[j] + f
      
      # compute least-squares coefficient when fitting this partial residual to x_{ij}
      # remember to use z instead of y as the "working response"
      
      ls_coef <- crossprod(w*X[,j], z - (y_tilde))
      
      # Update beta coefficient
      beta[j] <- ls_coef / sum(w*(X[,j]^2)) 
      
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


# 28. IRLS for all units --------------------------------------------------

IRLS_all <- function(data_source, p, max_iter, tol, m){
  #'  Function to estimate coefficients of the refitted GAPLA model for all dimensions 
  #'
  #' @param data_srouce data of interest
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param p number of dimenions
  #' @param m number of basis coefficients for splines
  #' @export  
  p = dim(data_source)[1]
  print(p)
  beta0=NULL; beta = matrix(NA, p ,p); f = list(); f = rep(list(f), p); 
  c = list(); c = rep(list(c), p);
  for (i in 1:p){
    print(i)
    res = IRLS_fit(data_source, i, max_iter, tol, m)
    beta0[i] = res$beta0
    beta[i,] = res$beta
    f[[i]] = res$f_orig
    c[[i]] = res$c
  }
  
  return(list(beta0=beta0, beta_hat=beta, f_hat = f, c_hat = c))
}


# 29. Adaptive lasso for single unit --------------------------------------

Adaptive_lasso = function(data, idx, max_iter, tol, lams, m, weight_matrix){
  #'  Function to estimate coefficients of the GLM for a given dimension and regularization parameter
  #'
  #' @param idx (scalar) to indicate for which data stream we want parameter estimates
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param lams Regularisation parameter
  #' @param m number of basis coefficients for splines
  #' @param weight_matrix matrix of weights for adaptive lasso (pre computed through IRLS or Ridge regression)
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
  lambda = lams[idx]
  weights = weight_matrix[idx,] # take row of weight matrix which corresponds to the neuron of interest
  
  # set initial parameters 
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
      
      # Compute partial residual
      # i.e. fitted value excluding contribution from x_{ij}
      
      y_tilde <- beta_0 + X %*% beta - X[,j] * beta[j] + f
      
      # compute least-squares coefficient when fitting this partial residual to x_{ij}
      # remember to use z instead of y as the "working response"
      
      ls_coef <- crossprod(w*X[,j], z - (y_tilde))
      
      # Update beta coefficient
      beta[j] <- ( sign(ls_coef  ) * pmax(0, abs(ls_coef ) - (lambda*weights[j]) ) ) / sum(w*(X[,j]^2)) #soft thresholding step
      
      
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
    
    # Check for convergence
    #if( sqrt(sum((beta - beta_tilde)^2)) < tol & sqrt(sum((beta_0 - beta0_tilde)^2)) < tol  & sqrt(sum((c - c_tilde)^2)) < tol) break
  }
  
  #DE-STANDARDISE THE COEFFICIENTS
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



# 30. Adaptive Lasso for all units ----------------------------------------

Ad_lasso_all <- function(data_source, p, max_iter, tol, lams, m, weight_matrix){
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
  c = list(); c = rep(list(c), p);
  for (i in 1:p){
    print(i)
    res = Adaptive_lasso(data_source, i, max_iter, tol, lams, m, weight_matrix)
    beta0[i] = res$beta0
    beta[i,] = res$beta
    f[[i]] = res$f_orig
    c[[i]] = res$c
  }
  
  return(list(beta0=beta0, beta_hat=beta, f_hat = f, c_hat = c))
}







# 32. Simulate data with normal and gamma ---------------------------------

Simulate_data_add_gamma_norm = function(p, N, beta, beta_0, cut_off, shape, scale, const, mu, sigma){
  
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
      if(p<=p/2){
        # first half add gamma
        y[i,j] = rbinom(1, 1 , prob = invlogit(beta_0[i]+t(beta[i,])%*%y_minus + const*dgamma(j, shape = shape, scale = scale))) 
      }
      if(p>p/2){
        # second half add normal
        y[i,j] = rbinom(1, 1 , prob = invlogit(beta_0[i]+t(beta[i,])%*%y_minus + const*dnorm(j, mean = mu, sd=sigma))) 
      }
    }
  }
  
  # get rid of first 100 samples => "burn in"
  new_y = y[, -(1:500)]
  return(new_y)
}


# Warm starts -------------------------------------------------------------

Estimate_coefficients_with_rate_warm = function(data, idx, max_iter, tol, lams, m, beta_0, beta, c){
  #'  Function to estimate coefficients of the GLM for a given dimension and regularization parameter
  #'
  #' @param idx (scalar) to indicate for which data stream we want parameter estimates
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param lams Regularisation parameter
  #' @param m number of basis coefficients for splines
  #' @param beta_0 (scalar) estimate obtained via c.d. algorithm for previous lambda - can be set to 0 if not employing warm starts
  #' @param beta (1 x p vector) estimate obtained via c.d. algorithm for previous lambda - can be set to rep(0,p) if no warm starts
  #' @param c (1 x m vector) estimate obtained via c.d. algorithm for previous lambda - can be set to rep(0,m) if no warm starts
  #' @export  
  
  # obtain data
  
  #y = data[idx,][2:9000] # use  8999 values
  #X1 = data[,(1:8999)] #i.e. y_{t-1} - matrix of predictors (for all p - p x N matrix)
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
  #beta_0 = 0
  beta = rep(0,p)
  
  #c <- rep(0, m)
  
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
    z = beta0_tilde + X %*% beta_tilde + f_tilde + ( (y-p_tilde)/ w )
    
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
    
    # Checks for convergence
    if( max((beta_tilde-beta)^2)<tol^2) break
  }
  
  #DE-STANDARDISE THE COEFFICIENTS
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
  return(list(beta0=beta_0, beta=beta, f=f, c=c, f_orig = f_orig, iter = iter))
}

Estimate_coefficients_logit_warm <- function(data_source, p, max_iter, tol, lams, m, beta_0, beta, c){
  #'  Function to estimate coefficients of the GLM for all dimensions and regularization parameter
  #'
  #' @param idx (scalar) to indicate for which data stream we want parameter estimates
  #' @param max_iter Maximum number of iterations
  #' @param tol tolereance level for the coordinate descent algorithm
  #' @param lams Regularisation parameter
  #' @param m number of basis coefficients for splines
  #' @export  
  p = dim(data_source)[1]
  #print(p)
  beta0_hat=NULL; beta_hat = matrix(NA, p ,p); f = list(); f_hat = rep(list(f), p); 
  c_hat = list(); c_hat = rep(list(c_hat), p); iter = NULL;
  for (i in 1:p){
    print(i)
    res = Estimate_coefficients_with_rate_warm(data_source, i, max_iter, tol, lams, m,  beta_0[i], beta[i,], c[[i]])
    beta0_hat[i] = res$beta0
    beta_hat[i,] = res$beta
    f_hat[[i]] = res$f
    c_hat[[i]] = res$c
    iter[i] = res$iter
  }
  
  return(list(beta0_hat=beta0_hat, beta_hat=beta_hat, f = f_hat, c_hat = c_hat, iter = iter))
}