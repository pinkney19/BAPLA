
# Load data ---------------------------------------------------------------

left_trial_spikes <- readRDS("~/Downloads/BAPLA/Real_Data/Data/left_trial_spikes.RDS")

# Remove regions with no spikes
left_data =  Filter(function(x) length(x) > 0, left_trial_spikes)
regions <- names(left_trial_spikes)

xs = seq(-0.2, 0.4, by = 0.001) #delta = 1ms


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

bin_all = function(spike_data){
  binned  = spike_data;
  for(i in 1:length(spike_data)){
    for(j in 1:length(spike_data[[i]])){
      binned[[i]][[j]] = lapply(spike_data[[i]][[j]], function(x) bin_data(x, xs))
    }
  }
  return(binned)
}

left_binned = bin_all(left_data)


# checks with first data stream
data = left_trial_spikes$CA1[[1]]
check = lapply(data, function(x) bin_data(x, xs))
# check done correctly
unlist(lapply(check, sum)) == lengths(data)
