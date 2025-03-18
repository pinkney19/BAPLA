import matplotlib.pyplot as plt
import numpy as np
from scipy.stats import gaussian_kde
import matplotlib.patches as mpatches
import pandas as pd
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler

def Alligned_Raster_plot(unit_spike_times, stim_on, before, after, title, bin_size):
    trial_spikes = []
    for time in stim_on:
        # Compute spike times relative to stimulus onset
        aligned_spikes = unit_spike_times - time
        # Keep only spike times in a given time window around the stimulus onset
        aligned_spikes = aligned_spikes[
            (-before < aligned_spikes) & (aligned_spikes < after)
        ]
        trial_spikes.append(aligned_spikes)
    #pre_stim = [x[x<0] for x in trial_spikes]
    #num_spikes = [len(x) for x in pre_stim]
    #pre_stim_list =[list(x) for x in pre_stim]

    # plot raster
    fig, axs = plt.subplots(2, 1, sharex="all")
    plt.xlabel("time (s)")
    axs[0].eventplot(trial_spikes)

    axs[0].set_ylabel("trial")
    axs[0].set_title("unit {}".format(title))
    axs[0].axvline(0, color=[0.5, 0.5, 0.5])

    bins = int((before + after) / bin_size)

    axs[1].hist(np.hstack(trial_spikes), bins)
    axs[1].axvline(0, color=[0.5, 0.5, 0.5])

    return trial_spikes

def firing_rate_plots(region_idx, n, title, bin_size, before, after , stim, spike_df) :
    """

    This function is used to plot example firing rates of neurons in a given region.

    Parameters:
    -----------
    
    region_idx : indices of spike_df corresponding to region of interest, e.g. idx_MB, idx_vis, idx_bas
    n : number of neurons/units to plot
    title : title of plot
    bin_size : bin size for binning spike times
    before : time before stimulus onset
    after : time after stimulus onset
    stim : stimulus onset times or response times

    Returns:
    --------
    plot:
        plots of firing rates.

    """
    rate = [list() for x in range(n)]
    fig, axs = plt.subplots(1,n, figsize=(20, 4))
    for i in range(n):
        #print(i)
        # Extract spike times aligned to stimulus onset
        times = Alligned_Raster_plot(spike_df["spike_times"][region_idx[i+1]], stim, before, after, i, bin_size)
        # Count spikes in each bin across all trials
        rate[i], bin_edges = np.histogram(np.hstack(times), bins=int((before+after)/bin_size))
        plt.close()
        # Normalize rates
        #rate_MB[i] = np.concatenate(preprocessing.normalize([rate_MB[i]])) #need concatenate for "de-listing"
        
        # plot rates
        plt.subplot(1, n, i+1)
        x = np.arange(-before, after, 0.001)
        axs[i].plot(x[:-1], rate[i])
        axs[i].axvline(0, color="r") #[0.5, 0.5, 0.5])
    fig.suptitle(title)
    plt.show()

def average_spike_count(stim_on, unit_spike_times, before, after): 
    """

    This function is used to calculate the average firing rates of neurons in a given region.

    Parameters:
    -----------
    
    stim_on             : stimulus onset times or response times
    unit_spike_times    : spike times of a given unit
    before              : time before stimulus onset
    after                  : time after stimulus onset
    

    Returns:
    --------
    plot:
        trial average firing rates.

    """
    trial_spikes = []
    for time in stim_on:
        # Compute spike times relative to stimulus onset
        aligned_spikes = unit_spike_times - time
        # Keep only spike times in a given time window around the stimulus onset
        aligned_spikes = aligned_spikes[
            (-before < aligned_spikes) & (aligned_spikes < after)
        ]
        trial_spikes.append(aligned_spikes)
    # count number spikes in each trial
    counts = [len(x) for x in trial_spikes]
    # calculate average number of spikes per trial
    avg_counts = np.mean(np.array(counts))

    return avg_counts

def extract_trial_spikes(stim_on, unit_spike_times, before, after) :
    """

    This function is used to extract spike times aligned to stimulus onset/response times.
    Parameters:
    -----------
    
    stim_on             : stimulus onset times or response times
    unit_spike_times    : spike times of a given unit
    before              : time before stimulus onset
    after                  : time after stimulus onset
    

    Returns:
    --------
    plot:
        trial_spikes (list) : list of trial spike times.

    """
    trial_spikes = []
    for time in stim_on:
        # Compute spike times relative to stimulus onset
        aligned_spikes = unit_spike_times - time
        # Keep only spike times in a given time window around the stimulus onset
        aligned_spikes = aligned_spikes[
            (-before < aligned_spikes) & (aligned_spikes < after)
        ]
        trial_spikes.append(aligned_spikes)
    return trial_spikes

def kde_estimates(event, spike_df, before, after) :
    """

    This function is used to calculate the kdes for each unit aligned to an event of interest
    -----------
    
    event    : stimulus onset times or response times
    spike_df : spike times of a given unit
    before   : time before stimulus onset
    after    : time after stimulus onset
    

    Returns:
    --------
    plot:
        kde_list (list) : list of kernel density estimates for each selected unit.
        idx_empty (list): list of indices of units with no spikes in the selected window.

    """
    idx_empty = []
    kde_list = []
    for i in range(len(spike_df)) :
        print(i)
        # Extract spike times aligned to stimulus onset
        #times = fn.extract_trial_spikes(stim_on, df_unaligned["spike times"][i], 0.2, 0.4)
        times = extract_trial_spikes(event, spike_df["spike_times"][i], before, after)
        
        # Check there are spikes in at least one trial of the required window
        if any (arr.size >1 for arr in times) :
        
            # Fit the KDE
            kde = gaussian_kde(np.hstack(times))    
            # Define the range for the x values
            x = np.linspace(-before, after, 1000)
            # Calculate the KDE values for these x values
            kde_values = kde(x)
            # Save in list
            kde_list.append(kde_values)
            # Plot the histogram and KDE
            #plt.hist(np.hstack(times), bins=100, density=True)
            plt.plot(x, kde_values)
        else : 
            # Save index of empty unit (for all trials)
            idx_empty.append(i)
    plt.show()
    return kde_list, idx_empty

def plot_kdes_by_region(kde_list, spike_df, before, after) :
    """
    This function is used to plot the kernel density estimates for the selected units color coded by region.
    -----------
    
    kde_list : list of kernel density estimates for each selected unit
    spike_df : spike times of a given unit
    before   : time before stimulus onset/response time
    after    : time after stimulus onset/response time
    
    Returns:
    --------
    plot:
        Kernel Density Estimators for the Considered Units.
    """
    # Create a color map for the regions
    kde_df = pd.DataFrame({'region': spike_df["region"], 'kde':kde_list})
    unique_regions = np.unique(kde_df['region'])
    unique_regions = [x[0] for x in unique_regions]
    colors = plt.cm.rainbow(np.linspace(0, 1, len(unique_regions)))
    color_map = dict(zip(unique_regions, colors))

    # Plot the KDEs colored by region
    plt.figure()
    for i in range(len(kde_list)):
        x = np.linspace(-before, after, 1000)
        plt.plot(x, kde_list[i], color=color_map[kde_df['region'].iloc[i][0]])
    plt.title("Kernel Density Estimators for the Considered Units")
    # Create a Patch object for each region
    patches = [mpatches.Patch(color=color, label=region) for region, color in color_map.items()]

    # Add the legend to the plot
    plt.legend(handles=patches, loc='upper left') 
    plt.show()
   
def plot_clustered_kdes(assignments, data_stim, kde_df, before, after, kde_list, n_clusters, title):
    """
    This function is used to plot the clustered kernel density estimates (coloured by cluster membership)
    -----------
    
    assignments : list of kernel density estimates for each selected unit
    data_stim : standardised data 
    kde_df : spike times of a given unit
    before   : time before stimulus onset/response time
    after    : time after stimulus onset/response time
    
    Returns:
    --------
    plot:
        Kernel Density Estimators for the Considered Units.
    """
    # Plot the data points colored by their cluster assignments
    unique_regions = np.unique(kde_df['region'])
    unique_regions = [x[0] for x in unique_regions]
    colors = plt.cm.rainbow(np.linspace(0, 1, len(unique_regions)))
    color_map2 = dict(zip(range(n_clusters), colors)) 
    x = np.linspace(-before, after, 1000)
    for i in range(data_stim.shape[0]):
        #plt.plot(x, data_stim[i], color=color_map2[assignments[i]])
        plt.plot(x, kde_list[i], color=color_map2[assignments[i]])
    plt.title(title)
    patches = [mpatches.Patch(color=color, label=cluster) for cluster, color in color_map2.items()]
    # Add the legend to the plot
    plt.legend(handles=patches, loc='upper left') # bbox_to_anchor=(1.05, 1) -< to put legend outside of plot
    plt.show()

def cluster_kdes(kde_list, n_clusters):
    """
    This function is used to cluster the KDEs according to Kmeans clustering.
    -----------
    
    kde_list : list of kernel density estimates for each selected unit
    n_clusters : number of clusters
    
    Returns:
    --------
    assignments : cluster assignments
    centroids : cluster centroids
    data_standardized : Standardised data
    """
    
    # Standardise data
    scaler = StandardScaler()
    data_standardized = scaler.fit_transform(kde_list)

    # Apply K-means clustering
    kmeans = KMeans(n_clusters, random_state=0)
    kmeans.fit(data_standardized)
    # Get the cluster assignments and centroids
    assignments = kmeans.labels_
    centroids = kmeans.cluster_centers_

    # Get the cluster assignments and centroids
    assignments = kmeans.labels_
    centroids = kmeans.cluster_centers_
    
    return assignments, centroids, data_standardized

def plot_standardized_kdes(data, spike_df, kde_list, before, after):
    """
    This function is used to plot the standardised kernel density estimates for the selected units color coded by region.
    -----------
    
    data : standardised data
    spike_df : spike times of a given unit
    kde_list : list of kernel density estimates for each selected unit
    before   : time before stimulus onset/response time
    after    : time after stimulus onset/response time
    
    Plot:
    --------
    standardised kernel density estimates for the considered units.
    """
    # Create a color map for the regions
    kde_df = pd.DataFrame({'region': spike_df["region"], 'kde':kde_list})
    unique_regions = np.unique(kde_df['region'])
    unique_regions = [x[0] for x in unique_regions]
    colors = plt.cm.rainbow(np.linspace(0, 1, len(unique_regions)))
    color_map = dict(zip(unique_regions, colors))
    for i in range(0, len(data)):
        x = np.linspace(-before, after, 1000)
        plt.plot(x, data[i], color=color_map[kde_df['region'].iloc[i][0]])
    plt.title("Standardised Kernel Density Estimators for the Considered Units")
    # Create a Patch object for each region
    patches = [mpatches.Patch(color=color, label=region) for region, color in color_map.items()]
    plt.legend(handles=patches, loc='upper left') #bbox_to_anchor=(1.05, 1)
    plt.show()

def plot_psth(spike_times, stim_on, bin_size, pre_time, post_time, title):
    # Calculate the time window
    time_window = np.arange(-pre_time, post_time, bin_size)
    
    # Initialize histogram
    hist = np.zeros(len(time_window) - 1)
    
    # Loop through each stimulus onset
    for stim in stim_on:
        aligned_spikes = spike_times - stim
        hist += np.histogram(aligned_spikes, bins=time_window)[0]
    
    # Normalize histogram
    hist = ( hist / len(stim_on) ) / bin_size
    
    # Plot PSTH
    plt.bar(time_window[:-1], hist, width=bin_size)
    plt.xlabel('Time (s)')
    plt.ylabel('Firing Rate (Hz)')
    plt.title(title)
    plt.axvline(0, color=[0.5, 0.5, 0.5])
    plt.show()

def Alligned_Raster(unit_spike_times, stim_on, before, after, title):
    trial_spikes = []
    for time in stim_on:
        # Compute spike times relative to stimulus onset
        aligned_spikes = unit_spike_times - time
        # Keep only spike times in a given time window around the stimulus onset
        aligned_spikes = aligned_spikes[
            (-before < aligned_spikes) & (aligned_spikes < after)
        ]
        trial_spikes.append(aligned_spikes)
    #pre_stim = [x[x<0] for x in trial_spikes]
    #num_spikes = [len(x) for x in pre_stim]
    #pre_stim_list =[list(x) for x in pre_stim]

    # plot raster
    #fig, axs = plt.subplots(2, 1, sharex="all")
    plt.xlabel("time (s)")
    plt.eventplot(trial_spikes)
    # set y label
    plt.ylabel("trial")
    plt.title(title)
    plt.axvline(0, color=[0.5, 0.5, 0.5])
    plt.show()
    
def get_trial_spikes(unit_spike_times, stim_on, before, after):
    trial_spikes = []
    for time in stim_on:
        # Compute spike times relative to stimulus onset
        aligned_spikes = unit_spike_times - time
        # Keep only spike times in a given time window around the stimulus onset
        aligned_spikes = aligned_spikes[
            (-before < aligned_spikes) & (aligned_spikes < after)
        ]
        trial_spikes.append(aligned_spikes)
    return trial_spikes

# Extract neurons from a given region 
def extract_region(spike_df, chosen_region):

    # get indices of region of interest
    idx = spike_df.loc[spike_df['region'] == chosen_region].index
    # how many neurons for this region?
    d = len(idx)
    # make spike_df just for specific region
    spike_df_chosen_region = spike_df.loc[idx]
    return d, spike_df_chosen_region

# Exract only trial aligned data - aligned to stimulus onset
def trial_aligned_data(spike_df_region, stim_on):
# get data aligned to stimulus onset
    trial_spikes_region = []
    for i in range(len(spike_df_region)):
        #i = 1
        trial_spikes = get_trial_spikes(spike_df_region.iloc[i]["spike_times"], stim_on, 0.2,0.4)
        #counts = [len(x) for x in trial_spikes]
        #if(any(count < 2 for count in counts)):
        #    print("Neuron {} has less than 2 spikes in some trials".format(i))
        #else:
        trial_spikes_region.append(trial_spikes)
    return trial_spikes_region

# calculate max firing rate for each neuron
def mean_firing(trial_spikes) :
    # Calculate the time window
    time_window = np.arange(-0.2, 0.4, 0.01)

    # Initialize histogram
    hist = [np.zeros(len(time_window)-1) for x in trial_spikes]
    # loop over each neuron 
    for j in range(len(trial_spikes)):
        for i in range(len(trial_spikes[j])):
            hist[j] += np.histogram(trial_spikes[j][i], bins=time_window)[0]

    # normalize histogram
    hist = [x / len(trial_spikes[j]) / 0.01 for j, x in enumerate(hist)]
        ## Normalize histogram
    #hist = [( hist / len(x) ) / 0.01 for x in trial_spikes_gpe]

    mean_firing = [np.mean(x) for x in hist]
    return mean_firing