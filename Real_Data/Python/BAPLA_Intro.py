import matplotlib.pyplot as plt
import numpy as np
from pynwb import NWBHDF5IO
import math 
import networkx as nx
import pandas as pd
import pickle
import DANDI_000017_Functions as fn
import seaborn as sns

# Open Spike Data 
with open('spike_df.pickle', 'rb') as handle:
    spike_df = pickle.load(handle)

# Open Stimulus Onset Times
with open('stim_on.pickle', 'rb') as handle:
    stim_on = pickle.load(handle)

# Open Left Data
with open('left_data.pickle', 'rb') as handle:
    left_data = pickle.load(handle)

# Open Right Data
with open('right_data.pickle', 'rb') as handle:
    right_data = pickle.load(handle)

# Open Included Data
with open('included_data.pickle', 'rb') as handle:
    included_data = pickle.load(handle)

# get indices of trials with left and right stimuli that were included
included_idx = np.where(included_data == 1)[0]
left_included_idx = np.where(left_data[included_idx] == 1)[0] # 106 with left contrast = 1  and were included
right_included_idx = np.where(right_data[included_idx] == 1)[0] # 73 right contrast = 1  and were included

# check what regions are in this dataset
regions = spike_df['region']
unique_regions = np.unique(regions.values)

r = [unique_regions[i][0] for i in range(len(unique_regions))]

# set up empty dictionaries to store spike times for each region

spike_times_all_regions = {region:[] for region in r}
left_times_all_regions = {region:[] for region in r}
right_times_all_regions = {region:[] for region in r}

for region in unique_regions:
    # Get data for the region
    d, spike_df_region = fn.extract_region(spike_df, region[0])
    
    # Get spike times aligned to stimulus onset for that region 
    trial_spikes_region = fn.trial_aligned_data(spike_df_region, stim_on)
    
    # Calculate mean firing rate
    mean_firing = fn.mean_firing(trial_spikes_region)
    
    # Get locations of neurons with mean firing rate > 5Hz
    idx = np.where(np.array(mean_firing) > 10)[0]
    
    # Extract firing times from trials data
    spike_times_region = [trial_spikes_region[i] for i in idx]
    
    # Store the result in a dictionary
    spike_times_all_regions[region[0]] = spike_times_region

    # Store left and right trials in separate dictionaries.  
    for unit in range(len(idx)):
        # Check if spike_times_region is not empty
        if len(spike_times_region[unit]) > 0:
            # left trials
            left_trials = [spike_times_region[unit][i] for i in left_included_idx]
            left_times_all_regions[region[0]].append(left_trials)
            # right trials 
            right_trials = [spike_times_region[unit][i] for i in right_included_idx]
            right_times_all_regions[region[0]].append(right_trials)
    # if spike_times_region:
    #     # Store left trials in a dictionary 
    #     left_trials = [spike_times_region[0][i] for i in left_included_idx]
    #     left_times_all_regions[region[0]] = left_trials

        # Store right trials in a dictionary
        # right_trials = [spike_times_region[0][i] for i in right_included_idx]
        # right_times_all_regions[region[0]] = right_trials
    
# Save the data
with open('spike_times_all_regions.pickle', 'wb') as handle:
    pickle.dump(spike_times_all_regions, handle, protocol=pickle.HIGHEST_PROTOCOL)

with open('left_times_all_regions.pickle', 'wb') as handle:
    pickle.dump(left_times_all_regions, handle, protocol=pickle.HIGHEST_PROTOCOL)

with open('right_times_all_regions.pickle', 'wb') as handle:
    pickle.dump(right_times_all_regions, handle, protocol=pickle.HIGHEST_PROTOCOL)


# how many neurons do we consider per region based on this criteria?
num_neurons = [len(spike_times_all_regions[region]) for region in spike_times_all_regions.keys()] #194 neurons for 5Hz or 84 for 10Hz

# Extracting left and right trials



# Get data for GPe
d, spike_df_gpe = fn.extract_region(spike_df, 'GPe')

# Get trial aligned spikes for GPe
trial_spikes_gpe   = fn.trial_aligned_data(spike_df_gpe, stim_on)

mean_firing = fn.mean_firing(trial_spikes_gpe)
# get locations of neurons with mean_firing rate > 3Hz
idx = np.where(np.array(mean_firing) > 3)[0]
# extract firing times from gpe trials data
spike_times_gpe_2 = [trial_spikes_gpe[i] for i in idx]

plt.xlabel("time (s)")
plt.eventplot(spike_times_gpe_2[1])
# set y label
plt.ylabel("trial")
plt.title("Check GPe Neuron")
plt.axvline(0, color=[0.5, 0.5, 0.5])
plt.show()

len(spike_times_gpe_2)