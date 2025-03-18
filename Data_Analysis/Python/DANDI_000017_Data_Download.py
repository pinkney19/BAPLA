import matplotlib.pyplot as plt
import numpy as np
from pynwb import NWBHDF5IO
import math 
import networkx as nx
import pandas as pd
import pickle

# Open the NWB file of interest
filepath = "sub-Hench_ses-20170617T120000.nwb"
# Open the file in read mode "r",
io = NWBHDF5IO(filepath, mode="r")
nwbfile = io.read()
# Acessing single unit data
units = nwbfile.units
units_df = units.to_dataframe()

# Get region for each unit
regions = []
channels = []
spike_dict = {}
for i in range(len(units_df)):
    # Get peak channel for each unit
    peak_channel = units_df["peak_channel"].iloc[i]
    # Save peack channel in list
    channels.append(peak_channel)
    # Extract region for each unit 
    region = nwbfile.electrodes[peak_channel]["location"].values
    # Store in list
    regions.append(region)
    # Get spike times for each unit
    spike_times = units_df["spike_times"].iloc[i]
    # Add to dictionary
    spike_dict[i] = {'region': region, 'spike_times': spike_times}

    # spike times based on iloc rather

# Convert dictionary to dataframe
spike_df = pd.DataFrame.from_dict(spike_dict, orient='index')

# Save left contrast data
left_data = nwbfile.trials["visual_stimulus_left_contrast"]
left_data = np.array(left_data)
with open("left_data.pickle", "wb") as handle:
    pickle.dump(left_data, handle, protocol=pickle.HIGHEST_PROTOCOL)

# Save right contrast data
right_data = nwbfile.trials["visual_stimulus_right_contrast"]
right_data = np.array(right_data)
with open("right_data.pickle", "wb") as handle:
    pickle.dump(right_data, handle, protocol=pickle.HIGHEST_PROTOCOL)

# Save included data
included_data = nwbfile.trials["included"]
included_data = np.array(included_data)
with open("included_data.pickle", "wb") as handle:
    pickle.dump(included_data, handle, protocol=pickle.HIGHEST_PROTOCOL)

# Save spike_df with pickle

with open('spike_df.pickle', 'wb') as handle:
    pickle.dump(spike_df, handle, protocol=pickle.HIGHEST_PROTOCOL)

# Save stimulus onset times
stim_on = nwbfile.trials["visual_stimulus_time"]
# Convert to numpy array
stim_on = np.array(stim_on)
with open("stim_on.pickle", "wb") as handle:
    pickle.dump(stim_on, handle, protocol=pickle.HIGHEST_PROTOCOL)

# Save wheel movement onset times
response_time = nwbfile.trials["response_time"]
# Convert to numpy array
response_time = np.array(response_time)
with open("response_time.pickle", "wb") as handle:
    pickle.dump(response_time, handle, protocol=pickle.HIGHEST_PROTOCOL)

# # Acess probe types - bit hacky here
# probes = units_df["electrode_group"].values

# # Get position in dataframe which corresponds to each probe insertion - i.e. NOT unit IDs just row IDs
# idx_probe1 = np.where(units_df["electrode_group"].values==probes[345])[0]
# print(idx_probe1)

# # print(probes[345])
# # print(probes[1200])
# # print(probes[0])