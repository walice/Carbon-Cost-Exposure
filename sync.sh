#!/bin/bash

# Sync Data
rclone copy ~/Carbon-Cost-Exposure/Data/codebook.csv Dropbox:CanadaPanel/Analysis/Data/
rclone copy ~/Carbon-Cost-Exposure/Data/Raw/ Dropbox:CanadaPanel/Analysis/Data/Raw/
rclone copy ~/Carbon-Cost-Exposure/Data/Processed/ Dropbox:CanadaPanel/Analysis/Data/Processed/

# Sync Figures
rclone copy ~/Carbon-Cost-Exposure/Figures/ Dropbox:CanadaPanel/Analysis/Figures/

# Sync Results
rclone copy ~/Carbon-Cost-Exposure/Results/ Dropbox:CanadaPanel/Analysis/Results/