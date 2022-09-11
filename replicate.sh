#!/bin/bash

Rscript Scripts/DataPreparation.R
echo "The relevant variables have been merged and cleaned."

Rscript Scripts/ExploratoryDataAnalysis.R
echo "Exploratory data analysis has been calculated."

Rscript Scripts/Analysis.R
echo "The analysis has completed."

rm Rplots.pdf