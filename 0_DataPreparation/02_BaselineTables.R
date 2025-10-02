# 02_BaselineTables.R
# This script uses the cleaned data and generates baseline tables for the 
# full dataset as well as the subsets used in the analyses

# 0. Load necessary packages and datasets --------------------------------------
source("./RequiredPackages.R")
fullData <- read.csv("./0_DataPreparation/CleanDataFiles/CleanData.csv",
                     row.names = 1)

varList <- read.csv("./0_DataPreparation/CleanDataFiles/VariableList.csv",
                    row.names = 1)
