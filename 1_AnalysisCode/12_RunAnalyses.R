# 12_RunAnalyses.R
# Run analysis for each intervention component

# 0. Load necessary code, packages, and datasets -------------------------------
source("./RequiredPackages.R")
source("./1_AnalysisCode/11_AnalysisFunctions.R")

# Full dataset with no exclusions
data_full <- read.csv("./0_DataPreparation/CleanDataFiles/data_full.csv", row.names = 1)
# HIV negative dataset
data_hiv_negative <- read.csv("./0_DataPreparation/CleanDataFiles/data_hiv_negative.csv", row.names = 1)
# HIV negative males dataset
data_hiv_negative_males <- read.csv("./0_DataPreparation/CleanDataFiles/data_hiv_negative_males.csv", row.names = 1)
# HIV negative untreated dataset
data_hiv_negative_untreated <- read.csv("./0_DataPreparation/CleanDataFiles/data_hiv_negative_untreated.csv", row.names = 1)
# HIV positive dataset
data_hiv_positive <- read.csv("./0_DataPreparation/CleanDataFiles/data_hiv_positive.csv", row.names = 1)
# HIV positive dataset
data_hiv_positive_untreated <- read.csv("./0_DataPreparation/CleanDataFiles/data_hiv_positive_untreated.csv", row.names = 1)

# 1. Impute Missing Indicators for Datasets ------------------------------------


# Define covariates that we want to consider for all analyses
chosenCovars = c("age", # No missing
                 "marital_status", # 2 missing in full dataset
                 "employment_status", # 11 missing in full dataset
                 "education", # 45 missing in full dataset
                 "monthly_income", # 13 missing in full dataset
                 "partners_12mos", # 995 missing in full dataset
                 "exchange_12mos", # 2149 missing in full dataset
                 "condom_lastsex", # 897 missing in full dataset
                 "prop_began_infected", # No missing
                 "prop_male", # No missing
                 "prop_vlsupp" # No missing
                 )

chosenCovars = c("age", # No missing
                 "partners_12mos", # 995 missing in full dataset
                 "prop_began_infected", # No missing
                 "prop_male", # No missing
                 "prop_vlsupp" # No missing
                 )

# Info on all covariates
# favstats(~age, data = data_full) # Age (numeric)
# tally(~marital_status, data = data_full) # Marital Status (categorical)
# tally(~employment_status, data = data_full) # Employment Status (categorical)
# tally(~education, data = data_full) # Education Level (categorical)
# tally(~monthly_income, data = data_full) # Monthly Income (categorical)
# favstats(~partners_12mos, data = data_full) # Partners in Last 12mos (numeric)
# tally(~exchange_12mos, data = data_full) # Exchange Sex (categorical)
# tally(~condom_lastsex, data = data_full) # Condom in last interaction (categorical)
# favstats(~prop_began_infected, data = data_full) # Proportion began HIV+ (numeric)
# favstats(~prop_male, data = data_full) # Proportion male (numeric)
# favstats(~prop_vlsupp, data = data_full) # Proportion HIV+ virall suppressed (numeric)

# Filling in all missing data with indicators (if categorical) or mean (if numeric)
data_full_complete <- data_full %>%
  mutate(marital_status = ifelse(is.na(marital_status), 
                                 "Missing", marital_status),
         employment_status = ifelse(is.na(employment_status), 
                                    "Missing", employment_status),
         education = ifelse(is.na(education),
                            "Missing", education),
         monthly_income = ifelse(is.na(monthly_income), 
                                 "Missing", monthly_income),
         partners_12mos = ifelse(is.na(partners_12mos),
                                 mean(partners_12mos, na.rm = TRUE), partners_12mos),
         exchange_12mos = ifelse(is.na(exchange_12mos),
                                 "Missing", exchange_12mos),
         condom_lastsex = ifelse(is.na(condom_lastsex),
                                 "Missing", condom_lastsex)
  )

data_hiv_negative_complete <- data_hiv_negative %>%
  mutate(marital_status = ifelse(is.na(marital_status), 
                                 "Missing", marital_status),
         employment_status = ifelse(is.na(employment_status), 
                                    "Missing", employment_status),
         education = ifelse(is.na(education),
                            "Missing", education),
         monthly_income = ifelse(is.na(monthly_income), 
                                 "Missing", monthly_income),
         partners_12mos = ifelse(is.na(partners_12mos),
                                 mean(partners_12mos, na.rm = TRUE), partners_12mos),
         exchange_12mos = ifelse(is.na(exchange_12mos),
                                 "Missing", exchange_12mos),
         condom_lastsex = ifelse(is.na(condom_lastsex),
                                 "Missing", condom_lastsex)
  )

data_hiv_negative_males_complete <- data_hiv_negative_males %>%
  mutate(marital_status = ifelse(is.na(marital_status), 
                                 "Missing", marital_status),
         employment_status = ifelse(is.na(employment_status), 
                                    "Missing", employment_status),
         education = ifelse(is.na(education),
                            "Missing", education),
         monthly_income = ifelse(is.na(monthly_income), 
                                 "Missing", monthly_income),
         partners_12mos = ifelse(is.na(partners_12mos),
                                 mean(partners_12mos, na.rm = TRUE), partners_12mos),
         exchange_12mos = ifelse(is.na(exchange_12mos),
                                 "Missing", exchange_12mos),
         condom_lastsex = ifelse(is.na(condom_lastsex),
                                 "Missing", condom_lastsex)
  )

data_hiv_negative_untreated_complete <- data_hiv_negative_untreated %>%
  mutate(marital_status = ifelse(is.na(marital_status), 
                                 "Missing", marital_status),
         employment_status = ifelse(is.na(employment_status), 
                                    "Missing", employment_status),
         education = ifelse(is.na(education),
                            "Missing", education),
         monthly_income = ifelse(is.na(monthly_income), 
                                 "Missing", monthly_income),
         partners_12mos = ifelse(is.na(partners_12mos),
                                 mean(partners_12mos, na.rm = TRUE), partners_12mos),
         exchange_12mos = ifelse(is.na(exchange_12mos),
                                 "Missing", exchange_12mos),
         condom_lastsex = ifelse(is.na(condom_lastsex),
                                 "Missing", condom_lastsex)
  )

data_hiv_positive_complete <- data_hiv_positive %>%
  mutate(marital_status = ifelse(is.na(marital_status), 
                                 "Missing", marital_status),
         employment_status = ifelse(is.na(employment_status), 
                                    "Missing", employment_status),
         education = ifelse(is.na(education),
                            "Missing", education),
         monthly_income = ifelse(is.na(monthly_income), 
                                 "Missing", monthly_income),
         partners_12mos = ifelse(is.na(partners_12mos),
                                 mean(partners_12mos, na.rm = TRUE), partners_12mos),
         exchange_12mos = ifelse(is.na(exchange_12mos),
                                 "Missing", exchange_12mos),
         condom_lastsex = ifelse(is.na(condom_lastsex),
                                 "Missing", condom_lastsex)
  )

data_hiv_positive_untreated_complete <- data_hiv_positive_untreated %>%
  mutate(marital_status = ifelse(is.na(marital_status), 
                                 "Missing", marital_status),
         employment_status = ifelse(is.na(employment_status), 
                                    "Missing", employment_status),
         education = ifelse(is.na(education),
                            "Missing", education),
         monthly_income = ifelse(is.na(monthly_income), 
                                 "Missing", monthly_income),
         partners_12mos = ifelse(is.na(partners_12mos),
                                 mean(partners_12mos, na.rm = TRUE), partners_12mos),
         exchange_12mos = ifelse(is.na(exchange_12mos),
                                 "Missing", exchange_12mos),
         condom_lastsex = ifelse(is.na(condom_lastsex),
                                 "Missing", condom_lastsex)
  )

# 2. Run Analysis for Component 1: VMMC ----------------------------------------

## a. Overall Analysis ---------------------------------------------------------
# Use all HIV- individuals (data_hiv_negative_complete)


## b. Individual Analysis ------------------------------------------------------
# Use all HIV- males (data_hiv_negative_males_complete)

# Unadjusted analysis (no covariates)
individual_vmmc_unadjusted <- runMediationAnalysis(
  roundNumber = 3, # Number of significant digits desired
  myData = data_hiv_negative_males_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X1_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = NULL # Vector of covariate names in dataset
  )
write_xlsx(individual_vmmc_unadjusted, 
           "./3_Results/32_ResultsVMMC/vmmc_individual_unadjusted.xlsx")

# Adjusted analysis (with covariates)
individual_vmmc_adjusted <- runMediationAnalysis(
  roundNumber = 3, # Number of significant digits desired
  myData = data_hiv_negative_males_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X1_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = chosenCovars # Vector of covariate names in dataset
  )
write_xlsx(individual_vmmc_adjusted, 
           "./3_Results/32_ResultsVMMC/vmmc_individual_adjusted.xlsx")

## c. Spillover Analysis -------------------------------------------------------
# Use all HIV- untreated (data_hiv_negative_untreated_complete)

# Unadjusted analysis (no covariates)
spillover_vmmc_unadjusted <- runMediationAnalysis(
  roundNumber = 3, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z1_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = NULL # Vector of covariate names in dataset
)
write_xlsx(spillover_vmmc_unadjusted, 
           "./3_Results/32_ResultsVMMC/vmmc_spillover_unadjusted.xlsx")

# Adjusted analysis (with covariates)
spillover_vmmc_adjusted <- runMediationAnalysis(
  roundNumber = 3, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z1_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = chosenCovars # Vector of covariate names in dataset
)
write_xlsx(spillover_vmmc_adjusted, 
           "./3_Results/32_ResultsVMMC/vmmc_spillover_adjusted.xlsx")

# 3. Run Analysis for Component 2: HTC -----------------------------------------

## a. Overall Analysis ---------------------------------------------------------
# Use all HIV- individuals (data_hiv_negative_complete)


## b. Individual Analysis ------------------------------------------------------
# Use all HIV- individuals (data_hiv_negative_complete)

# Unadjusted analysis (no covariates)
individual_htc_unadjusted <- runMediationAnalysis(
  roundNumber = 3, # Number of significant digits desired
  myData = data_hiv_negative_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X2_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = NULL # Vector of covariate names in dataset
)
write_xlsx(individual_htc_unadjusted, 
           "./3_Results/33_ResultsHTC/htc_individual_unadjusted.xlsx")

# Adjusted analysis (with covariates)
individual_htc_adjusted <- runMediationAnalysis(
  roundNumber = 3, # Number of significant digits desired
  myData = data_hiv_negative_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X2_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = chosenCovars # Vector of covariate names in dataset
)
write_xlsx(individual_htc_adjusted, 
           "./3_Results/33_ResultsHTC/htc_individual_adjusted.xlsx")

## c. Spillover Analysis -------------------------------------------------------
# Use all HIV- untreated (data_hiv_negative_untreated_complete)

# Unadjusted analysis (no covariates)
spillover_htc_unadjusted <- runMediationAnalysis(
  roundNumber = 3, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z2_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = NULL # Vector of covariate names in dataset
)
write_xlsx(spillover_htc_unadjusted, 
           "./3_Results/33_ResultsHTC/htc_spillover_unadjusted.xlsx")

# Adjusted analysis (with covariates)
spillover_htc_adjusted <- runMediationAnalysis(
  roundNumber = 3, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z2_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = chosenCovars # Vector of covariate names in dataset
)
write_xlsx(spillover_htc_adjusted, 
           "./3_Results/33_ResultsHTC/htc_spillover_adjusted.xlsx")

# 4. Run Analysis for Component 3: ART -----------------------------------------

## a. Overall Analysis ---------------------------------------------------------
# Use all HIV+ (data_hiv_positive_complete)

## b. Individual Analysis ------------------------------------------------------
# Use all HIV+ (data_hiv_positive_complete)

# Unadjusted analysis (no covariates)
individual_art_unadjusted <- runMediationAnalysis(
  roundNumber = 3, # Number of significant digits desired
  myData = data_hiv_positive_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X3_ik", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  myCovariates = NULL # Vector of covariate names in dataset
)
write_xlsx(individual_art_unadjusted, 
           "./3_Results/34_ResultsART/art_individual_unadjusted.xlsx")

# Adjusted analysis (with covariates)
individual_art_adjusted <- runMediationAnalysis(
  roundNumber = 3, # Number of significant digits desired
  myData = data_hiv_positive_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X3_ik", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  myCovariates = chosenCovars # Vector of covariate names in dataset
)
write_xlsx(individual_art_adjusted, 
           "./3_Results/34_ResultsART/art_individual_adjusted.xlsx")

## c. Spillover Analysis -------------------------------------------------------
# Use all HIV+ untreated (data_hiv_positive_untreated_complete)

# Unadjusted analysis (no covariates)
spillover_art_unadjusted <- runMediationAnalysis(
  roundNumber = 3, # Number of significant digits desired
  myData = data_hiv_positive_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z3_k", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  myCovariates = NULL # Vector of covariate names in dataset
)
write_xlsx(spillover_art_unadjusted, 
           "./3_Results/34_ResultsART/art_spillover_unadjusted.xlsx")

# Adjusted analysis (with covariates)
spillover_art_adjusted <- runMediationAnalysis(
  roundNumber = 3, # Number of significant digits desired
  myData = data_hiv_positive_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z3_k", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  myCovariates = chosenCovars # Vector of covariate names in dataset
)
write_xlsx(individual_art_adjusted, 
           "./3_Results/34_ResultsART/art_spillover_adjusted.xlsx")
