# 12_RunAnalyses.R
# Run analysis for each intervention component

# 0. Load necessary code, packages, and datasets -------------------------------
source("./RequiredPackages.R")
source("./1_AnalysisCode/11_AnalysisFunctions.R")

# Full dataset with no exclusions
data_full <- read.csv("./0_DataPreparation/CleanDataFiles/data_full.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))
# HIV negative dataset
data_hiv_negative <- read.csv("./0_DataPreparation/CleanDataFiles/data_hiv_negative.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))
# HIV negative males dataset
data_hiv_negative_males <- read.csv("./0_DataPreparation/CleanDataFiles/data_hiv_negative_males.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))
# HIV negative untreated dataset
data_hiv_negative_untreated <- read.csv("./0_DataPreparation/CleanDataFiles/data_hiv_negative_untreated.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))
# HIV positive dataset
data_hiv_positive <- read.csv("./0_DataPreparation/CleanDataFiles/data_hiv_positive.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))
# HIV positive untreated dataset
data_hiv_positive_untreated <- read.csv("./0_DataPreparation/CleanDataFiles/data_hiv_positive_untreated.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))

# 1. Impute Missing Indicators for Datasets ------------------------------------

# Info on all covariates
# tally(~gender, data = data_full) # Gender (categorical)
# favstats(~age, data = data_full) # Age (numeric)
# tally(~marital_status, data = data_full) # Marital Status (categorical)
# tally(~education, data = data_full) # Education Level (categorical)
# tally(~monthly_income, data = data_full) # Monthly Income (categorical)
# favstats(~alcohol_weekly, data = data_full) # Alcohol use weekly (numeric)
# favstats(~partners_12mos, data = data_full) # Partners in Last 12mos (numeric)
# 
# favstats(~prop_began_infected, data = data_full) # Proportion began HIV+ (numeric)
# favstats(~prop_male, data = data_full) # Proportion male (numeric)
# favstats(~prop_vlsupp, data = data_full) # Proportion HIV+ virall suppressed (numeric)

# Filling in all missing data with indicators (if categorical) or mean (if numeric)
data_full_complete <- data_full %>%
  mutate(
    # Marital Status
    marital_status = ifelse(is.na(marital_status), "Missing", marital_status),
    # Education Level
    education = ifelse(is.na(education), "Missing", education),
    # Monthly Income
    monthly_income = ifelse(is.na(monthly_income), "Missing", monthly_income),
    # Alcohol use weekly
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    # Number of partners in past 12 months
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )

data_hiv_negative_complete <- data_hiv_negative %>%
  mutate(
    # Marital Status
    marital_status = ifelse(is.na(marital_status), "Missing", marital_status),
    # Education Level
    education = ifelse(is.na(education), "Missing", education),
    # Monthly Income
    monthly_income = ifelse(is.na(monthly_income), "Missing", monthly_income),
    # Alcohol use weekly
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    # Number of partners in past 12 months
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )

data_hiv_negative_males_complete <- data_hiv_negative_males %>%
  mutate(
    # Marital Status
    marital_status = ifelse(is.na(marital_status), "Missing", marital_status),
    # Education Level
    education = ifelse(is.na(education), "Missing", education),
    # Monthly Income
    monthly_income = ifelse(is.na(monthly_income), "Missing", monthly_income),
    # Alcohol use weekly
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    # Number of partners in past 12 months
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )

data_hiv_negative_untreated_complete <- data_hiv_negative_untreated %>%
  mutate(
    # Marital Status
    marital_status = ifelse(is.na(marital_status), "Missing", marital_status),
    # Education Level
    education = ifelse(is.na(education), "Missing", education),
    # Monthly Income
    monthly_income = ifelse(is.na(monthly_income), "Missing", monthly_income),
    # Alcohol use weekly
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    # Number of partners in past 12 months
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )

data_hiv_positive_complete <- data_hiv_positive %>%
  mutate(
    # Marital Status
    marital_status = ifelse(is.na(marital_status), "Missing", marital_status),
    # Education Level
    education = ifelse(is.na(education), "Missing", education),
    # Monthly Income
    monthly_income = ifelse(is.na(monthly_income), "Missing", monthly_income),
    # Alcohol use weekly
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    # Number of partners in past 12 months
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )

data_hiv_positive_untreated_complete <- data_hiv_positive_untreated %>%
  mutate(
    # Marital Status
    marital_status = ifelse(is.na(marital_status), "Missing", marital_status),
    # Education Level
    education = ifelse(is.na(education), "Missing", education),
    # Monthly Income
    monthly_income = ifelse(is.na(monthly_income), "Missing", monthly_income),
    # Alcohol use weekly
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    # Number of partners in past 12 months
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )

# Define covariates that we want to consider for all analyses
# chosenCovars = c("gender", # 0 missing
#                  "age", # 0 missing
#                  "marital_status", # 2 missing in full dataset
#                  "education", # 45 missing in full dataset
#                  "monthly_income", # 13 missing in full dataset
#                  "alcohol_weekly", # 555 missing in full dataset
#                  "partners_12mos", # 995 missing in full dataset
# 
#                  "prop_began_infected", # No missing
#                  "prop_male", # No missing
#                  "prop_vlsupp" # No missing
#                  )
# 
# chosenInteractions = c("T_k*gender",
#                        "T_k*age",
#                        "T_k*monthly_income",
#                        "T_k*prop_began_infected"
#                        )

# 2. Run Overall Effects Analysis ----------------------------------------------




# 3. Run Analysis for Component 1: VMMC ----------------------------------------

## a. Individual Analysis ------------------------------------------------------
# Use all HIV- males (data_hiv_negative_males_complete)

# Unadjusted analysis (no covariates)
individual_vmmc_unadjusted <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_negative_males_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X1_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = NULL, # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(individual_vmmc_unadjusted, 
           "./3_Results/32_ResultsVMMC/vmmc_individual_unadjusted.xlsx")

# Adjusted analysis (with covariates)
individual_vmmc_adjusted <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_negative_males_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X1_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = c(#"gender", # 0 missing
                   "age", # 0 missing
                   "marital_status", # 2 missing in full dataset
                   "education", # 45 missing in full dataset
                   "monthly_income", # 13 missing in full dataset
                   "alcohol_weekly", # 555 missing in full dataset
                   "partners_12mos", # 995 missing in full dataset
                   
                   "prop_began_infected", # No missing
                   "prop_male", # No missing
                   "prop_vlsupp" # No missing
                   ), # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = 0.2
)
write_xlsx(individual_vmmc_adjusted, 
           "./3_Results/32_ResultsVMMC/vmmc_individual_adjusted.xlsx")

# Interaction adjusted analysis (with covariates and interactions)
individual_vmmc_interaction <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_negative_males_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X1_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = c(#"gender", # 0 missing
                   "age", # 0 missing
                   "marital_status", # 2 missing in full dataset
                   "education", # 45 missing in full dataset
                   "monthly_income", # 13 missing in full dataset
                   "alcohol_weekly", # 555 missing in full dataset
                   "partners_12mos", # 995 missing in full dataset
                   
                   "prop_began_infected", # No missing
                   "prop_male", # No missing
                   "prop_vlsupp" # No missing
                   ), # Vector of covariate names in dataset
  myInteractions = c(#"T_k*gender",
                     "T_k*age",
                     "T_k*monthly_income",
                     "T_k*prop_began_infected"),
  myForcedTerms = dplyr::filter(individual_vmmc_adjusted$`Covariate List`,
                                !is.na(`Final Covariates`), 
                                `Final Covariates` != "T_k")$`Final Covariates`,
  myCutoff = 0.2
)
write_xlsx(individual_vmmc_interaction,
           "./3_Results/32_ResultsVMMC/vmmc_individual_interaction.xlsx")

## b. Spillover Analysis -------------------------------------------------------
# Use all HIV- untreated (data_hiv_negative_untreated_complete)

# Unadjusted analysis (no covariates)
spillover_vmmc_unadjusted <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z1_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = NULL, # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(spillover_vmmc_unadjusted, 
           "./3_Results/32_ResultsVMMC/vmmc_spillover_unadjusted.xlsx")

# Adjusted analysis (with covariates)
spillover_vmmc_adjusted <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z1_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = c("gender", # 0 missing
                   "age", # 0 missing
                   "marital_status", # 2 missing in full dataset
                   "education", # 45 missing in full dataset
                   "monthly_income", # 13 missing in full dataset
                   "alcohol_weekly", # 555 missing in full dataset
                   "partners_12mos", # 995 missing in full dataset
                   
                   "prop_began_infected", # No missing
                   "prop_male", # No missing
                   "prop_vlsupp" # No missing
                   ), # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = 0.2
)
write_xlsx(spillover_vmmc_adjusted, 
           "./3_Results/32_ResultsVMMC/vmmc_spillover_adjusted.xlsx")

# Interaction adjusted analysis (with covariates)
spillover_vmmc_interaction <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z1_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = c("gender", # 0 missing
                   "age", # 0 missing
                   "marital_status", # 2 missing in full dataset
                   "education", # 45 missing in full dataset
                   "monthly_income", # 13 missing in full dataset
                   "alcohol_weekly", # 555 missing in full dataset
                   "partners_12mos", # 995 missing in full dataset
                   
                   "prop_began_infected", # No missing
                   "prop_male", # No missing
                   "prop_vlsupp" # No missing
                   ), # Vector of covariate names in dataset
  myInteractions = c("T_k*gender",
                     "T_k*age",
                     "T_k*monthly_income",
                     "T_k*prop_began_infected"),
  myForcedTerms = dplyr::filter(spillover_vmmc_adjusted$`Covariate List`,
                                !is.na(`Final Covariates`), 
                                `Final Covariates` != "T_k")$`Final Covariates`,
  myCutoff = 0.2
)
write_xlsx(spillover_vmmc_interaction,
           "./3_Results/32_ResultsVMMC/vmmc_spillover_interaction.xlsx")

# 4. Run Analysis for Component 2: HTC -----------------------------------------

## a. Individual Analysis ------------------------------------------------------
# Use all HIV- individuals (data_hiv_negative_complete)

# Unadjusted analysis (no covariates)
individual_htc_unadjusted <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_negative_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X2_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = NULL, # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(individual_htc_unadjusted, 
           "./3_Results/33_ResultsHTC/htc_individual_unadjusted.xlsx")

# Adjusted analysis (with covariates)
individual_htc_adjusted <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_negative_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X2_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = c("gender", # 0 missing
                   "age", # 0 missing
                   "marital_status", # 2 missing in full dataset
                   "education", # 45 missing in full dataset
                   "monthly_income", # 13 missing in full dataset
                   "alcohol_weekly", # 555 missing in full dataset
                   "partners_12mos", # 995 missing in full dataset
                   
                   "prop_began_infected", # No missing
                   "prop_male", # No missing
                   "prop_vlsupp" # No missing
  ), # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = 0.2
)
write_xlsx(individual_htc_adjusted, 
           "./3_Results/33_ResultsHTC/htc_individual_adjusted.xlsx")

# Interaction adjusted analysis (with covariates)
individual_htc_interaction <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_negative_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X2_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = c("gender", # 0 missing
                   "age", # 0 missing
                   "marital_status", # 2 missing in full dataset
                   "education", # 45 missing in full dataset
                   "monthly_income", # 13 missing in full dataset
                   "alcohol_weekly", # 555 missing in full dataset
                   "partners_12mos", # 995 missing in full dataset
                   
                   "prop_began_infected", # No missing
                   "prop_male", # No missing
                   "prop_vlsupp" # No missing
  ), # Vector of covariate names in dataset
  myInteractions = c("T_k*gender",
                     "T_k*age",
                     "T_k*monthly_income",
                     "T_k*prop_began_infected"),
  myForcedTerms = dplyr::filter(individual_htc_adjusted$`Covariate List`,
                                !is.na(`Final Covariates`), 
                                `Final Covariates` != "T_k")$`Final Covariates`,
  myCutoff = 0.2
)
write_xlsx(individual_htc_interaction, 
           "./3_Results/33_ResultsHTC/htc_individual_interaction.xlsx")

## b. Spillover Analysis -------------------------------------------------------
# Use all HIV- untreated (data_hiv_negative_untreated_complete)

# Unadjusted analysis (no covariates)
spillover_htc_unadjusted <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z2_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = NULL, # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(spillover_htc_unadjusted, 
           "./3_Results/33_ResultsHTC/htc_spillover_unadjusted.xlsx")

# Adjusted analysis (with covariates)
spillover_htc_adjusted <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z2_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = c("gender", # 0 missing
                   "age", # 0 missing
                   "marital_status", # 2 missing in full dataset
                   "education", # 45 missing in full dataset
                   "monthly_income", # 13 missing in full dataset
                   "alcohol_weekly", # 555 missing in full dataset
                   "partners_12mos", # 995 missing in full dataset
                   
                   "prop_began_infected", # No missing
                   "prop_male", # No missing
                   "prop_vlsupp" # No missing
  ), # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = 0.2
)
write_xlsx(spillover_htc_adjusted, 
           "./3_Results/33_ResultsHTC/htc_spillover_adjusted.xlsx")

# Interaction adjusted analysis (with covariates)
spillover_htc_interaction <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z2_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  myCovariates = c("gender", # 0 missing
                   "age", # 0 missing
                   "marital_status", # 2 missing in full dataset
                   "education", # 45 missing in full dataset
                   "monthly_income", # 13 missing in full dataset
                   "alcohol_weekly", # 555 missing in full dataset
                   "partners_12mos", # 995 missing in full dataset
                   
                   "prop_began_infected", # No missing
                   "prop_male", # No missing
                   "prop_vlsupp" # No missing
  ), # Vector of covariate names in dataset
  myInteractions = c("T_k*gender",
                     "T_k*age",
                     "T_k*monthly_income",
                     "T_k*prop_began_infected"),
  myForcedTerms = dplyr::filter(spillover_htc_adjusted$`Covariate List`,
                                !is.na(`Final Covariates`), 
                                `Final Covariates` != "T_k")$`Final Covariates`,
  myCutoff = 0.2
)
write_xlsx(spillover_htc_interaction, 
           "./3_Results/33_ResultsHTC/htc_spillover_interaction.xlsx")

# 5. Run Analysis for Component 3: ART -----------------------------------------

## a. Individual Analysis ------------------------------------------------------
# Use all HIV+ (data_hiv_positive_complete)

# Unadjusted analysis (no covariates)
individual_art_unadjusted <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_positive_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X3_ik", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  myCovariates = NULL, # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(individual_art_unadjusted, 
           "./3_Results/34_ResultsART/art_individual_unadjusted.xlsx")

# Adjusted analysis (with covariates)
individual_art_adjusted <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_positive_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X3_ik", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  myCovariates = c("gender", # 0 missing
                   "age", # 0 missing
                   "marital_status", # 2 missing in full dataset
                   "education", # 45 missing in full dataset
                   "monthly_income", # 13 missing in full dataset
                   "alcohol_weekly", # 555 missing in full dataset
                   "partners_12mos", # 995 missing in full dataset
                   
                   "prop_began_infected", # No missing
                   "prop_male", # No missing
                   "prop_vlsupp" # No missing
  ), # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = 0.2
)
write_xlsx(individual_art_adjusted, 
           "./3_Results/34_ResultsART/art_individual_adjusted.xlsx")


# Interaction adjusted analysis (with covariates)
individual_art_interaction <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_positive_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X3_ik", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  myCovariates = c("gender", # 0 missing
                   "age", # 0 missing
                   "marital_status", # 2 missing in full dataset
                   "education", # 45 missing in full dataset
                   "monthly_income", # 13 missing in full dataset
                   "alcohol_weekly", # 555 missing in full dataset
                   "partners_12mos", # 995 missing in full dataset
                   
                   "prop_began_infected", # No missing
                   "prop_male", # No missing
                   "prop_vlsupp" # No missing
  ), # Vector of covariate names in dataset
  myInteractions = c("T_k*gender",
                     "T_k*age",
                     "T_k*monthly_income",
                     "T_k*prop_began_infected"),
  myForcedTerms = dplyr::filter(individual_art_adjusted$`Covariate List`,
                                !is.na(`Final Covariates`), 
                                `Final Covariates` != "T_k")$`Final Covariates`,
  myCutoff = 0.2
)
write_xlsx(individual_art_interaction, 
           "./3_Results/34_ResultsART/art_individual_interaction.xlsx")

## b. Spillover Analysis -------------------------------------------------------
# Use all HIV+ untreated (data_hiv_positive_untreated_complete)

# Unadjusted analysis (no covariates)
spillover_art_unadjusted <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_positive_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z3_k", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  myCovariates = NULL, # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(spillover_art_unadjusted, 
           "./3_Results/34_ResultsART/art_spillover_unadjusted.xlsx")

# tally(~Y2_ik, data = data_hiv_positive_untreated_complete)
# favstats(~Z3_k, data = data_hiv_positive_untreated_complete)
# favstats(Z3_k~Y2_ik, data = data_hiv_positive_untreated_complete)

# Adjusted analysis (with covariates)
spillover_art_adjusted <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_positive_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z3_k", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  myCovariates = c("gender", # 0 missing
                   "age", # 0 missing
                   "marital_status", # 2 missing in full dataset
                   "education", # 45 missing in full dataset
                   "monthly_income", # 13 missing in full dataset
                   "alcohol_weekly", # 555 missing in full dataset
                   "partners_12mos", # 995 missing in full dataset
                   
                   "prop_began_infected", # No missing
                   "prop_male", # No missing
                   "prop_vlsupp" # No missing
  ), # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = 0.2
)
write_xlsx(spillover_art_adjusted, 
           "./3_Results/34_ResultsART/art_spillover_adjusted.xlsx")


# Interaction adjusted analysis (with covariates)
spillover_art_interaction <- runMediationAnalysis(
  roundNumber = 2, # Number of significant digits desired
  myData = data_hiv_positive_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z3_k", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  myCovariates = c("gender", # 0 missing
                   "age", # 0 missing
                   "marital_status", # 2 missing in full dataset
                   "education", # 45 missing in full dataset
                   "monthly_income", # 13 missing in full dataset
                   "alcohol_weekly", # 555 missing in full dataset
                   "partners_12mos", # 995 missing in full dataset
                   
                   "prop_began_infected", # No missing
                   "prop_male", # No missing
                   "prop_vlsupp" # No missing
  ), # Vector of covariate names in dataset
  myInteractions = c("T_k*gender",
                     "T_k*age",
                     "T_k*monthly_income",
                     "T_k*prop_began_infected"),
  myForcedTerms = dplyr::filter(spillover_art_adjusted$`Covariate List`,
                                !is.na(`Final Covariates`), 
                                `Final Covariates` != "T_k")$`Final Covariates`,
  myCutoff = 0.2
)
write_xlsx(spillover_art_interaction, 
           "./3_Results/34_ResultsART/art_spillover_interaction.xlsx")
