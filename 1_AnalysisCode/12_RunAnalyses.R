# 12_RunAnalyses.R
# Run analysis for each intervention component

# 0A. Load necessary code, packages, and datasets -------------------------------
source("./RequiredPackages.R")
source("./1_AnalysisCode/11_AnalysisFunctions.R")
source("./2_HelperFunctions/identifyMediatorConfounding.R")
source("./2_HelperFunctions/identifyConfounding.R")

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

# 0B. Impute Missing Indicators for Datasets ------------------------------------

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

# Get all the modes of the variables
modes <- data_full %>%
  dplyr::select(marital_status, education, monthly_income) %>%
  dplyr::summarise(across(
    everything(),
    ~ names(sort(table(.[!is.na(.)]), decreasing = TRUE))[1]
  ))
  

# Filling in all missing data with indicators (if categorical) or mean (if numeric)
data_full_complete <- data_full %>%
  mutate(
    # Marital Status
    marital_status = ifelse(is.na(marital_status), paste0(modes$marital_status), marital_status),
    # Education Level
    education = ifelse(is.na(education), paste0(modes$education), education),
    # Monthly Income
    monthly_income = ifelse(is.na(monthly_income), paste0(modes$monthly_income), monthly_income),
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
    marital_status = ifelse(is.na(marital_status), paste0(modes$marital_status), marital_status),
    # Education Level
    education = ifelse(is.na(education), paste0(modes$education), education),
    # Monthly Income
    monthly_income = ifelse(is.na(monthly_income), paste0(modes$monthly_income), monthly_income),
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
    marital_status = ifelse(is.na(marital_status), paste0(modes$marital_status), marital_status),
    # Education Level
    education = ifelse(is.na(education), paste0(modes$education), education),
    # Monthly Income
    monthly_income = ifelse(is.na(monthly_income), paste0(modes$monthly_income), monthly_income),
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
    marital_status = ifelse(is.na(marital_status), paste0(modes$marital_status), marital_status),
    # Education Level
    education = ifelse(is.na(education), paste0(modes$education), education),
    # Monthly Income
    monthly_income = ifelse(is.na(monthly_income), paste0(modes$monthly_income), monthly_income),
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
    marital_status = ifelse(is.na(marital_status), paste0(modes$marital_status), marital_status),
    # Education Level
    education = ifelse(is.na(education), paste0(modes$education), education),
    # Monthly Income
    monthly_income = ifelse(is.na(monthly_income), paste0(modes$monthly_income), monthly_income),
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
    marital_status = ifelse(is.na(marital_status), paste0(modes$marital_status), marital_status),
    # Education Level
    education = ifelse(is.na(education), paste0(modes$education), education),
    # Monthly Income
    monthly_income = ifelse(is.na(monthly_income), paste0(modes$monthly_income), monthly_income),
    # Alcohol use weekly
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    # Number of partners in past 12 months
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )

# 0C. Identify confounders -----------------------------------------------------

# Define covariates that we want to consider for all analyses
allCovars = c("gender", # 0 missing
              "age", # 0 missing
              "marital_status", # 2 missing in full dataset
              "education", # 45 missing in full dataset
              "monthly_income", # 13 missing in full dataset
              "alcohol_weekly", # 555 missing in full dataset
              "partners_12mos", # 995 missing in full dataset
              
              "prop_began_infected", # No missing
              "prop_male", # No missing
              "prop_vlsupp" # No missing
              )

allInteractions = c("T_k*gender",
                    "T_k*age",
                    "T_k*monthly_income",
                    "T_k*education",
                    "T_k*prop_began_infected"
                    )

allInteractionTerms <- c("gender",
                         "age",
                         "monthly_income",
                         "education",
                         "prop_began_infected"
                         )

currentRoundNumber <- 2


# Identify general confounders for HIV
hiv_confounder_output <- identifyConfounding(
  myData = data_hiv_negative_complete,
  myCluster = "cluster_id",
  myOutcome = "Y1_ik",
  myTreatment = "T_k",
  myCovariates = allCovars,
  pthresh = 0.2,
  roundNumber = currentRoundNumber)
hiv_confounders <- hiv_confounder_output$finalCovariates$ID
write_xlsx(hiv_confounder_output, "./3_Results/37_Confounders/1_hiv_confounders.xlsx")


# Identify general confounders for Mortality
mortality_confounder_output <- identifyConfounding(
  myData = data_hiv_positive_complete,
  myCluster = "cluster_id",
  myOutcome = "Y2_ik",
  myTreatment = "T_k",
  myCovariates = allCovars,
  pthresh = 0.2,
  roundNumber = currentRoundNumber)
mortality_confounders <- mortality_confounder_output$finalCovariates$ID
write_xlsx(mortality_confounder_output, "./3_Results/37_Confounders/2_mortality_confounders.xlsx")


# Identify VMMC component confounders
# Use population of individuals who can even get the mediator
# (i.e. HIV negative males)
vmmc_confounder_output <- identifyMediatorConfounding(
  myData = data_hiv_negative_males_complete,
  myCluster = "cluster_id",
  myComponent = "X1_ik",
  myTreatment = "T_k",
  myCovariates = setdiff(allCovars, "gender"),
  pthresh = 0.2,
  roundNumber = currentRoundNumber)
vmmc_confounders <- vmmc_confounder_output$finalCovariates$ID
write_xlsx(vmmc_confounder_output, "./3_Results/37_Confounders/3_vmmc_confounders.xlsx")

# Identify HTC component confounders
# Use population of individuals who can even get the mediator
# (i.e. HIV negative individuals)
htc_confounder_output <- identifyMediatorConfounding(
  myData = data_hiv_negative_complete,
  myCluster = "cluster_id",
  myComponent = "X2_ik",
  myTreatment = "T_k",
  myCovariates = allCovars,
  pthresh = 0.2,
  roundNumber = currentRoundNumber)
htc_confounders <- htc_confounder_output$finalCovariates$ID
write_xlsx(htc_confounder_output, "./3_Results/37_Confounders/4_htc_confounders.xlsx")

# Identify ART component confounders
# Use population of individuals who can even get the mediator
# (i.e. HIV positive individuals)
art_confounder_output <- identifyMediatorConfounding(
  myData = data_hiv_positive_complete,
  myCluster = "cluster_id",
  myComponent = "X3_ik",
  myTreatment = "T_k",
  myCovariates = allCovars,
  pthresh = 0.2,
  roundNumber = currentRoundNumber)
art_confounders <- art_confounder_output$finalCovariates$ID
write_xlsx(art_confounder_output, "./3_Results/37_Confounders/5_art_confounders.xlsx")

# IA. Run Overall Effects on HIV Analysis --------------------------------------
# Use all HIV- males (data_hiv_negative_complete)

# Unadjusted analysis (no covariates)
overall_hiv_unadjusted <- runOverallAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_negative_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Xany_ik", # Component name in dataset
  myComponentProp = "Zany_k_hivneg", # Component proportion in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  modelSelect = "No",
  myCovariates = NULL, # Vector of covariate names in dataset
  myInteractions = NULL, # Interactions desired
  myForcedTerms = NULL, # Terms that must remain in the model
  myCutoff = NULL, # P-value cutoff for model selection
  mySelectionCriteria = NULL
  )
write_xlsx(overall_hiv_unadjusted, 
           "./3_Results/35_ResultsHIVOverall/hiv_overall_unadjusted.xlsx")

# Adjusted analysis (with covariates)
overall_hiv_adjusted <- runOverallAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_negative_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Xany_ik", # Component name in dataset
  myComponentProp = "Zany_k_hivneg", # Component proportion in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  modelSelect = "No",
  myCovariates = c(hiv_confounders), # Vector of covariate names in dataset
  myInteractions = NULL, # Interactions desired
  myForcedTerms = NULL, # Terms that must remain in the model
  myCutoff = NULL, # P-value cutoff for model selection
  mySelectionCriteria = NULL
  )
write_xlsx(overall_hiv_adjusted, 
           "./3_Results/35_ResultsHIVOverall/hiv_overall_adjusted.xlsx")

# Adjusted analysis (with covariates)
overall_hiv_interaction <- runOverallAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_negative_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Xany_ik", # Component name in dataset
  myComponentProp = "Zany_k_hivneg", # Component proportion in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  modelSelect = "Yes",
  myCovariates = unique(c(hiv_confounders, 
                          allInteractionTerms)), # Vector of covariate names in dataset
  myInteractions = allInteractions,
  myForcedTerms = c(hiv_confounders),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
  )
write_xlsx(overall_hiv_interaction, 
           "./3_Results/35_ResultsHIVOverall/hiv_overall_interaction.xlsx")


# IB. Run Analysis for Component 1: VMMC ----------------------------------------


## a. Individual Analysis ------------------------------------------------------
# Use all HIV- males (data_hiv_negative_males_complete)

# Unadjusted analysis (no covariates)
individual_vmmc_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_negative_males_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X1_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  modelSelect = "No",
  myCovariates = NULL, # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(individual_vmmc_unadjusted, 
           "./3_Results/32_ResultsVMMC/vmmc_individual_unadjusted.xlsx")

# Adjusted analysis (with covariates)
individual_vmmc_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_negative_males_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X1_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  modelSelect = "No",
  myCovariates = setdiff(unique(c(hiv_confounders, vmmc_confounders)), 
                         "gender"), # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(individual_vmmc_adjusted, 
           "./3_Results/32_ResultsVMMC/vmmc_individual_adjusted.xlsx")

# Interaction adjusted analysis (with covariates and interactions)
individual_vmmc_interaction <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_negative_males_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X1_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  modelSelect = "Yes",
  myCovariates = setdiff(unique(c(hiv_confounders, vmmc_confounders, allInteractionTerms)), 
                         "gender"), # Vector of covariate names in dataset
  myInteractions = setdiff(allInteractions, "T_k*gender"),
  myForcedTerms = setdiff(unique(c(hiv_confounders, vmmc_confounders)), 
                          "gender"),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(individual_vmmc_interaction,
           "./3_Results/32_ResultsVMMC/vmmc_individual_interaction.xlsx")

## b. Spillover Analysis -------------------------------------------------------
# Use all HIV- untreated (data_hiv_negative_untreated_complete)

# Unadjusted analysis (no covariates)
spillover_vmmc_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z1_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  modelSelect = "No",
  myCovariates = NULL, # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(spillover_vmmc_unadjusted, 
           "./3_Results/32_ResultsVMMC/vmmc_spillover_unadjusted.xlsx")

# Adjusted analysis (with covariates)
spillover_vmmc_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z1_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  modelSelect = "No",
  myCovariates = unique(c(hiv_confounders, vmmc_confounders)), # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(spillover_vmmc_adjusted, 
           "./3_Results/32_ResultsVMMC/vmmc_spillover_adjusted.xlsx")

# Interaction adjusted analysis (with covariates)
spillover_vmmc_interaction <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z1_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  modelSelect = "Yes",
  myCovariates = unique(c(hiv_confounders, vmmc_confounders, 
                          allInteractionTerms)), # Vector of covariate names in dataset
  myInteractions = allInteractions,
  myForcedTerms = unique(c(hiv_confounders, vmmc_confounders)),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(spillover_vmmc_interaction,
           "./3_Results/32_ResultsVMMC/vmmc_spillover_interaction.xlsx")

# IC. Run Analysis for Component 2: HTC -----------------------------------------

## a. Individual Analysis ------------------------------------------------------
# Use all HIV- individuals (data_hiv_negative_complete)

# Unadjusted analysis (no covariates)
individual_htc_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_negative_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X2_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  modelSelect = "No",
  myCovariates = NULL, # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(individual_htc_unadjusted, 
           "./3_Results/33_ResultsHTC/htc_individual_unadjusted.xlsx")

# Adjusted analysis (with covariates)
individual_htc_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_negative_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X2_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  modelSelect = "No",
  myCovariates = unique(c(hiv_confounders, htc_confounders)), # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(individual_htc_adjusted, 
           "./3_Results/33_ResultsHTC/htc_individual_adjusted.xlsx")

# Interaction adjusted analysis (with covariates)
individual_htc_interaction <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_negative_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X2_ik", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  modelSelect = "Yes",
  myCovariates = unique(c(hiv_confounders, htc_confounders, 
                          allInteractionTerms)), # Vector of covariate names in dataset
  myInteractions = allInteractions,
  myForcedTerms = unique(c(hiv_confounders, htc_confounders)),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(individual_htc_interaction, 
           "./3_Results/33_ResultsHTC/htc_individual_interaction.xlsx")

## b. Spillover Analysis -------------------------------------------------------
# Use all HIV- untreated (data_hiv_negative_untreated_complete)

# Unadjusted analysis (no covariates)
spillover_htc_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z2_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  modelSelect = "No",
  myCovariates = NULL, # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(spillover_htc_unadjusted, 
           "./3_Results/33_ResultsHTC/htc_spillover_unadjusted.xlsx")

# Adjusted analysis (with covariates)
spillover_htc_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z2_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  modelSelect = "No",
  myCovariates = unique(c(hiv_confounders, htc_confounders)), # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(spillover_htc_adjusted, 
           "./3_Results/33_ResultsHTC/htc_spillover_adjusted.xlsx")

# Interaction adjusted analysis (with covariates)
spillover_htc_interaction <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_negative_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z2_k", # Component name in dataset
  myOutcome = "Y1_ik", # Outcome name in dataset
  modelSelect = "Yes",
  myCovariates = unique(c(hiv_confounders, htc_confounders, 
                          allInteractionTerms)), # Vector of covariate names in dataset
  myInteractions = allInteractions,
  myForcedTerms = unique(c(hiv_confounders, htc_confounders)),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(spillover_htc_interaction, 
           "./3_Results/33_ResultsHTC/htc_spillover_interaction.xlsx")

# IIA. Run Overall Effects on Mortality Analysis -------------------------------

# Unadjusted analysis (no covariates)
overall_mortality_unadjusted <- runOverallAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_positive_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Xany_ik", # Component name in dataset
  myComponentProp = "Zany_k_hivpos", # Component proportion in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  modelSelect = "No",
  myCovariates = NULL, # Vector of covariate names in dataset
  myInteractions = NULL, # Interactions desired
  myForcedTerms = NULL, # Terms that must remain in the model
  myCutoff = NULL, # P-value cutoff for model selection
  mySelectionCriteria = NULL
)
write_xlsx(overall_mortality_unadjusted, 
           "./3_Results/36_ResultsMortalityOverall/mortality_overall_unadjusted.xlsx")

# Adjusted analysis (with covariates)
overall_mortality_adjusted <- runOverallAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_positive_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Xany_ik", # Component name in dataset
  myComponentProp = "Zany_k_hivpos", # Component proportion in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  modelSelect = "No",
  myCovariates = c(mortality_confounders), # Vector of covariate names in dataset
  myInteractions = NULL, # Interactions desired
  myForcedTerms = NULL, # Terms that must remain in the model
  myCutoff = NULL, # P-value cutoff for model selection
  mySelectionCriteria = NULL
)
write_xlsx(overall_mortality_adjusted, 
           "./3_Results/36_ResultsMortalityOverall/mortality_overall_adjusted.xlsx")

# Adjusted analysis (with covariates)
overall_mortality_interaction <- runOverallAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_positive_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Xany_ik", # Component name in dataset
  myComponentProp = "Zany_k_hivpos", # Component proportion in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  modelSelect = "Yes",
  myCovariates = unique(c(mortality_confounders, 
                          allInteractionTerms)), # Vector of covariate names in dataset
  myInteractions = allInteractions,
  myForcedTerms = c(mortality_confounders),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(overall_mortality_interaction, 
           "./3_Results/36_ResultsMortalityOverall/mortality_overall_interaction.xlsx")

# IIB. Run Analysis for Component 3: ART ---------------------------------------

## a. Individual Analysis ------------------------------------------------------
# Use all HIV+ (data_hiv_positive_complete)

# Unadjusted analysis (no covariates)
individual_art_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_positive_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X3_ik", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  modelSelect = "No",
  myCovariates = NULL, # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(individual_art_unadjusted, 
           "./3_Results/34_ResultsART/art_individual_unadjusted.xlsx")

# Adjusted analysis (with covariates)
individual_art_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_positive_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X3_ik", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  modelSelect = "No",
  myCovariates = unique(c(mortality_confounders, art_confounders)), # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(individual_art_adjusted, 
           "./3_Results/34_ResultsART/art_individual_adjusted.xlsx")


# Interaction adjusted analysis (with covariates)
individual_art_interaction <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_positive_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "X3_ik", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  modelSelect = "Yes",
  myCovariates = unique(c(mortality_confounders, art_confounders, 
                          allInteractionTerms)), # Vector of covariate names in dataset
  myInteractions = allInteractions,
  myForcedTerms = unique(c(mortality_confounders, art_confounders)),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(individual_art_interaction, 
           "./3_Results/34_ResultsART/art_individual_interaction.xlsx")

## b. Spillover Analysis -------------------------------------------------------
# Use all HIV+ untreated (data_hiv_positive_untreated_complete)

# Unadjusted analysis (no covariates)
spillover_art_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_positive_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z3_k", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  modelSelect = "No",
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
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_positive_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z3_k", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  modelSelect = "No",
  myCovariates = unique(c(mortality_confounders, art_confounders)), # Vector of covariate names in dataset
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(spillover_art_adjusted, 
           "./3_Results/34_ResultsART/art_spillover_adjusted.xlsx")


# Interaction adjusted analysis (with covariates)
spillover_art_interaction <- runMediationAnalysis(
  roundNumber = currentRoundNumber, # Number of significant digits desired
  myData = data_hiv_positive_untreated_complete, # Dataset
  mySubjectID = "subject_id", # Subject ID name in dataset
  myClusterID = "cluster_id", # Cluster ID name in dataset
  myTreatment = "T_k", # Treatment name in dataset
  myComponent = "Z3_k", # Component name in dataset
  myOutcome = "Y2_ik", # Outcome name in dataset
  modelSelect = "Yes",
  myCovariates = unique(c(mortality_confounders, art_confounders, 
                          allInteractionTerms)), # Vector of covariate names in dataset
  myInteractions = allInteractions,
  myForcedTerms = unique(c(mortality_confounders, art_confounders)),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(spillover_art_interaction, 
           "./3_Results/34_ResultsART/art_spillover_interaction.xlsx")
