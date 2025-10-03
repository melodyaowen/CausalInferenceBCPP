# 02_BaselineTables.R
# This script uses the cleaned data and generates baseline tables for the 
# full dataset as well as the subsets used in the analyses

# 0. Load necessary packages and datasets --------------------------------------
source("./RequiredPackages.R")

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

# 1. Define functions for creating tables --------------------------------------

# Function for baseline characteristic table
createBaselineTable <- function(roundNumber,
                                myData, # Desired dataset
                                mySubjectID = "subject_id", # subject ID name in dataset
                                myClusterID = "cluster_id", # cluster ID name in dataset
                                myClusterSize = "cluster_size", # cluster size name in dataset
                                myTreatmentGroup = "T_k", # treatment assignment name in dataset
                                myVarList, # List of individual-level variables in dataset
                                myVarListLabels, # Labels for individual-level variables
                                myLevelOrder, # Order of individual-level variable levels
                                myClusterList, # List of cluster-level proportions
                                myClusterLabels # List of cluster labels
                                ){
  
  baselineTable <- myData %>%
    dplyr::select(subject_id = all_of(mySubjectID), cluster_id = all_of(myClusterID), 
                  cluster_size = all_of(myClusterSize), treatment_group = all_of(myTreatmentGroup),
                  all_of(myVarList)) %>% # Select only desired variables
    mutate(treatment_group = ifelse(treatment_group == 1, "Treatment",
                                     ifelse(treatment_group == 0, "Control",
                                            NA))) %>% # Update treatment group to be named and labelled correctly
    mutate(across(
      where(~ !is.numeric(.x)), # If not numeric, make NAs say "Missing"
      ~ ifelse(is.na(.x), "Missing", as.character(.x))
    ))
  
  # Subject count for each treatment group
  subjectCount <- baselineTable %>%
    dplyr::select(subject_id, treatment_group) %>%
    group_by(treatment_group) %>%
    dplyr::summarize(n = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = treatment_group, values_from = n) %>%
    mutate(Variable = "Number of Individuals",
           Level = "--") %>%
    relocate(Variable, Level) %>%
    mutate(Overall = Control + Treatment) %>%
    mutate(Control = as.character(Control),
           Treatment = as.character(Treatment),
           Overall = as.character(Overall))
  
  # Cluster count for each treatment group
  clusterCount <- baselineTable %>%
    dplyr::select(cluster_id, treatment_group) %>%
    distinct() %>%
    group_by(treatment_group) %>%
    dplyr::summarize(n = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = treatment_group, values_from = n) %>%
    mutate(Variable = "Number of Clusters",
           Level = "--") %>%
    relocate(Variable, Level) %>%
    mutate(Overall = Control + Treatment) %>%
    mutate(Control = as.character(Control),
           Treatment = as.character(Treatment),
           Overall = as.character(Overall))
  
  # Create separate missing columns
  missingColumns <- baselineTable %>%
    # Select only columns that have at least one missing value
    # and select necessary subject ID and treatment group
    dplyr::select(subject_id, treatment_group,
                  where(~ (is.character(.) && any(. == "Missing", na.rm = TRUE)) |
                          (is.numeric(.)   && any(is.na(.))))) %>%
    # Change it to be either missing or not missing
    mutate(across(-c(subject_id, treatment_group), 
                  ~ if (is.character(.)) {
                    ifelse(. == "Missing", "Missing", "Not Missing")
                    } else if (is.numeric(.)) {
                      ifelse(is.na(.), "Missing", "Not Missing")
                      } else {
                        .  # leave other types unchanged
                        }
                  )) %>%
    dplyr::select(-subject_id, -treatment_group) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Status") %>%
    count(Variable, Status) %>%
    pivot_wider(names_from = Status, values_from = n, values_fill = 0) %>%
    mutate(Overall = `Missing` + `Not Missing`) %>%
    mutate(`Missing (Overall)` = paste0(Missing, " (", ifelse(round((Missing/Overall)*100, roundNumber) == 0, "<1", 
                                                              round((Missing/Overall)*100, roundNumber)), "%)")) %>%
    dplyr::select(Variable, `Missing (Overall)`)
  
  # Summarizing all numeric variables in the dataset
  baselineTableNumeric <- baselineTable %>%
    dplyr::select(-subject_id, -cluster_id) %>%
    dplyr::select(treatment_group,
                  where(~ (is.numeric(.) ))) %>%
    pivot_longer(cols = -treatment_group,
                 names_to = "Variable", values_to = "Value") %>%
    group_by(treatment_group, Variable) %>%
    dplyr::summarize(Mean = mean(Value, na.rm = TRUE),
                     SD = sd(Value, na.rm = TRUE),
                     .groups = "drop") %>%
    mutate(Summary = paste0(round(Mean, 0), " (", round(SD, roundNumber), ")")) %>%
    dplyr::select(-Mean, -SD) %>%
    pivot_wider(names_from = treatment_group, values_from = Summary) %>%
    mutate(Level = "Mean (SD)") %>%
    relocate(Variable, Level)
  
  baselineTableNumericOverall <- baselineTable %>% # Get overall stats
    dplyr::select(-subject_id, -cluster_id) %>%
    dplyr::select(treatment_group,
                  where(~ (is.numeric(.) ))) %>%
    pivot_longer(cols = -treatment_group, 
                 names_to = "Variable", values_to = "Value") %>%
    group_by(Variable) %>%
    dplyr::summarise(Mean = mean(Value, na.rm = TRUE),
                     SD   = sd(Value, na.rm = TRUE),
                     .groups = "drop") %>%
    mutate(Overall = paste0(round(Mean, roundNumber), " (", round(SD, roundNumber), ")")) %>%
    dplyr::select(-Mean, -SD)
  
  baselineTableNumericFinal <- baselineTableNumeric %>%
    full_join(baselineTableNumericOverall, by = "Variable")
  
  # Summarizing all categorical variables in the dataset
  baselineTableCharacter <- baselineTable %>%
    dplyr::select(-subject_id, -cluster_id) %>%
    dplyr::select(treatment_group,
                  where(~ (is.character(.) ))) %>%
    pivot_longer(cols = -treatment_group,
                 names_to = "Variable", values_to = "Level") %>%
    arrange(Variable, treatment_group, Level) %>%
    filter(Level != "Missing") %>%
    group_by(treatment_group, Variable, Level) %>%
    dplyr::summarize(n = n(), .groups = "drop") %>%
    arrange(Variable, treatment_group) %>%
    group_by(treatment_group, Variable) %>%
    mutate(Total = sum(n)) %>%
    ungroup() %>%
    mutate(Value = paste0(n, " (", ifelse(round((n/Total)*100, roundNumber) == 0, "<1", 
                                          round((n/Total)*100, roundNumber)), "%", ")")) %>%
    dplyr::select(-n, -Total) %>%
    pivot_wider(names_from = treatment_group, values_from = Value)
  
  baselineTableCharacterOverall <- baselineTable %>%
    dplyr::select(-subject_id, -cluster_id, -treatment_group) %>%
    dplyr::select(where(~ (is.character(.) ))) %>%
    pivot_longer(cols = everything(),
                 names_to = "Variable", values_to = "Level") %>%
    arrange(Variable, Level) %>%
    filter(Level != "Missing") %>%
    group_by(Variable, Level) %>%
    dplyr::summarize(n = n(), .groups = "drop") %>%
    arrange(Variable) %>%
    group_by(Variable) %>%
    mutate(Total = sum(n)) %>%
    ungroup() %>%
    mutate(Overall = paste0(n, " (", ifelse(round((n/Total)*100, roundNumber) == 0, "<1", 
                                            round((n/Total)*100, roundNumber)), "%", ")")) %>%
    dplyr::select(-n, -Total)

  baselineTableCharacterFinal <- baselineTableCharacter %>%
    full_join(baselineTableCharacterOverall, by = c("Variable", "Level"))
  
  
  # Summarizing all cluster proportion data
  clusterTable <- myData %>%
    dplyr::select(treatment_group = all_of(myTreatmentGroup),
                  all_of(myClusterList)) %>% # Select only desired variables
    mutate(treatment_group = ifelse(treatment_group == 1, "Treatment",
                                    ifelse(treatment_group == 0, "Control",
                                           NA))) %>%
    pivot_longer(-treatment_group, names_to = "Variable", values_to = "Value") %>%
    arrange(treatment_group, Variable) %>%
    group_by(treatment_group, Variable) %>%
    dplyr::summarize(Mean = mean(Value), SD = sd(Value),
                     .groups = "drop") %>%
    mutate(Value = paste0(ifelse(round(Mean, 2) == 0, "<0.01", 
                                 round(Mean, 2)), " (", round(SD, 2), ")")) %>%
    dplyr::select(-Mean, -SD) %>%
    pivot_wider(names_from = treatment_group, values_from = Value)

  clusterTableOverall <- myData %>%
    dplyr::select(all_of(myClusterList)) %>% # Select only desired variables
    pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
    arrange(Variable) %>%
    group_by(Variable) %>%
    dplyr::summarize(Mean = mean(Value), SD = sd(Value),
                     .groups = "drop") %>%
    mutate(Overall = paste0(ifelse(round(Mean, 2) == 0, "<0.01", 
                                   round(Mean, 2)), " (", round(SD, 2), ")")) %>%
    dplyr::select(-Mean, -SD)
    
  clusterTableFinal <- clusterTable %>%
    full_join(clusterTableOverall, by = "Variable") %>%
    mutate(Level = "Mean (SD)") %>%
    relocate(Variable, Level) %>%
    mutate("Missing (Overall)" = NA)
  
  # Update list of variables based newly added 3
  varListFinal <- c("Number of Individuals", "Number of Clusters", 
                    "cluster_size", myVarList, myClusterList)
  
  # Update list of variable labels based on newly added 3
  varListLabelsFinal <- c("Number of Individuals", "Number of Clusters",
                          "Cluster Size", myVarListLabels, myClusterLabels)
  
  # Now create final baseline table with everything in it
  outputTable <- subjectCount %>%
    bind_rows(clusterCount) %>%
    bind_rows(baselineTableNumericFinal) %>%
    bind_rows(baselineTableCharacterFinal) %>%
    left_join(missingColumns, by = "Variable") %>%
    bind_rows(clusterTableFinal) %>%
    # Update Variable Order
    mutate(Variable = factor(Variable, levels = varListFinal, 
                             labels = varListLabelsFinal)) %>%
    # Update Level Order
    mutate(Level = factor(Level, levels = myLevelOrder)) %>%
    arrange(Variable, Level) %>%
    mutate(Variable = as.character(Variable),
           `Missing (Overall)` = as.character(`Missing (Overall)`)) %>%
    group_by(Variable) %>%
    # keep only on the first row of each Variable group
    mutate(`Missing (Overall)` = if_else(row_number() == 1, `Missing (Overall)`, ""),
           Variable            = if_else(row_number() == 1, Variable, "")) %>%
    ungroup() %>%
    mutate(`Missing (Overall)` = ifelse(is.na(`Missing (Overall)`), "", `Missing (Overall)`))
  
  # Return final output table
  return(outputTable)
  
} # End createBaselineTable()


# Function for component and outcome table
createComponentOutcomeTable <- function(roundNumber,
                                        myData, # Desired dataset
                                        mySubjectID = "subject_id", # subject ID name in dataset
                                        myClusterID = "cluster_id", # cluster ID name in dataset
                                        myTreatmentGroup = "T_k", # treatment assignment name in dataset
                                        
                                        myComponentList, # List of component variables
                                        myComponentLabels, # Labels for component variables
                                        
                                        myComponentPropList, # List of component proportion variables 
                                        myComponentPropLabels, # Labels of component proportion variables
                                        
                                        myOutcomeList, # List of outcome variables
                                        myOutcomeLabels, # Labels for outcome variables
                                        
                                        myLevelOrder # Order of individual-level variable levels
                                        ){
  
  # Subject count for each treatment group
  subjectCount <- myData %>%
    dplyr::select(subject_id = all_of(mySubjectID), 
                  treatment_group = all_of(myTreatmentGroup)) %>%
    mutate(treatment_group = ifelse(treatment_group == 1, "Treatment",
                                    ifelse(treatment_group == 0, "Control", NA))) %>%
    group_by(treatment_group) %>%
    dplyr::summarize(n = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = treatment_group, values_from = n) %>%
    mutate(Variable = "Number of Individuals",
           Level = "--") %>%
    relocate(Variable, Level) %>%
    mutate(Overall = Control + Treatment) %>%
    mutate(Control = as.character(Control),
           Treatment = as.character(Treatment),
           Overall = as.character(Overall))
  
  # Cluster count for each treatment group
  clusterCount <- myData %>%
    dplyr::select(subject_id = all_of(mySubjectID), 
                  treatment_group = all_of(myTreatmentGroup)) %>%
    mutate(treatment_group = ifelse(treatment_group == 1, "Treatment",
                                    ifelse(treatment_group == 0, "Control", NA))) %>%
    distinct() %>%
    group_by(treatment_group) %>%
    dplyr::summarize(n = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = treatment_group, values_from = n) %>%
    mutate(Variable = "Number of Clusters",
           Level = "--") %>%
    relocate(Variable, Level) %>%
    mutate(Overall = Control + Treatment) %>%
    mutate(Control = as.character(Control),
           Treatment = as.character(Treatment),
           Overall = as.character(Overall))
  
  # Component/Outcome Tables
  componentOutcomeTable <- myData %>%
    dplyr::select(treatment_group = all_of(myTreatmentGroup), 
                  all_of(myComponentList), all_of(myOutcomeList)) %>%
    mutate(across(all_of(myOutcomeList), ~ ifelse(. == 1, "Yes", 
                                                  ifelse(. == 0, "No", NA)))) %>%
    mutate(across(where(is.character), ~replace_na(., "Missing"))) %>%
    mutate(treatment_group = ifelse(treatment_group == 1, "Treatment",
                                    ifelse(treatment_group == 0, "Control", NA))) %>%
    pivot_longer(cols = -treatment_group,
                 names_to = "Variable", values_to = "Level") %>%
    arrange(Variable, treatment_group, Level) %>%
    filter(Level != "Missing") %>%
    group_by(treatment_group, Variable, Level) %>%
    dplyr::summarize(n = n(), .groups = "drop") %>%
    arrange(Variable, treatment_group) %>%
    group_by(treatment_group, Variable) %>%
    mutate(Total = sum(n)) %>%
    ungroup() %>%
    mutate(Value = paste0(n, " (", ifelse(round((n/Total)*100, roundNumber) == 0, "<1", 
                                          round((n/Total)*100, roundNumber)), "%", ")")) %>%
    dplyr::select(-n, -Total) %>%
    pivot_wider(names_from = treatment_group, values_from = Value)
  
  componentOutcomeOverallTable <- myData %>%
    dplyr::select(all_of(myComponentList), all_of(myOutcomeList)) %>%
    mutate(across(all_of(myOutcomeList), ~ ifelse(. == 1, "Yes", 
                                                  ifelse(. == 0, "No", NA)))) %>%
    mutate(across(where(is.character), ~replace_na(., "Missing"))) %>%
    pivot_longer(cols = everything(),
                 names_to = "Variable", values_to = "Level") %>%
    arrange(Variable, Level) %>%
    filter(Level != "Missing") %>%
    group_by(Variable, Level) %>%
    dplyr::summarize(n = n(), .groups = "drop") %>%
    arrange(Variable) %>%
    group_by(Variable) %>%
    mutate(Total = sum(n)) %>%
    ungroup() %>%
    mutate(Overall = paste0(n, " (", ifelse(round((n/Total)*100, roundNumber) == 0, "<1", 
                                            round((n/Total)*100, roundNumber)), "%", ")")) %>%
    dplyr::select(-n, -Total)
  
  componentOutcomeTableFinal <- componentOutcomeTable %>%
    full_join(componentOutcomeOverallTable, by = c("Variable", "Level"))
  
  
  
  # Component Proportion Table
  componentPropTable <- myData %>%
    dplyr::select(treatment_group = all_of(myTreatmentGroup), 
                  all_of(myComponentPropList)) %>% # Select only desired variables
    mutate(treatment_group = ifelse(treatment_group == 1, "Treatment",
                                    ifelse(treatment_group == 0, "Control",
                                           NA))) %>%
    pivot_longer(-treatment_group, names_to = "Variable", values_to = "Value") %>%
    arrange(treatment_group, Variable) %>%
    group_by(treatment_group, Variable) %>%
    dplyr::summarize(Mean = mean(Value), SD = sd(Value),
                     .groups = "drop") %>%
    mutate(Value = paste0(ifelse(round(Mean, 2) == 0, "<0.01", 
                                 round(Mean, 2)), " (", round(SD, 2), ")")) %>%
    dplyr::select(-Mean, -SD) %>%
    pivot_wider(names_from = treatment_group, values_from = Value)
  
  componentPropOverallTable <- myData %>%
    dplyr::select(all_of(myComponentPropList)) %>% # Select only desired variables
    pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
    arrange(Variable) %>%
    group_by(Variable) %>%
    dplyr::summarize(Mean = mean(Value), SD = sd(Value),
                     .groups = "drop") %>%
    mutate(Overall = paste0(ifelse(round(Mean, 2) == 0, "<0.01", 
                                   round(Mean, 2)), " (", round(SD, 2), ")")) %>%
    dplyr::select(-Mean, -SD)
  
  componentPropTableFinal <- componentPropTable %>%
    full_join(componentPropOverallTable, by = "Variable") %>%
    mutate(Level = "Mean (SD)") %>%
    relocate(Variable, Level)
  
  
  # Create separate missing columns
  missingColumns <- myData %>%
    # Select only columns that have at least one missing value
    # and select necessary subject ID and treatment group
    dplyr::select(subject_id = all_of(mySubjectID),
                  treatment_group = all_of(myTreatmentGroup), 
                  all_of(myComponentList), all_of(myOutcomeList)) %>%
    mutate(across(all_of(myOutcomeList), ~ ifelse(. == 1, "Yes", 
                                                  ifelse(. == 0, "No", NA)))) %>%
    mutate(across(where(is.character), ~replace_na(., "Missing"))) %>%
    mutate(treatment_group = ifelse(treatment_group == 1, "Treatment",
                                    ifelse(treatment_group == 0, "Control", NA))) %>%
    # Change it to be either missing or not missing
    mutate(across(-c(subject_id, treatment_group), 
                  ~ if (is.character(.)) {
                    ifelse(. == "Missing", "Missing", "Not Missing")
                  } else if (is.numeric(.)) {
                    ifelse(is.na(.), "Missing", "Not Missing")
                  } else {
                    .  # leave other types unchanged
                  }
    )) %>%
    dplyr::select(-subject_id, -treatment_group) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Status") %>%
    count(Variable, Status) %>%
    pivot_wider(names_from = Status, values_from = n, values_fill = 0) %>%
    mutate(Overall = `Missing` + `Not Missing`) %>%
    mutate(`Missing (Overall)` = paste0(Missing, " (", ifelse(round((Missing/Overall)*100, roundNumber) == 0, "<1", 
                                                              round((Missing/Overall)*100, roundNumber)), "%)")) %>%
    dplyr::select(Variable, `Missing (Overall)`)
  
  # Update list of variables based newly added 3
  varListFinal <- c("Number of Individuals", "Number of Clusters", 
                    myComponentList, myComponentPropList,
                    myOutcomeList)
  
  # Update list of variable labels based on newly added 3
  varListLabelsFinal <- c("Number of Individuals", "Number of Clusters",
                          myComponentLabels, myComponentPropLabels,
                          myOutcomeLabels)
  
  # Now create final baseline table with everything in it
  outputTable <- subjectCount %>%
    bind_rows(clusterCount) %>%
    bind_rows(componentOutcomeTableFinal) %>%
    bind_rows(componentPropTableFinal) %>%
    left_join(missingColumns, by = "Variable") %>%
    # Update Variable Order
    mutate(Variable = factor(Variable, levels = varListFinal, 
                             labels = varListLabelsFinal)) %>%
    # Update Level Order
    mutate(Level = factor(Level, levels = myLevelOrder)) %>%
    arrange(Variable, Level) %>%
    mutate(Variable = as.character(Variable),
           `Missing (Overall)` = as.character(`Missing (Overall)`)) %>%
    group_by(Variable) %>%
    # keep only on the first row of each Variable group
    mutate(`Missing (Overall)` = if_else(row_number() == 1, `Missing (Overall)`, ""),
           Variable            = if_else(row_number() == 1, Variable, "")) %>%
    ungroup() %>%
    mutate(`Missing (Overall)` = ifelse(is.na(`Missing (Overall)`), "", `Missing (Overall)`))
  
  # Return final output table
  return(outputTable)
  
} # End createComponentOutcomeTable()


# 2. Define necessary input variables for functions ----------------------------

var_individual_list <- c("hiv_status_current",
                         "gender",
                         "age",
                         "marital_status",
                         "education",
                         "employment_status",
                         "monthly_income",
                         "length_residence",
                         "partners_lifetime",
                         "partners_12mos",
                         "overall_access",
                         "exchange_12mos",
                         "condom_lastsex")

var_individual_list_names <- c("HIV Status at Study Start",
                               "Gender",
                               "Age",
                               "Marital Status",
                               "Education",
                               "Employment Status",
                               "Monthly Income",
                               "Length in Current Residence",
                               "Number of Partners (Lifetime)",
                               "Number of Partners (Last 12 Months)",
                               "Has Access to Healthcare",
                               "Transactional Sex Ever",
                               "Condom Use in Last Sexual Encounter")
  
var_village_list <- c("prop_male",
                      "prop_began_infected",
                      "hiv_refused_testing_prop")

var_village_list_names <- c("Village Proportion of Males in Cluster",
                            "Village Proportion of HIV Infected at Study Start",
                            "Village Proportion Refused HIV Testing at Study Start")

var_individual_levels_order <- c("Yes", "No",
                                 
                                 "Male", "Female", 
                                 
                                 "HIV-uninfected", "HIV-infected", 
                                 "Refused HIV testing", 
                                 
                                 "Single/never married", "Married", 
                                 "Divorced/separated", "Widowed",
                                 
                                 "Non-formal", "Primary", "Junior secondary", 
                                 "Senior secondary", "Higher than senior secondary",
                                 
                                 "Employed", "Unemployed looking for work", 
                                 "Unemployed not looking for work",
                                 
                                 "No income", "1-199 pula", "200-499 pula", 
                                 "500-999 pula", "1000-4999 pula", 
                                 "5000-10000 pula", "More than 10000 pula", 
                                 
                                 "Less than 6 months", "6 months to 12 months", 
                                 "1 to 5 years", "More than 5 years", 
                                 
                                 "Strongly disagree", "Disagree", 
                                 "Agree", "Strongly agree", 
                                 
                                 "Mean (SD)", "--")

componentList <- c("endpoint_coverage_mc", "endpoint_coverage_htc", "endpoint_coverage_onart")
componentLabels <- c("Component 1: VMMC", "Component 2: HTC", "Component 3: ART")

componentPropList <- c("Z1_k", "Z2_k", "Z3_k")
componentPropLabels <- c("Village Proportion of Men who received VMMC",
                         "Village Proportion of People who received HTC",
                         "Village Proportion of HIV+ who received ART")

outcomeList <- c("Y1_ik", "Y2_ik")
outcomeLabels <- c("Outcome 1: HIV Seroconversion (3-year period)",
                   "Outcome 2: Death (3-year period)")

levelOrder <- c("Yes", "No", "Female", "Mean (SD)", "--")

# 3. Run function on each subset dataset and save ------------------------------

# FULL DATASET
baseline_full <- createBaselineTable(roundNumber = 0,
                                     myData = data_full,
                                     mySubjectID = "subject_id",
                                     myClusterID = "cluster_id",
                                     myClusterSize = "cluster_size",
                                     myTreatmentGroup = "T_k",
                                     myVarList = var_individual_list,
                                     myVarListLabels = var_individual_list_names,
                                     myClusterList = var_village_list,
                                     myClusterLabels = var_village_list_names,
                                     myLevelOrder = var_individual_levels_order)
component_outcome_full <- createComponentOutcomeTable(roundNumber = 0,
                                                      myData = data_full,
                                                      mySubjectID = "subject_id",
                                                      myClusterID = "cluster_id",
                                                      myTreatmentGroup = "T_k",
                                                      myComponentList = componentList,
                                                      myComponentLabels = componentLabels,
                                                      myComponentPropList = componentPropList,
                                                      myComponentPropLabels = componentPropLabels,
                                                      myOutcomeList = outcomeList,
                                                      myOutcomeLabels = outcomeLabels,
                                                      myLevelOrder = levelOrder)


# HIV NEGATIVE DATASET
baseline_hiv_negative <- createBaselineTable(roundNumber = 0,
                                             myData = data_hiv_negative,
                                             mySubjectID = "subject_id",
                                             myClusterID = "cluster_id",
                                             myClusterSize = "cluster_size",
                                             myTreatmentGroup = "T_k",
                                             myVarList = var_individual_list,
                                             myVarListLabels = var_individual_list_names,
                                             myClusterList = var_village_list,
                                             myClusterLabels = var_village_list_names,
                                             myLevelOrder = var_individual_levels_order)
component_outcome_hiv_negative <- createComponentOutcomeTable(roundNumber = 0,
                                                      myData = data_hiv_negative,
                                                      mySubjectID = "subject_id",
                                                      myClusterID = "cluster_id",
                                                      myTreatmentGroup = "T_k",
                                                      myComponentList = componentList,
                                                      myComponentLabels = componentLabels,
                                                      myComponentPropList = componentPropList,
                                                      myComponentPropLabels = componentPropLabels,
                                                      myOutcomeList = outcomeList,
                                                      myOutcomeLabels = outcomeLabels,
                                                      myLevelOrder = levelOrder)


# HIV NEGATIVE MALE DATASET
baseline_hiv_negative_males <- createBaselineTable(roundNumber = 0,
                                                   myData = data_hiv_negative_males,
                                                   mySubjectID = "subject_id",
                                                   myClusterID = "cluster_id",
                                                   myClusterSize = "cluster_size",
                                                   myTreatmentGroup = "T_k",
                                                   myVarList = var_individual_list,
                                                   myVarListLabels = var_individual_list_names,
                                                   myClusterList = var_village_list,
                                                   myClusterLabels = var_village_list_names,
                                                   myLevelOrder = var_individual_levels_order)
component_outcome_hiv_negative_males <- createComponentOutcomeTable(roundNumber = 0,
                                                              myData = data_hiv_negative_males,
                                                              mySubjectID = "subject_id",
                                                              myClusterID = "cluster_id",
                                                              myTreatmentGroup = "T_k",
                                                              myComponentList = componentList,
                                                              myComponentLabels = componentLabels,
                                                              myComponentPropList = componentPropList,
                                                              myComponentPropLabels = componentPropLabels,
                                                              myOutcomeList = outcomeList,
                                                              myOutcomeLabels = outcomeLabels,
                                                              myLevelOrder = levelOrder)


# HIV NEGATIVE UNTREATED DATASET
baseline_hiv_negative_untreated <- createBaselineTable(roundNumber = 0,
                                                       myData = data_hiv_negative_untreated,
                                                       mySubjectID = "subject_id",
                                                       myClusterID = "cluster_id",
                                                       myClusterSize = "cluster_size",
                                                       myTreatmentGroup = "T_k",
                                                       myVarList = var_individual_list,
                                                       myVarListLabels = var_individual_list_names,
                                                       myClusterList = var_village_list,
                                                       myClusterLabels = var_village_list_names,
                                                       myLevelOrder = var_individual_levels_order)
component_outcome_hiv_negative_untreated <- createComponentOutcomeTable(roundNumber = 0,
                                                                    myData = data_hiv_negative_untreated,
                                                                    mySubjectID = "subject_id",
                                                                    myClusterID = "cluster_id",
                                                                    myTreatmentGroup = "T_k",
                                                                    myComponentList = componentList,
                                                                    myComponentLabels = componentLabels,
                                                                    myComponentPropList = componentPropList,
                                                                    myComponentPropLabels = componentPropLabels,
                                                                    myOutcomeList = outcomeList,
                                                                    myOutcomeLabels = outcomeLabels,
                                                                    myLevelOrder = levelOrder)


# HIV POSITIVE DATASET
baseline_hiv_positive <- createBaselineTable(roundNumber = 0,
                                             myData = data_hiv_positive,
                                             mySubjectID = "subject_id",
                                             myClusterID = "cluster_id",
                                             myClusterSize = "cluster_size",
                                             myTreatmentGroup = "T_k",
                                             myVarList = var_individual_list,
                                             myVarListLabels = var_individual_list_names,
                                             myClusterList = var_village_list,
                                             myClusterLabels = var_village_list_names,
                                             myLevelOrder = var_individual_levels_order)
component_outcome_hiv_positive <- createComponentOutcomeTable(roundNumber = 0,
                                                              myData = data_hiv_positive,
                                                              mySubjectID = "subject_id",
                                                              myClusterID = "cluster_id",
                                                              myTreatmentGroup = "T_k",
                                                              myComponentList = componentList,
                                                              myComponentLabels = componentLabels,
                                                              myComponentPropList = componentPropList,
                                                              myComponentPropLabels = componentPropLabels,
                                                              myOutcomeList = outcomeList,
                                                              myOutcomeLabels = outcomeLabels,
                                                              myLevelOrder = levelOrder)


# Full dataset with no exclusions tables
write.csv(baseline_full, "./3_Results/31_BaselineTables/baseline_full.csv")
write.csv(component_outcome_full, "./3_Results/31_BaselineTables/component_outcome_full.csv")

# HIV negative tables
write.csv(baseline_hiv_negative, "./3_Results/31_BaselineTables/baseline_hiv_negative.csv")
write.csv(component_outcome_hiv_negative, "./3_Results/31_BaselineTables/component_outcome_hiv_negative.csv")

# HIV negative males tables
write.csv(baseline_hiv_negative_males, "./3_Results/31_BaselineTables/baseline_hiv_negative_males.csv")
write.csv(component_outcome_hiv_negative_males, "./3_Results/31_BaselineTables/component_outcome_hiv_negative_males.csv")

# HIV negative untreated tables
write.csv(baseline_hiv_negative_untreated, "./3_Results/31_BaselineTables/baseline_hiv_negative_untreated.csv")
write.csv(component_outcome_hiv_negative_untreated, "./3_Results/31_BaselineTables/component_outcome_hiv_negative_untreated.csv")

# HIV positive tables
write.csv(baseline_hiv_positive, "./3_Results/31_BaselineTables/baseline_hiv_positive.csv")
write.csv(component_outcome_hiv_positive, "./3_Results/31_BaselineTables/component_outcome_hiv_positive.csv")


