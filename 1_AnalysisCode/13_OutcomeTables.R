# 13_OutcomeTables.R
# Generate main outcome tables comparing components to main outcomes

# 0. Load necessary packages and datasets --------------------------------------
source("./RequiredPackages.R")

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

# 1. Define functions for creating tables --------------------------------------

threeWayComponent <- function(myData,
                              myOutcome,
                              myComponent,
                              myTreatment){
  
  levelOrder <- c("Yes", "No", "Female", "Began study HIV-infected", 
                  "Control", "Treatment", "Missing")
  
  tableData <- myData %>%
    dplyr::select(Treatment = all_of(myTreatment),
                  Component = all_of(myComponent),
                  Outcome = all_of(myOutcome)
                  ) %>%
    mutate(Treatment = ifelse(Treatment == "Intervention", "Treatment", 
                              ifelse(Treatment == "Standard of Care", "Control", NA)),
           Outcome = ifelse(is.na(Outcome), "Missing", Outcome),
           Component = ifelse(is.na(Component), "Missing", Component)) %>%
    mutate(across(everything(), 
                  ~factor(., levels = levelOrder))) %>%
    group_by(across(everything())) %>%
    dplyr::summarize(Count = n(), .groups = "drop") %>%
    ungroup() %>%
    pivot_wider(
      names_from = Component,
      values_from = Count
    ) %>%
    arrange(Treatment, Outcome) %>%
    mutate(across(where(is.numeric), tidyr::replace_na, 0)) %>%
    mutate(`Total` = rowSums(across(where(is.numeric) & !any_of("Missing")), 
                           na.rm = TRUE)) %>%
    relocate(any_of("Missing"), .after = last_col())
  
  tableDataTotals <- tableData %>%
    filter(Outcome != "Missing") %>%
    group_by(Treatment) %>%
    dplyr::summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(Outcome = "Total", .before = 1)
    
  
  tableOutput <- bind_rows(tableData, tableDataTotals) %>%
    arrange(Treatment,
            factor(Outcome, levels = c("Yes", "No", "Total", "Missing")))
    
  return(tableOutput)
}


threeWayComponentProp <- function(myData,
                                  myOutcome,
                                  myComponentProp,
                                  myTreatment,
                                  roundNumber = 3){
  
  levelOrder <- c("Yes", "No", "Female", "Began study HIV-infected", 
                  "Control", "Treatment", "Total", "Missing")
  
  tableData <- myData %>%
    dplyr::select(Treatment = all_of(myTreatment),
                  Component = all_of(myComponentProp),
                  Outcome = all_of(myOutcome)
    ) %>%
    mutate(Treatment = ifelse(Treatment == "Intervention", "Treatment", 
                              ifelse(Treatment == "Standard of Care", "Control", NA)),
           Outcome = ifelse(is.na(Outcome), "Missing", Outcome)) %>%
    mutate(across(where(is.character), 
                  ~factor(., levels = levelOrder))) %>%
    group_by(Treatment, Outcome) %>%
    dplyr::summarize(Mean = round(mean(Component), roundNumber),
                     Min = round(min(Component), roundNumber),
                     Max = round(max(Component), roundNumber),
                     SD = round(sd(Component), roundNumber),
                     #Variance = round(sd(Component)^2, roundNumber),
                     Count = n(),
                     .groups = "drop")
  
  tableDataTotal <- myData %>%
    dplyr::select(Treatment = all_of(myTreatment),
                  Component = all_of(myComponentProp),
                  Outcome = all_of(myOutcome)
    ) %>%
    mutate(Treatment = ifelse(Treatment == "Intervention", "Treatment", 
                              ifelse(Treatment == "Standard of Care", "Control", NA)),
           Outcome = ifelse(is.na(Outcome), "Missing", "Total")) %>%
    mutate(across(where(is.character), 
                  ~factor(., levels = levelOrder))) %>%
    group_by(Treatment, Outcome) %>%
    dplyr::summarize(Mean = round(mean(Component), roundNumber),
                     Min = round(min(Component), roundNumber),
                     Max = round(max(Component), roundNumber),
                     SD = round(sd(Component), roundNumber),
                     #Variance = round(sd(Component)^2, roundNumber),
                     Count = n(),
                     .groups = "drop") %>%
    dplyr::filter(Outcome != "Missing")
  
  tableOutput <- bind_rows(tableData, tableDataTotal) %>%
    arrange(Treatment,
            factor(Outcome, levels = c("Yes", "No", "Total", "Missing")))
  
  return(tableOutput)
}


# 2. Call on functions to create tables ----------------------------------------

# VMMC ANALYSIS
## Individual analysis uses data_hiv_negative_males
vmmc_individual_table <- threeWayComponent(myData = data_hiv_negative_males,
                                           myOutcome = "endpoint_seroconvert",
                                           myComponent = "endpoint_coverage_mc",
                                           myTreatment = "random_arm")
## Spillover analysis uses data_hiv_negative_untreated
vmmc_spillover_table <- threeWayComponentProp(myData = data_hiv_negative_untreated,
                                              myOutcome = "endpoint_seroconvert",
                                              myComponentProp = "Z1_k",
                                              myTreatment = "random_arm",
                                              roundNumber = 3)
## Overall analysis uses data_full
vmmc_overall_table <- threeWayComponent(myData = data_full,
                                        myOutcome = "endpoint_seroconvert",
                                        myComponent = "endpoint_coverage_mc",
                                        myTreatment = "random_arm")
vmmc_list <- list(`Y1 by X1 Individual` = vmmc_individual_table,
                  `Y1 by Z1 Spillover` = vmmc_spillover_table,
                  `Y1 by X1 Overall` = vmmc_overall_table)

# HTC ANALYSIS
## Individual analysis uses data_hiv_negative
htc_individual_table <- threeWayComponent(myData = data_hiv_negative,
                                          myOutcome = "endpoint_seroconvert",
                                          myComponent = "endpoint_coverage_htc",
                                          myTreatment = "random_arm")
## Spillover analysis uses data_hiv_negative_untreated
htc_spillover_table <- threeWayComponentProp(myData = data_hiv_negative_untreated,
                                             myOutcome = "endpoint_seroconvert",
                                             myComponentProp = "Z2_k",
                                             myTreatment = "random_arm",
                                             roundNumber = 3)
## Overall analysis uses data_full
htc_overall_table <- threeWayComponent(myData = data_full,
                                       myOutcome = "endpoint_seroconvert",
                                       myComponent = "endpoint_coverage_htc",
                                       myTreatment = "random_arm")
htc_list <- list(`Y1 by X2 Individual` = htc_individual_table,
                 `Y1 by Z2 Spillover` = htc_spillover_table,
                 `Y1 by X2 Overall` = htc_overall_table)

# ART ANALYSIS
## Individual analysis data_hiv_positive
art_individual_table <- threeWayComponent(myData = data_hiv_positive,
                                          myOutcome = "endpoint_death",
                                          myComponent = "endpoint_coverage_onart",
                                          myTreatment = "random_arm")
## Spillover analysis uses data_hiv_positive_untreated
art_spillover_table <- threeWayComponentProp(myData = data_hiv_positive_untreated,
                                             myOutcome = "endpoint_death",
                                             myComponentProp = "Z3_k",
                                             myTreatment = "random_arm",
                                             roundNumber = 3)
## Overall analysis 
art_overall_table <- threeWayComponent(myData = data_full,
                                       myOutcome = "endpoint_death",
                                       myComponent = "endpoint_coverage_onart",
                                       myTreatment = "random_arm")

art_list <- list(`Y2 by X3 Individual` = art_individual_table,
                 `Y2 by Z3 Spillover` = art_spillover_table,
                 `Y2 by X3 Overall` = art_overall_table)

# 3. Save all table results ----------------------------------------------------

write_xlsx(vmmc_list, "./3_Results/32_ResultsVMMC/vmmc_three_way_tables.xlsx")

write_xlsx(htc_list, "./3_Results/33_ResultsHTC/htc_three_way_tables.xlsx")

write_xlsx(art_list, "./3_Results/34_ResultsART/art_three_way_tables.xlsx")

