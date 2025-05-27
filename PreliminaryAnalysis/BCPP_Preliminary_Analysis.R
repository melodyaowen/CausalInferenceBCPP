# Installing Packages and Setup ------------------------------------------------

# Package names
packages <- c("tidyverse", "lme4", "MASS", "Matrix", "matrixcalc", "latex2exp",
              "ICC", "nlme", "bindata", "gee", "crt2power", "mosaic", "haven",
              "tmvtnorm", "ggplot2", "latex2exp", "reshape2", "gridExtra",
              "table1")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Reading in Data --------------------------------------------------------------

# Loading in original datasets
dat1 <- read_sas("~/Desktop/3. Causal Inference Mediation/BCPP data/BCPP data/longyr1_20samp_open.sas7bdat")
dat2 <- read_sas("~/Desktop/3. Causal Inference Mediation/BCPP data/BCPP data/longyr2_20samp_open.sas7bdat")
dat3 <- read_sas("~/Desktop/3. Causal Inference Mediation/BCPP data/BCPP data/longyr3_20samp_open.sas7bdat")

# Checking Columns -------------------------------------------------------------

# Create a dataframe for each set of column names
names1 <- tibble(colname = sort(colnames(dat1)), dataset = "dat1")
names2 <- tibble(colname = sort(colnames(dat2)), dataset = "dat2")
names3 <- tibble(colname = sort(colnames(dat3)), dataset = "dat3")

# Bind and pivot to wide
all_names <- bind_rows(names1, names2, names3) %>%
  mutate(present = colname) %>%
  pivot_wider(names_from = dataset, values_from = present) %>%
  arrange(colname)

all_names_filtered <- bind_rows(names1, names2, names3) %>%
  mutate(present = colname) %>%
  pivot_wider(names_from = dataset, values_from = present) %>%
  arrange(colname) %>%
  dplyr::filter(colname %in% c("de_subj_idC", # Subject identifier
                               "community", # Community identifier
                               
                               "random_arm", # Randomization arm (Treatment Assignment)
                               "gender", # Gender
                               
                               # At start of study? Unclear
                               "hiv_status_current", # Current HIV status
                               "circumcised", # Male circumcision
                               
                               # "Coverage Endpoint"
                               "endpoint_coverage_htc", # Coverage endpoint: HIV-tested or diagnosed HIV+
                               "endpoint_coverage_mc", # Coverage endpoint: male circumcision
                               
                               
                               # Outcome
                               "endpoint_seroconvert") # Incidence endpoint: seroconverted by study completion
  )

# Viewing the results
View(all_names)
View(all_names_filtered)

# Number of individuals in each dataset
nrow(dat1); length(unique(dat1$de_subj_idC))
nrow(dat2); length(unique(dat2$de_subj_idC))
nrow(dat3); length(unique(dat3$de_subj_idC))

dat1_renamed <- dat1 %>% rename_with(~ paste0(., "_dat1"), -de_subj_idC)
dat2_renamed <- dat2 %>% rename_with(~ paste0(., "_dat2"), -de_subj_idC)
dat3_renamed <- dat3 %>% rename_with(~ paste0(., "_dat3"), -de_subj_idC)

# Circumcision in all three datasets
combinedDatMC <- full_join(dat1_renamed, dat2_renamed, by = "de_subj_idC") %>%
  full_join(dat3_renamed, by = "de_subj_idC") %>%
  dplyr::select(de_subj_idC, starts_with("random_arm_dat"), starts_with("Gender_dat"),
                starts_with("circumcised_dat"), contains("endpoint_coverage_mc")) %>%
  mutate(random_arm = ifelse(!is.na(random_arm_dat1), random_arm_dat1,
                             ifelse(!is.na(random_arm_dat2), random_arm_dat2,
                                    random_arm_dat3))) %>%
  mutate(gender = ifelse(!is.na(gender_dat1), gender_dat1,
                         ifelse(!is.na(gender_dat2), gender_dat2,
                                gender_dat3))) %>%
  dplyr::select(subject_ID = de_subj_idC, gender,
                random_arm, starts_with("circumcised"),
                contains("endpoint_coverage_mc")) %>%
  mutate(gender = case_when(gender == "F" ~ "Female",
                            gender == "M" ~ "Male",
                            TRUE ~ NA_character_
  )) %>%
  mutate(circumcised_dat1 = ifelse(gender == "Female",
                                   "Female", circumcised_dat1),
         circumcised_dat2 = ifelse(gender == "Female",
                                   "Female", circumcised_dat2),
         circumcised_dat3 = ifelse(gender == "Female",
                                   "Female", circumcised_dat3)) %>%
  mutate(circumcised_dat2 = ifelse(circumcised_dat1 == "Yes", "Yes",
                                   circumcised_dat2)) %>%
  mutate(circumcised_dat3 = ifelse(circumcised_dat2 == "Yes", "Yes",
                                   ifelse(circumcised_dat1 == "Yes", "Yes",
                                          circumcised_dat3))) %>%
  mutate(across(everything(), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(endpoint_coverage_mc_dat1 = ifelse(gender == "Female", "Female", endpoint_coverage_mc_dat1),
         endpoint_coverage_mc_dat2 = ifelse(gender == "Female", "Female", endpoint_coverage_mc_dat2),
         endpoint_coverage_mc_dat3 = ifelse(gender == "Female", "Female", endpoint_coverage_mc_dat3)) %>%
  mutate(endpoint_coverage_mc = ifelse(circumcised_dat1 == "Yes",
                                       "Already Circumcised", NA)) %>%
  mutate(endpoint_coverage_mc = ifelse(gender == "Female", "Female",
                                       endpoint_coverage_mc)) %>%
  mutate(endpoint_coverage_mc = ifelse(is.na(endpoint_coverage_mc) & endpoint_coverage_mc_dat3 == "Yes",
                                       "Yes", endpoint_coverage_mc)) %>%
  mutate(endpoint_coverage_mc = ifelse(is.na(endpoint_coverage_mc) & endpoint_coverage_mc_dat2 == "Yes",
                                       "Yes", endpoint_coverage_mc)) %>%
  mutate(endpoint_coverage_mc = ifelse(is.na(endpoint_coverage_mc) & endpoint_coverage_mc_dat1 == "Yes",
                                       "Yes", endpoint_coverage_mc)) %>%
  mutate(endpoint_coverage_mc = ifelse(is.na(endpoint_coverage_mc) & circumcised_dat3 == "Yes",
                                       "Yes", endpoint_coverage_mc)) %>%
  mutate(endpoint_coverage_mc = ifelse(is.na(endpoint_coverage_mc) & circumcised_dat2 == "Yes",
                                       "Yes", endpoint_coverage_mc)) %>%
  mutate(endpoint_coverage_mc = ifelse(is.na(endpoint_coverage_mc) &
                                         rowSums(across(contains("dat"), ~ replace_na(. == "No", FALSE))) > 0 &
                                         rowSums(across(contains("dat"), ~ replace_na(. == "Yes", FALSE))) == 0,
                                       "No", endpoint_coverage_mc))


View(combinedDatMC)
View(filter(combinedDatMC, is.na(circumcised_dat1)))

tally(~endpoint_coverage_mc, data = combinedDatMC)
tally(random_arm~endpoint_coverage_mc, data = combinedDatMC)

# View(filter(combinedDatMC, circumcised_dat3 == "No", circumcised_dat2 == "Yes"))
# View(filter(combinedDatMC, circumcised_dat3 == "No", circumcised_dat1 == "Yes"))

# Cleaning Data ----------------------------------------------------------------

cleanDat1 <- dat1 %>%
  dplyr::select(de_subj_idC, # Subject identifier
                community, # Community identifier
                
                random_arm, # Randomization arm (Treatment Assignment)
                gender, # Gender
                
                # At start of study? Unclear
                hiv_status_current, # Current HIV status
                circumcised, # Male circumcision
                
                # "Coverage Endpoint"
                endpoint_coverage_htc, # Coverage endpoint: HIV-tested or diagnosed HIV+
                endpoint_coverage_mc, # Coverage endpoint: male circumcision
                
                # Outcome
                endpoint_seroconvert # Incidence endpoint: seroconverted by study completion
  )  %>%
  mutate_if(is.character, list(~na_if(., ""))) %>%
  arrange(community) %>%
  rowid_to_column("subject_id") %>%
  group_by(community) %>%
  mutate(cluster_id = cur_group_id()) %>%
  mutate(subj_cluster_id = row_number()) %>%
  relocate(subject_id, subj_cluster_id, cluster_id) %>%
  ungroup() %>%
  dplyr::select(-de_subj_idC, -community) %>%
  mutate(random_arm = ifelse(random_arm == "Standard of Care", "Control",
                             ifelse(random_arm == "Intervention", "Treatment", NA)),
         gender = ifelse(gender == "F", "Female",
                         ifelse(gender == "M", "Male", NA))) %>%
  mutate(endpoint_seroconvert = ifelse(hiv_status_current == "HIV-infected",
                                       "Already Infected", endpoint_seroconvert)) %>%
  mutate(endpoint_coverage_htc = ifelse(hiv_status_current == "HIV-infected",
                                        "Already Infected", endpoint_coverage_htc)) %>%
  mutate(circumcised = ifelse(gender == "Female", "Female", circumcised)) %>%
  mutate(endpoint_coverage_mc = ifelse(circumcised == "Yes", "Already Circumcised",
                                       endpoint_coverage_mc)) %>%
  mutate(endpoint_coverage_mc = ifelse(gender == "Female", "Female",
                                       endpoint_coverage_mc))

tally(~endpoint_coverage_htc, data = cleanDat1)
tally(~endpoint_coverage_mc, data = cleanDat1) # Question: No people circumcised in year 1
tally(~endpoint_seroconvert, data = cleanDat1)

# Filtering out data 1
filteredDat1 <- cleanDat1 %>%
  dplyr::filter(hiv_status_current != "HIV-infected",
                hiv_status_current != "Refused HIV testing")

tally(~endpoint_coverage_htc, data = filteredDat1)
tally(~endpoint_coverage_mc, data = filteredDat1)
tally(~endpoint_seroconvert, data = filteredDat1)

View(filteredDat1)

# Table 1 ----------------------------------------------------------------------

table1_cleanDat1 <- table1(~
                             gender + # Gender
                             hiv_status_current + # Current HIV status
                             circumcised + # Male circumcision
                             endpoint_coverage_htc + # Coverage endpoint: HIV-tested or diagnosed HIV+
                             endpoint_coverage_mc + # Coverage endpoint: male circumcision
                             endpoint_seroconvert # Incidence endpoint: seroconverted by study completion
                           | random_arm, cleanDat1)

table1_filteredDat1 <- table1(~
                                gender + # Gender
                                hiv_status_current + # Current HIV status
                                circumcised + # Male circumcision
                                endpoint_coverage_htc + # Coverage endpoint: HIV-tested or diagnosed HIV+
                                endpoint_coverage_mc + # Coverage endpoint: male circumcision
                                endpoint_seroconvert # Incidence endpoint: seroconverted by study completion
                              | random_arm, filteredDat1)

table1_filteredDat1 <- table1(~
                                gender + # Gender
                                hiv_status_current + # Current HIV status
                                circumcised + # Male circumcision
                                endpoint_coverage_htc + # Coverage endpoint: HIV-tested or diagnosed HIV+
                                endpoint_coverage_mc + # Coverage endpoint: male circumcision
                                endpoint_seroconvert # Incidence endpoint: seroconverted by study completion
                              | random_arm, filteredDat1)

# Preliminary Analysis for Year 1 Data -----------------------------------------

# Prepare Modeling Dataset
modelDat1 <- filteredDat1 %>%
  mutate(T_k = ifelse(random_arm == "Treatment", 1, 
                      ifelse(random_arm == "Control", 0, NA))) %>%
  mutate(X1_ik = ifelse(endpoint_coverage_mc == "Already Circumcised", 1,
                        ifelse(endpoint_coverage_mc == "No", 0, 
                               endpoint_coverage_mc))) %>%
  mutate(X2_ik = ifelse(endpoint_coverage_htc == "Yes", 1,
                        ifelse(endpoint_coverage_htc == "No", 0, NA))) %>%
  mutate(X12_ik = ifelse(gender == "Male" & X1_ik == "1" & X2_ik == 1, 1, 
                         ifelse(gender == "Female" & X2_ik == 1, 1, 0))) %>%
  mutate(C1_ik = ifelse(gender == "Male", 1, 
                        ifelse(gender == "Female", 0, NA))) %>%
  add_count(cluster_id, name = "cluster_size") %>%
  group_by(cluster_id) %>%
  mutate(male_count = sum(gender == "Male", na.rm = TRUE)) %>%
  mutate(mc_count = sum(X1_ik == "1", na.rm = TRUE)) %>%
  mutate(htc_count = sum(X2_ik == 1, na.rm = TRUE)) %>%
  mutate(full_trt_count = sum(X12_ik == 1, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Z1_k = mc_count/male_count,
         Z2_k = htc_count/cluster_size,
         Z12_k = full_trt_count/cluster_size) %>%
  mutate(Y_ik = ifelse(endpoint_seroconvert == "Yes", 1,
                       ifelse(endpoint_seroconvert == "No", 0, NA)))

modelDat1_final <- modelDat1 %>%
  dplyr::select(subject_id, subj_cluster_id, cluster_id, T_k, Y_ik, C1_ik, 
                X1_ik, X2_ik, X12_ik, Z1_k, Z2_k, Z12_k)

# QUESTION: Calculate proportions before or after exclusions for each setup?
tally(T_k ~ X12_ik, data = modelDat1_final)

## Estimating Within-Village Spillover -----------------------------------------

# Setup
# Only include those in treatment group with X12_ik == 0 (non-adherent)
#   Want to look at treatment group individuals who didn't receive any part of the treatment
# Only include those in control group who DID NOT receive any part of treatment
modelDat1_SpW <- modelDat1_final %>%
  filter(!(T_k == 1 & X12_ik != 0),
         !(T_k == 0 & X1_ik == 1),
         !(T_k == 0 & X2_ik == 1))
tally(T_k ~ X12_ik, data = modelDat1_SpW)
tally(T_k ~ X1_ik, data = modelDat1_SpW)
tally(T_k ~ X2_ik, data = modelDat1_SpW)
# QUESTION: This still includes those who are partially adherent in intervention
# group, but excludes anyone in control who received any part of the treatment.
# Is this ok?

# Spillover Within Intervention Clusters
model_SpW <- glm(Y_ik ~ T_k,
                 family = binomial(link = 'logit'),
                 data = modelDat1_SpW) # Exclude those who received full trt

model_SpW_summary <- summary(model_SpW) # Save model summary

beta_SpW_0 <- model_SpW_summary$coefficients[1,1] # Intercept
beta_SpW_1 <- model_SpW_summary$coefficients[2,1] # T_k Coefficient

# Spillover Mediated by Male Circumcision
model_SpWM <- glm(Y_ik ~ T_k + Z1_k,
                  family = binomial(link = 'logit'),
                  data = modelDat1_SpW) # Excluding those who received full trt

model_SpWM_summary <- summary(model_SpWM) # Save model summary

beta_SpWM_0 <- model_SpWM_summary$coefficients[1,1] # Intercept
beta_SpWM_1 <- model_SpWM_summary$coefficients[2,1] # T_k Coefficient
beta_SpWM_2 <- model_SpWM_summary$coefficients[3,1] # Z1_k Coefficient

# Proportion of within-intervention village spillover effect mediated by MC
beta_SpWM_1/beta_SpW_1; beta_SpWM_1/beta_SpWM_2
# QUESTION: Unclear which one is correct, I think it's the first one

## Estimating Individual Effect ------------------------------------------------

# Setup
# Include only males
# In the treatment groups, include only those who were circumcised (X1_ik = 1)
# Include everyone in the control villages
# QUESTION: Exclude those in control who also were circumcised? Yes, exclude control group circumcised
modelDat1_Ind <- modelDat1_final %>%
  filter(C1_ik == 1, !(T_k == 1 & X1_ik == 0))
tally(T_k ~ X1_ik, data = modelDat1_Ind)

# Individual Effects
model_Ind <- glm(Y_ik ~ T_k,
                 family = binomial(link = 'logit'),
                 data = modelDat1_Ind) # Exclude those who received full trt

model_Ind_summary <- summary(model_Ind) # Save model summary

beta_Ind_0 <- model_Ind_summary$coefficients[1,1] # Intercept
beta_Ind_1 <- model_Ind_summary$coefficients[2,1] # T_k Coefficient, 
                                                  # total Ind effect

# Individual Mediated Effect
model_IndM <- glm(Y_ik ~ T_k + X1_ik,
                  family = binomial(link = 'logit'),
                  data = modelDat1_Ind)

model_IndM_summary <- summary(model_IndM)

beta_IndM_0 <- model_IndM_summary$coefficients[1,1] # Intercept
beta_IndM_1 <- model_IndM_summary$coefficients[2,1] # T_k Coefficient, 
                                                    # direct individual effect
beta_IndM_2 <- model_IndM_summary$coefficients[3,1] # T_k Coefficient

beta_Ind_1 - beta_IndM_1 # Indirect individual effect

# Proportion of individual effect due to circumcision
(beta_Ind_1 - beta_IndM_1)/beta_Ind_1

## Total Effects ---------------------------------------------------------------

# Intervention village total effect
beta_Ind_1 + beta_SpW_1 

# Proportion of total effect mediated by male circumcision
((beta_Ind_1 - beta_IndM_1) + (beta_SpW_1 - beta_SpW_1))/(beta_Ind_1 + beta_SpW_1) 

# Proportion of intervention village total effect due to spillover within intervention villages
beta_SpW_1/(beta_Ind_1 - beta_SpW_1)











## Spillover from Intervention Villages to Control Villages --------------------






