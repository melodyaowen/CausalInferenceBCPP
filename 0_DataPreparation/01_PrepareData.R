# 01_PrepareData.R
# This script reads in the original datasets from the BCPP data analysis
# and prepares them so that they are ready for analysis.
# It saves the master prepared dataset in the folder.
# Only run when you're needing to tweak the main dataset, otherwise can skip 
# to the next script where it just reads it

# 0. Load necessary packages ---------------------------------------------------
source("./RequiredPackages.R")

# 1. Read in the SAS BCPP datasets ---------------------------------------------

# Set the path to the BCPP datasets
bcpp_y1_path <- "~/Desktop/3. Causal Inference Mediation/BCPP data/BCPP data/longyr1_20samp_open.sas7bdat"
bcpp_y2_path <- "~/Desktop/3. Causal Inference Mediation/BCPP data/BCPP data/longyr2_20samp_open.sas7bdat"
bcpp_y3_path <- "~/Desktop/3. Causal Inference Mediation/BCPP data/BCPP data/longyr3_20samp_open.sas7bdat"

# Read in the datasets
data_y1 <- read_sas(bcpp_y1_path)
data_y2 <- read_sas(bcpp_y2_path)
data_y3 <- read_sas(bcpp_y3_path)

# 2. Choose variables that we want to keep -------------------------------------

# Create a dataframe for each set of column names across the 3 datasets
names_y1 <- tibble(colname = sort(colnames(data_y1)), dataset = "data_y1")
names_y2 <- tibble(colname = sort(colnames(data_y2)), dataset = "data_y2")
names_y3 <- tibble(colname = sort(colnames(data_y3)), dataset = "data_y3")

# Master dataframe of all of the columns available across the 3 datasets
variable_names_all <- bind_rows(names_y1, names_y2, names_y3) %>%
  mutate(present = colname) %>%
  pivot_wider(names_from = dataset, values_from = present) %>%
  arrange(colname)

# Dataframe of all of the columns that we want for our master analysis dataset
variable_names_keep <- variable_names_all %>%
  dplyr::filter(colname %in% c("de_subj_idC", # Subject identifier
                               "community", # Community identifier
                               
                               "random_arm", # Randomization arm (Treatment Assignment)
                               "hiv_status_current", # HIV status at study start
                               
                               "gender", # Gender
                               "circumcised", # Male circumcision
                               "circumcision_days",
                               
                               # "Coverage Endpoint"
                               "endpoint_coverage_htc", # Coverage endpoint: HIV-tested or diagnosed HIV+
                               "endpoint_coverage_mc", # Coverage endpoint: male circumcision
                               "endpoint_coverage_onart", # Coverage endpoint: on ARVs aong all HIV+
                               "endpoint_coverage_vlsupp", # Coverage endpoint: virally suppressed on ARVs among HIV+
                               
                               # ART Specific Info
                               "arv_status_current", # Current ARV status
                               
                               # Outcomes
                               "endpoint_seroconvert", # HIV seroconversion at end of 3 years
                               "endpoint_death", # Death by study completion
                               
                               #"death_group_cause", # Checked these on 10/22,
                               #"death_primary_cause", # no other data to be gained for mortality
                               #"death_days",

                               # Individual covariates
                               "gender", # Gender
                               "age_at_interview", # Age at interview
                               "marital_status", # Marital status
                               "education", # Highest education level
                               "employment_status", # employment status
                               "monthly_income", # Monthly income
                               "partners_12mos", # Number of sexual partners in past 12 months
                               "alcohol", # How often used alcohol in past 12 months
                               
                               "length_residence", # How long lived in this community
                               "partners_lifetime", # Total number of sexual partners in lifetime
                               "condom_lastsex", # Condom use during most recent sex
                               "exchange_12mos", # Transactional sex
                               "overall_access", # If I need medical care, I can see professional
                               
                               # Cluster covariates
                               "prob_housing", # Neighborhood problem: housing
                               "prob_schools", # Neighborhood problem: schools
                               "prob_hiv", # Neighborhood problem: HIV
                               "prob_alcohol_drug", # Neighborhood problem: alcohol/drugs
                               "prob_healthcare" # Neighborhood problem: healthcare"
    )
  )

# Keeping subject ID the same name so I can match the data
data_y1_renamed <- data_y1 %>% rename_with(~ paste0(., "_y1"), -de_subj_idC)
data_y2_renamed <- data_y2 %>% rename_with(~ paste0(., "_y2"), -de_subj_idC)
data_y3_renamed <- data_y3 %>% rename_with(~ paste0(., "_y3"), -de_subj_idC)

# 3. Combine datasets and clean/format -----------------------------------------

# Master list of all desired variables
all_variables_list <- tibble(vars = c(paste0(variable_names_keep$data_y1, "_y1"),
                                      paste0(variable_names_keep$data_y2, "_y2"),
                                      paste0(variable_names_keep$data_y3, "_y3"))) %>%
  filter(vars != "de_subj_idC_y1", vars != "de_subj_idC_y2", vars != "de_subj_idC_y3") %>%
  arrange(vars) %>%
  filter(!str_detect(vars, "NA_"))

# Combining the data into a big full dataset
data_full <- full_join(data_y1_renamed, data_y2_renamed, by = "de_subj_idC") %>%
  full_join(data_y3_renamed, by = "de_subj_idC") %>%
  dplyr::select(de_subj_idC, all_variables_list$vars) %>%
  
  # Formatting issues such as blanks etc.
  mutate(across(where(~ !is.numeric(.)), ~ na_if(., ""))) %>% # NA if there is a blank
  mutate(across(where(is.character), str_trim)) %>% # Trim any spaces
  
  # Start going through and removing redundant variables
  
  # Treatment Arm
  mutate(random_arm = ifelse(!is.na(random_arm_y1), random_arm_y1,
                             ifelse(!is.na(random_arm_y2), random_arm_y2,
                                    random_arm_y3))) %>%
  dplyr::select(-random_arm_y1, -random_arm_y2, -random_arm_y3) %>%
  
  # Cluster ID
  mutate(community = ifelse(!is.na(community_y1), community_y1,
                            ifelse(!is.na(community_y2), community_y2,
                                   community_y3))) %>%
  dplyr::select(-community_y1, -community_y2, -community_y3) %>%
  
  # Geographic location
  mutate(location = community) %>%
  
  # Age
  mutate(age = ifelse(!is.na(age_at_interview_y1), age_at_interview_y1,
                            ifelse(!is.na(age_at_interview_y2), age_at_interview_y2 - 1,
                                   ifelse(!is.na(age_at_interview_y3), age_at_interview_y3 - 2, NA)))) %>%
  dplyr::select(-age_at_interview_y1, -age_at_interview_y2, -age_at_interview_y3) %>%
  
  # Gender
  mutate(gender = ifelse(!is.na(gender_y1), gender_y1,
                         ifelse(!is.na(gender_y2), gender_y2,
                                ifelse(!is.na(gender_y3), gender_y3, NA)))) %>%
  dplyr::select(-gender_y1, -gender_y2, -gender_y3) %>%
  
  # HIV status at baseline
  mutate(hiv_status_current = ifelse(!is.na(hiv_status_current_y1), hiv_status_current_y1,
                                     ifelse(!is.na(hiv_status_current_y2), hiv_status_current_y2,
                                            ifelse(!is.na(hiv_status_current_y3), hiv_status_current_y3, NA)))) %>%
  mutate(hiv_status_current = case_when(hiv_status_current == "Refused HIV testing" & hiv_status_current_y3 == "HIV-uninfected" ~ "HIV-uninfected",
                                        hiv_status_current == "Refused HIV testing" & is.na(hiv_status_current_y3) & hiv_status_current_y2 == "HIV-uninfected" ~ "HIV-uninfected",
                                        TRUE ~ hiv_status_current  # default keeps original
                                        )
         ) %>%
  dplyr::select(-hiv_status_current_y1, -hiv_status_current_y2, -hiv_status_current_y3) %>%
  
  # On ARV at any point
  mutate(across(contains("arv_status"),~ ifelse(is.na(.) | . == "", "0", .))) %>%
  mutate(arv_status_current = ifelse(arv_status_current_y1 == "On ARVs" | arv_status_current_y2 == "On ARVs" | arv_status_current_y3 == "On ARVs", "On ARVs", 
                                     ifelse(arv_status_current_y1 == "ARV defaulter" | arv_status_current_y2 == "ARV defaulter" | arv_status_current_y3 == "ARV defaulter", "ARV defaulter",
                                            ifelse(arv_status_current_y1 == "ARV naive" | arv_status_current_y2 == "ARV naive" | arv_status_current_y3 == "ARV naive", "ARV naive", NA)))) %>%
  dplyr::select(-arv_status_current_y1, -arv_status_current_y2, -arv_status_current_y3) %>%
  
  # Generally circumcised, prioritize year 1 because this is not the
  # indicator for VMMC, just MC
  mutate(across(contains("circumcised"),~ ifelse(is.na(.) | . == "", "0", .))) %>%
  mutate(circumcised = ifelse(gender == "F", "Female",
                              ifelse(circumcised_y1 != "0", circumcised_y1,
                                     ifelse(circumcised_y2 != "0", circumcised_y2,
                                            ifelse(circumcised_y3 != "0", circumcised_y3, NA))))) %>%
  dplyr::select(-circumcised_y1, -circumcised_y2, -circumcised_y3) %>%
  
  
  # Circumcision Days
  mutate(circumcision_days = ifelse(!is.na(circumcision_days_y1), circumcision_days_y1,
                                    ifelse(!is.na(circumcision_days_y2), circumcision_days_y2,
                                           ifelse(!is.na(circumcision_days_y3), circumcision_days_y3, NA)))) %>%
  dplyr::select(-circumcision_days_y1, -circumcision_days_y2, -circumcision_days_y3) %>%
  
  # Received VMMC at any point
  mutate(across(contains("endpoint_coverage_mc"),~ ifelse(is.na(.) | . == "", "0", .))) %>%
  mutate(endpoint_coverage_mc = ifelse(gender == "F", "Female",
                                       ifelse(endpoint_coverage_mc_y1 == "Yes" | endpoint_coverage_mc_y2 == "Yes" | endpoint_coverage_mc_y3 == "Yes", "Yes",
                                              ifelse(endpoint_coverage_mc_y1 == "No" | endpoint_coverage_mc_y2 == "No" | endpoint_coverage_mc_y3 == "No", "No", NA)))) %>%
  dplyr::select(-endpoint_coverage_mc_y1, -endpoint_coverage_mc_y2, -endpoint_coverage_mc_y3) %>%
  
  # Update VMMC based on circumcised - if they were circumcised after the study start, then it counts as VMMC
  mutate(endpoint_coverage_mc = case_when(circumcised == "Yes" & circumcision_days >= 0 ~ "Yes",
                                              TRUE ~ endpoint_coverage_mc)) %>%
  
  # Received HTC at any point, only one variable at Y1
  mutate(endpoint_coverage_htc = endpoint_coverage_htc_y1) %>%
  dplyr::select(-endpoint_coverage_htc_y1) %>%
  
  # Received ART at any point
  mutate(across(contains("endpoint_coverage_onart"),~ ifelse(is.na(.) | . == "", "0", .))) %>%
  mutate(endpoint_coverage_onart = ifelse(endpoint_coverage_onart_y1 == "Yes" | endpoint_coverage_onart_y2 == "Yes" | endpoint_coverage_onart_y3 == "Yes", "Yes",
                                       ifelse(endpoint_coverage_onart_y1 == "No" | endpoint_coverage_onart_y2 == "No" | endpoint_coverage_onart_y3 == "No", "No", NA))) %>%
  dplyr::select(-endpoint_coverage_onart_y1, -endpoint_coverage_onart_y2, -endpoint_coverage_onart_y3) %>%
  
  # Virally supressed at all during study
  mutate(across(contains("endpoint_coverage_vlsupp"),~ ifelse(is.na(.) | . == "", "0", .))) %>%
  mutate(endpoint_coverage_vlsupp = ifelse(endpoint_coverage_vlsupp_y1 == "Yes" | endpoint_coverage_vlsupp_y2 == "Yes" | endpoint_coverage_vlsupp_y3 == "Yes", "Yes",
                                           ifelse(endpoint_coverage_vlsupp_y1 == "No" | endpoint_coverage_vlsupp_y2 == "No" | endpoint_coverage_vlsupp_y3 == "No", "No", NA))) %>%
  dplyr::select(-endpoint_coverage_vlsupp_y1, -endpoint_coverage_vlsupp_y2, -endpoint_coverage_vlsupp_y3) %>%
  
  # Outcome HIV seroconversion, only one variable at Y1
  mutate(endpoint_seroconvert = endpoint_seroconvert_y1) %>%
  dplyr::select(-endpoint_seroconvert_y1) %>%
  
  # Outcome death, only one variable at Y1
  mutate(endpoint_death = endpoint_death_y1) %>%
  dplyr::select(-endpoint_death_y1) %>%
  
  # Overall access to healthcare
  mutate(overall_access = overall_access_y1) %>%
  dplyr::select(-overall_access_y1) %>%
  
  # Transactional sex at any point
  mutate(across(contains("exchange_12mos"),~ ifelse(is.na(.) | . == "", "0", .))) %>%
  mutate(exchange_12mos = ifelse(exchange_12mos_y1 == "Yes" | exchange_12mos_y2 == "Yes" | exchange_12mos_y3 == "Yes", "Yes",
                                          ifelse(exchange_12mos_y1 == "No" | exchange_12mos_y2 == "No" | exchange_12mos_y3 == "No", "No", NA))) %>%
  dplyr::select(-exchange_12mos_y1, -exchange_12mos_y2, -exchange_12mos_y3) %>%
  
  # Did not use a condom at least once in a past interaction
  mutate(across(contains("condom_lastsex"),~ ifelse(is.na(.) | . == "", "0", .))) %>%
  mutate(condom_lastsex = ifelse(condom_lastsex_y1 == "No" | condom_lastsex_y2 == "No" | condom_lastsex_y3 == "No", "No",
                                 ifelse(condom_lastsex_y1 == "Yes" | condom_lastsex_y2 == "Yes" | condom_lastsex_y3 == "Yes", "Yes", NA))) %>%
  dplyr::select(-condom_lastsex_y1, -condom_lastsex_y2, -condom_lastsex_y3) %>%
  
  # Partners in lifetime, take the maximum across 3 years
  mutate(partners_lifetime = pmax(partners_lifetime_y1, partners_lifetime_y2, 
                                  partners_lifetime_y3, na.rm = TRUE)) %>%
  dplyr::select(-partners_lifetime_y1, -partners_lifetime_y2, -partners_lifetime_y3) %>%
  
  # Partners in past 12 months, taking the max
  mutate(across(contains("partners_12mos"), 
                ~ case_when(. == "1 partner" ~ 1L,
                            . == "2 partners" ~ 2L,
                            . == "3 partners" ~ 3L,
                            . == "4 or more partners" ~ 4L,
                            . == "None" ~ 0L,
                            TRUE ~ NA_integer_)
                )
         ) %>%
  mutate(partners_12mos = pmax(partners_12mos_y1, partners_12mos_y2, 
                               partners_12mos_y3, na.rm = TRUE)) %>%
  dplyr::select(-partners_12mos_y1, -partners_12mos_y2, -partners_12mos_y3) %>%
  
  # Length residence
  mutate(length_residence = ifelse(!is.na(length_residence_y1), length_residence_y1,
                                    ifelse(!is.na(length_residence_y2), length_residence_y2,
                                           ifelse(!is.na(length_residence_y3), length_residence_y3, NA)))) %>%
  dplyr::select(-length_residence_y1, -length_residence_y2, -length_residence_y3) %>%
  
  # Monthly income
  mutate(monthly_income = ifelse(!is.na(monthly_income_y1), monthly_income_y1,
                                 ifelse(!is.na(monthly_income_y2), monthly_income_y2,
                                        ifelse(!is.na(monthly_income_y3), monthly_income_y3, NA)))) %>%
  dplyr::select(-monthly_income_y1, -monthly_income_y2, -monthly_income_y3) %>%
  
  # Employment status
  mutate(employment_status = ifelse(!is.na(employment_status_y1), employment_status_y1,
                                    ifelse(!is.na(employment_status_y2), employment_status_y2,
                                           ifelse(!is.na(employment_status_y3), employment_status_y3, NA)))) %>%
  dplyr::select(-employment_status_y1, -employment_status_y2, -employment_status_y3) %>%
  
  # Education
  mutate(education = ifelse(!is.na(education_y1), education_y1,
                            ifelse(!is.na(education_y2), education_y2,
                                   ifelse(!is.na(education_y3), education_y3, NA)))) %>%
  dplyr::select(-education_y1, -education_y2, -education_y3) %>%
  
  # Marital Status
  mutate(marital_status = ifelse(!is.na(marital_status_y1), marital_status_y1,
                                 ifelse(!is.na(marital_status_y2), marital_status_y2,
                                        ifelse(!is.na(marital_status_y3), marital_status_y3, NA)))) %>%
  dplyr::select(-marital_status_y1, -marital_status_y2, -marital_status_y3) %>%
  
  # Alcohol consumption in the past 12 months
  mutate(alcohol_12mos = alcohol_y1) %>%
  dplyr::select(-alcohol_y1) %>%
  
  # Weekly alcohol consumption
  mutate(alcohol_weekly = case_when(alcohol_12mos == "2 to 3 times a week" ~ "3",
                                    alcohol_12mos == "Less then once a week" ~ "1",
                                    alcohol_12mos == "more than 3 times a week" ~ "4",
                                    alcohol_12mos == "Never" ~ "0",
                                    alcohol_12mos == "Once a week" ~ "2",
                                    TRUE ~ NA_character_),
         alcohol_weekly = as.numeric(alcohol_weekly)
         ) %>%
  
  # Prob alcohol/drug in the community
  mutate(prob_alcohol_drug = ifelse(!is.na(prob_alcohol_drug_y1), prob_alcohol_drug_y1,
                                 ifelse(!is.na(prob_alcohol_drug_y2), prob_alcohol_drug_y2, NA))) %>%
  dplyr::select(-prob_alcohol_drug_y1, -prob_alcohol_drug_y2) %>%
  
  # Prob healthcare in the community
  mutate(prob_healthcare = ifelse(!is.na(prob_healthcare_y1), prob_healthcare_y1,
                                    ifelse(!is.na(prob_healthcare_y2), prob_healthcare_y2, NA))) %>%
  dplyr::select(-prob_healthcare_y1, -prob_healthcare_y2) %>%
  
  # Prob HIV in the community
  mutate(prob_hiv = ifelse(!is.na(prob_hiv_y1), prob_hiv_y1,
                           ifelse(!is.na(prob_hiv_y2), prob_hiv_y2, NA))) %>%
  dplyr::select(-prob_hiv_y1, -prob_hiv_y2) %>%
  
  # Prob schools in the community
  mutate(prob_schools = ifelse(!is.na(prob_schools_y1), prob_schools_y1,
                               ifelse(!is.na(prob_schools_y2), prob_schools_y2, NA))) %>%
  dplyr::select(-prob_schools_y1, -prob_schools_y2) %>%
  
  # Prob housing in the community
  mutate(prob_housing = ifelse(!is.na(prob_housing_y1), prob_housing_y1,
                               ifelse(!is.na(prob_housing_y2), prob_housing_y2, NA))) %>%
  dplyr::select(-prob_housing_y1, -prob_housing_y2) %>%

  # relocate important variables
  relocate(de_subj_idC, community, random_arm, contains("current"), 
           contains("endpoint"), gender, age) %>%
  
  # New subject ID and cluster ID
  arrange(community, de_subj_idC) %>%
  mutate(subject_id = row_number()) %>%
  group_by(community) %>%
  mutate(cluster_id = cur_group_id()) %>%
  mutate(subject_cluster_id = row_number()) %>%
  ungroup() %>%
  add_count(cluster_id, name = "cluster_size") %>%
  relocate(subject_id, subject_cluster_id, cluster_id, cluster_size) %>%
  
  # Now start to clean the data and update the levels
  mutate(gender = case_when(gender == "F" ~ "Female",
                            gender == "M" ~ "Male",
                            TRUE ~ NA_character_)) %>%
  
  mutate(circumcised = ifelse(gender == "Female", "Female", circumcised)) %>%
  mutate(endpoint_coverage_mc = ifelse(gender == "Female", "Female", endpoint_coverage_mc)) %>%
  
  # If they began study HIV positive, then note it in the seroconversion var
  mutate(endpoint_seroconvert = ifelse(hiv_status_current == "HIV-infected" & 
                                         is.na(endpoint_seroconvert), 
                                       "Began study HIV-infected", endpoint_seroconvert)) %>%
  
  # Creating variables for modeling that are coded as numerics instead of characters
  mutate(T_k = ifelse(random_arm == "Intervention", 1, 0)) %>%
  mutate(X1_ik = ifelse(endpoint_coverage_mc == "Yes", 1,
                        ifelse(endpoint_coverage_mc == "No", 0, NA))) %>%
  mutate(X2_ik = ifelse(endpoint_coverage_htc == "Yes", 1,
                        ifelse(endpoint_coverage_htc == "No", 0, NA))) %>%
  mutate(X3_ik = ifelse(endpoint_coverage_onart == "Yes", 1,
                        ifelse(endpoint_coverage_onart == "No", 0, NA))) %>%
  mutate(Xany_ik = ifelse(gender == "Male" & hiv_status_current == "HIV-uninfected",
                          ifelse(X1_ik == 1 | X2_ik == 1, 1, 0),
                          ifelse(gender == "Female" & hiv_status_current == "HIV-uninfected", 
                                 X2_ik, NA)),
         Xany_ik = replace_na(Xany_ik, 0)) %>%
  mutate(Xany_ik = ifelse(hiv_status_current == "HIV-infected",
                          ifelse(X3_ik == 1, 1, 0), Xany_ik),
         Xany_ik = replace_na(Xany_ik, 0)) %>%
  
  mutate(Y1_ik = ifelse(endpoint_seroconvert == "Yes", 1, 
                        ifelse(endpoint_seroconvert == "No", 0, NA))) %>% # HIV seroconversion endpoint
  mutate(Y2_ik = ifelse(endpoint_death == "Yes", 1, 0)) %>% # Death endpoint
  mutate(Y3_ik = ifelse(endpoint_coverage_vlsupp == "Yes", 1, 
                        ifelse(endpoint_coverage_vlsupp == "No", 0, NA))) %>% # Viral suppression endpoint
  
  mutate(endpoint_coverage_any = ifelse(hiv_status_current == "HIV-infected",
                                        "HIV-infected", 
                                        ifelse(Xany_ik == 1, "Yes",
                                               ifelse(Xany_ik == 0, "No", NA)))) %>%
  
  # Count variables
  group_by(cluster_id) %>%
  
  # Counts in each cluster
  mutate(male_count = sum(gender == "Male", na.rm = TRUE),
         began_hiv_infected_count = sum(hiv_status_current == "HIV-infected", na.rm = TRUE),
         began_hiv_uninfected_count = sum(hiv_status_current == "HIV-uninfected", na.rm = TRUE),
         refused_hiv_testing_count = sum(hiv_status_current == "Refused HIV testing", na.rm = TRUE),
         vmmc_count = sum(endpoint_coverage_mc == "Yes", na.rm = TRUE),
         htc_count = sum(endpoint_coverage_htc == "Yes", na.rm = TRUE),
         art_count = sum(endpoint_coverage_onart == "Yes", na.rm = TRUE),
         
         any_count_hivpos = sum(Xany_ik == 1 & hiv_status_current == "HIV-infected", na.rm = TRUE),
         any_count_hivneg = sum(Xany_ik == 1 & hiv_status_current == "HIV-uninfected", na.rm = TRUE),
         
         vlsupp_count = sum(endpoint_coverage_vlsupp == "Yes", na.rm = TRUE)
  ) %>%
  ungroup() %>%
  
  # Cluster proportions
  mutate(Z1_k = vmmc_count/male_count, # Proportion of men VMMC in cluster
         Z2_k = htc_count/cluster_size, # Proportion of subjects HTC in cluster
         Z3_k = art_count/began_hiv_infected_count, # Proportion of HIV+ on ART in cluster
         
         Zany_k_hivpos = any_count_hivpos/began_hiv_infected_count,
         Zany_k_hivneg = any_count_hivneg/began_hiv_uninfected_count,
         
         prop_male = male_count/cluster_size, # Proportion of males in cluster
         prop_vlsupp = vlsupp_count/began_hiv_infected_count, # Proportion viral suppressed among HIV+ in cluster
         prop_began_infected = began_hiv_infected_count/cluster_size, # Proportion of HIV+ at baseline in cluster
         hiv_refused_testing_prop = refused_hiv_testing_count/cluster_size # Proportion refused testing at baseline in cluster
         ) %>%
  dplyr::select(-ends_with("_count"))

# 4. Create subsets for each analysis ------------------------------------------

# All HIV negative individuals
data_hiv_negative <- data_full %>%
  filter(hiv_status_current == "HIV-uninfected")

# All HIV negative males
data_hiv_negative_males <- data_full %>%
  filter(hiv_status_current == "HIV-uninfected", gender == "Male")

# All HIV negative individuals untreated
data_hiv_negative_untreated <- data_full %>%
  filter(hiv_status_current == "HIV-uninfected",
         endpoint_coverage_mc %in% c("No", "Female"),
         endpoint_coverage_htc == "No")

# endpoint_coverage_mc original
# Female     No    Yes   <NA> 
#   8340   1793   1514   1484 
# 4 more "Yes" cases were gotten from using circumcision days data

# All HIV positive individuals
data_hiv_positive <- data_full %>%
  filter(hiv_status_current == "HIV-infected")

# All HIV positive individuals untreated
data_hiv_positive_untreated <- data_full %>%
  filter(hiv_status_current == "HIV-infected",
         endpoint_coverage_onart == "No")

# 5. Save name data and main dataset -------------------------------------------

# Save the main 
write.csv(data_full, "./0_DataPreparation/CleanDataFiles/data_full.csv")

# Save all HIV negative individuals dataset
write.csv(data_hiv_negative, "./0_DataPreparation/CleanDataFiles/data_hiv_negative.csv")

# Save all HIV negative males dataset
write.csv(data_hiv_negative_males, "./0_DataPreparation/CleanDataFiles/data_hiv_negative_males.csv")

# Save all HIV negative individuals untreated
write.csv(data_hiv_negative_untreated, "./0_DataPreparation/CleanDataFiles/data_hiv_negative_untreated.csv")

# Save all HIV positive individuals
write.csv(data_hiv_positive, "./0_DataPreparation/CleanDataFiles/data_hiv_positive.csv")

# Save all HIV positive untreated individuals
write.csv(data_hiv_positive_untreated, "./0_DataPreparation/CleanDataFiles/data_hiv_positive_untreated.csv")

# Save name data
write.csv(variable_names_all, "./0_DataPreparation/CleanDataFiles/variable_names_all.csv")




