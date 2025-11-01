# ART Endpoint

# HIV positive dataset
data_hiv_positive <- read.csv("./0_DataPreparation/CleanDataFiles/data_hiv_positive.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))
# HIV positive untreated dataset
data_hiv_positive_untreated <- read.csv("./0_DataPreparation/CleanDataFiles/data_hiv_positive_untreated.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))


data_hiv_positive_untreated_2 <- data_hiv_positive %>%
  dplyr::filter(X2_ik == 0, X3_ik == 0)

nrow(data_hiv_positive_untreated) # Positive no ART
nrow(data_hiv_positive_untreated_2) # Positive no ART and no HTC

# All HIV positive
nrow(data_hiv_positive)
tally(T_k~Y2_ik, data = data_hiv_positive) # Viral suppression
tally(T_k~Y3_ik, data = data_hiv_positive) # Mortality

# HIV positive who did not take any treatment (HTC, VMMC, or ART)
nrow(filter(data_hiv_positive_untreated, 
            endpoint_coverage_mc %in% c("No", "Female"),
            endpoint_coverage_htc == "No",
            endpoint_coverage_onart == "No"))
tally(T_k~Y2_ik, data = filter(data_hiv_positive_untreated, 
                               endpoint_coverage_mc %in% c("No", "Female"),
                               endpoint_coverage_htc == "No",
                               endpoint_coverage_onart == "No")) # Mortality
tally(T_k~Y3_ik, data = filter(data_hiv_positive_untreated, 
                               endpoint_coverage_mc %in% c("No", "Female"),
                               endpoint_coverage_htc == "No",
                               endpoint_coverage_onart == "No")) # Viral suppression



# If we were to use untreated in terms of ART only
data_hiv_positive_untreated_art <- data_hiv_positive %>%
  filter(X3_ik == 0)

nrow(data_hiv_positive_untreated_art)
tally(T_k~Y2_ik, data = data_hiv_positive_untreated_art) # Mortality
tally(T_k~Y3_ik, data = data_hiv_positive_untreated_art) # Viral Suppression

# Look at mortality among HIV positive who are not on ART instead of both HTC and VMMC
# Explain why we're making this decision





# HTC or ART

# HIV positive who did not take any treatment (HTC, or ART)
nrow(filter(data_hiv_positive_untreated, 
            #endpoint_coverage_mc %in% c("No", "Female"),
            endpoint_coverage_htc == "No",
            endpoint_coverage_onart == "No"))
tally(T_k~Y2_ik, data = filter(data_hiv_positive_untreated, 
                               #endpoint_coverage_mc %in% c("No", "Female"),
                               endpoint_coverage_htc == "No",
                               endpoint_coverage_onart == "No")) # Mortality
tally(T_k~Y3_ik, data = filter(data_hiv_positive_untreated, 
                               #endpoint_coverage_mc %in% c("No", "Female"),
                               endpoint_coverage_htc == "No",
                               endpoint_coverage_onart == "No")) # Viral suppression

