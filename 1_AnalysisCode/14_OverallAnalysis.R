source("./RequiredPackages.R")

# HIV negative males dataset
data_hiv_negative <- read.csv("./0_DataPreparation/CleanDataFiles/data_hiv_negative.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))

# Create necessary variables indicating if a person received any component
# and proportion of individuals in a village who received any component
model_data <- data_hiv_negative %>%
  dplyr::select(cluster_id, T_k, Y1_ik, gender, X1_ik, X2_ik) %>%
  mutate(Xany_ik = ifelse(gender == "Male",
                       ifelse(X1_ik == 1 | X2_ik == 1, 1, 0),
                       ifelse(gender == "Female", X2_ik, NA)),
         Xany_ik = replace_na(Xany_ik, 0)) %>%
  group_by(cluster_id) %>%
  mutate(any_count = sum(Xany_ik == 1, na.rm = TRUE)) %>%
  ungroup() %>%
  add_count(cluster_id, name = "cluster_size") %>%
  mutate(Zany_k = any_count/cluster_size)

# Number to round to
roundNumber <- 3

# ROUTE A METHODOLOGY ----------------------------------------------------------

# Calculate p1, the cluster-size weight mean across all clusters of Zany_k
p1 <- sum(model_data$Zany_k)/nrow(model_data)

# Calculate Zany_k_bar_X0 for X_ik^any = 0
Zany_k_bar_X0 <- mean(filter(model_data, Xany_ik == 0)$Zany_k)

# Calculate Zany_k_bar_X1 for X_ik^any = 1
Zany_k_bar_X1 <- mean(filter(model_data, Xany_ik == 1)$Zany_k)

# Overall effects model
overall_formula <- stats::as.formula(paste("Y1_ik", "~", "T_k"))
model_overall_glmm <- glmer(update(overall_formula, . ~ . + (1 | cluster_id)),
                            family = binomial(link = "logit"),
                            data = model_data) # All HIV- 

tidy_overall_glmm <- broom.mixed::tidy(model_overall_glmm, effects = "fixed") %>%
  dplyr::select(term, estimate, std.error, p.value) %>%
  mutate(conf.low  = estimate - qnorm(0.975) * std.error,
         conf.high = estimate + qnorm(0.975) * std.error) %>%
  mutate(Model = "GLMM", 
         Term = term,
         Estimate = estimate,
         Lower = conf.low,
         Upper = conf.high,
         `p-value` = p.value,
         Variance = std.error^2,
         SE = std.error) %>%
  dplyr::select(Model, Term, Estimate, Lower, Upper, `p-value`, Variance, SE) %>%
  dplyr::filter(Term != "(Intercept)") %>%
  mutate(ICC = performance::icc(model_overall_glmm, tolerance = 0)$ICC_adjusted[[1]])

overall_effect_glmm <- dplyr::filter(tidy_overall_glmm, Term == "T_k")$Estimate


# Spillover effects model
spillover_formula <- stats::as.formula(paste("Y1_ik ~ T_k + Zany_k + T_k*Zany_k"))
model_spillover_glmm <- glmer(update(spillover_formula, . ~ . + (1 | cluster_id)),
                              family = binomial(link = "logit"),
                              data = dplyr::filter(model_data, 
                                            Xany_ik == 0)) # All UNTREATED HIV- 

tidy_spillover_glmm <- broom.mixed::tidy(model_spillover_glmm, effects = "fixed") %>%
  dplyr::select(term, estimate, std.error, p.value) %>%
  mutate(conf.low  = estimate - qnorm(0.975) * std.error,
         conf.high = estimate + qnorm(0.975) * std.error) %>%
  mutate(Model = "GLMM", 
         Term = term,
         Estimate = estimate,
         Lower = conf.low,
         Upper = conf.high,
         `p-value` = p.value,
         Variance = std.error^2,
         SE = std.error) %>%
  dplyr::select(Model, Term, Estimate, Lower, Upper, `p-value`, Variance, SE) %>%
  dplyr::filter(Term != "(Intercept)") %>%
  mutate(ICC = performance::icc(model_spillover_glmm, tolerance = 0)$ICC_adjusted[[1]])

spillover_effect_glmm <- dplyr::filter(tidy_spillover_glmm, Term == "T_k")$Estimate + 
  dplyr::filter(tidy_spillover_glmm, Term == "T_k:Zany_k")$Estimate*Zany_k_bar_X0

# Individual effects model
individual_formula <- stats::as.formula(paste("Y1_ik ~ T_k + Zany_k + T_k*Zany_k"))
model_individual_glmm <- glmer(update(individual_formula, . ~ . + (1 | cluster_id)),
                               family = binomial(link = "logit"),
                               data = dplyr::filter(model_data, 
                                                   Xany_ik == 1)) # All TREATED HIV- 

tidy_individual_glmm <- broom.mixed::tidy(model_individual_glmm, effects = "fixed") %>%
  dplyr::select(term, estimate, std.error, p.value) %>%
  mutate(conf.low  = estimate - qnorm(0.975) * std.error,
         conf.high = estimate + qnorm(0.975) * std.error) %>%
  mutate(Model = "GLMM", 
         Term = term,
         Estimate = estimate,
         Lower = conf.low,
         Upper = conf.high,
         `p-value` = p.value,
         Variance = std.error^2,
         SE = std.error) %>%
  dplyr::select(Model, Term, Estimate, Lower, Upper, `p-value`, Variance, SE) %>%
  dplyr::filter(Term != "(Intercept)") %>%
  mutate(ICC = performance::icc(model_individual_glmm, tolerance = 0)$ICC_adjusted[[1]])

individual_effect_glmm <- dplyr::filter(tidy_individual_glmm, Term == "T_k")$Estimate + 
  dplyr::filter(tidy_individual_glmm, Term == "T_k:Zany_k")$Estimate*Zany_k_bar_X1

# Format big table that has all results
overall_table <- bind_rows(mutate(tidy_overall_glmm, Model = "Overall GLMM"), 
                           mutate(tidy_spillover_glmm, Model = "Spillover GLMM"), 
                           mutate(tidy_individual_glmm, Model = "Individual GLMM")) %>%
  mutate(or = exp(Estimate),
         or.lower = exp(Lower),
         or.upper = exp(Upper)) %>%
  # Round to pre-specified number
  dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
  mutate(`Log-Odds Estimate (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
  mutate(`OR (95% CI)` = paste0(or, " (", or.lower, ", ", or.upper, ")")) %>%
  dplyr::select(Model, Term, `Log-Odds Estimate (95% CI)`, `OR (95% CI)`,
                `p-value`, Variance, SE, ICC)

# Decomposition in table form
decompositionTable <- tibble(`Estimate Form` = c("Log-Odds", "OR"),
                             Overall = c(overall_effect_glmm, exp(overall_effect_glmm)),
                             Spillover = c(spillover_effect_glmm, exp(spillover_effect_glmm)),
                             Individual = c(individual_effect_glmm, exp(individual_effect_glmm)),
                             `Overall Calculated` = c(p1*individual_effect_glmm + (1-p1)*spillover_effect_glmm,
                                                      exp(p1*individual_effect_glmm + (1-p1)*spillover_effect_glmm))
                             )

# Meta-Data about analysis
overall_methodA_info <- tibble(Information = c("n of HIV- individuals",
                                               "p1 - ptudy proportion of individuals who took at least 1 component",
                                               
                                               "Average Zany_k for X = 0",
                                               "Average Zany_k for X = 1",
                                               
                                               
                                               "X_ik^any = 0",
                                               "X_ik^any = 1",
                                               
                                               
                                               "Overall Effects Formula",
                                               "n for Overall Effects",
                                               
                                               "Spillover Effects Formula",
                                               "n for Spillover Effects",
                                               
                                               "Individual Effects Formula",
                                               "n for Individual Effects"
                                               
                                               ),
                               
                               Value = c(paste0(nrow(model_data)),
                                         paste0(p1),
                                         
                                         paste0(Zany_k_bar_X0),
                                         paste0(Zany_k_bar_X1),
                                         
                                         paste0(nrow(dplyr::filter(model_data, Xany_ik == 0))),
                                         paste0(nrow(dplyr::filter(model_data, Xany_ik == 1))),

                                         paste0(overall_formula),
                                         paste0(nrow(model_data)),
                                         
                                         paste0(spillover_formula),
                                         paste0(nrow(dplyr::filter(model_data, Xany_ik == 0))),
                                         
                                         paste0(individual_formula),
                                         paste0(nrow(dplyr::filter(model_data, Xany_ik == 1)))
                                         )
                               )


# Component Frequency Table
tableData <- model_data %>%
  dplyr::select(Treatment = T_k,
                Component = Xany_ik,
                Outcome = Y1_ik
  ) %>%
  mutate(Treatment = ifelse(Treatment == 1, "Treatment", 
                            ifelse(Treatment == 0, "Control", NA)),
         Outcome = ifelse(Outcome == 1, "Yes", ifelse(Outcome == 0, "No", "Missing")),
         Component = ifelse(Component == 1, "Yes", ifelse(Component == 0, "No", "Missing"))) %>%
  mutate(across(everything(), tidyr::replace_na, "Missing")) %>%
  
  mutate(across(everything(), 
                ~factor(., levels = c("Yes", "No", "Female", "Began study HIV-infected", 
                                      "Control", "Treatment", "Missing")))) %>%
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

# Distribution of proportion coverage
tableDataProp <- model_data %>%
  dplyr::select(Treatment = T_k,
                Component = Zany_k,
                Outcome = Y1_ik
  ) %>%
  mutate(Treatment = ifelse(Treatment == 1, "Treatment", 
                            ifelse(Treatment == 0, "Control", NA)),
         Outcome = ifelse(Outcome == 1, "Yes", ifelse(Outcome == 0, "No", "Missing"))) %>%
  mutate(across(where(is.character), tidyr::replace_na, "Missing")) %>%
  mutate(across(where(is.character), 
                ~factor(., levels = c("Yes", "No", "Female", "Began study HIV-infected", 
                                      "Control", "Treatment", "Total", "Missing")))) %>%
  group_by(Treatment, Outcome) %>%
  dplyr::summarize(Mean = round(mean(Component), roundNumber),
                   Min = round(min(Component), roundNumber),
                   Max = round(max(Component), roundNumber),
                   SD = round(sd(Component), roundNumber),
                   #Variance = round(sd(Component)^2, roundNumber),
                   Count = n(),
                   .groups = "drop")

tableDataTotalProp <- model_data %>%
  dplyr::select(Treatment = T_k,
                Component = Zany_k,
                Outcome = Y1_ik
  ) %>%
  mutate(Treatment = ifelse(Treatment == 1, "Treatment", 
                            ifelse(Treatment == 0, "Control", NA)),
         Outcome = ifelse(is.na(Outcome), "Missing", "Total")) %>%
  mutate(across(where(is.character), tidyr::replace_na, "Missing")) %>%
  mutate(across(where(is.character), 
                ~factor(., levels = c("Yes", "No", "Female", "Began study HIV-infected", 
                                      "Control", "Treatment", "Total", "Missing")))) %>%
  group_by(Treatment, Outcome) %>%
  dplyr::summarize(Mean = round(mean(Component), roundNumber),
                   Min = round(min(Component), roundNumber),
                   Max = round(max(Component), roundNumber),
                   SD = round(sd(Component), roundNumber),
                   #Variance = round(sd(Component)^2, roundNumber),
                   Count = n(),
                   .groups = "drop") %>%
  dplyr::filter(Outcome != "Missing")

tableOutputProp <- bind_rows(tableDataProp, tableDataTotalProp) %>%
  arrange(Treatment,
          factor(Outcome, levels = c("Yes", "No", "Total", "Missing")))

# Final output list for Method A Overall Effects
method_a_results_unadjusted <- list(`Component Frequencies` = tableOutput,
                                    `Proportion Distribution` = tableOutputProp,
                                    
                                    `Meta Data` = overall_methodA_info,
                                    `All Estimates` = overall_table,
                                    `Decomposition` = decompositionTable
                                    )

write_xlsx(method_a_results_unadjusted, 
           "./3_Results/35_ResultsOverall/method_a_overall_results_unadjusted.xlsx")


# ROUTE B METHODOLOGY ----------------------------------------------------------






