# Identify Confounders

identifyConfounding <- function(myData,
                                myCluster,
                                myOutcome,
                                myTreatment,
                                myCovariates,
                                pthresh = 0.2,
                                roundNumber = 2){
  
  # Select only required variables
  modelData <- myData %>%
    dplyr::select(cluster_id = all_of(myCluster), all_of(myOutcome),
                  all_of(myTreatment), all_of(myCovariates)) %>%
    dplyr::filter(!is.na(.data[[myTreatment]])) %>%
    dplyr::filter(!is.na(.data[[myOutcome]]))
  
  # Model Component ~ Covariates
  rhs_full <- paste(c(myTreatment, myCovariates), collapse = " + ")
  formula_full <- stats::as.formula(paste(myOutcome, "~", rhs_full))
  full_model <- glmer(update(formula_full, . ~ . + (1 | cluster_id)), # Uses exchangeable
                      family = binomial(link = "logit"),
                      data = modelData)
  
  
  # Model selection
  model_step_list <- backward_glmer_p(myFormulaGLMM = formula_full, 
                                      myDataGLMM = modelData, 
                                      clusterID = "cluster_id",
                                      required_terms = c("T_k"),
                                      pCutoff = pthresh,
                                      selectionCriteria = "p")
  
  # Model after stepping through
  step_model <- model_step_list[[1]]
  
  # Final list of covariates
  finalCovariates <- setdiff(model_step_list[[3]], myTreatment)

  # Generate summary table for component model
  full_table <- broom.mixed::tidy(full_model, effects = "fixed") %>%
    dplyr::select(term, estimate, std.error, p.value) %>%
    mutate(conf.low  = estimate - qnorm(0.975) * std.error,
           conf.high = estimate + qnorm(0.975) * std.error) %>%
    mutate(Term = term,
           Estimate = estimate,
           Lower = conf.low,
           Upper = conf.high,
           `p-value` = p.value,
           Variance = std.error^2,
           SE = std.error) %>%
    dplyr::select(Term, Estimate, Lower, Upper, `p-value`, Variance, SE) %>%
    dplyr::filter(Term != "(Intercept)") %>%
    mutate(ICC = performance::icc(full_model, tolerance = 0)$ICC_adjusted[[1]]) %>%
    dplyr::select(-Variance, -SE)
  full_table_or <- full_table %>% # OR version of the table
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    rename(`Estimate (OR)` = Estimate)
  
  # Generate summary table for treatment model
  step_table <- broom.mixed::tidy(step_model, effects = "fixed") %>%
    dplyr::select(term, estimate, std.error, p.value) %>%
    mutate(conf.low  = estimate - qnorm(0.975) * std.error,
           conf.high = estimate + qnorm(0.975) * std.error) %>%
    mutate(Term = term,
           Estimate = estimate,
           Lower = conf.low,
           Upper = conf.high,
           `p-value` = p.value,
           Variance = std.error^2,
           SE = std.error) %>%
    dplyr::select(Term, Estimate, Lower, Upper, `p-value`, Variance, SE) %>%
    dplyr::filter(Term != "(Intercept)") %>%
    mutate(ICC = performance::icc(step_model, tolerance = 0)$ICC_adjusted[[1]]) %>%
    dplyr::select(-Variance, -SE)
  step_table_or <- step_table %>% # OR version of the table
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    rename(`Estimate (OR)` = Estimate)
  

  returnList <- list(finalCovariates = tibble(ID = finalCovariates),
                     `Full Model` = dplyr::mutate(full_table,
                                                  across(where(is.numeric), ~ round(.x, digits = roundNumber))),
                     `Full Model OR` = dplyr::mutate(full_table_or,
                                                     across(where(is.numeric), ~ round(.x, digits = roundNumber))),
                     `Reduced Model` = dplyr::mutate(step_table,
                                                     across(where(is.numeric), ~ round(.x, digits = roundNumber))),
                     `Reduced Model OR` = dplyr::mutate(step_table_or,
                                                        across(where(is.numeric), ~ round(.x, digits = roundNumber)))
  )
  
  # Return vector of covariates
  return(returnList)
}