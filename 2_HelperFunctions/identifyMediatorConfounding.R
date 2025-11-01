# identifyMediatorConfounding.R

# Script to identify which covariates are associated with a given 
# component, but not the treatment.

identifyMediatorConfounding <- function(myData,
                                        myCluster,
                                        myTreatment,
                                        myComponent,
                                        myCovariates,
                                        pthresh = 0.2,
                                        roundNumber = 2){
  
  # Select only required variables
  modelData <- myData %>%
    dplyr::select(cluster_id = all_of(myCluster), all_of(myTreatment), 
                  all_of(myComponent), all_of(myCovariates)) %>%
    dplyr::filter(!is.na(.data[[myComponent]])) %>%
    dplyr::filter(!is.na(.data[[myTreatment]]))
  
  # Model Component ~ Covariates
  rhs_component <- paste(c(myCovariates), collapse = " + ")
  formula_component <- stats::as.formula(paste(myComponent, "~", rhs_component))
  component_model <- glmer(update(formula_component, . ~ . + (1 | cluster_id)), # Uses exchangeable
                           family = binomial(link = "logit"),
                           data = modelData)

  component_summary <- car::Anova(component_model, type = 3) %>%
    rownames_to_column(var = "ID") %>%
    as_tibble() %>%
    dplyr::select(ID, P = contains("Pr")) %>%
    dplyr::filter(ID != "(Intercept)")
  component_significant <- component_summary %>%
    dplyr::filter(P <= pthresh)
  component_vector <- component_significant$ID
  
  # Model Treatment ~ Covariates
  rhs_treatment <- paste(c(myCovariates), collapse = " + ")
  formula_treatment <- stats::as.formula(paste(myTreatment, "~", rhs_treatment))
  treatment_model <- glmer(update(formula_treatment, . ~ . + (1 | cluster_id)), # Uses exchangeable
                           family = binomial(link = "logit"),
                           data = modelData)
  
  treatment_summary <- car::Anova(treatment_model, type = 3) %>%
    rownames_to_column(var = "ID") %>%
    as_tibble() %>%
    dplyr::select(ID, P = contains("Pr")) %>%
    dplyr::filter(ID != "(Intercept)")
  treatment_significant <- treatment_summary %>%
    dplyr::filter(P <= pthresh)
  treatment_vector <- treatment_significant$ID
  
  # Get list of variables that are significant to the component
  # but NOT the treatment
  # If nothing significant to component, return null
  if(length(component_vector) == 0){
    finalCovariates <- NULL
  } else{ # If at least one thing is significant, remove anything in treatment vector
    finalCovariates <- setdiff(component_vector, treatment_vector)
  }
  
  # Generate summary table for component model
  component_table <- broom.mixed::tidy(component_model, effects = "fixed") %>%
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
    mutate(ICC = performance::icc(component_model, tolerance = 0)$ICC_adjusted[[1]]) %>%
    dplyr::select(-Variance, -SE)
  component_table_or <- component_table %>% # OR version of the table
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    rename(`Estimate (OR)` = Estimate)
  
  # Generate summary table for treatment model
  treatment_table <- broom.mixed::tidy(treatment_model, effects = "fixed") %>%
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
    mutate(ICC = performance::icc(treatment_model, tolerance = 0)$ICC_adjusted[[1]]) %>%
    dplyr::select(-Variance, -SE)
  treatment_table_or <- treatment_table %>% # OR version of the table
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    rename(`Estimate (OR)` = Estimate)
  
  returnList <- list(finalCovariates = tibble(ID = finalCovariates),
                     `Component Covariate` = dplyr::mutate(component_table,
                                                           across(where(is.numeric), ~ round(.x, digits = roundNumber))),
                     `Component Covariate OR` = dplyr::mutate(component_table_or,
                                                              across(where(is.numeric), ~ round(.x, digits = roundNumber))),
                     `Treatment Covariate` = dplyr::mutate(treatment_table,
                                                           across(where(is.numeric), ~ round(.x, digits = roundNumber))),
                     `Treatment Covariate OR` = dplyr::mutate(treatment_table_or,
                                                              across(where(is.numeric), ~ round(.x, digits = roundNumber)))
                     )
  
  # Return vector of covariates
  return(returnList)
}