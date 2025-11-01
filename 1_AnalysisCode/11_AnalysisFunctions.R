# 11_AnalysisFunctions.R
# Main functions needed to run a full analysis for a given treatment component

# 0. Load necessary packages ---------------------------------------------------
source("./RequiredPackages.R")
source("./2_HelperFunctions/calcIEandPM.R")
source("./2_HelperFunctions/backwardsGLMM.R")

# 1. Analysis Function for Mediation -------------------------------------------

# General function to get total, direct, indirect effects and proportion mediated
runMediationAnalysis <- function(roundNumber, # Number of significant digits desired
                                 myData, # Dataset
                                 mySubjectID, # Subject ID name in dataset
                                 myClusterID, # Cluster ID name in dataset
                                 myTreatment, # Treatment name in dataset
                                 myComponent, # Component name in dataset
                                 myOutcome, # Outcome name in dataset
                                 modelSelect = "No",
                                 myCovariates, # Vector of covariate names in dataset
                                 myInteractions, # Interactions desired
                                 myForcedTerms = NULL, # Terms that must remain in the model
                                 myCutoff = NULL, # P-value cutoff for model selection
                                 mySelectionCriteria = "p"
                                 ){
  
  
  
  # Define model formulas for each sub-analysis
  ## Total effects formula
  rhs_total <- paste(c(myTreatment, myCovariates, myInteractions), collapse = " + ")
  formula_total <- stats::as.formula(paste(myOutcome, "~", rhs_total))
  ## Direct effects formula
  rhs_direct <- paste(c(myTreatment, myComponent, myCovariates, myInteractions), collapse = " + ")
  formula_direct <- stats::as.formula(paste(myOutcome, "~", rhs_direct))
  
  # Updating model data so that names are consistent
  modelData <- myData %>%
    dplyr::select(subject_id = all_of(mySubjectID),
                  cluster_id = all_of(myClusterID),
                  all_of(myTreatment), 
                  all_of(myComponent), 
                  all_of(myOutcome), 
                  all_of(myCovariates))
  
  
  
  # This is the dataset that excludes people who do not have all of the
  # treatment, outcome, and mediator.
  # Then print how many observations are dropped in the models due to 
  # missing either outcome, treatment, or component
  mf_complete <- model.frame(formula_direct, data = modelData, na.action = na.omit)
  usedData <- modelData[rownames(mf_complete), , drop = FALSE]
  
  cat("\nThere are", nrow(usedData), "out of", nrow(myData), 
      "observations used in the models.\n", 
      nrow(myData) - nrow(usedData), "removed due to missing data\n\n")
  
  
  # FIT BOTH TOTAL AND DIRECT EFFECTS MODELS -----------------------------------
  
  
  # If there are no covariates or interactions, just run the model plainly
  if((is.null(myCovariates) & is.null(myInteractions)) | modelSelect == "No"){
    
    cat("Fitting model with no variable selection.\n\n")
    
    ## Total GLMM unadjusted
    model_total_glmm <- glmer(update(formula_total, . ~ . + (1 | cluster_id)), # Uses exchangeable
                              family = binomial(link = "logit"),
                              data = usedData)
    
    length_temp <- max(length(c(myCovariates, myInteractions)))
    total_variables_frame <- tibble(
      `Inputted Variables` = `length<-`(c(myCovariates, myInteractions), length_temp),
      `Removed Variables`  = `length<-`(c(NA), length_temp),
      `Final Covariates`   = `length<-`(c(NA), length_temp)
    )
    
    formula_total_updated <- formula_total
    
    ## Direct GLMM unadjusted
    model_direct_glmm <- glmer(update(formula_direct, . ~ . + (1 | cluster_id)), # Uses exchangeable
                               family = binomial(link = "logit"),
                               data = usedData)
    
    formula_direct_updated <- formula_direct
    
    # Case when we have covariates or interactions
  } else if((!is.null(myCovariates) | !is.null(myInteractions)) & modelSelect == "Yes"){
    
    if(mySelectionCriteria != "LRT" & mySelectionCriteria != "p"){
      stop("Must select selection criteria 'LRT' or 'p'.")
    }
    
    cat(paste0("Performing variable selection based off of ", mySelectionCriteria, "\n\n"))
    
    model_total_glmm_list <- backward_glmer_p(myFormulaGLMM = formula_total,
                                              myDataGLMM = usedData,
                                              clusterID = myClusterID,
                                              required_terms = c(myTreatment, 
                                                                 myForcedTerms),
                                              pCutoff = myCutoff,
                                              selectionCriteria = mySelectionCriteria)
    
    
    
    # Data frame to output comparing inputted variables to removed variables
    length_temp <- max(length(c(myCovariates, myInteractions)),
                       length(c(model_total_glmm_list[[2]])),
                       length(c(model_total_glmm_list[[3]])))
    total_variables_frame <- tibble(
      `Inputted Variables` = `length<-`(c(myCovariates, myInteractions), length_temp),
      `Removed Variables`  = `length<-`(c(model_total_glmm_list[[2]]), length_temp),
      `Final Covariates`   = `length<-`(c(model_total_glmm_list[[3]]), length_temp)
    )
    
    # Model fit for Total
    model_total_glmm <- model_total_glmm_list[[1]]
    
    # Updated total formula
    formula_total_updated <- update(formula(model_total_glmm_list[[1]]), 
                                    paste(". ~ . - (1|", myClusterID, ")", sep=""))
    
    # can't do model selection for Direct because it has to be the same model as 
    # total effects to validly do the difference method
    # Direct GLMM with only covariates included in total effects backwards selection
    formula_direct_updated <- paste0(paste(myOutcome, "~", myTreatment, "+ "),
                                     paste(myComponent, sub(paste0(".*\\b", myTreatment, "\\b\\s*\\+\\s*"), "", 
                                                            paste0(formula_total_updated)), sep = " + "))
    
    model_direct_glmm <- glmer(update(stats::as.formula(formula_direct_updated), 
                                      . ~ . + (1 | cluster_id)), # Uses exchangeable
                               family = binomial(link = "logit"),
                               data = usedData)
  }

  # INDIVIDUAL TOTAL EFFECTS MODELS --------------------------------------------
  
  ## Summary Tables for Total Effects Models
  tidy_total_glmm_full <- broom.mixed::tidy(model_total_glmm, effects = "fixed") %>%
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
    mutate(ICC = performance::icc(model_total_glmm, tolerance = 0)$ICC_adjusted[[1]])
  tidy_total_glmm <- dplyr::select(tidy_total_glmm_full, -Variance, -SE)
  
  tidy_total_glmm_or <- tidy_total_glmm %>% # OR version of the table
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    rename(`Estimate (OR)` = Estimate)
  
  ### Final Individual Total Effects Table
  table_total <- tidy_total_glmm
  table_total_or <- tidy_total_glmm_or
  
  table_total_full <- tidy_total_glmm_full
  
  
  # INDIVIDUAL DIRECT EFFECTS MODEL TABLES -------------------------------------
  
  ## Summary Tables for Direct Effects Models
  tidy_direct_glmm_full <- broom.mixed::tidy(model_direct_glmm, effects = "fixed") %>%
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
    mutate(ICC = performance::icc(model_direct_glmm, tolerance = 0)$ICC_adjusted[[1]])
  tidy_direct_glmm <- dplyr::select(tidy_direct_glmm_full, -Variance, -SE)
  
  tidy_direct_glmm_or <- tidy_direct_glmm %>% # OR version of the table
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    rename(`Estimate (OR)` = Estimate)
  
  ### Final Individual Direct Effects Table
  table_direct <- tidy_direct_glmm
  table_direct_or <- tidy_direct_glmm_or
  
  table_direct_full <- tidy_direct_glmm_full
  
  # CALCULATE INDIRECT EFFECT AND INDIRECT EFFECT VARIANCE ---------------------
  
  # GLMM
  listIEandPM_glmm <- calcIEandPM_glmm(formula = stats::as.formula(formula_direct_updated), 
                                       exposure = myTreatment, 
                                       mediator = myComponent, 
                                       df = usedData, 
                                       cluster = myClusterID,
                                       family = binomial(link = "logit"),
                                       corstr = "independence", 
                                       conf.level = 0.95, 
                                       pres = "sep", 
                                       niealternative = "two-sided")
  # listIEandPM_glmm$totalDat
  # listIEandPM_glmm$directDat
  # listIEandPM_glmm$indirectDat
  # listIEandPM_glmm$pmDat
  
  # GET ALL INFO FOR DE, IE, TE, and PM ----------------------------------------
  de_full_info_covars <- table_direct_full %>%
    mutate(Effect = "Direct") %>%
    relocate(Model, Term, Effect)
  
  de_full_info_covars_or <- de_full_info_covars %>%
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    rename(`Estimate (OR)` = Estimate)
  
  de_full_info <- de_full_info_covars %>%
    dplyr::filter(Term == myTreatment)
  
  ie_full_info <- listIEandPM_glmm$indirectDat %>%
    mutate(Effect = "Indirect") %>%
    relocate(Model, Term, Effect) %>%
    mutate(ICC = NA)
  
  te_full_info_covars <- table_total_full %>%
    mutate(Effect = "Total") %>%
    relocate(Model, Term, Effect)
  
  te_full_info_covars_or <- te_full_info_covars %>%
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    rename(`Estimate (OR)` = Estimate)
  
  te_full_info <- te_full_info_covars %>%
    dplyr::filter(Term == myTreatment)
    
  pm_full_info <- listIEandPM_glmm$pmDat %>%
    mutate(Effect = "PM") %>%
    relocate(Model, Term, Effect) %>%
    mutate(ICC = NA)
  
  full_table <- bind_rows(de_full_info,
                          ie_full_info,
                          te_full_info,
                          pm_full_info) %>%
    arrange(factor(Effect, levels = c("Total", "Direct", "Indirect", "PM")))
  
  iepm_test <- full_join(rename(dplyr::select(te_full_info, Model, 
                                              Term, Estimate), TE = Estimate), 
                         rename(dplyr::select(de_full_info, Model, 
                                              Term, Estimate), DE = Estimate), 
                         by = c("Model", "Term")) %>%
    mutate(`Indirect Calculated` = TE - DE,
           `PM Calculated` = `Indirect Calculated`/TE) %>%
    dplyr::select(-TE, -DE) %>%
    pivot_longer(cols = c("Indirect Calculated", "PM Calculated"), 
                 values_to = "Estimate", names_to = "Effect")
  
  # Log -odds table
  full_table_test <- bind_rows(full_table, iepm_test) %>%
    arrange(factor(Effect, levels = c("Total", "Direct", "Indirect", "Indirect Calculated",
                                      "PM", "PM Calculated")),
            factor(Model, levels = c("GLM", "GLMM", "GEE")))
    
  # OR table
  full_table_or <- full_table_test %>%
    mutate(Estimate = ifelse(Effect == "PM" | Effect == "PM Calculated", Estimate, exp(Estimate)),
           Lower = ifelse(Effect == "PM" | Effect == "PM Calculated", Lower, exp(Lower)),
           Upper = ifelse(Effect == "PM" | Effect == "PM Calculated", Upper, exp(Upper))) %>%
    rename(`Estimate (OR)` = Estimate) %>%
    arrange(factor(Effect, levels = c("Total", "Direct", "Indirect", "Indirect Calculated",
                                      "PM", "PM Calculated")),
            factor(Model, levels = c("GLM", "GLMM", "GEE")))
  
  # FINAL TABLES WITH ROUNDING TO RETURN ---------------------------------------
  
  # Direct Effects all model info with covariates
  de_full_info_covars_return <- de_full_info_covars %>% # return this
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(`Estimate (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    mutate(Model = "Direct Effects Model") %>%
    dplyr::select(Model, Term, `Estimate (95% CI)`, 
                  `p-value`, ICC, Variance, SE)
  # Direct Effects all model info with covariates (OR)
  de_full_info_covars_or_return <- de_full_info_covars_or %>% # return this
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(`OR (95% CI)` = paste0(`Estimate (OR)`, " (", Lower, ", ", Upper, ")")) %>%
    mutate(Model = "Direct Effects Model") %>%
    dplyr::select(Model, Term, `OR (95% CI)`, 
                  `p-value`, ICC, Variance, SE)
  
  # Total effects all model info with covariates
  te_full_info_covars_return <- te_full_info_covars %>% # return this
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(`Estimate (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    mutate(Model = "Total Effects Model") %>%
    dplyr::select(Model, Term, `Estimate (95% CI)`, 
                  `p-value`, ICC, Variance, SE)
  # Total effects all model info with covariates (OR)
  te_full_info_covars_or_return <- te_full_info_covars_or %>% # return this
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(`OR (95% CI)` = paste0(`Estimate (OR)`, " (", Lower, ", ", Upper, ")")) %>%
    mutate(Model = "Total Effects Model") %>%
    dplyr::select(Model, Term, `OR (95% CI)`, 
                  `p-value`, ICC, Variance, SE)
  
  # All Causal Effects table 
  full_table_return <- full_table_test %>% # return this
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    arrange(factor(Model, levels = c("GLM", "GLMM", "GEE")),
            factor(Effect, levels = c("Total", "Direct", "Indirect", "Indirect Calculated",
                                      "PM", "PM Calculated"))) %>%
    mutate(`Estimate (95% CI)` = paste0(`Estimate`, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, Effect, `Estimate (95% CI)`, `p-value`, ICC, Variance, SE)
    
  # All Causal Effects table (OR)
  full_table_or_return <- full_table_or %>% # return this
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    arrange(factor(Model, levels = c("GLM", "GLMM", "GEE")),
            factor(Effect, levels = c("Total", "Direct", "Indirect", "Indirect Calculated",
                                      "PM", "PM Calculated"))) %>%
    mutate(`OR (95% CI)` = paste0(`Estimate (OR)`, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, Effect, `OR (95% CI)`, `p-value`, ICC, Variance, SE)
  
  
  # Create dataset of meta-data about the analysis
  meta_data <- tibble(
    `Information` = c("Number of Observations in Inputted Dataset",
                      
                      "Number of Observations used in Models",
                      
                      "Number of Observations Missing the Treatment Variable",
                      "Number of Observations Missing the Outcome Variable",
                      "Number of Observations Missing the Component Variable",
                      "Number of Observations Missing both Outcome and Component Variable",
                      
                      "Total Effects Model Formula Primarily Fit",
                      "Total Effects Model Formula Final",
                      
                      "Direct Effects Model Formula Primarily Fit",
                      "Total Effects Model Formula Final"
                      ),
    `Value` = c(paste0(nrow(myData)),
                
                paste0(nrow(usedData)),
                
                paste0(nrow(dplyr::filter(myData, is.na(.data[[myTreatment]])))),
                paste0(nrow(dplyr::filter(myData, is.na(.data[[myOutcome]])))),
                paste0(nrow(dplyr::filter(myData, is.na(.data[[myComponent]])))),
                paste0(nrow(dplyr::filter(myData, is.na(.data[[myComponent]]), is.na(.data[[myOutcome]])))),
                
                paste0(formula_total),
                paste0(formula_total_updated),
                
                paste0(formula_direct),
                paste0(formula_direct_updated)
                )
  )
  
  # All output list
  output_list <- list(`Meta Data` = meta_data,
                      
                      `All Estimates` = full_table_return,
                      `All Estimates (OR)` = full_table_or_return,
                      
                      `Total Effects` = te_full_info_covars_return,
                      `Total Effects (OR)` = te_full_info_covars_or_return,
                      
                      `Direct Effects` = de_full_info_covars_return,
                      `Direct Effects (OR)` = de_full_info_covars_or_return,
                      
                      `Covariate List` = total_variables_frame
                      )
  
  return(output_list)
} # End runMediationAnalysis()



# 2. Analysis Function for Overall Effects -------------------------------------

# Overall analysis code
# Overall function to get total, direct, indirect effects and proportion mediated
runOverallAnalysis <- function(roundNumber, # Number of significant digits desired
                               myData, # Dataset
                               mySubjectID, # Subject ID name in dataset
                               myClusterID, # Cluster ID name in dataset
                               myTreatment, # Treatment name in dataset
                               myComponent, # Component name in dataset
                               myComponentProp, # Component proportion in dataset
                               myOutcome, # Outcome name in dataset
                               modelSelect = "No",
                               myCovariates, # Vector of covariate names in dataset
                               myInteractions, # Interactions desired
                               myForcedTerms = NULL, # Terms that must remain in the model
                               myCutoff = NULL, # P-value cutoff for model selection
                               mySelectionCriteria = "p"
                               ){
  
  # Updating model data so that names are consistent
  
  # Entire dataframe given
  modelDataOverall <- myData %>%
    dplyr::select(subject_id = all_of(mySubjectID),
                  cluster_id = all_of(myClusterID),
                  all_of(myTreatment), 
                  all_of(myComponent),
                  all_of(myComponentProp),
                  all_of(myOutcome), 
                  all_of(myCovariates))
  
  # Those in the dataframe who did not receive any treatment
  modelDataIndividual <- modelDataOverall %>%
    dplyr::filter(!!sym(myComponent) == 1)
  
  modelDataSpillover <- modelDataOverall %>%
    dplyr::filter(!!sym(myComponent) == 0)

  # Define formulas for each sub-analysis
  ## Overall Effects Formula
  rhs_overall <- paste(c(myTreatment, myCovariates, myInteractions), collapse = " + ")
  formula_overall <- stats::as.formula(paste(myOutcome, "~", rhs_overall))
  ## Overall Individual Effects Formula
  rhs_individual <- paste(c(myTreatment, myComponentProp, myCovariates, myInteractions), collapse = " + ")
  formula_individual <- stats::as.formula(paste(myOutcome, "~", rhs_individual))
  ## Overall Spillover Effects Formula
  rhs_spillover <- paste(c(myTreatment, myComponentProp, myCovariates, myInteractions), collapse = " + ")
  formula_spillover <- stats::as.formula(paste(myOutcome, "~", rhs_spillover))
  
  # Calculate p1, the cluster-size weight mean across all clusters of
  # any component coverage
  p1 <- sum(modelDataOverall[[myComponentProp]])/nrow(modelDataOverall)
  
  # Calculate Zany_k_bar_X1 for X_ik^any = 1
  Zany_k_bar_X1 <- mean(modelDataIndividual[[myComponentProp]])
  
  # Calculate Zany_k_bar_X0 for X_ik^any = 0
  Zany_k_bar_X0 <- mean(modelDataSpillover[[myComponentProp]])
  
  # If there are no covariates or interactions, just run the model plainly
  if((is.null(myCovariates) & is.null(myInteractions)) | modelSelect == "No"){
    
    cat("Fitting models with no variable selection.\n\n")
    
    ## Overall Effects Model
    model_overall_glmm <- glmer(update(formula_overall, . ~ . + (1 | cluster_id)),
                                family = binomial(link = "logit"),
                                data = modelDataOverall)
    
    ## Overall Individual Effects Model
    model_individual_glmm <- glmer(update(formula_individual, . ~ . + (1 | cluster_id)),
                                   family = binomial(link = "logit"),
                                   data = modelDataIndividual)
    
    ## Overall Spillover Effects Model
    model_spillover_glmm <- glmer(update(formula_spillover, . ~ . + (1 | cluster_id)),
                                  family = binomial(link = "logit"),
                                  data = modelDataSpillover)
    
    
    length_temp <- max(length(c(myCovariates, myInteractions)))
    total_variables_frame <- tibble(
      `Inputted Variables` = `length<-`(c(myCovariates, myInteractions), length_temp),
      `Removed Variables`  = `length<-`(c(NA), length_temp),
      `Final Covariates`   = `length<-`(c(NA), length_temp)
    )
    
    formula_overall_updated <- formula_overall
    formula_individual_updated <- formula_individual
    formula_spillover_updated <- formula_spillover
    
    # Case when we have covariates or interactions
  } else if((!is.null(myCovariates) | !is.null(myInteractions)) & modelSelect == "Yes"){
    
    if(is.null(mySelectionCriteria)){
      stop("Must select selection criteria 'LRT' or 'p'.")
    } else if(mySelectionCriteria != "LRT" & mySelectionCriteria != "p"){
      stop("Must select selection criteria 'LRT' or 'p'.")
    }
    
    cat(paste0("Performing variable selection based off of ", mySelectionCriteria, "\n\n"))
    
    
    # Step through Overall Effects Model
    model_overall_glmm_list <- backward_glmer_p(myFormulaGLMM = formula_overall,
                                                myDataGLMM = modelDataOverall,
                                                clusterID = myClusterID,
                                                required_terms = c(myTreatment, 
                                                                   myForcedTerms),
                                                pCutoff = myCutoff,
                                                selectionCriteria = mySelectionCriteria)
    
    # Data frame to output comparing inputted variables to removed variables
    length_temp <- max(length(c(myCovariates, myInteractions)),
                       length(c(model_overall_glmm_list[[2]])),
                       length(c(model_overall_glmm_list[[3]])))
    total_variables_frame <- tibble(
      `Inputted Variables` = `length<-`(c(myCovariates, myInteractions), length_temp),
      `Removed Variables`  = `length<-`(c(model_overall_glmm_list[[2]]), length_temp),
      `Final Covariates`   = `length<-`(c(model_overall_glmm_list[[3]]), length_temp)
    )
    
    # Model fit for Overall Effects
    model_overall_glmm <- model_overall_glmm_list[[1]]
    
    # Updated Overall Effects Formula
    formula_overall_updated <- update(formula(model_overall_glmm_list[[1]]), 
                                      paste(". ~ . - (1|", myClusterID, ")", sep=""))
    
    # Updated Individual Effects Formula
    formula_individual_updated <- paste0(paste(myOutcome, "~", myTreatment, "+ "),
                                paste(myComponentProp, sub(paste0(".*\\b", myTreatment, "\\b\\s*\\+\\s*"), "", 
                                paste0(formula_overall_updated)), sep = " + "))
    
    # Updated Spillover Effects Formula
    formula_spillover_updated <- paste0(paste(myOutcome, "~", myTreatment, "+ "),
        paste(myComponentProp, sub(paste0(".*\\b", myTreatment, "\\b\\s*\\+\\s*"), "", 
              paste0(formula_overall_updated)), sep = " + "))
    
    
    # Fit Individual Effects Model
    model_individual_glmm <- glmer(update(stats::as.formula(formula_individual_updated), 
                                          . ~ . + (1 | cluster_id)),
                                   family = binomial(link = "logit"),
                                   data = modelDataIndividual) # All TREATED HIV- 
    
    # Fit Spillover Effects Model
    model_spillover_glmm <- glmer(update(stats::as.formula(formula_spillover_updated), 
                                         . ~ . + (1 | cluster_id)),
                                  family = binomial(link = "logit"),
                                  data = modelDataSpillover) # All UNTREATED HIV- 
  }
  
  # Overall Effects Table
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
  
  # Individual Effects Table
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
  individual_effect_glmm <- dplyr::filter(tidy_individual_glmm, Term == myTreatment)$Estimate
  
  # Spillover Effects Table
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
  spillover_effect_glmm <- dplyr::filter(tidy_spillover_glmm, Term == myTreatment)$Estimate
  
  
  # Format big table that has all results
  overall_table <- bind_rows(mutate(tidy_overall_glmm, Model = "Overall GLMM"), 
                             mutate(tidy_individual_glmm, Model = "Individual GLMM"),
                             mutate(tidy_spillover_glmm, Model = "Spillover GLMM") 
                             ) %>%
    dplyr::filter(Term == myTreatment) %>%
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
  overall_method_info <- tibble(Information = c("n of HIV- individuals",
                                                "p1 - study proportion of individuals who took at least 1 component",
                                                 
                                                "Average Zany_k for X = 1",
                                                "Average Zany_k for X = 0",

                                                "X_ik^any = 1",
                                                "X_ik^any = 0",

                                                "Overall Effects Formula Original",
                                                "Overall Effects Formula Updated",
                                                "n for Overall Effects",
                                                 
                                                "Individual Effects Formula Original",
                                                "Individual Effects Formula Updated",
                                                "n for Individual Effects",
                                                 
                                                "Spillover Effects Formula Original",
                                                "Spillover Effects Formula Updated",
                                                "n for Spillover Effects"
                                                ),
  
  Value = c(paste0(nrow(modelDataOverall)),
            paste0(round(p1, roundNumber)),
            
            paste0(round(Zany_k_bar_X0, roundNumber)),
            paste0(round(Zany_k_bar_X1, roundNumber)),
            
            paste0(nrow(modelDataIndividual)),
            paste0(nrow(modelDataSpillover)),
            
            paste0(formula_overall),
            paste0(formula_overall_updated),
            paste0(nrow(modelDataOverall)),
            
            paste0(formula_individual),
            paste0(formula_individual_updated),
            paste0(nrow(modelDataIndividual)),
            
            paste0(formula_spillover),
            paste0(formula_spillover_updated),
            paste0(nrow(modelDataSpillover))
            )
  )
  
  # Formatting total model output for overall GLMM
  returnOverallTable <- tidy_overall_glmm %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(Model = "Overall Effects Model",
           `Estimate (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, `Estimate (95% CI)`, `p-value`, ICC, Variance, SE)
  returnOverallTable_or <- tidy_overall_glmm %>%
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(Model = "Overall Effects Model",
           `OR (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, `OR (95% CI)`, `p-value`, ICC, Variance, SE)
  
  # Formatting total model output for individual GLMM
  returnIndividualTable <- tidy_individual_glmm %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(Model = "Individual Effects Model",
           `Estimate (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, `Estimate (95% CI)`, `p-value`, ICC, Variance, SE)
  returnIndividualTable_or <- tidy_individual_glmm %>%
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(Model = "Individual Effects Model",
           `OR (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, `OR (95% CI)`, `p-value`, ICC, Variance, SE)
  
  # Formatting total model output for overall GLMM
  returnSpilloverTable <- tidy_spillover_glmm %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(Model = "Spillover Effects Model",
           `Estimate (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, `Estimate (95% CI)`, `p-value`, ICC, Variance, SE)
  returnSpilloverTable_or <- tidy_spillover_glmm %>%
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(Model = "Spillover Effects Model",
           `OR (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, `OR (95% CI)`, `p-value`, ICC, Variance, SE)
  
  
  
  # Getting component frequencies across treatment group and outcome
  frequencyData <- modelDataOverall %>%
    dplyr::select(Treatment = all_of(myTreatment),
                  Component = all_of(myComponent),
                  Outcome = all_of(myOutcome)
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
  frequencyDataTotals <- frequencyData %>%
    filter(Outcome != "Missing") %>%
    group_by(Treatment) %>%
    dplyr::summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(Outcome = "Total", .before = 1)
  frequencyOutput <- bind_rows(frequencyData, frequencyDataTotals) %>%
    arrange(Treatment,
            factor(Outcome, levels = c("Yes", "No", "Total", "Missing")))
  
  
  # Distribution of proportion coverage
  distDataProp <- modelDataOverall %>%
    dplyr::select(Treatment = all_of(myTreatment),
                  Component = all_of(myComponentProp),
                  Outcome = all_of(myOutcome)
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
  distDataTotalProp <- modelDataOverall %>%
    dplyr::select(Treatment = all_of(myTreatment),
                  Component = all_of(myComponentProp),
                  Outcome = all_of(myOutcome)
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
  distOutputProp <- bind_rows(distDataProp, distDataTotalProp) %>%
    arrange(Treatment,
            factor(Outcome, levels = c("Yes", "No", "Total", "Missing")))
  
  
  returnList <- list(`Component Frequencies` = frequencyOutput,
                     `Proportion Distributions` = distOutputProp,
                     `Meta Data` = overall_method_info,
                     `All Estimates` = overall_table,
                     `Decomposition` = decompositionTable,
                     
                     `Overall Effects` = returnOverallTable,
                     `Overall Effects (OR)` = returnOverallTable_or,
                     
                     `Individual Effects` = returnIndividualTable,
                     `Individual Effects (OR)` = returnIndividualTable_or,
                     
                     `Spillover Effects` = returnSpilloverTable,
                     `Spillover Effects (OR)` = returnSpilloverTable_or,
                     
                     `Covariate List` = total_variables_frame
  )
  
  
} # End runOverallAnalysis()



