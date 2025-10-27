# Backwards GLMM function to fit based on p-value threshold

backward_glmer_p <- function(myFormulaGLMM, 
                             myDataGLMM, 
                             clusterID = "cluster_id",
                             required_terms = c("T_k"),
                             pCutoff = 0.20) {
  
  ctrl_harder <- glmerControl(
    optimizer = "bobyqa",                 # generally more stable than Nelder–Mead
    optCtrl   = list(maxfun = 2e5),       # raise iteration budget (try 1e5–5e5)
    calc.derivs = TRUE,                   # better Hessian diagnostics
    check.conv.singular = "ignore",       # don't bail out on boundary (var~0)
    check.conv.grad     = "ignore",       # quiet gradient warnings
    check.conv.hess     = "ignore"        # quiet Hessian warnings
  )
  
  # Start GLMM from full fixed effects + random intercept
  form_full <- update(myFormulaGLMM, paste(". ~ . + (1|", clusterID, ")", sep=""))
  
  # Initial fit
  fit <- glmer(form_full, data = myDataGLMM, 
               family = binomial(link = "logit"), 
               control = ctrl_harder)
  
  required_terms <- c(required_terms, paste0("1 | ", clusterID))
  
  # Helper: term labels currently in the fixed-effects part
  fixed_terms <- function(fit) attr(terms(formula(fit)), "term.labels")
  
  cand <- setdiff(fixed_terms(fit), required_terms)
  removed_term_current <- NULL
  removed_vector <- c()
  
  forloopbreaker <- FALSE
  
  for(i in 1:100){
    if(forloopbreaker == TRUE){
      break
    }
    
    cat("Current iteration =", i, "\n")
    
    # Build current candidate drop set = all fixed terms minus required
    cand <- setdiff(cand, removed_term_current)
    
    # Updated formula
    if(is.null(removed_vector)){
      formulaUpdated <- update(myFormulaGLMM, paste(". ~ . + (1|", clusterID, ")", 
                                                    paste(removed_vector, collapse = " - "), sep=""))
    } else{
      formulaUpdated <- update(myFormulaGLMM, paste(". ~ . + (1|", clusterID, ")", "-",
                                                    paste(removed_vector, collapse = " - "), sep=""))
    }
    
    # Paste formula
    cat("Fitting the model: ", paste(formulaUpdated), "\n")
    
    # Fit new model
    fitCurrent <- glmer(formulaUpdated, data = myDataGLMM, 
                        family = binomial(link = "logit"), 
                        control = ctrl_harder)
    
    # Wald p-values for whole terms
    wald_tab <- car::Anova(fitCurrent, type = 3)
    
    
    # If nothing else in the candidate vector of variables to remove,
    # break out of the forloop
    if (length(cand) == 0L){
      cat("No remaining variables to check. Stop re-fitting.\n\n")
      break
    }
    
    # Look at only candidate terms to drop
    drop_tab <- wald_tab[rownames(wald_tab) %in% cand, , drop = FALSE]
    
    # Get only the variable name (ID) and p value (P)
    drop_tibble <- drop_tab %>%
      rownames_to_column(var = "ID") %>%
      as_tibble() %>%
      dplyr::select(ID, P = `Pr(>Chisq)`)
    
    # Printing current p-values to make sure things are running smoothly
    drop_matrix <- as.matrix(drop_tibble)
    cat(apply(drop_matrix, 1, paste, collapse = "\t"), sep = "\n")
    
    keepChecking <- TRUE
    while(keepChecking){
      cat("Running through keepChecking loop\n\n")
      
      # Define worst term based on worst (largest) p-value
      worst_row <- dplyr::filter(drop_tibble, P == max(P))
      worst_term <- worst_row$ID
      worst_p <- worst_row$P
      
      cat(paste0("Checking the term: ", worst_term, "\n\n"))
      
      # CASE 1: TERM IS BETTER THAN CUTOFF
      # Break out fof "keepChecking" loop and the main forloop
      if (is.na(worst_p) || worst_p <= pCutoff) {
        cat(paste0("Worst variable is ", worst_term, " and is better than threshold. Stop re-fitting.\n\n"))
        forloopbreaker <- TRUE
        keepChecking <- FALSE
        break
      } # END CASE 1: TERM IS BETTER THAN CUTOFF
      
      # CASE 2: TERM IS AN INTERACTION
      # Can remove term
      if(grepl(":", worst_term) == TRUE){
        cat(paste0("Worst variable is ", worst_term, " and will be removed. Re-fit model again.\n\n"))
        removed_term_current <- worst_term
        removed_vector <- c(removed_vector, removed_term_current)
        keepChecking <- FALSE
        break
      } # END CASE 2: TERM IS AN INTERACTION
      
      # We've discerned worst term is not an interaction, and is not better than
      # the threshold. So we have to check if it's a part of an interaction
      
      # Current interactions in the candidate list
      current_interactions <- drop_tibble %>%
        dplyr::filter(grepl(":", ID))
      
      # Interactions that have covariate wanting to be dropped in it
      checkInteraction <- current_interactions %>%
        dplyr::filter(grepl(paste0(worst_term), ID))
      
      # CASE 3: WORST TERM IS APART OF INTERACTIONS
      if(nrow(checkInteraction) != 0){ # If there are interactions
        
        # Interactions that are significant
        keepInteractions <- checkInteraction %>%
          dplyr::filter(P <= pCutoff)
        
        # CASE 3A: WORST TERM'S INTERACTIONS ARE SIGNIFICANT
        # If no interactions are significant, can drop the variable 
        # and its interactions and break out
        if(nrow(keepInteractions) == 0){
          
          cat(paste0("Worst variable is ", worst_term, " and will be removed. No interactions involving the variable were significant, so remove interactions as well. Re-fit model again.\n\n"))
          removed_term_current <- c(worst_term, checkInteraction$ID)
          removed_vector <- c(removed_vector, removed_term_current)
          keepChecking <- FALSE
          break
        }
        
        # CASE 3B: WORST TERM'S INTERACTIONS ARE SIGNIFICANT
        # Interactions are significant so move remove worst term from the potential
        # removal list and check next biggest term
        drop_tibble <- drop_tibble %>%
          dplyr::filter(ID != worst_term)
        next
        
      } # END CASE 3: WORST TERM IS APART OF INTERACTIONS
      
      # CASE 4: WORST TERM IS NOT APART OF INTERACTIONS
      if(nrow(checkInteraction) == 0){ # If there are no interactions
        cat(paste0("Worst variable is ", worst_term, " and will be removed. No interactions involving the variable. Refit-model again.\n\n"))
        removed_term_current <- worst_term
        removed_vector <- c(removed_vector, removed_term_current)
        keepChecking <- FALSE
        break
      } # END CASE 4: WORST TERM IS NOT APART OF INTERACTIONS
      
    } # End keepchecking
    
  } # End forloop 1:100
  
  # Final variables included in the list
  finalFormula <- formula(fitCurrent)
  finalCovariates <- attr(terms(nobars(finalFormula), keep.order = TRUE), "term.labels")

  # Return the fit object, removed variables, and final covariates
  return(list(fitCurrent, removed_vector, finalCovariates))
  
} # End function
