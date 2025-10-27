# calcIEandPM.R

# GLM VERSION ------------------------------------------------------------------
# Calculates the CIs for the indirect effect (IE) and the 
# proportion mediated (PM)
calcIEandPM_glm <- function(formula, 
                            exposure, 
                            mediator, 
                            df, 
                            family = binomial(link = "logit"),
                            corstr = "independence", 
                            conf.level = 0.95, 
                            pres = "sep", 
                            niealternative = "two-sided"){
  
  alp.conf <- (1 - conf.level)/2
  n.vars <- length(all.vars(formula))
  indep.vars <- all.vars(formula)[2:n.vars]
  
  # Checks to make sure inputs are ok
  if (!(pres %in% c("tog", "sep"))) {
    print("Using pres='sep'. pres argument is unknown.")
    pres <- "sep"
  }
  if (!(niealternative %in% c("two-sided", "one-sided"))) {
    warning(paste0("niealternative = '", niealternative, 
                   "' is not supported. Using 'two-sided' for nie test."))
    niealternative <- "two-sided"
  }
  if (!(exposure %in% indep.vars)) {
    stop("The exposure/treatment should be included in the model formula")
  }
  if (!(mediator %in% indep.vars)) {
    stop("The mediator should be included in the model formula")
  }
  if (is.null(df)) {
    stop("Argument 'df' was null. Data must be part of a data frame")
  }
  
  
  back <- list()
  back$call <- match.call()
  back$alter <- niealternative
  
  # Call on function that does the data duplication
  fit <- invisible(GEEmediateFit_glm(formula = formula, 
                                     exposure = exposure, 
                                     mediator = mediator, 
                                     df = df,
                                     family = family, 
                                     corstr = corstr))
  back$GEEfit <- fit
  nde <- fit$coefficients[names(fit$coefficients) == exposure]
  nie <- fit$coefficients[names(fit$coefficients) == paste0(exposure, ".star")] - nde
  te <- nde + nie
  pm <- nie/te
  cov.gee <- fit$robust.variance
  v <- cov.gee[dimnames(fit$robust.variance)[[1]] == exposure, 
               dimnames(fit$robust.variance)[[2]] == exposure]
  v.star <- cov.gee[dimnames(fit$robust.variance)[[1]] == 
                      paste0(exposure, ".star"), dimnames(fit$robust.variance)[[2]] == 
                      paste0(exposure, ".star")]
  covar <- cov.gee[dimnames(fit$robust.variance)[[1]] == exposure, 
                   dimnames(fit$robust.variance)[[2]] == paste0(exposure, 
                                                                ".star")]
  if (pm < 0 | pm > 1) {
    warning("Crude PM estimate not within [0,1]. Either NIE and NDE are in opposing directions or M is not a mediator. Inference for NIE only")
    var.nie <- v + v.star - 2 * covar
    nie.pval <- 2 * pnorm(abs(nie)/sqrt(var.nie), lower.tail = F)
    nie.ci <- nie + c(qnorm(alp.conf), qnorm(1 - alp.conf)) * 
      sqrt(var.nie)
    back$alter <- "two-sided"
    back$nie.pval <- nie.pval
    back$nie.ci <- nie.ci
  }
  else {
    var.pm <- v/te^2 + v.star * (nde^2)/(te^4) - 2 * covar * nde/(te^3)
    var.nie <- v + v.star - 2 * covar
    pm.pval <- pnorm(pm/sqrt(var.pm), lower.tail = F)
    if (niealternative == "two-sided") {
      nie.pval <- 2 * pnorm(abs(nie)/sqrt(var.nie), lower.tail = F)
    }
    else {
      nie.pval <- pnorm(nie/sqrt(var.nie), lower.tail = F)
    }
    pm.ci <- pm + c(qnorm(alp.conf), qnorm(1 - alp.conf)) * sqrt(var.pm)
    nie.ci <- nie + c(qnorm(alp.conf), qnorm(1 - alp.conf)) * sqrt(var.nie)
    back$pm.pval <- pm.pval
    back$pm.ci <- pm.ci
    back$nie.pval <- nie.pval
    back$nie.ci <- nie.ci
  }
  back$nde <- nde
  back$nie <- nie
  back$pm <- pm
  back$pres <- pres
  names(back$pm) <- "Prop Med"
  class(back) <- "GEEmediate"
  
  totalDat <- tibble(Estimate = as.numeric(te))
  directDat <- tibble(Estimate = as.numeric(nde))
  indirectDat <- tibble(Model = "GLM",
                        Term = exposure,
                        Estimate = as.numeric(nie),
                        Lower = as.numeric(nie.ci[1]),
                        Upper = as.numeric(nie.ci[2]),
                        `p-value` = as.numeric(nie.pval),
                        Variance = as.numeric(var.nie),
                        SE = sqrt(as.numeric(var.nie)))
  pmDat <- tibble(Model = "GLM",
                  Term = exposure,
                  Estimate = as.numeric(pm),
                  Lower = as.numeric(pm.ci[1]),
                  Upper = as.numeric(pm.ci[2]),
                  `p-value` = as.numeric(pm.pval),
                  Variance = as.numeric(var.pm),
                  SE = sqrt(as.numeric(var.pm)))
  
  
  return(list(totalDat = totalDat,
              directDat = directDat,
              indirectDat = indirectDat,
              pmDat = pmDat))
}


GEEmediateFit_glm <- function(formula, df, exposure, mediator, 
                              family = binomial(link = "logit"),
                              corstr = "independence"){
  outcome.name <- all.vars(formula)[1]
  df.relevant <- model.frame(formula = formula, data = df)
  
  # Call on DupliData function
  dupl.df <- DupliData_glm(df = df.relevant, 
                           mediator = mediator, 
                           outcome = outcome.name)
  
  
  dupl.formula <- as.formula(paste(outcome.name, 
                                   paste(colnames(dupl.df)[!(colnames(dupl.df) %in% 
                                                               c(outcome.name, "ID"))], 
                                         collapse = " + "), sep = " ~ "))
  dupl.formula <- update(dupl.formula, ~. - 1)
  dupl.fit <- gee::gee(formula = dupl.formula, 
                       id = ID, 
                       data = dupl.df, 
                       family = family, 
                       corstr = corstr)
  return(dupl.fit)
}


DupliData_glm <- function(df, 
                          mediator = NULL, 
                          outcome = NULL){
  
  if(missing(outcome)){
    stop("Outcome name missing.")
  }
  
  n.row <- nrow(df)
  df.star <- df
  df.star[[paste0(mediator)]] <- rep(0, n.row)
  names.covar.nofactor <- names(df)[!(names(df) %in% 
                                        c(outcome, mediator)) & !sapply(df, is.factor)]
  names(df.star)[!(names(df) %in% c(outcome, mediator)) & 
                   !sapply(df, is.factor)] <- paste0(names.covar.nofactor, ".star")
  
  for (var.name in names.covar.nofactor) {
    df[[paste0(var.name, ".star")]] <- rep(0, n.row)
    df.star[[paste0(var.name)]] <- rep(0, n.row)
    }
  
  names.covar.factor <- names(df)[!(names(df) %in% 
                                      c(outcome, mediator)) & sapply(df, is.factor)]
  names(df.star)[!(names(df) %in% c(outcome, mediator)) & 
                   sapply(df, is.factor)] <- paste0(names.covar.factor, ".star")
  
  for (var.name in names.covar.factor) {
    df[[paste0(var.name, ".star")]] <- factor(rep(levels(df[[var.name]])[1], 
                                                  n.row), levels = levels(df[[var.name]]))
    df.star[[paste0(var.name)]] <- factor(rep(levels(df[[var.name]])[1], 
                                              n.row), levels(df[[var.name]]))
    }
  
  df.dupl <- rbind(df[c(names.covar.nofactor, names.covar.factor, 
                        paste0(c(names.covar.nofactor, names.covar.factor), 
                               ".star"), paste0(c(mediator, outcome)))], 
                   df.star[c(names.covar.nofactor, names.covar.factor, 
                             paste0(c(names.covar.nofactor, names.covar.factor), 
                                    ".star"), paste0(c(mediator, outcome)))])
  
  df.dupl$INT <- factor(c(rep("", n.row), rep(".star", n.row)))
  df.dupl <- df.dupl[c("INT", paste0(c(outcome, mediator)), 
                       names.covar.nofactor, names.covar.factor, 
                       paste0(c(names.covar.nofactor, names.covar.factor), ".star"))]
  
  df.dupl$ID <- rep(1:n.row, 2)
  return(df.dupl)
}





# GLMM VERSION -----------------------------------------------------------------
# Calculates the CIs for the indirect effect (IE) and the 
# proportion mediated (PM)

calcIEandPM_glmm <- function(formula, 
                             exposure, 
                             mediator, 
                             df, 
                             cluster,
                             family = binomial(link = "logit"),
                             corstr = "independence", 
                             conf.level = 0.95, 
                             pres = "sep", 
                             niealternative = "two-sided"){
  
  alp.conf <- (1 - conf.level)/2
  n.vars <- length(all.vars(formula))
  indep.vars <- all.vars(formula)[2:n.vars]
  
  # Checks to make sure inputs are ok
  if (!(pres %in% c("tog", "sep"))) {
    print("Using pres='sep'. pres argument is unknown.")
    pres <- "sep"
  }
  if (!(niealternative %in% c("two-sided", "one-sided"))) {
    warning(paste0("niealternative = '", niealternative, 
                   "' is not supported. Using 'two-sided' for nie test."))
    niealternative <- "two-sided"
  }
  if (!(exposure %in% indep.vars)) {
    stop("The exposure/treatment should be included in the model formula")
  }
  if (!(mediator %in% indep.vars)) {
    stop("The mediator should be included in the model formula")
  }
  if (is.null(df)) {
    stop("Argument 'df' was null. Data must be part of a data frame")
  }
  
  
  back <- list()
  back$call <- match.call()
  back$alter <- niealternative
  
  # Call on function that does the data duplication
  fit <- invisible(GEEmediateFit_glmm(formula = formula, 
                                      exposure = exposure, 
                                      mediator = mediator, 
                                      df = df,
                                      cluster = cluster,
                                      family = family, 
                                      corstr = corstr))
  back$GEEfit <- fit
  
  # nde <- fit$coefficients[names(fit$coefficients) == exposure]
  nde <- dplyr::filter(broom.mixed::tidy(fit, effects = "fixed"), term == exposure)$estimate
 
  # nie <- fit$coefficients[names(fit$coefficients) == paste0(exposure, ".star")] - nde
  nie <- dplyr::filter(broom.mixed::tidy(fit, effects = "fixed"), term == paste0(exposure, ".star"))$estimate - nde
  
  te <- nde + nie
  pm <- nie/te
  
  #cov.gee <- fit$geese$vbeta
  #cn <- names(coef(fit))
  #dimnames(cov.gee) <- list(cn, cn)
  cov.gee <- as.matrix(vcov(fit))
  v <- cov.gee[dimnames(cov.gee)[[1]] == exposure, 
               dimnames(cov.gee)[[2]] == exposure]
  v.star <- cov.gee[dimnames(cov.gee)[[1]] == paste0(exposure, ".star"), 
                    dimnames(cov.gee)[[2]] == paste0(exposure, ".star")]
  covar <- cov.gee[dimnames(cov.gee)[[1]] == exposure, 
                   dimnames(cov.gee)[[2]] == paste0(exposure, ".star")]

  if (pm < 0 | pm > 1) {
    warning("Crude PM estimate not within [0,1]. Either NIE and NDE are in opposing directions or M is not a mediator. Inference for NIE only")
    var.nie <- v + v.star - 2 * covar
    nie.pval <- 2 * pnorm(abs(nie)/sqrt(var.nie), lower.tail = F)
    nie.ci <- nie + c(qnorm(alp.conf), qnorm(1 - alp.conf)) * 
      sqrt(var.nie)
    back$alter <- "two-sided"
    back$nie.pval <- nie.pval
    back$nie.ci <- nie.ci
  }
  else {
    var.pm <- v/te^2 + v.star * (nde^2)/(te^4) - 2 * covar * nde/(te^3)
    var.nie <- v + v.star - 2 * covar
    pm.pval <- pnorm(pm/sqrt(var.pm), lower.tail = F)
    if (niealternative == "two-sided") {
      nie.pval <- 2 * pnorm(abs(nie)/sqrt(var.nie), lower.tail = F)
    }
    else {
      nie.pval <- pnorm(nie/sqrt(var.nie), lower.tail = F)
    }
    pm.ci <- pm + c(qnorm(alp.conf), qnorm(1 - alp.conf)) * sqrt(var.pm)
    nie.ci <- nie + c(qnorm(alp.conf), qnorm(1 - alp.conf)) * sqrt(var.nie)
    back$pm.pval <- pm.pval
    back$pm.ci <- pm.ci
    back$nie.pval <- nie.pval
    back$nie.ci <- nie.ci
  }
  back$nde <- nde
  back$nie <- nie
  back$pm <- pm
  back$pres <- pres
  names(back$pm) <- "Prop Med"
  class(back) <- "GEEmediate"
  
  totalDat <- tibble(Estimate = as.numeric(te))
  directDat <- tibble(Estimate = as.numeric(nde))
  indirectDat <- tibble(Model = "GLMM",
                        Term = exposure,
                        Estimate = as.numeric(nie),
                        Lower = as.numeric(nie.ci[1]),
                        Upper = as.numeric(nie.ci[2]),
                        `p-value` = as.numeric(nie.pval),
                        Variance = as.numeric(var.nie),
                        SE = sqrt(as.numeric(var.nie)))
  pmDat <- tibble(Model = "GLMM",
                  Term = exposure,
                  Estimate = as.numeric(pm),
                  Lower = as.numeric(pm.ci[1]),
                  Upper = as.numeric(pm.ci[2]),
                  `p-value` = as.numeric(pm.pval),
                  Variance = as.numeric(var.pm),
                  SE = sqrt(as.numeric(var.pm)))
  
  return(list(totalDat = totalDat,
              directDat = directDat,
              indirectDat = indirectDat,
              pmDat = pmDat))
}


GEEmediateFit_glmm <- function(formula, df, cluster, exposure, mediator, 
                               family = binomial(link = "logit"),
                               corstr = "independence"){
  outcome.name <- all.vars(formula)[1]
  df.relevant <- df
  
  # Call on DupliData function
  dupl.df <- DupliData_glmm(df = df.relevant, 
                            mediator = mediator, 
                            outcome = outcome.name)
  dupl.df.nocluster <- dupl.df %>%
    dplyr::select(-cluster_id)
  
  dupl.formula <- as.formula(paste(outcome.name, 
                                   paste(colnames(dupl.df.nocluster)[!(colnames(dupl.df.nocluster) %in% 
                                                                         c(outcome.name, "ID"))], 
                                         collapse = " + "), sep = " ~ "))
  dupl.formula <- update(dupl.formula, ~. - 1)
  # dupl.fit_glm <- gee::gee(formula = dupl.formula, 
  #                      id = ID, 
  #                      data = dupl.df, 
  #                      family = family, 
  #                      corstr = corstr)
  
  # dupl.fit_gee <- geepack::geeglm(formula = dupl.formula, 
  #                             id = cluster_id, 
  #                             data = dupl.df, 
  #                             family = family, 
  #                             corstr = "exchangeable")
  
  # dupl.fit <- glmer(update(dupl.formula, . ~ . + (0 + INT || cluster_id)), # Uses exchangeable
  #                   family = binomial(link = "logit"),
  #                   data = dupl.df,
  #                   control = lme4::glmerControl(optimizer = "bobyqa", 
  #                                                optCtrl = list(maxfun = 2e5)))
  
  dupl.df.alt <- dupl.df %>%
    mutate(cluster_block = interaction(cluster_id, INT, drop = TRUE))
  dupl.fit.alt <- lme4::glmer(
    update(dupl.formula, . ~ . + (1 | cluster_block)),  # one RE per cluster×block
    data   = dupl.df.alt,
    family = binomial(link = "logit"),
    control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
  
  return(dupl.fit.alt)
}


DupliData_glmm <- function(df, 
                           mediator = NULL, 
                           outcome = NULL){
  
  if(missing(outcome)){
    stop("Outcome name missing.")
  }
  
  n.row <- nrow(df)
  df.star <- df
  df.star[[paste0(mediator)]] <- rep(0, n.row)
  names.covar.nofactor <- names(df)[!(names(df) %in% 
                                        c(outcome, mediator)) & !sapply(df, is.factor)]
  names(df.star)[!(names(df) %in% c(outcome, mediator)) & 
                   !sapply(df, is.factor)] <- paste0(names.covar.nofactor, ".star")
  
  for (var.name in names.covar.nofactor) {
    df[[paste0(var.name, ".star")]] <- rep(0, n.row)
    df.star[[paste0(var.name)]] <- rep(0, n.row)
  }
  
  names.covar.factor <- names(df)[!(names(df) %in% 
                                      c(outcome, mediator)) & sapply(df, is.factor)]
  names(df.star)[!(names(df) %in% c(outcome, mediator)) & 
                   sapply(df, is.factor)] <- paste0(names.covar.factor, ".star")
  
  for (var.name in names.covar.factor) {
    df[[paste0(var.name, ".star")]] <- factor(rep(levels(df[[var.name]])[1], 
                                                  n.row), levels = levels(df[[var.name]]))
    df.star[[paste0(var.name)]] <- factor(rep(levels(df[[var.name]])[1], 
                                              n.row), levels(df[[var.name]]))
  }
  
  df.dupl <- rbind(df[c(names.covar.nofactor, names.covar.factor, 
                        paste0(c(names.covar.nofactor, names.covar.factor), 
                               ".star"), paste0(c(mediator, outcome)))], 
                   df.star[c(names.covar.nofactor, names.covar.factor, 
                             paste0(c(names.covar.nofactor, names.covar.factor), 
                                    ".star"), paste0(c(mediator, outcome)))])
  
  df.dupl$INT <- factor(c(rep("", n.row), rep(".star", n.row)))
  df.dupl <- df.dupl[c("INT", paste0(c(outcome, mediator)), 
                       names.covar.nofactor, names.covar.factor, 
                       paste0(c(names.covar.nofactor, names.covar.factor), ".star"))]
  
  df.dupl$ID <- rep(1:n.row, 2)
  df.dupl.test <- df.dupl %>%
    mutate(cluster_id = cluster_id + cluster_id.star) %>%
    dplyr::select(-subject_id, -subject_id.star, -cluster_id.star)
  
  return(df.dupl.test)
}







# GEE VERSION ------------------------------------------------------------------
# Calculates the CIs for the indirect effect (IE) and the 
# proportion mediated (PM)

calcIEandPM_gee <- function(formula, 
                            exposure, 
                            mediator, 
                            df, 
                            cluster,
                            family = binomial(link = "logit"),
                            corstr = "independence", 
                            conf.level = 0.95, 
                            pres = "sep", 
                            niealternative = "two-sided"){
  
  alp.conf <- (1 - conf.level)/2
  n.vars <- length(all.vars(formula))
  indep.vars <- all.vars(formula)[2:n.vars]
  
  # Checks to make sure inputs are ok
  if (!(pres %in% c("tog", "sep"))) {
    print("Using pres='sep'. pres argument is unknown.")
    pres <- "sep"
  }
  if (!(niealternative %in% c("two-sided", "one-sided"))) {
    warning(paste0("niealternative = '", niealternative, 
                   "' is not supported. Using 'two-sided' for nie test."))
    niealternative <- "two-sided"
  }
  if (!(exposure %in% indep.vars)) {
    stop("The exposure/treatment should be included in the model formula")
  }
  if (!(mediator %in% indep.vars)) {
    stop("The mediator should be included in the model formula")
  }
  if (is.null(df)) {
    stop("Argument 'df' was null. Data must be part of a data frame")
  }
  
  
  back <- list()
  back$call <- match.call()
  back$alter <- niealternative
  
  # Call on function that does the data duplication
  fit <- invisible(GEEmediateFit_gee(formula = formula, 
                                     exposure = exposure, 
                                     mediator = mediator, 
                                     df = df,
                                     cluster = cluster,
                                     family = family, 
                                     corstr = corstr))
  back$GEEfit <- fit
  nde <- fit$coefficients[names(fit$coefficients) == exposure]
  nie <- fit$coefficients[names(fit$coefficients) == paste0(exposure, ".star")] - nde
  te <- nde + nie
  pm <- nie/te # left off here, things looking good so far

  cov.gee <- fit$geese$vbeta
  cn <- names(coef(fit))
  dimnames(cov.gee) <- list(cn, cn)
  v <- cov.gee[dimnames(cov.gee)[[1]] == exposure, 
               dimnames(cov.gee)[[2]] == exposure]
  v.star <- cov.gee[dimnames(cov.gee)[[1]] == paste0(exposure, ".star"), 
                    dimnames(cov.gee)[[2]] == paste0(exposure, ".star")]
  covar <- cov.gee[dimnames(cov.gee)[[1]] == exposure, 
                   dimnames(cov.gee)[[2]] == paste0(exposure, ".star")]
  
  
  if (pm < 0 | pm > 1) {
    warning("Crude PM estimate not within [0,1]. Either NIE and NDE are in opposing directions or M is not a mediator. Inference for NIE only")
    var.nie <- v + v.star - 2 * covar
    nie.pval <- 2 * pnorm(abs(nie)/sqrt(var.nie), lower.tail = F)
    nie.ci <- nie + c(qnorm(alp.conf), qnorm(1 - alp.conf)) * 
      sqrt(var.nie)
    back$alter <- "two-sided"
    back$nie.pval <- nie.pval
    back$nie.ci <- nie.ci
  }
  else {
    var.pm <- v/te^2 + v.star * (nde^2)/(te^4) - 2 * covar * nde/(te^3)
    var.nie <- v + v.star - 2 * covar
    pm.pval <- pnorm(pm/sqrt(var.pm), lower.tail = F)
    if (niealternative == "two-sided") {
      nie.pval <- 2 * pnorm(abs(nie)/sqrt(var.nie), lower.tail = F)
    }
    else {
      nie.pval <- pnorm(nie/sqrt(var.nie), lower.tail = F)
    }
    pm.ci <- pm + c(qnorm(alp.conf), qnorm(1 - alp.conf)) * sqrt(var.pm)
    nie.ci <- nie + c(qnorm(alp.conf), qnorm(1 - alp.conf)) * sqrt(var.nie)
    back$pm.pval <- pm.pval
    back$pm.ci <- pm.ci
    back$nie.pval <- nie.pval
    back$nie.ci <- nie.ci
  }
  back$nde <- nde
  back$nie <- nie
  back$pm <- pm
  back$pres <- pres
  names(back$pm) <- "Prop Med"
  class(back) <- "GEEmediate"
  
  totalDat <- tibble(Estimate = as.numeric(te))
  directDat <- tibble(Estimate = as.numeric(nde))
  indirectDat <- tibble(Model = "GEE",
                        Term = exposure,
                        Estimate = as.numeric(nie),
                        Lower = as.numeric(nie.ci[1]),
                        Upper = as.numeric(nie.ci[2]),
                        `p-value` = as.numeric(nie.pval),
                        Variance = as.numeric(var.nie),
                        SE = sqrt(as.numeric(var.nie)))
  pmDat <- tibble(Model = "GEE",
                  Term = exposure,
                  Estimate = as.numeric(pm),
                  Lower = as.numeric(pm.ci[1]),
                  Upper = as.numeric(pm.ci[2]),
                  `p-value` = as.numeric(pm.pval),
                  Variance = as.numeric(var.pm),
                  SE = sqrt(as.numeric(var.pm)))
  
  return(list(totalDat = totalDat,
              directDat = directDat,
              indirectDat = indirectDat,
              pmDat = pmDat))
}


GEEmediateFit_gee <- function(formula, df, cluster, exposure, mediator, 
                              family = binomial(link = "logit"),
                              corstr = "independence"){
  outcome.name <- all.vars(formula)[1]
  df.relevant <- df
  
  # Call on DupliData function
  dupl.df <- DupliData_gee(df = df.relevant, 
                           mediator = mediator, 
                           outcome = outcome.name)
  dupl.df.nocluster <- dupl.df %>%
    dplyr::select(-cluster_id)
  
  dupl.formula <- as.formula(paste(outcome.name, 
                                   paste(colnames(dupl.df.nocluster)[!(colnames(dupl.df.nocluster) %in% 
                                                               c(outcome.name, "ID"))], 
                                         collapse = " + "), sep = " ~ "))
  dupl.formula <- update(dupl.formula, ~. - 1)
  # dupl.fit <- gee::gee(formula = dupl.formula, 
  #                      id = cluster_id, 
  #                      data = dupl.df, 
  #                      family = family, 
  #                      corstr = corstr)
  
  dupl.fit <- geepack::geeglm(formula = dupl.formula, 
                              id = cluster_id, 
                              data = dupl.df, 
                              family = family, 
                              corstr = "exchangeable")
  return(dupl.fit)
}


DupliData_gee <- function(df, 
                          mediator = NULL, 
                          outcome = NULL){
  
  if(missing(outcome)){
    stop("Outcome name missing.")
  }
  
  n.row <- nrow(df)
  df.star <- df
  df.star[[paste0(mediator)]] <- rep(0, n.row)
  names.covar.nofactor <- names(df)[!(names(df) %in% 
                                        c(outcome, mediator)) & !sapply(df, is.factor)]
  names(df.star)[!(names(df) %in% c(outcome, mediator)) & 
                   !sapply(df, is.factor)] <- paste0(names.covar.nofactor, ".star")
  
  for (var.name in names.covar.nofactor) {
    df[[paste0(var.name, ".star")]] <- rep(0, n.row)
    df.star[[paste0(var.name)]] <- rep(0, n.row)
  }
  
  names.covar.factor <- names(df)[!(names(df) %in% 
                                      c(outcome, mediator)) & sapply(df, is.factor)]
  names(df.star)[!(names(df) %in% c(outcome, mediator)) & 
                   sapply(df, is.factor)] <- paste0(names.covar.factor, ".star")
  
  for (var.name in names.covar.factor) {
    df[[paste0(var.name, ".star")]] <- factor(rep(levels(df[[var.name]])[1], 
                                                  n.row), levels = levels(df[[var.name]]))
    df.star[[paste0(var.name)]] <- factor(rep(levels(df[[var.name]])[1], 
                                              n.row), levels(df[[var.name]]))
  }
  
  df.dupl <- rbind(df[c(names.covar.nofactor, names.covar.factor, 
                        paste0(c(names.covar.nofactor, names.covar.factor), 
                               ".star"), paste0(c(mediator, outcome)))], 
                   df.star[c(names.covar.nofactor, names.covar.factor, 
                             paste0(c(names.covar.nofactor, names.covar.factor), 
                                    ".star"), paste0(c(mediator, outcome)))])
  
  df.dupl$INT <- factor(c(rep("", n.row), rep(".star", n.row)))
  df.dupl <- df.dupl[c("INT", paste0(c(outcome, mediator)), 
                       names.covar.nofactor, names.covar.factor, 
                       paste0(c(names.covar.nofactor, names.covar.factor), ".star"))]
  
  df.dupl$ID <- rep(1:n.row, 2)
  df.dupl.test <- df.dupl %>%
    mutate(cluster_id = cluster_id + cluster_id.star) %>%
    dplyr::select(-subject_id, -subject_id.star, -cluster_id.star)
  
  return(df.dupl.test)
}
