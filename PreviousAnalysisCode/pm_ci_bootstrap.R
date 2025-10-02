pm_boot_all <- function(data,
                        cluster_id = "cluster_id",
                        treatment  = "T_k",
                        mediator   = "X1_ik_vmmc",
                        outcome    = "Y_ik",
                        B = 1000,
                        corstr = "exchangeable",
                        level = 0.95,
                        seed = NULL,
                        verbose = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  
  # --- formulas
  fM_glm  <- as.formula(paste(mediator, "~", treatment))
  fO_glm  <- as.formula(paste(outcome,  "~", treatment, "+", mediator))
  fM_glmm <- as.formula(paste(mediator, "~", treatment, "+ (1|", cluster_id, ")"))
  fO_glmm <- as.formula(paste(outcome,  "~", treatment, "+", mediator, " + (1|", cluster_id, ")"))
  
  has_lme4    <- requireNamespace("lme4",    quietly = TRUE)
  has_geepack <- requireNamespace("geepack", quietly = TRUE)
  
  paths <- function(a, b1, b2) {
    NIE <- a * b2
    TE  <- b1 + NIE
    PM  <- ifelse(is.finite(TE) && abs(TE) > .Machine$double.eps, NIE / TE, NA_real_)
    c(NDE = b1, NIE = NIE, TE = TE, PM = PM)
  }
  
  # --- point fits
  fit_M_glm <- glm(fM_glm, data = data, family = binomial(link = "logit"))
  fit_O_glm <- glm(fO_glm, data = data, family = binomial(link = "logit"))
  a_glm  <- coef(fit_M_glm)[treatment]
  b1_glm <- coef(fit_O_glm)[treatment]
  b2_glm <- coef(fit_O_glm)[mediator]
  pt_glm <- paths(a_glm, b1_glm, b2_glm)
  
  if (has_lme4) {
    ctrl <- lme4::glmerControl(optimizer = "bobyqa",
                               optCtrl = list(maxfun = 2e5),
                               check.conv.grad = "ignore",
                               check.conv.singular = "ignore")
    fit_M_glmm <- lme4::glmer(fM_glmm, data = data, family = binomial(link = "logit"), control = ctrl)
    fit_O_glmm <- lme4::glmer(fO_glmm, data = data, family = binomial(link = "logit"), control = ctrl)
    a_glmm  <- lme4::fixef(fit_M_glmm)[treatment]
    b1_glmm <- lme4::fixef(fit_O_glmm)[treatment]
    b2_glmm <- lme4::fixef(fit_O_glmm)[mediator]
    pt_glmm <- paths(a_glmm, b1_glmm, b2_glmm)
  }
  
  if (has_geepack) {
    id_full <- data[[cluster_id]]  # << key fix
    fit_M_gee <- geepack::geeglm(fM_glm, id = id_full, data = data,
                                 family = binomial(link = "logit"), corstr = corstr)
    fit_O_gee <- geepack::geeglm(fO_glm, id = id_full, data = data,
                                 family = binomial(link = "logit"), corstr = corstr)
    a_gee  <- coef(fit_M_gee)[treatment]
    b1_gee <- coef(fit_O_gee)[treatment]
    b2_gee <- coef(fit_O_gee)[mediator]
    pt_gee <- paths(a_gee, b1_gee, b2_gee)
  }
  
  # --- bootstrap storage
  quants <- c("NDE","NIE","TE","PM")
  keep_methods <- c("GLM",
                    if (has_lme4) "GLMM" else NULL,
                    if (has_geepack) paste0("GEE(", corstr, ")") else NULL)
  draws <- setNames(lapply(keep_methods, function(m)
    matrix(NA_real_, nrow = B, ncol = length(quants), dimnames = list(NULL, quants))
  ), keep_methods)
  
  ids <- unique(data[[cluster_id]])
  qlo <- (1 - level)/2; qhi <- 1 - qlo
  if (verbose) msg_every <- max(1, floor(B/10))
  
  for (b in seq_len(B)) {
    samp <- sample(ids, length(ids), replace = TRUE)
    dB <- do.call(rbind, lapply(samp, function(k) data[data[[cluster_id]] == k, , drop = FALSE]))
    
    # GLM
    fit_Mb <- try(glm(fM_glm, data = dB, family = binomial(link = "logit")), silent = TRUE)
    fit_Ob <- try(glm(fO_glm, data = dB, family = binomial(link = "logit")), silent = TRUE)
    if (!inherits(fit_Mb, "try-error") && !inherits(fit_Ob, "try-error")) {
      aB  <- suppressWarnings(coef(fit_Mb)[treatment])
      b1B <- suppressWarnings(coef(fit_Ob)[treatment])
      b2B <- suppressWarnings(coef(fit_Ob)[mediator])
      if (all(is.finite(c(aB,b1B,b2B)))) draws$GLM[b, ] <- paths(aB, b1B, b2B)
    }
    
    # GLMM
    if (has_lme4) {
      fit_Mb <- try(lme4::glmer(fM_glmm, data = dB, family = binomial(link = "logit"), control = ctrl), silent = TRUE)
      fit_Ob <- try(lme4::glmer(fO_glmm, data = dB, family = binomial(link = "logit"), control = ctrl), silent = TRUE)
      if (!inherits(fit_Mb, "try-error") && !inherits(fit_Ob, "try-error")) {
        feM <- try(lme4::fixef(fit_Mb), silent = TRUE)
        feO <- try(lme4::fixef(fit_Ob), silent = TRUE)
        if (!inherits(feM, "try-error") && !inherits(feO, "try-error")) {
          aB  <- feM[treatment]; b1B <- feO[treatment]; b2B <- feO[mediator]
          if (all(is.finite(c(aB,b1B,b2B)))) draws$GLMM[b, ] <- paths(aB, b1B, b2B)
        }
      }
    }
    
    # GEE
    if (has_geepack) {
      idB <- dB[[cluster_id]]  # << key fix
      fit_Mb <- try(geepack::geeglm(fM_glm, id = idB, data = dB,
                                    family = binomial(link = "logit"), corstr = corstr), silent = TRUE)
      fit_Ob <- try(geepack::geeglm(fO_glm, id = idB, data = dB,
                                    family = binomial(link = "logit"), corstr = corstr), silent = TRUE)
      if (!inherits(fit_Mb, "try-error") && !inherits(fit_Ob, "try-error")) {
        aB  <- suppressWarnings(coef(fit_Mb)[treatment])
        b1B <- suppressWarnings(coef(fit_Ob)[treatment])
        b2B <- suppressWarnings(coef(fit_Ob)[mediator])
        if (all(is.finite(c(aB,b1B,b2B)))) draws[[paste0("GEE(", corstr, ")")]][b, ] <- paths(aB, b1B, b2B)
      }
    }
    
    if (verbose && b %% msg_every == 0) message("bootstrap: ", b, "/", B)
  }
  
  # final table
  make_rowblock <- function(method, point_vec, boot_mat) {
    pt <- setNames(point_vec, quants)
    ci <- apply(boot_mat, 2, function(col)
      stats::quantile(col, probs = c(qlo, qhi), na.rm = TRUE))
    data.frame(Method   = method,
               Quantity = factor(quants, levels = quants),
               Estimate = as.numeric(pt[quants]),
               Lower    = as.numeric(ci[1, quants]),
               Upper    = as.numeric(ci[2, quants]),
               row.names = NULL)
  }
  
  table_list <- list(make_rowblock("GLM", pt_glm, draws$GLM))
  if (has_lme4)    table_list <- c(table_list, list(make_rowblock("GLMM", pt_glmm, draws$GLMM)))
  if (has_geepack) table_list <- c(table_list, list(make_rowblock(paste0("GEE(", corstr, ")"),
                                                                  pt_gee, draws[[paste0("GEE(", corstr, ")")]])))
  
  out_tab <- do.call(rbind, table_list)
  out_tab <- out_tab[order(out_tab$Method, out_tab$Quantity), ]
  
  list(table = out_tab, draws = draws, B = B, level = level)
}
