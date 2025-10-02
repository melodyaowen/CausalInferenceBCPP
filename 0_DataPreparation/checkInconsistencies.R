test <- fullData %>%
  dplyr::select(dplyr::contains("hiv_status_current")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))


test <- fullData %>%
  dplyr::select(dplyr::contains("arv_status_current")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))


test <- fullData %>%
  dplyr::select(dplyr::contains("endpoint_coverage_mc")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))



test <- fullData %>%
  dplyr::select(dplyr::contains("endpoint_coverage_onart")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))



test <- fullData %>%
  dplyr::select(dplyr::contains("endpoint_coverage_onart")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))



test <- fullData %>%
  dplyr::select(dplyr::contains("endpoint_coverage_vlsupp")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))


test <- fullData %>%
  dplyr::select(dplyr::contains("circumcised")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))


test <- fullData %>%
  dplyr::select(dplyr::contains("circumcision_days")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))


test <- fullData %>%
  dplyr::select(dplyr::contains("endpoint_coverage_mc")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))


test <- fullData %>%
  dplyr::select(dplyr::contains("overall_access")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))



test <- fullData %>%
  dplyr::select(dplyr::contains("exchange_12mos")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent, exchange_12mos))



test <- fullData %>%
  dplyr::select(dplyr::contains("condom_lastsex")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent, condom_lastsex))


test <- fullData %>%
  dplyr::select(dplyr::contains("partners_lifetime")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))



test <- fullData %>%
  dplyr::select(dplyr::contains("partners_12mos")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))


test <- fullData %>%
  dplyr::select(dplyr::contains("length_residence")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))


test <- fullData %>%
  dplyr::select(dplyr::contains("employment_status")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))


test <- fullData %>%
  dplyr::select(dplyr::contains("education")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))


test <- fullData %>%
  dplyr::select(dplyr::contains("marital_status")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))



test <- fullData %>%
  dplyr::select(dplyr::contains("prob_alcohol_drug")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))



test <- fullData %>%
  dplyr::select(dplyr::contains("prob_healthcare")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))


test <- fullData %>%
  dplyr::select(dplyr::contains("prob_hiv")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))


test <- fullData %>%
  dplyr::select(dplyr::contains("prob_schools")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))



test <- fullData %>%
  dplyr::select(dplyr::contains("prob_housing")) %>%
  { # coerce to character and replace NA with a sentinel
    vars_chr <- dplyr::mutate(., dplyr::across(dplyr::everything(), as.character))
    m <- as.matrix(vars_chr)
    m[is.na(m)] <- "__NA__"
    # rows are OK only if all entries in the row are identical (including all "__NA__")
    all_equal <- rowSums(m == m[, 1, drop = TRUE]) == ncol(m)
    dplyr::mutate(as.data.frame(.), inconsistent = ifelse(all_equal, "OK", "Inconsistent"))
  }
View(arrange(test, inconsistent))


