# RequiredPackages.R
# Each code script will run this code first to ensure that the user has
# all necessary packages installed and ready

# Package names
packages <- c("tidyverse", "lme4", "MASS", "Matrix", "matrixcalc", "latex2exp",
              "ICC", "nlme", "bindata", "gee", "crt2power", "mosaic", "haven",
              "tmvtnorm", "ggplot2", "latex2exp", "reshape2", "gridExtra",
              "table1", "broom", "xtable", "logistf", "kableExtra", "geepack",
              "elrm", "broom", "dplyr", "tibble", "lme4", "broom.mixed",
              "performance", "mice", "glmnet", "rms", "naniar", "writexl", "car")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
