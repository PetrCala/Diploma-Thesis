# This scripts should not be handling any imports/export through box, as it is used when setting up the initial environment. To avoid this, one would have to define the initial packages outside this script.

CONST <- list(
  INITIAL_PACKAGES = c(
    "rstudioapi",
    "devtools",
    "pbapply"
  ),
  #' A list of packages to use in the project. If you wish to specify a package version,
  #' set the list value to a string that corresponds to that version. Otherwise put NA,
  #' which will make R automatically fetch the latest version of the package.
  #' For more details, see 'source_master_thesis_cala.R::loadPackages'
  PACKAGES = list(
    "AER" = NA, # Applied econometrics with R
    "boot" = NA, # Wild bootstrap confidence intervals
    "BMS" = NA, # bayesian model averaging
    "DescTools" = NA, # Descriptive statistics and data analysis
    "bayesm" = NA, # bayesian modeling and inference
    "cachem" = NA, # Cache system - creating and deleting cache files
    "car" = NA, # Variance Inflation Factor
    "corrplot" = NA, # Graphical display of correlation matrices
    "data.table" = NA, # Fast data manipulation and aggregation
    "devtools" = NA, # Loading local packages
    "ddpcr" = NA, # Analysis of Droplet Digital PCR (ddPCR) data
    "fdrtool" = NA, # Elliott et al. (2022)
    "foreign" = NA, # Reading and writing data stored by other statistical software
    "gdata" = NA, # Elliott et al. (2022)
    "glue" = NA, # Python-like f-strings
    "grDevices" = NA, # Elliott et al. (2022)
    "ggtext" = NA, # ggplot axis text without warnings
    "haven" = NA, # Importing and exporting data from SAS, SPSS, and Stata
    "ivmodel" = NA, # Instrumental variable confidence interval (Anderson-Rubin)
    "lmtest" = NA, # Hypothesis testing and diagnostics for linear regression models
    "LowRankQP" = NA, # For Hierarchical Bayes
    "memoise" = NA, # Cache system - speeding up deterministic function calls
    "meta" = NA, # Meta-analysis package
    "metafor" = NA, # Conducting meta-analyses
    "multcomp" = NA, # Simultaneous inference for general linear hypotheses
    "multiwayvcov" = NA, # Computing clustered covariance matrix estimators
    "NlcOptim" = NA, # Elliott et al. (2022) - CoxShi
    "plm" = NA, # Random Effects, Between Effects
    "plotly" = NA, # Interactive plots
    "png" = NA, # PNG plots
    "puniform" = "0.2.2", # Computing the density, distribution function, and quantile function of the uniform distribution
    "pracma" = NA, # MAIVE Estimator, Elliott et al. (2022)
    "RColorBrewer" = NA, # Plot colors
    "rddensity" = NA, # Elliott et al. (2022)
    "readxl" = NA, # Reading Excel files
    "RoBMA" = NA, # Robust BMA, Bartos et al. (2021)
    "sandwich" = NA, # Computing robust covariance matrix estimators, MAIVE estimator
    "shiny" = NA, # Andrew & Kasy (2019) Selection model
    "spatstat" = NA, # Elliott et al. (2022)
    "stargazer" = NA, # LaTeX table generation
    "stats" = NA, # Statistical analysis and modeling
    "testthat" = NA, # Unit testing for R
    "tidyverse" = NA, # A collection of R packages designed for data science, including ggplot2, dplyr, tidyr, readr, purrr, and tibble
    "varhandle" = NA, # MAIVE estimator
    "xfun" = NA, # Proper ggplot label display
    "xtable" = NA, # Creating tables in LaTeX or HTML
    "yaml" = NA # User parameters
  )
)