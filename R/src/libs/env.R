source("base/const.R")

#' Set the CRAN mirror
set_mirror <- function(mirror = NULL) {
  if (is.null(mirror)) {
    # Set the CRAN mirror to the first available mirror
    available_mirrors <- getCRANmirrors()
    mirror <- available_mirrors$URL[1]
  }
  options(repos = mirror)
}

#' Quietly execute an expression
#'
#' This function suppresses package startup messages, warnings, and messages
#' while executing an expression. It is useful for keeping the console output
#' clean when loading or installing packages.
#'
#' @param expr [expression] The expression to be executed quietly.
#'
#' @return The result of the executed expression without any messages, warnings, or package startup messages.
quiet_packages <- function(expr) {
  suppressPackageStartupMessages(suppressWarnings(suppressMessages(expr)))
}

#' Function to install a package if it is not a part of the library yet
install_and_check <- function(pkg, version, verbose = TRUE) {
  if (verbose) {
    version_info <- ifelse(is.na(version), "", paste0(" (", version, ")"))
    message <- paste0("Processing package: ", pkg, version_info)
    cat(sprintf("%-100s", message)) # Add enough whitespace to make sure the whole line is cleared
    flush.console()
  }
  if (!pkg %in% rownames(installed.packages()) || (!is.na(version) && packageVersion(pkg) != version)) {
    tryCatch(
      {
        # Install specific version if provided, else install the latest version
        if (!is.na(version)) {
          devtools::install_version(pkg, version = version)
        } else {
          install.packages(pkg)
        }
      },
      error = function(e) {
        stop("\nPackage installation failed for ", pkg, ": ", e$message)
      }
    )
  }

  # Load the package
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))

  # Reset the cursor to the start of the line for the progress bar
  cat("\r")
}


#' Load packages required for environment preparation
load_initial_packages <- function(verbose = TRUE) {
  # Load several packages necessary for the environment preparation
  if (verbose) {
    cat("Loading initial packages...\n")
  }
  install_initial_package <- function(x) suppressPackageStartupMessages(install_and_check(x, NA, verbose))
  invisible(lapply(CONST$INITIAL_PACKAGES, install_initial_package))
}

#' Load and install a list of R packages
#'
#' This function checks if the specified packages are installed, installs any missing
#' packages, and then loads all of them. If an error occurs during the installation
#' or loading process, the function stops execution and displays an error message.
#'
#' Include a progress bar to track the loading process.
#'
#' @param package_list [character] A character vector of package names.
#' @param verbose [bool] If TRUE, print out verbose output about the package loading.
#'
#' @return A message indicating that all packages were loaded successfully or an error message if the process fails.
load_packages <- function(package_list, verbose = TRUE) {
  load_initial_packages(verbose)

  # Convert package_list to a named list with NULL versions if necessary
  if (!is.list(package_list) || is.null(names(package_list))) {
    package_list <- setNames(as.list(rep(NA, length(package_list))), package_list)
  }

  # Loading packages
  if (verbose) {
    cat("Loading packages...\n")
  }

  # Applying the function to each package with a progress bar
  pbapply::pblapply(names(package_list), function(pkg) install_and_check(pkg, package_list[[pkg]], verbose))

  if (verbose) {
    cat("\rAll packages loaded successfully\n")
  }
}


#' Setup the R environment
setup_env <- function() {
  tryCatch(
    {
      set_mirror()
    },
    error = function(e) {
      message("Error setting the CRAN mirror:")
      message(e)
    }
  )

  tryCatch(
    {
      load_packages(CONST$PACKAGES, verbose = TRUE) # Defined in static
    },
    error = function(e) {
      message("Error loading the packages:")
      message(e)
    }
  )
}

