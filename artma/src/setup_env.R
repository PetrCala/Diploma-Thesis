#!/usr/bin/env Rscript
source("libs/env.R")

cat("Setting up the environment.\n")
tryCatch(
  {
    setup_env()
  },
  error = function(e) {
    # Logger is not available yet - use message instead
    message("Error setting up the environment:")
    message(e)
    quit(status = 1)
  }
)
cat("Environment setup complete.\n")
