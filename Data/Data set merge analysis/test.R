#' Run using the 'source the active document' hotkey (locally set to Shift+F5)

library('readxl')

data_main <- read_excel("../data_set_master_thesis_cala.xlsm", sheet = 'var_list')
data_twins <- read_excel("../twin_data_master_thesis_cala.xlsm", sheet = 'var_list')

get_missing <- function(data_from, data_to) {
  missing <- c()
  for (var in data_from$var_name) {
    if (!var %in% data_to$var_name) {
      var_verbose <- data_from$var_name_verbose[data_from$var_name == var]
      missing <- append(missing, var_verbose)
    }
  }
  return(missing)
}

main_missing <- get_missing(data_main, data_twins)
twins_missing <- get_missing(data_twins, data_main)

cat("Variables present in the master set but not in the twin data set:\n\n")
cat(paste(main_missing, collapse = "\n"))
cat("\n\n")

cat("Variables present in the twin set but not in the master data set:\n\n")
cat(paste(twins_missing, collapse = "\n"))
cat("\n\n")