# Keys should always be the names of the output .csv files in the results/numeric folder
# Name - Verbose name for the table
# Colnames - names of columns to use from the file read through the `read_csv` functions
# Data transformations - transformations to apply to the source data frame after it has been subsetted to the desired columns, specified as a dictionary
# String transformations - transformations to apply to the latex string after the header/footer parts are replaced, specified as a dictionary
TABLES = {
    "effect_summary_stats": {
        "name": "Effect Summary Statistics",
        "colnames": ['Var Name', 'Mean', 'CI lower', 'CI upper', 'Weighted Mean', 'WM CI lower', 'WM CI upper', 'Obs'],
        "data_transformations": {
            "handle_special": "all" # Set to "all" to apply to all columns
        },
        "string_transformations": {},
        "verbose": False,
    },
    "linear_tests": {
        "name": "Linear models",
        "colnames": ["Unnamed: 0", "OLS", "Fixed Effects", "Between Effects", "Random Effects", "Study weighted OLS","Precision weighted OLS"],
        "data_transformations": {
            "handle_special": "all"
        },
        "string_transformations": {
            "rename": [("Total observations", "Observations")],
            "emphasize_rownames": ["Bootstrapped CI (PB)", "(Standard Error)", "Bootstrapped CI (EBB)", "(Constant)"],
            "insert_linespace": ["Effect Beyond Bias", "Observations"]
        },
        "verbose": False,
    }
    # The variable summary statistics table needs an overhaul in source code - var sum stats does not append the variable description
    # "variable_summary_stats": {
    #     "name": "Variable Summary Statistics",
    #     "new_colnames": ['Var Name', 'Var Class', 'Mean', 'Median', 'Min', 'Max', 'SD', 'Obs', 'Missing Obs'],
    #     "cols_to_drop": ['Var Name', 'Var Class', 'Mean', 'SD'],
    #     "verbose": False,
    # },
    # "": {
    #     "name": "",
    #     "new_colnames": [],
    #     "cols_to_drop": [],
    #     "verbose": False,
    # }
}

# List of CSV outputs in the numeric folder
# "bpe_econ_sig.csv"
# "bpe_res_all_studies.csv"
# "bpe_summary_stats.csv"
# "effect_summary_stats.csv"
# "exo_tests.csv"
# "linear_tests.csv"
# "ma_variables_description_table.csv"
# "ma.csv"
# "nonlinear_tests.csv"
# "p_hacking_tests_caliper.csv"
# "p_hacking_tests_elliott.scv"
# "p_hacking_tests_maive.csv"
# "robma_components.csv"
# "robma_estiates.csv"
# "variable_summary_stats.csv"
