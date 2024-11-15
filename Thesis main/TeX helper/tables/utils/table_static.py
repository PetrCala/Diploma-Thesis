﻿# Keys should always be the names of the output .csv files in the results/numeric folder
# Name - Verbose name for the table
# Colnames - names of columns to use from the file read through the `read_csv` functions
# Data transformations - transformations to apply to the source data frame after it has been subsetted to the desired columns, specified as a dictionary
# String transformations - transformations to apply to the latex string after the header/footer parts are replaced, specified as a dictionary
TABLES = {
    "bpe_econ_sig": {
        "name": "BPE Economic Significance",
        "colnames": ['Unnamed: 0', 'Effect on Sigma (1*ΔSD)', '% of best (1*ΔSD)',
       'Effect on Sigma (ΔMax)', '% of best(ΔMax)'],
        "data_transformations": {
            "handle_special": "all",
            "fill_na": "all",
        },
        "string_transformations": {},
        "verbose": False
    },
    "p_hacking_tests_caliper": {
        "name": "Caliper tests",
        "colnames": ["Unnamed: 0", "Threshold 1.645", "Threshold 1.96", "Threshold 2.58"],
        "data_transformations": {
            "handle_special": "all",
        },
        "string_transformations": {
            "rename": [
                (r' - Estimate &', r' &'),
                (r'\n.*?SE &', r'\n(SE) &'),
                (r'\n.*?n total &', r'\n(Observations) &'),
            ],
            "insert_linespace": ["Caliper width 0.1 ", "Caliper width 0.15"],
            "emphasize_rownames": ["(SE)", "(Observations)"]
        },
        "verbose": False
    },
    "effect_summary_stats": {
        "name": "Effect Summary Statistics",
        "colnames": ['Var Name', 'Mean', 'CI lower', 'CI upper', 'Weighted Mean', 'WM CI lower', 'WM CI upper', 'Obs'],
        "data_transformations": {
            "handle_special": "all", # Set to "all" to apply to all columns
            "fill_na": "all",
            "insert_thousand_separators": ["Obs"],
        },
        "string_transformations": {
            "rename": [
                ("Wage Earners <", "Self-employed >"),
                ("Private Sector <", "Public Sector >"),
                ("Male <", "Female >"),
                ("Rural <", "Urban >"),
            ],
            "insert_section": [
                # format: (<insert-before-this-string>,<section-name>)
                ("Estimate: City", "Estimate characteristics"),
                ("Study Size >=", "Data Characteristics"),
                ("Higher Education >=", "Spatial/structural variation"),
                ("Ability: Direct", "Estimation method"),
                ("Impact Factor >=", "Publication characteristics")
            ]
        },
        "verbose": False,
    },
    "exo_tests": {
        "name": "Tests relaxing the endogeneity assumption",
        "colnames": ["Unnamed: 0", "IV", "p-Uniform"],
        "data_transformations": {
            # "handle_special": "all",
            "fill_na": "all",
        },
        "string_transformations": {
            "rename": [("Total observations", "Observations")],
            "insert_linespace": ["Effect Beyond Bias", "Observations", "F-test"],
            "emphasize_rownames": ["(PB SE)", "(EBB SE)"]
        },
        "verbose": False,

    },
    "linear_tests": {
        "name": "Linear models",
        "colnames": ["Unnamed: 0", "OLS", "Fixed Effects", "Between Effects", "Random Effects", "Study weighted OLS","Precision weighted OLS"],
        "data_transformations": {
            "handle_special": "all",
            "fill_na": "all",
        },
        "string_transformations": {
            "rename": [
                ("(Standard Error)", "SE"),
                ("Bootstrapped CI (PB)", "Bootstrapped CI"),
                ("(Constant)", "SE"),
                ("Bootstrapped CI (EBB)", "Bootstrapped CI"),
            ],
            "emphasize_rownames": ["Bootstrapped CI", "SE"], 
            "insert_linespace": ["Effect Beyond Bias", "Total observations"]
        },
        "verbose": False,
    },
    "ma": {
        "name": "Model Averaging Results",
        "colnames": ["Unnamed: 0", "BMA P.Mean", "BMA SD", "BMA PIP", "FMA Coef", "FMA SE", "FMA p-val"],
        "data_transformations": {
            "handle_special": "all",
            "bold_if_pip_high": ["BMA PIP"],
        },
        "string_transformations": {
            "rename": [("(Intercept)", "Constant")],
            "insert_section": [
                # format: (<insert-before-this-string>,<section-name>)
                ("Estimate: City", "Estimate characteristics"),
                ("Study Size", "Data Characteristics"),
                ("Primary Education", "Spatial/structural variation"),
                ("Method: Cohort/FE", "Estimation method"),
                ("Impact Factor", "Publication characteristics")
            ]
        },
        "verbose": False,
    },
    "ma_variables_description_table": {
        "name": "Model Averaging Variable Descriptions",
        "colnames": ["Variable", "Description", "Mean", "SD"],
        "data_transformations": {
            "handle_special": "all",
        },
        "string_transformations": {
            "insert_section": [
                ("Estimate: City", "Estimate characteristics"),
                ("Study Size", "Data characteristics"),
                ("No Education", "Spatial/structural variation"),
                ("Method: OLS", "Estimation method"),
                ("Impact Factor", "Publication characteristics")
            ]
        },
        "verbose": False,
    },
    "nonlinear_tests": {
        "name": "Non-linear models",
        "colnames": ["Unnamed: 0", "WAAP", "Top10", "Stem", "Hierarch", "Selection", "Endogenous Kink"],
        "data_transformations": {
            "handle_special": "all",
            "fill_na": "all",
        },
        "string_transformations": {
            "emphasize_rownames": ["(PB SE)", "(EBB SE)"],
            "insert_linespace": ["Effect Beyond Bias", "Total observations", "Model observations"]
        },
        "verbose": False,
    }
    # The variable summary statistics table needs an overhaul in source code - var sum stats does not append the variable description
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
