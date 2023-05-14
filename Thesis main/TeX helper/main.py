import pandas as pd

import populate_tables_source as populate

SOURCE_PATH = r'C:/Users/hso20/OneDrive/Plocha/IES/Diploma-Thesis/R/_results/'
"variable_summary_stats" = "Variable summary stats"
"effect_summary_stats" = "Effect summary stats"
"linear_tests" = "Linear tests"
"nonlinear_tests" = "Nonlinear tests"
"exo_tests" = "Tests relaxing the exogeneity assumption"
"p_hacking_tests_caliper" = "Caliper tests"
"p_hacking_tests_eliott" = "Eliott tests"
"p_hacking_tests_maive" = "MAIVE"
"ma" = "Model averaging"
"ma_variables_description_table" = "Model averaging description table"
"bpe_res" = "Best practice estimate"
"bpe_econ_sig" = "Economic significance"


if __name__ == "__main__":
    df = pd.read_csv("ma_table.csv")
    bma_table = populate.populateBMATable(df)
    populate.write_latex_to_file(bma_table)