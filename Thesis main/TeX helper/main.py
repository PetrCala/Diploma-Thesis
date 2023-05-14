import pandas as pd

import populate_tables_source as populate

SOURCE_PATH = r'C:/Users/hso20/OneDrive/Plocha/IES/Diploma-Thesis/R/_results/'
VAR_SUM_STATS_PATH = "variable_summary_stats"
EFFECT_SUM_STATS_PATH = "effect_summary_stats"
LINEAR_TESTS_PATH = "linear_tests"
NONLINEAR_TESTS_PATH = "nonlinear_tests"
EXO_TESTS_PATH = "exo_tests"
CALIPER_PATH = "p_hacking_tests_caliper"
ELLIOTT_PATH = "p_hacking_tests_elliott"
MAIVE_PATH = "p_hacking_tests_maive"
MA_PATH = "ma"
MA_DESC_PATH = "ma_variables_description_table"
BPE_RES_PATH = "bpe_res"
BPE_ECON_SIG_PATH = "bpe_econ_sig"


if __name__ == "__main__":
    df = pd.read_csv("ma_table.csv")
    bma_table = populate.populateBMATable(df)
    populate.write_latex_to_file(bma_table)