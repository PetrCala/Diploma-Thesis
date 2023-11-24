import os

import pandas as pd

from tables.bma import populateBMATable
from tables.bpe_res import populateBpeResTable
from tables.bpe_econ_sig import populateBpeEconSigTable
from tables.effect_stats import populateEffectStatsTable
from tables.exo_tests import populateExoTestsTable
from tables.linear_tests import populateLinearTestsTable
from tables.ma_desc import populateMaDescTable
from tables.nonlinear_tests import populateNonlinearTestsTable
from tables.var_stats import populateVarStatsTable
import static as static

pd.set_option('display.max_colwidth', 200) # Long descriptions

PROJECT_ROOT = os.path.dirname(__file__)
DT_ROOT = os.path.dirname(os.path.dirname(PROJECT_ROOT)) # Two folders up
NUMERIC_RESULTS_PATH = os.path.join(DT_ROOT, "Results", "results", "numeric")

FILE_NAMES = { # Names of the files in values
    "var_sum_stats": "variable_summary_stats",
    "effect_sum_stats": "effect_summary_stats",
    "linear_tests": "linear_tests",
    "nonlinear_tests": "nonlinear_tests",
    "exo_tests": "exo_tests",
    # "caliper": "p_hacking_tests_caliper",
    # "elliott": "p_hacking_tests_elliott",
    # "maive": "p_hacking_tests_maive",
    "ma": "ma",
    "ma_desc": "ma_variables_description_table",
    "bpe_res": "bpe_res_all_studies",
    "bpe_econ_sig": "bpe_econ_sig"
}

ALL_METHODS = { # Names of the python functions associated with every method
    "var_sum_stats": populateVarStatsTable,
    "effect_sum_stats": populateEffectStatsTable,
    "linear_tests": populateLinearTestsTable,
    "nonlinear_tests": populateNonlinearTestsTable,
    "exo_tests": populateExoTestsTable,
    # "caliper": populate.populateCaliperTable,
    # "elliott": populate.populateElliottTable,
    # "maive": populate.populateMaiveTable,
    "ma": populateBMATable,
    "ma_desc": populateMaDescTable,
    "bpe_res": populateBpeResTable,
    "bpe_econ_sig": populateBpeEconSigTable
}

def readCSV(method_name:str):
    '''Specify the name of the method and read the file associated with
    this method. The name must be a string that is a part of the keys of ALL_PATHS.
    '''
    if not method_name in FILE_NAMES.keys():
        raise ValueError("Incorrectly specified method name.")
    file_name = FILE_NAMES.get(method_name)
    full_path = os.path.join(NUMERIC_RESULTS_PATH, f"{file_name}.csv")
    df = pd.read_csv(full_path, encoding = 'utf-8')
    return df

def runMethod(method_name:str, df:pd.DataFrame, verbose:bool = False):
    '''Specify the name of the method as a string as well as
    the data frame on which the method should be called and call the corresponding
    python function from the 'populate.py' script.
    '''
    if not method_name in ALL_METHODS.keys():
        raise ValueError("Incorrectly specified method name.")
    py_fun = ALL_METHODS.get(method_name)
    latex_output = py_fun(df, verbose = verbose) # Call the function with params
    return latex_output

def runOne(key:str, verbose:bool = True):
    '''Specify the name of the method and run this method.
    Return the LaTeX output.
    '''
    df = readCSV(key)
    latex_output = runMethod(key, df, verbose = verbose)
    return latex_output

def runAll(verbose:bool = False):
    '''Read all data frames into memory and call all the functions
    on them.'''
    # Assert validity of keys
    if ALL_METHODS.keys() != FILE_NAMES.keys():
        raise ValueError("Incorrectly specified keys.")
    # Run all methods
    print("Processing all tables...")
    output = list()
    for key in ALL_METHODS.keys():
        if ALL_METHODS.get(key) == "": # Undefined method
            raise ValueError("Undefined method in ALL_METHODS")
        single_latex_output = runOne(key, verbose=verbose)
        output.append(single_latex_output)
    print("All tables processed succesfully.")
    return output

def getDefaultOutputFolderPath()->str:
    return os.path.join(PROJECT_ROOT, static.OUTPUT_FOLDER_NAME)

def writeLatexToFile(latex_content:str, key:str, output_dir:str = None)->None:
    output_dir = getDefaultOutputFolderPath() if output_dir is None else output_dir
    if not os.path.exists(output_dir):
        os.mkdir(output_dir)
    output_path = os.path.join(output_dir, f"{key}_output.txt")
    with open(output_path, 'w', encoding="utf-8") as file:
        file.write(latex_content)
    return None
     
def resultsToTxt(results_list:list):
    '''Input the list of the results as strings and
    write the results into .txt files into the 
    /_output/ folder.
    '''
    keys =  ALL_METHODS.keys()
    if len(results_list) != len(keys):
        return ValueError("Incorrect list dimensions.")
    # Iterate over all results and the corresponding keys
    for res, key in zip(results_list, keys):
        if res not in ["", None]:
            print(f"Writing {key} to a .txt file...")
            writeLatexToFile(res, key)

if __name__ == "__main__":
    verbose = False
    tex_results = runAll(verbose = verbose)
    resultsToTxt(tex_results)