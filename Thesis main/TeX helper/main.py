import pandas as pd

import populate_tables_source as populate

SOURCE_PATH = r'C:/Users/hso20/OneDrive/Plocha/IES/Diploma-Thesis/Results/results/numeric/'
ALL_PATHS = { # Names of the files in values
    "var_sum_stats": "variable_summary_stats",
    "effect_sum_stats": "effect_summary_stats",
    "linear_tests": "linear_tests",
    "nonlinear_tests": "nonlinear_tests",
    "exo_tests": "exo_tests",
    "caliper": "p_hacking_tests_caliper",
    "elliott": "p_hacking_tests_elliott",
    "maive": "p_hacking_tests_maive",
    "ma": "ma",
    "ma_desc": "ma_variables_description_table",
    "bpe_res": "bpe_res",
    "bpe_econ_sig": "bpe_econ_sig"
}

ALL_METHODS = { # Names of the python functions associated with every method
    "var_sum_stats": populate.populateVarStatsTable,
    "effect_sum_stats": populate.populateEffectStatsTable,
    "linear_tests": populate.populateLinearTestsTable,
    "nonlinear_tests": populate.populateNonlinearTestsTable,
    "exo_tests": populate.populateExoTestsTable,
    "caliper": populate.populateCaliperTable,
    "elliott": populate.populateElliottTable,
    "maive": populate.populateMaiveTable,
    "ma": populate.populateBMATable,
    "ma_desc": populate.populateMaDescTable,
    "bpe_res": populate.populateBpeResTable,
    "bpe_econ_sig": populate.populateBpeEconSigTable
}

def readCSV(method_name:str):
    '''Specify the name of the method and read the file associated with
    this method. The name must be a string that is a part of the keys of ALL_PATHS.
    '''
    if not method_name in ALL_PATHS.keys():
        raise ValueError("Incorrectly specified method name.")
    method_path = ALL_PATHS.get(method_name)
    full_path = SOURCE_PATH + method_path + ".csv" 
    df = pd.read_csv(full_path, encoding = 'utf-8')
    return df

def runMethod(method_name:str, df:pd.DataFrame):
    '''Specify the name of the method as a string as well as
    the data frame on which the method should be called and call the corresponding
    python function from the 'populate.py' script.
    '''
    if not method_name in ALL_METHODS.keys():
        raise ValueError("Incorrectly specified method name.")
    py_fun = ALL_METHODS.get(method_name)
    latex_output = py_fun(df) # Call the function with params
    return latex_output


def runOne(key:str, verbose:bool = True):
    '''Specify the name of the method and run this method.
    Return the LaTeX output.
    '''
    df = readCSV(key)
    latex_output = runMethod(key, df)
    if verbose:
        print(latex_output)
    return latex_output

def runAll():
    '''Read all data frames into memory and call all the functions
    on them.'''
    # Assert validity of keys
    if ALL_METHODS.keys() != ALL_PATHS.keys():
        raise ValueError("Incorrectly specified keys.")
    # Run all methods
    output = list()
    for key in ALL_METHODS.keys():
        if ALL_METHODS.get(key) == "": # Undefined method
            raise ValueError("Undefined method in ALL_METHODS")
        single_latex_output = runOne(key, verbose=False)
        output.append(single_latex_output)
    return output

def writeLatexToFile(latex_content:str, key:str):
    output_str = './_output/' + key + '_output.txt'
    with open(output_str, 'w') as file:
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
    tex_results = runAll()
    resultsToTxt(tex_results)