import os

import pandas as pd

from tables.utils.table_static import TABLES
from populate import populate_table
import static as static

pd.set_option('display.max_colwidth', 200) # Long descriptions

PROJECT_ROOT = os.path.dirname(__file__)
DT_ROOT = os.path.dirname(os.path.dirname(PROJECT_ROOT)) # Two folders up
NUMERIC_RESULTS_PATH = os.path.join(DT_ROOT, "R", "results", "numeric")

# TABLES_TO_RUN = ['linear_tests', 'ma', 'nonlinear_tests']
TABLES_TO_RUN = ["ma_variables_description_table"]

def readCSV(method_name:str):
    '''Specify the name of the method and read the file associated with
    this method. The name should correspond exactly to the .csv output file name in results/numeric.
    '''
    if not method_name in TABLES.keys():
        raise ValueError("Incorrectly specified method name.")
    full_path = os.path.join(NUMERIC_RESULTS_PATH, f"{method_name}.csv")
    df = pd.read_csv(full_path, encoding = 'utf-8', delimiter = ";")
    return df

def runMethod(method_name:str, df:pd.DataFrame):
    '''Specify the name of the method as a string as well as
    the data frame on which the method should be called and call the corresponding
    python function from the 'populate.py' script.
    '''
    if not method_name in TABLES.keys():
        raise ValueError("Incorrectly specified method name.")
    table_info = TABLES[method_name]
    latex_output = populate_table(
        df=df,
        table_verbose_name = table_info["name"],
        src_folder_name = method_name,
        colnames = table_info["colnames"],
        data_transformations = table_info["data_transformations"],
        string_transformations = table_info["string_transformations"],
        verbose = table_info["verbose"]
    )
    return latex_output

def runOne(key:str):
    '''Specify the name of the method and run this method.
    Return the LaTeX output.
    '''
    df = readCSV(key)
    latex_output = runMethod(key, df)
    return latex_output

def runAll():
    '''Read all data frames into memory and call all the functions
    on them.'''
    # Run all methods
    print("Processing all tables...")
    output = list()
    for key in TABLES_TO_RUN:
        single_latex_output = runOne(key)
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
    keys =  TABLES_TO_RUN
    if len(results_list) != len(keys):
        raise ValueError("Incorrect list dimensions.")
    # Iterate over all results and the corresponding keys
    for res, key in zip(results_list, keys):
        if res not in ["", None]:
            print(f"Writing {key} to a .txt file...")
            writeLatexToFile(res, key)

if __name__ == "__main__":
    verbose = False
    tex_results = runAll()
    resultsToTxt(tex_results)