import pandas as pd

from tables.utils.classes import TABLE_PROCESSOR
from tables.src.src_utils import readSrcFiles

def populate_table(
    df:pd.DataFrame,
    table_verbose_name:str,
    src_folder_name:str,
    colnames:list[str],
    data_transformations: dict[str,any],
    string_transformations: dict[str,any],
    verbose:str = False
)->str:
    '''
    Using a source DF and a list of instructions, convert this data frame into a LaTeX string and modify it to be directly usable in a .tex file. Return this string.

    Args:
    - df (pd.DataFrame): Source DF, as obtained directly from the R output (results/numeric).
    - table_verbose_name (str): Verbose name of the table to populate
    - src_folder_name (str): Name of the folder where the header/footer .txt files for this folder are specifiec in the 'src' folder.
    - colnames (list[str]): List of column names to use from the source DF.
    - data_transformations (dict[str,any]): Transformations to apply to the source data frame after it has been subsetted to the desired columns, specified as a dictionary.
    - string_transformations (dict[str,any]): Transformations to apply to the latex string after the header/footer parts are replaced, specified as a dictionary.
    - verbose (str, optional): If true, print verbose output about the transfomraiton. Defaults to False.

    Returns:
        str: The formatted latex string that can be used directly in a .tex file.
    '''
    src_header, src_footer, new_header, new_footer = readSrcFiles(src_folder_name)
    # Validate input
    if len(colnames) < 1:
        raise ValueError(f"You must specify at least one column to keep for table '{table_verbose_name}'")
    # Initialize the processing class
    processor = TABLE_PROCESSOR(
        df=df,
        name=table_verbose_name,
        colnames=colnames,
        data_transformations = data_transformations,
        string_transformations = string_transformations,
    )
    # Transform the stored and validated data frame into a LaTeX string
    latex = processor.transform(
        src_header=src_header,
        src_footer=src_footer,
        new_header=new_header,
        new_footer=new_footer
    )
    if verbose:
        print(latex)
    return(latex)

