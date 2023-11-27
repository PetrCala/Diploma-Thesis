import re

import pandas as pd
import numpy as np

# DATA TRANSFORMATIONS - possibly move to a separate file
def boldIfPIPHigh(val):
    '''Convert high PIP values to bold before printing out the TeX object.
    This function is intended to be used with pandas DataFrame's apply method.
    '''
    if isinstance(val, (float, int)) and val >= 0.5:
        return '\\textbf{' + str(f'{val:.3f}') + '}'
    return val

def handleSpecial(df):
    '''Handle special cases.'''
    df = df.replace('\^2','$^2$', regex = True)
    df = df.replace('&', '\\&', regex = True) # Author2 \& Author1
    df = df.replace('%', '\\%', regex = True)
    return df

def fillNA(df):
    '''Replace NAs wil empty strings.'''
    df.fillna('', inplace = True)
    return df

def insertThousandSeparators(value):
    '''
    Insert comma in between the thousand separators.
    '''
    try:
        # Convert to float if it's a string
        if isinstance(value, str):
            value = float(value)

        # Format with commas
        return f"{value:,.0f}"
    except ValueError:
        raise ValueError(f"Cannot convert {value} to a float")

# STRING TRANSFORMATIONS

def renameLatex(latex:str, to_rename:list[tuple])->str:
    '''
    Given the latex string and a list of tuples (from-to), rename each tuple's from to the tuples'to. Return the modified string.
    '''
    for rename_from, rename_to in to_rename: # Iterate over a list of tuples (length 2)
        latex = re.sub(rename_from, rename_to, latex)
    return latex

def emphasizeRownames(latex:str, strings_to_emphasize:list[str])->str:
    '''
    Given a LaTeX string and a list of substrings to emphasize within, wrap all these strings in the LaTeX code ...

    Args:
        latex (str): LaTeX string to modify.
        strings_to_emphasize (list[str]): List of strings to emphasize within the main string

    Returns:
        str: The modified latex string.
    '''
    for string in strings_to_emphasize:
        new_string = "\emph{\hspace{0.2cm}" + string + "}"
        latex = latex.replace(string, new_string)
    return latex

def insertLinespace(latex:str, strings_to_insert_before:list[str])->str:
    '''Similar to 'emphasizeRownames'. Insert an emphasizeRownames in before a given string.'''
    for string in strings_to_insert_before:
        new_string = "\\addlinespace[0.5em]\n" + string
        latex = latex.replace(string, new_string)
    return latex

def insertSection(latex: str, insert_info:list[tuple], table_n_col:int)->str:
    '''Similar to 'insertLinespace'. Insert a section in before a given string.
    The input is a tuple where the first element represents the string to insert before, and the latter the name of the chapter to be inserted.

    Args:
    - latex (str): The main string to modify.
    - insert_info (list[tuple]): A list of tuples denoting the information to modify. Input in this format: (<insert-before-this-string>,<section-name>)
    - table_n_col (int): Number of columns the table has.
    '''
    for string, section_name in insert_info: # Iterate over a list of tuples (length 2)
        if not string in latex:
            raise ValueError(f"Trying to insert a section before a string {string} failed. The string could not be found within the transformed table.")
        #     \midrule
        # 
        # \multicolumn{7}{l}{\emph{Estimation method}}\\   
        new_string = "\\midrule\n\n\\multicolumn{" + str(table_n_col) + "}{l}{\\emph{" + section_name + "}}\\\\\n" + string
        latex = latex.replace(string, new_string)
    return latex
    
def translate(content:str)->str:
    '''Translate a string from a .txt format to a python string.'''
    # Replace all backslashes with double backslashes
    content = content.replace("\\", "\\\\")
    # Replace all line breaks with "\n" and merge the lines into one
    content = content.replace("\n", "\\n")
    return content

