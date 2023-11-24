import pandas as pd
import numpy as np

def boldIfPIPHigh(val):
    '''Convert high PIP values to bold before printing out the TeX object.
    '''
    if isinstance(val, (float, int)) and val >= 0.5:
        return '\\textbf{' + str(f'{val:.3f}') + '}'
    return val

def handleSpecial(df):
    '''Handle special cases.'''
    df = df.replace('\^2','$^2$', regex = True)
    df = df.replace('&', '\\&', regex = True) # Author2 \& Author1
    df = df.replace('%', '\\%', regex = True)
    df.fillna('', inplace = True)
    return df

def renameLatex(latex:str, to_rename:list[tuple])->str:
    '''
    Given the latex string and a list of tuples (from-to), rename each tuple's from to the tuples'to. Return the modified string.
    '''
    for rename_this in to_rename:
        if not len(rename_this) == 2:
            raise ValueError("Each value to rename must be given as a tuple containing exactly two elements - from and to.")
        rename_from, rename_to = rename_this[0], rename_this[1]
        latex = latex.replace(rename_from, rename_to)
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
    
def translate(content:str)->str:
    '''Translate a string from a .txt format to a python string.'''
    # Replace all backslashes with double backslashes
    content = content.replace("\\", "\\\\")
    # Replace all line breaks with "\n" and merge the lines into one
    content = content.replace("\n", "\\n")
    return content

