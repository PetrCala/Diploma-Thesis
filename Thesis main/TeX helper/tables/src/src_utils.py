import os
from typing import Tuple

def readFile(file_path:str)->str:
    '''Read a file based on the full path to it.'''
    if not os.path.exists(file_path):
        raise ValueError(f"The file {file_path} does not exist. Please check that the full path to this file is specified correctly.")
    with open(file_path, 'r') as file:
        # Read the contents of the file
        file_contents = file.read()
    return file_contents

def readSrcFiles(folder_name:str)->Tuple[str,str,str,str]:
    '''Define a name of a folder in the tables/src folder and read all the four files within it. Return these as a tuple.
    
    Usage:
    >>> folder_name = "effect_stats"
    >>> src_header, src_footer, new_header, new_footer = readSrcFiles(folder_name)
    '''
    current_folder = os.path.dirname(__file__)
    src_folder = os.path.join(current_folder, folder_name) # Assume this script lives within the 'tables/src' folder

    src_header_path = os.path.join(src_folder, "src_header.txt")
    src_footer_path = os.path.join(src_folder, "src_footer.txt")
    new_header_path = os.path.join(src_folder, "new_header.txt")
    new_footer_path = os.path.join(src_folder, "new_footer.txt")

    src_header = readFile(src_header_path)
    src_footer = readFile(src_footer_path)
    new_header = readFile(new_header_path)
    new_footer = readFile(new_footer_path)

    return src_header, src_footer, new_header, new_footer
