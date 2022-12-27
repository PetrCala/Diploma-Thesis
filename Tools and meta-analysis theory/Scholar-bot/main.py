import random
import time
import openpyxl
from os.path import exists

from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import pandas as pd

import static as st
import seleniumtools as sel

# Selenium

def main():
    # driver = sel.openDriver()
    # handleAllStudies(driver, test=False)
    # driver.quit()
    getAlreadyDownloadedList(update_excel=True)

    # handleSampleStudy(driver)
    return None

def handleAllStudies(driver, test:bool = True):
    '''Read the source excel file, read through all studies, check for their existence
    in the download folder, and try to download those studies, which have not been
    downloaded yet.
    :arg:
        test (bool) - Download only the first 3 studies of the excel file.
    '''
    downloaded = 0 # Downloaded insided this function
    already_downloaded = 0 # Downloaded beforehand
    new_failed = []
    ll = getLookupList() # [Number, Author, Cite]
    # Sample set
    if test:
        ll = ll[:200]
    # Existing failed studies
    with open(st.failed_path, 'r', encoding='utf-8') as f:
        failed_studies = f.readlines()
        failed_studies = [i.replace('\n','') for i in failed_studies] # Remove 'newline'
    # Main loop
    for l in ll:
        if l[0] in failed_studies: # Failed study
            continue
        outcome, fail_reason = handleStudy(driver, l)
        if fail_reason == 'EXIT':
            print('Process terminated.')
            break
        elif outcome is True: # Downloaded
            print(f'{l[0]} red and downloaded successfully.')
            downloaded += 1
            time.sleep(random.uniform(0.5, 2)) # Wait 
        elif (outcome is False) and (fail_reason is not None): # Failed to download
            l.append(fail_reason) # Now [number, author, cite, fail reason]
            new_failed.append(l)
            time.sleep(random.uniform(0.5, 2)) # Wait 
        elif outcome is None: # Download not necessary
            already_downloaded += 1
    handleFailedStudies(new_failed) # Print out the list of studies, that could not be downloaded, into a text file
    print(f'Study download complete.\nDownloaded {downloaded} new studies.')
    print(f'Found {already_downloaded} already downloaded studies.')
    print(f'Failed to download {len(failed_studies)} old, and {len(new_failed)} new studies.')
    if new_failed != []:
        print(f'The list of all studies that failed to download can be found in {st.failed_path}.')
    return None

def handleFailedStudies(l:list):
    '''Print out the list of studies, that could not be downloaded, into a text file
    '''
    if l == []:
        return None
    try:
        with open(st.failed_path, 'r', encoding='utf-8') as f:
            failed_studies = f.readlines()
    except FileNotFoundError:
        with open(st.failed_path, 'w', encoding='utf-8') as f:
            f.write('')
            failed_studies = []
    for study in l:
        name = study[0] + '\n'
        if name in failed_studies: # Study name already in the failed studies file
            continue
        failed_studies.append(name)
    failed_studies.sort() # Alphabetic sorting
    with open(st.failed_path, 'w', encoding='utf-8') as f:
        f.writelines(failed_studies)
    return None

def handleStudy(driver, l:list):
    '''Handle a whole single study. If the study was downloaded, return True.
    Otherwise return False.
    :arg:
        driver - Selenium driver.
        l(list) - A list in the form [Author, Citation].
    :return:
        outcome (bool) - True/False, depending on the download outcome.
            None, if already downloaded.
        fail_reason (str/None) - Reason for the unsuccessful download.
    '''
    if (not isinstance(l, list)) or (not len(l)==2):
        raise ValueError('Please specify the [author,citation] list correctly.')
    author, cite = l
    already_downloaded = checkForExistence(author)
    if already_downloaded:
        return None, None
    website_open = sel.openWebsite(driver, st.SCHOLAR_SITE)
    if not website_open: # Google scholar timeout message
        return False, 'EXIT'
    sel.searchGoogleScholar(driver, cite)
    WebDriverWait(driver, 10).until(EC.url_changes)
    downloadable = sel.openStudyForDownload(driver, cite)
    if not downloadable:
        return False, 'No download button on Google Scholar'
    downloaded = sel.downloadStudy(driver, author)
    if not downloaded:
        return False, 'Failed to download the study.'
    return True, None
    
#----- OTHER METHODS -----
def checkForExistence(author:str):
    '''Input the name of the study, and check, whether this study has been downloaded yet.
    If yes, return True, otherwise return False.
    '''
    path = st.download_path
    pdf_path = f'{path}\{author}.pdf'
    txt_path = f'{path}\{author}.txt'
    if exists(pdf_path) or exists(txt_path):
        return True
    return False

def getLookupList():
    '''Get the list of lists in the form [author, citation].
    '''
    excel = readExcel()
    ll = [] # Lookup list
    for iterrow in excel.iterrows():
        row = iterrow[1]
        author = row['Label']
        citation = row['Cite']
        out = [author, citation]
        if not isinstance(citation, float): # [nan, nan]
            ll.append(out)
    return ll

def getAlreadyDownloadedList(update_excel=True):
    '''Read the Literature excel file, get the list of all study names,
    and return a boolean list indicating the download status of all studies.
    True = already downloaded in P&P\Studies, False = not downloaded.
    :arg:
        update_excel (bool, Default False) - If true, create a .xlsx file in the
            Tools folder, which will contain the boolean list.
    '''
    dwnl_bool = [] # List for storing booleans
    excel = readExcel()
    authors = excel['Label'].to_list()
    for author in authors:
        path = st.download_path + str(f'\{author}.pdf')
        dwnl_bool.append(exists(path)) # True if already downloaded, False otherwise
    if update_excel:
        print('Opening the literature file for editing...')
        lit = openpyxl.load_workbook(st.lit_file)
        sheet = lit['P&P studies']
        col_letter = 'J' # Column to update
        # Source file malformatting check
        if not sheet[f'{col_letter}1'].value == 'Already downloaded':
            return ValueError('Please check the literature excel formatting.')
        range_length = len(dwnl_bool) + 1 # Last row to modify
        # Get the range of cells in the desired column
        column_range = f'{col_letter}2:{col_letter}{range_length}'
        cell_tuples = sheet[column_range] # (cell, ), (cell2, ),...
        column_cells = [i[0] for i in cell_tuples] # First elements of list of tuples
        # Update values
        for cell, new_value in zip(column_cells, dwnl_bool):
            cell.value = new_value
        lit.save(st.lit_file)
        print('Literature file updated.')
    return dwnl_bool

def readExcel():
    '''Read the excel under the source path, defined in static 'st.lit_file'.
    '''
    return pd.read_excel(st.lit_file)

def handleSampleStudy(driver):
    '''Handle a sample study using the samples in static.py.
    '''
    ll = st.test_ll
    sample_l = random.choice(ll)
    handleStudy(driver, sample_l)
    return True

if __name__ == '__main__':
    main()