import time
import re
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
    driver = sel.openDriver()
    handleStudy(driver, st.test_l)
    return None

def handleAllStudies(test:bool = True):
    '''Read the source excel file, read through all studies, check for their existence
    in the download folder, and try to download those studies, which have not been
    downloaded yet.
    :arg:
        test (bool) - Download only the first 3 studies of the excel file.
    '''
    downloaded = 0
    failed = []
    ll = getLookupList()
    driver = sel.openDriver()
    if test:
        ll = ll[:3] # Subset onto a sample set
    for l in ll:
        outcome = handleStudy(driver, l)
        if outcome:
            print(f'{l[0]} red and downloaded successfully.')
            downloaded += 1
        else:
            failed.append(l)
    handleFailedStudies(failed) # Print out the list of studies, that could not be downloaded, into a text file
    print(f'Study download complete.\nDownloaded {downloaded} studies.\nFailed to download {len(failed)} studies.')
    if failed != []:
        print(f'The list of failed download studies can be found in {st.failed_path}.')
    return None

def handleFailedStudies(l:list):
    '''Print out the list of studies, that could not be downloaded, into a text file
    '''
    if l == []:
        return None
    path = st.failed_path
    out_text = ''
    for study in l:
        name = study[0]
        cite = study[1]
        out = f'Study: {name}, Citation: {cite}\n'
        out_text += out
    with open(path, 'w') as f:
        f.write(out_text)
    return None

def handleStudy(driver, l:list):
    '''Handle a whole single study. If the study was downloaded, return True.
    Otherwise return False.
    :arg:
        driver - Selenium driver.
        l(list) - A list in the form [Author, Citation].
    '''
    if (not isinstance(l, list)) or (not len(l)==2):
        raise ValueError('Please specify the [author,citation] list correctly.')
    author, cite = l[0], l[1]
    already_downloaded = checkForExistence(author)
    if already_downloaded:
        print(f'{author} already downloaded.')
        return False
    sel.openWebsite(driver, st.SCHOLAR_SITE)
    sel.searchGoogleScholar(driver, st.test_cite)
    WebDriverWait(driver, 10).until(EC.url_changes)
    downloadable = sel.openStudyForDownload(driver, cite)
    if not downloadable:
        return False
    time.sleep(3)
    driver.quit()
    # except Exception as e:
    #     print("Exception occured:", e)
    return True
    
#----- OTHER METHODS -----

def validateStudy(cite:str):
    '''Input the citation of the study, and check, whether or not it is the first
    hit of the google scholar search.
    '''
    rgx = r'"(.*)"' # Extract between double quotes # Alternative - r'"([^"]*)"'
    match = re.search(rgx, cite)
    if match is None:
        print('This study is not in the correct citation form.')
        return False
    study_title = match.group(1)
    study_title_list = study_title.split()
    msg = I.readTextInRange(st.SCHOLAR_FIRST_STUDY_COORDS) # Scholar result
    matches = I.checkStringForMatches(msg, study_title_list)
    if matches > 3:
        print(f'Found the correct study title: {study_title}')
        return True
    print(f'Failed to find the title {study_title}')
    return False

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

def readExcel():
    '''Read the excel under the source path, defined in static 'st.lit_file'.
    '''
    return pd.read_excel(st.lit_file)

if __name__ == '__main__':
    main()