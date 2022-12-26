import random
import time
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
    handleAllStudies(driver, test=False)
    driver.quit()
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
    failed = []
    ll = getLookupList() # [Number, Author, Cite]
    if test:
        ll = ll[:3] # Subset onto a sample set
    for l in ll:
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
            failed.append(l)
            time.sleep(random.uniform(0.5, 2)) # Wait 
        elif outcome is None: # Download not necessary
            already_downloaded += 1
    handleFailedStudies(failed) # Print out the list of studies, that could not be downloaded, into a text file
    print(f'Study download complete.\nDownloaded {downloaded} new studies.')
    print(f'Found {already_downloaded} already downloaded studies.\nFailed to download {len(failed)} studies.')
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
        fail_reason = study[2]
        out = f'Study: {name}, Citation: {cite}, Fail reason: {fail_reason}\n'
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
        print(f'{author} already downloaded.')
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
        return False, 'Failed to click the download button'
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