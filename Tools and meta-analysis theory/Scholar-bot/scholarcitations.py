#--- Use a query to get citations of all studies, that the query finds

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


QUERY = '("ability bias" OR "intelligence bias") AND ("private returns") AND ("income" OR "earnings") AND ("schooling" OR "education")'

def main():
    driver = sel.openDriver()
    searchQuery(driver, QUERY) # Input and search query
    sel.handleCaptcha(driver)
    handleScholarPage(driver)

    # while nextPageAvailable(driver):
    #     goToNextPage(driver)
    #     studies_box = sel.getStudiesBox(driver)
    #     handleAllStudies(driver, studies_box)
        
    driver.quit()

    return None

def nextPageAvailable(driver):
    pass

def goToNextPage(driver):
    pass

def handleScholarPage(driver):
    '''While on a result google scholar page, identify all studies, get their citations,
    and store these.
    '''
    studies_box = sel.getStudiesBox(driver) # El of all studies
    all_studies_el = studies_box.find_elements(By.CLASS_NAME, st.STUDY_BOX_CLASS_NAME)
    for study_el in all_studies_el[:3]:
        citation = sel.getStudyCitation(driver, study_el)
        print(citation)


def searchQuery(driver, query):
    '''With an open driver, open the google scholar website and search using your query.
    '''
    website_open = sel.openWebsite(driver, st.SCHOLAR_SITE)
    if not website_open: # Google scholar timeout message
        return False
    sel.searchGoogleScholar(driver, query)
    try:
        WebDriverWait(driver, 10).until(EC.url_changes)
    except Exception as e:
        print('Query search failed:' + e)
        return False
    return True



if __name__ == '__main__':
    main()