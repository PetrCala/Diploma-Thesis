from os.path import exists
import time
import re

from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from bs4 import BeautifulSoup

import static as st

def main():
    try:
        driver = openDriver()
        openWebsite(driver, st.SCHOLAR_SITE)
        searchGoogleScholar(driver, st.test_cite)
        time.sleep(2)
        driver.quit()
    except Exception as e:
        print("Exception occured:", e)
    return None

#----- SELENIUM METHODS -----
def openDriver():
    '''Open the selenium driver.
    '''
    chrome_options = webdriver.ChromeOptions()
    chrome_options.add_argument('--service-log-path=/dev/null')
    ser = Service(executable_path=ChromeDriverManager().install())
    return webdriver.Chrome(service=ser, options=chrome_options)

def openWebsite(driver, link:str):
    '''Input a selenium driver and a link and open a website with that driver.
    '''
    current_url = driver.current_url
    if not current_url == link:
        driver.get(link)
        WebDriverWait(driver,10).until(EC.url_to_be(link))
    return None

def searchGoogleScholar(driver, search_string):
    '''Search the google scholar for a specific string (citation).
    '''
    search_bar = driver.find_element(By.NAME, 'q') # Find the search bar element
    search_bar.send_keys(search_string)
    search_button = driver.find_element(By.NAME, 'btnG') # Search button
    search_button.click() # Click the search button
    return None

def openStudyForDownload(driver, desired_study_cite):
    '''Check whether the Google Scholar search results yielded the desired
    study on the first hit. If it is so, click the download button and return
    True, else return False.
    '''
    rgx = r'"(.*)"' # Extract between double quotes # Alternative - r'"([^"]*)"'
    match = re.search(rgx, desired_study_cite)
    if match is None:
        print('This study is not in the correct citation form.')
        return False
    searched_title = match.group(1) # Title of searched study
    studies_box = driver.find_element(By.ID, 'gs_res_ccl_mid') # All studies
    study_idx = getStudyIndex(studies_box, searched_title)
    if not study_idx:
        return False
    study_box = studies_box[study_idx] # Box with the desired study
    print('trying to get the pdf link...')
    link = study_box.find_element(By.XPATH, f"//*[@class='gs_ggs gs_fl']/div/div/a")
    print(f"{link} found")
    try:
        link.click()
        return True
    except Exception as e:
        print('Download button not found.')
        return False

def getStudyIndex(studies_box, searched_title):
    '''Insert the box with all studies on the page, iterate through the first 4
    elements, and check, whether any matches the study title. If so, return
    the index of the correct study box minus one. If not, return False.
    '''
    for i in range(1,3):
        link_el = studies_box.find_element(By.XPATH, f"div[{i}]//*[@class='gs_ri']/h3/a") # Move this down later
        try:
            found_title = str(link_el.text)
            searched_list, found_list = searched_title.split(), found_title.split()
            matches = [(i in found_list) for i in searched_list]
            if sum(matches)/len(searched_list) > 0.3:
                print(f'{searched_title} found.')
                return i - 1
        except Exception as e:
            continue # Study not in this box
    print(f'{searched_title} not found.')
    return False

def openForDownload(driver):
    '''Look for the download button, and click it, if it is available.
    If not, return False.
    '''
    pdf_button = driver.find_element(By.CSS_SELECTOR, st.first_pdf_button_css)
    pdf_button.click()

def readWebsite(driver):
    '''Get the BeautifulSoup of the current website.
    '''
    page_source = driver.page_source
    return BeautifulSoup(page_source,features='html.parser')


if __name__ == '__main__':
    main()

