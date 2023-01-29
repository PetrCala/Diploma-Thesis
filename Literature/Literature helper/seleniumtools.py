import os
from os.path import exists
import requests
import time
import re

from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
from selenium.common.exceptions import TimeoutException, NoSuchElementException
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from pynput.keyboard import Key

import static as st
from input import Input

I = Input()

def main():
    pass

#----- SELENIUM METHODS -----
def openDriver():
    '''Open the selenium driver.
    '''
    chrome_options = webdriver.ChromeOptions()
    chrome_options.add_argument('--service-log-path=/dev/null')
    chrome_options.add_experimental_option('excludeSwitches', ['enable-logging'])
    ser = Service(executable_path=ChromeDriverManager().install())
    return webdriver.Chrome(service=ser, options=chrome_options)

def openWebsite(driver, link:str):
    '''Input a selenium driver and a link and open a website with that driver.
    In case of a timeout, return False, otherwise return True.
    '''
    current_url = driver.current_url
    if not current_url == link:
        driver.get(link)
        try:
            WebDriverWait(driver,10).until(EC.url_to_be(link))
        except TimeoutException:
            return False
    return True

def searchGoogleScholar(driver, search_string):
    '''Search the google scholar for a specific string (citation).
    '''
    search_bar = driver.find_element(By.NAME, st.SEARCH_BAR_NAME) # Find the search bar element
    search_bar.send_keys(search_string)
    search_button = driver.find_element(By.NAME, st.SEARCH_BUTTON_NAME) # Search button
    search_button.click() # Click the search button
    return None

def getStudiesBox(driver):
    '''Return the element with all studies.
    '''
    try:
        studies_box = driver.find_element(By.ID, st.STUDIES_BOX_ID) # All studies
    except NoSuchElementException: # Captcha window open most probably
        captcha_handled = handleCaptcha(driver)
        if not captcha_handled:
            return False
        studies_box = driver.find_element(By.ID, st.STUDIES_BOX_ID) # Search again
    return studies_box

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
    studies_box = getStudiesBox(driver) # All studies
    study_box = getStudyBox(studies_box, searched_title) # Correct study
    if not study_box:
        return False
    try: # Get the PDF button
        link = study_box.find_element(By.XPATH, st.STUDY_BOX_PDF_BTN_XPATH)
    except Exception as e: # Get the study title instead (no PDF button)
        link = study_box.find_element(By.XPATH, st.STUDY_BOX_TITLE_XPATH) # Existence validated in getStudyBox
    link.click()
    WebDriverWait(driver,10).until(EC.url_changes)
    return True

def handleCaptcha(driver):
    '''Handle the captcha window. If handled successfully, return True,
    else return False.
    '''
    captcha_url = driver.current_url
    WebDriverWait(driver,20).until(EC.presence_of_element_located((By.ID, st.STUDIES_BOX_ID)))
    if captcha_url == driver.current_url:
        print('Captcha failed.')
        return False
    return True

def getStudyBox(studies_box, searched_title):
    '''Insert the box with all studies on the page, iterate through the first 3
    elements, and check, whether any matches the study title. If so, return
    the correct study box element. Else return False.
    '''
    for i in range(1,4):
        try:
            study_box = studies_box.find_element(By.XPATH, f"div[{i}]") # Get a new study
            link_el = study_box.find_element(By.XPATH, st.STUDY_BOX_TITLE_XPATH) # Get the title of that study
            found_title = str(link_el.text)
            searched_list, found_list = searched_title.split(), found_title.split()
            matches = [(i in found_list) for i in searched_list]
            if sum(matches)/len(searched_list) > 0.6:
                print(f'{searched_title} found.')
                return link_el
        except Exception as e:
            continue # Study not in this box, or only citation
    print(f'{searched_title} not found.')
    return False

def getStudyCitation(driver, study_box):
    '''Input a study box and open the cite window. Get the harvard citation text,
    close the window, and return the text as a string, or None if it was not found.
    '''
    cite_el = study_box.find_element(By.CLASS_NAME, st.STUDY_BOX_CITE_CLASS_NAME) # Get cite el
    cite_el.click()
    try:
        WebDriverWait(driver,5).until(EC.presence_of_element_located((By.CLASS_NAME, st.CITE_CLASS)))
        citation_div = driver.find_element(By.CLASS_NAME, st.CITE_CLASS)
        print(citation_div.get_attribute('class'), citation_div.get_attribute('text'))
        text = citation_div.text
        # Close the window
        I.useKey(Key.esc)
        # WebDriverWait(driver, 5).until(EC.element_to_be_clickable((By.CLASS_NAME, st.CITE_WINDOW_CLOSE_BUTTON_CLASS))).click()
        return text
    except Exception as e:
        print(e)
        return None

def downloadStudy(driver, author):
    '''After the study download page has been opened, download the study.
    If the download is successful, return True. Otherwise return False.
    '''
    url_ = driver.current_url
    if not 'pdf' in url_:
        pdf_link = openPDFPage(driver) # Get the PDF page
        if response is None:
            return False
        response = requests.get(pdf_link)
    else:
        response = requests.get(url_) # Already on PDF page
    if not exists(st.download_path):
        os.makedirs(st.download_path)
    pdf_download_path = st.download_path + str(f'\{author}.pdf')
    with open(pdf_download_path, "wb") as f:
        f.write(response.content)
    print(f'{author} downloaded.')
    return True

def openPDFPage(driver):
    '''Check the type of the (potentially downloadable) website, which the driver is
    currently on. Perform any necessary preparations/send inputs, until the page with
    a downloadable PDF is open. If this succeeds, return the pdf link (response).
    Otherwise return None.
    '''
    url_ = driver.current_url
    if 'sciencedirect' in url_:
        view_pdf_btn = driver.find_element(By.XPATH, st.SCIENCE_DIRECT_VIEW_PDF_XPATH)
        view_pdf_btn.click()
        WebDriverWait(driver,10).until(EC.url_changes)
        download_pdf_btn = driver.find_element(By.XPATH, st.SCIENCE_DIRECT_PDF_BTN_XPATH)
        download_link = download_pdf_btn.get_attribute('href')
        return download_link
    return False


if __name__ == '__main__':
    main()

