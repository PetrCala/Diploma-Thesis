import os

# Pathing
abs_path = os.getcwd() # Scholar-bot folder
if not (abs_path[-11:] == 'Scholar-bot'):
    raise ValueError('Wrong folder')
thesis_path  = os.path.dirname(os.path.dirname(abs_path)) # Diploma-thesis folder
if not (thesis_path[-14:] == 'Diploma-Thesis'):
    raise ValueError('Wrong thesis folder')
lit_file = thesis_path + str(r'\Literature\Literature.xlsx')
download_path = thesis_path + str(r'\P&P (2018) analysis\Original studies\download')
# Path for the txt file where the names of failed download studies are stored
failed_path = thesis_path + str(r'\P&P (2018) analysis\Original studies\failed_to_download.txt')
# Selenium driver path
# driver_path = thesis_path + str(r'\Tools and meta-analysis theory\Study download bot\chromedriver.exe')

# Input handling
BROWSER_NAME = 'Google Chrome'
SCHOLAR_SITE = 'https://scholar.google.com/'

SCHOLAR_FIRST_STUDY_COORDS = [0.154, 0.265, 0.523, 0.316] # Title of the first study hit
SCHOLAR_LINK_CORODS = [0.59, 0.262, 0.711, 0.3]
SCHOLAR_LINK_CLICK = [0.622, 0.278]
DOWNLOAD_COORDS_DICT = {
    # Classic download button
    'pitt.edu': [0.925, 0.149],
    'uc3m.es': [0.925, 0.149],
    'core.ac.uk': [0.925, 0.149],

}
CHROME_SEARCH_BAR = [0.194, 0.063]
SCHOLAR_SEARCH_BAR = [0.382, 0.368]

#----- SELENIUM TOOLS -----
first_study_css = '.gs_r' # CSS selector of the first study hit by the Google Scholar search engine
pdf_button_css = '.gs_md_wp a' # First PDF button on the website

# Test files
test_l = ['Agrawal (2011)', 'Agrawal, T.  2011. "Returns to Education in India: Some Recent Evidence." Indira Gandhi Institute of Development Research, Mumbai, WP-2011-017.']
test_name ='Agrawal (2011)' 
test_cite = 'Agrawal, T. 2011. "Returns to Education in India: Some Recent Evidence." Indira Gandhi Institute of Development Research, Mumbai, WP-2011-017.'
