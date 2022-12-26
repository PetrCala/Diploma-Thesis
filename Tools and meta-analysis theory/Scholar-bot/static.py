﻿import os

# Pathing
abs_path = os.getcwd() # Scholar-bot folder
if not (abs_path[-11:] == 'Scholar-bot'):
    raise ValueError('Wrong folder')
thesis_path  = os.path.dirname(os.path.dirname(abs_path)) # Diploma-thesis folder
if not (thesis_path[-14:] == 'Diploma-Thesis'):
    raise ValueError('Wrong thesis folder')
lit_file = thesis_path + str(r'\Literature\Literature.xlsx')
download_path = thesis_path + str(r'\P&P (2018) analysis\Studies')
# Path for the txt file where the names of failed download studies are stored
failed_path = thesis_path + str(r'\P&P (2018) analysis\Tools\failed_to_download.txt')

# Input handling
BROWSER_NAME = 'Google Chrome'
SCHOLAR_SITE = 'https://scholar.google.com/'

#----- SELENIUM TOOLS -----
SEARCH_BAR_NAME = 'q'
SEARCH_BUTTON_NAME = 'btnG'
STUDIES_BOX_ID = 'gs_res_ccl_mid' # ID for the box containing all studies from the page after query search
STUDY_BOX_PDF_BTN_XPATH = f"//*[@class='gs_ggs gs_fl']/div/div/a" # PDF button XPATH, from study box
STUDY_BOX_TITLE_XPATH = f"//*[@class='gs_ri']/h3/a" # Study title XPATH, from study box

# Test files
test_l = ['Agrawal (2011)', 'Agrawal, T.  2011. "Returns to Education in India: Some Recent Evidence." Indira Gandhi Institute of Development Research, Mumbai, WP-2011-017.']
test_ll = [
    ['Agrawal (2011)', 'Agrawal, T.  2011. "Returns to Education in India: Some Recent Evidence." Indira Gandhi Institute of Development Research, Mumbai, WP-2011-017.'],
    ['Asadullah (2006)', 'Asadullah, M.N. 2006. "Returns to Education in Bangladesh." Education Economics 14 (4): 453-468.'],
    ['Baraka (1999)', 'Baraka, J. 1999. "Returns to Education in Taiwan: A Cross-Sectional Cohort Analysis." Research Program in Development Studies, Princeton University. '],
    ['Belli and Ayadi (1998)', 'Belli, P. and M.A. Ayadi. 1998. "Returns to Investment in Education: The Case of Nicaragua." World Bank (mimeo).'],
    ['Brainerd (1998)', 'Brainerd, E. 1998. "Winners and losers in Russia\'s economic transition." American Economic Review 1094-1116.'],
]
test_name ='Agrawal (2011)' 
test_cite = 'Agrawal, T. 2011. "Returns to Education in India: Some Recent Evidence." Indira Gandhi Institute of Development Research, Mumbai, WP-2011-017.'
