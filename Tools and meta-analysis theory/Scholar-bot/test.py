import re
import os
from os.path import exists
import requests

from main import checkForExistence
import static as st

def main():
    # checkDummyForExistence()
    # checkTitleRegex()
    link = 'https://www.sciencedirect.com/science/article/pii/S0304387805001380/pdfft?isDTMRedir=true&download=true'
    downloadTestStudy(link)

def downloadTestStudy(link = None):
    '''Download a study into the Tools folder using the requests package.
    '''
    if link is None:
        link = st.test_link
    download_path = st.tools_path + f'\{st.test_name}.pdf'
    response = requests.get(link)
    with open(download_path, "wb") as f:
        f.write(response.content)
    print(f'{st.test_name} downloaded.')
    return True 

def checkTitleRegex():
    '''Check the functionality of the study title lookup regex.
    '''
    study_cite = 'Agrawal, T. 2011. "Returns to Education in India: Some Recent Evidence." Indira Gandhi Institute of Development Research, Mumbai, WP-2011-017.'
    rgx = r'"([^"]*)"'
    match = re.search(rgx, study_cite)
    if not match is None:
        return match.group(1)
    raise ValueError('checkTitleRegex function is misspecified')

def checkDummyForExistence():
    '''Read a dummy list [author, citation] from the static file, and verify
    that the method checkForExistence works.
    '''
    test_l = st.test_l
    test_author = test_l[0]
    outcome = checkForExistence(test_author)
    if outcome:
        app_str = 'not '
    print(f'The study {test_author} was {app_str}found.')
    return True


if __name__ == "__main__":
    main()