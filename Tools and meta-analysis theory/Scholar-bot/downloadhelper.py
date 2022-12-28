#----- Download single studies, or single studies in unusual format
import requests

import static as st


def main():
    link = 'https://read.oecd-ilibrary.org/economics/oecd-economic-surveys-iceland-2006/adapting-the-education-system-to-a-changing-environment_eco_surveys-isl-2006-7-en'
    downloadStudyPDF(link)

    book_name = 'Earnings inequality, unemployment, and poverty in the Middle East and North Africa'
    # downloadStudyBook(book_name)

def downloadStudyPDF(link:str = None):
    '''Download a study into the Tools folder using the requests package.
    The link provided must lead to the pdf page of said study.
    '''
    if link is None:
        link = st.test_link
    download_path = st.tools_path + f'\{st.test_name}.pdf'
    response = requests.get(link)
    with open(download_path, "wb") as f:
        f.write(response.content)
    print(f'{st.test_name} downloaded.')
    return True 

def downloadStudyBook(book_name:str):
    '''Input a name of a book as a string, and attempt to download this book
    into the Tools folder. 
    '''
    download_path = st.tools_path + str(f"\{book_name}.pdf")
    # Step 1: Search for the book on Google Books using the Google Books API
    search_url = f"https://www.googleapis.com/books/v1/volumes?q={book_name}"
    r = requests.get(search_url)

    # Step 2: Parse the JSON response to get the URL of the book in PDF format
    book_data = r.json()
    pdf_url = book_data['items'][0]['accessInfo']['pdf']['acsTokenLink']

    # Step 3: Send a GET request to the URL of the book to download the PDF file
    r = requests.get(pdf_url)
    
    # Step 4: Save the PDF file to a location on your computer
    with open(download_path, "wb") as f:
        f.write(r.content)
        
    print(f"{book_name} downloaded.")
    return True

if __name__=="__main__":
    main()