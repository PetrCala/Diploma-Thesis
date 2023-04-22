#----- A python script to help preprocess literature information, and extract useful pieces of information,
# such as the author, year of publication, etc.

#### HOW TO: ####
# 1. Place the list of study citations in the empty text file 'citations_source.txt'. Place the list of
#   titles in the file "titles_source".
# 2. Run the script
# 3. A new file will be created, 'output.csv', where the information will be stored

import regex as re

import pandas as pd


SOURCE_NAME = 'source'
CITATION_LEN = 55 # Manually input the number of citations  you want to handle - check for validity of the file

R_NAME = r'([\p{L}]+(?:\s[\p{L}]\.)?)' # Matches Newman L., or Newman, with any special characters included

# Two authors
A2_RGX = f'{R_NAME}, {R_NAME}, and {R_NAME} {R_NAME}'

# Three or more authors
A3_FIRST_AUTHOR = f'{R_NAME}, {R_NAME}, '
A3_ADDITIONAL_AUTHOR = f'{R_NAME} {R_NAME}, '
A3_LAST_AUTHOR = f'and {R_NAME} {R_NAME}'

# Cite year regex
PUB_YEAR_RGX_1 = r'\((\d{4})\)' # foo (2020) foo
PUB_YEAR_RGX_2 = r'\s(\d{4})\.$' # foo 2020.end-of-line
PUB_YEAR_RGX_3 = r', (\d{4}),' # foo, (2020), foo
PUB_YEAR_RGXS = [PUB_YEAR_RGX_1, PUB_YEAR_RGX_2, PUB_YEAR_RGX_3]
PUB_YEAR_MISSING_RGX = r'(\d{4})' # No year information in the citation


def main():
    doAll('titles_source', 'citations_source')


def doAll(titles_file, citations_file):
    '''Input the names of the two files and create a .csv file in the same folder with
    all data neatly handled.
    '''
    print('Preprocessing all of the data...')
    # Preprocess the citations
    correct_citations = quoteCitations(titles_file, citations_file)

    # Get various data
    authors = extractAuthors(correct_citations)
    years = extractYear(correct_citations)
    labels = extractLabel(authors, years)
    titles = readSource(titles_file)

    # Create the pandas data frame
    df = pd.DataFrame({'Label': labels,
     'Author': authors, 
     'Year': years, 
     'Study title': titles,
     'Citation': correct_citations,
     })

    # Create the .csv file
    df = df.to_csv(sep = ';', header=None, index=False).strip('\n').split('\n')
    df_string = '\n'.join(df)  # <= this is the string that you can use with md5
    df_bytes = df_string.encode('utf8')  # <= this is bytes object to write the file
    with open('data/output.txt', 'wb') as f:
        f.write(df_bytes)
    print('Created the output file in the folder \"data\"')
    return df

def extractAuthors(citations):
    '''Return a list of authors of all the citations.
    '''
    authors = []
    unhandled = []

    for citation in citations:
        match = re.search('^[^"]*', citation) # Extract raw author information
        if not match: # Incorrect citation form
            continue
        raw_author_text = match.group()
        author = raw_author_text.replace(". ","").replace(";",",").replace("\ufeff","").title()
        # Single author case
        if not " And " in author:
            surname_match = re.search('^[^,]*', author)
            if not surname_match: # Funky name format, handle later
                continue
            # More commas
            surname = surname_match.group()
            if author.count(',') > 1: # Can be "author, author, author", or "author, et al"
                new_name = f'{surname} et al.'
                authors.append(new_name)
                continue
            authors.append(surname) # Single author, all standard
            continue
        # Multiple author case
        # Two authors
        a2_match = re.search(A2_RGX, author)
        if a2_match:
            author1 = a2_match.group(1)
            author2 = a2_match.group(4)
            new_name = f'{author1} & {author2}'
            authors.append(new_name)
            continue
        if author.count(',') == 2:
            author_split = author.split()
            author1 = author_split[0]
            author2 = author_split[-1]
            new_name = f'{author1} & {author2}'
            authors.append(new_name)
            continue
        # Three or more authors
        a3_iterator = 1
        a3_found = False
        while a3_iterator < 5:
            a3_rgx = A3_FIRST_AUTHOR + a3_iterator*A3_ADDITIONAL_AUTHOR + A3_LAST_AUTHOR
            a3_match = re.search(a3_rgx, author)
            if a3_match:
                author_name = a3_match.group(1)
                new_name = f'{author_name} et al.'
                authors.append(new_name)
                a3_found = True # Mark author found
                a3_iterator = 5 # Exit the iteration
            a3_iterator += 1
        if a3_found:
            continue
        if author.count(',') > 2:
            surname_match = re.search('^[^,]*', author) 
            if surname_match:
                surname = surname_match.group()
                new_name = f'{surname} et al.'
                authors.append(new_name)
                continue
        # Unhandled
        authors.append(author)


    missing = len(citations) - len(authors)
    if missing != 0:
        print(f'Missing {missing} authors...')
    return authors

def extractYear(citations):
    years = []
    for citation in citations:
        # Regular formats
        for rgx in PUB_YEAR_RGXS:
            year_match = re.search(rgx, citation)
            if year_match:
                year = year_match.group(1)
                years.append(year)
                break
        # No match found - no break called
        else:
            # No year number in citation
            any_year_in_citation = re.search(PUB_YEAR_MISSING_RGX, citation)
            if not any_year_in_citation:
                years.append(None)
                continue
            # Unhandled search pattern
            years.append('X')

    missing = years.count('X')
    if missing != 0:
        print(f'Missing {missing} years...')
    return years

def extractLabel(authors, years):
    labels = []
    if not len(authors) == len(years):
        raise ValueError('Mising some observations...')
    for author, year in zip(authors, years):
        label = f'{author} ({year})'
        labels.append(label)
    return labels


def quoteCitations(titles_file:str, citations_file:str, verbose = True):
    '''Input the name of titles file, and the citations file, and quote the titles around
    citations, which are not quoted yet.
    Return the list of quoted citations.
    '''
    # Read the source files - automatically validates length
    titles_source = readSource(titles_file)
    citations_source = readSource(citations_file)

    # Merge to a single list
    source = list(zip(titles_source, citations_source))
    quoted_list = []
    unhandled = []

    # Loop tools
    rgx = r'"(.*)"' # Extract between double quotes # Alternative - r'"([^"]*)"'

    print('Quoting all citations...')
    # Main loop
    for s in source:
        title, fancy_citation = s[0], s[1] # Unpack
        citation = fancy_citation.replace("‘", "'").replace("’", "'") # Replace fancy quotes
        match = re.search(rgx, citation)
        # Double quotes
        if match:
            searched_title = match.group(1)
            # Everything in order
            if title in searched_title:
                quoted_list.append(citation) # Correct citation form 
                continue
            # A little off - several words etc.
            a,b = searched_title.split(), title.split()
            a_matching = [i in b for i in a] # Words in a match words in b
            if sum(a_matching)/len(a_matching) > 0.7: # A very similar title
                quoted_list.append(citation)
                continue
            # Unhandled case with double quotes
            unhandled.append(citation) # Unhandled cases, unknown cause of misbehavior
            continue
        # No double quotes
        match = re.search(re.escape(title), citation) # re.escape to avoid handle special characters
        if match: # Citation contains the title
            new_citation = re.sub(re.escape(title), f'"{title}"', citation)
            new_citation = new_citation.replace("\".", ".\"") # Fix study title endings
            quoted_list.append(new_citation)
            continue
        # Title not inside citation, or any other unhandled case
        unhandled.append(citation)
        
    if not len(unhandled) == 0:
        if verbose:
            print(f'The citation list is still incomplete. {len(unhandled)} citations missing...')
        return False
    print('All citations quoted succesfully. :)')
    return quoted_list

def readSource(source_name):
    with open(f'{source_name}.txt', "r", encoding="utf-8") as f:
        lines = f.readlines()
    if not len(lines) == CITATION_LEN:
        raise ValueError(f'The source file has an incorrect number of rows.\nExpected: {CITATION_LEN}\nGot: {len(lines)}')
    lines = [item.replace('\n','') for item in lines] # Remove \n
    return lines

if __name__ == '__main__':
    main()
    
