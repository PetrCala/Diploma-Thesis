#---- A script for constructing the Google Scholar query
import re
from os.path import exists
import string # Punctuation handling
from collections import Counter

import pandas as pd

import static as st

def main():
    words = getAllWords()
    frequency_table = getFrequencyTable(words, descending=False)
    important_words = getImportantWords(frequency_table)
    return important_words

def getImportantWords(freq_table:dict, words = 10, verbose:bool = True):
    '''Input a frequency table, and return several of the most important words from it.
    :arg:
        freq_table (list) - The frequency table, as a dictionary.
        words (int, optional) - Number of words to return.
        verbose (bool, optional) - If True, print the table into the console.
    '''
    out = []
    while len(out) < words:
        key, value = freq_table.popitem()
        if key not in st.query_filler_words:
            out.append(key)
    if verbose:
        out_verbose = ", ".join([word.title() for word in out])
        print(f"Here are the {words} most common words: {out_verbose}.")
    return out

def getFrequencyTable(words:list, descending:bool=True):
    '''Input a list of all words across all study titles, and return a dictionary
    containing all words as keys, and their frequency (number of occurances) as values.
    If descending is True, return the frequency table in descending fashion by value count.
        Otherwise, return it in an ascending order.
    '''
    counts = Counter(words)
    frequency_table = {word: counts[word] for word in words}
    sorted_frequency_table = sorted(frequency_table.items(), key=lambda item: item[1], reverse=descending)
    return dict(sorted_frequency_table)

def getAllWords(get_titles_only=False):
    '''Return a list of all words, disregarding their uniqueness in relation to other studies.
    :arg:
        get_titles_only (bool) - If True, return a list of study titles, instead
            of all words.
    '''
    rgx = r'"([^"]*)"'
    lists_of_words = []
    titles = []
    studies_analyzed = 0 # Count number of studies in correct citation form
    lit = pd.read_excel(st.lit_file, sheet_name='P&P studies')
    citations = lit.loc[lit['Downloadable'], 'Cite'].to_list() # Citations of all downloadable studies
    for cite in citations:
        match = re.search(rgx, cite)
        if match:
            title = match.group(1)
            titles.append(title) # Save the whole title
            lists_of_words.append(title.split()) # All words from the title
            studies_analyzed += 1
        else:
            print(f"Failed to extract this citation: {cite}")
    # Unnest the list of words [['foo', 'foo'], ['foo', 'foo']] -> ['foo', 'foo', 'foo', 'foo']
    if studies_analyzed == len(citations):
        print(f'All study titles extracted successfully, out of a total of {studies_analyzed} studies.')
    else:
        print(f'{studies_analyzed} titles exctrated out of a total of {len(citations)} titles.')
    if get_titles_only:
        return titles
    words = [stripSpecialCharacters(word.lower()) for words in lists_of_words for word in words]
    return words

def stripSpecialCharacters(word:list):
    '''Input a word, and strip all special characters/punctuation.
    Return the updated word.
    '''
    translator = str.maketrans('','',string.punctuation)
    stripped = word.translate(translator)
    return stripped

if __name__ == '__main__':
    main()