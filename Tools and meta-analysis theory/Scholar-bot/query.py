#---- A script for constructing the Google Scholar query
import re
from os.path import exists
import string # Punctuation handling
from collections import Counter

import pandas as pd

import static as st

def main():
    words = getAllWords()
    frequency_table = getFrequencyTable(words)
    print(frequency_table)

def getFrequencyTable(words:list):
    '''Input a list of all words across all study titles, and return a dictionary
    containing all words as keys, and their frequency (number of occurances) as values.
    '''
    counts = Counter(words)
    frequency_table = {word: counts[word] for word in words}
    sorted_frequency_table = sorted(frequency_table.items(), key=lambda item: item[1], reverse=True)
    return dict(sorted_frequency_table)

def getAllWords(get_titles_only=False):
    '''Return a list of all words, disregarding their uniqueness in relation to other studies.
    :arg:
        get_titles_only (bool) - If True, return a list of study titles, instead
            of all words.
    '''
    rgx = r'"([^"]*)"'
    lists_of_words = []
    studies_analyzed = 0 # Count number of studies in correct citation form
    lit = pd.read_excel(st.lit_file, sheet_name='P&P studies')
    citations = lit['Cite'].to_list()
    for cite in citations:
        match = re.search(rgx, cite)
        if match:
            title = match.group(1)
            lists_of_words.append(title.split()) # All words from the title
            studies_analyzed += 1
    words = [stripSpecialCharacters(word.lower()) for words in lists_of_words for word in words]
    print(f'{studies_analyzed} titles exctrated out of a total of {len(citations)} titles.')
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