# Preprocess the original P&P data set into a usable excel file
# Create this excel file in the same folder under the name PP_processed.xlsx

from os.path import exists

import pandas as pd
import numpy as np

BASE_PATH = r'C:\Users\hso20\OneDrive\Plocha\IES\Diploma-Thesis\Data\P&P python preprocessing'
SOURCE_FILE_NAME = 'PP_source.xlsx'
SOURCE_FILE_COLNAMES = ['country', 'year', 'region', 'income_level', 'years_of_schooling', 'overall',
    'prim', 'sec', 'higher', 'del1', 'del2', 'del3', 'del4', 'del5', 'del6', 'gender_male', 'gender_female',
    'private_sector', 'public_sector', 'source']
INITIAL_COLNAMES = ['source', 'years_of_schooling', 'overall', 'prim', 'sec', 'higher',
    'gender_male', 'gender_female', 'private_sector', 'public_sector', 'country', 'year', 'region', 'income_level']
OUT_FILE_NAME = 'PP_preprocessed.xlsx'

def main(excel_out = True):
    df = loadData()
    df = orderColumns(df)
    df = dropRedundantRows(df)
    df = spreadEffects(df)
    df = sortAndIndex(df)
    if excel_out:
        excelOut(df)
    print(df.head())
    print(df.shape)
    return df


def excelOut(df):
    '''Create an excel file with the new dataset.
    '''
    path = str(BASE_PATH) + f'\{OUT_FILE_NAME}'
    df.to_excel(path, index = False)
    print('New excel file created successfully.')
    return None

def sortAndIndex(df):
    '''Sort the data frame rows and add an index.
    '''
    sort_order = ['source', 'country', 'public_sector', 'private_sector', 'gender_female', 'gender_male',
        'higher', 'sec', 'prim', 'overall', 'year']
    df = df.sort_values(by=sort_order).reset_index(drop=True) # Order observations by studies, alphabetically
    df.insert(0, 'obs_n', range(1, df.shape[0] + 1))
    df.insert(1, 'study_id', (df['source'] != df['source'].shift()).cumsum())
    return df

def spreadEffects(df):
    '''Transform the estimates of the original data frame each into their own rows,
    and store these under a new collumn 'effect'. Instead of the columns containing
    the original estimates, insert columns including dummies, indicating, whether the
    effect corresponds to that category.
    '''
    new_cols = INITIAL_COLNAMES[:2] + ['effect'] + INITIAL_COLNAMES[2:]

    # Define various columns
    id_cols = ['source', 'years_of_schooling', 'country', 'year', 'region', 'income_level']
    value_cols =  ['overall', 'prim', 'sec', 'higher', 'gender_male', 'gender_female',
                                        'private_sector', 'public_sector']

    # Melt the df to turn the data columns into rows
    melted_df = df.melt(id_vars = id_cols, value_vars = value_cols,
                        var_name = 'source_col', value_name = 'effect')

    # Drop all rows where effect is missing
    melted_df = melted_df.dropna(axis = 0, subset = ['effect']) 

    # Dummify the source column information
    for col in value_cols:
        melted_df[col] = melted_df['source_col'].apply(lambda x: 1 if x == col else 0)
    melted_df = melted_df.drop('source_col', axis = 1) # Get rid of the temporary column

    # Reorder the columns
    melted_df = melted_df[new_cols]
    return melted_df

def dropRedundantRows(df):
    '''Drop rows without a schooling year info, or rows redundant from the discounting method.
    '''
    if not all(df.columns == INITIAL_COLNAMES):
        raise ValueError('Please handle the column names first')
    df = df.dropna(subset=['years_of_schooling'])
    df = df.dropna(subset=['overall', 'prim', 'sec', 'higher',
                'gender_male', 'gender_female', 'private_sector', 'public_sector'], how='all')
    return df

def orderColumns(df):
    '''Input the inital data set, then remove the full discounting method columns,
    and reorder the columns as desired. Return the new data set.
    '''
    df = df.drop(columns=[col for col in df.columns if 'del' in col])
    # Shuffle and rename columns
    if not df.shape[1] == len(INITIAL_COLNAMES):
        raise ValueError(f'Incorrect list of new column names. The list must contain {df.shape[1]} names.')
    df = df[INITIAL_COLNAMES]
    return df

def loadData():
    path = str(BASE_PATH) + f'\{SOURCE_FILE_NAME}'
    if not exists(path):
        raise ValueError('Source file untraceable')
    file = pd.read_excel(path, skiprows=9)
    file.columns = SOURCE_FILE_COLNAMES
    return file

if __name__=='__main__':
    main()