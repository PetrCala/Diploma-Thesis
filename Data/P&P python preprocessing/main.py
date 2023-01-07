# Preprocess the original P&P data set into a usable excel file
# Create this excel file in the same folder under the name PP_processed.xlsx

from os.path import exists

import pandas as pd

BASE_PATH = r'C:\Users\hso20\OneDrive\Plocha\IES\Diploma-Thesis\Data\P&P python preprocessing'
SOURCE_FILE_NAME = 'PP_source.xlsx'
SOURCE_FILE_COLNAMES = ['country', 'year', 'region', 'income_level', 'years_of_schooling', 'overall',
    'prim', 'sec', 'higher', 'del1', 'del2', 'del3', 'del4', 'del5', 'del6', 'gender_male', 'gender_female',
    'private_sector', 'public_sector', 'source']
NEW_COLNAMES = ['obs_n', 'study_id', 'source', 'years_of_schooling', 'overall', 'prim', 'sec', 'higher',
    'gender_male', 'gender_female', 'private_sector', 'public_sector', 'country', 'year', 'region', 'income_level']
OUT_FILE_NAME = 'PP_preprocessed.xlsx'

def main(excel_out = True):
    df = loadData()
    df = orderColumns(df)
    df = dropRedundantRows(df)
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
    df = df.sort_values(by=['source', 'country']).reset_index(drop=True) # Order observations by studies, alphabetically
    df['obs_n'] = range(1, df.shape[0] + 1)
    df = df.assign(study_id=(df['source'] != df['source'].shift()).cumsum()) # Create study id column
    df = df[NEW_COLNAMES]
    return df

def dropRedundantRows(df):
    '''Drop rows without a schooling year info, or rows redundant from the discounting method.
    '''
    if not all(df.columns == NEW_COLNAMES[2:]):
        raise ValueError('Please handle the column names first')
    df = df.dropna(subset=['years_of_schooling'])
    df = df.dropna(subset=['overall', 'prim', 'sec', 'higher',
                'gender_male', 'gender_female', 'private_sector', 'public_sector'], how='all')
    return df

def orderColumns(df):
    '''Input the inital data set, then remove the full discounting method columns,
    and reorder the columns as desired. Return the new data set.
    '''
    base_cols = NEW_COLNAMES[2:]
    df = df.drop(columns=[col for col in df.columns if 'del' in col])
    # Shuffle and rename columns
    if not df.shape[1] == len(base_cols):
        raise ValueError(f'Incorrect list of new column names. The list must contain {df.shape[1]} names.')
    df = df[base_cols] # Without n_obs, study_id
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