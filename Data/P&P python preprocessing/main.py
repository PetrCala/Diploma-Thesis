# Preprocess the original P&P data set into a usable excel file
# Create this excel file in the same folder under the name PP_processed.xlsx

from os.path import exists

import pandas as pd

SOURCE_FILE_NAME = 'PP_source.xlsx'
SOURCE_FILE_COLNAMES = ['country', 'year', 'region', 'income_level', 'years_of_schooling', 'overall',
    'prim', 'sec', 'higher', 'del1', 'del2', 'del3', 'del4', 'del5', 'del6', 'gender_male', 'gender_female',
    'private_sector', 'public_sector', 'source']
NEW_COLNAMES = ['obs_n', 'study_id', 'source', 'years_of_schooling', 'overall', 'prim', 'sec', 'higher',
    'gender_male', 'gender_female', 'private_sector', 'public_sector', 'country', 'year', 'region', 'income_level']

def main(excel_out = True):
    df = loadData()
    df = orderColumns(df)
    df = dropRedundantRows(df)
    if excel_out:
        excelOut(df)
    print(df.head())
    print(df.shape)
    return df


def excelOut(df):
    '''Create an excel file with the new dataset.
    '''
    df.to_excel('PP_preprocessed.xlsx', index = False)
    print('New excel file created successfully.')
    return None

def dropRedundantRows(df):
    '''Drop rows without a schooling year info, or rows redundant from the discounting method.
    '''
    df = df.dropna(subset=['years_of_schooling'])
    df = df.dropna(subset=['overall', 'prim', 'sec', 'higher',
                'gender_male', 'gender_female', 'private_sector', 'public_sector'], how='all')
    return df

def orderColumns(df):
    '''Input the inital data set, then remove the full discounting method columns,
    and reorder the columns as desired. Return the new data set.
    '''
    df = df.drop(columns=[col for col in df.columns if 'del' in col])
    df = df.sort_values(by=['source', 'country']).reset_index(drop=True) # Order observations by studies, alphabetically
    df['obs_n'] = range(1, df.shape[0] + 1)
    df = df.assign(study_id=df.groupby('obs_n').cumcount() + 1) # Create study id column

    # Shuffle column order
    if not df.shape[1] == len(NEW_COLNAMES):
        raise ValueError(f'Incorrect list of new column names. The list must contain {df.shape[1]} names.')
    df = df[NEW_COLNAMES]
    return df

def loadData():
    if not exists(SOURCE_FILE_NAME):
        raise ValueError('Source file untraceable')
    file = pd.read_excel(SOURCE_FILE_NAME, skiprows=9)
    file.columns = SOURCE_FILE_COLNAMES
    return file

if __name__=='__main__':
    main()