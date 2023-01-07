from os.path import exists

import pandas as pd

BASE_PATH = r'C:\Users\hso20\OneDrive\Plocha\IES\Diploma-Thesis\Data\P&P python preprocessing'
SOURCE_FILE_NAME = 'PP_preprocessed.xlsx'

def main():
    df = loadData()
    df = testPreprocess(df)
    print(df.head())
    print(df.shape)
    return df

def testPreprocess(df):
    '''Test a short code on the preprocessed dataset.
    '''
    # df['study_id'] = df.apply(lambda x: (df['source'] != df['source'].shift()), axis = 1)


    df = df.assign(study_id=(df['source'] != df['source'].shift()).cumsum()) # Create study id column

    return df

def loadData():
    path = str(BASE_PATH) + f'\{SOURCE_FILE_NAME}'
    if not exists(path):
        raise ValueError('Source file untraceable')
    file = pd.read_excel(path)
    return file

if __name__=='__main__':
    main()