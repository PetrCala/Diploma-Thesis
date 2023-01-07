from os.path import exists

import pandas as pd

SOURCE_FILE_NAME = 'PP_preprocessed.xlsx'

def main():
    df = loadData()
    df = testPreprocess(df)
    # print(df.head())
    # print(df.shape)
    print(df.head())
    return df

def testPreprocess(df):
    '''Test a short code on the preprocessed dataset.
    '''
    # df['study_id'] = df.apply(lambda x: (df['source'] != df['source'].shift()), axis = 1)


    df = df.assign(study_id=(df['source'] != df['source'].shift()).cumsum()) # Create study id column

    return df

def loadData():
    if not exists(SOURCE_FILE_NAME):
        raise ValueError('Source file untraceable')
    file = pd.read_excel(SOURCE_FILE_NAME)
    return file

if __name__=='__main__':
    main()