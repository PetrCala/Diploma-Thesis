def boldIfPIPHigh(val):
    '''Convert high PIP values to bold before printing out the TeX object.
    '''
    if isinstance(val, (float, int)) and val >= 0.5:
        return '\\textbf{' + str(f'{val:.3f}') + '}'
    return val

def handleSpecial(df):
    '''Handle special cases.'''
    df = df.replace('\^2','$^2$', regex = True)
    df = df.replace('&', '\\&', regex = True) # Author2 \& Author1
    df = df.replace('%', '\\%', regex = True)
    return df
