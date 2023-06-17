import pandas as pd
from tabulate import tabulate

pd.set_option('display.max_colwidth', 200) # Long descriptions

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

def populateVarStatsTable(df:pd.DataFrame, verbose = False):
    pass

def populateEffectStatsTable(df:pd.DataFrame, expected_cols = 8, verbose = False):
    if expected_cols != df.shape[1]:
        raise ValueError(f'The input data frame for the table must have {expected_cols} columns.')

    # rename columns for display
    df.columns = ['Var Name', 'Mean', 'CI lower', 'CI upper', 'Weighted',
                  'WM CI lower', 'WM CI upper', 'Obs']
    # Exponents to mathematic notation
    df = df.apply(handleSpecial)

    # convert DataFrame to LaTeX tabular format
    latex = df.to_latex(index=False, escape=False, header = False)

    # insert LaTeX table formatting
    latex = latex.replace('\\begin{tabular}', '\\begin{singlespace}\n\\begin{scriptsize}\n\\begin{longtable}')
    latex = latex.replace('\\end{tabular}', '\\end{longtable}\n\\end{scriptsize}\n\\end{singlespace}')
    latex = latex.replace('lrrr', '@{\\hskip\\tabcolsep\\extracolsep}\nl % Description\n*{6}{c} % Middle columns\n>{\\centering\\arraybackslash}p{1cm} % Last column with fixed width\n@{}}')
    # insert LaTeX table headers, footers and caption
    latex = latex.replace('\\bottomrule', '    \\bottomrule\n    \n    \n\\multicolumn{8}{>{\\scriptsize}p{0.9\\linewidth}}{\\emph{Note:} This table presents basic summary statistics of the returns to additional year of schooling coefficient calculated on various subsets of the data. Unweighted = Original data set is used. Weighted = Estimates are weighted by the inverse number of estimates reported by each study. OLS = Ordinary Least Squares. For cutoff points, medians are used except for dummy variables where the cutoffs are 0.5.}')

    latex = latex.replace('\\toprule', '\\caption{Mean statistics across various subsets of data}  \\label{tab:sum}\\\\\n\\toprule\n   & \\multicolumn{3}{c}{Unweighted} &   \\multicolumn{3}{c}{Weighted} & \\\\\n   \\cmidrule(lr){2-4} \\cmidrule(lr){5-7}\n   & Mean & \\multicolumn{2}{c}{95\\% conf. int.} & Mean & \\multicolumn{2}{c}{95\\% conf. int.} & N. obs\\\\\n\\midrule\n\\endfirsthead\n\\caption{Mean statistics across various subsets of data (continued)}\\\\\n\\toprule\n  & \\multicolumn{3}{c}{Unweighted} &   \\multicolumn{3}{c}{Weighted} &  \\\\\n   \\cmidrule(lr){2-4} \\cmidrule(lr){5-7}\n   & Mean & \\multicolumn{2}{c}{95\\% conf. int.} & Mean & \\multicolumn{2}{c}{95\\% conf. int.} & No. of observations \\\\\n  \n\\midrule\n\\endhead\n\\bottomrule\n\\multicolumn{8}{r}{{\\scriptsize Continued on next page}} \\\\\n\\endfoot\n\n\\endlastfoot')


    # print the LaTeX string
    if verbose:
        print(latex)

    # return the string too
    return(latex)

def populateLinearTestsTable(df:pd.DataFrame, verbose = False):
    pass

def populateNonlinearTestsTable(df:pd.DataFrame, verbose = False):
    pass

def populateExoTestsTable(df:pd.DataFrame, verbose = False):
    pass

def populateCaliperTable(df:pd.DataFrame, verbose = False):
    pass

def populateElliottTable(df:pd.DataFrame, verbose = False):
    pass

def populateMaiveTable(df:pd.DataFrame, verbose = False):
    pass

def populateBMATable(df:pd.DataFrame, expected_cols = 7, verbose = False):
    if expected_cols != df.shape[1]:
        raise ValueError(f'The input data frame for the table must have {expected_cols} columns.')

    # rename columns for display
    df.columns = ['Response variable:', 'Post. mean', 'Post. SD', 'PIP', 'Coef.', 'SE', 'p-value']
    df['PIP'] = df['PIP'].apply(boldIfPIPHigh) # Large PIP to bold
    df = df.apply(handleSpecial) # Exponents to mathematic notation

    # convert DataFrame to LaTeX tabular format
    latex = df.to_latex(index=False, escape=False, header = False)

    # insert LaTeX table formatting
    latex = latex.replace('\\begin{tabular}', '\\begin{singlespace}\n\\begin{scriptsize}\n\\begin{longtable}')
    latex = latex.replace('\\end{tabular}', '\\end{longtable}\n\\end{scriptsize}\n\\end{singlespace}')
    # latex = latex.replace('lrrrrrr', '{@\\hskip\\tabcolsep\\extracolsep\\fill}\nl*{6}{c}')

    # insert LaTeX table headers, footers and caption
    latex = latex.replace('\\toprule', '\\caption{Model averaging results}  \\label{tab:BMA}\\\\\n\\toprule\n  \\multicolumn{1}{l}{Response variable:} &   \\multicolumn{3}{c}{Bayesian model averaging} & \\multicolumn{3}{c}{Frequentist model averaging} \\\\\n  \\cmidrule(lr){2-4} \\cmidrule(lr){5-7}\n  \\multicolumn{1}{l}{Returns to Year of Schooling} & Post. mean & Post. SD & PIP & Coef. & SE & p-value \\\\\n\\midrule\n\\endfirsthead\n\\caption[]{Model averaging results (continued)}\\\\\n\\toprule\n  \\multicolumn{1}{l}{Response variable:} &   \\multicolumn{3}{c}{Bayesian model averaging} & \\multicolumn{3}{c}{Frequentist model averaging} \\\\\n  \\cmidrule(lr){2-4} \\cmidrule(lr){5-7}\n  \\multicolumn{1}{l}{Returns to Year of Schooling} & Post. mean & Post. SD & PIP & Coef. & SE & p-value \\\\\n\\midrule\n\\endhead\n\\bottomrule\n\\multicolumn{7}{r}{{\\scriptsize Continued on next page}} \\\\\n\\endfoot\n\\endlastfoot')

    latex = latex.replace('\\bottomrule', '\\bottomrule\n\\multicolumn{7}{>{\\scriptsize}p{0.95\\linewidth}}{\\emph{Note:} This table presents the results of the Bayesian and Frequentist model averaging. Post. mean = Posterior Mean, Post. SD = Posterior Standard Deviation, PIP = Posterior Inclusion Probability, Coef. = Coefficient, SE = Standard Error, OLS = Ordinary Least Squares, FE = Fixed Effects, 2SLS = 2 Stage Least Squares. The variables with PIP > 0.5 are highlighted. For a detailed explanation of the variables, see table \\ref{tab:var}.}')

    # print the LaTeX string
    if verbose:
        print(latex)

    # return the string too
    return(latex)

def populateMaDescTable(df:pd.DataFrame, expected_cols = 4,verbose = False):
    if expected_cols != df.shape[1]:
        raise ValueError(f'The input data frame for the table must have {expected_cols} columns.')

    # rename columns for display
    df.columns = ['Variable', 'Description', 'Mean', 'SD']
    # Exponents to mathematic notation
    df = df.apply(handleSpecial)

    # convert DataFrame to LaTeX tabular format
    latex = df.to_latex(index=False, escape=False, header = False)

    # insert LaTeX table formatting
    latex = latex.replace('\\begin{tabular}', '\\begin{singlespace}\n\\begin{scriptsize}\n\\begin{longtable}')
    latex = latex.replace('\\end{tabular}', '\\end{longtable}\n\\end{scriptsize}\n\\end{singlespace}')
    latex = latex.replace('llrr', '@{\\hskip\\tabcolsep\\extracolsep\\fill}\nl\n%p{0.25\\hsize}\np{0.55\\hsize}\ncc\n@{}')
    # insert LaTeX table headers, footers and caption
    latex = latex.replace('\\bottomrule', '\\bottomrule\n   \n \\multicolumn{4}{>{\\scriptsize}p{0.95\\linewidth}}{\\emph{Note:} This table presents the summary statistics and descriptions for each of the various study characteristics. SD = standard deviation, FE = Fixed Effects, 2SLS = 2 Stage Least Squares.}')

    latex = latex.replace('\\toprule', '\\caption{Definition and summary statistics of regression variables}  \\label{tab:var}\\\\\n\\toprule\n  \\multicolumn{1}{l}{Variable} &   \\multicolumn{1}{l}{Description} &         \\multicolumn{1}{c}{Mean} &           \\multicolumn{1}{c}{SD} \\\\\n\\midrule\n\\endfirsthead\n\\caption[]{Definition and summary statistics of regression variables (continued)}\\\\\n\\toprule\n  \\multicolumn{1}{l}{Variable} &   \\multicolumn{1}{l}{Description} &         \\multicolumn{1}{c}{Mean} &           \\multicolumn{1}{c}{SD} \\\\\n\\midrule\n\\endhead\n\\bottomrule\n\\multicolumn{4}{r}{{\\scriptsize Continued on next page}} \\\\\n\\endfoot\n\\endlastfoot')


    # print the LaTeX string
    if verbose:
        print(latex)

    # return the string too
    return(latex)

def populateBpeResTable(df:pd.DataFrame, expected_cols = 4, verbose = False):
    if expected_cols != df.shape[1]:
        raise ValueError(f'The input data frame for the table must have {expected_cols} columns.')

    # Recalculate the CI bounds into an interval
    df['ci'] = '(' + df['ci_95_lower'].astype(str) + '; ' + df['ci_95_higher'].astype(str) + ')'
    # Drop the "ci_95_lower" and "ci_95_higher" columns
    df = df.drop(['ci_95_lower', 'ci_95_higher'], axis=1)

    # rename columns for display
    df.columns = ['Study', 'Estimate', '95\% Confidence Interval']
    df = df.apply(handleSpecial)

    # convert DataFrame to LaTeX tabular format
    latex = df.to_latex(index=False, escape=False, header=False)

    # insert LaTeX table formatting
    latex = latex.replace('\\begin{tabular}', '\\begin{singlespace}\n\\begin{scriptsize}\n\\begin{longtable}')
    latex = latex.replace('\\end{tabular}', '\\end{longtable}\n\\end{scriptsize}\n\\end{singlespace}')
    latex = latex.replace('lrl', '@{\\hskip\\tabcolsep}\nl\n*{2}{c}\n@{}')
    # insert LaTeX table headers, footers and caption
    latex = latex.replace('\\bottomrule', '\\bottomrule\n\\multicolumn{3}{>{\\scriptsize}p{0.6\\linewidth}}{\\emph{Note:} The table reports estimates of the best-practice estimate according to three different studies and the author\'s subjective best-practice. 95\\% confidence interval bounds are constructed as an approximate using OLS with study level clustered standard errors.}')

    latex = latex.replace('\\toprule', '\\caption{Implied best-practice}  \\label{tab:BPE}\\\\\n\\toprule\n    Study & Estimate & 95\\% Confidence Interval \\\\\n\\endfirsthead\n\\midrule')


    # print the LaTeX string
    if verbose:
        print(latex)

    # return the string too
    return(latex)

def populateBpeEconSigTable(df:pd.DataFrame, expected_cols = 5, verbose = False):
    if expected_cols != df.shape[1]:
        raise ValueError(f'The input data frame for the table must have {expected_cols} columns.')

    # rename columns for display
    df.columns = ['1','2','3','4','5']
    df = df.apply(handleSpecial)

    # convert DataFrame to LaTeX tabular format
    latex = df.to_latex(index=False, escape=False, header=False)

    # insert LaTeX table formatting
    latex = latex.replace('\\begin{tabular}', '\\begin{singlespace}\n\\begin{scriptsize}\n\\begin{longtable}')
    latex = latex.replace('\\end{tabular}', '\\end{longtable}\n\\end{scriptsize}\n\\end{singlespace}')
    latex = latex.replace('lrlrl', '@{\\hskip\\tabcolsep}\nl\n*{4}{c}\n@{}')
    # insert LaTeX table headers, footers and caption
    latex = latex.replace('\\bottomrule', '\\bottomrule\n\\multicolumn{5}{>{\\scriptsize}p{0.8\\linewidth}}{\\emph{Note:} This table presents ceteris paribus effect of several key variables on the partial correlation coefficient. Only those variables with \\emph{PIP} over 0.5 in the \\emph{BMA} model are included. \\emph{One SD change} implies how the effect changes when we increase a specific variable by one standard deviation. \\emph{Maximum change} represents the change in the effect when the variable is increased from its minimum to its maximum. The reference best-practice value is 6.845. SD = Standard Deviation, BP = Best-Practice. For a detailed explanation of the variables, see table \\ref{tab:var}.}')

    latex = latex.replace('\\toprule', '\\caption{Significance of key variables}  \\label{tab:key}\\\\\n\\toprule\n & \\multicolumn{2}{c}{One SD change} & \\multicolumn{2}{c}{Maximum change}\\\\\n & Effect on Returns & \\% of BP & Effect on Returns & \\% of BP \\\\\n\\midrule')


    # print the LaTeX string
    if verbose:
        print(latex)

    # return the string too
    return(latex)

def write_latex_to_file(latex_content):
    with open('populated_table.txt', 'w') as file:
        file.write(latex_content)

if __name__ == "__main__":
    pass

