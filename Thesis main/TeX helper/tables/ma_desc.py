import pandas as pd

from .utils.table_utils import handleSpecial, boldIfPIPHigh

def populateMaDescTable(df:pd.DataFrame, expected_cols = 4,verbose = False):
    if expected_cols != df.shape[1]:
        raise ValueError(f'The input data frame for the table must have {expected_cols} columns.')

    # rename columns for display
    df.columns = ['Variable', 'Description', 'Mean', 'SD']
    # Exponents to mathematic notation
    df = df.apply(handleSpecial)

    # convert DataFrame to LaTeX tabular format
    latex = df.to_latex(index=False, escape=False, header = False, float_format = '%.3f')

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
