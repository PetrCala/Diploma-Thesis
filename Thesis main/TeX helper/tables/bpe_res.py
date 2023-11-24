import pandas as pd

from .utils.table_utils import handleSpecial, boldIfPIPHigh

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
    latex = df.to_latex(index=False, escape=False, header=False, float_format = '%.3f')

    # insert LaTeX table formatting
    latex = latex.replace('\\begin{tabular}', '\\begin{singlespace}\n\\begin{scriptsize}\n\\begin{longtable}')
    latex = latex.replace('\\end{tabular}', '\\end{longtable}\n\\end{scriptsize}\n\\end{singlespace}')
    latex = latex.replace('lrl', '@{\\hskip\\tabcolsep}\nl\n*{2}{c}\n@{}')
    # insert LaTeX table headers, footers and caption
    latex = latex.replace('\\bottomrule', '\\bottomrule\n\\multicolumn{3}{>{\\scriptsize}p{0.6\\linewidth}}{\\emph{Note:} The table reports estimates of the best-practice estimate according to three different studies and the author\'s subjective best-practice. 95\\% confidence interval bounds are constructed as an approximate using OLS with study level clustered standard errors.}')

    latex = latex.replace('\\toprule', '\\caption{Implied best-practice}  \\label{tab:BPE}\\\\\n\\toprule\n    Study & Estimate & 95\\% Confidence Interval \\\\\n\\endfirsthead\n')

    # print the LaTeX string
    if verbose:
        print("Output of the BPE results table:")
        print(latex)
        print("\n")

    # return the string too
    return(latex)

