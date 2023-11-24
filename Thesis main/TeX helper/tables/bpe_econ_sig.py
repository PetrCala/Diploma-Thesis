import pandas as pd

from .utils.table_utils import handleSpecial, boldIfPIPHigh

def populateBpeEconSigTable(df:pd.DataFrame, expected_cols = 5, verbose = False):
    if expected_cols != df.shape[1]:
        raise ValueError(f'The input data frame for the table must have {expected_cols} columns.')

    # rename columns for display
    df.columns = ['1','2','3','4','5']
    df = df.apply(handleSpecial)

    # convert DataFrame to LaTeX tabular format
    latex = df.to_latex(index=False, escape=False, header=False, float_format = '%.3f')

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

