import pandas as pd

from .utils.table_utils import handleSpecial, boldIfPIPHigh

def populateEffectStatsTable(df:pd.DataFrame, expected_cols = 8, verbose = False):
    if expected_cols != df.shape[1]:
        raise ValueError(f'The input data frame for the table must have {expected_cols} columns.')

    # rename columns for display
    df.columns = ['Var Name', 'Mean', 'CI lower', 'CI upper', 'Weighted',
                  'WM CI lower', 'WM CI upper', 'Obs']
    # Exponents to mathematic notation
    df = df.apply(handleSpecial)

    # convert DataFrame to LaTeX tabular format
    latex = df.to_latex(index=False, escape=False, header = False, float_format = '%.3f')

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
