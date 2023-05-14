import pandas as pd
from tabulate import tabulate



def populateBMATable(df:pd.DataFrame, expected_cols = 7):
    if expected_cols != df.shape[1]:
        raise ValueError(f'The input data frame for the BMA table must have {expected_cols} columns.')

    # rename columns for display
    df.columns = ['Response variable:', 'Post. mean', 'Post. SD', 'PIP', 'Coef.', 'SE', 'p-value']

    # convert DataFrame to LaTeX tabular format
    latex = df.to_latex(index=False, escape=False)

    # insert LaTeX table formatting
    latex = latex.replace('\\begin{tabular}', '\\begin{singlespace}\n\\begin{scriptsize}\n\\begin{longtable}')
    latex = latex.replace('\\end{tabular}', '\\end{longtable}\n\\end{scriptsize}\n\\end{singlespace}')
    latex = latex.replace('lllllll', '{@\\hskip\\tabcolsep\\extracolsep\\fill}\nl*{6}{c}')

    # insert LaTeX table headers, footers and caption
    latex = latex.replace('\\toprule', '\\caption{Model averaging results}  \\label{tab:BMA}\\\\\n\\toprule\n  \\multicolumn{1}{l}{Response variable:} &   \\multicolumn{3}{c}{Bayesian model averaging} & \\multicolumn{3}{c}{Frequentist model averaging} \\\\\n  \\cmidrule(lr){2-4} \\cmidrule(lr){5-7}\n  \\multicolumn{1}{l}{Returns to Year of Schooling} & Post. mean & Post. SD & PIP & Coef. & SE & p-value \\\\\n\\midrule')
    latex = latex.replace('\\bottomrule', '\\bottomrule\n\\multicolumn{7}{>{\\scriptsize}p{0.95\\linewidth}}{\\emph{Note:} This table presents the results of the Bayesian and Frequentist model averaging. Post. mean = Posterior Mean, Post. SD = Posterior Standard Deviation, PIP = Posterior Inclusion Probability, Coef. = Coefficient, SE = Standard Error, OLS = Ordinary Least Squares, FE = Fixed Effects, 2SLS = 2 Stage Least Squares. The variables with PIP > 0.5 are highlighted. For a detailed explanation of the variables, see table \\ref{tab:var}.}')

    # print the LaTeX string
    print(latex)

    # return the string too
    return(latex)

def write_latex_to_file(latex_content):
    with open('populated_table.txt', 'w') as file:
        file.write(latex_content)

if __name__ == "__main__":
    df = pd.read_csv("ma_table.csv")
    bma_table = populateBMATable(df)
    write_latex_to_file(bma_table)

