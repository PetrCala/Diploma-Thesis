import pandas as pd

from .table_utils import handleSpecial, emphasizeRownames, boldIfPIPHigh, insertLinespace, renameLatex, fillNA, insertSection

class TABLE_PROCESSOR:
    def __init__(
        self, 
        df:pd.DataFrame,
        name:str,
        colnames:list[str],
        data_transformations: dict[str,any],
        string_transformations: dict[str,any]
    ):
        '''
        Constructor for the table processor class

        Args:
        - df (pd.DataFrame): Source DF as obtained from the R output (results/numeric).
        - name (str): Verbose name of the table to process
        - colnames (list[str]): List of column names to use from the source DF.
        - data_transformations (dict[str,any]): Transformations to apply to the source data frame after it has been subsetted to the desired columns, specified as a dictionary.
        - string_transformations (dict[str,any]): Transformations to apply to the latex string after the header/footer parts are replaced, specified as a dictionary.
        '''
        self.df = df
        self.name = name
        self.colnames = colnames
        self.data_transformations = data_transformations
        self.string_transformations = string_transformations
        self.validate_source() # Check that the input is valid
        self.preprocess() # Handle columns, special cases,...


    def __repr__(self):
        return f"Table '{self.name}'"

    def validate_source(self)->bool:
        '''Check that the source DF is of the correct shape and form. If not, raise a ValueError'''
        for col in self.colnames:
            if col not in self.df.columns:
                raise ValueError(f"In table '{self.name}', the source DF is missing the {col} column.")

    def subset_columns(self, cols_to_keep:list[str])->None:
        '''Given a list of columns as strings, subset the source DF to these columns only.'''
        # Validate input
        if len(cols_to_keep) < 1:
            raise ValueError(f"You must specify at least one column to keep for table '{self.name}'")
        self.df = self.df[cols_to_keep]

    def validate_columns(self)->None:
        '''
        Validate that the table contains all the expected columns. If it does not, raise a ValueError.
        '''
        if self.df.shape[1] != len(self.colnames):
            raise ValueError(f"Table '{self.name}' has incorrect column length.")
        # Add more possibly

    def preprocess(self)->None:
        '''
        Preprocess the source DF as read from the .csv files. The output should be a data frame prepared for the transformation.
        '''
        self.subset_columns(self.colnames)
        self.validate_columns()
        self.applyDataTransformations()

    def applyDataTransformations(self)->None:
        '''
        Using the source DF, apply data transformations on the data frame. Should be ran only once as a part of the "preprocess" routine.

        Returns:
            None: Applies the transformations to the source DF (class attribute).
        '''
        transformations = self.data_transformations
        if not transformations:
            return # Nothing to apply
        # Handle special characters
        if "handle_special" in transformations:
            cols_to_handle = transformations["handle_special"]
            cols_subset = self.df.columns if cols_to_handle == "all" else cols_to_handle
            self.df.loc[:, cols_subset] = self.df.loc[:, cols_subset].apply(handleSpecial)
        # Bold if PIP high
        if "bold_if_pip_high" in transformations:
            cols_to_emphasize = transformations["bold_if_pip_high"]
            cols_subset = self.df.columns if cols_to_emphasize == "all" else cols_to_emphasize
            self.df.loc[:, cols_subset] = self.df.loc[:, cols_subset].applymap(boldIfPIPHigh)
        if "fill_na" in transformations:
            cols_to_fill = transformations["fill_na"]
            cols_subset = self.df.columns if cols_to_fill == "all" else cols_to_fill
            self.df.loc[:, cols_subset] = self.df.loc[:,cols_subset].apply(fillNA)
        return

    def transform(
        self,
        src_header:str,
        src_footer:str,
        new_header:str,
        new_footer:str
    )->None:
        '''
        Transform the source DF into a Latex friendly string, replace the header/footer with the desired ones, and run any other modifications. Return the LaTeX string.

        Raise a ValueError if this operation fails anywhere in the process.

        Args:
            src_header (str): Header of the base LaTeX transformed object.
            src_footer (str): Footer of the base LaTeX transformed object.
            new_header (str): New header.
            new_footer (str): New footer.
        '''
        # Transform the data frame into a latex base form
        latex = self.df.to_latex(index=False, escape=False, header = False, float_format = '%.3f')
        check_these = {"header": src_header, "footer": src_footer}
        for name, val in check_these.items():
            if not val in latex:
                raise ValueError(f"The base table {name} is different than expected for table '{self.name}'. Check the static source files. Here is the base LaTeX string:\n{latex}")
        latex = latex.replace(src_header, new_header)
        latex = latex.replace(src_footer, new_footer)
        latex = self.applyExtraTransformations(latex) # Returns unmodified string by default
        return latex
    

    def applyExtraTransformations(self, latex:str)->str:
        '''
        Using the preprocessed and transformed latex string, apply extra transformations before returning it to be stored as a .txt file. If no extra transformations are specified, return the string as is.

        Args:
            latex (str): String to apply the extra transformations to.

        Returns:
            str: The transformed string.
        '''
        transformations = self.string_transformations
        if not transformations:
            # No transformations to apply
            return latex
        # Mapping of transformation names to functions
        transformation_functions = {
            "rename": lambda latex, args: renameLatex(latex, args),
            "emphasize_rownames": lambda latex, args: emphasizeRownames(latex, args),
            "insert_linespace": lambda latex, args: insertLinespace(latex, args),
            "insert_section": lambda latex, args: insertSection(latex, args, len(self.colnames))
        }

        # Apply transformations
        for transformation, args in transformations.items():
            if transformation in transformation_functions:
                latex = transformation_functions[transformation](latex, args)

        return latex
