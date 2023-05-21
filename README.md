# A simple repository for my Diploma Thesis
* **Topic** - Ability bias in the returns to schooling: How large it is and why it matters
* **Author** - Bc. Petr Čala
* **Year of defense** - 2024
* **Supervisor** - doc. PhDr. Zuzana Havránková Ph.D.

## About

### The reason for having a GitHub repository

The reason for keeping this repository is to allow for easier version tracking, and portable work. The repository is *not meant for direct cloning*. I may clean up the structure in the future to allow for this, but the main idea is to distribute the `Dist/` folder (`Dist` branch) that contains all the files important for the analysis (files listed below). You can download the folder itself using steps described below. The rest of the repository (`master` branch) is intended purely for my own research.

### Project structure

All necessary files for result reproduction can be found within the `Dist/` folder of the `master` branch, or within the contents `Dist` branch. To easily clone this folder onto your computer, simply download the folder using third party tools such as [download-directory](https://download-directory.github.io/) and inputting [this link](https://github.com/PetrCala/Diploma-Thesis/tree/master/Dist), or by manually downloading each individual file. I advise for the former approach.

The distribution folder is structured as follows:

```
.
├── data
│   ├── data_set_master_thesis_cala.csv
│   └── var_list_master_thesis_cala.csv
├── graphics
├── pckg
│   └── LowRankQP
├── results
├── scripts
│   ├── elliott_master_thesis_cala.R
│   ├── endo_kink_master_thesis_cala.R
│   ├── maive_master_thesis_cala.R
│   ├── selection_model_master_thesis_cala.R
│   └── stem_method_master_thesis_cala.R
├── main_master_thesis_cala.R
├── script_runner_master_thesis_cala.R
├── source_master_thesis_cala.R
├── README.md
├── README.pdf
└── user_parameters.yaml
```

* `data/` -> Store your `.csv` data files here. I include two files for inspiration of how the files should look like.
  - `data_set_master_thesis_cala.csv` -> Main data frame. Contains data of 115 studies with over 40 variables. All numeric results are derived from this file.
  - `var_list_master_thesis_cala.csv` -> Data frame with information about individual variables. The scripts rely on this data frame to identify variable types, their usage in various parts of the analysis, etc.
* `graphics/` -> All graphical results will be automatically stored here.
* `pckg/` -> Folder with external packages that are not available online anymore, such as `LowRankQP`.
* `reults/` -> All numerical/text-based results will be automatically stored here.
* `scripts/` -> Source scripts for various external methods.
  - `elliott_master_thesis_cala.R` -> Source code for the p-hacking tests developed by Elliott et al. (2022).
  - `endo_kink_master_thesis_cala.R` -> Source code for the Endogenous Kink method (Bom & Rachinger, 2019).
  - `maive_master_thesis_cala.r` -> Source code for the MAIVE estimator method (Irsova et al., 2023).
  - `selection_model_master_thesis_cala.R` -> Source code for the Selection model (Andrew & Kasy, 2019). Rewritten from STATA, should be quite robust.
  - `stem_method_master_thesis_cala.R` -> Source code for the STEM method (Furukawa, 2019).
* `main_master_thesis_cala.R` -> Main script. Using the `user_parameters.yaml` file, call the desired methods with the specified parameters. Automatically handle package installation, working directory handling, temporary file creation.
* `source_master_thesis_cala.R` -> Source script with all the functions. Virtaully any function called from the main script is located here. Every function (hopefully) has a docstring explaining its *functionality* (pun intended). Navigate the script using function names.
* `script_runner_master_thesis_cala.R` -> Script for running the code in an aesthetic way. Here you can modify the parameters without having to edit the `.yaml` file. Automatically calls the whole main script, but you can modify which parts of it should run within the parameters. With this, just run the *script runner* script as a whole and witness magic happen (after you handle all the bugs).
* `README.md` -> This README file.
* `README.pdf` -> The README file in a presentable format.
* `user_parameters.yaml` -> Script customizable parameters. Can be modified either directly using a text editor or from within the `script_runner_master_thesis_cala.R` file. Contains parameters with file names, parts of the script to run, and parameters with which those parts should be run.

The script run will also create these temporary folders:
* `_cache/` -> Temporary cache files will be stored here.

Furthermore, the existence of all folders will be verified. Note that some do not appear in the repository, as there is nothing to distribute within these folders. All results (along with the folders) will be created and updated automatically.

## Prerequisites:
 1. Make sure that your working directory contains all the files from the `Dist/` folder.
 2. The scripts are set in a way that recognizes the file names just as they are distributed. However, if you wish to customize the file names, you may do so from within the `script_runner_master_thesis_cala.R`, or by modifying the `user_parameters.yaml` file. Know that every run of the script runner will automatically modify the contents of the user parameters
 file, so I suggest you modify the parameters directly within the script. However, **do not to modify the names of the user parameter file and the script runner**! These are immutable.
 3. Try to eliminate as many missing values in your data frame as you can.
    The script will automatically use interpolation for missing data, so that model averaging
    can run, but in case of many missing values, the results may be unstable.
 4. The data frame must contain several columns, the names of which can be modified to fit your data frame. This can be done the same way you would modify the script names (described in step 2). In the `user_parameter.yaml` file or in the script runner, navigate to the section `required_cols`, where you can modify the names of the necessary columns to fit the column names in your data frame. For the columns that are marked as optional, you can set the value to `NA` if this column is not present in your data frame. The column will then be created automatically during the script run. Required columns must then appear within your data frame. Here is the list of the expected columns:
   * Required columns:
      - **obs_id** - Unique ID of the observation.
      - **study_name** - Name of the study, such as *Einstein et al. (1935)*.
      - **study_id** - ID of the study. Should be numeric and unique for each study.
      - **effect** -  The main effect/estimate values. If you employ any effect transformations, such as partial correlation coefficient, set this to the transformed effect.
      - **se** - Standard error of the effect (of the transformed effect).
      - **n_obs** - Number of observations associated with this estimate.
   * Optional columns:
      - **t_stat** - t-statistic of the main effect. If set to `NA`, calculated automatically as a ratio of the effect
     and its standard error.
     - **precision** - Precision of the effect. If set to `NA`, calculated automatically if omitted using the `precision_type` parameter within the `adjustable_parameters` list of the `user_parameters.yaml` file. Defaults to *1/Standard_Error*.
      - **study_size** - Number of estimates reported per study. If set to `NA`, calculated automatically if omitted.
      - **reg_df** - Number of degrees of freedom associated with the regression. If set to `NA`, the number of observations associated with the estimate will be used instead.
 5. In the file `var_list_master_thesis_cala.csv` (or your renamed version), input the list of variables you are using in your data frame,
   along with these parameters:
   * **var_name** - Name of the variable exactly as it appears in the data frame columns. Must not include
     spaces and various special characters. Underscores are allowed. Example: *n_obs*.
   * **var_name_verbose** - A verbose name for the variable. Needs not to limit to any subset of characters. Example: *Number of Observations*.
   * **var_name_description** - A lengthy description of the variable. Example: *Number of observations used in the study.*
   * **data_type** - Type of the data this variable holds. Can be only one type. Can be one of:
     - *int* - Integer. Any integer.
     - *category* - Categorical variable. Any string.
     - *float* - Float. Any number.
     - *dummy* - Dummy. Either 0 or 1.
     - *perc* - Percentage. Any value between 0 and 1, inclusive.
   * **group_category** - Group of the variable. Group similar together, otherwise make a new group.
     Examples - dummies, gender, urban vs. rural, short-run vs. long-run
   * **na_handling** - Specify how missing values should be handled for the variable. Can be one of:
     - *stop* - Do not allow missing values. Throw an error in case there is a missing value.
     - *mean* - Interpolate with the mean of the existing data.
     - *median* - Interpolate with the median of the existing data.
     - *allow* - Should **NOT** be used. Your data frame should not contain any missing observations that can not be interpolated (filled in automatically with a numeric value). If such a column exists in your data frame, consider omitting this column from the analysis.
   * **variable_summary** - Boolean. If `TRUE`, this variable will appear in the summary statistics table.
   * **effect_sum_stats** - Boolean. If `TRUE`, this variable will appear in the effect summary statistics table.
   * **equal** - Float. If set to any value, the effect summary statistics table will print out the statistics
     for the main effect of the data when subsetted to this variable equal to the specified value.
     If set to any value, can not set the `gtlt` column value.
   * **gtlt** - One of "*median*", "*mean*", float. Similar to "equal", but if set to *median*/*mean*, will print out the statistics
     for the effect of the data when subsetted to values above/below the median value of this variable.
     If set to float, the subsetting breakpoint will be that value instead.
   * **bma** - Boolean. If `TRUE`, this variable will be used in the Bayesian model averaging. Do NOT set all
     values of one variable group to `TRUE`. This would create a dummy trap.
   * **bma_reference_var** - Boolean. If `TRUE`, this variable is the reference variable for the **dummy**/**perc** group. Exactly one variable must be a reference variable
     for each **dummy**/**perc** group.
   * **to_log_for_bma** - Boolean. If `TRUE`, this variable will be converted to logarithm during the 
     Bayesian model averaging.
   * **bpe** - If set to any value, this value will be used when evaluating the best practice estimate. Can also be one of the following: `mean`, `median`, `max`, `min`. If you do not wish to use this variable in the best practice estimate, set its value to `stop`, **not** `FALSE`.

## How to Run
To run the code, follow these steps:
1. Put your `.csv` data files into the `data/` folder. You do not need to delete the distributed example files, but you **must change the expected names in the user parameters** for the script to recognize these new files. 
2. Open the `script_runner_master_thesis_cala.R` file and find the `user_params` object. Within this object, **without modifying the names of the sub-objects**, change the values as you see fit. Most importantly, find the `CUSOMIZABLE FILE NAMES` section and modify the file names to refer your new files. Make sure to keep the `.csv` suffix. As of the current version, the script recognizes only `.csv` files as valid input. Also make sure, that the rest of the `user_params` values is set in accordance to the requirements outlined in the **Prerequisites** section.
3. After modifying any paramters as you see fit, run the script. You may encounter errors caused by mismatching file names, package incompatibility, etc. The script will automatically attempt to install all the necessary packages (if they are not installed on your local machine), but I can not guarantee this will go smoothly.
4. If all does, however, work, you should see the output in the console, and in the results folders `results/` (for numerical and text-based output) and `graphics/` (for graphical output). Any existing files will be overwritten upon running the script (if not cached), so make sure to save any desired files outside these folders after they are generated.
5. To display the graphic plots, either double click the `.html` files in the graphics folder (this will open them in an interactive window in your browser), or simply write the object name into the console after running the script (find these names in the `main_master_thesis_cala.R` script under the respective code sections. Calling the objects will automatically plot them in the correct form.
6. If you wish to see into the code a bit more, or run it only in parts, then open the script `main_master_thesis_cala.R`. The script automatically loads the `user_parameters.yaml` file, so it is assumed you have modified the parameters to your desired form. Afterwards, you can run the script as usual either at once, or by parts.
7. If you wish to look under the hood of the code, see the file `source_master_thesis_cala.R`, which contains all the technical functions, preprocessing, and validation, that is hidden in the main file.

## Miscellaneous
I can not guarantee the code will run perfectly in case you attempt a replication using a different data set. In case you use the data sets provided, the only caveat might be package installation, otherwise the code should run smoothly. In case of using a custom data set, various combinations of data might break some methods. As such, use the project with caution, and I hope it will be useful in any way possible.
