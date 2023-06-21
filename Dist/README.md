# A simple repository for my Diploma Thesis
* **Topic** - Ability bias in the returns to schooling: How large it is and why it matters
* **Author** - Bc. Petr Čala
* **Year of defense** - 2024
* **Supervisor** - doc. PhDr. Zuzana Havránková Ph.D.

## About

### How To Use

Clone this repository onto your computer using `git clone https://github.com/PetrCala/Diploma-Thesis.git`, or download it directly using the `<> Code` button above.' Refer to the `Prerequisites` and `How To Run` sections for more detail.

### Project structure

All necessary files for result reproduction can be found within the `Dist/` folder of the `master` branch, or within the contents `Dist` branch. To easily clone this folder onto your computer, simply download the folder using third party tools such as [download-directory](https://download-directory.github.io/) and inputting [this link](https://github.com/PetrCala/Diploma-Thesis/tree/master/Dist), or by manually downloading each individual file. I advise for the former approach.

The distribution folder is structured as follows:

```
.
├── data
│   ├── source
│   │   ├── data_set_master_thesis_cala.xlsm
│   └── temp
├── pckg
│   └── LowRankQP
├── results
│   ├── graphic
│   └── numeric
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

* `data/` -> Folder for storing data. The folder is further split into two sub-folders:
  - `source/` -> Put your `.xlsx` or `.xlsm` source data file here. In the distributed folder, there is a placeholder file called `data_set_master_thesis_cala.xlsm`.
  - `temp/` - > This folder will automatically get created upon script run. Here will be the `.csv` files created from the sheets of your data set. This allows reproducibility and consistency within the script.
* `pckg/` -> Folder with external packages that are not available online anymore, such as `LowRankQP`.
* `results/` -> Folder with all results. A `.zip` file with all results will be automatically created here.
  - graphic/ -> All graphic results will be automatically stored here.
  - numeric/ -> All numeric results will be automatically stored here.
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
* `user_parameters.yaml` -> Script customizable parameters. Created upon running the script runner. Can be modified either directly using a text editor or from within the `script_runner_master_thesis_cala.R` file. Contains parameters with file names, parts of the script to run, and parameters with which those parts should be run.

The script run will also create these temporary folders:
* `_cache/` -> Temporary cache files will be stored here.

Furthermore, the existence of all folders will be verified. Note that some do not appear in the repository, as there is nothing to distribute within these folders. All results (along with the folders) will be created and updated automatically.

## Prerequisites:
 1. Install the newest version of [Rtools](https://cran.r-project.org/bin/windows/Rtools/). This is important to enable external package handling.
 2. Install [JAGS 4.3.1](https://mcmc-jags.sourceforge.io/). This software is required by the [RoBMA package](https://fbartos.github.io/RoBMA/). As of the current version, I am looking for a workaround to avoid having to install this application, but for now, there is no way around.
 3. Clone the repository:
	```
	git clone https://github.com/PetrCala/Diploma-Thesis
	```
 4. Change into the directory:
	```
	cd Diploma-Thesis
	```
 5. In case you wish to only test the functionality of the script using placeholder data within the data folder, skip to the **How to run** section below. If you wish, on the other hand, to run your own analysis, make sure to follow the next steps as well.
 6. If you wish to customize the source file names (such as scripts, result folders, etc.), you may do so from within the `script_runner_master_thesis_cala.R`, or by modifying the `user_parameters.yaml` file. Know that every run of the script runner will automatically modify the contents of the user parameters
    file, so I suggest you modify the parameters directly within the script if this is your preferred way of running the project. However, **do not to modify the names of the user parameter file and the script runner**! These are immutable.
 7. Try to eliminate as many missing values in your data frame as you can.
    The script will automatically use interpolation for missing data, so that model averaging
    can run, but in case of many missing values, the results may be unstable.
 8. The file with data must contain two sheets - `data_set` and `var_list` (these are modifiable within the user parameter file or from within the script runner). The former should contain all your data that satisfies the conditions described in step 9, while the latter should contain information about variables of the dataset, as described in step 10.
 9. The data frame must contain several columns, labelel **required columns**, and there are also several columns that are optional **optional columns**. These are:
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
 10. The sheet (labeled by default as `var_list`), contains the information about the nature and usage of all columns/variables that appear in the main data frame. Note that these must be listed exactly in the order in which they appear in the main data frame. For inspiration, see the placeholder file `data_set_master_thesis_cala.xlsm`.
   The sheet should contain the following columns:
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
     - *foo* - Interpolate with random values. These columns should not be used for further analysis (BMA,...).
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
1. There are two options of running the script:
  * **Use a script runner** - You can run the code and modify the customizable parameters from within a single R script - `script_runner_master_thesis_cala.R`.
	Open the file and find the `user_params` object. Within this object, **without modifying the names of the sub-objects**, change the values as you see fit, but within the guidelines described in step 2.
  * **Use the main script in combination with the `user_params.yaml` file** - You can also run the main code by directly calling the `main_master_thesis_cala.R` file. This assumes that there exists a `user_params.yaml` file within the root of the folder and that you have modified the parameters to your liking.
	When modifying the parameters, you may do so from directly within the `.yaml` file, but make sure to follow the guidelines in step 2 while doing so.
2. Guidelines for parameter modification:
  * Follow the `User parameter guide` for explicit information about allowed values and meaning of all parameters.
  * Do not change the names of any of the parameters, unless told explicitly. Change only the values. For the explanation of each of the parameters, see the script runner.
  * Parameters used for calling external functions are marked with the `param_` prefix within their name (see parameters for BMA, non-linear tests,...). Make sure to keep this convention when adding new such parameters.
  * Make sure to keep the object types, unless told explicitly. For example, if a value of a parameter is a vector, make sure it is still a vector after the modifications.
3. If you want to run the code using your own data and not the placeholder data provided within the distributed files, put your `.csv` data files into the `data/` folder. You do not need to delete the distributed placeholder files, but you **must change the expected names in the user parameters** for the script to recognize these new files (parameters `master_data_set_source` and `var_list_source`). The elements of the `data_files` list parameter must be changed to reflect the names of your `.csv` data files. If you do not modify these, the script will read the placeholder data files from the `data/` folder instead. ake sure to keep the `.csv` suffix. As of the current version, the script recognizes only `.csv` files as valid input.
4. After modifying any paramters as you see fit, run either the script runner or the main script. Note that running the script runner will automatically overwrite the `user_params.yaml` file with parameters defined within the script runner, so if you opt for the second approach from step 1, be careful with the script runner use.
5. You may encounter errors caused by mismatching file names, package incompatibility, etc. The script will automatically attempt to install all the necessary packages (if they are not installed on your local machine), so in case there are any conflicts, make sure to check that you have fulfilled all prerequisites from the prerequisites section.
5. If all goes well, you should see the output in the console, and in the results folders `results/` (for numerical and text-based output) and `graphics/` (for graphical output). Any existing files will be overwritten upon running the script (if not cached), so make sure to save any desired files outside these folders after they are generated.
6. To display the graphic plots, either double click the `.html` files in the graphics folder (this will open them in an interactive window in your browser), or simply write the object name into the console after running the script (find these names in the `main_master_thesis_cala.R` script under the respective code sections. Calling the objects will automatically plot them in the correct form.
7. If you wish to look under the hood of the code, see the file `source_master_thesis_cala.R`, which contains all the technical functions, preprocessing, and validation, that is hidden in the main file.

## Miscellaneous
I can not guarantee the code will run perfectly in case you attempt a replication using a different data set. In case you use the data sets provided, the only caveat might be package installation, otherwise the code should run smoothly. In case of using a custom data set, various combinations of data might break some methods. As such, use the project with caution, and I hope it will be useful in any way possible.
