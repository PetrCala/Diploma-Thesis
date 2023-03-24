# A simple repository for my Diploma Thesis
* **Topic** - Ability bias in the returns to schooling: How large it is and why it matters
* **Author** - Bc. Petr Čala
* **Year of defense** - 2024
* **Supervisor** - doc. PhDr. Zuzana Havránková Ph.D.

## About

The reason to keep this repository is to allow for easier version tracking, and portable work. The repository is *not meant for direct cloning*. I may clean up the structure in the future to allow for this, but the main idea is to distribute the `.R` and `.csv` files (listed below) to whomever may want to reproduce the results. If you wish to obtain these files directly from this repository, feel free to either download them manually, or clone the repository as a whole, and delete the reduntant parts. Note that the cloning creates a lot of clutter, so I advise against it.

## How to Run
To run the code, follow these steps:

1. Make sure that your working directory contains the following files (these can be found in the `.R/` folder):
  - `endo_kink_master_thesis_cala.R`
  - `main_master_thesis_cala.R`
  - `maive_master_thesis_cala.R`
  - `pretty_output_master_thesis_cala.R`
  - `selection_model_master_thesis_cala.R`
  - `source_master_thesis_cala.R`
  - `stem_master_thesis_cala.R`
  - `data_set_master_thesis_cala.csv`
  - `var_list_master_thesis_cala.csv`
2. If you do not want to parametrize anything, simply open the file `pretty_output_master_thesis_cala.R` and run it. We can not guarantee you will not encounter any errors, especially with package dependencies, but everything should run quite smoothly by deafult.
3. If you wish to see into the code a bit more, and maybe parametrize several parts, such as which tests should run, and which not, then open the script `main_master_thesis_cala.R`. This script is split into *two parts*:
  - **Customizable part**: Here you define which parts of the script you want to run and with which parameters
  - **Technical part**: The actual code, which should run without any problems, and all at once, if you specify the parameters correctly.
4. Go to the customizable part, and set which parts of the code you want to run. Use `T` to indicate that a part should be run, and `F` to indicate that it should not.
5. Adjust the parameters with which to run the script. Find the `adjustable_parameters` vector, and inside, feel free to adjust the various numeric or boolean parameters as you see fit.
6. Run the code **ALL AT ONCE**, and see the results in the console, and in the *Plots* section.
7. If you wish to look under the hood of the code, see the file `source_master_thesis_cala.R`, which contains all the technical functions, preprocessing, and validation, that is hidden in the main file.

