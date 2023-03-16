# Master script for my Diploma Thesis
* **Topic** - Ability bias in the returns to schooling: How large it is and why it matters
* **Author** - Bc. Petr Čala
* **Year of defense** - 2024
* **Supervisor** - doc. PhDr. Zuzana Havránková Ph.D.

## How to Run
To run the code, follow these steps:

1. Make sure that your working directory contains the following files:
  - `diploma_thesis_main.R`
  - `diploma_thesis_source.R`
  - `stem_method_ext.R`
2. The script should be run all at once, which should make for the most user-friendly experience. In order to achieve this, you can customize which parts of the code should be run during the global call. This is achieved by splitting the script into *two parts*:
  - **Customizable part**: Here you define which parts of the script you want to run and with which parameters
  - **Technical part**: The actual code, which should run without any problems, and all at once, if you specify the parameters correctly.
3. Go to the customizable part, and set which parts of the code you want to run. Use `T` to indicate that a part should be run, and `F` to indicate that it should not.
4. Adjust the parameters with which to run the script. Find the `adjustable_parameters` vector, and inside, feel free to adjust the various numeric or boolean parameters as you see fit.
5. Run the code **ALL AT ONCE**, and see the results in the console, and in the *Plots* section.
