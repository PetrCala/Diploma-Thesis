#!/bin/bash

# Update the user parameter files across folders using the following file:
# 	-R/custom_user_params/user_parameters_model.yaml
# The following .yaml files will be updated:
# 	-R/user_parameters.yaml
# 	-R/custom_user_params/user_parameters_twins.yaml
# 	-R/custom_user_params/user_parameters_green.yaml
# 	-R/custom_user_params/user_parameters_terka.yaml
# 	-Dist/user_parameters.yaml

#### STATIC ####

# Base path
BASE_PATH="C:/Users/hso20/OneDrive/Plocha/IES/Diploma-Thesis" 

# Source files
MODEL_FILE_PATH="$BASE_PATH/R/custom_user_parameters/user_parameters_model.yaml"
MAIN_FILE_PATH="$BASE_PATH/R/user_parameters.yaml"
TWIN_FILE_PATH="$BASE_PATH/R/custom_user_params/user_parameters_twins.yaml"
TERKA_FILE_PATH="$BASE_PATH/R/custom_user_params/user_parameters_terka.yaml"
GREEN_FILE_PATH="$BASE_PATH/R/custom_user_params/user_parameters_green.yaml"
VANY_FILE_PATH="$BASE_PATH/R/custom_user_params/user_parameters_vany.yaml"

#### MAIN SCRIPT START ####

cd $BASE_PATH

# Paths to sourcefiles
SOURCE_FILE_PATHS=(
  "$MODEL_FILE_PATH"
  "$MAIN_FILE_PATH"
  "$TWIN_FILE_PATH"
  "$TERKA_FILE_PATH"
  "$GREEN_FILE_PATH"
  "$VANY_FILE_PATH"
)


# Read the source files
SOURCE_FILE_NAMES=(
  "MODEL_FILE"
  "MAIN_FILE"
  "TWIN_FILE"
  "TERKA_FILE"
  "GREEN_FILE"
  "VANY_FILE"
)

# Check if the lengths are the same
if [ ! ${#SOURCE_FILE_PATHS[@]} -eq ${#SOURCE_FILE_NAMES[@]} ]
then
  echo "The source file vectors are incorrectly specified."
  exit 1
fi

# Validate the existence of all files
for f in "${SOURCE_FILE_PATHS[@]}"; do
	if ! test -f "$f"; then
	  echo "A source file seems to be missing."
	  echo "Expected location: $f"
	  exit 1
	fi
done
echo "All source files located successfully."

# Iterate over the file paths and assign the file contents to variables
for i in "${!SOURCE_FILE_PATHS[@]}"
do
  path="${SOURCE_FILE_PATHS[$i]}"
  var_name="${SOURCE_FILE_NAMES[$i]}"
  ## Read the file and remove carriage returns
  content=$(cat "$path" | tr -d '\r')
  ## Assign the file content to the variable
  declare "$var_name"="$content"
done
echo "All source files read successfully."





exit 1
