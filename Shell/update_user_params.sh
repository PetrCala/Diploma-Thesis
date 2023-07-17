#!/bin/bash

# Update the user parameter files across folders using the following file:
# 	-R/custom_user_params/user_parameters_model.yaml
# The following .yaml files will be updated:
# 	-R/custom_user_params/user_parameters_twins.yaml
# 	-R/custom_user_params/user_parameters_green.yaml
# 	-R/custom_user_params/user_parameters_terka.yaml


############### USER PARAM CHANGES ############### 

declare -A twin_changes
declare -A green_changes
declare -A terka_changes
declare -A vany_changes


#### TWIN DATASET CHANGES
twin_changes=(
    ['file_name: "data_set_master_thesis_cala"']='file_name: "twin_data_master_thesis_cala"'
    ['csv_suffix: "master_thesis_cala"']='csv_suffix: "twins"'
    ['file_suffix: ".xlsx"']='file_suffix: ".xlsm"'
    ['non_linear_param_selection_modelmu: "t"']='non_linear_param_selection_modelmu: "normal"'
    ['automatic_bma: false']='automatic_bma: true'
    ['export_zip_name: "results_all"']='export_zip_name: "results_twins"'
)

#### GREEN CHANGES
green_changes=(
    ['file_name: "data_set_master_thesis_cala"']='file_name: "data_set_green"'
    ['csv_suffix: "master_thesis_cala"']='csv_suffix: "green"'
    ['obs_id: "obs_n"']='obs_id: "obs_id"'
    ['reg_df: "reg_df"']='reg_df: .na'
    ['effect_name: "years_of schooling on wage']='effect_name: "technological progress on employment"'
    ['data_winsorization_level: 0.01']='data_winsorization_level: 0.05'
    ['allowed_missing_ratio: 0.7']='allowed_missing_ratio: 0.9'
    ['box_plot_group_by_factor_2: "country"']='box_plot_group_by_factor_2: .na'
    ['funnel_graph_scale: 2.5']='funnel_graph_scale: 3'
    ['non_linear_stem_legend_position: "topright"']='non_linear_stem_legend_position: "topleft"'
    ['non_linear_param_selection_modelmu: "t"']='non_linear_param_selection_modelmu: "normal"'
    ['automatic_bma: false']='automatic_bma: true'
    ['bpe_generate_graphs: true']='bpe_generate_graphs: false'
    ['export_zip_name: "results_all"']='export_zip_name: "results_all_green"'
    ['theme: "blue"']='theme: "green"'
)


#### TERKA CHANGES
terka_changes=(
    ['funnel_graph_scale: 2.5']='funnel_graph_scale: 3'
    ['file_name: "data_set_master_thesis_cala"']='file_name: "data_set_terka"'
    ['csv_suffix: "master_thesis_cala"']='csv_suffix: "terka"'
    ['study_size: "study_size"']='study_size: .na'
    ['reg_df: "reg_df"']='reg_df: .na'
    ['effect_name: "years_of schooling on wage']='effect_name: "peer status on academic achievement"'
    ['box_plot_max_boxes: 60']='box_plot_max_boxes: 80'
    ['non_linear_stem_legend_position: "topright"']='non_linear_stem_legend_position: "topleft"'
    ['non_linear_param_selection_modelmu: "t"']='non_linear_param_selection_modelmu: "normal"'
    ['bpe_generate_graphs: true']='bpe_generate_graphs: false'
    ['export_zip_name: "results_all"']='export_zip_name: "results_terka"'
    ['theme: "blue"']='theme: "purple"'
)


#### VANY CHANGES
vany_changes=(
    ['file_name: "data_set_master_thesis_cala"']='file_name: "data-set_bachelor-thesis_Nguyenova"'
    ['csv_suffix: "master_thesis_cala"']='csv_suffix: "vany"'
    ['se: "se"']='se: "standard_error"'
    ['effect_name: "years of schooling on wage"']='effect_name: "individual ability on wage"'
    ['allowed_missing_ratio: 0.7']='allowed_missing_ratio: 0.9'
    ['t_hist_highlight_mean: true']='t_hist_highlight_mean: false'
    ['t_hist_t_stats: \[-1.96, 1.96\]']='t_hist_t_stats: [1.645, 1.96, 2.58]'
    ['non_linear_param_selection_modelmu: "t"']='non_linear_param_selection_modelmu: "normal"'
    ['automatic_bma: false']='automatic_bma: true'
    ['bpe_generate_graphs: true']='bpe_generate_graphs: false'
    ['export_zip_name: "results_all"']='export_zip_name: "results_vany"'
)



############### END OF CHANGES ############### 


#### STATIC ####

# Base path
BASE_PATH="C:/Users/hso20/OneDrive/Plocha/IES/Diploma-Thesis" 

# Source files
MODEL_FILE_PATH="$BASE_PATH/R/custom_user_params/user_parameters_model.yaml"
TWIN_FILE_PATH="$BASE_PATH/R/custom_user_params/user_parameters_twins.yaml"
TERKA_FILE_PATH="$BASE_PATH/R/custom_user_params/user_parameters_terka.yaml"
GREEN_FILE_PATH="$BASE_PATH/R/custom_user_params/user_parameters_green.yaml"
VANY_FILE_PATH="$BASE_PATH/R/custom_user_params/user_parameters_vany.yaml"

#### MAIN SCRIPT START ####

cd $BASE_PATH

# Paths to sourcefiles
SOURCE_FILE_PATHS=(
  "$TWIN_FILE_PATH"
  "$TERKA_FILE_PATH"
  "$GREEN_FILE_PATH"
  "$VANY_FILE_PATH"
  "$MODEL_FILE_PATH"
)


# Read the source files
SOURCE_FILE_NAMES=(
  "TWIN_FILE"
  "TERKA_FILE"
  "GREEN_FILE"
  "VANY_FILE"
  "MODEL_FILE"
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
	  echo "Expected location: $f"! cmp -s "$source_file" "$f"
	  exit 1
	fi
done
echo "All source files located successfully."

# Read the model file
MODEL_FILE=$(cat "$MODEL_FILE_PATH" | tr -d '\r')

# Iterate over the file paths and assign the file contents to variables
for i in "${!SOURCE_FILE_PATHS[@]}"
do
  path="${SOURCE_FILE_PATHS[$i]}"
  var_name="${SOURCE_FILE_NAMES[$i]}"
  # Read the file contents
  contents=$(cat "$path" | tr -d '\r')
  ## Assign the existing file content to each variable - EXISTING_TWIN_FILE,...
  declare "EXISTING_$var_name"="$contents"
  ## Also assign the model file contents to each variable - TWIN_FILE,...
  declare "$var_name"="$MODEL_FILE"
done
echo "All source files read successfully."



### Declare a function to modify the desired files

# Usage:
# substitute <original_string> <file> <associative_array>
function substitute() {
    local original_string=$1
    local file=$2
    declare -n arr=$3

    for key in "${!arr[@]}"; do
        local target_string=$key
        local replacement_string=${arr[$key]}

        # escape forward slashes in the target and replacement strings
        target_string=$(echo $target_string | sed 's/\//\\\//g')
        replacement_string=$(echo $replacement_string | sed 's/\//\\\//g')

        original_string=$(echo "$original_string" | sed "s/$target_string/$replacement_string/g")
    done

    eval $file='$original_string'
}

### Actual substitution

# Modify the file objects using a custom function call
substitute "$TWIN_FILE" "TWIN_FILE" twin_changes
substitute "$GREEN_FILE" "GREEN_FILE" green_changes
substitute "$TERKA_FILE" "TERKA_FILE" terka_changes
substitute "$VANY_FILE" "VANY_FILE" vany_changes

# Bundle up all the modified files
MODIFIED_FILES=(
  "$TWIN_FILE"
  "$TERKA_FILE"
  "$GREEN_FILE"
  "$VANY_FILE"
)

EXISTING_FILES=(
  "$EXISTING_TWIN_FILE"
  "$EXISTING_TERKA_FILE"
  "$EXISTING_GREEN_FILE"
  "$EXISTING_VANY_FILE"
)


# Modify the actual files
for i in "${!MODIFIED_FILES[@]}"; do
  modified_file="${MODIFIED_FILES[$i]}"
  existing_file="${EXISTING_FILES[$i]}"
  path_to_write="${SOURCE_FILE_PATHS[$i]}" # Assume the model path is beyond index
  # Overwrite the files - in case of no changes, the file stays the same
  if ! cmp -s "$existing_file" "$modified_file"; then
   file_to_write="${SOURCE_FILE_NAMES[$i]}"
   echo "Overwriting file ${file_to_write}..."
   echo "$modified_file" > "${path_to_write}"
  fi
done

exit 1

### MAIN SCRIPT END ###
