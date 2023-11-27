#!/bin/bash

# Folder paths
project_root="$(dirname "$(readlink -f "$0")")"
shell_scripts_path="$project_root/Shell"

# Declare a list of scripts to run
declare -a script_list=(
    # "update_user_params"
    "update_dist"
)

cd $project_root

# A method that calls a script by the script name
run_script() {
    local script_base_name="$1"
    local script_full_name="$script_base_name.sh"
    local script_path="$shell_scripts_path/$script_full_name"
    
    echo "Running $script_full_name..."
    
    if ! [[ -f "$script_path" ]]; then
        echo "The $script_full_name could not be located under the path $script_path."
        return 1
    fi
    
    chmod +x "$script_path"
    "$script_path"
}

# Execute the scripts. Comment out any script in script_list you don't want to run.
for script in "${script_list[@]}"; do
    run_script "$script"
done
