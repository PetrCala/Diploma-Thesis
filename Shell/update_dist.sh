#!/bin/bash

# STATIC
SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
R_FOLDER_PATH="$PROJECT_ROOT/artma"
DIST_FOLDER_PATH="$PROJECT_ROOT/Distribute"

# Define a list of files to copy - relative to the R folder
SOURCE_FILES=(
  "data/source/data_set_master_thesis_cala.xlsx"
  "main_master_thesis_cala.R"
  "script_runner_master_thesis_cala.R"
  "source_master_thesis_cala.R"
  "README.md"
  ".gitignore"
  ".lintr"
  "LICENSE"
)

for f in "${SOURCE_FILES[@]}"; do
  # Assume all source files are located inside the R folder
  source_file="$R_FOLDER_PATH/$f"
  # Check if the file exists in the R folder
  if ! test -f "$source_file"; then
    echo "Error: $f not found under path $source_file"
    echo "Terminating the process"
    exit 1
  fi
done

# Create the Distribute folder (if does not exist) and clean it
[ ! -d "$DIST_FOLDER_PATH" ] && mkdir "$DIST_FOLDER_PATH"
rm -rf "$DIST_FOLDER_PATH/*"

# Copy the updated files from the R folder to the Distribution folder based on the file names in SOURCE_FILES
cd $DIST_FOLDER_PATH

# Copy the external package folder, scripts folder, resources folder
echo "Copying packages..."
cp -r "$R_FOLDER_PATH/pckg/" .
echo "Copying src..."
cp -r "$R_FOLDER_PATH/src/" .
echo "Copying scripts..."
cp -r "$R_FOLDER_PATH/scripts/" .
echo "Copying resources..."
cp -r "$R_FOLDER_PATH/resources/" .

# Create empty folders for results storing
new_folders=("data" "graphics" "results" "data/source" "data/temp")

# Loop through the list of characters
for folder in "${new_folders[@]}"; do
  # Check if the folder already exists
  if [ ! -d "$DIST_FOLDER_PATH/$folder" ]; then
    # Create the folder
    mkdir "$DIST_FOLDER_PATH/$folder"
    echo "Folder created: $folder"
  fi
done

# Handle R source files
for f in "${SOURCE_FILES[@]}"; do
  # Get the source file path
  source_file="$R_FOLDER_PATH/$f"

  # Construct the destination path
  destination="$DIST_FOLDER_PATH/$f"

  # Copy the file over to the destination
  echo "Copying file $f..."
  cp -fr "$source_file" "$destination"
done
