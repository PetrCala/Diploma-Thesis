#!/bin/bash

# STATIC
SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
R_FOLDER_PATH="$PROJECT_ROOT/R"
DIST_FOLDER_PATH="$PROJECT_ROOT/Distribute"

# Base path 

cd $PROJECT_ROOT

# Read dist_info.txt, remove carriage return characters and store the result in SOURCE_FILES variable
cd $R_FOLDER_PATH

# Copy the R README into the Dist folder
echo "Copying the R folder README.md file..."
cp "$R_FOLDER_PATH/README.md" "$DIST_FOLDER_PATH"

SOURCE_FILES=$(cat "$R_FOLDER_PATH/dist_info.txt" | tr -d '\r')
for f in $SOURCE_FILES; do
# Check if the file exists in the R folder, considering both .R and .r extensions
if ! test -f "$f" && ! test -f "${f%.*}.r"; then
  echo "Error: $f not found in R folder"
  exit 1
fi
done

# Copy the updated files from the R folder to the Dist folder based on the file names in SOURCE_FILES
cd $DIST_FOLDER_PATH

# Copy the external package folder, scripts folder, resources folder
echo "Copying packages..."
cp -r "$R_FOLDER_PATH/pckg/" .
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
for f in $SOURCE_FILES; do
 # Get the source file path considering both .R and .r extensions
 source_file="$R_FOLDER_PATH/$f"
 if test -f "${source_file%.*}.r"; then
   source_file="${source_file%.*}.R"
 fi
 # Check if the file doesn't exist in the Dist folder or if it's different from the one in the R folder
 if ! test -f "$f" || ! cmp -s "$source_file" "$f"; then
   # If the file contains "data", save to data folder instead
   if [[ $source_file == *"data"* ]]; then
     out_folder="$DIST_FOLDER_PATH/data/"
     if [[ $source_file == *"source"* ]]; then
 	out_folder="$DIST_FOLDER_PATH/data/source/"
     fi
   else
     out_folder="$DIST_FOLDER_PATH"
   fi
   # Copy the file from the R folder to the Dist (or data) folder
   cp -fr "$source_file" "$out_folder"
 fi
done
