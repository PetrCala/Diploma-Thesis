#!/bin/bash


# Copy the main text TeX file into the Dist folder
#cp -fr "Thesis main/Diploma Thesis Cala Returns To Education.zip" "Dist/Diploma Thesis Cala Returns To Education.zip"

# Read dist_info.txt, remove carriage return characters and store the result in SOURCE_FILES variable
cd R

# Copy the R README into the Dist folder
cp README.md ../Dist/

SOURCE_FILES=$(cat dist_info.txt | tr -d '\r')
for f in $SOURCE_FILES; do
# Check if the file exists in the R folder, considering both .R and .r extensions
if ! test -f "$f" && ! test -f "${f%.*}.r"; then
  echo "Error: $f not found in R folder"
  exit 1
fi
done

# Copy the updated files from the R folder to the Dist folder based on the file names in SOURCE_FILES
cd ../Dist

# Copy the external package folder, scripts folder
cp -r "../R/pckg/" .
cp -r "../R/scripts/" .

# Create empty folders for results storing
new_folders=("data" "graphics" "results" "data/source" "data/temp")

# Loop through the list of characters
for folder in "${new_folders[@]}"; do
    # Check if the folder already exists
    if [ ! -d "$folder" ]; then
        # Create the folder
        mkdir "$folder"
        echo "Folder created: $folder"
    fi
done

# Handle R source files
for f in $SOURCE_FILES; do
# Get the source file path considering both .R and .r extensions
source_file="../R/$f"
if test -f "${source_file%.*}.r"; then
  source_file="${source_file%.*}.R"
fi
# Check if the file doesn't exist in the Dist folder or if it's different from the one in the R folder
if ! test -f "$f" || ! cmp -s "$source_file" "$f"; then
  # If the file contains "data", save to data folder instead
  if [[ $source_file == *"data"* ]]; then
    out_folder="./data/"
    if [[ $source_file == *"source"* ]]; then
	out_folder="./data/source/"
    fi
  else
    out_folder="."
  fi
  # Copy the file from the R folder to the Dist (or data) folder
  cp -fr "$source_file" "$out_folder"
fi
done


# Modify the development option of the .yaml file
# sed -i 's/development_on: yes/development_on: no/g' "user_parameters.yaml"
