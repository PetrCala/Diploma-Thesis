#!/bin/bash

# Copy the README into the Dist folder
cp README.md Dist/

# Copy the main text TeX file into the Dist folder
cp -fr "Thesis main/Diploma Thesis Cala Returns To Education.zip" "Dist/Diploma Thesis Cala Returns To Education.zip"

# Read dist_info.txt, remove carriage return characters and store the result in SOURCE_FILES variable
cd R
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
for f in $SOURCE_FILES; do
# Get the source file path considering both .R and .r extensions
source_file="../R/$f"
if test -f "${source_file%.*}.r"; then
  source_file="${source_file%.*}.r"
fi
# Check if the file doesn't exist in the Dist folder or if it's different from the one in the R folder
if ! test -f "$f" || ! cmp -s "$source_file" "$f"; then
  # Copy the file from the R folder to the Dist folder
  cp -fr "$source_file" .
fi
done


