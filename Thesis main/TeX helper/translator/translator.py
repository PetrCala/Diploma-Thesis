# Read a text file with TeX raw table text and preprocess it so
# that the text is recognizable by the python latex functions.
# Store the output under "translate_out.txt"

IN_FILE = "translate_in.txt"
OUT_FILE = "translate_out.txt"

# Open the input file
with open(IN_FILE, "r") as in_file:
    # Read the contents
    content = in_file.read()

# Do the preprocessing
# Replace all backslashes with double backslashes
content = content.replace("\\", "\\\\")

# Replace all line breaks with "\n" and merge the lines into one
content = content.replace("\n", "\\n")

# Open the output file
with open(OUT_FILE, "w") as out_file:
    # Write the preprocessed content
    out_file.write(content)