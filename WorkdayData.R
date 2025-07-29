# Choose a file using a dialog box
file_path <- file.choose()

# Print the selected file path (for verification)
print(file_path)

# Now you can use file_path with your data loading function
# For example, if it's a CSV file:
if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
  feesCollected <- read.csv(file_path)
  print("CSV file loaded successfully!")
  head(feesCollected)
} else if (grepl("\\.txt$", file_path, ignore.case = TRUE)) {
  feesCollected <- read.table(file_path, header = TRUE) # Example for text file with header
  print("Text file loaded successfully!")
  head(feesCollected)
} else if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
  # You'll need the 'readxl' package for Excel files
  # install.packages("readxl") # Uncomment and run if you don't have it
  library(readxl)
  feesCollected <- read_excel(file_path)
  print("Excel file loaded successfully!")
  head(feesCollected)
} else {
  print("Unsupported file type or no file selected.")
}

summary(as.factor(feesCollected$`Cost Center ID`))
attributes(feesCollected)
# Make sure you have dplyr installed and loaded
# install.packages("dplyr")
library(dplyr)

feesCollected <- feesCollected %>%
  mutate(
    # Convert 'Department' and 'Division' to factors
    Department = as.factor(Department),
    Division = as.factor(Division),
    
    # Convert 'Amount' to numeric
    Amount = as.numeric(Amount),
    
    # Convert 'Header Memo' and 'Line Memo' to character
    `Header Memo` = as.character(`Header Memo`),
    `Line Memo` = as.character(`Line Memo`)
  )

# Display the structure of the data frame and the type of each column
str(feesCollected)