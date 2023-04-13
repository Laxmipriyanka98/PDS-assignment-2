library(dplyr)
# Read the CSV file into a dataframe
df <- read.csv('C:\Users\priya\OneDrive\Documents\Priyanka\downloads\major project\Personalized Market Basket Prediction with\Personalized Market Basket Prediction with\Code\pds\pds assignment\assignment-2')

# Check for missing values in each column
missing_values <- colSums(is.na(df))

# Print the missing values count for each columnn
cat("Missing Values Count:\n")
print(missing_values)
#Since there are no missing values the data provided is already clean