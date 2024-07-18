# Load necessary libraries
library(haven)
library(dplyr)
library(readr)
library(stringr)

# Function to analyze columns
analyze_columns <- function(file_path, file_type = "dta") {
  # Read the data
  if (file_type == "dta") {
    data <- read_dta(file_path)
  } else if (file_type == "csv") {
    data <- read_csv(file_path, col_names = TRUE, show_col_types = FALSE) # Specify delimiter and suppress column type message
  }
  
  # Get the column names
  column_names <- names(data)
  print(column_names)
  
  # Initialize a list to store results
  results <- list()
  
  # Analyze distinct values in each column
  for (col in column_names) {
    if (is.factor(data[[col]]) || is.character(data[[col]])) {
      distinct_values_counts <- data %>%
        group_by(!!sym(col)) %>%
        summarise(count = n()) %>%
        arrange(desc(count)) %>%
        slice_head(n = 10)
      results[[col]] <- distinct_values_counts
      print(distinct_values_counts)
    }
  }
  
  return(list(results = results, columns = column_names))
}

# Function to count CKD patients
count_ckd_patients <- function(data, column_name, ckd_code) {
  ckd_patients <- data %>%
    filter(str_detect(!!sym(column_name), ckd_code)) %>%
    summarise(count = n_distinct(PSTV01))
  return(ckd_patients$count)
}

# File paths and types
file_paths <- list(
  "1516" = list(path = "C:/Users/LENOVO/Documents/dataset/BPJS1516/03KunjunganFKTPnonkapitasi.csv", type = "csv"),
  "1718" = list(path = "C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/04_nonkapitasi.dta", type = "dta"),
  "1920" = list(path = "C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202004_nonkapitasi.dta", type = "dta")
)

# Analyze each dataset and get column names
results_1516 <- analyze_columns(file_paths$`1516`$path, file_paths$`1516`$type)
results_1718 <- analyze_columns(file_paths$`1718`$path, file_paths$`1718`$type)
results_1920 <- analyze_columns(file_paths$`1920`$path, file_paths$`1920`$type)

# Extract column names
columns_1516 <- results_1516$columns
columns_1718 <- results_1718$columns
columns_1920 <- results_1920$columns

# Function to check missing columns
check_missing_columns <- function(base_columns, compare_columns) {
  missing_in_compare <- setdiff(base_columns, compare_columns)
  missing_in_base <- setdiff(compare_columns, base_columns)
  return(list(missing_in_compare = missing_in_compare, missing_in_base = missing_in_base))
}

# Check missing columns
missing_1516_vs_1718 <- check_missing_columns(columns_1516, columns_1718)
missing_1516_vs_1920 <- check_missing_columns(columns_1516, columns_1920)
missing_1718_vs_1920 <- check_missing_columns(columns_1718, columns_1920)

# Print results
print("Missing columns between 1516 and 1718:")
print(missing_1516_vs_1718)
print("Missing columns between 1516 and 1920:")
print(missing_1516_vs_1920)
print("Missing columns between 1718 and 1920:")
print(missing_1718_vs_1920)

library(readr)
library(haven)

# Sample function to count CKD patients with added debugging
count_ckd_patients <- function(df, column_name, ckd_code) {
  # Check if the column exists
  if (!column_name %in% names(df)) {
    stop(paste("Column", column_name, "does not exist in the data frame"))
  }
  
  # Print column data type and first few entries
  print(paste("Data type of column", column_name, ":", class(df[[column_name]])))
  print("First few entries in the column:")
  print(head(df[[column_name]]))
  
  # Filter rows where the specified column starts with the ckd_code
  pattern <- paste0("^", ckd_code)
  matching_indices <- grepl(pattern, df[[column_name]])
  
  # Print debugging information
  print(paste("Pattern:", pattern))
  print(paste("Number of matches:", sum(matching_indices)))
  print(paste("Length of matching_indices:", length(matching_indices)))
  
  # Ensure matching_indices has the same length as the number of rows in the data frame
  if (length(matching_indices) != nrow(df)) {
    stop("The length of matching_indices does not match the number of rows in the data frame")
  }
  
  filtered_df <- df[matching_indices, ]
  
  # Return the count of filtered rows
  return(nrow(filtered_df))
}

# Load the datasets
fktp_non_kapitasi_1516 <- read_delim("C:/Users/LENOVO/Documents/dataset/BPJS1516/03KunjunganFKTPnonkapitasi.csv", delim = "|", col_names = TRUE, show_col_types = TRUE)
fktp_non_kapitasi_1718 <- read_dta("C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/04_nonkapitasi.dta")
fktp_non_kapitasi_1920 <- read_dta("C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202004_nonkapitasi.dta")

ckd_code <- "N18"

# Count CKD patients
ckd_patients_1516 <- count_ckd_patients(fktp_non_kapitasi_1516, "PNK13", ckd_code)
ckd_patients_1718 <- count_ckd_patients(fktp_non_kapitasi_1718, "PNK13A", ckd_code)
ckd_patients_1920 <- count_ckd_patients(fktp_non_kapitasi_1920, "PNK13A", ckd_code)

# Print results
print(paste("Number of CKD patients in 1516 dataset:", ckd_patients_1516))
print(paste("Number of CKD patients in 1718 dataset:", ckd_patients_1718))
print(paste("Number of CKD patients in 1920 dataset:", ckd_patients_1920))


#--------------------------------
#checking code


  

