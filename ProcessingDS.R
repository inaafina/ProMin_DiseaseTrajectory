library(haven)
library(dplyr)

process_distinct_values <- function(file_path, file_type, column_name) {
  # Read the data
  if (file_type == "dta") {
    data <- read_dta(file_path)
  } else if (file_type == "csv") {
    data <- read.csv(file_path, sep = "|", stringsAsFactors = FALSE)
  }
  
  # Get the column names
  column_names <- names(data)
  print(column_names)
  
  # Check distinct values in the specified column with their counts
  distinct_values_counts <- data %>%
    group_by(!!sym(column_name)) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    slice_head(n = 10) # Get the top 10 rows
  
  # Print the result
  print(distinct_values_counts)
  
  return(column_names)
}

# File paths and types
file_paths <- list(
  "1718" = list(path = "C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/05_diagnosissekunder.dta", type = "dta"),
  "1920" = list(path = "C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202005_diagnosissekunder.dta", type = "dta")
)

# Process each dataset and get columns
columns_1718 <- process_distinct_values(file_paths$`1718`$path, file_paths$`1718`$type, "FKL24A")
columns_1920 <- process_distinct_values(file_paths$`1920`$path, file_paths$`1920`$type, "FKL24A")

# Function to check missing columns
check_missing_columns <- function(base_columns, compare_columns) {
  missing_in_compare <- setdiff(base_columns, compare_columns)
  missing_in_base <- setdiff(compare_columns, base_columns)
  return(list(missing_in_compare = missing_in_compare, missing_in_base = missing_in_base))
}

# Check missing columns
missing_1718_vs_1920 <- check_missing_columns(columns_1718, columns_1920)

# Print results
print("Missing columns between 1718 and 1920:")
print(missing_1718_vs_1920)
