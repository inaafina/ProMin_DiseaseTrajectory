library(haven)
library(dplyr)
library(stringr)

# Function to process distinct values and filter data based on a pattern
process_distinct_values <- function(file_path, file_type, column_name, filter_pattern = NULL, filter_columns = NULL) {
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
  
  # If a filter pattern is provided, filter the data
  if (!is.null(filter_pattern) && !is.null(filter_columns)) {
    filtered_data <- data %>%
      select(all_of(filter_columns)) %>%
      filter(str_detect(!!sym(column_name), filter_pattern))
    
    print(head(filtered_data, 10))
    print(paste("Total rows after filtering:", nrow(filtered_data)))
    
    return(list(data = filtered_data, columns = column_names))
  } else {
    return(column_names)
  }
}

# File paths and types for fkrtl
file_paths <- list(
  "1516" = list(path = "C:/Users/LENOVO/Documents/dataset/BPJS1516/04KunjunganFKRTL.csv", type = "csv"),
  "1718" = list(path = "C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/03_fkrtl.dta", type = "dta"),
  "1920" = list(path = "C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202003_fkrtl.dta", type = "dta")
)

# Columns to filter
filter_columns <- c("PSTV01", "FKL15", "FKL17", "FKL18", "FKL14")

# Process each dataset and get columns
results_1516 <- process_distinct_values(file_paths$`1516`$path, file_paths$`1516`$type, "FKL18", "N18", filter_columns)
results_1718 <- process_distinct_values(file_paths$`1718`$path, file_paths$`1718`$type, "FKL18", "N18", filter_columns)
results_1920 <- process_distinct_values(file_paths$`1920`$path, file_paths$`1920`$type, "FKL18", "N18", filter_columns)

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
