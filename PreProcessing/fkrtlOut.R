# Load necessary libraries
library(dplyr)
library(stringr)
library(readr)
library(haven)

# Define patterns for the chapters to exclude
exclude_chapters <- c(
  "^O[0-9]", "^P[0-9]", "^R[0-9]", "^S[0-9]", "^T[0-9]", "^V[0-9]", "^W[0-9]", "^X[0-9]", "^Y[0-9]", "^Z[0-9]"
)

# Function to preprocess FKRTL data for 1516
preprocess_fkrtl_1516 <- function(file_path, exclude_chapters) {
  data <- read.delim(file_path, sep = "|", header = TRUE) # Use | as delimiter
  print("Column names and types for FKRTL 1516:")
  print(str(data)) # Print structure of the data
  
  total_rows_before <- nrow(data)
  print(paste("Total rows before filtering in FKRTL 1516:", total_rows_before))
  
  filtered_data <- data %>%
    filter(str_length(FKL17) == 3 & !str_detect(FKL17, paste(exclude_chapters, collapse = "|"))) %>%
    select(PSTV01, FKL03, FKL04, FKL17, FKL18) %>%
    rename(case_id = PSTV01, visit_date = FKL03, discharge_date = FKL04, diagnosis_code = FKL17, diagnosis_name = FKL18) %>%
    mutate(event_type = "FKRTL") %>%
    mutate(
      visit_date = as.Date(visit_date, format = "%d%b%Y"),
      discharge_date = as.Date(discharge_date, format = "%d%b%Y")
    )
  
  # Remove duplicates
  filtered_data <- filtered_data %>%
    distinct(case_id, visit_date, discharge_date, diagnosis_code, .keep_all = TRUE)
  
  total_rows_after <- nrow(filtered_data)
  print(paste("Total rows after filtering in FKRTL 1516:", total_rows_after))
  
  print("Filtered data FKRTL 1516:")
  print(str(filtered_data)) # Print structure of the filtered data
  return(filtered_data)
}

# Function to preprocess FKRTL data for 1718 and 1920
preprocess_fkrtl_1718_1920 <- function(file_path, exclude_chapters) {
  data <- read_dta(file_path)
  print("Column names and types for FKRTL 1718 and 1920:")
  print(str(data)) # Print structure of the data
  
  total_rows_before <- nrow(data)
  print(paste("Total rows before filtering in FKRTL 1718/1920:", total_rows_before))
  
  filtered_data <- data %>%
    filter(str_length(FKL17A) == 3 & !str_detect(FKL17A, paste(exclude_chapters, collapse = "|"))) %>%
    select(PSTV01, FKL03, FKL04, FKL17A, FKL18A) %>%
    rename(case_id = PSTV01, visit_date = FKL03, discharge_date = FKL04, diagnosis_code = FKL17A, diagnosis_name = FKL18A) %>%
    mutate(event_type = "FKRTL")
  
  # Remove duplicates
  filtered_data <- filtered_data %>%
    distinct(case_id, visit_date, discharge_date, diagnosis_code, .keep_all = TRUE)
  
  total_rows_after <- nrow(filtered_data)
  print(paste("Total rows after filtering in FKRTL 1718/1920:", total_rows_after))
  
  print("Filtered data FKRTL 1718 and 1920:")
  print(str(filtered_data)) # Print structure of the filtered data
  return(filtered_data)
}

# Preprocess each FKRTL dataset
fkrtl_1516 <- preprocess_fkrtl_1516("C:/Users/LENOVO/Documents/dataset/BPJS1516/04KunjunganFKRTL.csv", exclude_chapters)
fkrtl_1718 <- preprocess_fkrtl_1718_1920("C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/03_fkrtl.dta", exclude_chapters)
fkrtl_1920 <- preprocess_fkrtl_1718_1920("C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202003_fkrtl.dta", exclude_chapters)

# Print summaries to verify preprocessing
print("Summary of FKRTL 1516:")
print(summary(fkrtl_1516))

print("Summary of FKRTL 1718:")
print(summary(fkrtl_1718))

print("Summary of FKRTL 1920:")
print(summary(fkrtl_1920))

# Combine the datasets
combined_fkrtl_out <- bind_rows(
  fkrtl_1516,
  fkrtl_1718,
  fkrtl_1920
)

# Print summary of combined data to verify
print("Combined FKRTL:")
print(str(combined_fkrtl_out))

# Check types of each column
print("Column types in combined FKTP Non Kapitasi:")
print(sapply(combined_fkrtl_out, class))

print("Summary of Combined FKRTL:")
print(summary(combined_fkrtl_out))

# Check for missing timestamps and handle them if necessary
combined_fkrtl_out <- combined_fkrtl_out %>%
  filter(!is.na(visit_date) & !is.na(discharge_date))

# Save the combined dataset
write_csv(combined_fkrtl_out, "combined_fkrtl_out.csv")

# Calculate the sum of instances for each primary diagnosis code
diagnosis_code_summary <- combined_fkrtl_out %>%
  group_by(diagnosis_code) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  slice_head(n = 10)

# Print the summary of diagnosis codes
print("Top 10 diagnosis codes:")
print(diagnosis_code_summary)
