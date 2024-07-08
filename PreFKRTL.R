# Load necessary libraries
library(dplyr)
library(stringr)
library(readr)
library(haven)

# Define CKD-related diagnosis code
ckd_code <- "N18"

# Function to preprocess FKRTL data for 1516
preprocess_fkrtl_1516 <- function(file_path) {
  data <- read.delim(file_path, sep = "|", header = TRUE) # Use | as delimiter
  print("Column names and types for FKRTL 1516:")
  print(str(data)) # Print structure of the data
  filtered_data <- data %>%
    select(PSTV01, FKL03, FKL04, FKL15, FKL16) %>%
    rename(start_date = FKL03, complete_date = FKL04, diagnosis_code = FKL15, diagnosis_name = FKL16) %>%
    mutate(event_type = "FKRTL") %>%
    mutate(
      start_date = as.Date(start_date, format = "%d%b%Y"),
      complete_date = as.Date(complete_date, format = "%d%b%Y")
    )
  print("Filtered data FKRTL 1516:")
  print(str(filtered_data)) # Print structure of the filtered data
  return(filtered_data)
}

# Function to preprocess FKRTL data for 1718 and 1920
preprocess_fkrtl_1718_1920 <- function(file_path) {
  data <- read_dta(file_path)
  print("Column names and types for FKRTL 1718 and 1920:")
  print(str(data)) # Print structure of the data
  filtered_data <- data %>%
    select(PSTV01, FKL03, FKL04, FKL15A, FKL16A) %>%
    rename(start_date = FKL03, complete_date = FKL04, diagnosis_code = FKL15A, diagnosis_name = FKL16A) %>%
    mutate(event_type = "FKRTL") %>%
    mutate(
      start_date = as.Date(start_date, format = "%Y-%m-%d"),  # Assuming the date format is correct here
      complete_date = as.Date(complete_date, format = "%Y-%m-%d")
    )
  print("Filtered data FKRTL 1718 and 1920:")
  print(str(filtered_data)) # Print structure of the filtered data
  return(filtered_data)
}

# Preprocess each FKRTL dataset
fkrtl_1516 <- preprocess_fkrtl_1516("C:/Users/LENOVO/Documents/dataset/BPJS1516/04KunjunganFKRTL.csv")
fkrtl_1718 <- preprocess_fkrtl_1718_1920("C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/03_fkrtl.dta")
fkrtl_1920 <- preprocess_fkrtl_1718_1920("C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202003_fkrtl.dta")

# Print summaries to verify preprocessing
print("Summary of FKRTL 1516:")
print(summary(fkrtl_1516))

print("Summary of FKRTL 1718:")
print(summary(fkrtl_1718))

print("Summary of FKRTL 1920:")
print(summary(fkrtl_1920))

# Combine the datasets
combined_fkrtl <- bind_rows(
  fkrtl_1516,
  fkrtl_1718,
  fkrtl_1920
)

# Print summary of combined data to verify
print("Summary of Combined FKRTL:")
print(summary(combined_fkrtl))

# Check for missing timestamps and handle them if necessary
combined_fkrtl <- combined_fkrtl %>%
  filter(!is.na(start_date) & !is.na(complete_date))

# Save the combined dataset
write_csv(combined_fkrtl, "combined_fkrtl.csv")
