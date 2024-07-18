# Load necessary libraries
library(dplyr)
library(stringr)
library(readr)
library(haven)

# Define CKD-related diagnosis codes pattern
ckd_codes <- "^N18$|^N018$|^N181$|^N182$|^N183$|^N184$|^N185$|^N186$|^N187$|^N188$|^N189$"

# Function to preprocess FKRTL data for 1516
preprocess_fkrtl_1516 <- function(file_path, ckd_codes) {
  data <- read.delim(file_path, sep = "|", header = TRUE) # Use | as delimiter
  print("Column names and types for FKRTL 1516:")
  print(str(data)) # Print structure of the data
  filtered_data <- data %>%
    filter(str_detect(FKL15, ckd_codes) | str_detect(FKL17, ckd_codes)) %>%
    select(PSTV01, FKL02, FKL03, FKL04, FKL15, FKL16, FKL17, FKL18) %>%
    rename(visit_id = FKL02, visit_date = FKL03, discharge_date = FKL04, initial_diagnosis_code = FKL15, initial_diagnosis_name = FKL16, primary_diagnosis_code = FKL17, primary_diagnosis_name = FKL18) %>%
    mutate(event_type = "FKRTL") %>%
    mutate(
      arrival_date = as.Date(arrival_date, format = "%d%b%Y"),
      discharge_date = as.Date(discharge_date, format = "%d%b%Y")
    )
  print("Filtered data FKRTL 1516:")
  print(str(filtered_data)) # Print structure of the filtered data
  return(filtered_data)
}

# Function to preprocess FKRTL data for 1718 and 1920
preprocess_fkrtl_1718_1920 <- function(file_path, ckd_codes) {
  data <- read_dta(file_path)
  print("Column names and types for FKRTL 1718 and 1920:")
  print(str(data)) # Print structure of the data
  filtered_data <- data %>%
    filter(str_detect(FKL15A, ckd_codes) | str_detect(FKL17A, ckd_codes)) %>%
    select(PSTV01, FKL02, FKL03, FKL04, FKL15A, FKL16A, FKL17A, FKL18A) %>%
    rename(visit_id = FKL02, visit_date = FKL03, discharge_date = FKL04, initial_diagnosis_code = FKL15A, initial_diagnosis_name = FKL16A, primary_diagnosis_code = FKL17A, primary_diagnosis_name = FKL18A) %>%
    mutate(event_type = "FKRTL")
  print("Filtered data FKRTL 1718 and 1920:")
  print(str(filtered_data)) # Print structure of the filtered data
  return(filtered_data)
}

# Preprocess each FKRTL dataset
fkrtl_1516 <- preprocess_fkrtl_1516("C:/Users/LENOVO/Documents/dataset/BPJS1516/04KunjunganFKRTL.csv", ckd_codes)
fkrtl_1718 <- preprocess_fkrtl_1718_1920("C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/03_fkrtl.dta", ckd_codes)
fkrtl_1920 <- preprocess_fkrtl_1718_1920("C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202003_fkrtl.dta", ckd_codes)

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
print("Combined FKRTL:")
print(combined_fkrtl)

# Print summary of combined data to verify
print("Summary of Combined FKRTL:")
print(summary(combined_fkrtl))

# Check for missing timestamps and handle them if necessary
combined_fkrtl <- combined_fkrtl %>%
  filter(!is.na(arrival_date) & !is.na(discharge_date))

# Save the combined dataset
write_csv(combined_fkrtl, "combined_fkrtl.csv")
