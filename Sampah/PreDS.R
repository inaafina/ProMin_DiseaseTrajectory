# Load necessary libraries
library(dplyr)
library(stringr)
library(readr)
library(haven)

# Function to preprocess Diagnosis Sekunder data for 1718 and 1920
preprocess_diagnosis_sekunder <- function(file_path) {
  data <- read_dta(file_path)
  print("Column names and types for Diagnosis Sekunder:")
  print(str(data)) # Print structure of the data
  filtered_data <- data %>%
    select(FKL02, FKL24A, FKL24B) %>%
    rename(case_id = FKL02, diagnosis_code = FKL24A, diagnosis_name = FKL24B) %>%
    mutate(event_type = "Diagnosis Sekunder")
  print("Filtered data Diagnosis Sekunder:")
  print(str(filtered_data)) # Print structure of the filtered data
  return(filtered_data)
}

# Preprocess each Diagnosis Sekunder dataset
diagnosis_sekunder_1718 <- preprocess_diagnosis_sekunder("C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/05_diagnosissekunder.dta")
diagnosis_sekunder_1920 <- preprocess_diagnosis_sekunder("C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202005_diagnosissekunder.dta")

# Print summaries to verify preprocessing
print("Summary of Diagnosis Sekunder 1718:")
print(summary(diagnosis_sekunder_1718))

print("Summary of Diagnosis Sekunder 1920:")
print(summary(diagnosis_sekunder_1920))

# Combine the datasets
combined_diagnosis_sekunder <- bind_rows(
  diagnosis_sekunder_1718,
  diagnosis_sekunder_1920
)

# Check combined data structure
print("Combined data Diagnosis Sekunder:")
print(str(combined_diagnosis_sekunder))

# Print summary of combined data to verify
print("Summary of Combined Diagnosis Sekunder:")
print(summary(combined_diagnosis_sekunder))

# Save the combined dataset
write_csv(combined_diagnosis_sekunder, "combined_diagnosis_sekunder.csv")
