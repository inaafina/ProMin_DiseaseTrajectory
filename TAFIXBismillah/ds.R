# Load necessary libraries
library(dplyr)
library(stringr)
library(readr)
library(haven)
library(lubridate)

# Define patterns for the chapters to exclude
exclude_chapters <- c(
  "^O[0-9]", "^P[0-9]", "^R[0-9]", "^S[0-9]", "^T[0-9]", "^V[0-9]", "^W[0-9]", "^X[0-9]", "^Y[0-9]", "^Z[0-9]"
)

is_ckd_code <- function(code) {
  ifelse(str_detect(code, "^(N18[1-5]|N189|I12[0-9]|I13[0-9]|N18|I12|I13)$"), "CKD", code)
}

# Function to preprocess Diagnosis Sekunder data for 1718 and 1920
preprocess_diagnosis_sekunder <- function(file_path) {
  data <- read_dta(file_path)
  print("Column names and types for Diagnosis Sekunder:")
  print(str(data)) # Print structure of the data
  
  total_rows_before <- nrow(data)
  print(paste("Total rows before filtering in Diagnosis Sekunder:", total_rows_before))
  
  filtered_data <- data %>%
    select(FKL02, FKL24A, FKL24B) %>%
    rename(case_id = FKL02, diagnosis_code = FKL24A, diagnosis_name = FKL24B) %>%
    mutate(event_type = "Diagnosis Sekunder") %>%
    filter(str_length(diagnosis_code) >= 3 & str_length(diagnosis_code) <= 5 & !str_detect(diagnosis_code, paste(exclude_chapters, collapse = "|"))) %>%
    mutate(diagnosis_code = sapply(diagnosis_code, is_ckd_code))
  
  print("Filtered data Diagnosis Sekunder:")
  print(str(filtered_data)) # Print structure of the filtered data
  
  # Prioritize CKD (CKD) if multiple diseases exist for a case_id
  filtered_data <- filtered_data %>%
    group_by(case_id) %>%
    arrange(desc(diagnosis_code == "CKD"), .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  # Remove duplicates
  filtered_data <- filtered_data %>%
    distinct(case_id, diagnosis_code, .keep_all = TRUE)
  
  total_rows_after <- nrow(filtered_data)
  print(paste("Total rows after filtering in Diagnosis Sekunder:", total_rows_after))
  
  print("Data after prioritizing CKD (CKD) and removing duplicates:")
  print(str(filtered_data)) # Print structure after prioritizing CKD and removing duplicates
  
  return(filtered_data)
}

# Preprocess each Diagnosis Sekunder dataset
diagnosis_sekunder_1718 <- preprocess_diagnosis_sekunder("C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/05_diagnosissekunder.dta")
diagnosis_sekunder_1920 <- preprocess_diagnosis_sekunder("C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202005_diagnosissekunder.dta")

# Combine the datasets
combined_diagnosis_sekunder <- bind_rows(
  diagnosis_sekunder_1718,
  diagnosis_sekunder_1920
)

# Load the non-preprocessed FKRTL datasets and select necessary columns
fkrtl_1516 <- read_delim("C:/Users/LENOVO/Documents/dataset/BPJS1516/04KunjunganFKRTL.csv", delim = "|") %>%
  select(FKL02, FKL03, FKL04) %>%
  mutate(FKL02 = as.character(FKL02), FKL03 = as.Date(FKL03, format="%d-%b-%Y"), FKL04 = as.Date(FKL04, format="%d-%b-%Y"))
fkrtl_1718 <- read_dta("C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/03_fkrtl.dta") %>%
  select(FKL02, FKL03, FKL04) %>%
  mutate(FKL02 = as.character(FKL02), FKL03 = as.Date(FKL03, format="%Y-%m-%d"), FKL04 = as.Date(FKL04, format="%Y-%m-%d"))
fkrtl_1920 <- read_dta("C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202003_fkrtl.dta") %>%
  select(FKL02, FKL03, FKL04) %>%
  mutate(FKL02 = as.character(FKL02), FKL03 = as.Date(FKL03, format="%Y-%m-%d"), FKL04 = as.Date(FKL04, format="%Y-%m-%d"))

# Combine FKRTL datasets
combined_fkrtl <- bind_rows(fkrtl_1516, fkrtl_1718, fkrtl_1920)

# Merge Diagnosis Sekunder with FKRTL to get timestamps and correct case IDs
combined_diagnosis_sekunder <- combined_diagnosis_sekunder %>%
  left_join(combined_fkrtl %>% select(FKL02, FKL03, FKL04) %>% rename(case_id = FKL02, visit_date = FKL03, discharge_date = FKL04), by = "case_id") %>%
  mutate(visit_date = as.POSIXct(visit_date) + 10*60, # Add 10 minutes to FKRTL timestamp
         discharge_date = as.POSIXct(discharge_date) + 10*60,
         new_case_id = case_id) %>%
  select(new_case_id, visit_date, discharge_date, diagnosis_code, diagnosis_name, event_type) %>%
  rename(case_id = new_case_id)

# Verify the column types to ensure they are POSIXct
print("Column types after ensuring POSIXct format in Diagnosis Sekunder:")
print(sapply(combined_diagnosis_sekunder, class))

# Check combined data structure
print("Combined data Diagnosis Sekunder with timestamps:")
print(str(combined_diagnosis_sekunder))

# Print summary of combined data to verify
print("Summary of Combined Diagnosis Sekunder with timestamps:")
print(summary(combined_diagnosis_sekunder))

# Save the combined dataset
write_csv(combined_diagnosis_sekunder, "combined_diagnosis_sekunder.csv")

# Display total number of rows
total_rows <- nrow(combined_diagnosis_sekunder)
print(paste("Total number of rows:", total_rows))

# Get the top 5 diseases
top_5_diseases <- combined_diagnosis_sekunder %>%
  group_by(diagnosis_code, diagnosis_name) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  slice_head(n = 5)

print("Top 5 diseases in Diagnosis Sekunder:")
print(top_5_diseases)
