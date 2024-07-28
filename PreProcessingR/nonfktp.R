# Load necessary libraries
library(dplyr)
library(stringr)
library(readr)
library(haven)

# Define patterns for the chapters to exclude
exclude_chapters <- c(
  "^O[0-9]", "^P[0-9]", "^R[0-9]", "^S[0-9]", "^T[0-9]", "^V[0-9]", "^W[0-9]", "^X[0-9]", "^Y[0-9]", "^Z[0-9]"
)

# Function to check for CKD related diagnosis codes and label them as "CKD"
is_ckd_code <- function(code) {
  ifelse(str_detect(code, "^(N18[1-5]|N189|I12[0-9]|I13[0-9]|N18|I12|I13)$"), "CKD", code)
}

# Function to ensure diagnosis codes are 3 characters in length
normalize_diagnosis_code <- function(code) {
  if (nchar(code) > 3) {
    substr(code, 1, 3)
  } else {
    str_pad(code, 3, side = "right", pad = "0")
  }
}

# Function to preprocess FKTP Non Kapitasi data for 1516
preprocess_fktp_non_kapitasi_1516 <- function(file_path) {
  data <- read.delim(file_path, sep = "|", header = TRUE) # Use | as delimiter
  print("Column names and types for FKTP Non Kapitasi 1516:")
  print(str(data)) # Print structure of the data
  
  total_rows_before <- nrow(data)
  print(paste("Total rows before filtering in FKTP Non Kapitasi 1516:", total_rows_before))
  
  # Check for N18 before filtering
  n18_before_filter <- sum(data$PNK13 == "N18")
  print(paste("N18 count before filtering in FKTP Non Kapitasi 1516:", n18_before_filter))
  
  filtered_data <- data %>%
    select(PSTV01, PNK03, PNK05, PNK13, PNK14) %>%
    rename(case_id = PSTV01, visit_date = PNK03, discharge_date = PNK05, diagnosis_code = PNK13, diagnosis_name = PNK14) %>%
    mutate(event_type = "FKTP Non-Kapitasi") %>%
    filter(str_length(diagnosis_code) >= 3 & str_length(diagnosis_code) <= 5 & !str_detect(diagnosis_code, paste(exclude_chapters, collapse = "|"))) %>%
    mutate(
      visit_date = as.POSIXct(visit_date, format = "%d%b%Y", tz = "UTC"),
      discharge_date = as.POSIXct(discharge_date, format = "%d%b%Y", tz = "UTC"),
      diagnosis_code = sapply(diagnosis_code, is_ckd_code)
    )
  
  # Remove duplicates
  filtered_data <- filtered_data %>%
    distinct(case_id, visit_date, discharge_date, diagnosis_code, .keep_all = TRUE)
  
  total_rows_after <- nrow(filtered_data)
  print(paste("Total rows after filtering in FKTP Non Kapitasi 1516:", total_rows_after))
  
  # Check for CKD after filtering
  ckd_after_filter <- sum(filtered_data$diagnosis_code == "CKD")
  print(paste("CKD count after filtering in FKTP Non Kapitasi 1516:", ckd_after_filter))
  
  print("Filtered data FKTP Non Kapitasi 1516:")
  print(str(filtered_data)) # Print structure of the filtered data
  return(filtered_data)
}

# Function to preprocess FKTP Non Kapitasi data for 1718 and 1920
preprocess_fktp_non_kapitasi_1718_1920 <- function(file_path) {
  data <- read_dta(file_path)
  print("Column names and types for FKTP Non Kapitasi 1718 and 1920:")
  print(str(data)) # Print structure of the data
  
  total_rows_before <- nrow(data)
  print(paste("Total rows before filtering in FKTP Non Kapitasi 1718/1920:", total_rows_before))
  
  # Check for CKD before filtering
  ckd_before_filter <- sum(is_ckd_code(data$PNK13A) == "CKD")
  print(paste("CKD count before filtering in FKTP Non Kapitasi 1718/1920:", ckd_before_filter))
  
  filtered_data <- data %>%
    select(PSTV01, PNK03, PNK05, PNK14, PNK15) %>%
    rename(case_id = PSTV01, visit_date = PNK03, discharge_date = PNK05, diagnosis_code = PNK14, diagnosis_name = PNK15) %>%
    mutate(event_type = "FKTP Non-Kapitasi") %>%
    filter(str_length(diagnosis_code) >= 3 & str_length(diagnosis_code) <= 5 & !str_detect(diagnosis_code, paste(exclude_chapters, collapse = "|"))) %>%
    mutate(
      visit_date = as.POSIXct(visit_date, format = "%Y-%m-%d", tz = "UTC"),
      discharge_date = as.POSIXct(discharge_date, format = "%Y-%m-%d", tz = "UTC"),
      diagnosis_code = sapply(diagnosis_code, is_ckd_code)
    )
  
  # Remove duplicates
  filtered_data <- filtered_data %>%
    distinct(case_id, visit_date, discharge_date, diagnosis_code, .keep_all = TRUE)
  
  total_rows_after <- nrow(filtered_data)
  print(paste("Total rows after filtering in FKTP Non Kapitasi 1718/1920:", total_rows_after))
  
  # Check for CKD after filtering
  ckd_after_filter <- sum(filtered_data$diagnosis_code == "CKD")
  print(paste("CKD count after filtering in FKTP Non Kapitasi 1718/1920:", ckd_after_filter))
  
  print("Filtered data FKTP Non Kapitasi 1718 and 1920:")
  print(str(filtered_data)) # Print structure of the filtered data
  return(filtered_data)
}

# Preprocess each FKTP Non Kapitasi dataset
fktp_non_kapitasi_1516 <- preprocess_fktp_non_kapitasi_1516("C:/Users/LENOVO/Documents/dataset/BPJS1516/03KunjunganFKTPnonkapitasi.csv")
fktp_non_kapitasi_1718 <- preprocess_fktp_non_kapitasi_1718_1920("C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/04_nonkapitasi.dta")
fktp_non_kapitasi_1920 <- preprocess_fktp_non_kapitasi_1718_1920("C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202004_nonkapitasi.dta")

# Print summaries to verify preprocessing
print("Summary of FKTP Non Kapitasi 1516:")
print(summary(fktp_non_kapitasi_1516))

print("Summary of FKTP Non Kapitasi 1718:")
print(summary(fktp_non_kapitasi_1718))

print("Summary of FKTP Non Kapitasi 1920:")
print(summary(fktp_non_kapitasi_1920))

# Combine the datasets
combined_fktp_non_kapitasi <- bind_rows(
  fktp_non_kapitasi_1516,
  fktp_non_kapitasi_1718,
  fktp_non_kapitasi_1920
)

# Normalize diagnosis codes to be 3 characters in length
combined_fktp_non_kapitasi <- combined_fktp_non_kapitasi %>%
  mutate(diagnosis_code = sapply(diagnosis_code, normalize_diagnosis_code))

# Check combined data structure
print("Combined data FKTP Non Kapitasi:")
print(str(combined_fktp_non_kapitasi))

# Check types of each column
print("Column types in combined FKTP Non Kapitasi:")
print(sapply(combined_fktp_non_kapitasi, class))

# Print summary of combined data to verify
print("Summary of Combined FKTP Non Kapitasi:")
print(summary(combined_fktp_non_kapitasi))

# Check for missing timestamps and handle them if necessary
combined_fktp_non_kapitasi <- combined_fktp_non_kapitasi %>%
  filter(!is.na(visit_date) & !is.na(discharge_date))

# Save the combined dataset
write_csv(combined_fktp_non_kapitasi, "combined_fktp_non_kapitasi.csv")

# Calculate the sum of instances for each primary diagnosis code
diagnosis_code_summary <- combined_fktp_non_kapitasi %>%
  group_by(diagnosis_code) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  slice_head(n = 10)

# Print the summary of diagnosis codes
print("Top 10 diagnosis codes:")
print(diagnosis_code_summary)

# Find the rank of CKD in the diagnosis summary
ckd_rank <- combined_fktp_non_kapitasi %>%
  group_by(diagnosis_code) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  mutate(rank = row_number()) %>%
  filter(diagnosis_code == "CKD") %>%
  select(diagnosis_code, count, rank)

print("Rank of CKD in the diagnosis summary:")
print(ckd_rank)

# Check for CKD count in combined data
ckd_combined_count <- sum(combined_fktp_non_kapitasi$diagnosis_code == "CKD")
print(paste("CKD count in combined data:", ckd_combined_count))
