# Load necessary libraries
library(dplyr)
library(stringr)
library(readr)
library(haven)

# Define patterns for the chapters to exclude
exclude_chapters <- c(
  "^O[0-9]", "^P[0-9]", "^R[0-9]", "^S[0-9]", "^T[0-9]", "^V[0-9]", "^W[0-9]", "^X[0-9]", "^Y[0-9]", "^Z[0-9]"
)

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
    filter(str_length(diagnosis_code) == 3 & !str_detect(diagnosis_code, paste(exclude_chapters, collapse = "|"))) %>%
    mutate(
      visit_date = as.POSIXct(visit_date, format = "%d%b%Y", tz = "UTC"),
      discharge_date = as.POSIXct(discharge_date, format = "%d%b%Y", tz = "UTC")
    )
  
  # Remove duplicates
  filtered_data <- filtered_data %>%
    distinct(case_id, visit_date, discharge_date, diagnosis_code, .keep_all = TRUE)
  
  total_rows_after <- nrow(filtered_data)
  print(paste("Total rows after filtering in FKTP Non Kapitasi 1516:", total_rows_after))
  
  # Check for N18 after filtering
  n18_after_filter <- sum(filtered_data$diagnosis_code == "N18")
  print(paste("N18 count after filtering in FKTP Non Kapitasi 1516:", n18_after_filter))
  
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
  
  # Check for N18 before filtering
  n18_before_filter <- sum(data$PNK13A == "N18")
  print(paste("N18 count before filtering in FKTP Non Kapitasi 1718/1920:", n18_before_filter))
  
  filtered_data <- data %>%
    select(PSTV01, PNK03, PNK05, PNK13A, PNK15) %>%
    rename(case_id = PSTV01, visit_date = PNK03, discharge_date = PNK05, diagnosis_code = PNK13A, diagnosis_name = PNK15) %>%
    mutate(event_type = "FKTP Non-Kapitasi") %>%
    filter(str_length(diagnosis_code) == 3 & !str_detect(diagnosis_code, paste(exclude_chapters, collapse = "|"))) %>%
    mutate(
      visit_date = as.POSIXct(visit_date, format = "%Y-%m-%d", tz = "UTC"),
      discharge_date = as.POSIXct(discharge_date, format = "%Y-%m-%d", tz = "UTC")
    )
  
  # Remove duplicates
  filtered_data <- filtered_data %>%
    distinct(case_id, visit_date, discharge_date, diagnosis_code, .keep_all = TRUE)
  
  total_rows_after <- nrow(filtered_data)
  print(paste("Total rows after filtering in FKTP Non Kapitasi 1718/1920:", total_rows_after))
  
  # Check for N18 after filtering
  n18_after_filter <- sum(filtered_data$diagnosis_code == "N18")
  print(paste("N18 count after filtering in FKTP Non Kapitasi 1718/1920:", n18_after_filter))
  
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

# Find the rank of N18 in the diagnosis summary
n18_rank <- combined_fktp_non_kapitasi %>%
  group_by(diagnosis_code) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  mutate(rank = row_number()) %>%
  filter(diagnosis_code == "N18") %>%
  select(diagnosis_code, count, rank)

print("Rank of N18 in the diagnosis summary:")
print(n18_rank)

# Check for N18 count in combined data
n18_combined_count <- sum(combined_fktp_non_kapitasi$diagnosis_code == "N18")
print(paste("N18 count in combined data:", n18_combined_count))
