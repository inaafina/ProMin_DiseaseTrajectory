# Load necessary libraries
library(dplyr)
library(stringr)
library(readr)
library(haven)

# Function to preprocess FKTP Non Kapitasi data for 1516
preprocess_fktp_non_kapitasi_1516 <- function(file_path) {
  data <- read.delim(file_path, sep = "|", header = TRUE) # Use | as delimiter
  print("Column names and types for FKTP Non Kapitasi 1516:")
  print(str(data)) # Print structure of the data
  filtered_data <- data %>%
    select(PSTV01, PNK02, PNK03, PNK05, PNK13, PNK14) %>%
    rename(visit_id = PNK02, visit_date = PNK03, discharge_date = PNK05, diagnosis_code = PNK13, diagnosis_name = PNK14) %>%
    mutate(event_type = "FKTP Non-Kapitasi") %>%
    mutate(
      visit_date = as.Date(visit_date, format = "%d%b%Y"),
      discharge_date = as.Date(discharge_date, format = "%d%b%Y")
    )
  print("Filtered data FKTP Non Kapitasi 1516:")
  print(str(filtered_data)) # Print structure of the filtered data
  return(filtered_data)
}

# Function to preprocess FKTP Non Kapitasi data for 1718 and 1920
preprocess_fktp_non_kapitasi_1718_1920 <- function(file_path) {
  data <- read_dta(file_path)
  print("Column names and types for FKTP Non Kapitasi 1718 and 1920:")
  print(str(data)) # Print structure of the data
  filtered_data <- data %>%
    select(PSTV01, PNK02, PNK03, PNK05, PNK13A, PNK14) %>%
    rename(visit_id = PNK02, visit_date = PNK03, discharge_date = PNK05, diagnosis_code = PNK13A, diagnosis_name = PNK14) %>%
    mutate(event_type = "FKTP Non-Kapitasi") %>%
    mutate(
      visit_date = as.Date(visit_date, format = "%Y-%m-%d"),  # Assuming the date format is correct here
      discharge_date = as.Date(discharge_date, format = "%Y-%m-%d")
    )
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

#check combined data structure
print("Combined data FKTP Non ")
print(combined_fktp_non_kapitasi)

# Print summary of combined data to verify
print("Summary of Combined FKTP Non Kapitasi:")
print(summary(combined_fktp_non_kapitasi))

# Check for missing timestamps and handle them if necessary
combined_fktp_non_kapitasi <- combined_fktp_non_kapitasi %>%
  filter(!is.na(visit_date) & !is.na(discharge_date))

# Save the combined dataset
write_csv(combined_fktp_non_kapitasi, "combined_fktp_non_kapitasi.csv")
