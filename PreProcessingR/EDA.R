# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(haven)

# Function to load and preprocess data
load_and_preprocess <- function(file_path, year) {
  if (year == "1516") {
    data <- read.delim(file_path, sep = "|", header = TRUE) %>%
      select(PSTV01, PNK13) %>%
      rename(case_id = PSTV01, diagnosis_code = PNK13) %>%
      mutate(year = year)
  } else {
    data <- read_dta(file_path) %>%
      select(PSTV01, PNK14) %>%
      rename(case_id = PSTV01, diagnosis_code = PNK14) %>%
      mutate(year = year)
  }
  return(data)
}

# Load data
fktp_non_kapitasi_1516 <- load_and_preprocess("C:/Users/LENOVO/Documents/dataset/BPJS1516/03KunjunganFKTPnonkapitasi.csv", "1516")
fktp_non_kapitasi_1718 <- load_and_preprocess("C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/04_nonkapitasi.dta", "1718")
fktp_non_kapitasi_1920 <- load_and_preprocess("C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202004_nonkapitasi.dta", "1920")

# Combine datasets
combined_data <- bind_rows(fktp_non_kapitasi_1516, fktp_non_kapitasi_1718, fktp_non_kapitasi_1920)

# Calculate ICD code lengths
combined_data <- combined_data %>%
  mutate(code_length = nchar(diagnosis_code))

# Summary statistics of ICD code lengths
summary_stats <- combined_data %>%
  group_by(year) %>%
  summarise(
    min_length = min(code_length, na.rm = TRUE),
    max_length = max(code_length, na.rm = TRUE),
    mean_length = mean(code_length, na.rm = TRUE),
    median_length = median(code_length, na.rm = TRUE),
    count = n()
  )

print(summary_stats)

# Plot distribution of ICD code lengths
ggplot(combined_data, aes(x = code_length, fill = year)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Distribution of ICD Code Lengths by Year", x = "ICD Code Length", y = "Count") +
  theme_minimal()

# Plot boxplot of ICD code lengths by year
ggplot(combined_data, aes(x = year, y = code_length, fill = year)) +
  geom_boxplot() +
  labs(title = "Boxplot of ICD Code Lengths by Year", x = "Year", y = "ICD Code Length") +
  theme_minimal()

# Sample 10 ICD codes from each year
sample_1516 <- combined_data %>% filter(year == "1516") %>% sample_n(5)
sample_1718 <- combined_data %>% filter(year == "1718") %>% sample_n(5)
sample_1920 <- combined_data %>% filter(year == "1920") %>% sample_n(5)

# Combine the samples
sample_icd_codes <- bind_rows(sample_1516, sample_1718, sample_1920)

print("Sample ICD codes from each year:")
print(sample_icd_codes)
