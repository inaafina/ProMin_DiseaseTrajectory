# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(haven)

# Function to load and preprocess Diagnosis Sekunder data
preprocess_diagnosis_sekunder <- function(file_path, year) {
  data <- read_dta(file_path) %>%
    select(FKL02, FKL24) %>%
    rename(case_id = FKL02, diagnosis_code = FKL24) %>%
    mutate(year = year)
  return(data)
}

# Load Diagnosis Sekunder data
diagnosis_sekunder_1718 <- preprocess_diagnosis_sekunder("C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/05_diagnosissekunder.dta", "1718")
diagnosis_sekunder_1920 <- preprocess_diagnosis_sekunder("C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202005_diagnosissekunder.dta", "1920")

# Combine datasets
combined_diagnosis_sekunder <- bind_rows(diagnosis_sekunder_1718, diagnosis_sekunder_1920)

# Calculate ICD code lengths
combined_diagnosis_sekunder <- combined_diagnosis_sekunder %>%
  mutate(code_length = nchar(diagnosis_code))

# Summary statistics of ICD code lengths
summary_stats <- combined_diagnosis_sekunder %>%
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
ggplot(combined_diagnosis_sekunder, aes(x = code_length, fill = year)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Distribution of ICD Code Lengths by Year", x = "ICD Code Length", y = "Count") +
  theme_minimal()

# Plot boxplot of ICD code lengths by year
ggplot(combined_diagnosis_sekunder, aes(x = year, y = code_length, fill = year)) +
  geom_boxplot() +
  labs(title = "Boxplot of ICD Code Lengths by Year", x = "Year", y = "ICD Code Length") +
  theme_minimal()

# Sample 10 ICD codes from each year
sample_1718 <- combined_diagnosis_sekunder %>% filter(year == "1718") %>% sample_n(5)
sample_1920 <- combined_diagnosis_sekunder %>% filter(year == "1920") %>% sample_n(5)

# Combine the samples
sample_icd_codes <- bind_rows(sample_1718, sample_1920)

print("Sample ICD codes from each year:")
print(sample_icd_codes)
