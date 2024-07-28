# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(haven)

# Function to load and preprocess FKRTL data
preprocess_fkrtl <- function(file_path, year) {
  if (year == "1516") {
    data <- read.delim(file_path, sep = "|", header = TRUE) %>%
      select(PSTV01, FKL17) %>%
      rename(case_id = PSTV01, diagnosis_code = FKL17) %>%
      mutate(year = year)
  } else {
    data <- read_dta(file_path) %>%
      select(PSTV01, FKL16) %>%
      rename(case_id = PSTV01, diagnosis_code = FKL16) %>%
      mutate(year = year)
  }
  return(data)
}

# Load FKRTL data
fkrtl_1516 <- preprocess_fkrtl("C:/Users/LENOVO/Documents/dataset/BPJS1516/04KunjunganFKRTL.csv", "1516")
fkrtl_1718 <- preprocess_fkrtl("C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/03_fkrtl.dta", "1718")
fkrtl_1920 <- preprocess_fkrtl("C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202003_fkrtl.dta", "1920")

# Combine datasets
combined_fkrtl <- bind_rows(fkrtl_1516, fkrtl_1718, fkrtl_1920)

# Calculate ICD code lengths
combined_fkrtl <- combined_fkrtl %>%
  mutate(code_length = nchar(diagnosis_code))

# Summary statistics of ICD code lengths
summary_stats <- combined_fkrtl %>%
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
ggplot(combined_fkrtl, aes(x = code_length, fill = year)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Distribution of ICD Code Lengths by Year", x = "ICD Code Length", y = "Count") +
  theme_minimal()

# Plot boxplot of ICD code lengths by year
ggplot(combined_fkrtl, aes(x = year, y = code_length, fill = year)) +
  geom_boxplot() +
  labs(title = "Boxplot of ICD Code Lengths by Year", x = "Year", y = "ICD Code Length") +
  theme_minimal()

# Sample 10 ICD codes from each year
sample_1516 <- combined_fkrtl %>% filter(year == "1516") %>% sample_n(5)
sample_1718 <- combined_fkrtl %>% filter(year == "1718") %>% sample_n(5)
sample_1920 <- combined_fkrtl %>% filter(year == "1920") %>% sample_n(5)

# Combine the samples
sample_icd_codes <- bind_rows(sample_1516, sample_1718, sample_1920)

print("Sample ICD codes from each year:")
print(sample_icd_codes)
