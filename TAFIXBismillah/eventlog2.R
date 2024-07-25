# Load necessary libraries
library(dplyr)
library(readr)
library(lubridate)

# Load the datasets
combined_fktp <- read_csv("combined_fktp_non_kapitasi.csv", col_types = cols(
  case_id = col_character(),
  visit_date = col_datetime(format = ""),
  discharge_date = col_datetime(format = ""),
  diagnosis_code = col_character(),
  diagnosis_name = col_character(),
  event_type = col_character()
))

combined_fkrtl <- read_csv("combined_fkrtl.csv", col_types = cols(
  case_id = col_character(),
  visit_date = col_datetime(format = ""),
  discharge_date = col_datetime(format = ""),
  diagnosis_code = col_character(),
  diagnosis_name = col_character(),
  event_type = col_character()
))

combined_diagnosis_sekunder <- read_csv("combined_diagnosis_sekunder_with_timestamps.csv", col_types = cols(
  case_id = col_character(),
  visit_date = col_datetime(format = ""),
  discharge_date = col_datetime(format = ""),
  diagnosis_code = col_character(),
  diagnosis_name = col_character(),
  event_type = col_character()
))

# Combine all datasets
combined_data <- bind_rows(combined_fktp, combined_fkrtl, combined_diagnosis_sekunder)

# Identify case IDs connected with CKD
ckd_case_ids <- combined_data %>%
  filter(diagnosis_code == "CKD") %>%
  pull(case_id) %>%
  unique()

# Filter rows for cases connected with CKD
combined_data <- combined_data %>%
  filter(case_id %in% ckd_case_ids)

# Count the total number of rows
total_rows <- nrow(combined_data)
print(paste("Total number of rows connected with CKD:", total_rows))

# Sort the dataset by case_id, visit_date, and event_type
combined_data <- combined_data %>%
  arrange(case_id, visit_date, event_type)

# Add activity_instance_id and resource_id
combined_data <- combined_data %>%
  group_by(case_id) %>%
  mutate(activity_instance_id = paste0(case_id, "_", row_number()),
         resource_id = "Admin") %>%
  ungroup()

# Save the final event log without START and END events
write_csv(combined_data, "event_log.csv")

# Print the final event log to verify
print(head(combined_data))

# Check the top 5 diseases in each event type
top_diseases <- combined_data %>%
  group_by(event_type, diagnosis_code) %>%
  summarise(count = n()) %>%
  arrange(event_type, desc(count)) %>%
  group_by(event_type) %>%
  slice_head(n = 5)

print(top_diseases)
