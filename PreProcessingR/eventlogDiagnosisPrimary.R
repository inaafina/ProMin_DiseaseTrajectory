# Load necessary libraries
library(dplyr)
library(readr)
library(lubridate)
library(bupaR)  # For process mining
library(eventdataR)  # For process mining

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

# Load the labellingRaw dataset
labelling_raw <- read_csv("labellingRaw.csv", col_types = cols(
  diagnosis_code = col_character(),
  diagnosis_category = col_character()
))

# Combine FKTP Non-Kapitasi and FKRTL datasets
event_log_1 <- bind_rows(combined_fktp, combined_fkrtl)

# Merge with labellingRaw to add diagnosis_category
event_log_1 <- event_log_1 %>%
  left_join(labelling_raw, by = "diagnosis_code")

# Identify case IDs with CKD
ckd_case_ids_1 <- event_log_1 %>%
  filter(diagnosis_code == "CKD") %>%
  pull(case_id) %>%
  unique()

# Filter rows for cases connected with CKD and include all related events
ckd_related_cases_1 <- event_log_1 %>%
  filter(case_id %in% ckd_case_ids_1)

# Sort the dataset by case_id, visit_date, and event_type
ckd_related_cases_1 <- ckd_related_cases_1 %>%
  arrange(case_id, visit_date, event_type)

# Add activity_instance_id and resource_id
ckd_related_cases_1 <- ckd_related_cases_1 %>%
  group_by(case_id) %>%
  mutate(activity_instance_id = paste0(case_id, "_", row_number()),
         resource_id = "Admin") %>%
  ungroup()

# Save the final event log
write_csv(ckd_related_cases_1, "ckd_related_event_log_1.csv")

# Print the final event log to verify
print(head(ckd_related_cases_1))

# Check the top 5 diseases in each event type
top_diseases <- ckd_related_cases %>%
  group_by(event_type, diagnosis_category) %>%
  summarise(count = n()) %>%
  arrange(event_type, desc(count)) %>%
  group_by(event_type) %>%
  slice_head(n = 5)

print(top_diseases)

# Find the total number of rows in the combined_data dataset
total_rows <- nrow(combined_data)
print(paste("Total number of rows:", total_rows))

