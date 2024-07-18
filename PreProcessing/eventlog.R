# Load necessary libraries
library(dplyr)
library(readr)
library(bupaR)
library(lubridate)

# Load the preprocessed combined datasets
combined_fktp_non_kapitasi <- read_csv("combined_fktp_non_kapitasi.csv", col_types = cols(
  case_id = col_character(),
  visit_date = col_date(),
  discharge_date = col_date(),
  diagnosis_code = col_character(),
  diagnosis_name = col_character(),
  event_type = col_character()
))

# Print summary of combined_fktp_non_kapitasi
print("Summary of combined_fktp_non_kapitasi:")
print(summary(combined_fktp_non_kapitasi))

# Print distinct event types of combined_fktp_non_kapitasi
distinct_event_types_fktp <- unique(combined_fktp_non_kapitasi$event_type)
print("Distinct event types in combined_fktp_non_kapitasi:")
print(distinct_event_types_fktp)

combined_fkrtl <- read_csv("combined_fkrtl_out.csv", col_types = cols(
  case_id = col_character(),
  visit_date = col_date(),
  discharge_date = col_date(),
  diagnosis_code = col_character(),
  diagnosis_name = col_character(),
  event_type = col_character()
))

# Print summary of combined_fkrtl
print("Summary of combined_fkrtl:")
print(summary(combined_fkrtl_out))

# Print distinct event types of combined_fkrtl
distinct_event_types_fkrtl <- unique(combined_fkrtl_out$event_type)
print("Distinct event types in combined_fkrtl:")
print(distinct_event_types_fkrtl)

combined_diagnosis_sekunder_with_timestamps <- read_csv("combined_diagnosis_sekunder_with_timestamps.csv", col_types = cols(
  case_id = col_character(),
  visit_date = col_datetime(format = ""),
  discharge_date = col_datetime(format = ""),
  diagnosis_code = col_character(),
  diagnosis_name = col_character(),
  event_type = col_character()
))

# Print summary of combined_diagnosis_sekunder_with_timestamps
print("Summary of combined_diagnosis_sekunder_with_timestamps:")
print(summary(combined_diagnosis_sekunder_with_timestamps))

# Print distinct event types of combined_diagnosis_sekunder_with_timestamps
distinct_event_types_diagnosis_sekunder <- unique(combined_diagnosis_sekunder_with_timestamps$event_type)
print("Distinct event types in combined_diagnosis_sekunder_with_timestamps:")
print(distinct_event_types_diagnosis_sekunder)

# Add a default time to date columns in FKTP and FKRTL datasets
combined_fktp_non_kapitasi <- combined_fktp_non_kapitasi %>%
  mutate(visit_date = ymd_hms(paste(visit_date, "00:00:00")),
         discharge_date = ymd_hms(paste(discharge_date, "00:00:00")))

combined_fkrtl <- combined_fkrtl %>%
  mutate(visit_date = ymd_hms(paste(visit_date, "00:00:00")),
         discharge_date = ymd_hms(paste(discharge_date, "00:00:00")))

# Combine all datasets into one event log
combined_data <- bind_rows(
  combined_fkrtl %>% select(case_id, visit_date, discharge_date, diagnosis_code, diagnosis_name, event_type),
  combined_fktp_non_kapitasi %>% select(case_id, visit_date, discharge_date, diagnosis_code, diagnosis_name, event_type),
  combined_diagnosis_sekunder_with_timestamps %>% select(case_id, visit_date, discharge_date, diagnosis_code, diagnosis_name, event_type)
) %>%
  mutate(activity_instance_id = row_number(), resource_id = "unknown")

# Ensure all timestamps are in the correct format and convert to POSIXct
combined_data <- combined_data %>%
  mutate(
    visit_date = as.POSIXct(visit_date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    discharge_date = as.POSIXct(discharge_date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

# Verify the column types
print("Column types after ensuring POSIXct format:")
print(sapply(combined_data, class))

# Check for any remaining character timestamps and convert if necessary
if(is.character(combined_data$visit_date)) {
  print("Warning: visit_date is still in character format. Attempting alternative conversion.")
  combined_data$visit_date <- parse_date_time(combined_data$visit_date, orders = c("ymd HMS", "ymd"))
}

if(is.character(combined_data$discharge_date)) {
  print("Warning: discharge_date is still in character format. Attempting alternative conversion.")
  combined_data$discharge_date <- parse_date_time(combined_data$discharge_date, orders = c("ymd HMS", "ymd"))
}

# Check if activity_instance_id exists and is unique
print("Number of rows in combined_data:")
print(nrow(combined_data))
print("Number of unique activity_instance_ids:")
print(length(unique(combined_data$activity_instance_id)))

# Check the first few activity_instance_ids
print("First few activity_instance_ids:")
print(head(combined_data$activity_instance_id))

# Create the event log
event_log <- combined_data %>%
  filter(!is.na(visit_date)) %>%
  eventlog(
    case_id = "case_id",
    activity_id = "diagnosis_code",
    activity_instance_id = "activity_instance_id",
    timestamp = "visit_date",
    lifecycle_id = "event_type",
    resource_id = "resource_id"
  )

# Check if the event log was created successfully
if(is.eventlog(event_log)) {
  print("Event log created successfully!")
  
  # Print the structure of the event log to verify
  print(str(event_log))
  
  # Print number of rows and distinct event types
  num_rows <- nrow(event_log)
  print(paste("Number of rows in the event log:", num_rows))
  
  num_distinct_strings <- length(unique(event_log$event_type))
  print(paste("Number of distinct event types:", num_distinct_strings))
  
  # Print the distinct event types
  distinct_event_types <- unique(event_log$event_type)
  print("Distinct event types:")
  print(distinct_event_types)
  
} else {
  print("Failed to create event log. Checking timestamp format again.")
  print(class(combined_data$visit_date))
  print(head(combined_data$visit_date))
}

# Export the event log to a CSV file
write_csv(as.data.frame(event_log), "event_log.csv")