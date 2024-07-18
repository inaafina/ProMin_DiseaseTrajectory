# Load necessary libraries
library(dplyr)
library(readr)
library(bupaR)
library(heuristicsmineR)
library(processmapR)
library(processmonitR)
library(progress)
library(DiagrammeR)

# Load the event log
event_log <- read_csv("event_log_fix.csv")

# Define patterns for the chapters to exclude
exclude_chapters <- c(
  "^O[0-9]", "^P[0-9]", "^R[0-9]", "^S[0-9]", "^T[0-9]", "^V[0-9]", "^W[0-9]", "^X[0-9]", "^Y[0-9]", "^Z[0-9]"
)

# Check if the event log contains any diagnosis codes that match the exclude patterns
contains_excluded_chapters <- event_log %>%
  filter(str_detect(diagnosis_code, paste(exclude_chapters, collapse = "|")))

# Print the result
if (nrow(contains_excluded_chapters) > 0) {
  print("The event log contains the following diagnosis codes that match the exclude patterns:")
  print(contains_excluded_chapters)
} else {
  print("The event log does not contain any diagnosis codes that match the exclude patterns.")
}

# Print the distinct event types
distinct_event_types <- unique(event_log$event_type)
print("Distinct event types:")
print(distinct_event_types)

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

# Print the structure of the event log to verify
print(str(event_log))

# Print number of rows and distinct event types
num_rows <- nrow(event_log)
print(paste("Number of rows in the dataset:", num_rows))

num_distinct_strings <- length(unique(event_log$event_type))
print(paste("Number of distinct event types:", num_distinct_strings))

# Print the distinct event types
distinct_event_types <- unique(event_log$event_type)
print("Distinct event types:")
print(distinct_event_types)

# Get the top 5 diagnosis codes from each event type
top_diagnosis_codes <- event_log %>%
  group_by(event_type, diagnosis_code) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(event_type, desc(count)) %>%
  group_by(event_type) %>%
  slice_head(n = 5) %>%
  ungroup()

# Print the summary of top 5 diagnosis codes from each event type
print("Top 5 diagnosis codes from each event type:")
print(top_diagnosis_codes)

# Calculate the sum of instances for each diagnosis code and find the rank of N18
diagnosis_code_summary <- event_log %>%
  group_by(event_type, diagnosis_code) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(event_type, desc(count)) %>%
  mutate(rank = row_number())

# Find the rank of N18 in the diagnosis summary
n18_rank <- diagnosis_code_summary %>%
  filter(diagnosis_code == "N18") %>%
  select(event_type, diagnosis_code, count, rank)

print("Rank of N18 in the diagnosis summary for each event type:")
print(n18_rank)
