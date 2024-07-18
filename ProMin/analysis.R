# Load required libraries
library(bupaR)
library(edeaR)
library(processmapR)
library(dplyr)
library(lubridate)

# Load your event_log dataset
event_log <- read.csv('event_log.csv')

# Convert event_log to eventlog object
event_log <- event_log %>%
  eventlog(
    case_id = "case_id",
    activity_id = "diagnosis_code",
    activity_instance_id = "activity_instance_id",
    timestamp = "visit_date",
    lifecycle_id = "event_type",
    resource_id = "resource_id"
  )

# Analysis Section

# 1. Basic Statistics
print("Basic Statistics:")
print(summary(event_log))

# 2. Diagnosis Code Analysis
print("Diagnosis Code Analysis:")
diagnosis_analysis <- event_log %>% 
  activity_frequency("absolute") %>%
  arrange(desc(absolute_frequency))
print(head(diagnosis_analysis, 10))  # Top 10 most frequent diagnosis codes

# 3. Event Type Analysis
print("Event Type Analysis:")
event_type_analysis <- event_log %>%
  group_by(lifecycle_id) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
print(event_type_analysis)

# 4. Trajectory Analysis
print("Trajectory Analysis:")
trajectory_analysis <- event_log %>%
  trace_explorer(coverage = 0.8)  # Covers 80% of cases
print(head(trajectory_analysis, 10))  # Top 10 most common trajectories

# 5. Process Map
print("Generating Process Map...")
process_map <- event_log %>%
  process_map(type = frequency("absolute"), render = FALSE)
print(process_map)

# 6. Time-based Analysis
print("Time-based Analysis:")
event_log$timestamp <- ymd_hms(event_log$timestamp)  # Ensure timestamp is in correct format
time_analysis <- event_log %>%
  time_series_metrics(metrics = c("number_of_events", "number_of_cases"), time_unit = "months")
print(head(time_analysis, 10))  # First 10 time periods

# 7. Transition Analysis
print("Transition Analysis:")
transition_matrix <- event_log %>%
  precedence_matrix(type = "relative")
print(head(transition_matrix, 10))  # Top 10 transitions

# 8. Resource Analysis
print("Resource Analysis:")
resource_analysis <- event_log %>%
  resource_involvement("resource") %>%
  arrange(desc(resources))
print(head(resource_analysis, 10))  # Top 10 most involved resources

# 9. Case Duration Analysis
print("Case Duration Analysis:")
case_duration <- event_log %>%
  case_summary() %>%
  arrange(desc(throughput_time))
print(head(case_duration, 10))  # Top 10 longest cases

# 10. Event Type Sequence Analysis
print("Event Type Sequence Analysis:")
event_type_sequence <- event_log %>%
  trace_explorer(coverage = 0.8, type = "lifecycle_id")
print(head(event_type_sequence, 10))  # Top 10 most common event type sequences

# Output key findings
cat("\nKey Findings:\n")
cat("1. Total number of events:", nrow(event_log), "\n")
cat("2. Most frequent diagnosis code:", diagnosis_analysis$activity[1], "\n")
cat("3. Most common event type:", event_type_analysis$lifecycle_id[1], "\n")
cat("4. Number of unique trajectories:", nrow(trajectory_analysis), "\n")
cat("5. Average case duration:", mean(case_duration$throughput_time), "time units\n")
cat("6. Most common transition:", transition_matrix$antecedent[1], "to", transition_matrix$consequent[1], "\n")