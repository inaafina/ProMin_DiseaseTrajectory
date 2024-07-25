# Load necessary libraries
library(dplyr)
library(readr)
library(bupaR)
library(heuristicsmineR)
library(processmapR)
library(progress)
library(DiagrammeR)

# Initialize progress bar
pb <- progress_bar$new(
  format = "[:bar] :percent in :elapsed",
  total = 5, clear = FALSE, width = 60
)

# Load the event log
event_log <- read_csv("event_log.csv")
pb$tick()  # Step 1

# Check the column names to ensure they match
print(colnames(event_log))

# Print distinct event types
distinct_event_types <- unique(event_log$event_type)
print("Distinct event types in event_log:")
print(distinct_event_types)
pb$tick()  # Step 2

# Convert event_log to eventlog object
event_log <- event_log %>%
  eventlog(
    case_id = "case_id",
    activity_id = "diagnosis_code",
    timestamp = "visit_date",
    resource_id = "resource_id",
    lifecycle_id = "event_type",
    activity_instance_id = "activity_instance_id"
  )
pb$tick()  # Step 3

# Print 5 random samples from the event log including event type
print("5 random samples from the event log including event type:")
print(event_log %>% select(case_id, diagnosis_code, event_type, visit_date) %>% sample_n(5))

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
pb$tick()  # Step 4

# Apply the Heuristic Miner
heuristic_model <- causal_net(event_log)
pb$tick()  # Step 5

# Inspect the structure of the heuristic_model object
str(heuristic_model)

# Sample a subset of the event log
sampled_event_log <- event_log %>% sample_frac(0.1)  # Adjust the fraction as needed

# Create process map based on the sampled event log and plot it
process_map <- processmapR::process_map(sampled_event_log)
DiagrammeR::render_graph(process_map)

# Analyze the Process Model

## 1. Most Common Pathways in CKD Progression
common_pathways <- trace_explorer(event_log, coverage = 0.8)
print("Most Common Pathways in CKD Progression:")
print(common_pathways)

## 2. Average Durations of Each CKD Stage
durations <- throughput_time(event_log, units = "days")
print("Average Durations of Each CKD Stage:")
print(durations)

## 3. Conformance to Clinical Guidelines
#conformance <- conformance_checking(event_log, heuristic_model)
#print("Conformance to Clinical Guidelines:")
#print(conformance)

# Performance Measurements

## Replay Fitness
fitness <- replay_fitness(event_log, heuristic_model)
print("Replay Fitness:")
print(fitness)

## Simplicity
simplicity <- calculate_simplicity(heuristic_model)
print("Simplicity:")
print(simplicity)

## Generalization
generalization <- calculate_generalization(heuristic_model)
print("Generalization:")
print(generalization)

## Precision
precision <- calculate_precision(event_log, heuristic_model)
print("Precision:")
print(precision)
