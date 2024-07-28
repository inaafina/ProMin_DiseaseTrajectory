# Combine FKRTL and Diagnosis Sekunder datasets
event_log_2 <- bind_rows(combined_fkrtl, combined_diagnosis_sekunder)

# Merge with labellingRaw to add diagnosis_category
event_log_2 <- event_log_2 %>%
  left_join(labelling_raw, by = "diagnosis_code")

# Identify case IDs with CKD
ckd_case_ids_2 <- event_log_2 %>%
  filter(diagnosis_code == "CKD") %>%
  pull(case_id) %>%
  unique()

# Filter rows for cases connected with CKD and include all related events
ckd_related_cases_2 <- event_log_2 %>%
  filter(case_id %in% ckd_case_ids_2)

# Sort the dataset by case_id, visit_date, and event_type
ckd_related_cases_2 <- ckd_related_cases_2 %>%
  arrange(case_id, visit_date, event_type)

# Add activity_instance_id and resource_id
ckd_related_cases_2 <- ckd_related_cases_2 %>%
  group_by(case_id) %>%
  mutate(activity_instance_id = paste0(case_id, "_", row_number()),
         resource_id = "Admin") %>%
  ungroup()

# Save the final event log
write_csv(ckd_related_cases_2, "ckd_related_event_log_2.csv")

# Print the final event log to verify
print(head(ckd_related_cases_2))
