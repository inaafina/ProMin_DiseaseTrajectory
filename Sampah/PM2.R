# Install packages
#install.packages("remotes")
#remotes::install_github("gertjanssenswillen/bupaR")
#remotes::install_github("gertjanssenswillen/heuristicsmineR")

# Load libraries
library(bupaR)
library(heuristicsmineR)
library(dplyr)
library(ggplot2)
library(eventdataR)
library(haven) # for read_dta function
library(readr) # for read_csv function
library(processmapR)
library(edeaR)
library(stringr) # for str_detect function

# Define CKD-related diagnosis codes (specifically "N18" for CKD)
ckd_code <- "N18"

# Read the data
fktp_non_kapitasi_1516 <- read_csv("C:/Users/LENOVO/Documents/dataset/BPJS1516/03KunjunganFKTPnonkapitasi.csv")
fkrtl_1516 <- read_csv("C:/Users/LENOVO/Documents/dataset/BPJS1516/04KunjunganFKRTL.csv")

fktp_non_kapitasi_1718 <- read_dta("C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/04_nonkapitasi.dta")
fkrtl_1718 <- read_dta("C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/03_fkrtl.dta")
diagnosis_sekunder_1718 <- read_dta("C:/Users/LENOVO/Documents/dataset/BPJS1718/Stata(.dta)/05_diagnosissekunder.dta")

fktp_non_kapitasi_1920 <- read_dta("C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202004_nonkapitasi.dta")
fkrtl_1920 <- read_dta("C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202003_fkrtl.dta")
diagnosis_sekunder_1920 <- read_dta("C:/Users/LENOVO/Documents/dataset/BPJS1920/Reguler/2019202005_diagnosissekunder.dta")

# Filter for CKD-related records in FKTP Non-Kapitation datasets
fktp_non_kapitasi_1516 <- fktp_non_kapitasi_1516 %>%
  filter(str_detect(PNK13, paste(ckd_codes_1516, collapse = "|"))) %>%
  select(PSTV01, PNK03, PNK05, PNK13, PNK14, PNK16) %>%
  rename(start_date = PNK03, complete_date = PNK05, diagnosis_code = PNK13, diagnosis_name = PNK14, treatment_name = PNK16) %>%
  mutate(event_type = "FKTP Non-Kapitasi")

fktp_non_kapitasi_1718 <- fktp_non_kapitasi_1718 %>%
  filter(str_detect(PNK13, ckd_code)) %>%
  select(PSTV01, PNK03, PNK05, PNK13, PNK14, PNK16) %>%
  rename(start_date = PNK03, complete_date = PNK05, diagnosis_code = PNK13, diagnosis_name = PNK14, treatment_name = PNK16) %>%
  mutate(event_type = "FKTP Non-Kapitasi")

fktp_non_kapitasi_1920 <- fktp_non_kapitasi_1920 %>%
  filter(str_detect(PNK13, ckd_code)) %>%
  select(PSTV01, PNK03, PNK05, PNK13, PNK14, PNK16) %>%
  rename(start_date = PNK03, complete_date = PNK05, diagnosis_code = PNK13, diagnosis_name = PNK14, treatment_name = PNK16) %>%
  mutate(event_type = "FKTP Non-Kapitasi")

# Filter for CKD-related records in FKRTL datasets
fkrtl_1516 <- fkrtl_1516 %>%
  filter(str_detect(FKL18, paste(ckd_codes_1516, collapse = "|"))) %>%
  select(PSTV01, FKL03, FKL04, FKL15, FKL16, FKL22, FKL28) %>%
  rename(start_date = FKL03, complete_date = FKL04, diagnosis_code = FKL15, diagnosis_name = FKL16, diagnosis_treatment_name = FKL22, health_facility_type = FKL28) %>%
  mutate(event_type = "FKRTL")

fkrtl_1718 <- fkrtl_1718 %>%
  filter(str_detect(FKL18, ckd_code)) %>%
  select(PSTV01, FKL03, FKL04, FKL15A, FKL18A) %>%
  rename(start_date = FKL03, complete_date = FKL04, diagnosis_code = FKL15A, diagnosis_name = FKL18A) %>%
  mutate(event_type = "FKRTL")

fkrtl_1920 <- fkrtl_1920 %>%
  filter(str_detect(FKL18, ckd_code)) %>%
  select(PSTV01, FKL03, FKL04, FKL15A, FKL18A) %>%
  rename(start_date = FKL03, complete_date = FKL04, diagnosis_code = FKL15A, diagnosis_name = FKL18A) %>%
  mutate(event_type = "FKRTL")

# Filter for CKD-related records in Diagnosis Sekunder datasets
diagnosis_sekunder_1718 <- diagnosis_sekunder_1718 %>%
  filter(str_detect(FKL24A, ckd_code)) %>%
  select(PSTV01, FKL24A) %>%
  rename(diagnosis_code = FKL24A) %>%
  mutate(event_type = "Diagnosis Sekunder")

diagnosis_sekunder_1920 <- diagnosis_sekunder_1920 %>%
  filter(str_detect(FKL24A, ckd_code)) %>%
  select(PSTV01, FKL24A) %>%
  rename(diagnosis_code = FKL24A) %>%
  mutate(event_type = "Diagnosis Sekunder")

# Combine the datasets
combined_1516 <- bind_rows(
  fktp_non_kapitasi_1516 %>% select(PSTV01, timestamp = start_date, diagnosis_code, event_type),
  fktp_non_kapitasi_1516 %>% select(PSTV01, timestamp = complete_date, diagnosis_code, event_type),
  fkrtl_1516 %>% select(PSTV01, timestamp = start_date, diagnosis_code, event_type),
  fkrtl_1516 %>% select(PSTV01, timestamp = complete_date, diagnosis_code, event_type)
)

combined_1718 <- bind_rows(
  fktp_non_kapitasi_1718 %>% select(PSTV01, timestamp = start_date, diagnosis_code, event_type),
  fktp_non_kapitasi_1718 %>% select(PSTV01, timestamp = complete_date, diagnosis_code, event_type),
  fkrtl_1718 %>% select(PSTV01, timestamp = start_date, diagnosis_code, event_type),
  fkrtl_1718 %>% select(PSTV01, timestamp = complete_date, diagnosis_code, event_type),
  diagnosis_sekunder_1718 %>% mutate(timestamp = as.Date(NA)) %>% filter(!is.na(timestamp))
)

combined_1920 <- bind_rows(
  fktp_non_kapitasi_1920 %>% select(PSTV01, timestamp = start_date, diagnosis_code, event_type),
  fktp_non_kapitasi_1920 %>% select(PSTV01, timestamp = complete_date, diagnosis_code, event_type),
  fkrtl_1920 %>% select(PSTV01, timestamp = start_date, diagnosis_code, event_type),
  fkrtl_1920 %>% select(PSTV01, timestamp = complete_date, diagnosis_code, event_type),
  diagnosis_sekunder_1920 %>% mutate(timestamp = as.Date(NA)) %>% filter(!is.na(timestamp))
)

# Combine all periods into one dataset
combined_data <- bind_rows(combined_1516, combined_1718, combined_1920)

# Drop rows with missing values in essential columns
#cleaned_data <- combined_data %>%
#  filter(!is.na(PSTV01) & !is.na(diagnosis_code) & !is.na(timestamp))

# Convert the dataset into an event log
event_log <- cleaned_data %>%
  eventlog(
    case_id = "PSTV01",         # Patient ID
    activity_id = "diagnosis_code", # Activity is the diagnosis code
    timestamp = "timestamp",   # Timestamp of the diagnosis or visit
    lifecycle_id = "event_type"  # Type of event (FKTP Non-Kapitasi, FKRTL, or Diagnosis Sekunder)
  )

# Summary of the event log
summary(event_log)

# Apply heuristic mining
heuristic_model <- heuristicsmineR::heuristic_miner(event_log)

# Visualize the heuristic model
heuristicsmineR::render_PN(heuristic_model)

# Perform conformance checking
conformance <- conformance(event_log, heuristic_model)
print(conformance)

# Create a process map
process_map <- process_map(event_log)

# Plot the process map
plot(process_map)
