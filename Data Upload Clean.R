
library(tidyverse)
library(readxl)
library(writexl)

# ============================================================
# 1. LOAD DATA
# ============================================================

# Read Excel dataset containing arthroplasty outcomes
Arthroplasty_Data <- read_excel(path = "Project 4 data.xlsx")


# ============================================================
# 2. DATA CLEANING AND RECODING
# ============================================================

# Rename key variables, create patient ID, and reorder columns
Arthroplasty_Data <- Arthroplasty_Data %>%
  rename(
    Age = age,
    Gender = gender,
    Side = side,
    Complications = complication
  ) %>%
  mutate(PatientID = row_number()) %>%
  relocate(PatientID, .before = Age)

# Recode gender variable into readable labels
Arthroplasty_Data$Gender <- factor(Arthroplasty_Data$Gender) %>%
  fct_recode("Male" = "1", "Female" = "2")

# Recode surgical side variable into labels
Arthroplasty_Data$Side <- factor(Arthroplasty_Data$Side) %>%
  fct_recode("Right" = "1", "Left" = "2")

# Standardize column names: replace "pre" with "Preoperative"
Arthroplasty_Data <- Arthroplasty_Data %>%
  rename_with(~ gsub("pre", "Preoperative", .x))

# Rename specific outcome variables for consistency
Arthroplasty_Data <- Arthroplasty_Data %>%
  rename(
    "Pelvic_Obliquity Postoperative" = `Pelvic obliquity  post`,
    "Pelvic_Obliquity Preoperative" = `Pelvic obliquity Preoperative`,
    "LLD Postoperative" = `LLD post`,
    "FD Postoperative" = `FD post`
  )
