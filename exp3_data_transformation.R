# Experiment 3: Data Transformation (R)
# Course: 26AI2003 - Statistical Programming for Clinical Data Analytics
#
# AIM
#   To merge related clinical datasets on a common patient identifier and to create
#   derived variables from the merged dataset, in order to produce an
#   analysis-ready dataset.
#
# DESCRIPTION
#   clinical_demographics.csv (patient details at enrollment) and
#   clinical_vitals_labs.csv (blood pressure, heart rate, weight, BMI, labs) are
#   merged on USUBJID. After checking that the merge did not add or drop patients,
#   four derived variables are created:
#     AGE_GROUP      recode of one column : AGE < 40 "<40", 40-59 "40-59", 60+ "60+"
#     BP_CATEGORY    from two columns     : SYSBP >= 130 or DIABP >= 80 "High";
#                                           else SYSBP >= 120 "Elevated"; else "Normal"
#     DAYS_ON_STUDY  date difference      : cutoff date (2025-12-31) - ENROLL_DATE
#     HIGH_RISK_FLAG composite flag       : "Yes" if BP_CATEGORY is "High" AND
#                                           (AGE >= 60 OR BMI >= 30)
#
# Needs: dplyr, readr  ->  install.packages(c("dplyr", "readr"))
# Keep both CSV files in the working directory (or use setwd("path/to/your/folder")).

library(dplyr)
library(readr)

# ------------------------------------------------ Step 1: Load both datasets
demographics <- read.csv("clinical_demographics.csv")
vitals_labs  <- read.csv("clinical_vitals_labs.csv")
cat("Demographics:", dim(demographics), "\n")
cat("Vitals/labs :", dim(vitals_labs), "\n")

# --------------------------------- Step 2: Check the merge key is unique
cat("\nDemographics: rows =", nrow(demographics),
    ", unique USUBJID =", n_distinct(demographics$USUBJID), "\n")
cat("Vitals/labs : rows =", nrow(vitals_labs),
    ", unique USUBJID =", n_distinct(vitals_labs$USUBJID), "\n")

# ------------------------------------------------ Step 3: Merge on USUBJID
# inner_join keeps only patients found in both datasets.
df <- inner_join(demographics, vitals_labs, by = "USUBJID")

# ------------------------- Step 4: Verify the merge did not add/drop records
cat("\nMerged dimensions:", dim(df), "\n")
cat("Same row count as demographics:", nrow(df) == nrow(demographics), "\n")
cat("Same row count as vitals/labs :", nrow(df) == nrow(vitals_labs), "\n")
cat("In demographics only:",
    nrow(anti_join(demographics, vitals_labs, by = "USUBJID")), "\n")
cat("In vitals/labs only :",
    nrow(anti_join(vitals_labs, demographics, by = "USUBJID")), "\n")

# ------------------------------------- Step 5: Convert ENROLL_DATE to a date
df$ENROLL_DATE <- as.Date(df$ENROLL_DATE)

# ------------------------------- Step 6: Recode - AGE_GROUP (one column)
df <- df %>%
  mutate(AGE_GROUP = case_when(
    AGE < 40  ~ "<40",
    AGE < 60  ~ "40-59",
    TRUE      ~ "60+"
  ))

# -------------------------- Step 7: From two columns - BP_CATEGORY
df <- df %>%
  mutate(BP_CATEGORY = case_when(
    SYSBP >= 130 | DIABP >= 80 ~ "High",
    SYSBP >= 120               ~ "Elevated",
    TRUE                       ~ "Normal"
  ))

# --------------------------- Step 8: Date difference - DAYS_ON_STUDY
cutoff_date <- as.Date("2025-12-31")
df$DAYS_ON_STUDY <- as.numeric(cutoff_date - df$ENROLL_DATE)

# -------------------------------- Step 9: Composite flag - HIGH_RISK_FLAG
df <- df %>%
  mutate(HIGH_RISK_FLAG = if_else(
    BP_CATEGORY == "High" & (AGE >= 60 | BMI >= 30), "Yes", "No"
  ))

# -------------------------------------- Step 10: Review derived variables
cat("\nAGE_GROUP:\n");      print(table(df$AGE_GROUP))
cat("\nBP_CATEGORY:\n");    print(table(df$BP_CATEGORY))
cat("\nHIGH_RISK_FLAG:\n"); print(table(df$HIGH_RISK_FLAG))
cat("\nDAYS_ON_STUDY:\n");  print(summary(df$DAYS_ON_STUDY))

# ------------------------------------------- Step 11: Save analysis dataset
write_csv(df, "clinical_trial_data_analysis.csv")
cat("\nSaved: clinical_trial_data_analysis.csv", dim(df), "\n")
