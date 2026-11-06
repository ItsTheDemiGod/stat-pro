# Experiment 5: SDTM Conversion (R)
# Course: 26AI2003 - Statistical Programming for Clinical Data Analytics
#
# AIM
#   To convert raw clinical data from multiple tables into standardized SDTM
#   (Study Data Tabulation Model) datasets, preparing the data for regulatory
#   submission and analysis.
#
# DESCRIPTION
#   Five raw source files are converted into five SDTM domains:
#     raw_demographics.csv    -> DM  (1 row per subject)
#     raw_vitals.csv          -> VS  (1 row per vital sign per visit; wide -> long)
#     raw_labs.csv            -> LB  (1 row per lab test per visit;   wide -> long)
#     raw_adverse_events.csv  -> AE  (1 row per event)
#     raw_exposure.csv        -> EX  (1 row per drug)
#   Each domain gets STUDYID, DOMAIN, USUBJID, a sequence number XXSEQ, and ISO 8601
#   dates (YYYY-MM-DD). Every domain is then checked against the SDTM checklist.
#   Variable names follow the lab sheet (VSRESN, VSUNIT, VSSTDTC, EXDRUG ...).
#
# TRACEABILITY (raw -> SDTM)
#   USUBJID            every raw file SUBJID            "STUDY01-" + SUBJID
#   DM.RFSTDTC/RFENDTC FIRST_DOSE_DATE/LAST_DOSE_DATE   MM/DD/YYYY -> ISO 8601
#   VS.VSTESTCD/VSRESN column names SYSBP/DIABP/HEARTRATE and their values (pivot_longer)
#   VS.VSTEST/VSUNIT   mapping from VSTESTCD
#   VS.VSSTDTC         VISIT_DATE                       DD-Mon-YYYY -> ISO 8601
#   LB.LBTESTCD/LBRESN GLUCOSE/CHOLESTEROL (pivot_longer; GLUC, CHOL; blanks removed)
#   AE.*               AE_TERM, AE_START, AE_END, SEVERITY, RELATIONSHIP (renamed)
#   EX.*               DRUG, DOSE, DOSE_UNIT, START_DATE, END_DATE (renamed)
#   XXSEQ              none - row number within subject after sorting
#
# Needs: dplyr, tidyr  ->  install.packages(c("dplyr", "tidyr"))
# Keep the five raw CSV files in the working directory (or use setwd()).

library(dplyr)
library(tidyr)

STUDYID <- "STUDY01"

# ---------------------------------------- Step 1: Load the raw data
# SUBJID is read as text so that "001" keeps its leading zeros.
read_raw <- function(file) read.csv(file, colClasses = c(SUBJID = "character"))

raw_dm <- read_raw("raw_demographics.csv")
raw_vs <- read_raw("raw_vitals.csv")
raw_lb <- read_raw("raw_labs.csv")
raw_ae <- read_raw("raw_adverse_events.csv")
raw_ex <- read_raw("raw_exposure.csv")

cat("Raw shapes  DM:", dim(raw_dm), " VS:", dim(raw_vs), " LB:", dim(raw_lb),
    " AE:", dim(raw_ae), " EX:", dim(raw_ex), "\n")

# ---------------------------------------- Step 2: Helper function
# Convert a raw date to ISO 8601 (YYYY-MM-DD).
# Raw formats: MM/DD/YYYY ("%m/%d/%Y") and DD-Mon-YYYY ("%d-%b-%Y").
iso_date <- function(x, fmt) format(as.Date(x, format = fmt), "%Y-%m-%d")

# ---------------------------------------- Step 3: DM - Demographics
dm <- raw_dm %>%
  transmute(
    STUDYID = STUDYID,
    DOMAIN  = "DM",
    USUBJID = paste0(STUDYID, "-", SUBJID),
    SUBJID, SITEID, AGE,
    AGEU    = "YEARS",
    SEX, RACE, ARM,
    RFSTDTC = iso_date(FIRST_DOSE_DATE, "%m/%d/%Y"),
    RFENDTC = iso_date(LAST_DOSE_DATE,  "%m/%d/%Y")
  ) %>%
  arrange(USUBJID) %>%
  group_by(USUBJID) %>% mutate(DMSEQ = row_number()) %>% ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, DMSEQ, SUBJID, SITEID, AGE, AGEU,
         SEX, RACE, ARM, RFSTDTC, RFENDTC)
cat("\nDM rows:", nrow(dm), "\n")

# ---------------------------------------- Step 4: VS - Vital Signs (wide -> long)
# pivot_longer turns each vital-sign column into its own row.
vs_test <- c(SYSBP = "Systolic Blood Pressure",
             DIABP = "Diastolic Blood Pressure",
             HEARTRATE = "Heart Rate")
vs_unit <- c(SYSBP = "mmHg", DIABP = "mmHg", HEARTRATE = "beats/min")

vs <- raw_vs %>%
  pivot_longer(cols = c(SYSBP, DIABP, HEARTRATE),
               names_to = "VSTESTCD", values_to = "VSRESN") %>%
  mutate(
    STUDYID = STUDYID,
    DOMAIN  = "VS",
    USUBJID = paste0(STUDYID, "-", SUBJID),
    VSTEST  = unname(vs_test[VSTESTCD]),
    VSUNIT  = unname(vs_unit[VSTESTCD]),
    VSSTDTC = iso_date(VISIT_DATE, "%d-%b-%Y")
  ) %>%
  arrange(USUBJID, VISITNUM, VSTESTCD) %>%
  group_by(USUBJID) %>% mutate(VSSEQ = row_number()) %>% ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, VSSEQ, VSTESTCD, VSTEST, VSRESN,
         VSUNIT, VISITNUM, VISIT, VSSTDTC)
cat("VS rows:", nrow(vs), "(= 40 subjects x 3 visits x 3 tests)\n")

# ---------------------------------------- Step 5: LB - Laboratory (wide -> long)
# A lab test that was not done (blank result) is not written to SDTM.
lb_code <- c(GLUCOSE = "GLUC", CHOLESTEROL = "CHOL")
lb_name <- c(GLUC = "Glucose", CHOL = "Cholesterol")

lb_long <- raw_lb %>%
  pivot_longer(cols = c(GLUCOSE, CHOLESTEROL),
               names_to = "LBTEST_RAW", values_to = "LBRESN")

cat("Lab results missing in raw data and removed:", sum(is.na(lb_long$LBRESN)), "\n")

lb <- lb_long %>%
  filter(!is.na(LBRESN)) %>%
  mutate(
    STUDYID  = STUDYID,
    DOMAIN   = "LB",
    USUBJID  = paste0(STUDYID, "-", SUBJID),
    LBTESTCD = unname(lb_code[LBTEST_RAW]),
    LBTEST   = unname(lb_name[LBTESTCD]),
    LBUNIT   = "mg/dL",
    LBSTDTC  = iso_date(VISIT_DATE, "%d-%b-%Y")
  ) %>%
  arrange(USUBJID, VISITNUM, LBTESTCD) %>%
  group_by(USUBJID) %>% mutate(LBSEQ = row_number()) %>% ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, LBSEQ, LBTESTCD, LBTEST, LBRESN,
         LBUNIT, VISITNUM, VISIT, LBSTDTC)
cat("LB rows:", nrow(lb), "\n")

# ---------------------------------------- Step 6: AE - Adverse Events
ae <- raw_ae %>%
  transmute(
    STUDYID = STUDYID,
    DOMAIN  = "AE",
    USUBJID = paste0(STUDYID, "-", SUBJID),
    AETERM  = toupper(AE_TERM),
    AESTDTC = iso_date(AE_START, "%d-%b-%Y"),
    AEENDTC = iso_date(AE_END,   "%d-%b-%Y"),
    AESEV   = SEVERITY,
    AEREL   = RELATIONSHIP
  ) %>%
  arrange(USUBJID, AESTDTC) %>%
  group_by(USUBJID) %>% mutate(AESEQ = row_number()) %>% ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, AESEQ, AETERM, AESTDTC, AEENDTC, AESEV, AEREL)
cat("AE rows:", nrow(ae), "\n")

# ---------------------------------------- Step 7: EX - Exposure
ex <- raw_ex %>%
  transmute(
    STUDYID = STUDYID,
    DOMAIN  = "EX",
    USUBJID = paste0(STUDYID, "-", SUBJID),
    EXDRUG  = DRUG,
    EXDOSE  = DOSE,
    EXUNIT  = DOSE_UNIT,
    EXSTDTC = iso_date(START_DATE, "%m/%d/%Y"),
    EXENDTC = iso_date(END_DATE,   "%m/%d/%Y")
  ) %>%
  arrange(USUBJID, EXSTDTC) %>%
  group_by(USUBJID) %>% mutate(EXSEQ = row_number()) %>% ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, EXSEQ, EXDRUG, EXDOSE, EXUNIT, EXSTDTC, EXENDTC)
cat("EX rows:", nrow(ex), "\n")

# ---------------------------------------- Step 8: Validation checklist
show <- function(label, ok) cat("   ", if (isTRUE(ok)) "PASS" else "FAIL", "-", label, "\n")

check_domain <- function(d, code, seq, dtc = NULL) {
  cat("\n", code, "-", nrow(d), "rows\n")
  show(paste0("DOMAIN variable equals '", code, "'"), all(d$DOMAIN == code))
  show("no missing USUBJID / DOMAIN / sequence",
       !anyNA(d[, c("USUBJID", "DOMAIN", seq)]))
  show(paste(seq, "is unique within each subject"),
       !any(duplicated(d[, c("USUBJID", seq)])))
  show("no duplicate records", !any(duplicated(d)))
  if (!is.null(dtc)) {
    show(paste(dtc, "is ISO 8601 (YYYY-MM-DD)"),
         all(grepl("^\\d{4}-\\d{2}-\\d{2}$", d[[dtc]])))
  }
  for (col in names(d)[grepl("RESN$", names(d))]) {
    show(paste(col, "is numeric with no blanks"),
         is.numeric(d[[col]]) && !anyNA(d[[col]]))
  }
}

check_domain(dm, "DM", "DMSEQ")
check_domain(vs, "VS", "VSSEQ", "VSSTDTC")
check_domain(lb, "LB", "LBSEQ", "LBSTDTC")
check_domain(ae, "AE", "AESEQ", "AESTDTC")
check_domain(ex, "EX", "EXSEQ", "EXSTDTC")

# Every subject in VS, LB, AE and EX must also exist in DM
cat("\nSubjects in DM:", n_distinct(dm$USUBJID), "\n")
cat("VS: subjects not found in DM =", length(setdiff(vs$USUBJID, dm$USUBJID)), "\n")
cat("LB: subjects not found in DM =", length(setdiff(lb$USUBJID, dm$USUBJID)), "\n")
cat("AE: subjects not found in DM =", length(setdiff(ae$USUBJID, dm$USUBJID)), "\n")
cat("EX: subjects not found in DM =", length(setdiff(ex$USUBJID, dm$USUBJID)), "\n")

# Long-format check: VS must have 3 rows per subject per visit
cat("VS rows per subject per visit (should all be 3):",
    unique((vs %>% count(USUBJID, VISITNUM))$n), "\n")

# Timeline check: every date must fall between the subject's first and last dose
# (ISO dates compare correctly as text)
timeline_check <- function(d, code, dtc) {
  m <- inner_join(d, dm[, c("USUBJID", "RFSTDTC", "RFENDTC")], by = "USUBJID")
  bad <- m[m[[dtc]] < m$RFSTDTC | m[[dtc]] > m$RFENDTC, ]
  cat("\n", code, ": dates outside the study timeline =", nrow(bad), "\n")
  if (nrow(bad) > 0) print(bad[, c("USUBJID", dtc, "RFSTDTC", "RFENDTC")])
}
timeline_check(vs, "VS", "VSSTDTC")
timeline_check(lb, "LB", "LBSTDTC")
timeline_check(ae, "AE", "AESTDTC")
timeline_check(ex, "EX", "EXSTDTC")

# ---------------------------------------- Step 9: Save and summarise
write.csv(dm, "dm.csv", row.names = FALSE)
write.csv(vs, "vs.csv", row.names = FALSE)
write.csv(lb, "lb.csv", row.names = FALSE)
write.csv(ae, "ae.csv", row.names = FALSE)
write.csv(ex, "ex.csv", row.names = FALSE)

summary_tbl <- data.frame(
  Domain      = c("DM", "VS", "LB", "AE", "EX"),
  Raw_rows    = c(nrow(raw_dm), nrow(raw_vs), nrow(raw_lb), nrow(raw_ae), nrow(raw_ex)),
  SDTM_rows   = c(nrow(dm), nrow(vs), nrow(lb), nrow(ae), nrow(ex)),
  Variables   = c(ncol(dm), ncol(vs), ncol(lb), ncol(ae), ncol(ex))
)
cat("\nSaved: dm.csv, vs.csv, lb.csv, ae.csv, ex.csv\n")
print(summary_tbl)
