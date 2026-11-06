# Experiment 2: Data Cleaning of a Clinical Trial Dataset (R)
# Course: 26AI2003 - Statistical Programming for Clinical Data Analytics
#
# AIM
#   To detect and correct common data-quality problems in a raw clinical trial
#   dataset (missing values, inconsistent categories, implausible values, mixed
#   date formats, exact duplicate rows and duplicate patient IDs) and produce a
#   clean, analysis-ready dataset with a printed record of every change.
#
# DESCRIPTION
#   Input : clinical_trial_data_raw.csv   (193 rows, 180 unique patients)
#   Output: clinical_trial_data_clean.csv (180 rows, one per patient, no missing)
#
#   Cleaning steps, in order:
#     1. Report the "before" problems.
#     2. Standardize SEX and ARM (trim spaces, normalize case, map to one value).
#     3. Parse the mixed ENROLL_DATE formats and rewrite as YYYY-MM-DD.
#     4. Set implausible AGE values (outside 18-100) to missing.
#     5. Remove exact duplicate rows (keep one copy).
#     6. Remove duplicate USUBJID rows with conflicting data (keep first).
#     7. Impute missing values: median for numeric columns, "Unknown" for SITEID.
#     8. Print the "after" confirmation and save the clean file.
#
# Run:  Rscript exp2_data_cleaning.R   (base R only, no packages needed)

raw_file   <- "clinical_trial_data_raw.csv"
clean_file <- "clinical_trial_data_clean.csv"

age_min <- 18
age_max <- 100
numeric_cols <- c("AGE", "WEIGHT_KG", "BMI", "GLUCOSE_MGDL",
                  "CHOLESTEROL_MGDL", "HEARTRATE")

sex_map <- c("M" = "M", "MALE" = "M", "F" = "F", "FEMALE" = "F")
arm_map <- c("PLACEBO"  = "Placebo",
             "DRUG10MG" = "Drug 10mg",
             "DRUG20MG" = "Drug 20mg")

line <- function(title) {
  cat("\n", strrep("=", 70), "\n", title, "\n", strrep("=", 70), "\n", sep = "")
}

# ---------------------------------------------------------------- date helpers
month_abb <- c("jan", "feb", "mar", "apr", "may", "jun",
               "jul", "aug", "sep", "oct", "nov", "dec")

date_format_label <- function(s) {
  s <- trimws(as.character(s))
  ifelse(is.na(s) | s == "", "missing",
  ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", s), "YYYY-MM-DD",
  ifelse(grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{4}$", s), "DD-Mon-YYYY",
  ifelse(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", s),
         "slash (MM/DD/YYYY or DD/MM/YYYY)", "unrecognised"))))
}

ambiguous_dates <- character(0)

# Parse a single date string into a Date (locale independent)
parse_date <- function(s) {
  s <- trimws(as.character(s))
  if (is.na(s) || s == "") return(as.Date(NA))
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", s)) return(as.Date(s, format = "%Y-%m-%d"))
  if (grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{4}$", s)) {
    p <- strsplit(s, "-")[[1]]
    mo <- match(tolower(p[2]), month_abb)
    return(as.Date(sprintf("%s-%02d-%02d", p[3], mo, as.integer(p[1]))))
  }
  if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", s)) {
    p <- as.integer(strsplit(s, "/")[[1]])
    a <- p[1]; b <- p[2]; y <- p[3]
    if (a > 12) {                       # must be DD/MM/YYYY
      return(as.Date(sprintf("%d-%02d-%02d", y, b, a)))
    } else if (b > 12) {                # must be MM/DD/YYYY
      return(as.Date(sprintf("%d-%02d-%02d", y, a, b)))
    } else {                            # ambiguous: assume MM/DD/YYYY, log it
      ambiguous_dates <<- c(ambiguous_dates, s)
      return(as.Date(sprintf("%d-%02d-%02d", y, a, b)))
    }
  }
  as.Date(NA)
}

# ====================================================================== LOAD
df <- read.csv(raw_file, stringsAsFactors = FALSE, na.strings = c("", "NA"))
n_raw <- nrow(df)

# ============================================================ 1. BEFORE
line("1. BEFORE CLEANING")
cat("Rows:", n_raw, "  Columns:", ncol(df), "\n")
cat("Unique USUBJID:", length(unique(df$USUBJID)), "\n")

cat("\nMissing values per column:\n")
miss_before <- colSums(is.na(df))
print(miss_before[miss_before > 0])
cat("Total missing cells:", sum(miss_before), "\n")

cat("\nDistinct SEX values:", sort(unique(na.omit(df$SEX))), "\n")
cat("Distinct ARM values:", paste0("'", sort(unique(na.omit(df$ARM))), "'"), "\n")

bad_age_before <- df$AGE[!is.na(df$AGE) & (df$AGE < age_min | df$AGE > age_max)]
cat("\nImplausible AGE values (outside ", age_min, "-", age_max, "): ",
    length(bad_age_before), "  -> ", paste(sort(bad_age_before), collapse = ", "),
    "\n", sep = "")

cat("\nENROLL_DATE formats found:\n")
print(table(date_format_label(df$ENROLL_DATE)))

n_exact <- sum(duplicated(df))
cat("\nExact duplicate rows:", n_exact, "\n")
no_exact <- df[!duplicated(df), ]
n_dup_id <- sum(duplicated(no_exact$USUBJID))
cat("Duplicate USUBJID with conflicting data:", n_dup_id, "\n")
if (n_dup_id > 0) {
  conflict_ids <- unique(no_exact$USUBJID[duplicated(no_exact$USUBJID)])
  cat("Conflicting records (same USUBJID, different HEARTRATE):\n")
  conf <- no_exact[no_exact$USUBJID %in% conflict_ids,
                   c("USUBJID", "ARM", "AGE", "HEARTRATE")]
  print(conf[order(conf$USUBJID), ], row.names = FALSE)
}

# ============================================== 2. STANDARDIZE CATEGORIES
line("2. STANDARDIZE SEX AND ARM")
sex_key <- toupper(trimws(df$SEX))
arm_key <- gsub("\\s+", "", toupper(trimws(df$ARM)))
df$SEX <- unname(sex_map[sex_key])
df$ARM <- unname(arm_map[arm_key])
cat("Unmapped SEX values:", sum(is.na(df$SEX)) - miss_before["SEX"], "\n")
cat("Unmapped ARM values:", sum(is.na(df$ARM)) - miss_before["ARM"], "\n")
cat("SEX after:", sort(unique(na.omit(df$SEX))), "\n")
cat("ARM after:", sort(unique(na.omit(df$ARM))), "\n")

# ============================================================ 3. DATES
line("3. STANDARDIZE ENROLL_DATE TO YYYY-MM-DD")
parsed <- do.call(c, lapply(df$ENROLL_DATE, parse_date))
cat("Dates that could not be parsed:", sum(is.na(parsed)), "\n")
cat("Ambiguous slash dates (assumed MM/DD/YYYY):", length(ambiguous_dates), "\n")
df$ENROLL_DATE <- format(parsed, "%Y-%m-%d")
cat("Example:", head(df$ENROLL_DATE, 3), "\n")

# ============================================================== 4. AGE
line("4. IMPLAUSIBLE AGE VALUES -> MISSING")
bad_age <- !is.na(df$AGE) & (df$AGE < age_min | df$AGE > age_max)
cat("AGE values set to missing:", sum(bad_age), "\n")
df$AGE[bad_age] <- NA

# ====================================================== 5. EXACT DUPLICATES
line("5. REMOVE EXACT DUPLICATE ROWS")
before <- nrow(df)
df <- df[!duplicated(df), ]
cat("Rows before:", before, "  after:", nrow(df), "  removed:", before - nrow(df), "\n")

# ================================================= 6. DUPLICATE PATIENT IDS
line("6. REMOVE DUPLICATE USUBJID (KEEP FIRST OCCURRENCE)")
before <- nrow(df)
df <- df[!duplicated(df$USUBJID), ]
cat("Rows before:", before, "  after:", nrow(df), "  removed:", before - nrow(df), "\n")
cat("Rule: first occurrence kept. In a real study the discrepancy would be",
    "traced to the source CRF.\n")

# ======================================================== 7. MISSING VALUES
line("7. IMPUTE MISSING VALUES")
audit <- data.frame(Column = character(), Filled = integer(),
                    Value_used = character(), stringsAsFactors = FALSE)
for (col in numeric_cols) {
  n <- sum(is.na(df[[col]]))
  if (n > 0) {
    med <- median(df[[col]], na.rm = TRUE)
    df[[col]][is.na(df[[col]])] <- med
    audit[nrow(audit) + 1, ] <- list(col, n, sprintf("median = %.2f", med))
  }
}
n <- sum(is.na(df$SITEID))
if (n > 0) {
  df$SITEID[is.na(df$SITEID)] <- "Unknown"
  audit[nrow(audit) + 1, ] <- list("SITEID", n, "'Unknown'")
}
print(audit, row.names = FALSE)

df$AGE       <- as.integer(round(df$AGE))
df$HEARTRATE <- as.integer(round(df$HEARTRATE))

# ============================================================== 8. AFTER
line("8. AFTER CLEANING")
df <- df[order(df$USUBJID), ]
rownames(df) <- NULL
cat("Rows:", nrow(df), "  Unique USUBJID:", length(unique(df$USUBJID)), "\n")
cat("Total missing cells:", sum(is.na(df)), "\n")
cat("Exact duplicate rows:", sum(duplicated(df)), "\n")
cat("Duplicate USUBJID:", sum(duplicated(df$USUBJID)), "\n")
cat("SEX values:", sort(unique(df$SEX)), "\n")
cat("ARM values:", sort(unique(df$ARM)), "\n")
cat("AGE range:", min(df$AGE), "-", max(df$AGE), "\n")
cat("ENROLL_DATE all YYYY-MM-DD:",
    all(grepl("^\\d{4}-\\d{2}-\\d{2}$", df$ENROLL_DATE)), "\n")

stopifnot(nrow(df) == 180, !anyDuplicated(df$USUBJID), sum(is.na(df)) == 0)

write.csv(df, clean_file, row.names = FALSE)
cat("\nClean dataset saved to", clean_file, "\n")
