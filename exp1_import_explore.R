# Experiment 1: Data Import and Exploration of a Clinical Trial Dataset (R)
# Course: 26AI2003 - Statistical Programming for Clinical Data Analytics
#
# AIM
#   To import a clinical trial dataset from a CSV file and explore its structure,
#   quality and basic statistical characteristics.
#
# DESCRIPTION
#   The script reads clinical_trial_sample.csv, inspects its dimensions and data
#   types, checks missing values and duplicates, computes descriptive statistics
#   for numeric and categorical variables, derives the change in systolic blood
#   pressure (SBP) from baseline to week 12, compares treatment arms, and saves
#   simple plots.
#
# Run:  Rscript exp1_import_explore.R   (base R only, no packages needed)

csv_file <- "clinical_trial_sample.csv"

# ---------------------------------------------------------------- 1. Import
df <- read.csv(csv_file, stringsAsFactors = FALSE, na.strings = c("", "NA"))
cat("=== 1. DATA IMPORTED ===\n")
cat("File:", csv_file, "\n")

# ------------------------------------------------------- 2. Structure check
cat("\n=== 2. STRUCTURE ===\n")
cat("Dimensions (rows, columns):", dim(df), "\n")
cat("\nFirst 5 rows:\n");  print(head(df, 5))
cat("\nLast 5 rows:\n");   print(tail(df, 5))
cat("\nColumn names:", names(df), "\n")
cat("\nStructure:\n");     str(df)

# Convert categorical columns to factors
for (col in c("site", "treatment", "sex", "adverse_event")) {
  df[[col]] <- as.factor(df[[col]])
}

# ------------------------------------------------------ 3. Data quality
cat("\n=== 3. DATA QUALITY ===\n")
missing <- colSums(is.na(df))
missing_pct <- round(colMeans(is.na(df)) * 100, 2)
print(data.frame(missing = missing, percent = missing_pct))
cat("\nDuplicate rows:", sum(duplicated(df)), "\n")
cat("Duplicate patient IDs:", sum(duplicated(df$patient_id)), "\n")
cat("Complete cases:", sum(complete.cases(df)), "of", nrow(df), "\n")

# ------------------------------------------------ 4. Descriptive statistics
cat("\n=== 4. DESCRIPTIVE STATISTICS ===\n")
num_cols <- c("age", "bmi", "sbp_baseline", "sbp_week12", "cholesterol")
cat("Numeric variables:\n")
print(summary(df[, num_cols]))

desc <- sapply(df[, num_cols], function(x) {
  c(mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE))
})
cat("\nMean / median / SD / min / max:\n")
print(round(desc, 2))

cat("\nCategorical variables:\n")
for (col in c("site", "treatment", "sex", "adverse_event")) {
  cat("\n", col, ":\n", sep = "")
  print(table(df[[col]], useNA = "ifany"))
  print(round(prop.table(table(df[[col]])) * 100, 1))
}

# ------------------------------------------ 5. Derived variable and by-arm
cat("\n=== 5. CHANGE IN SBP BY TREATMENT ARM ===\n")
df$sbp_change <- df$sbp_week12 - df$sbp_baseline

summary_arm <- aggregate(
  cbind(age, bmi, sbp_baseline, sbp_change) ~ treatment,
  data = df, FUN = function(x) round(mean(x), 2), na.action = na.omit
)
summary_arm$n <- as.vector(table(df$treatment))
summary_arm$sd_sbp_change <- round(
  tapply(df$sbp_change, df$treatment, sd, na.rm = TRUE), 2)
print(summary_arm)

cat("\nSex distribution by treatment:\n")
print(table(df$treatment, df$sex))
cat("\nAdverse events by treatment:\n")
print(table(df$treatment, df$adverse_event))

cat("\nOutliers (IQR rule):\n")
for (col in num_cols) {
  q <- quantile(df[[col]], c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  n_out <- sum(df[[col]] < q[1] - 1.5 * iqr |
               df[[col]] > q[2] + 1.5 * iqr, na.rm = TRUE)
  cat(" ", col, ":", n_out, "\n")
}

cat("\nCorrelation matrix:\n")
print(round(cor(df[, c(num_cols, "sbp_change")], use = "pairwise.complete.obs"), 2))

# ------------------------------------------------------------- 6. Plots
png("exp1_R_plots.png", width = 1100, height = 800, res = 120)
par(mfrow = c(2, 2))
hist(df$age, breaks = 12, col = "lightblue", main = "Age distribution",
     xlab = "Age (years)")
boxplot(sbp_change ~ treatment, data = df, col = c("orange", "lightgreen"),
        main = "Change in SBP by treatment", xlab = "Treatment",
        ylab = "Change (mmHg)")
barplot(table(df$treatment), col = "steelblue", main = "Patients per arm")
plot(df$bmi, df$cholesterol, pch = 19, col = "darkgray",
     main = "BMI vs cholesterol", xlab = "BMI", ylab = "Cholesterol")
dev.off()
cat("\nPlots saved to exp1_R_plots.png\n")

# Save derived dataset
write.csv(df, "clinical_trial_R_output.csv", row.names = FALSE)
cat("Derived dataset saved to clinical_trial_R_output.csv\n")
