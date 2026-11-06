# Experiment 4: Data Visualization (R)
# Course: 26AI2003 - Statistical Programming for Clinical Data Analytics
#
# AIM
#   To create visual representations of clinical data (histograms, bar charts,
#   box plots, scatter plots and a line plot) in R, to explore distributions,
#   compare patient groups, and communicate findings effectively in clinical
#   research.
#
# DESCRIPTION
#   Reads clinical_trial_data_viz.csv (180 patients: age, sex, race, treatment
#   arm, height, weight, BMI, blood pressure, glucose, age group, risk category)
#   and clinical_visits.csv (weight every 2 weeks, for the line plot). Draws one
#   plot for each row of the observation table in the lab sheet using ggplot2.
#   Every plot has a title and axis labels with units and uses a colour-blind
#   friendly palette. Each plot is also saved as a PNG file.
#
#     Plot 1  Histogram    AGE
#     Plot 2  Histogram    BMI
#     Plot 3  Bar chart    SEX
#     Plot 4  Bar chart    RACE
#     Plot 5  Box plot     BMI by AGE_GROUP
#     Plot 6  Box plot     SYSBP and DIABP by ARM
#     Plot 7  Box plot     GLUCOSE_MGDL by RISK_CATEGORY
#     Plot 8  Scatter plot HEIGHT_CM vs WEIGHT_KG
#     Plot 9  Scatter plot SYSBP vs DIABP
#     Plot 10 Line plot    mean weight by WEEK and ARM
#
# Needs: ggplot2  ->  install.packages("ggplot2")
# Keep both CSV files in the working directory (or use setwd("path/to/folder")).

library(ggplot2)

# Colour-blind friendly palette (Okabe-Ito)
cb <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7")

theme_set(theme_minimal(base_size = 13))

# ------------------------------------------------ Step 1: Load and check data
df     <- read.csv("clinical_trial_data_viz.csv")
visits <- read.csv("clinical_visits.csv")

cat("Patients:", dim(df), "\n")
cat("Visits  :", dim(visits), "\n")
cat("Missing values per column:\n")
print(colSums(is.na(df))[colSums(is.na(df)) > 0])

# Put categories in a sensible order
arm_order <- c("Placebo", "Drug 10mg", "Drug 20mg")
df$ARM        <- factor(df$ARM, levels = arm_order)
df$AGE_GROUP  <- factor(df$AGE_GROUP, levels = c("<40", "40-59", "60+"))
df$RISK_CATEGORY <- factor(df$RISK_CATEGORY, levels = c("Low", "Medium", "High"))
visits$ARM    <- factor(visits$ARM, levels = arm_order)

# ------------------------------------------------- Plot 1: Age histogram
p1 <- ggplot(df, aes(x = AGE)) +
  geom_histogram(bins = 15, fill = cb[1], colour = "white") +
  labs(title = "Age Distribution of Patients",
       x = "Age (years)", y = "Number of patients")
ggsave("plot01_age_histogram.png", p1, width = 7, height = 4.5, dpi = 150)
print(p1)

# ------------------------------------------------- Plot 2: BMI histogram
p2 <- ggplot(df, aes(x = BMI)) +
  geom_histogram(bins = 15, fill = cb[1], colour = "white") +
  labs(title = "BMI Distribution of Patients",
       x = "BMI (kg/m²)", y = "Number of patients")
ggsave("plot02_bmi_histogram.png", p2, width = 7, height = 4.5, dpi = 150)
print(p2)

# ------------------------------------------------- Plot 3: Gender bar chart
p3 <- ggplot(df, aes(x = SEX)) +
  geom_bar(fill = cb[1]) +
  labs(title = "Gender Distribution",
       x = "Sex (M = male, F = female)", y = "Number of patients")
ggsave("plot03_gender_bar.png", p3, width = 5.5, height = 4.5, dpi = 150)
print(p3)

# ------------------------------------------------- Plot 4: Race bar chart
df$RACE <- factor(df$RACE, levels = names(sort(table(df$RACE), decreasing = TRUE)))
p4 <- ggplot(df, aes(x = RACE)) +
  geom_bar(fill = cb[1]) +
  labs(title = "Race Distribution", x = "Race", y = "Number of patients")
ggsave("plot04_race_bar.png", p4, width = 7, height = 4.5, dpi = 150)
print(p4)

# ------------------------------------------------- Plot 5: BMI by age group
p5 <- ggplot(df, aes(x = AGE_GROUP, y = BMI, fill = AGE_GROUP)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = cb[1:3]) +
  labs(title = "BMI by Age Group",
       x = "Age group (years)", y = "BMI (kg/m²)")
ggsave("plot05_bmi_by_age_group.png", p5, width = 7, height = 4.5, dpi = 150)
print(p5)

# ------------------------------------------------- Plot 6: BP by treatment arm
# Stack systolic and diastolic into one table so they can be drawn side by side.
bp <- rbind(
  data.frame(ARM = df$ARM, TYPE = "Systolic BP",  VALUE = df$SYSBP),
  data.frame(ARM = df$ARM, TYPE = "Diastolic BP", VALUE = df$DIABP)
)
p6 <- ggplot(bp, aes(x = ARM, y = VALUE, fill = ARM)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ TYPE) +
  scale_fill_manual(values = cb[1:3]) +
  labs(title = "Blood Pressure by Treatment Arm",
       x = "Treatment arm", y = "Blood pressure (mmHg)")
ggsave("plot06_bp_by_arm.png", p6, width = 11, height = 4.5, dpi = 150)
print(p6)

# ------------------------------------------------- Plot 7: Glucose by risk
p7 <- ggplot(df, aes(x = RISK_CATEGORY, y = GLUCOSE_MGDL, fill = RISK_CATEGORY)) +
  geom_boxplot(show.legend = FALSE, na.rm = TRUE) +
  scale_fill_manual(values = cb[1:3]) +
  labs(title = "Glucose by Risk Category",
       x = "Risk category", y = "Glucose (mg/dL)")
ggsave("plot07_glucose_by_risk.png", p7, width = 7, height = 4.5, dpi = 150)
print(p7)

# ------------------------------------------------- Plot 8: Height vs weight
p8 <- ggplot(df, aes(x = HEIGHT_CM, y = WEIGHT_KG, colour = SEX)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_colour_manual(values = cb[1:2]) +
  labs(title = "Height vs Weight",
       x = "Height (cm)", y = "Weight (kg)", colour = "Sex")
ggsave("plot08_height_vs_weight.png", p8, width = 7, height = 5, dpi = 150)
print(p8)

# ------------------------------------------------- Plot 9: Systolic vs diastolic
p9 <- ggplot(df, aes(x = SYSBP, y = DIABP, colour = ARM)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_colour_manual(values = cb[1:3]) +
  labs(title = "Systolic vs Diastolic Blood Pressure",
       x = "Systolic BP (mmHg)", y = "Diastolic BP (mmHg)",
       colour = "Treatment arm")
ggsave("plot09_sys_vs_dia.png", p9, width = 7, height = 5, dpi = 150)
print(p9)

# ------------------------------------------------- Plot 10: Weight over time
# Average weight of each arm at each visit.
mean_weight <- aggregate(WEIGHT_KG ~ WEEK + ARM, data = visits, FUN = mean)

p10 <- ggplot(mean_weight, aes(x = WEEK, y = WEIGHT_KG, colour = ARM)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = cb[1:3]) +
  labs(title = "Mean Weight Over Study Weeks",
       x = "Study week", y = "Mean weight (kg)", colour = "Treatment arm")
ggsave("plot10_weight_over_time.png", p10, width = 7.5, height = 5, dpi = 150)
print(p10)

# ------------------------------- Step 3: Numbers behind the plots
cat("\nAge: mean", round(mean(df$AGE), 1), ", median", median(df$AGE),
    ", range", range(df$AGE), "\n")
cat("BMI: mean", round(mean(df$BMI), 1), ", median", median(df$BMI), "\n")
print(table(df$SEX))
print(table(df$RACE))
cat("\nMedian SYSBP by arm:\n");  print(tapply(df$SYSBP, df$ARM, median))
cat("\nMedian glucose by risk:\n")
print(tapply(df$GLUCOSE_MGDL, df$RISK_CATEGORY, median, na.rm = TRUE))
cat("\nHeight-weight correlation:", round(cor(df$HEIGHT_CM, df$WEIGHT_KG), 2), "\n")
cat("SYSBP-DIABP correlation  :", round(cor(df$SYSBP, df$DIABP), 2), "\n")

# ------------------------------- Step 4: Outliers and data-quality checks
cat("\nMissing glucose values:", sum(is.na(df$GLUCOSE_MGDL)), "\n")

# IQR rule: outlier if beyond 1.5 x IQR from the quartiles
for (col in c("BMI", "SYSBP", "DIABP", "GLUCOSE_MGDL", "WEIGHT_KG")) {
  x <- df[[col]]
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  is_out <- !is.na(x) & (x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr)
  cat(col, ":", sum(is_out), "outliers ->", df$USUBJID[is_out], "\n")
}

cat("\nShort and heavy (height < 155 cm and weight > 100 kg):\n")
print(df[df$HEIGHT_CM < 155 & df$WEIGHT_KG > 100,
         c("USUBJID", "HEIGHT_CM", "WEIGHT_KG", "BMI")])
