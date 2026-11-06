"""
Experiment 1: Data Import and Exploration of a Clinical Trial Dataset (Python)
Course: 26AI2003 - Statistical Programming for Clinical Data Analytics

AIM
    To import a clinical trial dataset from a CSV file and explore its structure,
    quality and basic statistical characteristics.

DESCRIPTION
    The script reads clinical_trial_sample.csv, inspects its dimensions and data
    types, checks missing values and duplicates, computes descriptive statistics
    for numeric and categorical variables, derives the change in systolic blood
    pressure (SBP) from baseline to week 12, compares treatment arms, and saves
    simple plots.

Run:  python exp1_import_explore.py
Needs: pandas, matplotlib
"""

import pandas as pd
import matplotlib.pyplot as plt

CSV_FILE = "clinical_trial_sample.csv"

# ---------------------------------------------------------------- 1. Import
df = pd.read_csv(CSV_FILE)
print("=== 1. DATA IMPORTED ===")
print(f"File: {CSV_FILE}")

# ------------------------------------------------------- 2. Structure check
print("\n=== 2. STRUCTURE ===")
print("Dimensions (rows, columns):", df.shape)
print("\nFirst 5 rows:")
print(df.head())
print("\nLast 5 rows:")
print(df.tail())
print("\nColumn names:", list(df.columns))
print("\nData types:")
print(df.dtypes)
print("\nInfo:")
df.info()

# Convert categorical columns to category dtype
for col in ["site", "treatment", "sex", "adverse_event"]:
    df[col] = df[col].astype("category")

# ------------------------------------------------------ 3. Data quality
print("\n=== 3. DATA QUALITY ===")
missing = df.isna().sum()
missing_pct = (df.isna().mean() * 100).round(2)
print("Missing values per column:")
print(pd.DataFrame({"missing": missing, "percent": missing_pct}))
print("\nDuplicate rows:", df.duplicated().sum())
print("Duplicate patient IDs:", df["patient_id"].duplicated().sum())
print("Complete cases:", df.dropna().shape[0], "of", df.shape[0])

# ------------------------------------------------ 4. Descriptive statistics
print("\n=== 4. DESCRIPTIVE STATISTICS ===")
num_cols = ["age", "bmi", "sbp_baseline", "sbp_week12", "cholesterol"]
print("Numeric variables:")
print(df[num_cols].describe().round(2))
print("\nMedian:\n", df[num_cols].median().round(2))
print("\nSkewness:\n", df[num_cols].skew().round(3))

print("\nCategorical variables:")
for col in ["site", "treatment", "sex", "adverse_event"]:
    print(f"\n{col}:")
    print(df[col].value_counts(dropna=False))
    print((df[col].value_counts(normalize=True) * 100).round(1).astype(str) + " %")

# ------------------------------------------ 5. Derived variable and by-arm
print("\n=== 5. CHANGE IN SBP BY TREATMENT ARM ===")
df["sbp_change"] = df["sbp_week12"] - df["sbp_baseline"]
summary = df.groupby("treatment", observed=True).agg(
    n=("patient_id", "count"),
    mean_age=("age", "mean"),
    mean_bmi=("bmi", "mean"),
    mean_sbp_baseline=("sbp_baseline", "mean"),
    mean_sbp_change=("sbp_change", "mean"),
    sd_sbp_change=("sbp_change", "std"),
).round(2)
print(summary)

print("\nSex distribution by treatment:")
print(pd.crosstab(df["treatment"], df["sex"]))
print("\nAdverse events by treatment:")
print(pd.crosstab(df["treatment"], df["adverse_event"]))

# Outlier check (IQR rule)
print("\nOutliers (IQR rule):")
for col in num_cols:
    q1, q3 = df[col].quantile([0.25, 0.75])
    iqr = q3 - q1
    n_out = ((df[col] < q1 - 1.5 * iqr) | (df[col] > q3 + 1.5 * iqr)).sum()
    print(f"  {col}: {n_out}")

# Correlation
print("\nCorrelation matrix:")
print(df[num_cols + ["sbp_change"]].corr().round(2))

# ------------------------------------------------------------- 6. Plots
fig, axes = plt.subplots(2, 2, figsize=(11, 8))
df["age"].plot.hist(bins=12, ax=axes[0, 0], edgecolor="black")
axes[0, 0].set_title("Age distribution")
axes[0, 0].set_xlabel("Age (years)")

df.boxplot(column="sbp_change", by="treatment", ax=axes[0, 1])
axes[0, 1].set_title("Change in SBP by treatment")
axes[0, 1].set_xlabel("Treatment")
axes[0, 1].set_ylabel("Change (mmHg)")

df["treatment"].value_counts().plot.bar(ax=axes[1, 0], rot=0)
axes[1, 0].set_title("Patients per arm")

df.plot.scatter(x="bmi", y="cholesterol", ax=axes[1, 1])
axes[1, 1].set_title("BMI vs cholesterol")

plt.suptitle("")
plt.tight_layout()
plt.savefig("exp1_python_plots.png", dpi=150)
print("\nPlots saved to exp1_python_plots.png")

# Save cleaned/derived dataset
df.to_csv("clinical_trial_python_output.csv", index=False)
print("Derived dataset saved to clinical_trial_python_output.csv")
