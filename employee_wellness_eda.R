################################################################################
# EMPLOYEE WELLNESS ANALYSIS — EDA PROJECT (R version)
# Converted from Python (pandas/matplotlib/seaborn) to R (tidyverse/ggplot2)
################################################################################

# ---- Packages ----
# install.packages(c("tidyverse", "corrplot", "GGally", "scales"))
library(tidyverse)
library(corrplot)
library(GGally)
library(scales)

################################################################################
# DATA LOADING
################################################################################

df <- read_csv("employee_wellness_dataset.csv")
glimpse(df)
head(df)
tail(df)
summary(df$Age)  # equivalent of df.describe() for the numeric Age column

# Null counts per column (equivalent of df.isnull().sum())
colSums(is.na(df))

################################################################################
# DATA CLEANING AND PRE-PROCESSING
################################################################################

# ---- STEP 1 — Drop Irrelevant Columns ----
# S.No -> serial number, no analytical value
# comments -> 916/1048 NaN (87% empty)
# state -> 412 NaN, mostly non-US respondents, too sparse
# Timestamp -> 3-day survey window, no temporal insight
# Country -> skewed toward US, not useful for global comparison
# no_employees -> not needed per problem statement
df <- df %>%
  select(-`S.No`, -comments, -state, -Timestamp, -Country, -no_employees)

# ---- STEP 2 — Fix Age (Handling Outliers using Box Plot) ----

# Boxplot BEFORE cleaning
ggplot(df, aes(x = Age)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(title = "Age Distribution - Before Cleaning") +
  theme_minimal()

# Values outside 18-65 = data entry errors -> replace with median of valid ages
median_age <- median(df$Age[df$Age >= 18 & df$Age <= 65], na.rm = TRUE)
df$Age <- ifelse(df$Age < 18 | df$Age > 65, median_age, df$Age)
df$Age <- as.integer(df$Age)

# Boxplot AFTER cleaning
ggplot(df, aes(x = Age)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(title = "Age Distribution - After Cleaning") +
  theme_minimal()

# Quantiles
Q1 <- quantile(df$Age, 0.25)
Q3 <- quantile(df$Age, 0.75)

cat("Min    :", min(df$Age), "\n")
cat("Q1     :", Q1, "\n")
cat("Median :", median(df$Age), "\n")
cat("Q3     :", Q3, "\n")
cat("Max    :", max(df$Age), "\n")
cat("IQR    :", Q3 - Q1, "\n")

# Observation: Before cleaning, the boxplot showed an extreme outlier making the
# scale blow out to ~1e11. After cleaning, all 6 invalid age entries were
# replaced with the median age of valid employees (18-65). Ages beyond 50 that
# still appear at the edge of the plot are valid senior employees, not errors —
# the dataset is young-skewed, concentrated between 27 and 36.

# ---- STEP 3 — Standardize Gender Column ----
# 45 unique free-text values -> standardized to Male / Female / Other

male_terms <- c("male", "m", "male-ish", "maile", "cis male", "mal",
                "male (cis)", "make", "male ", "msle", "malr", "mail",
                "man", "guy (-ish) ^_^", "male leaning androgynous")

female_terms <- c("female", "f", "woman", "cis female", "femake",
                   "female ", "female (cis)", "cis-female/femme",
                   "female (trans)", "femail", "trans-female", "trans woman")

# other_terms captured implicitly — anything not in male/female falls to "Other"

clean_gender <- function(g) {
  g <- str_trim(str_to_lower(as.character(g)))
  case_when(
    g %in% male_terms   ~ "Male",
    g %in% female_terms ~ "Female",
    TRUE                 ~ "Other"
  )
}

df$Gender <- clean_gender(df$Gender)

# Observation: Gender column cleaned from 45 unique values down to 3
# standardized categories — Male, Female, Other.
table(df$Gender)

# ---- STEP 4 — Handle Null Values (Imputation) ----
# self_employed      -> 18 nulls  -> "No"
# work_interfere      -> 236 nulls -> "Not Applicable"
# benefits            -> 13 nulls  -> "Don't Know"
# wellness_program    -> 4 nulls   -> "Don't Know"
# leave               -> 4 nulls   -> "Don't Know"

df <- df %>%
  mutate(
    self_employed    = replace_na(self_employed, "No"),
    work_interfere   = replace_na(work_interfere, "Not Applicable"),
    benefits         = replace_na(benefits, "Don't Know"),
    wellness_program = replace_na(wellness_program, "Don't Know"),
    leave            = replace_na(leave, "Don't Know")
  )

# ---- STEP 5 — Lowercase Standardization ----
# Apply lowercase + trim to all character columns EXCEPT Gender (already clean)

char_cols <- df %>% select(where(is.character)) %>% select(-Gender) %>% names()

df <- df %>%
  mutate(across(all_of(char_cols), ~ str_to_lower(str_trim(.))))

# Observation: All text columns (except Gender) standardized to lowercase,
# trimmed of whitespace, ready for grouping/analysis.

# ---- STEP 6 — Checking Duplicates & Final Verification ----
cat("Duplicate rows:", sum(duplicated(df)), "\n")
df <- df %>% distinct()
dim(df)
colSums(is.na(df))
glimpse(df)
head(df)

# Data Cleaning Summary
# - Removed S.No, comments, Timestamp, Country, no_employees, state
# - Corrected 6 invalid Age entries
# - Standardized Gender into Male / Female / Other
# - Imputed missing values in relevant columns
# - Lowercased + trimmed all text columns
# Result: 1046 clean, analysis-ready rows.

################################################################################
# PROBLEM STATEMENT
# Which workplace support gaps, stigma-related factors, and work interference
# patterns are most associated with employees reporting a need for mental
# health treatment?
################################################################################

################################################################################
# BUILDING KPIs
################################################################################

# ---- KPI 1 — Employer Support Index ----
# yes=1, no=0, don't know=0.5 — average of benefits, wellness, seek_help, anonymity
supp_index <- c("yes" = 1, "no" = 0, "don't know" = 0.5)

df <- df %>%
  mutate(
    benefits_score          = supp_index[benefits],
    wellness_score           = supp_index[wellness_program],
    seek_help_score           = supp_index[seek_help],
    anonymity_score            = supp_index[anonymity],
    employer_support_index = (benefits_score + wellness_score +
                                 seek_help_score + anonymity_score) / 4
  )
summary(df$employer_support_index)

# mean ~0.44 -> average company provides only 44% of possible support
# min 0.00   -> some employees get zero support
# median 0.375 -> half the workforce falls below that
# max 1.00   -> some employees get full support on all 4 factors
# sd ~0.27   -> wide variation across companies

# ---- KPI 2 — Workplace Interference Score ----
interference_map <- c("often" = 3, "sometimes" = 2, "rarely" = 1,
                       "never" = 0, "not applicable" = 0)
df$interference_score <- interference_map[df$work_interfere]
table(df$interference_score)

# ---- KPI 3 — Stigma Index ----
# mental_health_consequence: yes=1 (high stigma), maybe=0.5, no=0
stigma_map <- c("yes" = 1, "maybe" = 0.5, "no" = 0)
obs_map    <- c("yes" = 1, "no" = 0)

df <- df %>%
  mutate(
    mh_stigma_score  = stigma_map[mental_health_consequence],
    obs_stigma_score = obs_map[obs_consequence],
    stigma_index      = (mh_stigma_score + obs_stigma_score) / 2
  )
summary(df$stigma_index)

# 75% of employees score <=0.50, but the top 25% above that are the most
# at-risk group — likely avoiding treatment out of fear of consequences.
# Note: physical health consequence intentionally excluded from stigma_index,
# reserved for a separate mental-vs-physical stigma-gap comparison.

# ---- KPI 4 — Disclosure Comfort Score ----
disclosure_map <- c("yes" = 1, "some of them" = 0.5, "no" = 0)

df <- df %>%
  mutate(
    coworker_comfort          = disclosure_map[coworkers],
    supervisor_comfort         = disclosure_map[supervisor],
    disclosure_comfort_score = (coworker_comfort + supervisor_comfort) / 2
  )
summary(df$disclosure_comfort_score)

# mean = median = 0.50 -> workforce evenly split between comfortable and
# uncomfortable; comfort depends heavily on individual company culture.

# ---- KPI 5 — Leave Accessibility Score ----
leave_map <- c("very easy" = 4, "somewhat easy" = 3, "don't know" = 2,
               "somewhat difficult" = 1, "very difficult" = 0)
df$leave_accessibility_score <- leave_map[df$leave]
summary(df$leave_accessibility_score)

# Median = 2.0 ("don't know") but 75th percentile = 3.0 ("somewhat easy") ->
# dominance of "don't know" suggests employees are unaware of leave policy
# rather than facing an actual access barrier — a communication gap.

# ---- KPI 6 — Family Risk Flag ----
df$family_risk_flag <- ifelse(df$family_history == "yes", 1, 0)
table(df$family_risk_flag)

################################################################################
# DATA VISUALIZATION AND GAINING INSIGHTS
################################################################################

# ---- 1. Univariate Analysis — Categorical Columns (Bar Plots) ----

cat_cols <- c("treatment", "family_history", "work_interfere", "Gender",
              "benefits", "wellness_program", "seek_help", "anonymity",
              "leave", "mental_health_consequence", "phys_health_consequence",
              "obs_consequence", "coworkers", "supervisor", "remote_work")

plot_list <- map(cat_cols, function(col) {
  vc <- df %>% count(.data[[col]]) %>% arrange(desc(n))
  ggplot(vc, aes(x = reorder(.data[[col]], -n), y = n, fill = .data[[col]])) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -0.4, size = 3) +
    labs(title = paste("Distribution of", col), x = col, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
})

# Combine into a grid (needs patchwork or gridExtra)
# library(patchwork)
# wrap_plots(plot_list, ncol = 3)

# Print univariate insight summary
for (col in cat_cols) {
  vc <- df %>% count(.data[[col]]) %>% arrange(desc(n)) %>%
    mutate(pct = round(n / sum(n) * 100, 1))
  cat("\n*", toupper(col), "\n")
  cat("  Most common value:", vc[[col]][1], paste0("(", vc$pct[1], "%)\n"))
  print(vc)
}

# Observation: Most employees work in companies with moderate employer
# support. Treatment need is nearly evenly split. Family history and work
# interference show significant presence — both biological and workplace
# factors are active in this dataset.

# ---- Univariate Analysis — Numerical/KPI Columns (Histogram + KDE) ----

num_cols <- c("Age", "employer_support_index", "stigma_index",
              "interference_score", "disclosure_comfort_score",
              "leave_accessibility_score", "family_risk_flag")

hist_list <- map(num_cols, function(col) {
  mean_val <- mean(df[[col]], na.rm = TRUE)
  ggplot(df, aes(x = .data[[col]])) +
    geom_histogram(aes(y = after_stat(density)), bins = 15,
                    fill = "#8dd3c7", color = "black") +
    geom_density(color = "black", linewidth = 1) +
    geom_vline(xintercept = mean_val, color = "red", linetype = "dashed") +
    annotate("text", x = mean_val, y = Inf, vjust = 2,
             label = paste("Mean =", round(mean_val, 2)), color = "red") +
    labs(title = paste("Distribution of", col), x = col, y = "Density") +
    theme_minimal()
})
# wrap_plots(hist_list, ncol = 3)

# Observations:
# - Age concentrated late-20s to mid-30s, slight right skew.
# - Employer Support Index mostly low-to-mid: strong support is uncommon.
# - Stigma Index concentrated at lower values; smaller group faces high stigma.
# - Interference Score spread across levels, a notable group at higher end.
# - Disclosure Comfort Score centered mid-range — mixed comfort levels.
# - Leave Accessibility moderate overall — not uniformly easy.
# - Family Risk Flag: majority no family history, but a substantial minority does.

# ---- 2. Bivariate Analysis — Box Plots ----

# GROUP 1 — KPI Scores (0-1 scale) vs Treatment
kpi_0_1 <- c("employer_support_index", "stigma_index", "disclosure_comfort_score")

df_melt1 <- df %>%
  select(all_of(kpi_0_1), treatment) %>%
  pivot_longer(cols = all_of(kpi_0_1), names_to = "KPI", values_to = "Score")

means1 <- df_melt1 %>% group_by(KPI, treatment) %>% summarise(mean_val = mean(Score, na.rm = TRUE), .groups = "drop")

ggplot(df_melt1, aes(x = KPI, y = Score, fill = treatment)) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.5) +
  geom_point(data = means1, aes(x = KPI, y = mean_val, group = treatment),
             position = position_dodge(width = 0.6), shape = 18, size = 3, color = "red") +
  scale_fill_manual(values = c("no" = "#6A0DAD", "yes" = "#DAA520")) +
  labs(title = "KPI Scores (0-1 Scale) vs Treatment", x = "KPI Column", y = "Score (0 to 1)") +
  theme_minimal()

# Insights:
# - Employer Support Index: treatment=yes group has lower mean (~0.41 vs 0.48)
#   -> employees needing treatment tend to have less workplace support,
#   though overlap is large so this alone isn't a perfect separator.
# - Stigma Index: treatment=yes group has LOWER mean (~0.21 vs 0.34) —
#   counter to the naive assumption; does not directly support "more stigma
#   -> more treatment" in this KPI comparison.
# - Disclosure Comfort: nearly identical (~0.51 vs 0.52) — weak standalone
#   differentiator.

# GROUP 2 — Interference & Leave Accessibility vs Treatment
kpi_scores <- c("interference_score", "leave_accessibility_score")

df_melt3 <- df %>%
  select(all_of(kpi_scores), treatment) %>%
  pivot_longer(cols = all_of(kpi_scores), names_to = "KPI", values_to = "Score")

means3 <- df_melt3 %>% group_by(KPI, treatment) %>% summarise(mean_val = mean(Score, na.rm = TRUE), .groups = "drop")

ggplot(df_melt3, aes(x = KPI, y = Score, fill = treatment)) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.5) +
  geom_point(data = means3, aes(x = KPI, y = mean_val, group = treatment),
             position = position_dodge(width = 0.6), shape = 18, size = 3, color = "red") +
  scale_fill_manual(values = c("no" = "#6A0DAD", "yes" = "#DAA520")) +
  labs(title = "Interference & Leave Accessibility vs Treatment", x = "KPI Column", y = "Score") +
  theme_minimal()

# Observations:
# - Interference score shows the strongest split (treatment=yes mean ~2.51
#   vs treatment=no ~0.86 on the 0-3 scale — the biggest driver of treatment need).
# - Leave accessibility is similar across both groups — weak influence.

# ---- 3. Multivariate Analysis ----

# Correlation Heatmap
kpi_cols <- c("employer_support_index", "stigma_index", "interference_score",
              "disclosure_comfort_score", "leave_accessibility_score", "family_risk_flag")

corr_matrix <- cor(df[kpi_cols], use = "pairwise.complete.obs")
corrplot(corr_matrix, method = "color", type = "full",
         addCoef.col = "black", tl.col = "black", tl.srt = 0,
         col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
         title = "Correlation Heatmap — KPI Columns", mar = c(0, 0, 2, 0))

# Observations:
# - stigma_index vs disclosure_comfort_score: strongest negative correlation
#   -> higher stigma means less comfort disclosing mental health.
# - stigma_index vs leave_accessibility_score: negative -> high-stigma
#   workplaces also make leave harder to access.
# - interference_score vs family_risk_flag: positive -> family history
#   linked to more work interference.
# - employer_support_index vs leave_accessibility_score: positive ->
#   supportive companies also provide easier leave access.
# - All other correlations are low -> no multicollinearity issue; each KPI
#   captures a distinct dimension.

# Pair Plot
df$treatment_encoded <- ifelse(df$treatment == "yes", 1, 0)
pair_cols <- c("employer_support_index", "stigma_index", "interference_score",
               "disclosure_comfort_score", "family_risk_flag", "treatment_encoded")

pair_df <- df %>% select(all_of(pair_cols)) %>% drop_na() %>%
  mutate(treatment_encoded = factor(treatment_encoded, labels = c("No", "Yes")))

ggpairs(pair_df, columns = 1:5, aes(color = treatment_encoded, alpha = 0.5)) +
  labs(title = "Pair Plot — KPI Columns by Treatment") +
  theme_minimal()

# Observations:
# - interference_score density shows clearest separation — treatment=yes
#   peaks at higher interference values, confirming it as the strongest predictor.
# - family_risk_flag density: treatment=yes group visibly shifted toward 1
#   -> family history strongly associated with treatment need.
# - stigma_index density: treatment=no group slightly higher -> high-stigma
#   employees avoid seeking treatment.
# - employer_support_index vs stigma_index scatter: negative trend -> low
#   support companies have higher stigma.

# ---- 4. Violin Plots ----

p1 <- ggplot(df, aes(x = treatment, y = employer_support_index, fill = treatment)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  scale_fill_manual(values = c("no" = "#003366", "yes" = "#CC5500")) +
  labs(title = "Employer Support Index by Treatment") +
  theme_minimal()

p2 <- ggplot(df, aes(x = treatment, y = stigma_index, fill = treatment)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  scale_fill_manual(values = c("no" = "#003366", "yes" = "#CC5500")) +
  labs(title = "Stigma Index by Treatment") +
  theme_minimal()

p3 <- ggplot(df, aes(x = Gender, y = interference_score, fill = Gender)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  scale_fill_manual(values = c("Male" = "#003366", "Female" = "#CC5500", "Other" = "#2E8B57")) +
  labs(title = "Interference Score by Gender") +
  theme_minimal()

# wrap_plots(p1, p2, p3, ncol = 3)

# Observations:
# - Employer support violin: treatment=yes group has a wider spread at lower
#   support values -> poor support consistently linked to treatment need.
# - Stigma index violin: treatment=no group has a heavier upper tail ->
#   high-stigma employees cluster in the no-treatment group, supporting the
#   idea that stigma blocks treatment-seeking.
# - Gender vs interference: Male violin wider at mid-range (1-2); Female
#   violin shows more concentration at higher interference -> females
#   experience more intense work interference despite being a smaller group.

# ---- 5. Stacked Bar — Work Interference vs Treatment ----

interfere_treat_pct <- df %>%
  count(work_interfere, treatment) %>%
  group_by(work_interfere) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(work_interfere = factor(work_interfere,
                                   levels = c("never", "not applicable", "rarely", "sometimes", "often")))

ggplot(interfere_treat_pct, aes(x = work_interfere, y = pct, fill = treatment)) +
  geom_col(width = 0.6, color = "white") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  scale_fill_manual(values = c("no" = "#003366", "yes" = "#CC5500")) +
  labs(title = "Treatment Need by Work Interference Level",
       x = "Work Interference Level", y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Observations:
# - "Often" interference group has the highest treatment=yes rate -> severe
#   interference strongly drives treatment-seeking.
# - "Never" group is almost entirely treatment=no -> no interference, no need.
# - "Sometimes" group is close to a 50-50 split -> the most at-risk
#   undecided group.
# - "Not applicable" group is overwhelmingly treatment=no, as expected.

################################################################################
# KEY INSIGHTS
################################################################################
# 1. Family history is a strong risk marker for treatment need.
# 2. Work interference is the strongest practical driver in the analysis.
# 3. Difficulty taking leave is linked to higher treatment need.
# 4. Stigma and fear of consequences are closely associated with treatment need.
# 5. Lower employer support is associated with higher treatment need.
# 6. Disclosure comfort is lower in more stigma-prone environments.
# 7. Gender-based differences are visible but should be interpreted cautiously
#    (the "Other" category has very few observations).

################################################################################
# BUSINESS RECOMMENDATIONS
################################################################################
# 1. Identify and support employees showing work interference early
#    (wellness check-ins, manager observation frameworks, early referrals).
# 2. Reduce stigma through manager training and safe/anonymous reporting
#    channels.
# 3. Simplify and clearly communicate mental health leave policy to remove
#    the "don't know" barrier.

################################################################################
# FINAL VERDICT
################################################################################
# Employees most likely to report a need for mental health treatment show
# stronger work interference, lower workplace support, greater difficulty
# around leave access, and stigma/fear-linked responses. Treatment need is
# associated with both personal risk factors (family history) and workplace
# environment factors (interference, support, stigma).

################################################################################
# LIMITATIONS
################################################################################
# - Survey data spans only 3 days (Aug 27-29, 2014); may not reflect
#   longer-term workplace patterns.
# - Dataset lacks workload, absenteeism, job satisfaction, and performance
#   variables that would allow deeper causal analysis.
# - Findings show associations, not direct cause-and-effect relationships.
