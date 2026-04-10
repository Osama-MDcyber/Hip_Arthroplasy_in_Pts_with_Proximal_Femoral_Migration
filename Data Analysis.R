
source(file = "Data Upload Clean.R")
library(gtsummary)
library(broom)
library(rstatix)
library(flextable)
library(patchwork)
library(glue)
library(ggsignif)
library(ggdist)

  # ============================================================
  # 3. NORMALITY TESTING FUNCTION (DIAGNOSTIC TOOL)
  # ============================================================

# This function generates:
# - Histogram
# - Density plot
# - QQ plot
# - Shapiro-Wilk test
# Useful for deciding between parametric vs non-parametric tests

Normality_Testing <- function(data, x) {
  p1 <- data %>%
    ggplot(aes(x = {{x}})) +
    geom_histogram() +
    ggtitle("Histogram")
  
  p2 <- data %>%
    ggplot(aes(x = {{x}})) +
    geom_density() +
    ggtitle("Density Plot")
  
  p3 <- data %>%
    ggplot(aes(sample = {{x}})) +
    geom_qq() +
    geom_qq_line() +
    ggtitle("Q-Q Plot")
  
  # Combine plots for visual diagnostics
  print(p1 / p2 / p3)
  
  # Shapiro-Wilk normality test
  data %>% shapiro_test({{x}})
}

# Apply normality test to Age variable
Arthroplasty_Data %>%
  Normality_Testing(Age)


# ============================================================
# 4. DESCRIPTIVE STATISTICS (BASELINE CHARACTERISTICS)
# ============================================================

# Summary table for demographic and clinical variables
Arthroplasty_Data %>%
  select(Age, Gender, Side, Complications, `Acetabular reconstruction`) %>%
  tbl_summary(
    statistic = Age ~ "{mean} ± ({sd})"
  ) %>%
  bold_labels() %>%
  as_flex_table()


# ============================================================
# 5. PRE/POST LONGITUDINAL ANALYSIS (LLD & FD)
# ============================================================

# Reshape data from wide to long format for repeated measures analysis
Arthroplasty_Data %>%
  select(starts_with("LLD"), starts_with("FD"), PatientID) %>%
  pivot_longer(
    cols = -PatientID,
    names_to = c("Measure", "Time"),
    names_sep = " ",
    values_to = "Value"
  ) %>%
  mutate(Time = factor(Time, levels = c("Preoperative", "Postoperative"))) %>%
  pivot_wider(names_from = Measure, values_from = Value) %>%
  
  # Summary table by time point
  tbl_summary(
    by = Time,
    include = c(LLD, FD),
    label = list(
      LLD ~ "Limb Length Discrepancy",
      FD ~ "Flexion Deformity"
    )
  ) %>%
  
  # Paired non-parametric test for repeated measures
  add_p(
    test = list(all_continuous() ~ "paired.wilcox.test"),
    group = PatientID
  ) %>%
  add_ci() %>%
  bold_labels() %>%
  as_flex_table()


# ============================================================
# 6. HARRIS HIP SCORE (HHS) LONGITUDINAL PREPARATION
# ============================================================

# Rename time points for clarity
Arthroplasty_Data_for_long <- Arthroplasty_Data %>%
  rename(
    "Preoperative" = `HHS Preoperative`,
    "Post 6 weeks" = `HHS post 1 (6w)`,
    "Post 6 months" = `HHS post 2 (6m)`,
    "Post 1 year" = `HHS post 3 (1y)`
  )

# Convert dataset to long format for repeated measures analysis
data_long <- Arthroplasty_Data_for_long %>%
  select(PatientID, Preoperative, `Post 6 weeks`,
         `Post 6 months`, `Post 1 year`) %>%
  pivot_longer(
    cols = c(Preoperative, `Post 6 weeks`,
             `Post 6 months`, `Post 1 year`),
    names_to = "TimePoint",
    values_to = "HHS_Score"
  ) %>%
  mutate(
    TimePoint = factor(TimePoint, levels = c(
      "Preoperative", "Post 6 weeks",
      "Post 6 months", "Post 1 year"
    ))
  )


# ============================================================
# 7. ASSUMPTION CHECKS FOR RM-ANOVA
# ============================================================

# Outlier detection per time point
data_long %>%
  group_by(TimePoint) %>%
  identify_outliers(HHS_Score)

# Normality testing per time point
data_long %>%
  group_by(TimePoint) %>%
  shapiro_test(HHS_Score)

# Visual normality check (QQ plot per time point)
data_long %>%
  ggplot(aes(sample = HHS_Score)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~TimePoint)


# ============================================================
# 8. REPEATED MEASURES ANOVA
# ============================================================

# Perform repeated measures ANOVA
repeaetd_measures_anova <- data_long %>%
  anova_test(
    dv = HHS_Score,
    wid = PatientID,
    within = TimePoint
  )

# Extract ANOVA table
table_repeated_anova <- get_anova_table(repeaetd_measures_anova)


# ============================================================
# 9. DESCRIPTIVE TABLE FOR HHS
# ============================================================

# Summary of HHS scores over time
data_long %>%
  tbl_summary(
    by = TimePoint,
    include = HHS_Score,
    statistic = list(all_continuous() ~ "{mean} ± {sd}"),
    label = list(HHS_Score ~ "HHS Score")
  ) %>%
  modify_header(label ~ "**Outcome**") %>%
  modify_table_body(~ .x %>%
                      mutate(p_value = style_pvalue(table_repeated_anova$p))) %>%
  modify_header(p_value ~ "**p-value**") %>%
  bold_labels() %>%
  as_flex_table()


# ============================================================
# 10. POST-HOC ANALYSIS (PAIRED COMPARISONS)
# ============================================================

# Pairwise comparisons between time points
posthoc_result <- data_long %>%
  pairwise_t_test(
    HHS_Score ~ TimePoint,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  )

# Format post-hoc results into publication-ready table
posthoc_result %>%
  mutate(
    HHS_Comparison = paste(group1, "vs", group2),
    p.adj = gtsummary::style_pvalue(p.adj)
  ) %>%
  select(HHS_Comparison, p.adj) %>%
  flextable() %>%
  set_header_labels(
    HHS_Comparison = "Harris Hip Score Pairwise Comparison",
    p.adj = "Adjusted p-value"
  ) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  autofit()


# ============================================================
# 11. PELVIC OBLIQUITY (PAIRED BINARY OUTCOME)
# ============================================================

# Prepare paired binary outcome for McNemar test
pelvic_obl_long <- Arthroplasty_Data %>%
  select(PatientID,
         `Pelvic_Obliquity Preoperative`,
         `Pelvic_Obliquity Postoperative`) %>%
  rename(
    Preoperative = `Pelvic_Obliquity Preoperative`,
    Postoperative = `Pelvic_Obliquity Postoperative`
  ) %>%
  pivot_longer(
    cols = c(Preoperative, Postoperative),
    names_to = "Time",
    values_to = "Pelvic Obliquity"
  ) %>%
  mutate(
    `Pelvic Obliquity` = factor(
      `Pelvic Obliquity`,
      levels = c(0,1),
      labels = c("No", "Yes")
    )
  )

# Summary table + McNemar test for paired proportions
pelvic_obl_long %>%
  tbl_summary(
    by = Time,
    include = `Pelvic Obliquity`,
    type = list(`Pelvic Obliquity` ~ "categorical")
  ) %>%
  add_p(
    test = list(`Pelvic Obliquity` ~ "mcnemar.test"),
    group = PatientID
  ) %>%
  bold_labels() %>%
  as_flex_table()


# ============================================================
# 12. LONGITUDINAL PLOT (HHS)
# ============================================================

# Spaghetti plot with mean trend and 95% CI
ggplot(data_long, aes(x = TimePoint, y = HHS_Score, fill = TimePoint)) +
  
  # Individual patient trajectories
  geom_line(
    aes(group = PatientID),
    alpha = 0.25,
    linewidth = 0.6,
    colour = "gray40"
  ) +
  
  # Raw data points
  geom_jitter(
    alpha = 0.4,
    color = "#2C7BB6",
    size = 2,
    width = 0.2
  ) +
  
  # Mean trend line
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    linewidth = 1.3,
    colour = "black"
  ) +
  
  # 95% confidence interval
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    width = 0.15,
    linewidth = 0.8,
    colour = "black"
  ) +
  
  labs(
    x = "Time Point",
    y = "Harris Hip Score"
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text = element_text(color = "black")
  )
