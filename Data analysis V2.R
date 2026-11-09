# =====================================================================
#  Cardiovascular (CV) outcome reporting in psychiatric RCTs
#  Figures and statistical tests for the poster
# =====================================================================
#
#  WHAT THIS SCRIPT DOES (runs top to bottom, in order)
#    1. Setup ............ packages, random seed
#    2. Data ............. read the CSV, fix spelling, keep the 2 drug classes
#    3. Plot styling ..... shared theme, colors, legend style
#    4. Descriptive figures
#         4.1  Studies by disease category
#         4.2  Studies by drug class
#         Figure 2 — Drug class
#           4.3  (A) CV outcomes reported, Yes / No
#           4.4  (B) Primary vs secondary CV outcomes
#           4.5  (C) Number of CV outcomes per trial (box plot)
#           4.6  Figure 2 panel
#         Figure 3 — Other trial characteristics
#           4.7  (A) CV reporting by geographic region
#           4.8  (B) CV reporting by publication year, 2015–2024
#           4.9  (C) Trial sample size vs number of CV outcomes
#           4.10 Figure 3 panel
#    5. Inferential tests (1–6) and summary table
#    6. Power analysis (minimum detectable effects)
#    7. Session info (package versions, for reproducibility)
#
#  HOW TO RUN
#    - Put "psych-cv project.csv" in the same folder as this script and set
#      that folder as the working directory (RStudio: Session > Set Working
#      Directory > To Source File Location).
#    - Install any missing packages once (see section 1), then run the whole file.
#    - Output: two PNG files (Figure2_drug_class.png, Figure3_trial_characteristics.png)
#      are saved to the working directory; everything else prints to the console.
#
#  KEY COLUMNS USED FROM THE CSV
#    Drug.Classification, Disease.Category, Cardiovascular.Outcomes.Reported,
#    Number.of.Cardiovascular.outcomes.reported, Primary.Cardiovascular.Outcomes,
#    Secondary.Outcomes, Geographic.Region, Publication.Year, Total.N.in.Trial
# =====================================================================


# =====================================================================
# 1. SETUP
# =====================================================================
# Install once if needed:
# install.packages(c("tidyverse", "scales", "patchwork", "logistf", "pwr"))

library(dplyr); library(tidyr); library(ggplot2)
library(scales); library(forcats)
library(patchwork)   # combining plots into multi-panel figures
library(tidyverse)   # used in the inferential-test section (includes readr)
library(logistf)     # Firth penalized logistic regression (test 5)
library(pwr)         # power analysis (section 6)

# Fixes the random jitter of points in the box plot and scatter plot,
# so every run (and every reader) gets an identical figure.
set.seed(2025)


# =====================================================================
# 2. DATA
# =====================================================================
cv <- read.csv("psych-cv project.csv")

# The raw data spells "Antipyschotic"; correct it so class names match below.
cv$Drug.Classification <- trimws(gsub("Antipyschotic", "Antipsychotic", cv$Drug.Classification))

# Analytic sample: single-drug trials only.
# "Antipsychotic and Antidepressant" (combination) trials are excluded from every plot and test.
classes <- c("Antidepressant only", "Antipsychotic only")

cv2 <- cv %>%
  filter(Drug.Classification %in% classes) %>%
  mutate(Drug.Classification = factor(Drug.Classification, levels = classes))
print(table(cv2$Drug.Classification))   # should show only the 2 classes


# =====================================================================
# 3. PLOT STYLING (shared by all figures)
# =====================================================================
# Poster theme: bold Arial text, clean axes
theme_poster <- theme_classic(base_size = 16) +
  theme(text = element_text(family = "Arial", face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text = element_text(face = "bold"),
        legend.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

# Optional: 45-degree x labels; hjust/vjust = 1 anchors the end of each label under its bar.
# (Currently switched off in the plots below — remove the "#" before "+ slant_x" to use it.)
slant_x <- theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, lineheight = 0.9))

# Drug-class colours
class_cols <- c("Antidepressant only" = "royalblue", "Antipsychotic only" = "darkred")

# Compact legend: small keys, one row, sits above the plot
cute_legend <- theme(legend.position = "top",
                     legend.direction = "horizontal",
                     legend.title = element_text(size = 13, face = "bold"),
                     legend.text  = element_text(size = 13),
                     legend.key.size = unit(0.45, "cm"),
                     legend.margin = margin(0, 0, -6, 0))

# Style of the A / B / C panel labels in the multi-panel figures
tag_style <- theme(plot.tag = element_text(family = "Arial", face = "bold", size = 22))


# =====================================================================
# 4. DESCRIPTIVE FIGURES
# =====================================================================

# ---------- 4.1 Studies by disease category ----------
df_dc <- cv %>%
  filter(Drug.Classification %in% classes) %>%   # exclude combination trials
  mutate(Disease.Category = trimws(Disease.Category)) %>%
  filter(!is.na(Disease.Category), Disease.Category != "") %>%
  count(Disease.Category, name = "n")
print(df_dc)

fig_dc <- ggplot(df_dc, aes(Disease.Category, n, fill = Disease.Category)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("n = ", n)), vjust = -0.4, size = 5,
            family = "Arial", fontface = "bold") +
  scale_fill_manual(values = c("#1B9E77", "#7570B3", "royalblue")) +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Disease Category", y = "Number of Studies") +
  theme_poster + theme(legend.position = "none") #+ slant_x
fig_dc

# ---------- 4.2 Studies by drug class ----------
df_dc2 <- cv2 %>% count(Drug.Classification, name = "n")

fig_drug2 <- ggplot(df_dc2, aes(Drug.Classification, n, fill = Drug.Classification)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("n = ", n)), vjust = -0.4, size = 5,
            family = "Arial", fontface = "bold") +
  scale_fill_manual(values = class_cols) +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Drug Classification", y = "Number of Studies") +
  theme_poster + theme(legend.position = "none") #+ slant_x
fig_drug2


# ---------------------------------------------------------------------
# FIGURE 2 — DRUG CLASS
# ---------------------------------------------------------------------

# ---------- 4.3 Figure 2A: CV outcomes reported (Yes / No) by drug class ----------
df_dc2_cv <- cv2 %>%
  mutate(Cardiovascular.Outcomes.Reported = trimws(Cardiovascular.Outcomes.Reported)) %>%
  filter(Cardiovascular.Outcomes.Reported %in% c("Yes", "No")) %>%
  mutate(Cardiovascular.Outcomes.Reported =
           factor(Cardiovascular.Outcomes.Reported, levels = c("Yes", "No"))) %>%
  count(Drug.Classification, Cardiovascular.Outcomes.Reported, name = "n") %>%
  complete(Drug.Classification, Cardiovascular.Outcomes.Reported, fill = list(n = 0)) %>%
  mutate(fill_key = paste0(Drug.Classification, ": ", Cardiovascular.Outcomes.Reported))
print(df_dc2_cv)

fig_drug2_cv <- ggplot(df_dc2_cv, aes(Drug.Classification, n,
                                      fill = Cardiovascular.Outcomes.Reported)) +
  geom_col(position = position_dodge(0.9)) +
  geom_text(aes(label = paste0("n = ", n)), position = position_dodge(0.9),
            vjust = -0.4, size = 5, family = "Arial", fontface = "bold") +
  scale_fill_manual(values = c(Yes = "darkred", No = "lightblue"), name = "CV outcomes") +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Drug Classification", y = "Number of Studies") +
  theme_poster + cute_legend #+ slant_x
fig_drug2_cv

# ---------- 4.4 Figure 2B: Primary vs secondary CV outcomes by drug class ----------
# A study counts as reporting an outcome type if that cell is not NA / blank.
# One study can count in both bars (if it reports primary AND secondary outcomes).
df_dc2_ps <- cv2 %>%
  mutate(Primary   = !is.na(Primary.Cardiovascular.Outcomes) &
           trimws(as.character(Primary.Cardiovascular.Outcomes)) != "",
         Secondary = !is.na(Secondary.Outcomes) &
           trimws(as.character(Secondary.Outcomes)) != "") %>%
  group_by(Drug.Classification) %>%
  summarise(Primary = sum(Primary), Secondary = sum(Secondary), .groups = "drop") %>%
  pivot_longer(c(Primary, Secondary), names_to = "Outcome.Type", values_to = "n") %>%
  mutate(fill_key = paste0(Drug.Classification, ": ", Outcome.Type))
print(df_dc2_ps)

fig_drug2_ps <- ggplot(df_dc2_ps, aes(Drug.Classification, n, fill = Outcome.Type)) +
  geom_col(position = position_dodge(0.9)) +
  geom_text(aes(label = paste0("n = ", n)), position = position_dodge(0.9),
            vjust = -0.4, size = 5, family = "Arial", fontface = "bold") +
  scale_fill_manual(values = c(Primary = "darkred", Secondary = "lightblue"), name = NULL) +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Drug Classification", y = "Number of Studies") +
  theme_poster + cute_legend #+ slant_x
fig_drug2_ps

# ---------- 4.5 Figure 2C: Number of CV outcomes per trial (box plot) ----------
# FALSE = only trials that reported at least 1 CV outcome
# TRUE  = all trials, with non-reporting trials plotted at 0
include_zeros <- FALSE

df_box2 <- cv2 %>%
  mutate(n_cv = coalesce(as.numeric(Number.of.Cardiovascular.outcomes.reported), 0)) %>%
  filter(include_zeros | n_cv > 0)

print(df_box2 %>% group_by(Drug.Classification) %>%
        summarise(trials = n(), median = median(n_cv),
                  Q1 = quantile(n_cv, 0.25), Q3 = quantile(n_cv, 0.75)))

fig_box2 <- ggplot(df_box2, aes(Drug.Classification, n_cv, fill = Drug.Classification)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.7) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5, color = "darkgrey", size = 2) +
  scale_fill_manual(values = class_cols) +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  labs(x = "Drug Classification", y = "Number of CV Outcomes Reported per Trial") +
  theme_poster + theme(legend.position = "none") #+ slant_x
fig_box2

# ---------- 4.6 Figure 2 panel (A | B | C) ----------
fig2_panel <- (fig_drug2_cv | fig_drug2_ps | fig_box2) +
  plot_annotation(tag_levels = "A") & tag_style
fig2_panel
ggsave("Figure2_drug_class.png", fig2_panel, width = 18, height = 6.5, dpi = 300, bg = "white")


# ---------------------------------------------------------------------
# FIGURE 3 — OTHER TRIAL CHARACTERISTICS
# ---------------------------------------------------------------------

# ---------- 4.7 Figure 3A: CV outcomes reported (Yes / No) by geographic region ----------
df_reg_yn <- cv2 %>%
  mutate(Geographic.Region = trimws(Geographic.Region),
         Cardiovascular.Outcomes.Reported = trimws(Cardiovascular.Outcomes.Reported),
         # combine small regions into one "Other" group (case-insensitive match)
         Geographic.Region = if_else(
           tolower(Geographic.Region) %in% c("other", "multiple locations", "africa", "australia"),
           "Other", Geographic.Region)) %>%
  filter(!is.na(Geographic.Region), Geographic.Region != "",
         Geographic.Region != "Not Reported",
         Cardiovascular.Outcomes.Reported %in% c("Yes", "No")) %>%
  mutate(Cardiovascular.Outcomes.Reported =
           factor(Cardiovascular.Outcomes.Reported, levels = c("Yes", "No"))) %>%
  count(Geographic.Region, Cardiovascular.Outcomes.Reported, name = "n") %>%
  complete(Geographic.Region, Cardiovascular.Outcomes.Reported, fill = list(n = 0)) %>%
  # order regions by total number of studies (largest first), "Other" last
  mutate(Geographic.Region = fct_reorder(Geographic.Region, n, .fun = sum, .desc = TRUE),
         Geographic.Region = fct_relevel(Geographic.Region, "Other", after = Inf))
print(df_reg_yn)

fig_reg_yn <- ggplot(df_reg_yn, aes(Geographic.Region, n, fill = Cardiovascular.Outcomes.Reported)) +
  geom_col(position = position_dodge(0.9)) +
  geom_text(aes(label = paste0("n = ", n)), position = position_dodge(0.9),
            vjust = -0.4, size = 4, family = "Arial", fontface = "bold") +
  scale_fill_manual(values = c(Yes = "darkred", No = "lightblue"),
                    name = "CV Outcomes Reported") +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(breaks = pretty_breaks(),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Region", y = "Number of Studies") +
  theme_poster + #slant_x +
  theme(legend.position = "inside", legend.position.inside = c(0.75, 0.85))
fig_reg_yn

# ---------- 4.8 Figure 3B: Proportion of studies reporting CV outcomes, 2015–2024 ----------
dat_trend <- cv2 %>%
  mutate(Year = as.integer(Publication.Year),
         Cardiovascular.Outcomes.Reported = trimws(Cardiovascular.Outcomes.Reported)) %>%
  filter(between(Year, 2015, 2024),
         Cardiovascular.Outcomes.Reported %in% c("Yes", "No")) %>%
  mutate(y = as.integer(Cardiovascular.Outcomes.Reported == "Yes"),
         Year_c = Year - 2015)

df_year <- dat_trend %>%
  group_by(Year) %>%
  summarise(prop_yes = mean(y), n = n(), .groups = "drop")
print(df_year)

# Logistic-regression trend line with 95% CI band
m_trend <- glm(y ~ Year_c, family = binomial, data = dat_trend)
pred_grid <- tibble::tibble(Year = 2015:2024, Year_c = Year - 2015)
pl <- predict(m_trend, newdata = pred_grid, type = "link", se.fit = TRUE)
pred_grid <- pred_grid %>%
  mutate(fit  = plogis(pl$fit),
         low  = plogis(pl$fit - 1.96 * pl$se.fit),
         high = plogis(pl$fit + 1.96 * pl$se.fit))

fig_trend <- ggplot(df_year, aes(Year, prop_yes)) +
  geom_ribbon(data = pred_grid, aes(x = Year, ymin = low, ymax = high),
              inherit.aes = FALSE, fill = "palegreen", alpha = 0.2) +
  geom_line(data = pred_grid, aes(x = Year, y = fit), inherit.aes = FALSE,
            color = "royalblue", linewidth = 1, linetype = "dashed") +
  geom_line(color = "darkred", linewidth = 1.2) +
  geom_point(aes(size = n), color = "darkred") +
  geom_text(aes(label = paste0(round(100 * prop_yes), "% ")),
            vjust = -1.4, size = 4, family = "Arial", fontface = "bold") +
  scale_size_continuous(range = c(2, 5), guide = "none") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.05),
                     breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = 2015:2024) +
  coord_cartesian(clip = "off") +
  labs(x = "Publication Year", y = "Proportion of Studies Reporting CV Outcomes") +
  theme_poster
fig_trend

# ---------- 4.9 Figure 3C: Trial sample size vs number of CV outcomes ----------
dat_sp <- cv2 %>%
  mutate(N_trial = as.numeric(gsub("[^0-9.]", "", as.character(Total.N.in.Trial))),  # strips commas etc.
         n_cv    = coalesce(as.numeric(Number.of.Cardiovascular.outcomes.reported), 0)) %>%
  filter(!is.na(N_trial), N_trial > 0)
cat("Trials in plot:", nrow(dat_sp), "\n")

# Spearman correlation (rank-based: fine for skewed counts and sample sizes)
sp <- cor.test(dat_sp$N_trial, dat_sp$n_cv, method = "spearman", exact = FALSE)
print(sp)

fig_sp <- ggplot(dat_sp, aes(N_trial, n_cv)) +
  geom_smooth(method = "loess", se = TRUE, color = "black", fill = "palegreen",
              linewidth = 1, formula = y ~ x) +
  geom_jitter(aes(color = Drug.Classification), width = 0, height = 0.1,
              size = 3, alpha = 0.75) +
  scale_color_manual(values = class_cols, name = "Drug Classification") +
  scale_x_log10(labels = comma) +
  scale_y_continuous(breaks = pretty_breaks()) +
  labs(x = "Trial Sample Size (log scale)", y = "Number of CV Outcomes Reported") +
  theme_poster +
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.9))
fig_sp

# ---------- 4.10 Figure 3 panel (A on top; B | C below) ----------
fig3_panel <- (fig_reg_yn / (fig_trend | fig_sp)) +
  plot_annotation(tag_levels = "A") & tag_style
fig3_panel
ggsave("Figure3_trial_characteristics.png", fig3_panel, width = 16, height = 12, dpi = 300, bg = "white")


# =====================================================================
# 5. INFERENTIAL TESTS
#    Uses objects created above: cv2, df_dc2_cv, df_box2, df_reg_yn
# =====================================================================

# Shared analysis dataset: one row per trial, clean yes/no outcome
dat <- cv2 %>%
  mutate(
    CV_reported = trimws(Cardiovascular.Outcomes.Reported),
    n_cv   = coalesce(as.numeric(Number.of.Cardiovascular.outcomes.reported), 0),
    TotalN = readr::parse_number(as.character(Total.N.in.Trial)),
    Year   = as.integer(Publication.Year),
    Year_c = Year - 2015
  ) %>%
  filter(CV_reported %in% c("Yes", "No")) %>%
  mutate(y = as.integer(CV_reported == "Yes"),
         Drug.Classification = droplevels(factor(Drug.Classification)))

# Check sample size parsed correctly (should be numeric, few/no NAs)
print(summary(dat$TotalN))

# ---------------------------------------------------------------
# Test 1. Drug class × any CV outcome reported — Chi-square (2×2)
# ---------------------------------------------------------------
tab1 <- xtabs(n ~ Drug.Classification + Cardiovascular.Outcomes.Reported,
              data = df_dc2_cv)
print(tab1)
t1 <- chisq.test(tab1, correct = FALSE)
print(t1)
print(t1$expected)                 # confirm all expected counts >= 5
or1 <- fisher.test(tab1)           # odds ratio + 95% CI
cat(sprintf("OR = %.2f, 95%% CI [%.2f, %.2f]\n",
            or1$estimate, or1$conf.int[1], or1$conf.int[2]))

# ---------------------------------------------------------------
# Test 2. Drug class × number of CV outcomes (reporting trials only)
#         — Mann–Whitney U / Wilcoxon rank-sum
# ---------------------------------------------------------------
t2 <- wilcox.test(n_cv ~ Drug.Classification, data = df_box2,
                  exact = FALSE, conf.int = TRUE)
print(t2)
# Effect size r = |Z| / sqrt(N)
z2 <- qnorm(t2$p.value / 2)
r2 <- abs(z2) / sqrt(nrow(df_box2))
cat(sprintf("W = %.1f, p = %.3f, r = %.2f\n", t2$statistic, t2$p.value, r2))

# ---------------------------------------------------------------
# Test 3. Sample size × number of CV outcomes — Spearman's rho
#         (all trials; non-reporting trials = 0 outcomes)
# ---------------------------------------------------------------
dat3 <- dat %>% filter(!is.na(TotalN))
t3 <- cor.test(dat3$TotalN, dat3$n_cv, method = "spearman", exact = FALSE)
print(t3)
cat(sprintf("rho = %.3f, p = %.3f, n = %d\n",
            t3$estimate, t3$p.value, nrow(dat3)))

# ---------------------------------------------------------------
# Test 4. Geographic region × any CV outcome — Fisher's exact
# ---------------------------------------------------------------
tab4 <- xtabs(n ~ Geographic.Region + Cardiovascular.Outcomes.Reported,
              data = df_reg_yn)
print(tab4)
print(suppressWarnings(chisq.test(tab4))$expected)  # shows why Fisher is needed
t4 <- fisher.test(tab4)
print(t4)
cat(sprintf("Fisher's exact p = %.3f, n = %d\n", t4$p.value, sum(tab4)))

# ---------------------------------------------------------------
# Test 5. Drug class adjusted for year and sample size
#         — Firth penalized logistic regression
# ---------------------------------------------------------------
dat5 <- dat %>% filter(!is.na(TotalN), TotalN > 0, !is.na(Year_c))
m5 <- logistf(y ~ Drug.Classification + Year_c + log(TotalN), data = dat5)
summary(m5)
res5 <- tibble(
  term = names(coef(m5)),
  OR   = exp(coef(m5)),
  low  = exp(m5$ci.lower),
  high = exp(m5$ci.upper),
  p    = m5$prob
)
print(res5)
cat(sprintf("n = %d, events (Yes) = %d, non-events (No) = %d\n",
            nrow(dat5), sum(dat5$y), sum(1 - dat5$y)))

# ---------------------------------------------------------------
# Test 6. Temporal trend 2015–2024 — logistic regression
# ---------------------------------------------------------------
dat6 <- dat %>% filter(between(Year, 2015, 2024))
m6 <- glm(y ~ Year_c, family = binomial, data = dat6)
summary(m6)
or6 <- exp(cbind(OR = coef(m6), confint(m6)))   # profile-likelihood CI
print(or6)
print(anova(m6, test = "Chisq"))                 # likelihood-ratio test vs null
# Sensitivity check: Cochran–Armitage test for trend
yr <- dat6 %>% group_by(Year) %>% summarise(yes = sum(y), n = n())
print(prop.trend.test(yr$yes, yr$n, score = yr$Year))

# ---------------------------------------------------------------
# Summary table of tests 1–6
# ---------------------------------------------------------------
summary_tbl <- tribble(
  ~Question,                          ~Test,                       ~Statistic,                            ~p,
  "1. Drug class × CV reported",      "Chi-square (2×2)",          sprintf("χ²(1) = %.2f", t1$statistic),  t1$p.value,
  "2. Drug class × no. CV outcomes",  "Mann–Whitney U",            sprintf("W = %.0f", t2$statistic),      t2$p.value,
  "3. Sample size × no. CV outcomes", "Spearman",                  sprintf("ρ = %.3f", t3$estimate),       t3$p.value,
  "4. Region × CV reported",          "Fisher's exact",            "—",                                    t4$p.value,
  "5. Drug class (adjusted)",         "Firth logistic regression", sprintf("OR = %.2f", res5$OR[2]),       res5$p[2],
  "6. Year trend",                    "Logistic regression",       sprintf("OR/yr = %.2f", or6[2, "OR"]),  summary(m6)$coefficients[2, 4]
)

print(summary_tbl)


# =====================================================================
# 6. POWER ANALYSIS — minimum detectable effects at 80% power, alpha = 0.05
# =====================================================================
# The group sizes and the 62% overall rate below are typed in by hand.
# They should match the counts printed here — if your data change, update them.
print(table(dat$Drug.Classification))                  # expected: 30 AD, 36 AP (66 total)
print(table(df_box2$Drug.Classification))              # expected: 17 AD, 24 AP reporters
cat(sprintf("Overall CV reporting rate: %.0f%%\n", 100 * mean(dat$y)))   # expected: ~62%

## 6.1 Drug class vs. any CV outcome (30 AD vs 36 AP)
h <- pwr.2p2n.test(n1 = 30, n2 = 36, sig.level = 0.05, power = 0.80)$h
h   # minimum detectable Cohen's h

# Convert h to a percentage-point difference centered on the overall 62% rate
f <- function(d) 2*asin(sqrt(0.62 + d/2)) - 2*asin(sqrt(0.62 - d/2)) - h
d <- uniroot(f, c(0.01, 0.7))$root
c(diff = d, p_low = 0.62 - d/2, p_high = 0.62 + d/2)

## 6.2 Number of CV outcomes, AD vs AP among the 41 reporters (17 vs 24)
# t-test approximation; Wilcoxon needs about 5% more trials for the same power
pwr.t2n.test(n1 = 17, n2 = 24, sig.level = 0.05, power = 0.80)$d

## 6.3 Spearman: sample size vs. number of CV outcomes (n = 66)
pwr.r.test(n = 66, sig.level = 0.05, power = 0.80)$r


# =====================================================================
# 7. SESSION INFO — R and package versions used for these results
# =====================================================================
sessionInfo()