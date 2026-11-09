# Cardiovascular Outcome Reporting in Psychiatric RCTs

Analysis code for a manuscript on how often randomized controlled trials (RCTs) of psychiatric drugs report **cardiovascular (CV) outcomes**. It compares antidepressant-only and antipsychotic-only trials and examines reporting by geographic region, publication year (2015–2024), and trial sample size.

> **Author:** Takunda Chikuvire, Cornell University

---

## Research questions

| # | Question | Test |
|---|----------|------|
| 1 | Does the proportion of trials reporting any CV outcome differ by drug class? | Chi-square (2×2), with Fisher's exact odds ratio |
| 2 | Among reporting trials, does the *number* of CV outcomes differ by drug class? | Mann–Whitney U (Wilcoxon rank-sum) |
| 3 | Is trial sample size associated with the number of CV outcomes reported? | Spearman's ρ |
| 4 | Does CV reporting differ by geographic region? | Fisher's exact test |
| 5 | Is drug class associated with CV reporting after adjusting for year and sample size? | Firth penalized logistic regression |
| 6 | Did CV reporting change over 2015–2024? | Logistic regression (likelihood-ratio test); Cochran–Armitage trend test as a sensitivity check |

A power analysis reports the minimum effect sizes detectable at 80% power for tests 1–3.

**Analytic sample:** only trials classified as *Antidepressant only* or *Antipsychotic only*. Combination trials (*Antipsychotic and Antidepressant*) are excluded from all figures and tests.

---

## Repository contents

```
.
├── README.md                      ← this file
├── psych_cv_poster_analysis.R     ← full analysis: data cleaning, figures, tests, power
├── psych-cv project.csv           ← extracted trial data 
├── figures/                       ← figures
└── .gitignore
```

---

## How to reproduce the results

1. **Install R** (version 4.3 or later recommended) and, optionally, RStudio.
2. **Clone or download** this repository.
3. **Install the required packages** once:
   ```r
   install.packages(c("tidyverse", "scales", "patchwork", "logistf", "pwr"))
   ```
4. **Set the working directory** to the repository folder, so the script can find the CSV.
   In RStudio: *Session → Set Working Directory → To Source File Location*.
5. **Run the whole script:**
   ```r
   source("psych_cv_poster_analysis.R")
   ```

### Output
- `Figure2_drug_class.png`: Figure 2, drug class (A: CV outcomes reported, B: primary vs. secondary outcomes, C: number of CV outcomes per trial)
- `Figure3_trial_characteristics.png`: Figure 3, other trial characteristics (A: geographic region, B: publication year, C: sample size)
- Test results, the summary table, the power analysis, and package versions are printed to the console.

The script sets a random seed (`set.seed(2025)`), so the jittered points in the plots are identical on every run.

---

## Script structure

| Section | Contents |
|---------|----------|
| 1. Setup | Load packages, set random seed |
| 2. Data | Read CSV, fix a spelling error in drug class names, restrict to the two single-drug classes |
| 3. Plot styling | Shared poster theme, colors, legend and panel-label styles |
| 4. Descriptive figures | Disease category, drug class, Figure 2 (A–C) and Figure 3 (A–C) |
| 5. Inferential tests | Tests 1–6 and a summary table |
| 6. Power analysis | Minimum detectable effects for tests 1–3 |
| 7. Session info | R and package versions |

---

## Data

Each row of `psych-cv project.csv` is one trial. The script uses these columns:

| Column | Description |
|--------|-------------|
| `Drug.Classification` | Antidepressant only / Antipsychotic only / Antipsychotic and Antidepressant |
| `Disease.Category` | Psychiatric condition studied |
| `Cardiovascular.Outcomes.Reported` | Yes / No |
| `Number.of.Cardiovascular.outcomes.reported` | Count of CV outcomes (blank = 0) |
| `Primary.Cardiovascular.Outcomes` | Primary CV outcomes reported (blank / NA = none) |
| `Secondary.Outcomes` | Secondary outcomes reported (blank / NA = none) |
| `Geographic.Region` | Region where the trial was conducted |
| `Publication.Year` | Year of publication |
| `Total.N.in.Trial` | Total number of participants randomized |

**Data-cleaning notes (handled in the script):**
- The raw data spells "Antipyschotic"; the script corrects this to "Antipsychotic".
- *Other*, *Multiple Locations*, *Africa* and *Australia* are combined into one **Other** region for Figure 3A and Test 4. *Not Reported* regions are excluded.
- Leading and trailing spaces are trimmed from category values.

---

## Citation

If you use this code, please cite:

> Takunda Chikuvire (2026). *Cardiovascular outcome reporting in psychiatric randomized controlled trials* [Computer software]. GitHub. https://github.com/TakundaChikuvire/psych-cv-reporting 

---

## License


