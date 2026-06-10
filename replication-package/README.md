# README for "Building Early Support for Water Conservation Policies through Game-Based Learning"

---

## Overview

This replication package reproduces all tables and figures in the paper "Building Early Support for Water Conservation Policies through Game-Based Learning." The study is a randomized controlled trial (RCT) conducted in three primary schools in Turin, Italy, comparing the effectiveness of a board game (H2gO!) against a traditional lecture and a control condition in building water-conservation knowledge, attitudes, and behavior in students aged 8–11.

The package consists of five R scripts that run sequentially: one data-cleaning script that reads raw data and produces processed files, and four analysis scripts that each read exclusively from the processed files and write output to the `Output/` directory.

A replicator who runs the five scripts in order—on a standard desktop machine with R 4.4.3 and the listed packages installed—should be able to reproduce **all** tables and figures in the paper and appendix in **under 10 minutes**.

---

## Data Availability and Provenance Statements

The data used in this paper were collected by the authors through a field experiment carried out in three primary schools in Turin, Italy, during the 2023–24 academic year. No external or third-party datasets are used. All data are provided as part of this replication package.

The experiment involved:
- A **pre-treatment survey** on water-related behavior and attitudes (`Experiment Info/Surveys/Survey on Behavior and Attitudes (PRE).pdf`).
- Two **post-treatment survey** administered after the intervention; the first right after the intervention, the second two weeks later (`Survey on Knowledge and Mechanisms.pdf`; `Survey on Behaviors and Attitudes (POST).pdf`).
- Three treatment arms assigned at the class level: **Control**, **Lecture**, and **Game** (H2gO! board game).

Experimental materials—game rules, MVP of board and card images, lecture slides and script, school invitation letter, and authorization forms—are archived in `Experiment Info/`.

IRB approval was obtained prior to data collection. Written parental informed consent was collected for all participating students. These documents are provided in:

- `Experiment Info/IRB approval.docx.pdf`
- `Experiment Info/Authorization (format)/Consenso alla partecipazione.pdf`
- `Experiment Info/Authorization (format)/Informativa Privacy.pdf`
- `Experiment Info/Authorization (format)/Lettera di Autorizzazione Scuole.pdf`

### Statement about Rights

- [x] I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript.
- [x] I certify that the author(s) of the manuscript have documented permission to redistribute/publish the data contained within this replication package. Data were collected by the authors under IRB approval and with written parental consent. No third-party redistribution restrictions apply.

### License for Data

The data are licensed under a [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/) license. See `LICENSE.txt` for details.

### Summary of Availability

- [x] All data **are** publicly available and provided in this replication package.

### Summary of Data Files

| Data file | Source | Format | Provided |
|-----------|--------|--------|----------|
| `Data/Raw/Raw data.xlsx` | Authors (primary collection) | Excel (.xlsx), 3 sheets | Yes |
| `Data/Processed/Knowledge.csv` | Derived from `Raw data.xlsx` (sheet: *Conoscenze*) | CSV | Yes |
| `Data/Processed/Dif.csv` | Derived from `Raw data.xlsx` (sheets: *Behavior PRE*, *Behavior POST*) | CSV | Yes |
| `Data/Processed/Attitudes.csv` | Derived from `Raw data.xlsx` (sheets: *Behavior PRE*, *Behavior POST*) | CSV | Yes |
| `Data/Processed/behavior_long.csv` | Derived from `Raw data.xlsx` (sheets: *Behavior PRE*, *Behavior POST*) | CSV | Yes |

### Details on Raw Data

**`Data/Raw/Raw data.xlsx`** contains three sheets:

| Sheet | Contents | N (students) |
|-------|----------|-------------|
| *Conoscenze* | Knowledge survey responses (Q1–Q8, scored 0/1; plus demographic and cognitive controls) | 225 |
| *Behavior PRE* | Pre-treatment Likert responses on water-related behaviors and attitudes; demographic covariates | 216 |
| *Behavior POST* | Post-treatment Likert responses on behaviors and attitudes; spillover indicator | 215 |

Key variables across sheets:

- **Group / Gruppo**: treatment arm (*Controllo* = Control, *Gioco* = Game, *Lezione* = Lecture)
- **Scuola, Classe**: school and class identifiers
- **Codice**: anonymous student identifier
- **Lava denti, Lava mani, Parla genitori emergenze, Parla genitori spreco, Parla amici spreco**: five 5-point Likert behavior items (1 = never, 5 = always; "Lava denti/mani" are reverse-coded in cleaning)
- **Daresti acqua, Importanza tecnologie, Episodi eventi estremi**: three binary attitude questions (Sì/No/Don't know)
- **Parlato con altre classi** (*Behavior POST* only): indicator for spillover to other classes
- **Sesso, Età, Giochi tavolo, INTELLIGENZA, PRE-CONOSCENZA**: demographic and cognitive controls (sex, age, board-game experience, cognitive ability, self-assessed prior water knowledge)
- **Valutazione 1–8** (*Conoscenze*): binary scores for each of the 8 knowledge questions

---

## Computational Requirements

### Software Requirements

- **R 4.4.3** (2025-02-28) — the version used by the authors.

The following packages are required. Install them with `install.packages(c(...))` before running the code:

| Package | Used in script(s) | Purpose |
|---------|-------------------|---------|
| `readxl` | 01 | Read raw Excel file |
| `readr` | 01 | CSV I/O |
| `tidyverse` | 01 | Data manipulation |
| `dplyr` | 01–05 | Data wrangling |
| `tidyr` | 01–03 | Reshaping |
| `ggplot2` | 02–04 | Figures |
| `sandwich` | 02, 04 | HC-robust standard errors |
| `fixest` | 03–04 | OLS with fixed effects and clustered SEs |
| `lmtest` | 03–04 | Coefficient tests |
| `emmeans` | 03 | Post-hoc pairwise comparisons |
| `car` | 03 | ANOVA / hypothesis tests |
| `ggstatsplot` | 03–04 | Statistical visualization (Bonferroni plots) |
| `lme4` | 03 | Mixed-effects models |
| `lmerTest` | 03 | Satterthwaite p-values for mixed models |
| `nnet` | 05 | Multinomial logit |

A convenience installation script:

```r
install.packages(c(
  "readxl", "readr", "tidyverse", "dplyr", "tidyr", "ggplot2",
  "sandwich", "fixest", "lmtest", "emmeans", "car", "ggstatsplot",
  "lme4", "lmerTest", "nnet"
))
```

### Controlled Randomness

- [x] No pseudo-random number generator is used in the analysis described here. All results are fully deterministic.

### Memory, Runtime, and Storage Requirements

#### Approximate time to reproduce

- [x] < 10 minutes

Estimated on a standard 2024 laptop (Apple M-series or equivalent Intel x86-64) running macOS 14+ or comparable Linux/Windows.

#### Approximate storage space

- [x] < 25 MB (raw data ≈ 200 KB; output files ≈ 5 MB total)

#### Computational details

Code was developed and last run on an **Apple M-series laptop running macOS 14 (Sonoma) with 16 GB RAM and R 4.4.3**.

---

## Description of Programs/Code

All scripts reside in `Code/`. They must be run in the order listed. Scripts 02–05 do **not** read raw data; they read only from `Data/Processed/`.

| Script | Input | Output directory | Description |
|--------|-------|-----------------|-------------|
| `01. Data Cleaning.R` | `Data/Raw/Raw data.xlsx` | `Data/Processed/` | Reads all three Excel sheets; reverse-codes Likert items for teeth-brushing and hand-washing; creates Spillover indicator; merges demographics; exports four processed CSV files |
| `02. Descriptive.R` | `Data/Processed/Knowledge.csv`, `behavior_long.csv`, `Dif.csv` | `Output/Descriptive/` | Produces knowledge and behavior distribution figures; generates balance table |
| `03. Knowledge and mechanisms.R` | `Data/Processed/Knowledge.csv` | `Output/Results knowledge/`, `Output/Results mechanisms/` | OLS and multilevel regressions for knowledge outcomes; ANOVA, ANCOVA, Kruskal–Wallis nonparametric checks; mechanism regressions |
| `04. Behavior.R` | `Data/Processed/Dif.csv` | `Output/Results behavior/` | OLS difference-in-differences for behavior outcomes; robustness checks; Bonferroni-corrected item-level plot |
| `05. Attitudes.R` | `Data/Processed/Attitudes.csv` | `Output/Results attitudes/` | Multinomial logit DiD for three attitude outcomes; two-panel table (full sample; Lecture vs. Game) |

### Directory structure after running all scripts

```
Replication/
├── Code/
│   ├── 01. Data Cleaning.R
│   ├── 02. Descriptive.R
│   ├── 03. Knowledge and mechanisms.R
│   ├── 04. Behavior.R
│   └── 05. Attitudes.R
├── Data/
│   ├── Raw/
│   │   └── Raw data.xlsx
│   └── Processed/
│       ├── Attitudes.csv
│       ├── Dif.csv
│       ├── Knowledge.csv
│       └── behavior_long.csv
├── Experiment Info/
│   ├── Experimental Design.pdf
│   ├── IRB approval.docx.pdf
│   ├── Authorization (format)/
│   ├── Selection materials/
│   ├── Surveys/
│   └── Treatments/
│       ├── Game/
│       └── Lecture/
├── Output/
│   ├── Descriptive/
│   │   └── balance_table.tex
│   │   ├── Distrib BEHAVIOR.jpeg
│   │   └── Distrib KNOWLEDGE.jpeg
│   ├── Results attitudes/
│   │   └── attitudes_multinom_two_panel.tex
│   ├── Results behavior/
│   │   ├── behavior_questions_bonferroni_plot.pdf
│   │   ├── behavior_regression_table.tex
│   │   └── behavior_robustness_table.tex
│   ├── Results knowledge/
│   │   ├── ANCOVA_Mechanisms.pdf
│   │   ├── ancova_selfperceived_knowledge.tex
│   │   ├── ancova_tukey_knowledge.tex
│   │   ├── anova_tukey_knowledge.tex
│   │   ├── knowledge_fraction_robustness_table.tex
│   │   ├── knowledge_questions_bonferroni_plot.pdf
│   │   ├── knowledge_regression_table.tex
│   │   ├── knowledge_robustness_alternative_outcomes.tex
│   │   ├── kruskal-wallis_plot.pdf
│   │   └── multilevel_regression_knowledge.tex
│   └── Results mechanisms/
│       ├── knowledge_mechanisms_controls_table.tex
│       └── mechanisms_bonferroni_table.tex
└── Paper/
│   ├── cas-refs.bib
│   ├── Paper.pdf
│   ├── Paper.tex
│   └── Figures (non reproducible)/
```

### License for Code

The code is licensed under the [MIT License](https://opensource.org/licenses/MIT). See `LICENSE.txt` for details.

---

## Instructions to Replicators

1. **Install R 4.4.3** (or a later compatible version) from [https://cran.r-project.org](https://cran.r-project.org).

2. **Install required packages** by running the installation snippet above in an R console, or by opening `Replication.Rproj` in RStudio and running:
   ```r
   install.packages(c(
     "readxl", "readr", "tidyverse", "dplyr", "tidyr", "ggplot2",
     "sandwich", "fixest", "lmtest", "emmeans", "car", "ggstatsplot",
     "lme4", "lmerTest", "nnet"
   ))
   ```

3. **Set the working directory** to the root `Replication/` folder.

4. **Run the five scripts in order**:
   ```
   Code/01. Data Cleaning.R
   Code/02. Descriptive.R
   Code/03. Knowledge and mechanisms.R
   Code/04. Behavior.R
   Code/05. Attitudes.R
   ```
   Each script creates its own output directories if they do not already exist; no manual folder creation is needed.

5. **Output files** will appear in the directories listed in the table above. All `.tex` files are self-contained and can be included in a LaTeX document via `\input{}`. The balance table (`Output/Descriptive/balance_table.tex`) additionally requires the macros `\sym`, `\msd`, and `\estse` in the LaTeX preamble (definitions are provided as comments inside the file itself).

> **Note on `Sesso` encoding.** The variable `Female` in the balance table is constructed as `Sex %in% c("F", "Femmina")`. If the encoding in `Raw data.xlsx` differs, update the single line in `Code/02. Descriptive.R` (section 04, "Prepare dataset") before running. Use `unique(Dif$Sex)` after running `01. Data Cleaning.R` to verify.

> **Note on school name encoding.** The balance table grouping uses `c("D'Assisi", "D'Azeglio", "Rayneri")`. Run `unique(Dif$Scuola)` after step 4a to confirm exact spelling if the table shows empty columns.

> **Note on folder structure.** The directories "Output (complete)/" and "Processed (complete)/" already contain the files generated with the scripts for double checking.
---

## List of Tables and Figures

The provided code reproduces:

- [x] All tables and figures in the paper
- [x] All tables and figures in the appendix

| Exhibit | Program | Output file | Notes |
|---------|---------|-------------|-------|
| Figure: Knowledge distribution | `02. Descriptive.R` | `Paper/Figures/Distrib KNOWLEDGE.jpeg` | Mean score ± SE by group and question |
| Figure: Behavior distribution | `02. Descriptive.R` | `Paper/Figures/Distrib BEHAVIOR.jpeg` | Stacked Likert % PRE (filled) vs POST (dashed) |
| Table: Balance table | `02. Descriptive.R` | `Output/Descriptive/balance_table.tex` | Means/SDs and pairwise OLS differences; HC1 SEs |
| Table: Knowledge regression | `03. Knowledge and mechanisms.R` | `Output/Results knowledge/knowledge_regression_table.tex` | OLS with school + cohort FE; SEs clustered at class; aggregated grades are measured as sum of correct answers |
| Table: Knowledge robustness (alternative outcomes) | `03. Knowledge and mechanisms.R` | `Output/Results knowledge/knowledge_robustness_alternative_outcomes.tex` | OLS with school + cohort FE; SEs clustered at class; alternative grades for he open-ended questions are used |
| Table: Knowledge fraction robustness | `03. Knowledge and mechanisms.R` | `Output/Results knowledge/knowledge_fraction_robustness_table.tex` | OLS with school + cohort FE; SEs clustered at class; aggregated grades are measured as fraction of correct answers |
| Table: ANOVA + Tukey (knowledge) | `03. Knowledge and mechanisms.R` | `Output/Results knowledge/anova_tukey_knowledge.tex` | ANOVA and Tukey comparisons for Knowledge outcome |
| Table: ANCOVA + Tukey (knowledge) | `03. Knowledge and mechanisms.R` | `Output/Results knowledge/ancova_tukey_knowledge.tex` | ANCOVA and Tukey comparisons for Knowledge outcome |
| Table: ANCOVA self-perceived knowledge | `03. Knowledge and mechanisms.R` | `Output/Results knowledge/ancova_selfperceived_knowledge.tex` | ANCOVA comparisons for Self-assessed Knowledge |
| Table: Multilevel regression (knowledge) | `03. Knowledge and mechanisms.R` | `Output/Results knowledge/multilevel_regression_knowledge.tex` | Random intercepts + slopes at class level |
| Figure: Bonferroni plot (knowledge questions) | `03. Knowledge and mechanisms.R` | `Output/Results knowledge/knowledge_questions_bonferroni_plot.pdf` | Coefficient plot for single questions on Knowledge; SEs adjusted for multiple hypoesis testing using Bonferroni |
| Figure: Kruskal–Wallis plot | `03. Knowledge and mechanisms.R` | `Output/Results knowledge/kruskal-wallis_plot.pdf` | Kruskal-Wallis coefficient plot for Knowledge outcomes |
| Figure: ANCOVA mechanisms | `03. Knowledge and mechanisms.R` | `Output/Results knowledge/ANCOVA_Mechanisms.pdf` | ANCOVA comparisons for Mechanisms |
| Table: Mechanisms (Bonferroni) | `03. Knowledge and mechanisms.R` | `Output/Results mechanisms/mechanisms_bonferroni_table.tex` | OLS with school + cohort FE|
| Table: Knowledge mechanisms with controls | `03. Knowledge and mechanisms.R` | `Output/Results mechanisms/knowledge_mechanisms_controls_table.tex` | OLS with school + cohort FE |
| Table: Behavior regression | `04. Behavior.R` | `Output/Results behavior/behavior_regression_table.tex` | OLS DiD; SEs clustered at class |
| Table: Behavior robustness | `04. Behavior.R` | `Output/Results behavior/behavior_robustness_table.tex` | OLS DiD; SEs clustered at class; individuals with spillovers excluded |
| Figure: Bonferroni plot (behavior questions) | `04. Behavior.R` | `Output/Results behavior/behavior_questions_bonferroni_plot.pdf` | Coefficient plot for single questions on Behavior; SEs adjusted for multiple hypoesis testing using Bonferroni|
| Table: Attitudes (multinomial logit) | `05. Attitudes.R` | `Output/Results attitudes/attitudes_multinom_two_panel.tex` | DiD multinomial logit; two panels (full sample; Lecture vs. Game) |

---

## References

The paper cites no external datasets. All data were collected by the authors; relevant documentation is archived in `Experiment Info/`.

---
