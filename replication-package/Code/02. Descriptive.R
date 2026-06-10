####################
## DESCRIPTIVE STATISTICS (NEW)
# Generates:
#   Paper/Figures/Distrib KNOWLEDGE.jpeg
#   Paper/Figures/Distrib BEHAVIOR.jpeg
#   Output/Descriptive/balance_table.tex
#
# Knowledge figure: mean score by group and question (Q1-Q8).
# Behavior figure:  stacked % distribution of Likert responses by group and item,
#                   PRE (filled bars) vs POST (dashed outlines).
# Balance table:    means/SDs and pairwise OLS differences by treatment arm (Panel A)
#                   and by school (Panel B).  HC1-robust SEs throughout.
#
# NOTE: Female is coded as Sex %in% c("F", "Femmina").
#       Verify against the Sesso encoding in Raw data.xlsx if values differ.
# NOTE: School names in schools_B must match the Scuola column in Dif.csv exactly.
#       Run  unique(Dif$Scuola)  to check.
#
# Input (processed): Knowledge.csv
#                    behavior_long.csv
#                    Dif.csv
####################

## 00. Packages and directory
library(dplyr)
library(tidyr)
library(ggplot2)
library(sandwich)   # vcovHC for balance table

base_dir <- "~/Desktop/Ricerche/WaterGame/Replication"
proc_dir <- file.path(base_dir, "Data/Processed")
out_dir  <- file.path(base_dir, "Output/Descriptive")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## 01. Load processed data
Knowledge     <- read.csv(file.path(proc_dir, "Knowledge.csv"),     check.names = FALSE)
behavior_long <- read.csv(file.path(proc_dir, "behavior_long.csv"), check.names = FALSE)
Dif           <- read.csv(file.path(proc_dir, "Dif.csv"),           check.names = FALSE)

## 02. Distrib KNOWLEDGE figure
# Mean score per question (Q1-Q8) by treatment group

knowledge_long <- Knowledge %>%
  mutate(
    Gruppo = dplyr::recode(
      as.character(Gruppo),
      "Controllo" = "Control",
      "Lezione"   = "Lecture",
      "Gioco"     = "Game"
    ),
    Gruppo = factor(Gruppo, levels = c("Control", "Lecture", "Game"))
  ) %>%
  dplyr::select(
    Gruppo,
    `Valutazione 1`, `Valutazione 2`, `Valutazione 3`, `Valutazione 4`,
    `Valutazione 5`, `Valutazione 6`, `Valutazione 7`, `Valutazione 8`
  ) %>%
  rename(
    Q1 = `Valutazione 1`, Q2 = `Valutazione 2`,
    Q3 = `Valutazione 3`, Q4 = `Valutazione 4`,
    Q5 = `Valutazione 5`, Q6 = `Valutazione 6`,
    Q7 = `Valutazione 7`, Q8 = `Valutazione 8`
  ) %>%
  pivot_longer(
    cols      = Q1:Q8,
    names_to  = "Question",
    values_to = "score"
  ) %>%
  mutate(Question = factor(Question, levels = paste0("Q", 1:8)))

knowledge_summary <- knowledge_long %>%
  group_by(Gruppo, Question) %>%
  summarise(
    mean_score = mean(score, na.rm = TRUE),
    se         = sd(score, na.rm = TRUE) / sqrt(sum(!is.na(score))),
    .groups    = "drop"
  )

p_knowledge <- ggplot(
  knowledge_summary,
  aes(x = Question, y = mean_score, fill = Gruppo)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = mean_score - se, ymax = mean_score + se),
    position = position_dodge(width = 0.75),
    width = 0.2
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x    = "Knowledge survey question",
    y    = "Mean score (%)",
    fill = "Treatment"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave(
  filename = file.path(out_dir, "Distrib KNOWLEDGE.jpeg"),
  plot     = p_knowledge,
  width    = 10,
  height   = 6,
  dpi      = 300
)

## 03. Distrib BEHAVIOR figure
# Stacked % distribution of Likert responses by group, item, and survey wave.
# Filled bars = PRE; dashed outlines = POST.
# Replicates reference script logic; reads from behavior_long.csv
# (Group, Periodo, and the five Italian-named behavior columns).

item_levels <- c(
  "Brushing theet",
  "Washing hands",
  "Talk with parents (extreme events)",
  "Talk with parents (water conservation)",
  "Talk with friends (water conservation)"
)

# --- PRE: filter, rename, reshape, summarise --------------------------------
behavior_long_PRE <- behavior_long %>%
  filter(Periodo == "PRE") %>%
  dplyr::select(
    Group,
    `Lava denti`, `Lava mani`,
    `Parla genitori emergenze`, `Parla genitori spreco`, `Parla amici spreco`
  ) %>%
  rename(
    `Brushing theet`                         = `Lava denti`,
    `Washing hands`                          = `Lava mani`,
    `Talk with parents (extreme events)`     = `Parla genitori emergenze`,
    `Talk with parents (water conservation)` = `Parla genitori spreco`,
    `Talk with friends (water conservation)` = `Parla amici spreco`
  ) %>%
  pivot_longer(
    cols      = all_of(item_levels),
    names_to  = "item",
    values_to = "response"
  ) %>%
  mutate(
    item     = factor(item, levels = item_levels),
    response = factor(response, levels = 1:5, ordered = TRUE)
  )

behavior_summary_PRE <- behavior_long_PRE %>%
  group_by(Group, item, response) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(
    prop   = n / sum(n) * 100,
    Survey = "PRE"
  ) %>%
  ungroup()

# --- POST: same -------------------------------------------------------------
behavior_long_POST <- behavior_long %>%
  filter(Periodo == "POST") %>%
  dplyr::select(
    Group,
    `Lava denti`, `Lava mani`,
    `Parla genitori emergenze`, `Parla genitori spreco`, `Parla amici spreco`
  ) %>%
  rename(
    `Brushing theet`                         = `Lava denti`,
    `Washing hands`                          = `Lava mani`,
    `Talk with parents (extreme events)`     = `Parla genitori emergenze`,
    `Talk with parents (water conservation)` = `Parla genitori spreco`,
    `Talk with friends (water conservation)` = `Parla amici spreco`
  ) %>%
  pivot_longer(
    cols      = all_of(item_levels),
    names_to  = "item",
    values_to = "response"
  ) %>%
  mutate(
    item     = factor(item, levels = item_levels),
    response = factor(response, levels = 1:5, ordered = TRUE)
  )

behavior_summary_POST <- behavior_long_POST %>%
  group_by(Group, item, response) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(
    prop   = n / sum(n) * 100,
    Survey = "POST"
  ) %>%
  ungroup()

# --- Combine and plot -------------------------------------------------------
behavior_both <- bind_rows(behavior_summary_PRE, behavior_summary_POST)

p_behavior <- ggplot() +
  # Filled bars = PRE
  geom_col(
    data     = filter(behavior_both, Survey == "PRE"),
    aes(x = response, y = prop, fill = Group),
    position = position_dodge(width = 0.8)
  ) +
  # Dashed outlines = POST
  geom_col(
    data      = filter(behavior_both, Survey == "POST"),
    aes(x = response, y = prop, group = Group),
    position  = position_dodge(width = 0.8),
    fill      = NA,
    colour    = "black",
    linetype  = "dashed",
    linewidth = 0.4
  ) +
  facet_wrap(~ item) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x    = "5-point Likert Scale",
    y    = "Answers (%)",
    fill = "Treatment"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  filename = file.path(out_dir, "Distrib BEHAVIOR.jpeg"),
  plot     = p_behavior,
  width    = 11,
  height   = 7,
  dpi      = 300
)

## 04. Balance table
# Two-panel LaTeX table saved to Output/Descriptive/balance_table.tex.
# The \sym, \msd, \estse macros and \usepackage{booktabs,threeparttable,makecell}
# must be in the LaTeX preamble.  The table can then be included via \input{}.

# ---- Prepare dataset -------------------------------------------------------
bal <- Dif %>%
  mutate(
    Group  = dplyr::recode(
      as.character(Group),
      "Controllo" = "Control", "Gioco" = "Game", "Lezione" = "Lecture"
    ),
    Female = as.numeric(Sex %in% c("F", "Femmina")),
    Age    = as.numeric(Age),
    GameA  = as.numeric(GameA),
    Cog    = as.numeric(Cog),
    PreK   = as.numeric(PreK),
    Scuola = as.character(Scuola)
  )

# Ordered group and school lists (school names must match Dif$Scuola exactly)
groups_A  <- c("Control", "Game", "Lecture")
schools_B <- c("D'Assisi", "D'Azeglio", "Rayneri")

# Covariates: variable name in bal → display label in table
cov_vars   <- c("Female", "Age", "GameA",          "Cog",                "PreK")
cov_labels <- c("Female", "Age", "Game Activity",  "Cognitive Ability",  "Pre-knowledge")

# ---- Helper functions ------------------------------------------------------
fmt3 <- function(x) {
  if (is.na(x)) return("--")
  formatC(round(as.numeric(x), 3), format = "f", digits = 3)
}

msd_cell   <- function(m, s) sprintf("\\msd{%s}{%s}", fmt3(m), fmt3(s))
estse_cell <- function(d, s, stars) sprintf("\\estse{%s}{%s}{%s}", fmt3(d), fmt3(s), stars)

# Pairwise OLS: g2 − g1, HC1-robust SE
pw_diff <- function(df, yvar, gcol, g1, g2) {
  sub   <- df[df[[gcol]] %in% c(g1, g2), , drop = FALSE]
  y     <- as.numeric(sub[[yvar]])
  dummy <- as.numeric(sub[[gcol]] == g2)
  ok    <- !is.na(y)
  if (sum(ok) < 3) return(list(d = NA, se = NA, stars = ""))
  dat   <- data.frame(y = y[ok], dummy = dummy[ok])
  mod   <- lm(y ~ dummy, data = dat)
  vc    <- vcovHC(mod, type = "HC1")
  d     <- coef(mod)["dummy"]
  se    <- sqrt(vc["dummy", "dummy"])
  pv    <- 2 * pt(-abs(d / se), df = nrow(dat) - 2)
  stars <- ifelse(pv < 0.001, "***", ifelse(pv < 0.01, "**",
           ifelse(pv < 0.05,  "*",   "")))
  list(d = d, se = se, stars = stars)
}

# Build one LaTeX data row: label & mean1(sd1) & mean2(sd2) & mean3(sd3) & diff21 & diff31 & diff32 \\
make_row <- function(df, yvar, gcol, g_vec, label) {
  ms <- sapply(g_vec, function(g) {
    v <- as.numeric(df[[yvar]][df[[gcol]] == g])
    c(mean(v, na.rm = TRUE), sd(v, na.rm = TRUE))
  })
  mean_part <- paste(apply(ms, 2, function(x) msd_cell(x[1], x[2])), collapse = " & ")

  pairs     <- list(c(1, 2), c(1, 3), c(2, 3))
  diff_part <- paste(sapply(pairs, function(p) {
    r <- pw_diff(df, yvar, gcol, g_vec[p[1]], g_vec[p[2]])
    estse_cell(r$d, r$se, r$stars)
  }), collapse = " & ")

  paste0(label, " & ", mean_part, " & ", diff_part, " \\\\")
}

# ---- Build rows ------------------------------------------------------------
rows_A <- mapply(
  FUN      = make_row,
  yvar     = cov_vars,
  label    = cov_labels,
  MoreArgs = list(df = bal, gcol = "Group",  g_vec = groups_A),
  SIMPLIFY = FALSE
)

rows_B <- mapply(
  FUN      = make_row,
  yvar     = cov_vars,
  label    = cov_labels,
  MoreArgs = list(df = bal, gcol = "Scuola", g_vec = schools_B),
  SIMPLIFY = FALSE
)

obs_A <- sapply(groups_A,  function(g) sum(bal$Group  == g, na.rm = TRUE))
obs_B <- sapply(schools_B, function(g) sum(bal$Scuola == g, na.rm = TRUE))

obs_line <- function(obs) {
  paste0("Observations & ", paste(obs, collapse = " & "),
         " & \\multicolumn{3}{c}{} \\\\")
}

col_header <- function(g1, g2, g3) {
  c(
    paste0(" & \\multicolumn{3}{c}{\\textbf{Means (SD)}}",
           " & \\multicolumn{3}{c}{\\textbf{Differences (SE)}} \\\\"),
    "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
    paste0("& (1) ", g1, " & (2) ", g2, " & (3) ", g3,
           " & (2)-(1) & (3)-(1) & (3)-(2) \\\\")
  )
}

# ---- Assemble LaTeX --------------------------------------------------------
tab <- c(
  "% Generated by 02_new. Descriptive.R",
  "% Requires in preamble:",
  "%   \\usepackage{booktabs,threeparttable,makecell}",
  "%   \\newcommand{\\sym}[1]{\\textsuperscript{#1}}",
  "%   \\newcommand{\\msd}[2]{\\makecell{#1 \\\\ (#2)}}",
  "%   \\newcommand{\\estse}[3]{\\makecell{#1\\sym{#3} \\\\ (#2)}}",
  "",
  "\\begin{table}[!ht]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Balance table}",
  "\\label{tab:balance_three_groups}",
  "\\small",
  "\\fontsize{7pt}{8.4pt}\\selectfont",
  "\\setlength{\\tabcolsep}{5pt}",
  "\\renewcommand{\\arraystretch}{1.1}",
  "",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  col_header("Control", "Game", "Lecture"),
  "\\midrule",
  "\\textbf{Panel A: Covariates by treatment arm} \\\\",
  unlist(rows_A),
  "\\addlinespace",
  obs_line(obs_A),
  "\\midrule",
  "\\multicolumn{7}{l}{\\textbf{Panel B: Covariates by school}}\\\\",
  "\\midrule",
  col_header("D'Assisi", "D'Azeglio", "Rayneri"),
  unlist(rows_B),
  "\\addlinespace",
  obs_line(obs_B),
  "\\bottomrule",
  "\\end{tabular}",
  "",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0("\\item Note: table reports means and standard deviations of baseline",
         " covariates by treatment arm (Panel~A) and by school (Panel~B),",
         " together with pairwise differences and HC1-robust standard errors."),
  "\\item \\sym{*} $p<0.05$, \\sym{**} $p<0.01$, \\sym{***} $p<0.001$.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tab, file.path(out_dir, "balance_table.tex"))
