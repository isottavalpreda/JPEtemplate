#######################
## EFFECT ON BEHAVIOR (NEW)
# Input:   Data/Processed/Dif.csv
# Outputs: Paper/Results behavior/*.tex, *.pdf
#######################

## 00. Packages and directories
library(tidyverse)
library(lmtest)
library(sandwich)
library(fixest)
library(emmeans)
library(ggplot2)
library(dplyr)

base_dir  <- "~/Desktop/Ricerche/WaterGame/Replication"
proc_dir  <- file.path(base_dir, "Data/Processed")

b_results <- file.path(base_dir, "Output/Results behavior")
dir.create(b_results, recursive = TRUE, showWarnings = FALSE)

## 01. Upload and prepare data
Dif <- read.csv(file.path(proc_dir, "Dif.csv"))

Dif <- Dif %>%
  mutate(
    Group       = factor(Group, levels = c("Controllo", "Lezione", "Gioco")),
    grade       = as.numeric(grade),
    difBehavior = as.numeric(difBehavior)
  )

Dif$Group <- relevel(Dif$Group, ref = "Controllo")

DifNC <- Dif %>%
  filter(Group != "Controllo") %>%
  mutate(Group = factor(Group, levels = c("Lezione", "Gioco")))

DifNC$Group <- relevel(DifNC$Group, ref = "Lezione")

DifNS <- Dif %>%
  filter(Spillover == 0) %>%
  mutate(Group = factor(Group, levels = c("Controllo", "Lezione", "Gioco")))

DifNS$Group <- relevel(DifNS$Group, ref = "Controllo")

DifNCNS <- DifNC %>%
  filter(Spillover == 0) %>%
  mutate(Group = factor(Group, levels = c("Lezione", "Gioco")))

DifNCNS$Group <- relevel(DifNCNS$Group, ref = "Lezione")

## 02. Global objects for tables
signif_note <- "*** $p < 0.01$, ** $p < 0.05$, * $p < 0.1$."

term_dict_common <- c(
  "GroupLezione"           = "Lecture",
  "GroupGioco"             = "Game",
  "SexMaschio"             = "Male",
  "SexPreferisconondirlo"  = "Prefer not to say the gender",
  "Age"                    = "Age",
  "GameA"                  = "Board games",
  "Cog"                    = "Cognitive ability",
  "PreK"                   = "Baseline knowledge",
  "Scuola"                 = "School",
  "grade"                  = "Cohort"
)

behavior_labels <- c(
  "diffDenti"  = "Q1: Brushing teeth",
  "diffMani"   = "Q2: Washing hands",
  "diffEmerg"  = "Q3: Talk with parents (extreme events)",
  "diffSpreco" = "Q4: Talk with parents (water conservation)",
  "diffAmici"  = "Q5: Talk with friends (water conservation)"
)

## 03. Helper functions
fmt_num <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x)) return("")
  paste0("\\num{", formatC(x, format = "f", digits = digits), "}")
}

star_fun <- function(p) {
  if (length(p) == 0 || is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

latex_row <- function(x) paste0(paste(x, collapse = " & "), " \\\\")

label_term <- function(term, dict = NULL) {
  if (!is.null(dict) && term %in% names(dict)) return(unname(dict[term]))
  if (grepl("^Sex", term)) {
    lev <- sub("^Sex", "", term)
    if (lev %in% c("Maschio", "Male")) return("Male")
    if (grepl("Prefer", lev, ignore.case = TRUE)) return("Prefer not to say the gender")
    return(lev)
  }
  term
}

make_table_header <- function(n_models, caption, label, header_groups, column_labels = NULL) {
  if (is.null(column_labels)) column_labels <- paste0("(", seq_len(n_models), ")")

  group_line <- paste0(
    "\\rule{0pt}{1.2em} & ",
    paste(
      mapply(
        function(nm, sp) paste0("\\multicolumn{", sp, "}{c}{\\textbf{", nm, "}}"),
        names(header_groups), unname(header_groups), USE.NAMES = FALSE
      ),
      collapse = " & "
    ),
    " \\\\"
  )

  clines <- character()
  start_col <- 2
  for (sp in unname(header_groups)) {
    end_col   <- start_col + sp - 1
    clines    <- c(clines, paste0("\\cline{", start_col, "-", end_col, "}"))
    start_col <- end_col + 1
  }

  c(
    "\\begin{table}[htbp]", "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\fontsize{7pt}{8.4pt}\\selectfont",
    paste0("\\begin{tabular}{l", paste(rep("c", n_models), collapse = ""), "}"),
    "\\hline", "\\hline",
    group_line,
    paste(clines, collapse = " "),
    paste0("\\rule{0pt}{1.2em} & ", paste(column_labels, collapse = " & "), " \\\\"),
    "\\hline",
    paste0("\\multicolumn{1}{l}{\\textbf{\\rule{0pt}{1.2em}VARIABLES}} & ",
           paste(rep("", n_models), collapse = " & "), " \\\\"),
    "\\hline", "\\\\"
  )
}

make_table_footer <- function(add_rows, notes) {
  lines <- c("\\\\", "\\hline")
  for (nm in names(add_rows)) {
    lines <- c(lines, latex_row(c(nm, add_rows[[nm]])))
  }
  c(lines, "\\\\", "\\hline", "\\hline", "\\end{tabular}",
    "{\\begin{flushleft}\\parbox{\\textwidth}{\\tiny \\hspace*{0.1cm}",
    paste0("\\textit{Notes:}} ", notes),
    "\\end{flushleft}}", "\\end{table}")
}

default_term_order <- function(ct_list, extra_terms = NULL) {
  all_terms <- unique(unlist(lapply(ct_list, names)))
  sex_terms <- sort(all_terms[grepl("^Sex", all_terms)])

  preferred_order <- c(
    "GroupLezione", "GroupGioco",
    sex_terms, "Age", "GameA", "Cog", "PreK",
    extra_terms
  )

  preferred_order[preferred_order %in% all_terms]
}

make_stats_rows <- function(models, controls, extra_rows = list()) {
  nobs_vec <- sapply(models, nobs)
  r2_vec   <- sapply(models, function(m) as.numeric(fixest::fitstat(m, "r2"))[1])

  c(
    list(
      "\\rule{0pt}{1.2em}Fixed Effects" = rep("Yes", length(models)),
      "Controls"                         = controls
    ),
    extra_rows,
    list(
      "Num.Obs." = sapply(nobs_vec, function(x) fmt_num(x, 0)),
      "R2"       = sapply(r2_vec,   function(x) fmt_num(x, 3))
    )
  )
}

write_fixest_latex_table <- function(models,
                                     file,
                                     caption,
                                     label,
                                     header_groups,
                                     notes,
                                     column_labels  = NULL,
                                     dict           = NULL,
                                     term_order     = NULL,
                                     custom_pvalues = list(),
                                     add_rows       = list(),
                                     groupline      = NULL) {
  n_models <- length(models)
  ct_list  <- lapply(models, function(m) fixest::coeftable(m, list = TRUE))

  if (is.null(term_order)) term_order <- default_term_order(ct_list)

  lines <- make_table_header(
    n_models = n_models, caption = caption, label = label,
    header_groups = header_groups, column_labels = column_labels
  )

  if (!is.null(groupline)) {
    idx_var <- which(grepl("^\\\\multicolumn\\{1\\}\\{l\\}\\{\\\\textbf\\{\\\\rule\\{0pt\\}", lines))[1]
    lines   <- append(lines, values = groupline, after = idx_var - 1)
  }

  for (term in term_order) {
    coef_line <- c(label_term(term, dict))
    se_line   <- c("")

    for (j in seq_along(ct_list)) {
      ct <- ct_list[[j]]

      if (term %in% names(ct)) {
        b     <- ct[[term]]$coef
        se    <- ct[[term]]$se
        p_raw <- ct[[term]]$pvalue
        p_use <- p_raw

        if (term %in% names(custom_pvalues)) {
          cv <- custom_pvalues[[term]]
          if (length(cv) >= j && !is.na(cv[j])) p_use <- cv[j]
        }

        coef_line <- c(coef_line, paste0(fmt_num(b, 3), star_fun(p_use)))
        se_line   <- c(se_line,   paste0("(", fmt_num(se, 3), ")"))
      } else {
        coef_line <- c(coef_line, "")
        se_line   <- c(se_line,   "")
      }
    }

    lines <- c(lines, latex_row(coef_line), latex_row(se_line))
  }

  lines <- c(lines, make_table_footer(add_rows = add_rows, notes = notes))
  writeLines(lines, con = file)
}

#################
## MAIN RESULTS
#################

## 04. Effect on behavior (simple difference)
m1 <- fixest::feols(difBehavior ~ Group | Scuola + grade, data = Dif,   vcov = ~cluster_id)
m2 <- fixest::feols(difBehavior ~ Group + Sex + Age + GameA + Cog + PreK | Scuola + grade, data = Dif,   vcov = ~cluster_id)
m3 <- fixest::feols(difBehavior ~ Group | Scuola + grade, data = DifNC, vcov = ~cluster_id)
m4 <- fixest::feols(difBehavior ~ Group + Sex + Age + GameA + Cog + PreK | Scuola + grade, data = DifNC, vcov = ~cluster_id)

main_models <- list(m1, m2, m3, m4)

p_raw_main <- c(
  "Control vs Lecture" = fixest::pvalue(m2)["GroupLezione"],
  "Control vs Game"    = fixest::pvalue(m2)["GroupGioco"],
  "Game vs Lecture"    = fixest::pvalue(m4)["GroupGioco"]
)

p_bonf_main <- p.adjust(p_raw_main, method = "bonferroni")

custom_p_main <- list(
  "GroupLezione" = c(NA, p_bonf_main["Control vs Lecture"], NA, NA),
  "GroupGioco"   = c(NA, p_bonf_main["Control vs Game"], NA, p_bonf_main["Game vs Lecture"])
)

main_rows <- make_stats_rows(
  models     = main_models,
  controls   = c("No", "Yes", "No", "Yes"),
  extra_rows = list(
    "Bonferroni p-value (Lecture)" = c("", fmt_num(p_bonf_main["Control vs Lecture"], 3), "", ""),
    "Bonferroni p-value (Game)"    = c("", fmt_num(p_bonf_main["Control vs Game"], 3), "", fmt_num(p_bonf_main["Game vs Lecture"], 3))
  )
)

write_fixest_latex_table(
  models        = main_models,
  file          = file.path(b_results, "behavior_regression_table.tex"),
  caption       = "Regression coefficients on behavior",
  label         = "tab:behavior_main",
  header_groups = c("Full sample" = 2, "Lecture vs Game" = 2),
  column_labels = paste0("(", 1:4, ")"),
  dict          = term_dict_common,
  custom_pvalues = custom_p_main,
  add_rows      = main_rows,
  notes = paste(
    "Columns (1) and (2) report estimates for the full sample, while columns (3) and (4) restrict the sample to Lecture and Game only.",
    "The reference category is Control in columns (1)-(2) and Lecture in columns (3)-(4).",
    "The outcome variable is the simple difference in the aggregate behavior index.",
    "Standard errors clustered at the \\texttt{cluster\\_id} level are reported in parentheses.",
    "For the treatment coefficients in the controlled specifications, significance markers are based on Bonferroni-adjusted p-values across the three main contrasts in columns (1)-(4).",
    signif_note
  )
)

## 05. Effect on single behavior items (coefficient plot)
behaviors <- names(Dif)[startsWith(names(Dif), "diff")]
behaviors <- behaviors[behaviors %in% names(behavior_labels)]

fit_cluster_feols <- function(outcome, data) {
  fml <- as.formula(
    paste0(outcome, " ~ Group + Sex + Age + GameA + Cog + PreK | Scuola + grade")
  )

  mod <- feols(fml, data = data, vcov = ~cluster_id)
  s   <- summary(mod)

  ct <- cbind(
    Estimate     = s$coeftable[, "Estimate"],
    `Std. Error` = s$coeftable[, "Std. Error"],
    `t value`    = s$coeftable[, "t value"],
    `Pr(>|t|)`   = s$coeftable[, "Pr(>|t|)"]
  )

  list(model = mod, coeftest = ct)
}

results <- setNames(
  purrr::map(behaviors, ~ fit_cluster_feols(.x, Dif)),
  behaviors
)

coef_df <- purrr::map_dfr(names(results), function(outcome_name) {
  ct   <- as.matrix(results[[outcome_name]]$coeftest)
  rn   <- rownames(ct)
  keep <- grepl("^Group", rn)

  data.frame(
    outcome  = outcome_name,
    group    = sub("^Group", "", rn[keep]),
    estimate = unname(ct[keep, "Estimate"]),
    se       = unname(ct[keep, "Std. Error"]),
    p_raw    = unname(ct[keep, "Pr(>|t|)"]),
    stringsAsFactors = FALSE
  )
})

coef_df <- coef_df %>%
  filter(group %in% c("Lezione", "Gioco")) %>%
  group_by(group) %>%
  mutate(
    p_bonf       = p.adjust(p_raw, method = "bonferroni"),
    n_tests      = n(),
    alpha_bonf   = 0.05 / n_tests,
    z_bonf       = qnorm(1 - alpha_bonf / 2),
    ci_low_bonf  = estimate - z_bonf * se,
    ci_high_bonf = estimate + z_bonf * se
  ) %>%
  ungroup()

coef_df_plot <- coef_df %>%
  mutate(
    outcome_lab = dplyr::recode(as.character(outcome), !!!behavior_labels),
    group_lab   = dplyr::recode(as.character(group),
                                "Lezione" = "Lecture",
                                "Gioco"   = "Game")
  ) %>%
  filter(!is.na(outcome_lab)) %>%
  mutate(
    outcome_lab = factor(outcome_lab, levels = unname(behavior_labels)),
    group_lab   = factor(group_lab,   levels = c("Game", "Lecture"))
  )

p_beh <- ggplot(coef_df_plot, aes(x = estimate, y = group_lab, color = group_lab)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  geom_errorbarh(
    aes(xmin = ci_low_bonf, xmax = ci_high_bonf),
    height = 0.10, linewidth = 0.8, alpha = 0.7
  ) +
  geom_point(size = 2.8) +
  facet_wrap(~ outcome_lab, ncol = 2, scales = "free_x") +
  scale_color_manual(
    name   = "Treatment",
    values = c("Lecture" = "#fc8d62", "Game" = "#8da0cb")
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position    = "bottom",
    strip.text         = element_text(size = 11),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.major.x = element_line(color = "grey92"),
    axis.ticks         = element_blank()
  )

ggsave(
  filename = file.path(b_results, "behavior_questions_bonferroni_plot.pdf"),
  plot = p_beh, width = 13, height = 9
)

ggsave(
  filename = file.path(b_results, "behavior_questions_bonferroni_plot.png"),
  plot = p_beh, width = 13, height = 9, dpi = 300
)

####################
## ROBUSTNESS
####################

## 06. Effect on behavior, dropping observations with spillovers
r1 <- fixest::feols(difBehavior ~ Group | Scuola + grade, data = DifNS,   vcov = ~cluster_id)
r2 <- fixest::feols(difBehavior ~ Group + Sex + Age + GameA + Cog + PreK | Scuola + grade, data = DifNS,   vcov = ~cluster_id)
r3 <- fixest::feols(difBehavior ~ Group | Scuola + grade, data = DifNCNS, vcov = ~cluster_id)
r4 <- fixest::feols(difBehavior ~ Group + Sex + Age + GameA + Cog + PreK | Scuola + grade, data = DifNCNS, vcov = ~cluster_id)

rob_models <- list(r1, r2, r3, r4)

p_raw_rob <- c(
  "Control vs Lecture" = fixest::pvalue(r2)["GroupLezione"],
  "Control vs Game"    = fixest::pvalue(r2)["GroupGioco"],
  "Game vs Lecture"    = fixest::pvalue(r4)["GroupGioco"]
)

p_bonf_rob <- p.adjust(p_raw_rob, method = "bonferroni")

custom_p_rob <- list(
  "GroupLezione" = c(NA, p_bonf_rob["Control vs Lecture"], NA, NA),
  "GroupGioco"   = c(NA, p_bonf_rob["Control vs Game"], NA, p_bonf_rob["Game vs Lecture"])
)

rob_rows <- make_stats_rows(
  models     = rob_models,
  controls   = c("No", "Yes", "No", "Yes"),
  extra_rows = list(
    "Spillovers excluded"          = rep("Yes", 4),
    "Bonferroni p-value (Lecture)" = c("", fmt_num(p_bonf_rob["Control vs Lecture"], 3), "", ""),
    "Bonferroni p-value (Game)"    = c("", fmt_num(p_bonf_rob["Control vs Game"], 3), "", fmt_num(p_bonf_rob["Game vs Lecture"], 3))
  )
)

write_fixest_latex_table(
  models        = rob_models,
  file          = file.path(b_results, "behavior_robustness_table.tex"),
  caption       = "Robustness checks on behavior excluding spillovers",
  label         = "tab:behavior_robustness",
  header_groups = c("Full sample" = 2, "Lecture vs Game" = 2),
  column_labels = paste0("(", 1:4, ")"),
  dict          = term_dict_common,
  custom_pvalues = custom_p_rob,
  add_rows      = rob_rows,
  notes = paste(
    "Columns (1) and (2) report estimates for the full sample without spillover observations, while columns (3) and (4) restrict the sample to Lecture and Game only without spillovers.",
    "The reference category is Control in columns (1)-(2) and Lecture in columns (3)-(4).",
    "The outcome variable is the simple difference in the aggregate behavior index.",
    "Standard errors clustered at the \\texttt{cluster\\_id} level are reported in parentheses.",
    "For the treatment coefficients in the controlled specifications, significance markers are based on Bonferroni-adjusted p-values across the three robustness contrasts in columns (1)-(4).",
    signif_note
  )
)