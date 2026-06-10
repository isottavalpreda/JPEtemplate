#######################
## EFFECT ON KNOWLEDGE AND MECHANISMS (NEW)
# Input:   Data/Processed/Knowledge.csv
# Outputs: Paper/Results knowledge/*.tex, *.pdf
#          Paper/Results mechanisms/*.tex
#######################

## 00. Packages and directories
library(tidyverse)
library(lmtest)
library(sandwich)
library(fixest)
library(emmeans)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(dplyr)
library(ggstatsplot)

base_dir  <- "~/Desktop/Ricerche/WaterGame/Replication"
proc_dir  <- file.path(base_dir, "Data/Processed")

k_results <- file.path(base_dir, "Output/Results knowledge")
m_results <- file.path(base_dir, "Output/Results mechanisms")
dir.create(k_results, recursive = TRUE, showWarnings = FALSE)
dir.create(m_results, recursive = TRUE, showWarnings = FALSE)

## 01. Upload and prepare data
Knowledge <- read.csv(file.path(proc_dir, "Knowledge.csv"))

Knowledge <- Knowledge %>%
  mutate(
    Gruppo     = factor(Gruppo, levels = c("Controllo", "Lezione", "Gioco")),
    grade      = as.numeric(grade),
    CONOSCENZE = as.numeric(CONOSCENZE),
    MECCANISMI = as.numeric(MECCANISMI)
  )

Knowledge$Gruppo <- relevel(Knowledge$Gruppo, ref = "Controllo")

KnowledgeNC <- Knowledge %>%
  filter(Gruppo != "Controllo") %>%
  mutate(Gruppo = factor(Gruppo, levels = c("Lezione", "Gioco")))

KnowledgeNC$Gruppo <- relevel(KnowledgeNC$Gruppo, ref = "Lezione")

# Rename Valutazione columns that may have a spurious space after the dot
Knowledge <- Knowledge %>%
  rename_with(
    ~ gsub("^Valutazione\\.\\s+", "Valutazione.", .x),
    starts_with("Valutazione. ")
  )

KnowledgeNC <- KnowledgeNC %>%
  rename_with(
    ~ gsub("^Valutazione\\.\\s+", "Valutazione.", .x),
    starts_with("Valutazione. ")
  )

## 02. Descriptive analysis (silent; used to check data only)
aggregate(
  Risposta17 ~ Gruppo,
  data = Knowledge,
  function(x) round(c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)), 2)
)

## 03. Global objects for tables
signif_note <- "*** $p < 0.01$, ** $p < 0.05$, * $p < 0.1$."

term_dict_common <- c(
  "GruppoLezione"            = "Lecture",
  "GruppoGioco"              = "Game",
  "SessoMaschio"             = "Male",
  "SessoPreferisconondirlo"  = "Prefer not to say the gender",
  "Età"                      = "Age",
  "Giochi.tavolo"            = "Board games",
  "INTELLIGENZA"             = "Cognitive ability",
  "PRE.CONOSCENZA"           = "Baseline knowledge",
  "Scuola"                   = "School",
  "grade"                    = "Cohort"
)

term_dict_mechanisms <- c(
  term_dict_common,
  "Risposta11" = "Fun",
  "Risposta12" = "Competition",
  "Risposta13" = "Enjoy",
  "Risposta14" = "Cooperation",
  "Risposta15" = "Design",
  "Risposta16" = "Classmates",
  "Risposta17" = "Facilitator"
)

outcome_labels <- c(
  "Valutazione.1"  = "Q1: When do water collection technologies work?",
  "Valutazione.2"  = "Q2: Is water a finite or infinite resource?",
  "Valutazione.3"  = "Q3: When it rains, which of these helps collect more water?",
  "Valutazione.4"  = "Q4: When it's sunny, which of these helps collect more water?",
  "Valutazione.5"  = "Q5: If a flood comes after a drought, what happens?",
  "Valutazione.6"  = "Q6: Is water needed for machines at work?",
  "Valutazione.7"  = "Q7: What is grey water? Can it be reused?",
  "Valutazione.8"  = "Q8: Why are rain gardens useful?",
  "Valutazione.9"  = "Q9: Why does buying a lot of clothes affect water?",
  "Valutazione.10" = "Q10: Why do green areas help during drought or floods?"
)

## 04. Helper functions
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

latex_row <- function(x) {
  paste0(paste(x, collapse = " & "), " \\\\")
}

label_term <- function(term, dict = NULL) {
  if (!is.null(dict) && term %in% names(dict)) {
    return(unname(dict[term]))
  }

  if (grepl("^Sesso", term)) {
    lev <- sub("^Sesso", "", term)
    if (lev %in% c("Maschio", "Male")) return("Male")
    if (grepl("Prefer", lev, ignore.case = TRUE)) return("Prefer not to say the gender")
    return(lev)
  }

  term
}

make_table_header <- function(n_models, caption, label, header_groups, column_labels = NULL) {
  if (is.null(column_labels)) {
    column_labels <- paste0("(", seq_len(n_models), ")")
  }

  group_line <- paste0(
    "\\rule{0pt}{1.2em} & ",
    paste(
      mapply(
        function(nm, sp) paste0("\\multicolumn{", sp, "}{c}{\\textbf{", nm, "}}"),
        names(header_groups),
        unname(header_groups),
        USE.NAMES = FALSE
      ),
      collapse = " & "
    ),
    " \\\\"
  )

  clines <- character()
  start_col <- 2
  for (sp in unname(header_groups)) {
    end_col <- start_col + sp - 1
    clines <- c(clines, paste0("\\cline{", start_col, "-", end_col, "}"))
    start_col <- end_col + 1
  }

  c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\fontsize{7pt}{8.4pt}\\selectfont",
    paste0("\\begin{tabular}{l", paste(rep("c", n_models), collapse = ""), "}"),
    "\\hline",
    "\\hline",
    "\\bottomrule",
    group_line,
    paste(clines, collapse = " "),
    paste0("\\rule{0pt}{1.2em} & ", paste(column_labels, collapse = " & "), " \\\\"),
    "\\hline",
    paste0("\\multicolumn{1}{l}{\\textbf{\\rule{0pt}{1.2em}VARIABLES}} & ",
           paste(rep("", n_models), collapse = " & "), " \\\\"),
    "\\hline",
    "\\\\"
  )
}

make_table_footer <- function(add_rows, notes) {
  lines <- c("\\\\", "\\hline")

  for (nm in names(add_rows)) {
    lines <- c(lines, latex_row(c(nm, add_rows[[nm]])))
  }

  c(
    lines,
    "\\\\",
    "\\bottomrule",
    "\\hline",
    "\\hline",
    "\\end{tabular}",
    "{\\begin{flushleft}\\parbox{\\textwidth}{\\tiny \\hspace*{0.1cm}",
    paste0("\\textit{Notes:}} ", notes),
    "\\end{flushleft}}",
    "\\end{table}"
  )
}

default_term_order <- function(ct_list, extra_terms = NULL) {
  all_terms <- unique(unlist(lapply(ct_list, names)))
  sex_terms  <- sort(all_terms[grepl("^Sesso", all_terms)])
  mech_terms <- sort(all_terms[all_terms %in% paste0("Risposta", 11:17)])

  preferred_order <- c(
    "GruppoLezione",
    "GruppoGioco",
    sex_terms,
    "Età",
    "Giochi.tavolo",
    "INTELLIGENZA",
    "PRE.CONOSCENZA",
    extra_terms,
    mech_terms
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

  if (is.null(term_order)) {
    term_order <- default_term_order(ct_list)
  }

  lines <- make_table_header(
    n_models      = n_models,
    caption       = caption,
    label         = label,
    header_groups = header_groups,
    column_labels = column_labels
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
        b      <- ct[[term]]$coef
        se     <- ct[[term]]$se
        p_raw  <- ct[[term]]$pvalue
        p_use  <- p_raw

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


#########################
## MAIN RESULTS
#########################

## 05. Effect on aggregated knowledge and self-assessed knowledge
m1 <- feols(
  CONOSCENZE ~ Gruppo | Scuola + grade,
  data    = Knowledge,
  cluster = ~cluster_id
)

m2 <- feols(
  CONOSCENZE ~ Gruppo + Sesso + Età + Giochi.tavolo +
    INTELLIGENZA + PRE.CONOSCENZA | Scuola + grade,
  data    = Knowledge,
  cluster = ~cluster_id
)

m3 <- feols(
  CONOSCENZE ~ Gruppo | Scuola + grade,
  data    = KnowledgeNC,
  cluster = ~cluster_id
)

m4 <- feols(
  CONOSCENZE ~ Gruppo + Sesso + Età + Giochi.tavolo +
    INTELLIGENZA + PRE.CONOSCENZA | Scuola + grade,
  data    = KnowledgeNC,
  cluster = ~cluster_id
)

m5 <- feols(
  Risposta18 ~ Gruppo + Sesso + Età + Giochi.tavolo +
    INTELLIGENZA + PRE.CONOSCENZA | Scuola + grade,
  data    = KnowledgeNC,
  cluster = ~cluster_id
)

main_models <- list(m1, m2, m3, m4, m5)

p_raw_main <- c(
  "Control vs Lecture" = fixest::pvalue(m2)["GruppoLezione"],
  "Control vs Game"    = fixest::pvalue(m2)["GruppoGioco"],
  "Game vs Lecture"    = fixest::pvalue(m4)["GruppoGioco"]
)

p_bonf_main <- p.adjust(p_raw_main, method = "bonferroni")

custom_p_main <- list(
  "GruppoLezione" = c(NA, p_bonf_main["Control vs Lecture"], NA, NA, NA),
  "GruppoGioco"   = c(NA, p_bonf_main["Control vs Game"], NA, p_bonf_main["Game vs Lecture"], NA)
)

main_rows <- make_stats_rows(
  models   = main_models,
  controls = c("No", "Yes", "No", "Yes", "Yes")
)

extra_header_line <- "\\emph{\\textbf{}} & \\multicolumn{4}{c}{Observed} & Self-assessed\\\\"

write_fixest_latex_table(
  models        = main_models,
  file          = file.path(k_results, "knowledge_regression_table.tex"),
  caption       = "Regression coefficients on knowledge",
  label         = "tab:res_knowledge",
  header_groups = c("Full sample" = 2, "Lecture vs Game" = 3),
  column_labels = paste0("(", 1:5, ")"),
  dict          = term_dict_common,
  custom_pvalues = custom_p_main,
  add_rows      = main_rows,
  notes = paste(
    "Columns (1) and (2) report estimates for the full sample, while columns (3), (4) and (5) restrict the sample to Lecture and Game only.",
    "The reference category is Control in columns (1)-(2) and Lecture in columns (3)-(5).",
    "The outcome variable is the score obtained in the Knowledge survey in columns (1)-(4), and the self-assessed knowledge in column (5).",
    "Standard errors clustered at the \\texttt{cluster\\_id} level are reported in parentheses.",
    "For the treatment coefficients in the controlled specifications, significance markers are based on Bonferroni-adjusted p-values across the three main contrasts in columns (1) - (4).",
    signif_note
  ),
  groupline = extra_header_line
)


## 06. Effect on single knowledge items (coefficient plot)
valutazioni <- names(Knowledge)[startsWith(names(Knowledge), "Valutazione.")]

fit_cluster_lm <- function(outcome, data) {
  fml <- as.formula(paste0(
    outcome,
    " ~ Gruppo + Sesso + Età + Giochi.tavolo + INTELLIGENZA + PRE.CONOSCENZA + Scuola + grade"
  ))

  mod <- lm(fml, data = data)

  ct <- lmtest::coeftest(
    mod,
    vcov = sandwich::vcovCL(mod, cluster = data$cluster_id)
  )

  list(model = mod, coeftest = ct)
}

results <- setNames(
  purrr::map(valutazioni, ~ fit_cluster_lm(.x, Knowledge)),
  valutazioni
)

coef_df <- purrr::map_dfr(names(results), function(outcome_name) {
  ct <- as.matrix(results[[outcome_name]]$coeftest)
  rn <- rownames(ct)
  keep <- grepl("^Gruppo", rn)

  data.frame(
    outcome  = outcome_name,
    group    = sub("^Gruppo", "", rn[keep]),
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
    n_tests      = n(),
    p_bonf       = p.adjust(p_raw, method = "bonferroni"),
    alpha_bonf   = 0.05 / n_tests,
    z_bonf       = qnorm(1 - alpha_bonf / 2),
    ci_low_bonf  = estimate - z_bonf * se,
    ci_high_bonf = estimate + z_bonf * se
  ) %>%
  ungroup()

coef_df_plot <- coef_df %>%
  mutate(
    outcome_lab = dplyr::recode(as.character(outcome), !!!outcome_labels),
    group_lab   = dplyr::recode(as.character(group),
                                "Lezione" = "Lecture",
                                "Gioco"   = "Game")
  ) %>%
  filter(!is.na(outcome_lab)) %>%
  mutate(
    outcome_lab = factor(outcome_lab, levels = unname(outcome_labels)),
    group_lab   = factor(group_lab, levels = c("Game", "Lecture"))
  )

p <- ggplot(coef_df_plot, aes(x = estimate, y = group_lab, color = group_lab)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  geom_errorbarh(
    aes(xmin = ci_low_bonf, xmax = ci_high_bonf),
    height    = 0.10,
    linewidth = 0.8,
    alpha     = 0.7
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
  filename = file.path(k_results, "knowledge_questions_bonferroni_plot.pdf"),
  plot     = p,
  width    = 13,
  height   = 9
)

ggsave(
  filename = file.path(k_results, "knowledge_questions_bonferroni_plot.png"),
  plot     = p,
  width    = 13,
  height   = 9,
  dpi      = 300
)

## 07. Effect on each mechanism
mechanism_vars <- paste0("Risposta", 11:17)

make_mech_formula <- function(y) {
  as.formula(paste0(
    y,
    " ~ Gruppo + Sesso + Età + Giochi.tavolo + ",
    "INTELLIGENZA + PRE.CONOSCENZA | Scuola + grade"
  ))
}

mechanism_models <- setNames(
  lapply(mechanism_vars, function(y) {
    feols(make_mech_formula(y), data = KnowledgeNC, cluster = ~cluster_id)
  }),
  mechanism_vars
)

p_raw_game  <- sapply(mechanism_models, function(m) fixest::pvalue(m)["GruppoGioco"])
p_bonf_game <- p.adjust(p_raw_game, method = "bonferroni")

mechanism_rows <- make_stats_rows(
  models     = mechanism_models,
  controls   = rep("Yes", length(mechanism_models)),
  extra_rows = list(
    "Bonferroni p-value (Game)" = sapply(p_bonf_game, function(x) fmt_num(x, 3))
  )
)

write_fixest_latex_table(
  models        = mechanism_models,
  file          = file.path(m_results, "mechanisms_bonferroni_table.tex"),
  caption       = "Regression coefficients on activity-evaluation mechanisms",
  label         = "tab:mechanisms_bonferroni",
  header_groups = c("Lecture vs Game" = 7),
  column_labels = paste0("(", 1:7, ")"),
  dict          = term_dict_common,
  term_order    = c(
    "GruppoGioco",
    sort(unique(unlist(lapply(
      lapply(mechanism_models, fixest::coeftable, list = TRUE),
      names
    ))[grepl("^Sesso", unique(unlist(lapply(
      lapply(mechanism_models, fixest::coeftable, list = TRUE),
      names
    ))))])),
    "Età", "Giochi.tavolo", "INTELLIGENZA", "PRE.CONOSCENZA"
  ),
  custom_pvalues = list("GruppoGioco" = as.numeric(p_bonf_game)),
  add_rows       = mechanism_rows,
  notes = paste(
    "Each column reports a separate OLS model estimated on the treated sample only, where the dependent variable is one activity-evaluation mechanism measured on a 5-point Likert scale.",
    "The omitted category is Lecture, so the coefficient on Game is interpreted as Game relative to Lecture.",
    "Standard errors clustered at the \\texttt{cluster\\_id} level are reported in parentheses.",
    "Bonferroni correction is applied across the seven treatment coefficients; significance markers on the Game row are based on Bonferroni-adjusted p-values.",
    "For the remaining covariates, significance markers are based on conventional clustered p-values.",
    signif_note
  )
)

#########################
## ROBUSTNESS
#########################

## 08. Robustness on alternative knowledge measures
rb1 <- feols(
  CONOSCENZE.bis ~ Gruppo + Sesso + Età + Giochi.tavolo +
    INTELLIGENZA + PRE.CONOSCENZA | Scuola + grade,
  data = Knowledge, vcov = ~cluster_id
)

rb1nc <- feols(
  CONOSCENZE.bis ~ Gruppo + Sesso + Età + Giochi.tavolo +
    INTELLIGENZA + PRE.CONOSCENZA | Scuola + grade,
  data = KnowledgeNC, vcov = ~cluster_id
)

rb2 <- feols(
  CONOSCCENZE.tris ~ Gruppo + Sesso + Età + Giochi.tavolo +
    INTELLIGENZA + PRE.CONOSCENZA | Scuola + grade,
  data = Knowledge, vcov = ~cluster_id
)

rb2nc <- feols(
  CONOSCCENZE.tris ~ Gruppo + Sesso + Età + Giochi.tavolo +
    INTELLIGENZA + PRE.CONOSCENZA | Scuola + grade,
  data = KnowledgeNC, vcov = ~cluster_id
)

rb_models <- list(rb1, rb1nc, rb2, rb2nc)

rb_rows <- make_stats_rows(
  models   = rb_models,
  controls = rep("Yes", length(rb_models))
)

write_fixest_latex_table(
  models        = rb_models,
  file          = file.path(k_results, "knowledge_robustness_alternative_outcomes.tex"),
  caption       = "Robustness checks using alternative knowledge measures",
  label         = "tab:knowledge_robustness_alternative_outcomes",
  header_groups = c("Grade 0-1" = 2, "Grade 0-3" = 2),
  column_labels = c("(1)", "(2)", "(3)", "(4)"),
  dict = c(
    "GruppoLezione"              = "Lecture",
    "GruppoGioco"                = "Game",
    "SessoMaschio"               = "Male",
    "SessoPreferisco non dirlo"  = "Prefer not to say the gender",
    "Età"                        = "Age",
    "Giochi.tavolo"              = "Board games",
    "INTELLIGENZA"               = "Cognitive ability",
    "PRE.CONOSCENZA"             = "Baseline knowledge"
  ),
  term_order = c(
    "GruppoLezione", "GruppoGioco",
    "SessoMaschio", "SessoPreferisco non dirlo",
    "Età", "Giochi.tavolo", "INTELLIGENZA", "PRE.CONOSCENZA"
  ),
  add_rows = rb_rows,
  notes = paste(
    "Each column reports an OLS model with school and grade fixed effects.",
    "Columns (1) and (3) use the full sample, while columns (2) and (4) restrict the sample to Lecture and Game only.",
    "The omitted category is Control in columns (1) and (3), and Lecture in columns (2) and (4).",
    "Standard errors clustered at the \\texttt{cluster\\_id} level are reported in parentheses.",
    "Significance markers are based on conventional clustered p-values.",
    signif_note
  )
)

## 09. Robustness using fraction-based knowledge measures
normalize_open_text <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_replace_all(" ", " ") %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[[:punct:]]+", " ") %>%
    stringr::str_squish()
}

is_idk_open <- function(x) {
  x_clean <- normalize_open_text(x)

  is.na(x) |
    x_clean == "" |
    stringr::str_detect(
      x_clean,
      paste(
        c(
          "^non lo so(?:\\b.*)?$",
          "^non so(?:\\b.*)?$",
          "^non saprei(?:\\s+rispondere)?(?:\\b.*)?$",
          "^non ricordo(?:\\b.*)?$",
          "^non mi ricordo(?:\\b.*)?$",
          "^non me lo ricordo(?:\\b.*)?$"
        ),
        collapse = "|"
      )
    )
}

add_fraction_knowledge_scores <- function(df) {
  df %>%
    mutate(
      tmp_all_1  = dplyr::coalesce(Valutazione.1, 0) / 3,
      tmp_all_2  = dplyr::coalesce(Valutazione.2, 0) / 3,
      tmp_all_3  = dplyr::coalesce(Valutazione.3, 0) / 4,
      tmp_all_4  = dplyr::coalesce(Valutazione.4, 0) / 4,
      tmp_all_5  = dplyr::coalesce(Valutazione.5, 0) / 4,
      tmp_all_6  = dplyr::coalesce(Valutazione.6, 0) / 4,
      tmp_all_7  = dplyr::coalesce(Valutazione.7, 0) / 4,
      tmp_all_8  = dplyr::coalesce(Valutazione.8, 0) / 4,
      tmp_all_9  = dplyr::coalesce(Valutazione.9, 0) / 2,
      tmp_all_10 = dplyr::coalesce(Valutazione.10, 0) / 2,

      CONOSCENZE_frac_all = rowMeans(
        dplyr::pick(
          tmp_all_1, tmp_all_2, tmp_all_3, tmp_all_4, tmp_all_5,
          tmp_all_6, tmp_all_7, tmp_all_8, tmp_all_9, tmp_all_10
        ),
        na.rm = TRUE
      ),

      tmp_ans_1  = if_else(!is.na(Risposta1)  & !is_idk_open(Risposta1),  1, 0),
      tmp_ans_2  = if_else(!is.na(Risposta2)  & !is_idk_open(Risposta2),  1, 0),
      tmp_ans_3  = if_else(!is.na(Risposta3)  & !is_idk_open(Risposta3),  1, 0),
      tmp_ans_4  = if_else(!is.na(Risposta4)  & !is_idk_open(Risposta4),  1, 0),
      tmp_ans_5  = if_else(!is.na(Risposta5)  & !is_idk_open(Risposta5),  1, 0),
      tmp_ans_6  = if_else(!is.na(Risposta6)  & !is_idk_open(Risposta6),  1, 0),
      tmp_ans_7  = if_else(!is.na(Risposta7)  & !is_idk_open(Risposta7),  1, 0),
      tmp_ans_8  = if_else(!is.na(Risposta8)  & !is_idk_open(Risposta8),  1, 0),
      tmp_ans_9  = if_else(!is.na(Risposta9)  & !is_idk_open(Risposta9),  1, 0),
      tmp_ans_10 = if_else(!is.na(Risposta10) & !is_idk_open(Risposta10), 1, 0),

      tmp_noidk_1  = if_else(tmp_ans_1  == 1, dplyr::coalesce(Valutazione.1,  0),       NA_real_),
      tmp_noidk_2  = if_else(tmp_ans_2  == 1, dplyr::coalesce(Valutazione.2,  0),       NA_real_),
      tmp_noidk_3  = if_else(tmp_ans_3  == 1, dplyr::coalesce(Valutazione.3,  0),       NA_real_),
      tmp_noidk_4  = if_else(tmp_ans_4  == 1, dplyr::coalesce(Valutazione.4,  0),       NA_real_),
      tmp_noidk_5  = if_else(tmp_ans_5  == 1, dplyr::coalesce(Valutazione.5,  0),       NA_real_),
      tmp_noidk_6  = if_else(tmp_ans_6  == 1, dplyr::coalesce(Valutazione.6,  0),       NA_real_),
      tmp_noidk_7  = if_else(tmp_ans_7  == 1, dplyr::coalesce(Valutazione.7,  0),       NA_real_),
      tmp_noidk_8  = if_else(tmp_ans_8  == 1, dplyr::coalesce(Valutazione.8,  0),       NA_real_),
      tmp_noidk_9  = if_else(tmp_ans_9  == 1, dplyr::coalesce(Valutazione.9,  0) / 2,   NA_real_),
      tmp_noidk_10 = if_else(tmp_ans_10 == 1, dplyr::coalesce(Valutazione.10, 0) / 2,   NA_real_),

      tmp_denom_noidk = rowSums(
        dplyr::pick(
          tmp_ans_1, tmp_ans_2, tmp_ans_3, tmp_ans_4, tmp_ans_5,
          tmp_ans_6, tmp_ans_7, tmp_ans_8, tmp_ans_9, tmp_ans_10
        ),
        na.rm = TRUE
      ),

      CONOSCENZE_frac_noidk = if_else(
        tmp_denom_noidk > 0,
        rowSums(
          dplyr::pick(
            tmp_noidk_1, tmp_noidk_2, tmp_noidk_3, tmp_noidk_4, tmp_noidk_5,
            tmp_noidk_6, tmp_noidk_7, tmp_noidk_8, tmp_noidk_9, tmp_noidk_10
          ),
          na.rm = TRUE
        ) / tmp_denom_noidk,
        NA_real_
      )
    ) %>%
    dplyr::select(
      -starts_with("tmp_all_"),
      -starts_with("tmp_ans_"),
      -starts_with("tmp_noidk_"),
      -tmp_denom_noidk
    )
}

Knowledge   <- add_fraction_knowledge_scores(Knowledge)
KnowledgeNC <- add_fraction_knowledge_scores(KnowledgeNC)

rf1 <- feols(
  CONOSCENZE_frac_all ~ Gruppo + Sesso + Età + Giochi.tavolo +
    INTELLIGENZA + PRE.CONOSCENZA | Scuola + grade,
  data = Knowledge, cluster = ~cluster_id
)

rf1nc <- feols(
  CONOSCENZE_frac_all ~ Gruppo + Sesso + Età + Giochi.tavolo +
    INTELLIGENZA + PRE.CONOSCENZA | Scuola + grade,
  data = KnowledgeNC, cluster = ~cluster_id
)

rf2 <- feols(
  CONOSCENZE_frac_noidk ~ Gruppo + Sesso + Età + Giochi.tavolo +
    INTELLIGENZA + PRE.CONOSCENZA | Scuola + grade,
  data = Knowledge, cluster = ~cluster_id
)

rf2nc <- feols(
  CONOSCENZE_frac_noidk ~ Gruppo + Sesso + Età + Giochi.tavolo +
    INTELLIGENZA + PRE.CONOSCENZA | Scuola + grade,
  data = KnowledgeNC, cluster = ~cluster_id
)

rob_frac_models <- list(rf1, rf1nc, rf2, rf2nc)

rob_frac_ct <- lapply(rob_frac_models, fixest::coeftable, list = TRUE)
rob_frac_terms <- c(
  "GruppoLezione", "GruppoGioco",
  "SessoMaschio", "SessoPreferisconondirlo",
  "Età", "Giochi.tavolo", "INTELLIGENZA", "PRE.CONOSCENZA"
)
rob_frac_term_order <- rob_frac_terms[
  rob_frac_terms %in% unique(unlist(lapply(rob_frac_ct, names)))
]

rob_frac_rows <- make_stats_rows(
  models   = rob_frac_models,
  controls = rep("Yes", length(rob_frac_models))
)

write_fixest_latex_table(
  models        = rob_frac_models,
  file          = file.path(k_results, "knowledge_fraction_robustness_table.tex"),
  caption       = "Robustness checks using fraction-based knowledge measures",
  label         = "tab:knowledge_fraction_robustness",
  header_groups = c("Fraction over all answers" = 2, "Fraction excluding IDK" = 2),
  column_labels = c("(1)", "(2)", "(3)", "(4)"),
  dict          = term_dict_common,
  term_order    = rob_frac_term_order,
  add_rows      = rob_frac_rows,
  notes = paste(
    "Columns (1) and (2) use as dependent variable the average fraction score computed over all possible answers for each question.",
    "Columns (3) and (4) use as dependent variable the average fraction score computed excluding explicit I don't know answers from the denominator.",
    "For closed-ended questions, columns (1)-(2) weight correct answers by the total number of possible options in each item; for open-ended questions, scores are normalized by the maximum value of 2.",
    "Columns (2) and (4) restrict the sample to Lecture and Game only.",
    "The omitted category is Control in columns (1) and (3), and Lecture in columns (2) and (4).",
    "Standard errors clustered at the \\texttt{cluster\\_id} level are reported in parentheses.",
    "Significance markers are based on conventional clustered p-values.",
    signif_note
  )
)


#########################
## APPENDIX
#########################

## Helper functions for ANOVA / ANCOVA / Wilcoxon tables
fmt_num_plain <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x)) return("---")
  formatC(x, format = "f", digits = digits)
}

fmt_num_sci <- function(x, digits = 2) {
  if (length(x) == 0 || is.na(x)) return("---")
  formatC(x, format = "e", digits = digits)
}

p_stars_plain <- function(p) {
  if (length(p) == 0 || is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1)  return("*")
  ""
}

fmt_p_plain <- function(p, digits = 3, eps = 1e-04) {
  if (length(p) == 0 || is.na(p)) return("---")
  out   <- gsub(" ", "", format.pval(p, digits = digits, eps = eps))
  stars <- p_stars_plain(p)
  if (stars == "") out else paste0(out, " ", stars)
}

tex_row <- function(x) paste0(paste(x, collapse = " & "), " \\\\")

pairwise_wilcox_to_df <- function(obj) {
  as.data.frame(as.table(obj$p.value)) %>%
    dplyr::rename(group1 = Var1, group2 = Var2, p_value = Freq) %>%
    tidyr::drop_na(p_value) %>%
    dplyr::mutate(
      comparison    = paste(group1, "--", group2),
      p_value_label = sapply(p_value, fmt_p_plain)
    ) %>%
    dplyr::select(comparison, p_value, p_value_label)
}

get_tukey_contrasts_full <- function(model) {
  emm <- emmeans::emmeans(model, ~ Gruppo)

  ct <- emmeans::contrast(
    emm,
    method = list(
      "Game -- Control"    = c(-1, 0, 1),
      "Lecture -- Control" = c(-1, 1, 0),
      "Lecture -- Game"    = c(0, 1, -1)
    ),
    adjust = "tukey"
  )

  as.data.frame(summary(ct)) %>%
    dplyr::select(contrast, estimate, SE, df, t.ratio, p.value)
}

write_anova_tukey_table <- function(anova_df, tukey_df, file, caption, label, notes) {
  lines <- c(
    "\\begin{table}[ht]", "\\centering",
    paste0("\\caption{", caption, "}"),
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    "\\textbf{Source} & \\textbf{Df} & \\textbf{Sum Sq} & \\textbf{Mean Sq} & \\textbf{Pr($>F$)} \\\\",
    "\\midrule"
  )

  for (i in seq_len(nrow(anova_df))) {
    lines <- c(lines, tex_row(c(
      anova_df$Source[i], anova_df$Df[i],
      anova_df$SumSq[i], anova_df$MeanSq[i], anova_df$PValue[i]
    )))
  }

  lines <- c(
    lines,
    "\\midrule",
    "\\multicolumn{5}{l}{\\textbf{Tukey Post-Hoc Comparisons}} \\\\",
    "\\midrule",
    "Comparison & Estimate & Std. Error & t value & Pr($>|t|$) \\\\",
    "\\midrule"
  )

  for (i in seq_len(nrow(tukey_df))) {
    lines <- c(lines, tex_row(c(
      tukey_df$contrast[i], tukey_df$estimate[i],
      tukey_df$SE[i], tukey_df$t_ratio[i], tukey_df$p_value[i]
    )))
  }

  lines <- c(
    lines,
    "\\bottomrule", "\\end{tabular}",
    paste0("\\label{", label, "}"),
    "{\\begin{flushleft}\\parbox{\\textwidth}{\\tiny \\hspace*{0.1cm}",
    paste0("\\textit{Notes:}} ", notes),
    "\\item \\sym{*} $p<0.1$, \\sym{**} $p<0.05$, \\sym{***} $p<0.01$.",
    "\\end{flushleft}}", "\\end{table}"
  )

  writeLines(lines, con = file)
}

write_ancova_tukey_table <- function(ancova_df, tukey_df, file, caption, label, notes) {
  lines <- c(
    "\\begin{table}[ht]", "\\centering",
    paste0("\\caption{", caption, "}"),
    "\\begin{tabular}{lrrrrr}",
    "\\toprule",
    "\\textbf{Source} & \\textbf{Df} & \\textbf{Sum Sq} & \\textbf{Mean Sq} & \\textbf{F-value} & \\textbf{Pr($>F$)} \\\\",
    "\\midrule"
  )

  for (i in seq_len(nrow(ancova_df))) {
    lines <- c(lines, tex_row(c(
      ancova_df$Source[i], ancova_df$Df[i], ancova_df$SumSq[i],
      ancova_df$MeanSq[i], ancova_df$FValue[i], ancova_df$PValue[i]
    )))
  }

  lines <- c(
    lines,
    "\\midrule",
    "\\multicolumn{6}{l}{\\textbf{Tukey HSD Post-Hoc Comparisons for Gruppo}} \\\\",
    "\\midrule",
    "Contrast & Estimate & Std. Error & t-ratio & p-adj & \\\\",
    "\\midrule"
  )

  for (i in seq_len(nrow(tukey_df))) {
    lines <- c(lines, tex_row(c(
      tukey_df$contrast[i], tukey_df$estimate[i], tukey_df$SE[i],
      tukey_df$t_ratio[i], tukey_df$p_value[i], ""
    )))
  }

  lines <- c(
    lines,
    "\\bottomrule", "\\end{tabular}",
    paste0("\\label{", label, "}"),
    "{\\begin{flushleft}\\parbox{\\textwidth}{\\tiny \\hspace*{0.1cm}",
    paste0("\\textit{Notes:}} ", notes),
    "\\item \\sym{*} $p<0.1$, \\sym{**} $p<0.05$, \\sym{***} $p<0.01$.",
    "\\end{flushleft}}", "\\end{table}"
  )

  writeLines(lines, con = file)
}

write_ancova_only_table <- function(df, file, caption, label, notes) {
  lines <- c(
    "\\begin{table}[htbp]", "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\begin{tabular}{lrrrr}",
    "\\hline",
    "\\textbf{Variable} & \\textbf{Df} & \\textbf{Sum Sq} & \\textbf{F} & \\textbf{p-value} \\\\",
    "\\hline"
  )

  for (i in seq_len(nrow(df))) {
    lines <- c(lines, tex_row(c(
      df$Source[i], df$Df[i], df$SumSq[i], df$FValue[i], df$PValue[i]
    )))
  }

  lines <- c(
    lines,
    "\\hline", "\\end{tabular}",
    "{\\begin{flushleft}\\parbox{\\textwidth}{\\tiny \\hspace*{0.1cm}",
    paste0("\\textit{Notes:}} ", notes),
    "\\item \\sym{*} $p<0.1$, \\sym{**} $p<0.05$, \\sym{***} $p<0.01$.",
    "\\end{flushleft}}", "\\end{table}"
  )

  writeLines(lines, con = file)
}

write_pairwise_wilcox_table <- function(df, file, caption, label, notes) {
  lines <- c(
    "\\begin{table}[htbp]", "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\begin{tabular}{lc}",
    "\\toprule",
    "\\textbf{Comparison} & \\textbf{Adjusted p-value} \\\\",
    "\\midrule"
  )

  for (i in seq_len(nrow(df))) {
    lines <- c(lines, tex_row(c(df$comparison[i], df$p_value_label[i])))
  }

  lines <- c(
    lines,
    "\\bottomrule", "\\end{tabular}",
    "{\\begin{flushleft}\\parbox{\\textwidth}{\\tiny \\hspace*{0.1cm}",
    paste0("\\textit{Notes:}} ", notes),
    "\\item \\sym{*} $p<0.1$, \\sym{**} $p<0.05$, \\sym{***} $p<0.01$.",
    "\\end{flushleft}}", "\\end{table}"
  )

  writeLines(lines, con = file)
}

## 10. Effect on knowledge controlling for mechanisms
make_mm_formula <- function(mech_vars) {
  rhs <- paste(
    "Gruppo + Sesso + Età + Giochi.tavolo + INTELLIGENZA + PRE.CONOSCENZA",
    paste(mech_vars, collapse = " + "),
    sep = " + "
  )
  as.formula(paste0("CONOSCENZE ~ ", rhs, " | Scuola + grade"))
}

mm_list <- setNames(
  lapply(mechanism_vars, function(v) {
    feols(make_mm_formula(v), data = KnowledgeNC, cluster = ~cluster_id)
  }),
  paste0("mm", 11:17)
)

mm <- feols(
  make_mm_formula(mechanism_vars),
  data    = KnowledgeNC,
  cluster = ~cluster_id
)

mm_models <- c(mm_list, list(mm = mm))

mm_rows <- make_stats_rows(
  models     = mm_models,
  controls   = rep("Yes", length(mm_models)),
  extra_rows = list(
    "Mechanism control" = c("R11", "R12", "R13", "R14", "R15", "R16", "R17", "All")
  )
)

write_fixest_latex_table(
  models        = mm_models,
  file          = file.path(m_results, "knowledge_mechanisms_controls_table.tex"),
  caption       = "Regression coefficients on acquired knowledge controlling for mechanisms",
  label         = "tab:knowledge_mechanisms_controls",
  header_groups = c("Lecture vs Game" = 8),
  column_labels = paste0("(", 1:8, ")"),
  dict          = term_dict_mechanisms,
  term_order    = c(
    "GruppoGioco", "SessoMaschio", "SessoPreferisconondirlo",
    "Età", "Giochi.tavolo", "INTELLIGENZA", "PRE.CONOSCENZA",
    mechanism_vars
  ),
  add_rows = mm_rows,
  notes = paste(
    "Each column reports a separate OLS model estimated on the treated sample only, where the dependent variable is the aggregate knowledge score.",
    "The omitted category is Lecture, so the coefficient on Game is interpreted as Game relative to Lecture.",
    "Columns (1)-(7) add one mechanism at a time, while column (8) includes all mechanisms jointly.",
    "Standard errors clustered at the \\texttt{cluster\\_id} level are reported in parentheses.",
    "Significance markers are based on conventional clustered p-values.",
    signif_note
  )
)

## 11. Diagnostics: Shapiro-Wilk, Levene, Kruskal-Wallis
shapiro_control <- shapiro.test(subset(Knowledge, Gruppo == "Controllo")$CONOSCENZE)
shapiro_lecture <- shapiro.test(subset(Knowledge, Gruppo == "Lezione")$CONOSCENZE)
shapiro_game    <- shapiro.test(subset(Knowledge, Gruppo == "Gioco")$CONOSCENZE)

levene_knowledge <- car::leveneTest(CONOSCENZE ~ Gruppo, data = Knowledge)

kw_full <- kruskal.test(CONOSCENZE ~ Gruppo, data = Knowledge)
kw_nc   <- kruskal.test(CONOSCENZE ~ Gruppo, data = KnowledgeNC)

diagnostics_df <- data.frame(
  test = c(
    "Shapiro Control", "Shapiro Lecture", "Shapiro Game",
    "Levene", "Kruskal-Wallis full sample", "Kruskal-Wallis Lecture vs Game"
  ),
  statistic = c(
    unname(shapiro_control$statistic), unname(shapiro_lecture$statistic),
    unname(shapiro_game$statistic),    levene_knowledge[1, "F value"],
    unname(kw_full$statistic),         unname(kw_nc$statistic)
  ),
  p_value = c(
    shapiro_control$p.value, shapiro_lecture$p.value, shapiro_game$p.value,
    levene_knowledge[1, "Pr(>F)"],
    kw_full$p.value, kw_nc$p.value
  )
)

write.csv(
  diagnostics_df,
  file      = file.path(k_results, "knowledge_diagnostics.csv"),
  row.names = FALSE
)

## 12. Kruskal-Wallis plot
# Note: output filename corrected from kruswal-wallis (typo) to kruskal-wallis
kw_plot <- ggbetweenstats(
  data                  = Knowledge,
  x                     = Gruppo,
  y                     = CONOSCENZE,
  type                  = "nonparametric",
  var.equal             = TRUE,
  plot.type             = "box",
  pairwise.comparisons  = TRUE,
  pairwise.display      = "significant",
  centrality.plotting   = FALSE,
  bf.message            = FALSE
)

ggsave(
  filename = file.path(k_results, "kruskal-wallis_plot.pdf"),
  plot     = kw_plot,
  width    = 13,
  height   = 9
)

ggsave(
  filename = file.path(k_results, "kruskal-wallis_plot.png"),
  plot     = kw_plot,
  width    = 13,
  height   = 9,
  dpi      = 300
)

## 13. One-way ANOVA + Tukey
anova_model <- aov(CONOSCENZE ~ Gruppo, data = Knowledge)
anova_tab <- summary(anova_model)[[1]] %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Source") %>%
  dplyr::mutate(
    Source = dplyr::recode(Source, "Gruppo" = "Treatment Group", "Residuals" = "Residuals"),
    Df     = as.character(Df),
    SumSq  = sapply(`Sum Sq`, fmt_num_plain, digits = 1),
    MeanSq = sapply(`Mean Sq`, fmt_num_plain, digits = 2),
    PValue = sapply(`Pr(>F)`, fmt_p_plain)
  ) %>%
  dplyr::select(Source, Df, SumSq, MeanSq, PValue)

anova_tukey_tab <- get_tukey_contrasts_full(anova_model) %>%
  dplyr::transmute(
    contrast = contrast,
    estimate = sapply(estimate, fmt_num_plain, digits = 4),
    SE       = sapply(SE, fmt_num_plain, digits = 4),
    t_ratio  = sapply(t.ratio, fmt_num_plain, digits = 3),
    p_value  = sapply(p.value, fmt_p_plain)
  )

write_anova_tukey_table(
  anova_df = anova_tab,
  tukey_df = anova_tukey_tab,
  file     = file.path(k_results, "anova_tukey_knowledge.tex"),
  caption  = "Results of ANOVA and Tukey Post-Hoc Test",
  label    = "tab:anova_tukey",
  notes    = "The table reports the result for the ANOVA and Tukey test without the use of stratified, clustered and bootstrap standard errors."
)

## 14. ANCOVA + Tukey
ancova_model <- lm(
  CONOSCENZE ~ Gruppo + Sesso + `Età` + Giochi.tavolo +
    INTELLIGENZA + PRE.CONOSCENZA + Scuola + grade,
  data = Knowledge
)

ancova_tab_raw <- car::Anova(ancova_model, type = 2) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Source")

ancova_tab <- ancova_tab_raw %>%
  dplyr::mutate(
    Source = dplyr::recode(
      Source,
      "Gruppo"         = "Treatment",       "Sesso"   = "Gender",
      "Età"            = "Age",             "Giochi.tavolo" = "Board games experience",
      "INTELLIGENZA"   = "Riddles",         "PRE.CONOSCENZA" = "Baseline knowledge",
      "Scuola"         = "School",          "grade"   = "Cohort",
      "Residuals"      = "Residuals"
    ),
    MeanSq_num = `Sum Sq` / Df,
    Df     = as.character(Df),
    SumSq  = sapply(`Sum Sq`, fmt_num_plain, digits = 1),
    MeanSq = sapply(MeanSq_num, fmt_num_plain, digits = 2),
    FValue = sapply(`F value`, fmt_num_plain, digits = 3),
    PValue = sapply(`Pr(>F)`, fmt_p_plain)
  ) %>%
  dplyr::select(Source, Df, SumSq, MeanSq, FValue, PValue)

ancova_tukey_tab <- get_tukey_contrasts_full(ancova_model) %>%
  dplyr::transmute(
    contrast = contrast,
    estimate = sapply(estimate, fmt_num_plain, digits = 3),
    SE       = sapply(SE, fmt_num_plain, digits = 3),
    t_ratio  = sapply(t.ratio, fmt_num_plain, digits = 3),
    p_value  = sapply(p.value, fmt_p_plain)
  )

write_ancova_tukey_table(
  ancova_df = ancova_tab,
  tukey_df  = ancova_tukey_tab,
  file      = file.path(k_results, "ancova_tukey_knowledge.tex"),
  caption   = "ANCOVA and Tukey Post-Hoc Results for the Knowledge Score",
  label     = "tab:ancova_tukey",
  notes     = "The table reports the result for the ANCOVA and Tukey test without the use of cluster bootstrap standard errors."
)

## 15. ANCOVA on self-perceived acquired knowledge (treated sample only)
ancova18_model <- lm(
  Risposta18 ~ Gruppo + Sesso + `Età` + Giochi.tavolo +
    INTELLIGENZA + PRE.CONOSCENZA + Scuola + grade,
  data = KnowledgeNC
)

ancova18_tab_raw <- car::Anova(ancova18_model, type = 2) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Source")

n_excluded_ancova18 <- nrow(KnowledgeNC) - nobs(ancova18_model)

ancova18_tab <- ancova18_tab_raw %>%
  dplyr::mutate(
    Source = dplyr::recode(
      Source,
      "Gruppo"         = "Group",         "Sesso"          = "Gender",
      "Età"            = "Age",           "Giochi.tavolo"  = "Game activity",
      "INTELLIGENZA"   = "Cognitive ability", "PRE.CONOSCENZA" = "Pre-knowledge",
      "Scuola"         = "School",        "grade"          = "Cohort",
      "Residuals"      = "Residuals"
    ),
    Df     = as.character(Df),
    SumSq  = sapply(`Sum Sq`, fmt_num_plain, digits = 3),
    FValue = sapply(`F value`, fmt_num_plain, digits = 3),
    PValue = sapply(`Pr(>F)`, fmt_p_plain)
  ) %>%
  dplyr::select(Source, Df, SumSq, FValue, PValue)

write_ancova_only_table(
  df      = ancova18_tab,
  file    = file.path(k_results, "ancova_selfperceived_knowledge.tex"),
  caption = "Self-perceived Acquired Knowledge, ANCOVA",
  label   = "tab:ancova18",
  notes   = paste0(n_excluded_ancova18, " observations have been excluded due to missing values.")
)

## 16. Appendix figure: ANCOVA adjusted mean differences for mechanisms
mechanism_labels <- c(
  "Risposta11" = "Mechanism 1", "Risposta12" = "Mechanism 2",
  "Risposta13" = "Mechanism 3", "Risposta14" = "Mechanism 4",
  "Risposta15" = "Mechanism 5", "Risposta16" = "Mechanism 6",
  "Risposta17" = "Mechanism 7"
)

run_mechanism_ancova <- function(y, data) {
  fml <- as.formula(paste0(
    y,
    " ~ Gruppo + Sesso + `Età` + Giochi.tavolo + ",
    "INTELLIGENZA + PRE.CONOSCENZA + Scuola + grade"
  ))
  lm(fml, data = data)
}

mechanism_ancova_models <- setNames(
  lapply(mechanism_vars, run_mechanism_ancova, data = KnowledgeNC),
  mechanism_vars
)

mechanism_ancova_plot_df <- purrr::map_dfr(names(mechanism_ancova_models), function(v) {
  mod <- mechanism_ancova_models[[v]]

  ct <- emmeans::contrast(
    emmeans::emmeans(mod, ~ Gruppo),
    method = list("Game - Lecture" = c(-1, 1)),
    adjust = "none"
  )

  out <- as.data.frame(summary(ct, infer = TRUE))

  data.frame(
    mechanism     = v,
    mechanism_lab = unname(mechanism_labels[v]),
    estimate      = out$estimate[1],
    se            = out$SE[1],
    df            = out$df[1],
    ci_low        = out$lower.CL[1],
    ci_high       = out$upper.CL[1],
    p_value       = out$p.value[1]
  )
})

mechanism_ancova_plot_df <- mechanism_ancova_plot_df %>%
  mutate(
    mechanism_lab = factor(mechanism_lab, levels = rev(unname(mechanism_labels))),
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

p_mechanisms_ancova <- ggplot(
  mechanism_ancova_plot_df,
  aes(x = estimate, y = mechanism_lab)
) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey40") +
  geom_errorbarh(
    aes(xmin = ci_low, xmax = ci_high),
    height = 0.15, linewidth = 0.8, color = "#4C78A8"
  ) +
  geom_point(size = 2.8, color = "#4C78A8") +
  geom_text(aes(label = sig, x = ci_high), nudge_x = 0.03, size = 4) +
  labs(x = "Adjusted mean difference (Game - Lecture)", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey88"),
    axis.ticks         = element_blank()
  )

ggsave(
  filename = file.path(k_results, "ANCOVA_Mechanisms.pdf"),
  plot     = p_mechanisms_ancova,
  width    = 8.5,
  height   = 5.5
)

ggsave(
  filename = file.path(k_results, "ANCOVA_Mechanisms.png"),
  plot     = p_mechanisms_ancova,
  width    = 8.5,
  height   = 5.5,
  dpi      = 300
)

## 17. Multilevel regression
multi <- lmerTest::lmer(
  CONOSCENZE ~ Gruppo + Sesso + Età + Giochi.tavolo +
    INTELLIGENZA + PRE.CONOSCENZA + (1 + Gruppo | cluster_id),
  data = Knowledge
)

multinoc <- lmerTest::lmer(
  CONOSCENZE ~ Gruppo + Sesso + Età + Giochi.tavolo +
    INTELLIGENZA + PRE.CONOSCENZA + (1 + Gruppo | cluster_id),
  data = KnowledgeNC
)

multi_models <- list(multi, multinoc)

get_lmer_ct <- function(model) {
  sm <- summary(model)
  ct <- as.data.frame(sm$coefficients)
  ct$term <- rownames(ct)
  rownames(ct) <- NULL
  names(ct) <- c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)", "term")
  ct %>% dplyr::select(term, Estimate, `Std. Error`, df, `t value`, `Pr(>|t|)`)
}

make_multilevel_stats_rows <- function(models, controls, extra_rows = list()) {
  nobs_vec <- sapply(models, nobs)
  aic_vec  <- sapply(models, AIC)
  bic_vec  <- sapply(models, BIC)

  c(
    list(
      "\\rule{0pt}{1.2em}Controls"           = controls,
      "Random intercept"                      = rep("Yes", length(models)),
      "Random slope for treatment"            = rep("Yes", length(models)),
      "Num.Obs."                              = sapply(nobs_vec, function(x) fmt_num(x, 0)),
      "AIC"                                   = sapply(aic_vec,  function(x) fmt_num(x, 2)),
      "BIC"                                   = sapply(bic_vec,  function(x) fmt_num(x, 2))
    ),
    extra_rows
  )
}

write_lmer_latex_table <- function(models,
                                   file,
                                   caption,
                                   label,
                                   header_groups,
                                   notes,
                                   column_labels = NULL,
                                   dict          = NULL,
                                   term_order    = NULL,
                                   add_rows      = list()) {
  n_models <- length(models)
  ct_list  <- lapply(models, get_lmer_ct)

  if (is.null(term_order)) {
    all_terms  <- unique(unlist(lapply(ct_list, function(x) x$term)))
    sex_terms  <- sort(all_terms[grepl("^Sesso", all_terms)])
    term_order <- c(
      "GruppoLezione", "GruppoGioco", sex_terms,
      "Età", "Giochi.tavolo", "PRE.CONOSCENZA", "INTELLIGENZA"
    )
    term_order <- term_order[term_order %in% all_terms]
  }

  lines <- make_table_header(
    n_models = n_models, caption = caption, label = label,
    header_groups = header_groups, column_labels = column_labels
  )

  for (term in term_order) {
    coef_line <- c(label_term(term, dict))
    se_line   <- c("")

    for (j in seq_along(ct_list)) {
      ct <- ct_list[[j]]

      if (term %in% ct$term) {
        row_j     <- ct[ct$term == term, , drop = FALSE]
        b         <- row_j$Estimate[1]
        se        <- row_j$`Std. Error`[1]
        p         <- row_j$`Pr(>|t|)`[1]
        coef_line <- c(coef_line, paste0(fmt_num(b, 3), star_fun(p)))
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

multi_rows <- make_multilevel_stats_rows(
  models   = multi_models,
  controls = c("Yes", "Yes")
)

multi_term_dict <- c(
  "GruppoLezione"             = "Lecture",
  "GruppoGioco"               = "Game",
  "SessoMaschio"              = "Male",
  "SessoPreferisco non dirlo" = "Prefer not to say the gender",
  "Età"                       = "Age",
  "Giochi.tavolo"             = "Game activity",
  "INTELLIGENZA"              = "Cognitive ability",
  "PRE.CONOSCENZA"            = "Baseline knowledge"
)

write_lmer_latex_table(
  models        = multi_models,
  file          = file.path(k_results, "multilevel_regression_knowledge.tex"),
  caption       = "Multilevel regression model",
  label         = "tab:multilevel_knowledge",
  header_groups = c("Aggregate knowledge score" = 2),
  column_labels = c("(1)", "(2)"),
  dict          = multi_term_dict,
  term_order    = c(
    "GruppoLezione", "GruppoGioco",
    "SessoMaschio", "SessoPreferisco non dirlo",
    "Età", "Giochi.tavolo", "INTELLIGENZA", "PRE.CONOSCENZA"
  ),
  add_rows = multi_rows,
  notes = paste(
    "Columns (1) and (2) report linear mixed-effects models for the aggregate knowledge score.",
    "Both models include random intercepts and random slopes for treatment at the \\texttt{cluster\\_id} level.",
    "Column (1) uses the full sample, while column (2) restricts the sample to Lecture and Game only.",
    "The omitted category is Control in column (1) and Lecture in column (2).",
    "Standard errors are reported in parentheses.",
    "P-values are based on Satterthwaite approximations provided by \\texttt{lmerTest}.",
    signif_note
  )
)
