#######################
## EFFECT ON ATTITUDES (NEW)
# Input:   Data/Processed/Attitudes.csv
# Output:  Paper/Results attitudes/attitudes_multinom_two_panel.tex
#
# Fits multinomial logit DID models for three attitude outcomes,
# applies Bonferroni correction, and writes the two-panel summary table
# (Panel A: full sample; Panel B: Lecture vs Game only) cited in the paper.
#######################

## 00. Packages and directories
library(tidyverse)
library(nnet)
library(ggplot2)
library(dplyr)

base_dir  <- "~/Desktop/Ricerche/WaterGame/Replication"
proc_dir  <- file.path(base_dir, "Data/Processed")

a_results <- file.path(base_dir, "Output/Results attitudes")
dir.create(a_results, recursive = TRUE, showWarnings = FALSE)

## 01. Upload and prepare data
# check.names = FALSE preserves column names with spaces (Giochi tavolo, PRE-CONOSCENZA)
# so the formula backtick-quoted names match the actual column names
Attitudes_long <- read.csv(
  file.path(proc_dir, "Attitudes.csv"),
  check.names = FALSE
)

# Re-apply attitude factor in case CSV round-trip dropped the levels
make_attitude_cat <- function(x) {
  x_chr <- as.character(x)
  out <- dplyr::case_when(
    x_chr %in% c("No", "NO", "no")                                  ~ "No",
    x_chr %in% c("DontKnow", "Don't Know", "Non so")                ~ "DontKnow",
    x_chr %in% c("Yes", "YES", "yes", "Sì", "Si")                   ~ "Yes",
    TRUE                                                              ~ NA_character_
  )
  factor(out, levels = c("No", "DontKnow", "Yes"))
}

if (!"attitude1_cat" %in% names(Attitudes_long) && "attitude1" %in% names(Attitudes_long)) {
  Attitudes_long$attitude1_cat <- make_attitude_cat(Attitudes_long$attitude1)
}
if (!"attitude2_cat" %in% names(Attitudes_long) && "attitude2" %in% names(Attitudes_long)) {
  Attitudes_long$attitude2_cat <- make_attitude_cat(Attitudes_long$attitude2)
}
if (!"attitude3_cat" %in% names(Attitudes_long) && "attitude3" %in% names(Attitudes_long)) {
  Attitudes_long$attitude3_cat <- make_attitude_cat(Attitudes_long$attitude3)
}

Attitudes_long <- Attitudes_long %>%
  mutate(
    Group        = factor(Group, levels = c("Controllo", "Lezione", "Gioco")),
    Periodo      = factor(Periodo, levels = c("PRE", "POST")),
    Post         = ifelse(Periodo == "POST", 1, 0),
    Sesso        = as.factor(Sesso),
    Scuola       = as.factor(Scuola),
    grade        = as.factor(grade),
    cluster_id   = as.factor(cluster_id),
    attitude1_cat = factor(attitude1_cat, levels = c("No", "DontKnow", "Yes")),
    attitude2_cat = factor(attitude2_cat, levels = c("No", "DontKnow", "Yes")),
    attitude3_cat = factor(attitude3_cat, levels = c("No", "DontKnow", "Yes"))
  )

Attitudes_long$Group <- relevel(Attitudes_long$Group, ref = "Controllo")

AttitudesNC <- Attitudes_long %>%
  filter(Group != "Controllo") %>%
  mutate(Group = factor(Group, levels = c("Lezione", "Gioco")))

AttitudesNC$Group <- relevel(AttitudesNC$Group, ref = "Lezione")

## 02. Global objects
signif_note <- "*** $p < 0.01$, ** $p < 0.05$, * $p < 0.1$."

attitude_labels <- c(
  "attitude1_cat" = "Cooperation",
  "attitude2_cat" = "Saliency of technologies",
  "attitude3_cat" = "Proximity of extreme events"
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

label_term_att <- function(term) {
  if (term == "GroupLezione")       return("Lecture")
  if (term == "GroupGioco")         return("Game")
  if (term == "Post")               return("Post")
  if (term == "Post:GroupLezione")  return("Post $\\times$ Lecture")
  if (term == "Post:GroupGioco")    return("Post $\\times$ Game")

  if (grepl("^Sesso", term)) {
    lev <- sub("^Sesso", "", term)
    if (grepl("Maschio|Male", lev, ignore.case = TRUE))  return("Male")
    if (grepl("Prefer", lev, ignore.case = TRUE))        return("Prefer not to say the gender")
    return(lev)
  }

  if (term %in% c("Età", "`Età`"))                           return("Age")
  if (term %in% c("Giochi.tavolo", "`Giochi tavolo`", "Giochi tavolo")) return("Game activity")
  if (term %in% c("INTELLIGENZA", "`INTELLIGENZA`"))         return("Cognitive ability")
  if (term %in% c("PRE.CONOSCENZA", "`PRE-CONOSCENZA`", "PRE-CONOSCENZA")) return("Pre-knowledge")

  term
}

extract_multinom_ct <- function(model) {
  sm    <- summary(model)
  coefs <- sm$coefficients
  ses   <- sm$standard.errors

  if (is.null(dim(coefs))) {
    coefs <- matrix(coefs, nrow = 1)
    ses   <- matrix(ses, nrow = 1)
    rownames(coefs) <- rownames(ses) <- model$lev[-1]
    colnames(coefs) <- names(sm$coefficients)
    colnames(ses)   <- names(sm$standard.errors)
  }

  ct <- lapply(seq_len(nrow(coefs)), function(j) {
    out <- data.frame(
      term     = colnames(coefs),
      estimate = as.numeric(coefs[j, ]),
      se       = as.numeric(ses[j, ]),
      stringsAsFactors = FALSE
    )
    out$z       <- out$estimate / out$se
    out$p_value <- 2 * pnorm(abs(out$z), lower.tail = FALSE)
    out
  })

  names(ct) <- rownames(coefs)
  ct
}

run_multinom_did <- function(outcome, data) {
  fml <- as.formula(paste0(
    outcome,
    " ~ Post * Group + Sesso + `Età` + `Giochi tavolo` + ",
    "INTELLIGENZA + `PRE-CONOSCENZA` + Scuola + grade"
  ))

  mod <- nnet::multinom(
    formula   = fml,
    data      = data,
    trace     = FALSE,
    Hess      = TRUE,
    na.action = na.omit
  )

  list(model = mod, coeftable = extract_multinom_ct(mod))
}

get_p_term <- function(ct, outcome_level, term_name) {
  if (!outcome_level %in% names(ct)) return(NA_real_)
  tab <- ct[[outcome_level]]
  if (!term_name %in% tab$term) return(NA_real_)
  tab$p_value[tab$term == term_name][1]
}

get_cell <- function(ct_list, outcome_level, term, custom_pvalues = NULL) {
  if (!outcome_level %in% names(ct_list)) return(c("", ""))
  tab <- ct_list[[outcome_level]]
  if (!term %in% tab$term) return(c("", ""))

  row_j <- tab[tab$term == term, , drop = FALSE]
  p_use <- row_j$p_value[1]

  key <- paste0(outcome_level, "::", term)
  if (!is.null(custom_pvalues) && key %in% names(custom_pvalues) && !is.na(custom_pvalues[[key]])) {
    p_use <- custom_pvalues[[key]]
  }

  c(
    paste0(fmt_num(row_j$estimate[1], 3), star_fun(p_use)),
    paste0("(", fmt_num(row_j$se[1], 3), ")")
  )
}

collect_terms <- function(results_list) {
  unique(unlist(lapply(results_list, function(x) {
    unlist(lapply(x$coeftable, function(df) df$term))
  })))
}

pick_first_term <- function(candidates, all_terms) {
  hit <- candidates[candidates %in% all_terms]
  if (length(hit) == 0) character(0) else hit[1]
}

build_term_order <- function(results_list, sample = c("full", "nc")) {
  sample    <- match.arg(sample)
  all_terms <- collect_terms(results_list)
  sex_terms <- sort(all_terms[grepl("^Sesso", all_terms)])

  age_term  <- pick_first_term(c("Età", "`Età`"), all_terms)
  game_term <- pick_first_term(c("Giochi.tavolo", "`Giochi tavolo`", "Giochi tavolo"), all_terms)
  iq_term   <- pick_first_term(c("INTELLIGENZA", "`INTELLIGENZA`"), all_terms)
  prek_term <- pick_first_term(c("PRE.CONOSCENZA", "`PRE-CONOSCENZA`", "PRE-CONOSCENZA"), all_terms)

  if (sample == "full") {
    preferred <- c(
      "GroupLezione", "GroupGioco", "Post",
      "Post:GroupLezione", "Post:GroupGioco",
      sex_terms, age_term, game_term, iq_term, prek_term
    )
  } else {
    preferred <- c(
      "GroupGioco", "Post", "Post:GroupGioco",
      sex_terms, age_term, game_term, iq_term, prek_term
    )
  }

  preferred[preferred %in% all_terms]
}

make_custom_p_full <- function(results_list) {
  att_names <- names(results_list)

  p_raw_lecture_yes <- sapply(results_list, function(x) get_p_term(x$coeftable, "Yes",      "Post:GroupLezione"))
  p_raw_lecture_dk  <- sapply(results_list, function(x) get_p_term(x$coeftable, "DontKnow", "Post:GroupLezione"))
  p_raw_game_yes    <- sapply(results_list, function(x) get_p_term(x$coeftable, "Yes",      "Post:GroupGioco"))
  p_raw_game_dk     <- sapply(results_list, function(x) get_p_term(x$coeftable, "DontKnow", "Post:GroupGioco"))

  p_bonf_lecture_all <- p.adjust(c(p_raw_lecture_yes, p_raw_lecture_dk), method = "bonferroni")
  p_bonf_game_all    <- p.adjust(c(p_raw_game_yes,    p_raw_game_dk),    method = "bonferroni")

  n_att <- length(att_names)

  p_bonf_lecture_yes <- p_bonf_lecture_all[1:n_att]
  p_bonf_lecture_dk  <- p_bonf_lecture_all[(n_att + 1):(2 * n_att)]
  p_bonf_game_yes    <- p_bonf_game_all[1:n_att]
  p_bonf_game_dk     <- p_bonf_game_all[(n_att + 1):(2 * n_att)]

  custom_p_list <- list()
  for (v in att_names) {
    i <- match(v, att_names)
    custom_p_list[[v]] <- list()
    custom_p_list[[v]][["Yes::Post:GroupLezione"]]      <- unname(p_bonf_lecture_yes[i])
    custom_p_list[[v]][["DontKnow::Post:GroupLezione"]] <- unname(p_bonf_lecture_dk[i])
    custom_p_list[[v]][["Yes::Post:GroupGioco"]]        <- unname(p_bonf_game_yes[i])
    custom_p_list[[v]][["DontKnow::Post:GroupGioco"]]   <- unname(p_bonf_game_dk[i])
  }

  list(custom_p_list = custom_p_list)
}

make_custom_p_nc <- function(results_list) {
  att_names <- names(results_list)

  p_raw_game_yes <- sapply(results_list, function(x) get_p_term(x$coeftable, "Yes",      "Post:GroupGioco"))
  p_raw_game_dk  <- sapply(results_list, function(x) get_p_term(x$coeftable, "DontKnow", "Post:GroupGioco"))

  p_bonf_game_all <- p.adjust(c(p_raw_game_yes, p_raw_game_dk), method = "bonferroni")
  n_att <- length(att_names)

  p_bonf_game_yes <- p_bonf_game_all[1:n_att]
  p_bonf_game_dk  <- p_bonf_game_all[(n_att + 1):(2 * n_att)]

  custom_p_list <- list()
  for (v in att_names) {
    i <- match(v, att_names)
    custom_p_list[[v]] <- list()
    custom_p_list[[v]][["Yes::Post:GroupGioco"]]      <- unname(p_bonf_game_yes[i])
    custom_p_list[[v]][["DontKnow::Post:GroupGioco"]] <- unname(p_bonf_game_dk[i])
  }

  list(custom_p_list = custom_p_list)
}

#################
## MAIN RESULTS
#################

## 04. DID multinomial logit for each attitude (full sample)
attitude_vars <- c("attitude1_cat", "attitude2_cat", "attitude3_cat")
attitude_vars <- attitude_vars[attitude_vars %in% names(Attitudes_long)]

attitude_results <- setNames(
  lapply(attitude_vars, run_multinom_did, data = Attitudes_long),
  attitude_vars
)

term_order_full <- build_term_order(attitude_results, sample = "full")
full_bonf       <- make_custom_p_full(attitude_results)

## 05. DID multinomial logit on treated sample only
attitude_results_nc <- setNames(
  lapply(attitude_vars, run_multinom_did, data = AttitudesNC),
  attitude_vars
)

term_order_nc <- build_term_order(attitude_results_nc, sample = "nc")
nc_bonf       <- make_custom_p_nc(attitude_results_nc)

## 06. Two-panel summary table (the only output cited in the paper)
write_multinom_attitudes_two_panel <- function(results_full,
                                               results_nc,
                                               file,
                                               caption,
                                               label,
                                               custom_p_full = NULL,
                                               custom_p_nc   = NULL,
                                               model_order   = c("attitude1_cat", "attitude2_cat", "attitude3_cat"),
                                               outcomes_order = c("DontKnow", "Yes")) {

  panel_terms_full <- c(
    "GroupLezione", "GroupGioco", "Post",
    "Post:GroupLezione", "Post:GroupGioco"
  )

  panel_terms_nc <- c("GroupGioco", "Post", "Post:GroupGioco")

  group_line <- paste0(
    "\\rule{0pt}{1.2em} & ",
    paste(
      mapply(
        function(nm) paste0("\\multicolumn{", length(outcomes_order), "}{c}{\\textbf{", nm, "}}"),
        unname(attitude_labels[model_order]),
        USE.NAMES = FALSE
      ),
      collapse = " & "
    ),
    " \\\\"
  )

  clines <- character()
  start_col <- 2
  for (i in seq_along(model_order)) {
    end_col   <- start_col + length(outcomes_order) - 1
    clines    <- c(clines, paste0("\\cline{", start_col, "-", end_col, "}"))
    start_col <- end_col + 1
  }

  col_labels <- paste0("(", seq_len(length(model_order) * length(outcomes_order)), ")")

  lines <- c(
    "\\begin{table}[htbp]", "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\fontsize{7pt}{8.4pt}\\selectfont",
    paste0("\\begin{tabular}{l", paste(rep("c", length(model_order) * length(outcomes_order)), collapse = ""), "}"),
    "\\hline", "\\hline",
    group_line,
    paste(clines, collapse = " "),
    paste0("\\rule{0pt}{1.2em} & ", paste(col_labels, collapse = " & "), " \\\\"),
    "\\hline",
    paste0(
      "\\multicolumn{1}{l}{\\textbf{VARIABLES}} & ",
      paste(rep("\\multicolumn{2}{c}{Reference: No}", length(model_order)), collapse = " & "),
      " \\\\"
    ),
    paste0("\\rule{0pt}{1.1em} & ",
           paste(rep(c("DK", "Yes"), length(model_order)), collapse = " & "),
           " \\\\"),
    "\\hline", "\\\\"
  )

  ## Panel A: full sample
  lines <- c(
    lines,
    paste0("\\multicolumn{", 1 + length(model_order) * length(outcomes_order),
           "}{l}{\\textbf{Panel A. Full sample}} \\\\"),
    "\\hline"
  )

  for (term in panel_terms_full) {
    coef_line <- c(label_term_att(term))
    se_line   <- c("")

    for (att in model_order) {
      ct_list  <- results_full[[att]]$coeftable
      custom_p <- if (!is.null(custom_p_full) && att %in% names(custom_p_full)) custom_p_full[[att]] else NULL

      for (lev in outcomes_order) {
        vals      <- get_cell(ct_list, lev, term, custom_pvalues = custom_p)
        coef_line <- c(coef_line, vals[1])
        se_line   <- c(se_line,   vals[2])
      }
    }

    lines <- c(lines, latex_row(coef_line), latex_row(se_line))
  }

  nobs_row_A <- c("Num.Obs.")
  aic_row_A  <- c("AIC")

  for (att in model_order) {
    n_i        <- nrow(results_full[[att]]$model$fitted.values)
    a_i        <- AIC(results_full[[att]]$model)
    nobs_row_A <- c(nobs_row_A, fmt_num(n_i, 0), fmt_num(n_i, 0))
    aic_row_A  <- c(aic_row_A,  fmt_num(a_i, 2), fmt_num(a_i, 2))
  }

  lines <- c(
    lines,
    "\\hline",
    latex_row(c("Fixed Effects", rep("Yes", length(model_order) * length(outcomes_order)))),
    latex_row(c("Controls",      rep("Yes", length(model_order) * length(outcomes_order)))),
    latex_row(nobs_row_A),
    latex_row(aic_row_A),
    "\\\\"
  )

  ## Panel B: Lecture vs Game only
  lines <- c(
    lines,
    paste0("\\multicolumn{", 1 + length(model_order) * length(outcomes_order),
           "}{l}{\\textbf{Panel B. Lecture vs Game}} \\\\"),
    "\\hline"
  )

  for (term in panel_terms_nc) {
    coef_line <- c(label_term_att(term))
    se_line   <- c("")

    for (att in model_order) {
      ct_list  <- results_nc[[att]]$coeftable
      custom_p <- if (!is.null(custom_p_nc) && att %in% names(custom_p_nc)) custom_p_nc[[att]] else NULL

      for (lev in outcomes_order) {
        vals      <- get_cell(ct_list, lev, term, custom_pvalues = custom_p)
        coef_line <- c(coef_line, vals[1])
        se_line   <- c(se_line,   vals[2])
      }
    }

    lines <- c(lines, latex_row(coef_line), latex_row(se_line))
  }

  nobs_row_B <- c("Num.Obs.")
  aic_row_B  <- c("AIC")

  for (att in model_order) {
    n_i        <- nrow(results_nc[[att]]$model$fitted.values)
    a_i        <- AIC(results_nc[[att]]$model)
    nobs_row_B <- c(nobs_row_B, fmt_num(n_i, 0), fmt_num(n_i, 0))
    aic_row_B  <- c(aic_row_B,  fmt_num(a_i, 2), fmt_num(a_i, 2))
  }

  lines <- c(
    lines,
    "\\hline",
    latex_row(c("Fixed Effects", rep("Yes", length(model_order) * length(outcomes_order)))),
    latex_row(c("Controls",      rep("Yes", length(model_order) * length(outcomes_order)))),
    latex_row(nobs_row_B),
    latex_row(aic_row_B),
    "\\hline", "\\hline", "\\end{tabular}",
    "{\\begin{flushleft}\\parbox{\\textwidth}{\\tiny \\hspace*{0.1cm}",
    paste0(
      "\\textit{Notes:}} Table reports multinomial logit DID estimates for the three attitude questions. ",
      "Each pair of columns corresponds to one attitude outcome, with No as the omitted response category; columns labelled DK and Yes report coefficients for DontKnow and Yes relative to No. ",
      "Panel A reports the full sample, where the omitted treatment category is Control. ",
      "Panel B reports the treated sample only, where the omitted treatment category is Lecture; accordingly, the coefficients on Lecture and Post $\\times$ Lecture are not identified and are left blank. ",
      "The omitted time period is PRE. ",
      "Standard errors are the model-based standard errors from the multinomial logit. ",
      "Bonferroni correction is applied to the DID interaction coefficients using the pre-specified families described in the text. ",
      signif_note
    ),
    "\\end{flushleft}}",
    "\\end{table}"
  )

  writeLines(lines, con = file)
}

write_multinom_attitudes_two_panel(
  results_full  = attitude_results,
  results_nc    = attitude_results_nc,
  file          = file.path(a_results, "attitudes_multinom_two_panel.tex"),
  caption       = "Regression coefficients for attitudes",
  label         = "tab:attitudes_two_panel",
  custom_p_full = full_bonf$custom_p_list,
  custom_p_nc   = nc_bonf$custom_p_list,
  model_order   = attitude_vars,
  outcomes_order = c("DontKnow", "Yes")
)
