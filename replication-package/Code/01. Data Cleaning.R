####################
## DATA CLEANING (NEW)
# Reads from raw Excel (Raw data.xlsx).
# Computes Denti corretto and Mani corretto from raw Lava denti / Lava mani
# Creates Spillover in Behavior POST from Parlato con altre classi.
# Outputs: Knowledge.csv, Attitudes.csv, Dif.csv -> Data/Processed/
####################

## 00. Packages and directory
library(readxl)
library(readr)
library(tidyverse)
library(dplyr)

base_dir <- "~/Desktop/Ricerche/WaterGame/Replication"
raw_dir  <- file.path(base_dir, "Data/Raw")
proc_dir <- file.path(base_dir, "Data/Processed")
dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)

## 01. Upload raw data
Behavior_PRE <- read_excel(
  file.path(raw_dir, "Raw data.xlsx"),
  sheet = "Behavior PRE"
)

Behavior_POST <- read_excel(
  file.path(raw_dir, "Raw data.xlsx"),
  sheet = "Behavior POST"
)

Knowledge <- read_excel(
  file.path(raw_dir, "Raw data.xlsx"),
  sheet = "Conoscenze"
)

## 02. Create cluster_id and grade
add_ids <- function(df) {
  df %>%
    mutate(
      cluster_id = interaction(Scuola, Classe, drop = TRUE),
      grade      = as.integer(sub("^([0-9]+).*", "\\1", Classe))
    )
}

Knowledge     <- add_ids(Knowledge)
Behavior_PRE  <- add_ids(Behavior_PRE)
Behavior_POST <- add_ids(Behavior_POST)

## 03. Recode Likert scale (reversal formula from Excel)
# 5->1, 4->2, 3->3, 2->4, 1->5
recode_likert <- function(x) {
  dplyr::case_when(
    x == 5 ~ 1,
    x == 4 ~ 2,
    x == 1 ~ 5,
    x == 2 ~ 4,
    TRUE   ~ as.numeric(x)
  )
}

Behavior_PRE <- Behavior_PRE %>%
  mutate(
    `Denti corretto` = recode_likert(as.numeric(`Lava denti`)),
    `Mani corretto`  = recode_likert(as.numeric(`Lava mani`))
  )

Behavior_POST <- Behavior_POST %>%
  mutate(
    `Denti corretto` = recode_likert(as.numeric(`Lava denti`)),
    `Mani corretto`  = recode_likert(as.numeric(`Lava mani`))
  )

## 04. Create Spillover indicator in POST
# Spillover = 1 if respondent reported talking with students from other classes
Behavior_POST <- Behavior_POST %>%
  mutate(
    Spillover = if_else(`Parlato con altre classi` == "Sì", 1L, 0L)
  )

## 05. Recode attitude variables
recode_attitude <- function(x) {
  dplyr::case_when(
    x == "Sì" ~ "Yes",
    x == "No" ~ "No",
    TRUE      ~ "DontKnow"
  ) %>%
    factor(levels = c("No", "DontKnow", "Yes"))
}

Behavior_PRE <- Behavior_PRE %>%
  mutate(
    attitude1_cat = recode_attitude(`Daresti acqua`),
    attitude2_cat = recode_attitude(`Importanza tecnologie`),
    attitude3_cat = recode_attitude(`Episodi eventi estremi`)
  )

Behavior_POST <- Behavior_POST %>%
  mutate(
    attitude1_cat = recode_attitude(`Daresti acqua`),
    attitude2_cat = recode_attitude(`Importanza tecnologie`),
    attitude3_cat = recode_attitude(`Episodi eventi estremi`)
  )

## 06. Build Controls (demographics from PRE)
Controls <- Behavior_PRE %>%
  dplyr::select(
    "Codice", "cluster_id", "Sesso", "Età",
    "Giochi tavolo", "INTELLIGENZA", "PRE-CONOSCENZA", "grade"
  )

Spill <- Behavior_POST %>%
  dplyr::select(
    "Codice", "cluster_id", "Spillover", "grade"
  )

## 07. Build final Knowledge dataset
# Merge Conoscenze with Controls so each student row has demographics
Knowledge <- merge(
  Knowledge,
  Controls,
  by   = c("Codice", "cluster_id", "grade"),
  all.x = TRUE
)

Knowledge <- Knowledge %>%
  mutate(
    Gruppo     = as.factor(Gruppo),
    Sesso      = as.factor(Sesso),
    Scuola     = as.factor(Scuola),
    grade      = as.factor(grade),
    cluster_id = as.factor(cluster_id)
  )

write.csv(
  Knowledge,
  file.path(proc_dir, "Knowledge.csv"),
  row.names = FALSE
)

## 08. Merge Behavior datasets with Controls and Spill
# POST receives demographic controls; PRE receives spillover indicators
Behavior_POST <- Behavior_POST %>%
  left_join(Controls, by = c("Codice", "cluster_id", "grade"))

Behavior_PRE <- Behavior_PRE %>%
  left_join(Spill, by = c("Codice", "cluster_id", "grade"))

## 09. Combine PRE and POST into long format (keep only shared columns)
common_vars <- intersect(names(Behavior_PRE), names(Behavior_POST))

pre  <- Behavior_PRE  %>%
  dplyr::select(all_of(common_vars)) %>%
  mutate(Periodo = "PRE",  Post = 0)

post <- Behavior_POST %>%
  dplyr::select(all_of(common_vars)) %>%
  mutate(Periodo = "POST", Post = 1)

# Coerce behavior variables to numeric before binding
num_vars <- c(
  "Lava denti",
  "Lava mani",
  "Parla genitori emergenze",
  "Mani corretto",
  "Spillover",
  "COMPORTAMENTI",
  "Denti corretto",
  "Parla genitori spreco",
  "Parla amici spreco"
)

for (v in num_vars) {
  if (v %in% names(pre))  pre[[v]]  <- as.numeric(pre[[v]])
  if (v %in% names(post)) post[[v]] <- as.numeric(post[[v]])
}

Behavior_long <- bind_rows(pre, post) %>%
  mutate(
    ID         = interaction(Codice, cluster_id, drop = TRUE),
    Periodo    = factor(Periodo, levels = c("PRE", "POST")),
    Group      = as.factor(Group),
    Sesso      = as.factor(Sesso),
    Scuola     = as.factor(Scuola),
    grade      = as.factor(grade),
    cluster_id = as.factor(cluster_id)
  )

## 10. Build Attitudes dataset (long format, with Spillover appended for POST)
Attitudes_long <- Behavior_long %>%
  dplyr::select(
    ID, Codice, Periodo, Post,
    Group, cluster_id, Scuola, grade,
    Sesso, `Età`, `Giochi tavolo`, INTELLIGENZA, `PRE-CONOSCENZA`,
    Spillover,
    attitude1_cat, attitude2_cat, attitude3_cat
  )

write.csv(
  Attitudes_long,
  file.path(proc_dir, "Attitudes.csv"),
  row.names = FALSE
)

## 11. Build Dif dataset (first differences in behavior outcomes per student)
Dif <- Behavior_long %>%
  group_by(ID) %>%
  summarise(
    difBehavior  = COMPORTAMENTI[Periodo == "POST"] -
                   COMPORTAMENTI[Periodo == "PRE"],
    diffDenti    = `Denti corretto`[Periodo == "POST"] -
                   `Denti corretto`[Periodo == "PRE"],
    diffMani     = `Mani corretto`[Periodo == "POST"] -
                   `Mani corretto`[Periodo == "PRE"],
    diffEmerg    = `Parla genitori emergenze`[Periodo == "POST"] -
                   `Parla genitori emergenze`[Periodo == "PRE"],
    diffSpreco   = `Parla genitori spreco`[Periodo == "POST"] -
                   `Parla genitori spreco`[Periodo == "PRE"],
    diffAmici    = `Parla amici spreco`[Periodo == "POST"] -
                   `Parla amici spreco`[Periodo == "PRE"],
    Group        = first(Group),
    cluster_id   = first(cluster_id),
    Sex          = first(Sesso),
    Age          = first(`Età`),
    GameA        = first(`Giochi tavolo`),
    Cog          = first(INTELLIGENZA),
    PreK         = first(`PRE-CONOSCENZA`),
    Scuola       = first(Scuola),
    grade        = first(grade),
    Spillover    = first(Spillover),
    .groups = "drop"
  )

write.csv(
  Dif,
  file.path(proc_dir, "Dif.csv"),
  row.names = FALSE
)

## 12. Export behavior long format for descriptive figure
# Derived from Behavior_long (assembled via common_vars in section 09),
# which reliably contains the five behavior columns in both PRE and POST.
# Avoids fragile range-selection on the modified Behavior_PRE/Behavior_POST
# dataframes (which have been altered by left_join calls in section 08).
behavior_long_export <- Behavior_long %>%
  dplyr::select(
    Group, Periodo,
    `Lava denti`, `Lava mani`,
    `Parla genitori emergenze`, `Parla genitori spreco`, `Parla amici spreco`
  )

write.csv(
  behavior_long_export,
  file.path(proc_dir, "behavior_long.csv"),
  row.names = FALSE
)


