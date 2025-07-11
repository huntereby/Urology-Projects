# ========================
# Load Libraries
# ========================
library(tidyverse)
library(forcats)
library(ggplot2)

# ========================
# File Paths
# ========================
# Folder containing the one year results
dir_1yr  <- "Results1/Unnamed_Analysis_359371/"   # <--- update if renamed
# Folder containing the five year results
dir_5yr  <- "Results1/ESRD_ED_5_year/"           # <--- update this path

# ========================
# Outcome Mapping (for Outcome_8+)
# ========================
outcome_map <- tribble(
  ~OutcomeNum,     ~ICD10,     ~SubLabel,                                ~MasterLabel,
  "Outcome_8",     "I21",      "Acute myocardial infarction",           "Ischemic Heart Disease",
  "Outcome_9",     "I22",      "Subsequent STEMI/NSTEMI",               "Ischemic Heart Disease",
  "Outcome_10",    "I63",      "Cerebral infarction",                   "Cerebrovascular Complications",
  "Outcome_11",    "I60",      "Subarachnoid hemorrhage",               "Cerebrovascular Complications",
  "Outcome_12",    "I61",      "Intracerebral hemorrhage",              "Cerebrovascular Complications",
  "Outcome_13",    "I62",      "Intracranial hemorrhage",               "Cerebrovascular Complications",
  "Outcome_14",    "I65",      "Pre-cerebral artery occlusion",         "Cerebrovascular Complications",
  "Outcome_15",    "I66",      "Cerebral artery occlusion",             "Cerebrovascular Complications",
  "Outcome_16",    "I67",      "Other cerebrovascular diseases",        "Cerebrovascular Complications",
  "Outcome_17",    "I67",      "Other cerebrovascular diseases",        "Cerebrovascular Complications",
  "Outcome_18",    "I47",      "Paroxysmal tachycardia",                "Arrhythmia",
  "Outcome_19",    "I48",      "Atrial fibrillation",                   "Arrhythmia",
  "Outcome_20",    "I49",      "Other cardiac arrhythmias",             "Arrhythmia",
  "Outcome_21",    "I26",      "Pulmonary embolism",                    "Thrombotic Disorders",
  "Outcome_22",    "I80.0",    "Phlebitis - superficial",               "Thrombotic Disorders",
  "Outcome_23",    "I80.1",    "Phlebitis - femoral vein",              "Thrombotic Disorders",
  "Outcome_24",    "I80.2",    "Phlebitis - deep vessels",              "Thrombotic Disorders",
  "Outcome_25",    "I80.3",    "Phlebitis - unspecified LE",            "Thrombotic Disorders",
  "Outcome_26",    "I80.8",    "Phlebitis - other sites",               "Thrombotic Disorders",
  "Outcome_27",    "I80.9",    "Phlebitis - unspecified site",          "Thrombotic Disorders",
  "Outcome_28",    "G43.0",    "Migraine without aura",                 "Headaches",
  "Outcome_29",    "G43.1",    "Migraine with aura",                    "Headaches",
  "Outcome_30",    "G43.7",    "Chronic migraine",                      "Headaches",
  "Outcome_31",    "G43.8",    "Other migraine",                        "Headaches",
  "Outcome_32",    "346",      "Migraine (ICD-9)",                      "Headaches",
  "Outcome_33",    "I95.0",    "Idiopathic hypotension",                "Hypotension",
  "Outcome_34",    "I95.1",    "Orthostatic hypotension",              "Hypotension",
  "Outcome_35",    "I95.2",    "Hypotension due to drugs",             "Hypotension",
  "Outcome_36",    "I95.3",    "Hemodialysis-related hypotension",     "Hypotension",
  "Outcome_37",    "I95.8",    "Other hypotension",                    "Hypotension",
  "Outcome_38",    "I95.9",    "Hypotension, unspecified",             "Hypotension",
  "Outcome_39",    "458",      "Hypotension (ICD-9)",                  "Hypotension"
)

# ========================
# Reader for RR files
# ========================
read_outcome_file <- function(file) {
  lines <- readLines(file)
  outcome_id <- str_extract(basename(file), "Outcome_\\d+")
  
  cohort_start <- which(str_detect(lines, "^Cohort,Cohort Name"))[1]
  cohort_block <- lines[cohort_start:(cohort_start + 2)]
  cohort_df <- read_csv(paste(cohort_block, collapse = "\n"), show_col_types = FALSE) %>%
    mutate(Row = row_number())
  
  rr_start <- which(str_detect(lines, "^Risk Ratio,"))[1]
  rr_block <- lines[rr_start:(rr_start + 1)]
  rr_df <- read_csv(paste(rr_block, collapse = "\n"), show_col_types = FALSE) %>%
    mutate(Row = row_number())
  
  left_join(cohort_df, rr_df, by = "Row") %>%
    select(-Row) %>%
    mutate(OutcomeNum = outcome_id)
}

# ========================
# Load all RR files for both time horizons
# ========================
read_rr_dir <- function(directory, label) {
  files <- list.files(directory, pattern = "Outcome_\\d+_Result_a_MOA_table\\.csv", full.names = TRUE)
  if (length(files) == 0) return(NULL)
  map_dfr(files, read_outcome_file) %>% mutate(TimeHorizon = label)
}

rr_data <- bind_rows(
  read_rr_dir(dir_1yr, "1-year"),
  read_rr_dir(dir_5yr, "5-year")
)

# Label master + sub outcomes
labeled <- rr_data %>%
  left_join(outcome_map, by = "OutcomeNum") %>%
  mutate(
    SubLabel = if_else(
      as.integer(str_remove(OutcomeNum, "Outcome_")) <= 7,
      OutcomeNum,
      SubLabel
    ),
    MasterLabel = case_when(
      OutcomeNum == "Outcome_1" ~ "Ischemic Heart Disease",
      OutcomeNum == "Outcome_2" ~ "Cerebrovascular Complications",
      OutcomeNum == "Outcome_3" ~ "Arrhythmia",
      OutcomeNum == "Outcome_4" ~ "Thrombotic Disorders",
      OutcomeNum == "Outcome_5" ~ "Mortality",
      OutcomeNum == "Outcome_6" ~ "Headaches",
      OutcomeNum == "Outcome_7" ~ "Hypotension",
      TRUE ~ MasterLabel
    )
  )

# ========================
# Plot Forest Plot
# ========================
plot_df <- labeled %>%
  filter(!is.na(`Risk Ratio`)) %>%
  mutate(
    rr_ci = sprintf("RR = %.2f [%.2f–%.2f]", `Risk Ratio`, `95 % CI Lower`, `95 % CI Upper`),
    Label = fct_inorder(SubLabel),
    MasterLabel = fct_inorder(MasterLabel),
    TimeHorizon = factor(TimeHorizon, levels = c("1-year", "5-year"))
  )

pd <- position_dodge(width = 0.5)

forest_plot <- ggplot(plot_df, aes(x = `Risk Ratio`, y = Label, group = TimeHorizon)) +
  geom_point(aes(shape = TimeHorizon), position = pd, size = 3, fill = "black") +
  geom_errorbarh(aes(xmin = `95 % CI Lower`, xmax = `95 % CI Upper`, linetype = TimeHorizon),
                 position = pd, height = 0.2) +
  geom_text(aes(label = rr_ci), position = pd, hjust = -0.1, size = 3.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  facet_wrap(~ MasterLabel, scales = "free_y", ncol = 1, strip.position = "left") +
  scale_x_continuous(trans = "log2", breaks = c(0.5, 1, 2, 4, 8),
                     expand = expansion(mult = c(0.01, 0.3))) +
  scale_shape_manual(values = c("1-year" = 21, "5-year" = 22)) +
  scale_linetype_manual(values = c("1-year" = "solid", "5-year" = "dotted")) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.y = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(2, "lines")
  ) +
  labs(
    title = "Forest Plot Grouped by Master Outcome",
    x = "Risk Ratio (log scale, 95% CI)"
  )

print(forest_plot)
# ggsave("forest_plot.svg", forest_plot, width = 8.5, height = 11)

# ========================
# KM Plot for Outcomes 1-3
# ========================
km_labels <- tribble(
  ~OutcomeNum, ~MasterLabel,
  "Outcome_1", "Ischemic Heart Disease",
  "Outcome_2", "Cerebrovascular Complications",
  "Outcome_3", "Arrhythmia"
)

read_km_file <- function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  outcome_id <- str_extract(basename(file), "Outcome_\\d+")
  df$OutcomeNum <- outcome_id
  return(df)
}

km_files <- list.files(dir_1yr, pattern = "Outcome_[123]_Result_b_KM_graph\\.csv", full.names = TRUE)
km_data <- map_dfr(km_files, read_km_file) %>%
  left_join(km_labels, by = "OutcomeNum")

km_plot <- ggplot(km_data, aes(x = `Time (Days)`, y = `Survival Probability`, color = `Cohort Name`)) +
  geom_step(size = 1) +
  facet_wrap(~ MasterLabel, scales = "free_y") +
  labs(
    title = "Kaplan-Meier Survival Curves (Outcomes 1–3)",
    x = "Time (Days)",
    y = "Survival Probability",
    color = "Cohort"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(km_plot)
# ggsave("km_plot.svg", km_plot, width = 10, height = 6)
