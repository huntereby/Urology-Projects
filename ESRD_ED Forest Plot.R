# Load libraries
library(tidyverse)
library(forcats)
library(ggplot2)

# Define outcome mapping (Outcome_8 and beyond are subcategories)
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

# File reader that extracts cohorts + RR from TriNetX result format
read_outcome_file <- function(file) {
  lines <- readLines(file)
  outcome_id <- str_extract(basename(file), "Outcome_\\d+")
  
  # Cohort section
  cohort_start <- which(str_detect(lines, "^Cohort,Cohort Name"))[1]
  cohort_block <- lines[cohort_start:(cohort_start + 2)]
  cohort_df <- read_csv(paste(cohort_block, collapse = "\n"), show_col_types = FALSE) %>%
    mutate(Row = row_number())
  
  # Risk Ratio section
  rr_start <- which(str_detect(lines, "^Risk Ratio,"))[1]
  rr_block <- lines[rr_start:(rr_start + 1)]
  rr_df <- read_csv(paste(rr_block, collapse = "\n"), show_col_types = FALSE) %>%
    mutate(Row = row_number())
  
  # Join + tag
  left_join(cohort_df, rr_df, by = "Row") %>%
    select(-Row) %>%
    mutate(OutcomeNum = outcome_id)
}

# Read all files in folder
main_dir <- "results1/Unnamed_Analysis_359371/"  # <---- CHANGE THIS
all_files <- list.files(main_dir, pattern = "Outcome_\\d+_Result_a_MOA_table\\.csv", full.names = TRUE)
raw_data <- map_dfr(all_files, read_outcome_file)

# Annotate with master + sub outcome labels
annotated <- raw_data %>%
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

# Clean for plotting
plot_df <- annotated %>%
  filter(!is.na(`Risk Ratio`)) %>%
  mutate(
    rr_ci = sprintf("RR = %.2f [%.2f–%.2f]", `Risk Ratio`, `95 % CI Lower`, `95 % CI Upper`),
    Label = fct_inorder(SubLabel),
    MasterLabel = fct_inorder(MasterLabel)
  )

# Plot
ggplot(plot_df, aes(x = `Risk Ratio`, y = Label)) +
  geom_point(size = 3, shape = 21, fill = "black") +
  geom_errorbarh(aes(xmin = `95 % CI Lower`, xmax = `95 % CI Upper`), height = 0.2) +
  geom_text(aes(label = rr_ci), hjust = -0.1, size = 3.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  facet_wrap(~ MasterLabel, scales = "free_y", ncol = 1, strip.position = "left") +
  scale_x_continuous(trans = "log2", breaks = c(0.5, 1, 2, 4, 8),
                     expand = expansion(mult = c(0.01, 0.3))) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.y = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(1, "lines")
  ) +
  labs(
    title = "Forest Plot Grouped by Master Outcome",
    x = "Risk Ratio (log scale, 95% CI)",
    ggsave("forest_plot1.1.1.svg", width = 10, height = 12)
  )

# Optional: Save to SVG
ggsave("forest_plot.svg", width = 10, height = 12)




