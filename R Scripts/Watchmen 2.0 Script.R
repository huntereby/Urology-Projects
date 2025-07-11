#Forest Plot
library(tidyverse)

# Define your parent directory that holds all "Concuss_Xx..." folders
main_dir <- "Results1"

library(tidyverse)

# Set your top-level folder
main_dir <- "Results1"

# Find all outcome result files recursively
outcome_files <- list.files(
  path = main_dir,
  pattern = "Outcome_\\d+_Result_a_MOA_table\\.csv",
  recursive = TRUE,
  full.names = TRUE
)

# Function to extract cohort stats, risk ratio, and time window
extract_outcome_info <- function(file) {
  lines <- readLines(file)
  
  # Extract tags from file path
  outcome <- str_extract(basename(file), "Outcome_\\d+")
  cohort_name <- str_extract(file, "Unnamed_Analysis_(1)_383852")
  
  
  # Extract Cohort Statistics
  cohort_start <- which(str_detect(lines, "Cohort,Cohort Name"))
  cohort_block <- c(
    lines[cohort_start],
    lines[(cohort_start + 1):(cohort_start + 2)]
  )
  cohort_df <- read_csv(paste(cohort_block, collapse = "\n"), show_col_types = FALSE) %>%
    mutate(Row = row_number())
  
  # Extract Risk Ratio
  rr_start <- which(str_detect(lines, "^Risk Ratio,"))
  rr_block <- c(lines[rr_start], lines[rr_start + 1])
  rr_df <- read_csv(paste(rr_block, collapse = "\n"), show_col_types = FALSE) %>%
    mutate(Row = row_number())
  
  # Combine and annotate
  left_join(cohort_df, rr_df, by = "Row") %>%
    select(-Row) %>%
    mutate(
      Outcome = outcome,
      CohortFile = cohort_name
    )
}

# Apply across all files
outcome_combined <- map_dfr(outcome_files, extract_outcome_info)

# Inspect the result
glimpse(outcome_combined)





outcome_combined <- outcome_combined %>%
  mutate(OutcomeLabel = case_when(
    Outcome == "Outcome_1" ~ "Acute Myocardial infarction",
    Outcome == "Outcome_2" ~ "Cerebral infraction",
    Outcome == "Outcome_3" ~ "Epilepsy",
    Outcome == "Outcome_4" ~ "Blood Pressure",
    Outcome == "Outcome_5" ~ "Mortality",
    Outcome == "Outcome_6" ~ "TIA",
    Outcome == "Outcome_7" ~ "Hypotension",
    Outcome == "Outcome_8" ~ "Hemoragic stroke",
    Outcome == "Outcome_8" ~ "Depressive Episode",
    TRUE ~ NA_character_
  ))


library(tidyverse)
library(tidyverse)

# Prepare data: filter to 5-year only and drop NA
forest_data <- outcome_combined %>%
  filter(`Cohort Name` != "Unnamed", !is.na(`Risk Ratio`)) %>%
  mutate(
    OutcomeLabel = factor(OutcomeLabel,
                          levels = c(
                            "Acute Myocardial infarction", "Cerebral infraction",
                            "Epilepsy","Blood Pressure","Mortality","TIA",
                            "Hypotension","Hemoragic stroke","Depressive Episode"
                          )
    ),
    
  ) %>%
  arrange(OutcomeLabel, CohortFile)

# Create dummy "Reference" row for each outcome
ref_rows <- forest_data  %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    `Risk Ratio` = 1,
    `95 % CI Lower` = 1,
    `95 % CI Upper` = 1,
    Label = "Reference",
    is_ref = TRUE
  )

# Add a flag and combine
forest_data <- forest_data %>%
  mutate(is_ref = FALSE)

plot_df <- bind_rows(forest_data, ref_rows) %>%
  arrange( desc(is_ref)) %>%
  mutate(
    OutcomeLabel = fct_inorder(OutcomeLable),
  )

# Create the plot
ggplot(plot_df, aes(x = `Risk Ratio`, y = Label)) +
  geom_point(aes(color = is_ref), size = 3, shape = 21, fill = "black") +
  geom_errorbarh(
    aes(xmin = `95 % CI Lower`, xmax = `95 % CI Upper`),
    height = 0.2
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  facet_wrap(~ Group, scales = "free_y", ncol = 1, strip.position = "left") +
  scale_x_continuous(trans = "log2", breaks = c(0.5, 1, 2, 4, 8)) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black"), guide = "none") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.y = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(1, "lines")
  ) +
  labs(
    title = "Forest Plot of 5-Year Risk Ratios by Outcome and Concussion Count",
    x = "Risk Ratio (log scale, 95% CI)"
  )

install.packages("svglite")  # needed for SVG output
library(svglite)
library(ggplot2)
ggsave("forest_plot.svg", plot = last_plot(), width = 8, height = 10, units = "in")






#######
watchmen_df <- watchmen_df %>%
  mutate(OutcomeLabel = case_when(
    Outcome == "Outcome_1" ~ "Cerebral Infarction",
    Outcome == "Outcome_2" ~ "Cardiac Arrest",
    Outcome == "Outcome_3" ~ "STEMI",
    Outcome == "Outcome_4" ~ "Embolism and Thrombosis",
    Outcome == "Outcome_5" ~ "Gastrointestinal Hemorrhage",
    Outcome == "Outcome_6" ~ "Nontraumatic Intracerebral Hemorrhage",
    TRUE ~ NA_character_
  ))

# Clean and filter
forest_data <- watchmen_df %>%
  filter(`Cohort Name` != "Unnamed", !is.na(`Risk Ratio`)) %>%
  mutate(
    Label = paste(TimeWindow, OutcomeLabel, sep = " - "),
    OutcomeLabel = factor(OutcomeLabel, levels = unique(OutcomeLabel)),
    TimeWindow = factor(TimeWindow, levels = c("3 month", "9 month", "1 year", "5 year"))
  )

# Plot
ggplot(forest_data, aes(x = `Risk Ratio`, y = fct_reorder(Label, `Risk Ratio`))) +
  geom_point() +
  geom_errorbarh(aes(xmin = `95 % CI Lower`, xmax = `95 % CI Upper`), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  labs(
    x = "Risk Ratio (95% CI)",
    y = NULL,
    title = "Forest Plot by Outcome and Time Window"
  ) +
  theme_minimal(base_size = 13)

######New code####
library(tidyverse)
library(svglite)
library(ggplot2)

# Load your cleaned watchmen dataset (already recoded OutcomeLabel)
watchmen_df <- read_csv("riskratiowatchmen.csv") %>%
  mutate(OutcomeLabel = case_when(
    Outcome == "Outcome_1" ~ "Cerebral Infarction",
    Outcome == "Outcome_2" ~ "Cardiac Arrest",
    Outcome == "Outcome_3" ~ "STEMI",
    Outcome == "Outcome_4" ~ "Embolism and Thrombosis",
    Outcome == "Outcome_5" ~ "Gastrointestinal Hemorrhage",
    Outcome == "Outcome_6" ~ "Nontraumatic Intracerebral Hemorrhage",
    TRUE ~ NA_character_
  ))

# Step 1: Clean and prep the data
forest_data <- watchmen_df %>%
  filter(`Cohort Name` != "Unnamed", !is.na(`Risk Ratio`)) %>%
  mutate(
    OutcomeLabel = factor(OutcomeLabel),
    TimeWindow = factor(TimeWindow, levels = c("3 month", "9 month", "1 year", "5 year")),
    Group = OutcomeLabel,
    Label = paste0("  ", TimeWindow)
  ) %>%
  arrange(OutcomeLabel, TimeWindow)

# Step 2: Create dummy reference rows
ref_rows <- forest_data %>%
  group_by(OutcomeLabel) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    `Risk Ratio` = 1,
    `95 % CI Lower` = 1,
    `95 % CI Upper` = 1,
    Label = "Reference",
    is_ref = TRUE
  )

# Step 3: Combine and finalize
forest_data <- forest_data %>% mutate(is_ref = FALSE)

plot_df <- bind_rows(forest_data, ref_rows) %>%
  arrange(OutcomeLabel, desc(is_ref), TimeWindow) %>%
  mutate(
    OutcomeLabel = fct_inorder(OutcomeLabel),
    TimeWindow = fct_inorder(TimeWindow),
    Label = fct_inorder(Label)
  )

# Step 4: Plot
ggplot(plot_df, aes(x = `Risk Ratio`, y = Label)) +
  geom_point(aes(color = is_ref), size = 3, shape = 21, fill = "black") +
  geom_errorbarh(aes(xmin = `95 % CI Lower`, xmax = `95 % CI Upper`), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  facet_wrap(~ OutcomeLabel, scales = "free_y", ncol = 1, strip.position = "left") +
  scale_x_continuous(trans = "log2", breaks = c(0.5, 1, 2, 4, 8)) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black"), guide = "none") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.y = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(1, "lines")
  ) +
  labs(
    title = "Forest Plot of Risk Ratios by Outcome and Time Window",
    x = "Risk Ratio (log scale, 95% CI)"
  )

library(svglite)
# Step 5: Export to Illustrator-editable SVG
ggsave("forest_plot_watchmen.svg", plot = last_plot(), width = 8, height = 10, units = "in")


# Add a new column with formatted RR and CI
plot_df <- plot_df %>%
  mutate(
    rr_ci = ifelse(
      is_ref,
      "Ref.",
      sprintf("RR = %.2f [%.2f–%.2f]", `Risk Ratio`, `95 % CI Lower`, `95 % CI Upper`)
    )
  )

# Create the plot with annotation on the right
ggplot(plot_df, aes(x = `Risk Ratio`, y = Label)) +
  geom_point(aes(color = is_ref), size = 3, shape = 21, fill = "black") +
  geom_errorbarh(aes(xmin = `95 % CI Lower`, xmax = `95 % CI Upper`), height = 0.2) +
  geom_text(aes(label = rr_ci), hjust = -0.1, size = 3.2) +  # right-side text
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  facet_wrap(~ OutcomeLabel, scales = "free_y", ncol = 1, strip.position = "left") +
  scale_x_continuous(trans = "log2", breaks = c(0.5, 1, 2, 4, 8), expand = expansion(mult = c(0.01, 0.4))) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black"), guide = "none") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.y = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(1, "lines")
  ) +
  labs(
    title = "Forest Plot of Risk Ratios by Outcome and Time Window",
    x = "Risk Ratio (log scale, 95% CI)"
  )

