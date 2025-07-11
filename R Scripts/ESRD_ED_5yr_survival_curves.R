# ========================
# Kaplan-Meier Curves for 5-Year ESRD ED Outcomes
# ========================

# This script reads the TriNetX export files for the 5 year follow up
# and generates Kaplan-Meier survival curves for Outcomes 1–3.

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# --------------------------------------------------
# Helper to read and plot a TriNetX KM CSV
# --------------------------------------------------
plot_trinetx_surv <- function(csv_path,
                              cohort_labels = c("Cohort 1", "Cohort 2"),
                              plot_title = "Kaplan-Meier Survival Curve",
                              flip_survival = FALSE,
                              time_cutoff = 1826,
                              custom_colors = NULL) {
  df <- read_csv(
    csv_path,
    skip = 9,
    col_names = c(
      "time",
      "cohort1_surv", "cohort1_lower", "cohort1_upper",
      "cohort2_surv", "cohort2_lower", "cohort2_upper"
    ),
    col_types = cols(
      time = col_double(),
      cohort1_surv = col_double(),
      cohort1_lower = col_double(),
      cohort1_upper = col_double(),
      cohort2_surv = col_double(),
      cohort2_lower = col_double(),
      cohort2_upper = col_double()
    )
  ) %>%
    fill(everything(), .direction = "down") %>%
    filter(time <= time_cutoff)

  df_long <- df %>%
    pivot_longer(cols = -time,
                 names_to = c("cohort", ".value"),
                 names_pattern = "cohort(\\d+)_(.*)") %>%
    mutate(cohort = recode(cohort,
                           `1` = cohort_labels[1],
                           `2` = cohort_labels[2]))

  if (flip_survival) {
    df_long <- df_long %>%
      mutate(
        surv = 1 - surv,
        lower = 1 - upper,
        upper = 1 - lower
      )
    y_label <- "Cumulative Incidence"
  } else {
    y_label <- "Survival Probability"
  }

  p <- ggplot(df_long, aes(x = time, y = surv, color = cohort, fill = cohort)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    labs(title = plot_title, x = "Time (Days)", y = y_label) +
    theme_minimal()

  if (!is.null(custom_colors)) {
    p <- p +
      scale_color_manual(values = custom_colors) +
      scale_fill_manual(values = custom_colors)
  }

  p
}

# --------------------------------------------------
# Generate plots for Outcomes 1–3 (5 year follow up)
# --------------------------------------------------
base_dir <- file.path("Results1", "5 year ESRD ED")

p1 <- plot_trinetx_surv(
  file.path(base_dir, "Outcome_1_Result_b_KM_graph.csv"),
  cohort_labels = c("ED Meds", "No ED Meds"),
  plot_title = "Ischemic Heart Disease",
  custom_colors = c("ED Meds" = "#1f77b4", "No ED Meds" = "#999999")
)

p2 <- plot_trinetx_surv(
  file.path(base_dir, "Outcome_2_Result_b_KM_graph.csv"),
  cohort_labels = c("ED Meds", "No ED Meds"),
  plot_title = "Cerebrovascular Complications",
  custom_colors = c("ED Meds" = "#1f77b4", "No ED Meds" = "#999999")
)

p3 <- plot_trinetx_surv(
  file.path(base_dir, "Outcome_3_Result_b_KM_graph.csv"),
  cohort_labels = c("ED Meds", "No ED Meds"),
  plot_title = "Arrhythmia",
  custom_colors = c("ED Meds" = "#1f77b4", "No ED Meds" = "#999999")
)

final_km_plot <- (p1 | p2 | p3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(final_km_plot)
# To save the plot uncomment the line below
# ggsave("KM_5yr_outcomes1_3.svg", final_km_plot, width = 12, height = 5)

