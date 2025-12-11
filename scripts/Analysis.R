# scripts/Analysis.R
# Reproducible analysis for "GPA vs Study Hours" (project root assumed)
# To run:
# install.packages(c("tidyverse","broom","ggpubr","readr")) # uncomment if needed
# source("scripts/Analysis.R")

# ---- Setup ----
required_pkgs <- c("tidyverse", "broom", "ggpubr", "readr")
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs)) {
  message("Missing packages: ", paste(missing_pkgs, collapse = ", "),
          "\nInstall them before running (uncomment install.packages line in header).")
  # install.packages(missing_pkgs)  # uncomment to auto-install
}

library(tidyverse)
library(broom)
library(ggpubr)
library(readr)

# ---- Data path detection (edit possible_paths if your layout differs) ----
possible_paths <- c(
  "data/gpa.csv",
  "./data/gpa.csv",
  "data/gpa.csv",
  "E:/7COM1079-Group-A-68/data/gpa.csv",
  "C:/Users/USERNAME/Desktop/7COM1079-Group-A-68/data/gpa.csv" # optional fallback
)

data_file <- NULL
for (p in possible_paths) {
  if (file.exists(p)) {
    data_file <- p
    break
  }
}
if (is.null(data_file)) stop("Data file not found. Place gpa.csv in ./data/ or update possible_paths.")

# ---- Output directories ----
out_plot_dir <- "plots"
out_data_dir <- "data/results"

dir.create(out_plot_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_data_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Read data ----
df <- read_csv(data_file, show_col_types = FALSE)

# ---- Data cleaning & coercion ----
df <- df %>%
  mutate(
    gpa = as.numeric(gpa),
    studyweek = as.numeric(studyweek),
    sleepnight = as.numeric(sleepnight),
    out = as.numeric(out),
    gender = as.factor(gender)
  ) %>%
  filter(!is.na(gpa) & !is.na(studyweek))

# ---- Summary statistics ----
summary_stats <- tibble(
  n = nrow(df),
  mean_gpa = mean(df$gpa, na.rm = TRUE),
  sd_gpa = sd(df$gpa, na.rm = TRUE),
  median_gpa = median(df$gpa, na.rm = TRUE),
  min_gpa = min(df$gpa, na.rm = TRUE),
  max_gpa = max(df$gpa, na.rm = TRUE),
  mean_studyweek = mean(df$studyweek, na.rm = TRUE),
  sd_studyweek = sd(df$studyweek, na.rm = TRUE),
  median_studyweek = median(df$studyweek, na.rm = TRUE),
  min_studyweek = min(df$studyweek, na.rm = TRUE),
  max_studyweek = max(df$studyweek, na.rm = TRUE)
)
write_csv(summary_stats, file.path(out_data_dir, "summary_stats.csv"))

# ---- Normality tests (Shapiro-Wilk) ----
shapiro_gpa <- shapiro.test(df$gpa)
shapiro_study <- shapiro.test(df$studyweek)
write_csv(broom::tidy(shapiro_gpa), file.path(out_data_dir, "shapiro_gpa_result.csv"))
write_csv(broom::tidy(shapiro_study), file.path(out_data_dir, "shapiro_studyweek_result.csv"))

# ---- Plot: GPA histogram with density + normal curve (required for report) ----
p_hist_gpa <- ggplot(df, aes(x = gpa)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "grey80", color = "black") +
  geom_density(color = "blue", size = 1) +
  stat_function(
    fun = dnorm,
    args = list(mean = mean(df$gpa, na.rm = TRUE), sd = sd(df$gpa, na.rm = TRUE)),
    color = "red", linetype = "dashed", size = 1
  ) +
  labs(title = "Histogram of GPA with Normal Curve",
       x = "GPA (0–4)", y = "Density") +
  theme_minimal(base_size = 14)

ggsave(filename = file.path(out_plot_dir, "hist_gpa_normalcurve.png"),
       plot = p_hist_gpa, width = 6, height = 4, dpi = 300)

# ---- Plot: Study hours histogram (recommended for workflow / decision) ----
p_hist_study <- ggplot(df, aes(x = studyweek)) +
  geom_histogram(fill = "grey80", color = "black", bins = 10, aes(y = ..count..)) +
  geom_density(aes(y = ..count.. * diff(range(df$studyweek)) / 10), color = "blue", size = 1) +
  labs(title = "Histogram of Study Hours per Week",
       x = "Study hours per week (hours)", y = "Count") +
  theme_minimal(base_size = 14)

ggsave(filename = file.path(out_plot_dir, "hist_studyweek.png"),
       plot = p_hist_study, width = 6, height = 4, dpi = 300)

# ---- Plot: Scatterplot (main plot) ----
p_scatter <- ggplot(df, aes(x = studyweek, y = gpa)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", formula = y ~ x) +
  labs(title = "Scatterplot of GPA vs Study Hours per Week",
       x = "Study hours per week (hours)", y = "GPA (0–4)") +
  theme_minimal(base_size = 14)

ggsave(filename = file.path(out_plot_dir, "scatter_gpa_study.png"),
       plot = p_scatter, width = 7, height = 5, dpi = 300)

# ---- Decide correlation method using decision logic (both variables checked) ----
gpa_normal <- shapiro_gpa$p.value > 0.05
study_normal <- shapiro_study$p.value > 0.05

if (gpa_normal && study_normal) {
  corr_method <- "pearson"
} else {
  corr_method <- "spearman"
}

# ---- Correlation test ----
corr_test <- cor.test(df$studyweek, df$gpa, method = corr_method)
corr_tidy <- broom::tidy(corr_test)
corr_tidy <- corr_tidy %>% mutate(method_used = corr_method)
write_csv(corr_tidy, file.path(out_data_dir, "correlation_test_result.csv"))

# ---- Linear model (for interpretation & effect size) ----
lm_model <- lm(gpa ~ studyweek, data = df)
lm_coefs <- broom::tidy(lm_model)
lm_glance <- broom::glance(lm_model)
write_csv(lm_coefs, file.path(out_data_dir, "lm_coefficients.csv"))
write_csv(as_tibble(lm_glance), file.path(out_data_dir, "lm_model_summary.csv"))

# ---- Save a small diagnostics table (optional) ----
diagnostics <- tibble(
  shapiro_gpa_W = shapiro_gpa$statistic,
  shapiro_gpa_p = shapiro_gpa$p.value,
  shapiro_study_W = shapiro_study$statistic,
  shapiro_study_p = shapiro_study$p.value,
  corr_method = corr_method,
  corr_estimate = corr_test$estimate,
  corr_p = corr_test$p.value,
  lm_r_squared = lm_glance$r.squared
)
write_csv(diagnostics, file.path(out_data_dir, "analysis_diagnostics_summary.csv"))

# ---- Console summary (concise, copy into report if desired) ----
cat("---- Analysis Summary ----\n")
cat("Sample size (n):", summary_stats$n, "\n")
cat(sprintf("GPA mean = %.3f; sd = %.3f; median = %.3f; range = [%.2f, %.2f]\n",
            summary_stats$mean_gpa, summary_stats$sd_gpa, summary_stats$median_gpa,
            summary_stats$min_gpa, summary_stats$max_gpa))
cat(sprintf("Study hours mean = %.2f; sd = %.2f; range = [%.2f, %.2f]\n",
            summary_stats$mean_studyweek, summary_stats$sd_studyweek,
            summary_stats$min_studyweek, summary_stats$max_studyweek))
cat(sprintf("Shapiro-Wilk (GPA): W = %.4f, p = %.6f\n", shapiro_gpa$statistic, shapiro_gpa$p.value))
cat(sprintf("Shapiro-Wilk (Study hours): W = %.4f, p = %.6f\n", shapiro_study$statistic, shapiro_study$p.value))
cat("Selected correlation method:", corr_method, "\n")
cat(sprintf("Correlation estimate = %.4f; p = %.6f\n", corr_test$estimate, corr_test$p.value))
cat(sprintf("Linear model: Intercept = %.4f; Slope = %.6f; R-squared = %.4f\n",
            coef(lm_model)[1], coef(lm_model)[2], lm_glance$r.squared))
cat("Plots saved to:", out_plot_dir, "\n")
cat("CSV results saved to:", out_data_dir, "\n")

# ---- End of script ----
