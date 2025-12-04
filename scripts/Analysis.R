# ---- Setup ----
required_pkgs <- c("tidyverse", "broom", "ggpubr", "readr")
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if(length(missing_pkgs)) {
  message("Missing packages: ", paste(missing_pkgs, collapse = ", "),
          "\nInstall them before running or uncomment install.packages below.")
  # install.packages(missing_pkgs) # uncomment to auto-install
}

library(tidyverse)
library(broom)
library(ggpubr)
library(readr)

# ---- Paths ----
data_file <- "E:\\7COM1079-Group-A-68\\data\\gpa.csv"
out_plot_dir <- "plots"
out_data_dir <- "data/results"

dir.create(out_plot_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_data_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Read data ----
if(!file.exists(data_file)) stop("Data file not found: ", data_file)
df <- read_csv(data_file, show_col_types = FALSE)

# ---- Clean & coerce ----
df <- df %>%
  mutate(
    gpa = as.numeric(gpa),
    studyweek = as.numeric(studyweek),
    sleepnight = as.numeric(sleepnight),
    out = as.numeric(out),
    gender = as.factor(gender)
  ) %>%
  filter(!is.na(gpa) & !is.na(studyweek))

# ---- Summary stats ----
summary_stats <- tibble(
  n = nrow(df),
  mean_gpa = mean(df$gpa, na.rm = TRUE),
  sd_gpa = sd(df$gpa, na.rm = TRUE),
  mean_studyweek = mean(df$studyweek, na.rm = TRUE),
  sd_studyweek = sd(df$studyweek, na.rm = TRUE),
  min_gpa = min(df$gpa, na.rm = TRUE),
  max_gpa = max(df$gpa, na.rm = TRUE),
  min_studyweek = min(df$studyweek, na.rm = TRUE),
  max_studyweek = max(df$studyweek, na.rm = TRUE)
)
write_csv(summary_stats, file.path(out_data_dir, "summary_stats.csv"))

# ---- Pearson correlation ----
pearson <- cor.test(df$studyweek, df$gpa, method = "pearson")
pearson_tidy <- tidy(pearson)
write_csv(pearson_tidy, file.path(out_data_dir, "pearson_test_result.csv"))

# ---- Linear regression ----
lm_model <- lm(gpa ~ studyweek, data = df)
lm_coefs <- tidy(lm_model)
write_csv(lm_coefs, file.path(out_data_dir, "lm_coefficients.csv"))

lm_glance <- broom::glance(lm_model)
write_csv(as_tibble(lm_glance), file.path(out_data_dir, "lm_model_summary.csv"))

# ---- Plots ----
p_scatter <- ggplot(df, aes(x = studyweek, y = gpa)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  labs(title = "GPA vs Study Hours per Week",
       subtitle = "Scatterplot with linear regression line (95% CI)",
       x = "Study hours per week",
       y = "GPA") +
  theme_minimal(base_size = 14)

ggsave(filename = file.path(out_plot_dir, "scatter_studyweek_gpa.png"), plot = p_scatter, width = 7, height = 5, dpi = 300)

p_hist_study <- ggplot(df, aes(x = studyweek)) +
  geom_histogram(binwidth = 5, boundary = 0, color = "black", fill = "grey80") +
  labs(title = "Histogram of Study Hours per Week", x = "Study hours per week", y = "Count") +
  theme_minimal(base_size = 14)
ggsave(filename = file.path(out_plot_dir, "hist_studyweek.png"), plot = p_hist_study, width = 6, height = 4, dpi = 300)

p_hist_gpa <- ggplot(df, aes(x = gpa)) +
  geom_histogram(binwidth = 0.25, boundary = 0, color = "black", fill = "grey80") +
  labs(title = "Histogram of GPA", x = "GPA", y = "Count") +
  theme_minimal(base_size = 14)
ggsave(filename = file.path(out_plot_dir, "hist_gpa.png"), plot = p_hist_gpa, width = 6, height = 4, dpi = 300)

# Residual diagnostics
resid_df <- tibble(fitted = lm_model$fitted.values, resid = lm_model$residuals)
p_resid <- ggplot(resid_df, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
  theme_minimal(base_size = 14)
ggsave(filename = file.path(out_plot_dir, "residuals_vs_fitted.png"), plot = p_resid, width = 6, height = 4, dpi = 300)

png(file.path(out_plot_dir, "qqplot_residuals.png"), width = 800, height = 600, res = 120)
qqnorm(lm_model$residuals); qqline(lm_model$residuals, col = "red")
dev.off()

# ---- Console summary ----
cat("Sample size (n):", summary_stats$n, "\n\n")
cat(sprintf("Pearson correlation: r = %.5f, p = %.6f\n\n", pearson$estimate, pearson$p.value))
cat("Linear regression (gpa ~ studyweek):\n")
cat("Intercept =", round(coef(lm_model)[1], 5), "\n")
cat("Slope =", round(coef(lm_model)[2], 6), "\n")
cat("R-squared =", round(lm_glance$r.squared, 5), "Adj R-squared =", round(lm_glance$adj.r.squared, 5), "\n")
cat("F-statistic =", round(lm_glance$statistic, 3), ", p =", format.pval(lm_glance$p.value, digits = 6), "\n")
