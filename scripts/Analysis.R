# ---- Setup ----
required_pkgs <- c("tidyverse", "broom", "ggpubr", "readr")
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if(length(missing_pkgs)) {
  message("Missing packages: ", paste(missing_pkgs, collapse = ", "),
          "\nInstall them before running (uncomment install.packages line if desired).")
  # install.packages(missing_pkgs) # uncomment if you want automatic install
}

library(tidyverse)
library(broom)
library(ggpubr)
library(readr)


data_file <- NULL
for(p in possible_paths) {
  if(file.exists(p)) { data_file <- p; break }
}
if(is.null(data_file)) {
  stop("Data file not found in expected locations. Please set 'data_file' variable to the correct path.")
}

out_plot_dir <- "plots"
out_data_dir <- "data/results"

dir.create(out_plot_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_data_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Read data ----
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
  median_gpa = median(df$gpa, na.rm = TRUE),
  iqr_gpa = IQR(df$gpa, na.rm = TRUE),
  min_gpa = min(df$gpa, na.rm = TRUE),
  max_gpa = max(df$gpa, na.rm = TRUE)
)
write_csv(summary_stats, file.path(out_data_dir, "summary_stats.csv"))

# ---- Normality test for GPA (Shapiro-Wilk) ----
# Note: Shapiro is sensitive for large N; interpret alongside histogram.
shapiro_res <- shapiro.test(df$gpa)
shapiro_tidy <- broom::tidy(shapiro_res)
write_csv(as_tibble(shapiro_tidy), file.path(out_data_dir, "shapiro_gpa_result.csv"))

# ---- Histogram of GPA with kernel density + normal curve ----
p_hist_gpa <- ggplot(df, aes(x = gpa)) +
  geom_histogram(aes(y = ..density..),
                 bins = 10,
                 fill = "grey80",
                 color = "black") +
  geom_density(color = "blue", linewidth = 1) +
  stat_function(
    fun = dnorm,
    args = list(mean = mean(df$gpa, na.rm = TRUE),
                sd = sd(df$gpa, na.rm = TRUE)),
    color = "red",
    linewidth = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "Histogram of GPA with Normal Curve",
    x = "GPA",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

ggsave(filename = file.path(out_plot_dir, "hist_gpa_normalcurve.png"),
       plot = p_hist_gpa, width = 6, height = 4, dpi = 300)

# ---- Scatterplot: Study hours vs GPA (main plot) ----
p_scatter <- ggplot(df, aes(x = studyweek, y = gpa)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of GPA vs Study Hours per Week",
       x = "Study hours per week",
       y = "GPA") +
  theme_minimal(base_size = 14)

ggsave(filename = file.path(out_plot_dir, "scatter_gpa_study.png"),
       plot = p_scatter, width = 7, height = 5, dpi = 300)

# ---- Correlation test selection based on Shapiro result ----
shapiro_p <- shapiro_res$p.value

if(shapiro_p > 0.05) {
  corr_method <- "pearson"
} else {
  corr_method <- "spearman"
}

corr_test <- cor.test(df$studyweek, df$gpa, method = corr_method)
corr_tidy <- broom::tidy(corr_test)
corr_tidy$method <- corr_method
write_csv(corr_tidy, file.path(out_data_dir, "correlation_test_result.csv"))

# ---- Console summary (concise) ----
cat("---- Analysis Summary ----\n")
cat("Sample size (n):", summary_stats$n, "\n")
cat(sprintf("GPA mean = %.3f; sd = %.3f; median = %.3f\n",
            summary_stats$mean_gpa, summary_stats$sd_gpa, summary_stats$median_gpa))
cat(sprintf("Shapiro-Wilk W = %.4f, p = %.6f\n", shapiro_res$statistic, shapiro_res$p.value))
cat("Chosen correlation method:", corr_method, "\n")
cat(sprintf("Correlation estimate = %.4f; p = %.6f\n", corr_test$estimate, corr_test$p.value))
cat("Histogram saved to:", file.path(out_plot_dir, "hist_gpa_normalcurve.png"), "\n")
cat("Scatterplot saved to:", file.path(out_plot_dir, "scatter_gpa_study.png"), "\n")
cat("CSV results saved to:", out_data_dir, "\n")

