library(readxl)
library(dplyr)
library(readr)     # parse_number()
library(stringr)
library(boot)      # bootstrap
library(ggplot2)   # plotting (optional)
library(janitor)
library(tidyr)
library(forcats)

# 1. Load and clean data
xls_file  <- "excel_data/yield_predictions_data_with_local_dT_filled.xlsx"
xls_sheet <- "Sheet1"

raw <- read_excel(xls_file, sheet = xls_sheet) %>%
  clean_names()  # ensures snake_case column names

# 2. Filter to Maize only & exclude "Yield Potential" adaptations -----------
filtered_data <- raw %>%
  filter(crop == "Maize") %>%
  filter(adaptation_type != "Yield Potential")

# Create control dataset
control_data <- filtered_data %>%
  filter(adaptation_type == "No") %>%
  rename(yield_control = climate_impacts_percent)

# Create treatment dataset
treatment_data <- filtered_data %>%
  filter(adaptation_type != "No") %>%
  rename(yield_treat = climate_impacts_percent)

# Merge on key study-level identifiers
merged_data <- merge(
  control_data, treatment_data,
  by = c("ref_no", "future_mid_point", "climate_scenario", "site_location", "crop", "gcm_rcm", "crop_model"),
  all = FALSE
)

# 2. Clean and preprocess ------------------------------------------------------

dat_maize <- merged_data %>%
  transmute(
    ref_no = ref_no,
    dt = as.numeric(local_delta_t_from_2005.y),
    dp = yield_treat - yield_control,
    adapt_raw = adaptation_type.y
  ) %>%
  filter(!is.na(dp), !is.na(dt), !is.na(adapt_raw)) %>%
  mutate(
    dp = pmax(pmin(dp, 150), -150),  # Cap extremes to ±150%
    adapt_group = case_when(
      adapt_raw %in% c("Cultivar", "Cultivar - Long duration", "Cultivar - Short duration") ~ "Cultivar",
      TRUE ~ adapt_raw
    ),
    adapt_group = fct_infreq(factor(adapt_group)),
    ref_no = factor(ref_no)
  )

cat("✅ Adaptation groups used:\n")
print(levels(dat_maize$adapt_group))

# 3. Bootstrap regression -------------------------------------------------------

model_formula <- dp ~ dt * adapt_group

# Rebuild the reference model to reset the structure of coefficients
ref_model <- lm(model_formula, data = dat_maize)
ref_coefs <- names(coef(ref_model))


boot_group <- function(d, i) {
  d2 <- d[i, ]
  tryCatch({
    fit <- lm(model_formula, data = d2)
    b <- coef(fit)
    out <- rep(NA, length(ref_coefs))
    names(out) <- ref_coefs
    out[names(b)] <- b
    return(out)
  }, error = function(e) rep(NA, length(ref_coefs)))
}

set.seed(42)
boot_out <- boot(dat_maize, boot_group, R = 500, strata = dat_maize$ref_no)

# 4. Make prediction grid -------------------------------------------------------

grid_df <- expand.grid(
  dt = seq(0.5, 4, length.out = 100),
  adapt_group = levels(dat_maize$adapt_group)
)

X <- model.matrix(~ dt * adapt_group, data = grid_df)


pred_mat <- X %*% t(boot_out$t)

grid_df <- grid_df %>%
  mutate(
    fit  = rowMeans(pred_mat, na.rm = TRUE),
    lo95 = apply(pred_mat, 1, quantile, probs = 0.025, na.rm = TRUE),
    hi95 = apply(pred_mat, 1, quantile, probs = 0.975, na.rm = TRUE)
  )

# 5. Plot ΔYield vs ΔT with shaded CIs -----------------------------------------

ggplot() +
  geom_point(data = dat_maize, aes(x = dt, y = dp, color = adapt_group), alpha = 0.3) +
  geom_ribbon(data = grid_df, aes(x = dt, ymin = lo95, ymax = hi95, fill = adapt_group), alpha = 0.2) +
  geom_line(data = grid_df, aes(x = dt, y = fit, color = adapt_group), linewidth = 1.1) +
  scale_x_continuous(limits = c(0, 3)) +   # <-- this line sets x-axis from 0 to 3
  labs(
    title = "African maize: Yield response to ΔT by adaptation strategy",
    subtitle = "Bootstrapped 95% CI by adaptation (treatment – control)",
    x = "ΔT from 2005 (°C)",
    y = expression(Delta~"Yield (%) vs. Control"),
    color = "Adaptation strategy",
    fill  = "Adaptation strategy"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")


# 6. Study counts as bar indicator ---------------------------------------------

study_counts <- dat_maize %>%
  group_by(adapt_group) %>%
  summarise(n = n_distinct(ref_no), .groups = "drop")

ggplot(study_counts, aes(x = adapt_group, y = n, fill = adapt_group)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.3) +
  labs(
    title = "Number of studies per adaptation strategy",
    x = "Adaptation strategy",
    y = "Study count"
  ) +
  theme_minimal(base_size = 14)

# 7. Mean ΔYield per adaptation strategy ---------------------------------------

mean_yield_plot <- dat_maize %>%
  group_by(adapt_group) %>%
  summarise(mean_delta = mean(dp, na.rm = TRUE),
            n = n_distinct(ref_no),
            .groups = "drop")

ggplot(mean_yield_plot, aes(x = adapt_group, y = mean_delta, fill = adapt_group)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(mean_delta, 1)), vjust = -0.3, size = 4) +
  labs(
    title = "Mean ΔYield (%) by adaptation strategy",
    y = expression(Delta~"Yield (%) vs. Control"),
    x = "Adaptation strategy"
  ) +
  ylim(-50, 50) +
  theme_minimal(base_size = 14)

# 7. Prediction grid
grid_df <- expand.grid(
  dt = seq(min(dat_maize$dt), max(dat_maize$dt), length.out = 100),
  adapt_type = levels(dat_maize$adapt_type)
)

X <- model.matrix(~ poly(dt, 2, raw = TRUE) * adapt_type, data = grid_df)
pred_mat <- X %*% t(boot_out$t)

grid_df <- grid_df %>%
  mutate(
    fit  = rowMeans(pred_mat, na.rm = TRUE),
    lo95 = apply(pred_mat, 1, quantile, probs = 0.025, na.rm = TRUE),
    hi95 = apply(pred_mat, 1, quantile, probs = 0.975, na.rm = TRUE)
  )

# 8. Final plot
ggplot() +
  geom_point(data = dat_maize, aes(x = dt, y = dp, color = adapt_type), alpha = 0.3) +
  geom_ribbon(data = grid_df, aes(x = dt, ymin = lo95, ymax = hi95, fill = adapt_type), alpha = 0.2) +
  geom_line(data = grid_df, aes(x = dt, y = fit, color = adapt_type), linewidth = 1.1) +
  coord_cartesian(ylim = c(-100, 100)) +
  labs(
    title = "African maize: Yield response to ΔT by adaptation strategy",
    subtitle = "Bootstrapped 95% CI (treatment – control)",
    x = "ΔT from 2005 (°C)",
    y = expression(Delta~"Yield (%) vs. Control"),
    color = "Adaptation strategy",
    fill  = "Adaptation strategy"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")