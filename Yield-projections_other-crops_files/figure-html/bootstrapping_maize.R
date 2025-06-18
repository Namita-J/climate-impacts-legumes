# 1. Load and clean data
xls_file  <- "yield_predictions_data_with_local_dT_filled.xlsx"
xls_sheet <- "Sheet1"

raw <- read_excel(xls_file, sheet = xls_sheet) %>%
  clean_names()  # standardize column names

# 2. Filter to maize only
dat_maize <- raw %>%
  filter(crop == "Maize") %>%
  mutate(
    dp = as.numeric(climate_impacts_percent),
    dt = as.numeric(local_delta_t_from_2005),
    adapt_raw = as.character(adaptation_type)
  ) %>%
  filter(!is.na(dp), !is.na(dt), !is.na(adapt_raw))

# 4. Group adaptation strategies
dat_maize <- dat_maize %>%
  mutate(adapt_group = case_when(
    adapt_raw %in% c("Fertilizer", "High Fertilizer", "Medium Fertilizer", "Low Fertiliser") ~ "Fertilizer",
    adapt_raw %in% c("Cultivar", "Cultivar - Long duration", "Cultivar - Short duration") ~ "Cultivar",
    TRUE ~ adapt_raw
  )) %>%
  mutate(adapt_group = factor(adapt_group))

cat("✅ Adaptation groups used:\n")
print(levels(dat_maize$adapt_group))

# 5. Bootstrap regression function
# Save model formula for consistency
model_formula <- dp ~ poly(dt, 2, raw = TRUE) * adapt_group

# Build a template model on the full data to get reference coefficient names
ref_model <- lm(model_formula, data = dat_maize)
ref_coefs <- names(coef(ref_model))

# Safe bootstrap function
boot_fun <- function(d, i) {
  d2 <- d[i, ]
  tryCatch({
    fit <- lm(model_formula, data = d2)
    b <- coef(fit)
    # Ensure fixed length by padding with NA for any missing coefficients
    out <- rep(NA, length(ref_coefs))
    names(out) <- ref_coefs
    out[names(b)] <- b
    return(out)
  }, error = function(e) {
    # Return all NA if error occurs
    rep(NA, length(ref_coefs))
  })
}

# 6. Run bootstrap
set.seed(123)
boot_out <- boot(dat_maize, boot_fun, R = 500, strata = dat_maize$ref_no)

# 7. Create prediction grid
grid_df <- expand.grid(
  dt = seq(min(dat_maize$dt), max(dat_maize$dt), length.out = 100),
  adapt_group = levels(dat_maize$adapt_group)
)

# 8. Create design matrix
X <- model.matrix(~ poly(dt, 2, raw = TRUE) * adapt_group, data = grid_df)

# 9. Multiply to get predictions
pred_mat <- X %*% t(boot_out$t)

# 10. Summarize to CI bands
grid_df <- grid_df %>%
  mutate(
    fit  = rowMeans(pred_mat, na.rm = TRUE),
    lo95 = apply(pred_mat, 1, quantile, probs = 0.025, na.rm = TRUE),
    hi95 = apply(pred_mat, 1, quantile, probs = 0.975, na.rm = TRUE)
  )

# 11. Plot
ggplot() +
  geom_point(data = dat_maize, aes(x = dt, y = dp, color = adapt_group), alpha = 0.3) +
  geom_ribbon(data = grid_df, aes(x = dt, ymin = lo95, ymax = hi95, fill = adapt_group), alpha = 0.2) +
  geom_line(data = grid_df, aes(x = dt, y = fit, color = adapt_group), linewidth = 1.1) +
  labs(
    title = "African maize: Yield response to ΔT by adaptation strategy",
    subtitle = "Bootstrapped 95% CI by grouped adaptation",
    x = "ΔT from 2005 (°C)",
    y = "Yield change (%)",
    color = "Adaptation strategy",
    fill = "Adaptation strategy"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
