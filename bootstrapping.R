## ──────────────────────────────────────────────────────────────
##  African-legume yield-ΔT boot-strap plot
## ──────────────────────────────────────────────────────────────
## ------------------------------------------------------------
##  Climate-impacts legumes: clean + prepare for bootstrap
## ------------------------------------------------------------
##  0.  Packages  ----
library(readxl)
library(dplyr)
library(readr)     # parse_number()
library(stringr)
library(boot)      # bootstrap
library(ggplot2)   # plotting (optional)
library(janitor)

# 1.  File path & sheet name
xls_file  <- "yield_predictions_data_with_local_dT_filled.xlsx"
xls_sheet <- "Sheet1"   # adjust if needed
 
raw <- read_excel(xls_file, sheet = xls_sheet) %>%
  clean_names()  # ensures column names like "adaptation_type"

# 2. Subset to legumes
legumes <- c("Bambara", "Bush Bean", "Chickpea", "Climbing Bean",
             "Common Bean", "Cowpea", "Faba Bean", "Green grams",
             "Groundnut", "Soybean")

dat_legume <- raw %>%
  filter(crop %in% legumes)

# 3. Clean and prepare variables
dat <- dat_legume %>%
  mutate(
    dp = as.numeric(climate_impacts_percent),       # % yield change
    dt = as.numeric(local_delta_t_from_2005),       # ΔT
    study_id = factor(ref_no),                      # study ID
    adaptation = if_else(adaptation_type == "No", "No-adapt", "Adapted"),
    adaptation = factor(adaptation, levels = c("No-adapt", "Adapted"))
  ) %>%
  filter(!is.na(dp), !is.na(dt))

# Sanity check
cat("# rows after filtering:", nrow(dat), "\n")

# 4. Bootstrap regression function (fit model to resampled studies)
boot_fun <- function(d, i) {
  d2 <- d[i, ]
  fit <- lm(dp ~ poly(dt, 2, raw = TRUE) * adaptation, data = d2)
  coef(fit)
}

# 5. Run bootstrap
set.seed(123)
boot_out <- boot(dat, boot_fun, R = 500, strata = dat$study_id)

# 6. Make prediction grid
grid_df <- expand.grid(
  dt = seq(min(dat$dt), max(dat$dt), length.out = 100),
  adaptation = levels(dat$adaptation)
)

# 7. Create design matrix using same formula as in bootstrap
X <- model.matrix(~ poly(dt, 2, raw = TRUE) * adaptation, data = grid_df)

# 8. Multiply to get predictions for each bootstrap draw
#    Each column of pred_mat is predictions from one bootstrap model
pred_mat <- X %*% t(boot_out$t)

# 9. Summarize to CI bands
grid_df <- grid_df %>%
  mutate(
    fit  = rowMeans(pred_mat, na.rm = TRUE),
    lo95 = apply(pred_mat, 1, quantile, probs = 0.025, na.rm = TRUE),
    hi95 = apply(pred_mat, 1, quantile, probs = 0.975, na.rm = TRUE)
  )

# 10. Plot bootstrapped fit
ggplot() +
  geom_point(data = dat, aes(x = dt, y = dp, color = adaptation), alpha = 0.4) +
  geom_ribbon(data = grid_df, aes(x = dt, ymin = lo95, ymax = hi95, fill = adaptation), alpha = 0.2) +
  geom_line(data = grid_df, aes(x = dt, y = fit, color = adaptation), size = 1.2) +
  labs(
    title = "African legumes: Yield response to ΔT",
    subtitle = "Bootstrapped 95% CI",
    x = "ΔT from 2005 (°C)",
    y = "Yield change (%)",
    color = "Adaptation",
    fill = "Adaptation"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#=============================================================================
# Alternative method

# 1) read in your filled data
dat <- read_excel("yield_predictions_data_with_local_dT_filled.xlsx", sheet = "Sheet1")

# 2) keep only legumes & select the three fields we need
legumes <- c(
  "Bambara","Bush Bean","Chickpea","Climbing Bean","Common Bean",
  "Cowpea","Faba Bean","Green grams","Groundnut","Soybean"
)

dat2 <- dat %>%
  filter(Crop %in% legumes) %>% 
  transmute(
    dt             = as.numeric(`Local delta T from 2005`),
    dp             = as.numeric(`Climate impacts (%)`),
    AdaptationRaw  = as.character(`Adaptation type`)
  ) %>%
  filter(!is.na(dt), !is.na(dp), !is.na(AdaptationRaw))

# 3) collapse into just 2 categories
dat2 <- dat2 %>%
  mutate(
    Adaptation = if_else(AdaptationRaw == "No", "No-adapt", "Adapted"),
    Adaptation = factor(Adaptation, levels = c("No-adapt","Adapted"))
  )

# quick check
print(table(dat2$Adaptation))

# 4) plot
ggplot(dat2, aes(x = dt, y = dp, colour = Adaptation, fill = Adaptation)) +
  geom_point(alpha = 0.4, size = 1.5) +
  stat_smooth(
    method    = "loess",
    span      = 0.6,
    se        = TRUE,
    linewidth = 0.8,
    alpha     = 0.2
  ) +
  scale_colour_manual(values = c("No-adapt" = "#D95F02", "Adapted" = "#1B9E77")) +
  scale_fill_manual  (values = c("No-adapt" = "#D95F02", "Adapted" = "#1B9E77")) +
  labs(
    x      = expression(Delta * "T from 2005 (°C)"),
    y      = "Yield change (%)",
    title  = "African legume yield response\nNo-adapt vs. Adapted",
    colour = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
