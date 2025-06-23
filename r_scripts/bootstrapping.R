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
xls_file  <- "excel_data/yield_predictions_data_with_local_dT_filled.xlsx"
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
    adaptation = if_else(adaptation_type == "No", "No-adaptation", "Adaptation"),
    adaptation = factor(adaptation, levels = c("No-adaptation", "Adaptation"))
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
# Count number of observations by adaptation group
obs_counts <- dat %>%
  count(adaptation) %>%
  mutate(label = paste0("n = ", n))

temp_ad_vs_noad<-ggplot() +
  geom_point(data = dat, aes(x = dt, y = dp, color = adaptation), alpha = 0.4) +
  geom_ribbon(data = grid_df, aes(x = dt, ymin = lo95, ymax = hi95, fill = adaptation), alpha = 0.2) +
  geom_line(data = grid_df, aes(x = dt, y = fit, color = adaptation), size = 1.2) +
  # Add text labels with counts
  geom_text(data = obs_counts,
            aes(x = 5, y = c(110, 90), label = label, color = adaptation),
           hjust = 1, size = 4.5, show.legend = FALSE) +
  labs(
    title = "African legumes: Yield response to ΔT",
    subtitle = "Bootstrapped 95% CI",
    x = "ΔT from 2005 (°C)",
    y = "Yield change (%)",
    color = "Adaptation",
    fill = "Adaptation"
  ) +
  ylim(-150, 150) +
  theme_minimal() +
  theme(legend.position = "bottom")

temp_ad_vs_noad

#=============================================================================
# WITH PRECIP
# 1.  File path & sheet name
xls_file  <- "excel_data/yield_predictions_data_with_local_dT&dP_filled.xlsx"
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
    dprec = as.numeric(annual_precipitation_change_from_2005_mm_cal),       # ΔT
    study_id = factor(ref_no),                      # study ID
    adaptation = if_else(adaptation_type == "No", "No-adaptation", "Adaptation"),
    adaptation = factor(adaptation, levels = c("No-adaptation", "Adaptation"))
  ) %>%
  filter(!is.na(dp), !is.na(dprec))

# Sanity check
cat("# rows after filtering:", nrow(dat), "\n")

# 4. Bootstrap regression function (fit model to resampled studies)
boot_fun <- function(d, i) {
  d2 <- d[i, ]
  fit <- lm(dp ~ poly(dprec, 2, raw = TRUE) * adaptation, data = d2)
  coef(fit)
}

# 5. Run bootstrap
set.seed(123)
boot_out <- boot(dat, boot_fun, R = 500, strata = dat$study_id)

# 6. Make prediction grid
grid_df <- expand.grid(
  dprec = seq(min(dat$dprec), max(dat$dprec), length.out = 100),
  adaptation = levels(dat$adaptation)
)

# 7. Create design matrix using same formula as in bootstrap
X <- model.matrix(~ poly(dprec, 2, raw = TRUE) * adaptation, data = grid_df)

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
# Count number of observations by adaptation group
obs_counts <- dat %>%
  count(adaptation) %>%
  mutate(label = paste0("n = ", n))

# Plot
precip_ad_vs_noad <- ggplot() +
  geom_point(data = dat, aes(x = dprec, y = dp, color = adaptation), alpha = 0.4) +
  geom_ribbon(data = grid_df, aes(x = dprec, ymin = lo95, ymax = hi95, fill = adaptation), alpha = 0.2) +
  geom_line(data = grid_df, aes(x = dprec, y = fit, color = adaptation), size = 1.2) +
  # Add text labels with counts
  geom_text(data = obs_counts,
            aes(x = 200, y = c(110, 90), label = label, color = adaptation),
            hjust = 1, size = 4.5, show.legend = FALSE) +
  labs(
    title = "African legumes: Yield response to ΔP",
    subtitle = "Bootstrapped 95% CI",
    x = "ΔP from 2005 (mm)",
    y = "Yield change (%)",
    color = "Adaptation",
    fill = "Adaptation"
  ) +
  xlim(-200, 200) +
  ylim(-150, 150) +
  theme_minimal() +
  theme(legend.position = "bottom")

precip_ad_vs_noad
#=============================================================================
# Combining plots
# Load the library
library(patchwork)

# Combine side by side
temp_ad_vs_noad + precip_ad_vs_noad


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
    Adaptation = if_else(AdaptationRaw == "No", "No-adaptation", "Adaptation"),
    Adaptation = factor(Adaptation, levels = c("No-adaptation","Adaptation"))
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

#==============================================================================
# Resampling based on crop model for adaptation
library(broom)
library(purrr)
xls_file  <- "excel_data/yield_predictions_data_with_local_dT&dP_filled.xlsx"
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
    adaptation = if_else(adaptation_type == "No", "No-adaptation", "Adaptation"),
    adaptation = factor(adaptation, levels = c("No-adaptation", "Adaptation"))
  ) %>%
  filter(!is.na(dp), !is.na(dt))

# Sanity check
cat("# rows after filtering:", nrow(dat), "\n")
# 1. Define function to bootstrap slope vs. temperature ---------------------
boot_slope <- function(df_group, R = 500) {
  if (nrow(df_group) < 3 || all(is.na(df_group$dt)) || all(is.na(df_group$dp))) {
    return(tibble(slope_mean = NA, ci_low = NA, ci_high = NA))
  }
  
  slope_fun <- function(d, i) {
    d2 <- d[i, ]
    if (length(unique(d2$dt)) < 2) return(NA_real_)
    coef <- tryCatch(coef(lm(dp ~ dt, data = d2))[2], error = function(e) NA_real_)
    return(coef)
  }
  
  bt <- boot(df_group, slope_fun, R = R)
  valid_slopes <- bt$t[!is.na(bt$t)]
  if (length(valid_slopes) < 10) {
    return(tibble(slope_mean = NA, ci_low = NA, ci_high = NA))
  }
  
  tibble(
    slope_mean = mean(valid_slopes, na.rm = TRUE),
    ci_low = quantile(valid_slopes, 0.025, na.rm = TRUE),
    ci_high = quantile(valid_slopes, 0.975, na.rm = TRUE)
  )
}

# 2. Apply bootstrapping by crop model × adaptation -------------------------
slope_results <- dat %>%
  group_by(crop_model, adaptation) %>%
  group_split() %>%
  map_dfr(~ {
    group_info <- .x %>% slice(1) %>% select(crop_model, adaptation)
    slope_summary <- boot_slope(.x, R = 500)
    bind_cols(group_info, slope_summary)
  })

# 3. Filter out unhelpful model categories ----------------------------------
slope_results <- slope_results %>%
  filter(!crop_model %in% c("Other", "NA", "Unspecified", "Statistical model"))

# 4. Plot the bootstrapped slopes -------------------------------------------
ggplot(slope_results, aes(x = crop_model, y = slope_mean, fill = adaptation)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "ΔYield per °C by Crop Model and Adaptation Status",
    x = "Crop Model",
    y = "ΔYield per °C (bootstrapped)",
    fill = "Adaptation"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#===============================================================================
# Control vs trt

xls_file  <- "excel_data/yield_predictions_data_with_local_dT&dP_filled.xlsx"
xls_sheet <- "Sheet1"   # adjust if needed

raw <- read_excel(xls_file, sheet = xls_sheet) %>%
  clean_names()  # ensures column names like "adaptation_type"

# 2. Subset to legumes
legumes <- c("Bambara", "Bush Bean", "Chickpea", "Climbing Bean",
             "Common Bean", "Cowpea", "Faba Bean", "Green grams",
             "Groundnut", "Soybean")
raw <- raw %>%
  filter(crop %in% legumes)

filtered_data <- raw %>%
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

# 5. Prepare for bootstrapping ----------------------------------------------
dat <- merged_data %>%
  mutate(
    d_yield = yield_treat - yield_control,
    adapt_group = case_when(
      adaptation_type.y %in% c("Fertilizer", "High Fertilizer", "Medium Fertilizer", "Low Fertiliser") ~ "Fertilizer",
      adaptation_type.y %in% c("Cultivar", "Cultivar - Long duration", "Cultivar - Short duration") ~ "Cultivar",
      TRUE ~ adaptation_type.y
    )
  ) %>%
  filter(!is.na(d_yield)) %>%
  mutate(adapt_group = factor(adapt_group))

# 6. Bootstrap function -----------------------------------------------------
boot_ci <- function(d) {
  boot_mean <- function(data, i) mean(data[i], na.rm = TRUE)
  bt <- boot(d, boot_mean, R = 1000)
  ci <- boot.ci(bt, type = "perc")
  tibble(
    mean = mean(d, na.rm = TRUE),
    lower = ci$percent[4],
    upper = ci$percent[5]
  )
}

# 7. Apply bootstrap by adaptation group ------------------------------------
results <- dat %>%
  group_by(adapt_group) %>%
  summarise(boot_ci = list(boot_ci(d_yield)), .groups = "drop") %>%
  unnest(cols = boot_ci)

# 8. Plot -------------------------------------------------------------------
ggplot(results, aes(x = reorder(adapt_group, -mean), y = mean)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Bootstrapped Mean ΔYield by Adaptation Strategy",
    x = "Adaptation Strategy",
    y = "Δ Yield (%) vs. Control (Bootstrapped 95% CI)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
