library(readxl)
library(dplyr)
library(readr)     # parse_number()
library(stringr)
library(boot)      # bootstrap
library(ggplot2)   # plotting (optional)
library(janitor)
library(tidyr)
library(forcats)
library(purrr)
library(patchwork)

#=============================================================================
# Trt minus Control

# 1. Load and clean data
xls_file  <- "excel_data/yield_predictions_data_with_local_dT&dP_filled.xlsx"
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




#=============================================================================
# Trt WITH  Control

# 1. Load and clean data
xls_file  <- "excel_data/yield_predictions_data_with_local_dT&dP_filled.xlsx"
xls_sheet <- "Sheet1"

raw <- read_excel(xls_file, sheet = xls_sheet) %>%
  clean_names()  # ensures snake_case column names

# --- 2. Prepare data for all three crops ---
# Filter for Maize, Rice, and Wheat at the same time
filtered_data_all <- raw %>%
  filter(crop %in% c("Maize", "Rice", "Wheat"), adaptation_type != "Yield Potential") %>%
  mutate(
    dt = as.numeric(local_delta_t_from_2005),
    dp = as.numeric(climate_impacts_percent),
    adapt_group = case_when(
      adaptation_type == "No" ~ "No adaptation",
      adaptation_type %in% c("Cultivar", "Cultivar - Long duration", "Cultivar - Short duration") ~ "Variety",
      adaptation_type %in% c("High Fertilizer", "Medium Fertilizer", "Low Fertiliser", "Fertilizer") ~ "Fertilizer",
      adaptation_type == "Combined" ~ "Bundled",
      TRUE ~ adaptation_type
    ),
    adapt_group = factor(adapt_group),
    crop = factor(crop),
    ref_no = factor(ref_no)
  ) %>%
  filter(!adapt_group %in% c("Mulch", "Others")) %>%
  filter(!is.na(dt), !is.na(dp), !is.na(adapt_group))

filtered_data <- filtered_data_all %>%
  group_by(crop, adapt_group) %>%
  mutate(
    q1 = quantile(dp, 0.25, na.rm = TRUE),
    q3 = quantile(dp, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    lower_bound = q1 - 1.5 * iqr,
    upper_bound = q3 + 1.5 * iqr
  ) %>%
  filter(dp >= lower_bound & dp <= upper_bound) %>%
  ungroup()

cat("✅ Adaptation groups used:\\n")
print(levels(droplevels(filtered_data$adapt_group)))

# --- 3. Bootstrap prediction ---
nested_data <- filtered_data %>%
  group_by(crop) %>%
  nest()

run_bootstrap_for_crop <- function(df_crop) {
  # Precompute the dt range for the control group (No adaptation)
  control_range <- df_crop %>%
    filter(adapt_group == "No adaptation") %>%
    summarise(min_dt = min(dt, na.rm = TRUE), max_dt = max(dt, na.rm = TRUE))
  
  run_bootstrap_for_group <- function(df_group) {
    if (nrow(df_group) < 10 || n_distinct(df_group$dt) < 3) return(NULL)
    
    model_formula <- dp ~ poly(dt, 2, raw = TRUE)
    boot_func <- function(d, i) {
      d2 <- d[i, ]
      if (n_distinct(d2$dt) < 3) return(rep(NA, 3))
      fit <- lm(model_formula, data = d2)
      return(coef(fit))
    }
    
    set.seed(42)
    boot_out <- boot(df_group, boot_func, R = 500, strata = df_group$ref_no)
    if (is.null(boot_out) || all(is.na(boot_out$t))) return(NULL)
    
    # Get dt range for this group
    group_range <- df_group %>%
      summarise(min_dt = min(dt, na.rm = TRUE), max_dt = max(dt, na.rm = TRUE))
    
    # Clip to overlap between control and group dt ranges
    pred_min <- max(control_range$min_dt, group_range$min_dt)
    pred_max <- min(control_range$max_dt, group_range$max_dt)
    grid_df <- data.frame(dt = seq(pred_min, pred_max, length.out = 100))
    
    X <- model.matrix(~ poly(dt, 2, raw = TRUE), data = grid_df)
    pred_mat <- X %*% t(boot_out$t)
    
    valid_boots <- apply(pred_mat, 2, function(col) all(col > -200 & col < 200))
    pred_mat <- pred_mat[, valid_boots, drop = FALSE]
    
    grid_df %>%
      mutate(
        fit = rowMeans(pred_mat, na.rm = TRUE),
        lo95 = apply(pred_mat, 1, quantile, probs = 0.025, na.rm = TRUE),
        hi95 = apply(pred_mat, 1, quantile, probs = 0.975, na.rm = TRUE)
      ) %>%
      mutate(ribbon_width = hi95 - lo95) %>%
      filter(ribbon_width <= 70) %>%
      select(-ribbon_width)
  }
  
  df_crop %>%
    group_by(adapt_group) %>%
    nest() %>%
    mutate(predictions = map(data, run_bootstrap_for_group)) %>%
    select(adapt_group, predictions) %>%
    unnest(cols = c(predictions))
}

# Run for all crops
prediction_results <- nested_data %>%
  mutate(predictions = map(data, run_bootstrap_for_crop)) %>%
  select(crop, predictions) %>%
  unnest(cols = c(predictions))

# --- 4. Plotting ---

adaptation_palette <- c(
  "No adaptation" = "#66c2a5",  # teal
  "Fertilizer" = "#8da0cb",     # light blue
  "Variety" = "#a6d854",        # green
  "Bundled" = "#fc8d62",        # orange
  "Irrigation" = "#e78ac3",     # pink
  "Planting date" = "#ffd92f"   # yellow
)

create_crop_plot <- function(crop_name) {
  # Get control range for this crop
  control_range <- filtered_data %>%
    filter(crop == crop_name, adapt_group == "No adaptation") %>%
    summarise(min_dt = min(dt, na.rm = TRUE), max_dt = max(dt, na.rm = TRUE))
  
  # Only show data points within control ΔT range
  plot_data_points <- filtered_data %>%
    filter(crop == crop_name, dt >= control_range$min_dt, dt <= control_range$max_dt) %>%
    droplevels()
  
  plot_data_lines <- prediction_results %>%
    filter(crop == crop_name) %>%
    droplevels()
  
  used_adapt_levels <- unique(plot_data_lines$adapt_group)
  
  ggplot() +
    geom_point(data = plot_data_points, aes(x = dt, y = dp, color = adapt_group), alpha = 0.2) +
    geom_ribbon(data = plot_data_lines, aes(x = dt, ymin = lo95, ymax = hi95, fill = adapt_group), alpha = 0.2) +
    geom_line(data = plot_data_lines, aes(x = dt, y = fit, color = adapt_group), linewidth = 1) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.6, linetype = "dotted") +
    coord_cartesian(xlim = c(0.5, 5.5), ylim = c(-100, 100), expand = FALSE) +
    scale_color_manual(values = adaptation_palette, limits = used_adapt_levels, drop = TRUE) +
    scale_fill_manual(values = adaptation_palette, limits = used_adapt_levels, drop = TRUE) +
    guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
    labs(
      title = crop_name,
      x = if (crop_name == "Rice") "ΔT from 2005 (°C)" else "",
      y = if (crop_name == "Maize") "Yield change (%)" else ""
    ) +
    theme_bw(base_size = 14) +
    theme(
      legend.position = c(0.99, 0.99),
      legend.justification = c("right", "top"),
      legend.title = element_blank(),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(0.5, "cm"),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA),
      axis.line = element_line(color = "black")
    )
}

successful_crops <- unique(prediction_results$crop)
plot_list <- map(successful_crops, create_crop_plot)
final_plot <- wrap_plots(plot_list, nrow = 1)
print(final_plot)

# ============================================================================
# Bar plots
# --- 1. Load and clean data ---
# Ensure the file path is correct for your environment
xls_file <- "excel_data/yield_predictions_data_with_local_dT_filled.xlsx"
raw <- read_excel(xls_file, sheet = "Sheet1") %>%
  clean_names()

# --- 2. Filter for all relevant crops and prep data ---
# We now process Maize, Rice, and Wheat simultaneously.
filtered_data <- raw %>%
  filter(crop %in% c("Maize", "Rice", "Wheat"), adaptation_type != "Yield Potential")

# Separate control data (no adaptation) for all crops
control_data <- filtered_data %>%
  filter(adaptation_type == "No") %>%
  rename(yield_control = climate_impacts_percent)

# Separate treatment data (adaptation strategies) for all crops
treatment_data <- filtered_data %>%
  filter(adaptation_type != "No") %>%
  rename(yield_treat = climate_impacts_percent)

# --- 3. Create the Analysis Datasets using ABSOLUTE values ---

# Dataset 1: Adaptation strategies (using absolute values)
dat_adaptation <- treatment_data %>%
  transmute(
    ref_no = factor(ref_no),
    crop = factor(crop),
    dt = as.numeric(local_delta_t_from_2005),
    dp = as.numeric(yield_treat),
    adapt_raw = adaptation_type
  ) %>%
  mutate(
    # Consolidate related adaptation strategies for clarity
    adapt_group = case_when(
      adapt_raw %in% c("Cultivar", "Cultivar - Long duration", "Cultivar - Short duration") ~ "Cultivar",
      adapt_raw %in% c("Fertilizer", "High Fertilizer", "Medium Fertilizer", "Low Fertiliser") ~ "Fertilizer",
      TRUE ~ adapt_raw
    )
  )

# Dataset 2: Control group impacts (already using absolute values)
dat_control <- control_data %>%
  transmute(
    ref_no = factor(ref_no),
    crop = factor(crop),
    dt = as.numeric(local_delta_t_from_2005),
    dp = as.numeric(yield_control),
    adapt_group = "Control"
  )

# Combine into a single dataframe for analysis
all_dat <- bind_rows(dat_adaptation, dat_control) %>%
  # Remove strategies with little data; "Combined" is now kept.
  filter(!adapt_group %in% c("Mulch")) %>%
  # Remove data points where there is no temperature increase
  filter(round(dt) > 0) %>%
  mutate(
    # Center temperature on the nearest degree instead of using bins.
    dt_centered = factor(round(dt)),
    
    # Set the explicit factor order, now including "Combined"
    adapt_group = factor(adapt_group, levels = c("Control", "Combined", "Cultivar", "Fertilizer", "Irrigation", "Planting date"))
  ) %>%
  # Remove any rows with missing essential data
  filter(!is.na(dp), !is.na(dt_centered), !is.na(adapt_group), !is.na(crop))


# --- 4. Enforce Pairing and Bootstrap CIs ---

# To address the issue of lone 'Control' bars, we will only keep data for temperature
# points where there is both a 'Control' group and at least one adaptation treatment group.
all_dat_paired <- all_dat %>%
  group_by(crop, dt_centered) %>%
  filter("Control" %in% adapt_group & n_distinct(adapt_group) > 1) %>%
  ungroup()

# Define the bootstrap function
boot_ci <- function(x, R = 1000) {
  if (length(x) < 3) return(tibble(mean = NA_real_, lo = NA_real_, hi = NA_real_))
  b <- boot(x, statistic = function(data, i) mean(data[i]), R = R)
  ci <- quantile(b$t, c(0.025, 0.975), na.rm = TRUE)
  tibble(mean = mean(b$t, na.rm = TRUE), lo = ci[1], hi = ci[2])
}

# Create a complete grid of all possible combinations for plotting.
all_combinations <- expand_grid(
  crop = unique(all_dat_paired$crop),
  dt_centered = unique(all_dat_paired$dt_centered),
  adapt_group = unique(all_dat_paired$adapt_group)
)

# Apply the bootstrap function to each group
summary_df <- all_dat_paired %>%
  group_by(crop, dt_centered, adapt_group) %>%
  summarise(boot_stats = list(boot_ci(dp)), .groups = "drop") %>%
  unnest(boot_stats)

# Join summary stats with the complete grid.
plot_data <- all_combinations %>%
  left_join(summary_df, by = c("crop", "dt_centered", "adapt_group"))


# --- 5. Plotting ---
# This single plot now incorporates all the feedback.

# THE FIX: Filter out rows where no data exists AND remove the specific Wheat @ 5 degrees data point.
plot_data_final <- plot_data %>%
  filter(!is.na(mean)) %>%
  filter(!(crop == "Wheat" & dt_centered == "5"))

ggplot(plot_data_final, aes(x = dt_centered, y = mean, fill = adapt_group)) +
  
  # Use grouped bars.
  geom_col(position = "dodge", width = 0.8) +
  
  geom_errorbar(
    aes(ymin = lo, ymax = hi),
    position = position_dodge(width = 0.8),
    width = 0.25,
    color = "gray30"
  ) +
  
  # Create vertical facets for each crop
  facet_grid(crop ~ ., scales = "free_y") +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  scale_fill_brewer(palette = "Set2", name = "Adaptation Strategy:") +
  
  labs(
    title = "Projected Yield Change by Crop, Temperature, and Adaptation",
    subtitle = "Bars show absolute yield change (%). Controls are included for direct comparison.",
    x = "Centered Change in Temperature from 2005 (°C)",
    y = "Projected Yield Change (%)"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90", color = "gray90"),
    strip.text.y = element_text(face = "bold", size = 12, angle = 0), # Use strip.text.y for vertical facets
    panel.grid.minor = element_blank()
  )

###
### OPTION 2: POINT AND LINE CHART
### This is great for emphasizing trends across temperature bins.
###

# We will remove the 'Control' group for the main lines to avoid clutter,
# but we'll plot its points in the background for context.
control_points <- filter(plot_data, adapt_group == "Control")
adaptation_lines <- filter(plot_data, adapt_group != "Control")


ggplot(adaptation_lines, aes(x = dt_bin, y = mean, group = adapt_group, color = adapt_group)) +
  # Add control data points in the background for context
  geom_point(data = control_points, aes(y = mean), color = "gray50", size = 3, shape = 18) +
  geom_errorbar(data = control_points, aes(ymin = lo, ymax = hi), color = "gray50", width = 0.2) +
  # Main adaptation data
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_brewer(palette = "Set1", name = "Adaptation Strategy:") +
  labs(
    title = "Benefit of Maize Adaptation Strategies by Degree of Warming",
    subtitle = "Points are bootstrapped mean benefit over control. Control impact shown in grey.",
    x = "Change in Temperature from 2005 (°C)",
    y = expression(Delta~"Yield (%)")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.major.y = element_line(color = "gray90")
  )


### OPTION 3: SMOOTHED REGRESSION PLOT (like the example image)
### This plot shows all data points and trend lines on a continuous temperature scale.
###

ggplot(all_dat, aes(x = dt, y = dp, color = adapt_group)) +
  # Add semi-transparent points for all the raw data
  geom_point(alpha = 0.3) +
  # Add a smoothed regression line with a 95% confidence interval ribbon for each group
  geom_smooth(aes(fill = adapt_group), method = "loess", se = TRUE, alpha = 0.15) +
  # Use a clear color palette
  scale_color_brewer(palette = "Set2", name = "Adaptation Strategy") +
  scale_fill_brewer(palette = "Set2", name = "Adaptation Strategy") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Maize Yield Response to ΔT by Adaptation Strategy",
    subtitle = "Showing absolute projected yield change with 95% CI regression bands.",
    x = "ΔT from 2005 (°C)",
    y = "Projected Yield Change (%)"
  ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")
