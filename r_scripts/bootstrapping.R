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
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(gridExtra)

# 1.  File path & sheet name
xls_file  <- "excel_data/yield_predictions_data_with_nextgddp_ensemble_deltas.xlsx"
xls_sheet <- "Sheet1"   # adjust if needed
 
raw <- read_excel(xls_file, sheet = xls_sheet) %>%
  clean_names()  # ensures column names like "adaptation_type"

# 2. Subset to legumes
legumes <- c("Bambara", "Bush Bean", "Chickpea", "Climbing Bean",
             "Common Bean", "Cowpea", "Faba Bean", "Green grams",
             "Groundnut", "Soybean")

dat_legume <- raw %>%
  filter(crop %in% legumes)
############################################################

# Create summary table
summary_table <- dat_legume %>%
  # Keep only relevant columns
  select(reference, co2, fertiliser, irrigation, cultivar, 
         soil_organic_matter_management, planting_time, tillage, others, adaptation_type) %>%
  # For each row, pivot the practice columns to identify which are "Yes"
  mutate(row_id = row_number()) %>%
  pivot_longer(
    cols = c(fertiliser, irrigation, cultivar, soil_organic_matter_management, 
             planting_time, tillage, others),
    names_to = "practice",
    values_to = "value"
  ) %>%
  filter(value == "Yes") %>%
  # Clean up practice names
  mutate(practice = case_when(
    practice == "fertiliser"                    ~ "Fertiliser",
    practice == "irrigation"                    ~ "Irrigation",
    practice == "cultivar"                      ~ "Cultivar",
    practice == "soil_organic_matter_management"~ "Soil Organic Matter Management",
    practice == "planting_time"                 ~ "Planting Time",
    practice == "tillage"                       ~ "Tillage",
    practice == "others"                        ~ "Others"
  )) %>%
  # Keep unique reference x practice x co2 combinations
  distinct(reference, co2, practice, adaptation_type) %>%
  arrange(reference, practice) %>%
  rename(
    Reference       = reference,
    CO2_modelled    = co2,
    Practice        = practice,
    Adaptation_type = adaptation_type
  )

# View
print(summary_table)

# Optional: save to CSV
write_csv(filtered_data, "excel_data/legume_adaptation_summary.csv")
######Plot papers###########################################
# 3. Clean and recode crop types
dat_legume <- raw %>%
  filter(crop %in% legumes) %>%
  mutate(
    crop_grouped = case_when(
      crop %in% c("Bush Bean", "Climbing Bean", "Common Bean") ~ "Common Bean",
      TRUE ~ crop
    )
  )


# 4. Group rare legumes into "Other Legumes"
crop_counts <- dat_legume %>%
  group_by(crop_grouped) %>%
  summarise(N = n_distinct(reference), .groups = "drop") %>%
  mutate(
    crop_final = if_else(N == 1, "Other Legumes", crop_grouped)
  )

# 5. Join back to main dataset
dat_legume <- dat_legume %>%
  left_join(crop_counts %>% select(crop_grouped, crop_final), by = "crop_grouped")

# 6. Clean coordinates
filtered_data <- dat_legume %>%
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  ) %>%
  filter(!is.na(longitude) & !is.na(latitude))

# 7. Count papers per country
paper_counts <- filtered_data %>%
  group_by(country) %>%
  summarise(N_Papers = n_distinct(reference), .groups = "drop")

# 8. Convert to spatial and prepare map base
filtered_sf <- st_as_sf(filtered_data, coords = c("longitude", "latitude"), crs = 4326)

world <- rnaturalearth::ne_countries(scale = "medium", continent = "Africa", returnclass = "sf") %>%
  mutate(admin = if_else(admin == "United Republic of Tanzania", "Tanzania", admin)) %>%
  st_transform(crs = 4326)

map_data <- world %>%
  select(admin, geometry) %>%
  rename(country = admin) %>%
  left_join(paper_counts, by = "country")

# 9. Create map
map <- ggplot() +
  geom_sf(data = map_data, aes(fill = N_Papers), color = "white") +
  geom_point(data = filtered_data, aes(x = longitude, y = latitude),
             shape = 21, color = "black", fill = "white", size = 2, alpha = 0.5) +
  scale_fill_viridis_c(option = "mako", direction = -1, na.value = "gray95") +
  labs(fill = "Crop Papers") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.5, title.position = "top", title.hjust = 0.5)) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35, 50), expand = FALSE)

# 10. Create table of top crops (with grouping)
final_crop_counts <- dat_legume %>%
  group_by(crop_final) %>%
  summarise(N = n_distinct(reference), .groups = "drop") %>%
  arrange(desc(N))

table_grob <- gridExtra::tableGrob(final_crop_counts)

# 11. Combine map and table
gridExtra::grid.arrange(map, table_grob, ncol = 2, widths = c(2, 1))

#########Back to the analysis##############################################
# 3. Clean and prepare variables
dat_all <- dat  # save full dataset before outlier removal
dat <- dat_legume %>%
  mutate(
    dp = as.numeric(climate_impacts_percent),
    dt = as.numeric(temp_change_nextgddp),
    study_id = factor(ref_no),
    adaptation = if_else(adaptation_type == "No", "No-adaptation", "Adaptation"),
    adaptation = factor(adaptation, levels = c("No-adaptation", "Adaptation"))
  ) %>%
  filter(!is.na(dp), !is.na(dt))

cat("# rows before outlier removal:", nrow(dat), "\n")

# 3b. Remove outliers using IQR method per adaptation group
dat <- dat %>%
  group_by(adaptation) %>%
  mutate(
    Q1    = quantile(dp, 0.25, na.rm = TRUE),
    Q3    = quantile(dp, 0.75, na.rm = TRUE),
    IQR   = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR
  ) %>%
  filter(dp >= lower & dp <= upper) %>%
  select(-Q1, -Q3, -IQR, -lower, -upper) %>%
  ungroup()

cat("# rows after outlier removal:", nrow(dat), "\n")

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
# Temp with CO2 impacts
# 1. Load data
xls_file  <- "excel_data/yield_predictions_data_with_nextgddp_yearly_baseline_deltas.xlsx"
xls_sheet <- "Sheet1"

raw <- read_excel(xls_file, sheet = xls_sheet) %>%
  clean_names()

# 2. Define crop lists
legumes <- c("Bambara", "Bush Bean", "Chickpea", "Climbing Bean",
             "Common Bean", "Cowpea", "Faba Bean", "Green grams",
             "Groundnut", "Soybean")
# Assuming 'Maize' is the crop name in your data
maize <- c("Maize")


# 3. Clean and prepare data for BOTH crop types
# This step now includes a 'crop_type' column
dat <- raw %>%
  mutate(
    # Create the new crop_type column
    crop_type = case_when(
      crop %in% legumes ~ "Legumes",
      crop %in% maize   ~ "Maize",
      TRUE              ~ "Other"
    )
  ) %>%
  # Filter to keep only the crop types we want
  filter(crop_type %in% c("Legumes", "Maize")) %>%
  mutate(
    dp = as.numeric(climate_impacts_percent),
    dt = as.numeric(temp_change_nextgddp),
    study_id = factor(ref_no),
    adaptation = if_else(adaptation_type == "No", "No-adaptation", "Adaptation"),
    adaptation = factor(adaptation, levels = c("No-adaptation", "Adaptation")),
    co2_group = case_when(
      co2 == "Yes" ~ "With CO2 fertilization",
      co2 %in% c("No", "Unspecified", NA) ~ "Without CO2 fertilization"
    )
  ) %>%
  filter(!is.na(dp), !is.na(dt), !is.na(co2_group))


# Save full dataset before outlier removal (for supplementary)
dat_all <- dat

cat("# rows before outlier removal:", nrow(dat), "\n")

# 3b. Remove outliers using IQR method per adaptation group x crop_type x co2_group
dat <- dat %>%
  group_by(adaptation, crop_type, co2_group) %>%
  mutate(
    Q1    = quantile(dp, 0.25, na.rm = TRUE),
    Q3    = quantile(dp, 0.75, na.rm = TRUE),
    IQR   = Q3 - Q1,
    lower = Q1 - 3 * IQR,
    upper = Q3 + 3 * IQR
  ) %>%
  filter(dp >= lower & dp <= upper) %>%
  select(-Q1, -Q3, -IQR, -lower, -upper) %>%
  ungroup()

cat("# rows after outlier removal:", nrow(dat), "\n")


# 4. Bootstrap regression function (remains the same)
boot_fun <- function(d, i) {
  d2 <- d[i, ]
  fit <- lm(dp ~ poly(dt, 2, raw = TRUE) * adaptation, data = d2)
  coef(fit)
}

# 5. Function to run bootstrapping (remains the same, but will be applied to more groups)
get_grid <- function(subdat, group_name) {
  if (nrow(subdat) == 0) {
    warning(paste("No data available for the group:", group_name, ". Skipping bootstrap."), call. = FALSE)
    return(NULL)
  }
  set.seed(123)
  boot_out <- boot(subdat, boot_fun, R = 1000, strata = subdat$study_id)
  grid <- expand.grid(
    dt = seq(min(subdat$dt, na.rm = TRUE), max(subdat$dt, na.rm = TRUE), length.out = 100),
    adaptation = levels(subdat$adaptation)
  )
  X <- model.matrix(~ poly(dt, 2, raw = TRUE) * adaptation, data = grid)
  pred_mat <- X %*% t(boot_out$t)
  grid %>%
    mutate(
      fit = rowMeans(pred_mat, na.rm = TRUE),
      lo95 = apply(pred_mat, 1, quantile, probs = 0.025, na.rm = TRUE),
      hi95 = apply(pred_mat, 1, quantile, probs = 0.900, na.rm = TRUE)
    )
}

# 6. Run analysis for all four groups (Crop Type x CO2 Group)
# We can now use group_by() and nest() to apply this cleanly to all groups
all_grids <- dat %>%
  group_by(crop_type, co2_group) %>%
  nest() %>%
  mutate(
    # Create a name for each group for the warning message
    group_name = paste(crop_type, co2_group),
    # Apply the get_grid function to the data for each group
    grid_data = purrr::map2(data, group_name, get_grid)
  ) %>%
  select(crop_type, co2_group, grid_data) %>%
  # Unnest the results to create a single large dataframe
  unnest(cols = grid_data)

# --- ROBUSTNESS CHECK: Stop if no data was processed ---
if (nrow(all_grids) == 0) {
  stop("No groups contained data to process. Cannot create a plot. Please check your input data and filters.", call. = FALSE)
}
# --- End of check ---


# 7. NEW: Create labels with observation counts for the plot
plot_labels <- dat %>%
  group_by(crop_type, co2_group, adaptation) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    # Create the text label, e.g., "Adaptation (n=123)"
    label = paste0(adaptation, " (n=", n, ")"),
    # Define x and y positions for the labels inside the plot
    x_pos = 2.5, # Start near the left edge (instead of 3)
    # *** FIX HERE: Use Y positions that are visible (e.g., 55 and 45) ***
    y_pos = if_else(adaptation == "Adaptation", 55, 45) 
  )

# 8. Plot the results with in-panel legends
ggplot() +
  # Use the main 'dat' frame for points
  geom_point(data = dat_all, aes(x = dt, y = dp, color = adaptation), alpha = 0.35, size = 1.5) +
  
  # Use the full, unfiltered 'all_grids' for ribbons and lines
  geom_ribbon(data = all_grids, aes(x = dt, ymin = lo95, ymax = hi95, fill = adaptation), alpha = 0.3) +
  geom_line(data = all_grids, aes(x = dt, y = fit, color = adaptation), linewidth = 1.2) +
  
  # Add in-panel text labels (sample sizes)
  geom_text(
    data = plot_labels,
    aes(x = x_pos, y = y_pos, label = label, color = adaptation),
    hjust = 0,
    fontface = "bold",
    size = 5  # Increased for better readability
  ) +
  
  # Optional dashed model line (e.g., all-crop trend)
  #geom_line(data = model_line, aes(x = dt, y = pred), 
  #          color = "black", linetype = "dashed", size = 1) +
  
  # Custom colors
  scale_color_manual(values = c("No-adaptation" = "#E4717A", "Adaptation" = "#5D99C6")) +
  scale_fill_manual(values = c("No-adaptation" = "#E4717A", "Adaptation" = "#5D99C6")) +
  
  # Facet by crop and CO2 group
  facet_grid(crop_type ~ co2_group) +
  
  # Axis labels
  labs(
    x = "Annual Mean Temperature Change (ΔT) °C",
    y = "Yield Change (%)"
  ) +
  
  # Define axis breaks and limits
  scale_x_continuous(
    limits = c(0, 4.5),
    breaks = seq(0.5, 4.5, by = 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(-80, 80, by = 20),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(-60, 60)) +
  
  # Theme adjustments for poster clarity
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "grey85", color = "black"),
    strip.text = element_text(face = "bold", color = "black", size = 14),   # Facet labels
    panel.spacing = unit(1, "lines"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 13),         # Axis tick labels
    axis.title = element_text(size = 15, face = "bold")  # Axis titles
  )


# Count unique studies by CO2 group
co2_counts_legumes <- l %>%
  mutate(
    co2_group = case_when(
      co2 == "Yes" ~ "With CO2 fertilization",
      co2 %in% c("No", "Unspecified", NA) ~ "Without CO2 fertilization"
    )
  ) %>%
  group_by(ref_no) %>%
  summarise(
    has_with_co2 = any(co2_group == "With CO2 fertilization"),
    has_without_co2 = any(co2_group == "Without CO2 fertilization")
  ) %>%
  ungroup() %>%
  summarise(
    only_with_co2 = sum(has_with_co2 & !has_without_co2),
    only_without_co2 = sum(!has_with_co2 & has_without_co2),
    both_co2 = sum(has_with_co2 & has_without_co2),
    total_studies = n()
  )

print(co2_counts_legumes)

co2_counts_maize <- m %>%
  mutate(
    co2_group = case_when(
      co2 == "Yes" ~ "With CO2 fertilization",
      co2 %in% c("No", "Unspecified", NA) ~ "Without CO2 fertilization"
    )
  ) %>%
  group_by(ref_no) %>%
  summarise(
    has_with_co2 = any(co2_group == "With CO2 fertilization"),
    has_without_co2 = any(co2_group == "Without CO2 fertilization")
  ) %>%
  ungroup() %>%
  summarise(
    only_with_co2 = sum(has_with_co2 & !has_without_co2),
    only_without_co2 = sum(!has_with_co2 & has_without_co2),
    both_co2 = sum(has_with_co2 & has_without_co2),
    total_studies = n()
  )

print(co2_counts_maize)


    
#################Same but for rainfall

# 1. Load data
xls_file  <- "excel_data/yield_predictions_data_with_nextgddp_yearly_baseline_deltas.xlsx"
xls_sheet <- "Sheet1"

raw <- read_excel(xls_file, sheet = xls_sheet) %>%
  clean_names()

# 2. Define crop lists
legumes <- c("Bambara", "Bush Bean", "Chickpea", "Climbing Bean",
             "Common Bean", "Cowpea", "Faba Bean", "Green grams",
             "Groundnut", "Soybean")
maize <- c("Maize")

# 3. Clean and prepare data
dat <- raw %>%
  mutate(
    crop_type = case_when(
      crop %in% legumes ~ "Legumes",
      crop %in% maize   ~ "Maize",
      TRUE              ~ "Other"
    )
  ) %>%
  filter(crop_type %in% c("Legumes", "Maize")) %>%
  mutate(
    dp = as.numeric(climate_impacts_percent),
    dprecip = as.numeric(precip_change_nextgddp),
    study_id = factor(ref_no),
    adaptation = if_else(adaptation_type == "No", "No-adaptation", "Adaptation"),
    adaptation = factor(adaptation, levels = c("No-adaptation", "Adaptation")),
    co2_group = case_when(
      co2 == "Yes" ~ "With CO2 fertilization",
      co2 %in% c("No", "Unspecified", NA) ~ "Without CO2 fertilization"
    )
  ) %>%
  filter(!is.na(dp), !is.na(dprecip), !is.na(co2_group))

# Save full dataset before outlier removal
dat_all <- dat

# Apply 3x IQR outlier removal
dat <- dat %>%
  group_by(adaptation, crop_type, co2_group) %>%
  mutate(
    Q1    = quantile(dp, 0.25, na.rm = TRUE),
    Q3    = quantile(dp, 0.75, na.rm = TRUE),
    IQR   = Q3 - Q1,
    lower = Q1 - 3 * IQR,
    upper = Q3 + 3 * IQR
  ) %>%
  filter(dp >= lower & dp <= upper) %>%
  select(-Q1, -Q3, -IQR, -lower, -upper) %>%
  ungroup()


# 4. Bootstrap regression function
boot_fun <- function(d, i) {
  d2 <- d[i, ]
  fit <- lm(dp ~ poly(dprecip, 2, raw = TRUE) * adaptation, data = d2)
  coef(fit)
}

# 5. Function to run bootstrapping
get_grid <- function(subdat, group_name) {
  if (nrow(subdat) == 0) {
    warning(paste("No data available for the group:", group_name, ". Skipping bootstrap."), call. = FALSE)
    return(NULL)
  }
  set.seed(123)
  boot_out <- boot(subdat, boot_fun, R = 1000, strata = subdat$study_id)
  grid <- expand.grid(
    dprecip = seq(min(subdat$dprecip, na.rm = TRUE), max(subdat$dprecip, na.rm = TRUE), length.out = 100),
    adaptation = levels(subdat$adaptation)
  )
  X <- model.matrix(~ poly(dprecip, 2, raw = TRUE) * adaptation, data = grid)
  pred_mat <- X %*% t(boot_out$t)
  grid %>%
    mutate(
      fit = rowMeans(pred_mat, na.rm = TRUE),
      lo95 = apply(pred_mat, 1, quantile, probs = 0.025, na.rm = TRUE),
      hi95 = apply(pred_mat, 1, quantile, probs = 0.975, na.rm = TRUE)
    )
}

# 6. Run bootstrapping for all groups
all_grids <- dat %>%
  group_by(crop_type, co2_group) %>%
  nest() %>%
  mutate(
    group_name = paste(crop_type, co2_group),
    grid_data = purrr::map2(data, group_name, get_grid)
  ) %>%
  select(crop_type, co2_group, grid_data) %>%
  unnest(cols = grid_data) %>%
  mutate(uncertainty_width = hi95 - lo95) %>%
  filter(uncertainty_width <= 80) %>%
  ungroup()

# 7. Create plot labels with observation counts
plot_labels <- dat %>%
  group_by(crop_type, co2_group, adaptation) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    label = paste0(adaptation, " (n=", n, ")")
  )

# 8. Plot
ggplot() +
  geom_point(
    data = dat,
    aes(x = dprecip, y = dp, color = adaptation),
    alpha = 0.35, size = 2.5
  ) +
  geom_ribbon(
    data = all_grids,
    aes(x = dprecip, ymin = lo95, ymax = hi95, fill = adaptation),
    alpha = 0.3
  ) +
  geom_line(
    data = all_grids,
    aes(x = dprecip, y = fit, color = adaptation),
    linewidth = 1.5
  ) +
  geom_text(
    data = plot_labels,
    aes(x = 20, y = if_else(adaptation == "Adaptation", 147, 128),
        label = label, color = adaptation),
    hjust = 0, fontface = "bold", size = 5  # changed from 8 to 5
  ) +
  scale_color_manual(values = c("No-adaptation" = "orangered2", "Adaptation" = "turquoise4")) +
  scale_fill_manual(values = c("No-adaptation" = "orangered2", "Adaptation" = "turquoise4")) +
  facet_grid(crop_type ~ co2_group) +
  labs(
    x = "Local mean precipitation change (%)",
    y = "Yield Change (%)"
  ) +
  scale_x_continuous(
    limits = c(-15, 50),
    breaks = seq(-10, 50, by = 10),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(-80, 150)) +
  theme_bw(base_size = 20) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "grey85", color = "black"),
    strip.text = element_text(face = "bold", color = "black", size = 14),
    panel.spacing = unit(1, "lines"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 17, face = "bold")
  )

#=============================================================================
# WITH PRECIP
# 1.  File path & sheet name
xls_file  <- "excel_data/yield_predictions_data_with_nextgddp_ensemble_deltas.xlsx"
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
    dp = as.numeric(climate_impacts_percent),         # % yield change
    dprec = as.numeric(annual_precipitation_change_from_2005_mm_cal),         # ΔP
    study_id = factor(ref_no),                        # study ID
    adaptation = if_else(adaptation_type == "No", "No-adaptation", "Adaptation"),
    adaptation = factor(adaptation, levels = c("No-adaptation", "Adaptation"))
  ) %>%
  filter(!is.na(dp), !is.na(dprec)) %>%
  
  # --- KEY CHANGE: Filter data before bootstrapping ---
  # Keep only projections where the precipitation change is between -50mm and +50mm
  filter(dprec >= -50 & dprec <= 160)


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
# The grid is now automatically created for the new, filtered dprec range
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
            aes(x = 140, y = c(140, 120), label = label, color = adaptation),
            hjust = 1, size = 3.5, show.legend = FALSE) +
  labs(
    title = "African legumes: Yield response to ΔP",
    subtitle = "Bootstrapped 95% CI",
    x = "ΔP from 2005 (mm)",
    y = "Yield change (%)",
    color = "Adaptation",
    fill = "Adaptation"
  ) +
  
  # --- KEY CHANGE: Use coord_cartesian to set plot limits ---
  # This "zooms" the plot without removing any data points from the layers
  coord_cartesian(xlim = c(-50, 160), ylim = c(-150, 150), expand = FALSE) +
  
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
dat <- read_excel("excel_data/yield_predictions_data_with_local_dT_filled.xlsx", sheet = "Sheet1")

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
xls_file  <- "excel_data/yield_predictions_data_with_local_dT&dP_filled_update_final.xlsx"
xls_sheet <- "Sheet1"   # adjust if needed

raw <- read_excel(xls_file, sheet = xls_sheet) %>%
  clean_names()  # ensures column names like "adaptation_type"

# 2. Subset to legumes
legumes <- c("Bambara", "Bush Bean", "Chickpea", "Climbing Bean",
             "Common Bean", "Cowpea", "Faba Bean", "Green grams",
             "Groundnut", "Soybean")

# Filter for legumes and remove unhelpful model categories
dat <- raw %>%
  filter(crop %in% legumes) %>%
  filter(!is.na(crop_model), !crop_model %in% c("Other", "NA", "Unspecified", "Statistical model")) %>%
  mutate(
    # Combine DSSAT and CROPGRO into a single "CROPGRO" category
    crop_model = if_else(crop_model %in% c("DSSAT", "CROPGRO"), "CROPGRO", crop_model),
    
    # Create a clean "Adaptation" vs "No-adaptation" variable
    adaptation = if_else(adaptation_type == "No", "No-adaptation", "Adaptation"),
    
    # Create warming level bins from the temperature change (ΔT)
    dt = as.numeric(local_delta_t_from_2005),
    warming_level = cut(
      dt,
      breaks = c(-Inf, 1, 2, 3, Inf),
      labels = c("0-1°C", "1-2°C", "2-3°C", ">3°C"),
      right = FALSE
    )
  ) %>%
  # Remove any rows where the warming level or adaptation type is missing
  filter(!is.na(warming_level), !is.na(adaptation))

# --- 3. Summarize data for plotting ---
# Count the number of unique studies for each combination of model, warming level, and adaptation
model_summary <- dat %>%
  group_by(crop_model, warming_level, adaptation) %>%
  summarise(n_studies = n_distinct(ref_no), .groups = "drop")

# --- 4. Create the faceted bar chart ---
# This plot is now faceted by Adaptation strategy, with bars stacked by warming level.
ggplot(model_summary, aes(x = fct_reorder(crop_model, n_studies, .fun = sum, .desc = TRUE), y = n_studies, fill = warming_level)) +
  
  # geom_col creates the bars. position="stack" is the default and creates the stacked bars.
  geom_col(position = "stack") +
  
  # Flip coordinates to make model names readable
  coord_flip() +
  
  # Create a separate plot for each adaptation strategy
  facet_wrap(~ adaptation, ncol = 2) +
  
  # Use a clear, colorblind-friendly color palette
  scale_fill_viridis_d(option = "viridis", name = "Warming Level") +
  
  # Set breaks for the y-axis (which becomes the x-axis after coord_flip) to be whole numbers
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  
  # Set labels and title
  labs(
    title = "How Crop Models are Used to Study Legumes",
    subtitle = "Comparing studies with and without adaptation measures across different warming scenarios",
    x = "Crop Model",
    y = "Number of Unique Studies"
  ) +
  
  # Apply a clean theme and adjust text
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    # Make the facet labels (adaptation strategy) stand out
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(face = "bold", size = 11, color = "black"),
    panel.grid.major.y = element_blank() # Remove horizontal grid lines for a cleaner look
  )

#===============================================================================
# Control vs trt

xls_file  <- "excel_data/yield_predictions_data_with_nextgddp_yearly_baseline_deltas.xlsx"
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
filtered_data <- raw %>%
  filter(adaptation_type != "Late Planting")

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
      adaptation_type.y %in% c("Fertilizer", "High Fertilizer", "Medium Fertilizer") ~ "Fertilizer application",
      adaptation_type.y %in% c("Cultivar", "Cultivar - Long duration", "Cultivar - Short duration") ~ "Cultivar adjustments",
      adaptation_type.y %in% "Combined" ~ "Bundled practices",
      adaptation_type.y %in% c("Irrigation") ~ "Irrigation",
      adaptation_type.y %in% c("Tillage", "No-till", "Conservation tillage") ~ "Tillage",
      adaptation_type.y %in% c("Early Planting", "Early sowing", "Planting time") ~ "Planting time",
      TRUE ~ adaptation_type.y
    )
  ) %>%
  filter(!is.na(d_yield)) %>%
  filter(adapt_group %in% c("Bundled practices", "Cultivar adjustments", "Irrigation", 
                            "Tillage", "Planting time")) %>%
  mutate(adapt_group = factor(adapt_group, levels = c(
    "Bundled practices",
    "Cultivar adjustments",
    "Irrigation",
    "Tillage",
    "Planting time"
  )))

# Before
dat %>% group_by(adapt_group) %>% summarise(n = n())

# 5b. Remove outliers using IQR method
dat <- dat %>%
  group_by(adapt_group) %>%
  mutate(
    Q1 = quantile(d_yield, 0.25, na.rm = TRUE),
    Q3 = quantile(d_yield, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 3 * IQR,
    upper = Q3 + 3 * IQR
  ) %>%
  filter(d_yield >= lower & d_yield <= upper) %>%
  select(-Q1, -Q3, -IQR, -lower, -upper) %>%
  ungroup()

# After outlier removal
dat %>% group_by(adapt_group) %>% summarise(n = n())

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
# Count number of observations per adaptation group
obs_counts <- dat %>%
  group_by(adapt_group) %>%
  summarise(n = n(), .groups = "drop")

# Join the counts into results
results_with_counts <- results %>%
  left_join(obs_counts, by = "adapt_group")

results_with_counts <- results %>%
  left_join(obs_counts, by = "adapt_group")


# 8. Plot -------------------------------------------------------------------
ggplot(results_with_counts, aes(x = reorder(adapt_group, -mean), y = mean)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = paste0("n=", n)),
            vjust = -0.5, size = 8) +  # Adjust size of 'n=' labels
  labs(
    x = "Adaptation Strategy",
    y = "Δ Yield (%) with Adaptation Strategies"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 20, angle = 0, hjust = 0.5,face = "bold"),  # <-- Bigger x-labels
    axis.text.y = element_text(size = 20)
  )



# 9. Obs count ---------------------------------------------------------------
obs_counts <- dat %>%
  group_by(adapt_group) %>%
  summarise(n_obs = n()) %>%
  arrange(desc(n_obs))

obs_counts


#=============================================================================
# Protein convertion
# --- LOAD LIBRARIES ---
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(boot)
library(cowplot)
library(janitor)

# --- 1. Load data ---
xls_file <- "excel_data/yield_predictions_data_with_local_dT&dP_filled_update_final.xlsx"
raw_df   <- read_excel(xls_file, sheet = "Sheet1") |> clean_names()

# --- 2. Define legumes and period bins ---
legumes <- c("Bambara", "Bush Bean", "Chickpea", "Climbing Bean",
             "Common Bean", "Cowpea", "Faba Bean", "Green grams",
             "Groundnut", "Soybean")

period_levels <- c("NF (2020-2040)", "MC (2041-2070)", "LC (2071-2100)")

add_period <- function(df) {
  df |>
    mutate(
      future_mid_point = as.numeric(future_mid_point),
      decade = 10 * floor(future_mid_point / 10),
      period = case_when(
        decade <= 2040 ~ "NF (2020-2040)",
        decade <= 2070 ~ "MC (2041-2070)",
        TRUE           ~ "LC (2071-2100)"
      ),
      period = factor(period, levels = period_levels)
    )
}

# --- 3. Bootstrap function ---
boot_mean <- function(x, i) mean(x[i], na.rm = TRUE)

# --- 4. Protein factors ---
protein_factors <- tibble(
  crop = c("Common Bean", "Cowpea", "Groundnut", "Soybean",
           "Bambara", "Bush Bean", "Chickpea", "Climbing Bean", "Green grams", "Faba Bean"),
  digestible_protein_percent = c(17, 19, 22, 32, 16, 18, 19, 18, 21, 20)
)

# --- 5. Prepare yield data ---
dat_yield <- raw_df |>
  filter(crop %in% legumes) |>
  add_period() |>
  mutate(
    baseline_yield_t_ha  = suppressWarnings(as.numeric(baseline_yield_t_ha)),
    projected_yield_t_ha = suppressWarnings(as.numeric(projected_yield_t_ha)),
    yield_change = projected_yield_t_ha - baseline_yield_t_ha
  ) |>
  filter(!is.na(yield_change))

# --- 6. Bootstrap summary for yield ---
yield_summary <- dat_yield |>
  group_by(crop, period) |>
  summarise(
    n = n(),
    papers = n_distinct(ref_no),
    boot = list({
      if (n() >= 5) {
        bt <- boot(yield_change, boot_mean, R = 500)
        tibble(
          mean_yield_change = mean(bt$t0),
          lo95 = quantile(bt$t, 0.025, na.rm = TRUE),
          hi95 = quantile(bt$t, 0.975, na.rm = TRUE)
        )
      } else NULL
    }),
    .groups = "drop"
  ) |>
  unnest(boot) |>
  mutate(ci_range = hi95 - lo95)

# --- 7. Complete grid for yield ---
all_combos <- expand.grid(
  crop   = sort(unique(dat_yield$crop)),
  period = period_levels,
  stringsAsFactors = FALSE
)

plot_df_yield <- as_tibble(all_combos) |>
  mutate(period = factor(period, levels = period_levels)) |>
  left_join(yield_summary, by = c("crop", "period")) |>
  mutate(
    label = if_else(
      is.na(mean_yield_change),
      "No data",
      sprintf("%.2f t/ha\n(n=%d; p=%d)", mean_yield_change, n, papers)
    ),
    border_width = case_when(
      is.na(ci_range)  ~ 0.3,
      ci_range <= 0.05 ~ 0.3,
      ci_range <= 0.10 ~ 1,
      TRUE             ~ 2
    )
  ) |>
  filter(!crop %in% c("Green grams", "Climbing Bean", "Bush Bean", "Faba Bean")) |>
  mutate(crop = factor(crop))

# --- 8. Yield plot ---
p_yield <- ggplot(plot_df_yield, aes(x = period, y = crop)) +
  geom_tile(aes(fill = mean_yield_change, linewidth = border_width),
            color = "black", width = 0.9, height = 0.9) +
  geom_tile(
    data = subset(plot_df_yield, is.na(mean_yield_change)),
    fill = "grey90", color = "black", linewidth = 0.3, width = 0.9, height = 0.9
  ) +
  # In p_yield geom_text:
  geom_text(aes(label = label), size = 6, lineheight = 0.95) +
  scale_fill_gradient2(
    name = expression(Delta ~ "Yield (t/ha)"),
    low = "red", mid = "white", high = "darkgreen", midpoint = 0,
    na.value = "grey99"
  ) +
  scale_x_discrete(limits = period_levels) +
  guides(linewidth = "none",
         fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 0.5)) +
  labs(
    title = "Projected Change in Legume Yield (t/ha)",
    subtitle = "Fill = mean Δ yield; label shows n (obs) and p (papers).\nBorder thickness reflects uncertainty: thin ≤ 0.05 t/ha, medium 0.05–0.10, thick ≥ 0.10",
    x = NULL, y = NULL
  ) +
  # In p_yield theme:
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 16),
    plot.title   = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16)
  )


# --- 9. Prepare protein data ---
dat_protein <- raw_df |>
  filter(crop %in% legumes) |>
  add_period() |>
  mutate(
    baseline_yield_t_ha  = suppressWarnings(as.numeric(baseline_yield_t_ha)),
    projected_yield_t_ha = suppressWarnings(as.numeric(projected_yield_t_ha))
  ) |>
  left_join(protein_factors, by = "crop") |>
  mutate(
    protein_baseline  = baseline_yield_t_ha  * (digestible_protein_percent / 100),
    protein_projected = projected_yield_t_ha * (digestible_protein_percent / 100),
    protein_change    = protein_projected - protein_baseline
  ) |>
  filter(!is.na(protein_change))

# --- 10. Bootstrap summary for protein ---
protein_summary <- dat_protein |>
  group_by(crop, period) |>
  summarise(
    n = n(),
    papers = n_distinct(ref_no),
    boot = list({
      if (n() >= 5) {
        bt <- boot(protein_change, boot_mean, R = 500)
        tibble(
          mean_protein_yield = mean(bt$t0),
          lo95 = quantile(bt$t, 0.025, na.rm = TRUE),
          hi95 = quantile(bt$t, 0.975, na.rm = TRUE)
        )
      } else NULL
    }),
    .groups = "drop"
  ) |>
  unnest(boot) |>
  mutate(ci_range = hi95 - lo95)

# --- 11. Complete grid for protein ---
plot_df_protein <- as_tibble(all_combos) |>
  mutate(period = factor(period, levels = period_levels)) |>
  left_join(protein_summary, by = c("crop", "period")) |>
  mutate(
    label = if_else(
      is.na(mean_protein_yield),
      "No data",
      sprintf("%.2f\n(n=%d; p=%d)", mean_protein_yield, n, papers)
    ),
    border_width = case_when(
      is.na(ci_range)  ~ 0.3,
      ci_range <= 0.05 ~ 0.3,
      ci_range <= 0.10 ~ 1,
      TRUE             ~ 2
    )
  ) |>
  filter(!crop %in% c("Green grams", "Climbing Bean", "Bush Bean", "Faba Bean")) |>
  mutate(crop = factor(crop))

# --- 12. Protein plot ---
p_protein <- ggplot(plot_df_protein, aes(x = period, y = crop)) +
  geom_tile(aes(fill = mean_protein_yield, linewidth = border_width),
            color = "black", width = 0.9, height = 0.9) +
  geom_tile(
    data = subset(plot_df_protein, is.na(mean_protein_yield)),
    fill = "grey90", color = "black", linewidth = 0.3, width = 0.9, height = 0.9
  ) +
  geom_text(aes(label = label), size = 6, lineheight = 0.95) +
  scale_fill_gradient2(
    name = expression(Delta ~ "Protein yield (t/ha)"),
    low = "red", mid = "white", high = "darkgreen", midpoint = 0,
    na.value = "grey99"
  ) +
  scale_x_discrete(limits = period_levels) +
  guides(linewidth = "none",
         fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 0.5)) +
  labs(
    title = "Projected Change in Digestible Protein Yield (t/ha)",
    subtitle = "Fill = mean Δ protein yield; label shows n (obs) and p (papers).\nBorder thickness reflects uncertainty: thin ≤ 0.05 t/ha, medium 0.05–0.10, thick ≥ 0.10",
    x = NULL, y = NULL
  ) +
  # In p_protein theme:
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 16),
    plot.title   = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16)
  )



# --- 13. Combine both plots ---
plot_grid(
  p_yield,
  p_protein,
  ncol = 1,
  labels = c("A", "B"),
  label_size = 18
)
