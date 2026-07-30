#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––
#  FILL "Local delta T from 2005"  using WorldClim 2.1 CMIP-6
#  Namita — June 2025
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––

library(readxl)
library(writexl)
library(dplyr)
library(terra)

# 1 ── data + id column
df <- read_excel("excel_data/yield_predictions_data_with_local_dT&dP_filled.xlsx", sheet = "Sheet1") |>
  mutate(row_index = row_number())

# 2 ── rows that need ΔT and keys
df_todo <- df |>
  filter(is.na(`Local delta T from 2005`)) |>
  mutate(
    # ------------------------------------------------------------------
    # pick an ssp ID for both CMIP6 (SSPxxx) and CMIP3 (A1B, B2, etc.)
    ssp = case_when(
      ##— CMIP6 inputs (as before) —
      `Climate scenario` %in% c("RCP2.6", "SSP126")                  ~ "ssp126",
      `Climate scenario` %in% c("RCP4.5", "SSP245")                  ~ "ssp245",
      `Climate scenario` %in% c("RCP6.0", "SSP370")                  ~ "ssp370",
      `Climate scenario` %in% c("RCP8.5", "SSP585")                  ~ "ssp585",
      ##— Approximate CMIP3 SRES → SSP mapping —
      `Climate scenario` %in% c("B1", "A1B1")                        ~ "ssp126",
      `Climate scenario` == "B2"                                     ~ "ssp245",
      `Climate scenario` == "A1B"                                    ~ "ssp370",
      `Climate scenario` == "A2"                                     ~ "ssp585",
      TRUE                                                            ~ NA_character_
    ),
    win = case_when(
      between(`Future_Mid-point`, 2021, 2040) ~ "2021-2040",
      between(`Future_Mid-point`, 2041, 2060) ~ "2041-2060",
      between(`Future_Mid-point`, 2061, 2080) ~ "2061-2080",
      between(`Future_Mid-point`, 2081, 2100) ~ "2081-2100"
    )
  ) |>
  filter(!is.na(ssp), !is.na(win))

df_todo$delta_T <- NA_real_

# 3 ── baseline BIO1  (already °C !)
base_r <- rast("C:/Users/JNamita/OneDrive - Wageningen University & Research/PhD- Namita/Chapter 1- Yield change synthesis/worldclim_data/wc2.1_2.5m_bio_1.tif")

# 4 ── loop over ssp × window
for (cmb in split(df_todo, list(df_todo$ssp, df_todo$win), drop = TRUE)) {
  
  ssp_id <- cmb$ssp[1];  win_id <- cmb$win[1]
  message("▶  ", ssp_id, "  ", win_id)
  
  pat <- paste0("wc2.1_2.5m_bioc_.*_", ssp_id, "_", win_id, "\\.tif$")
  f_files <- list.files("C:/Users/JNamita/OneDrive - Wageningen University & Research/PhD- Namita/Chapter 1- Yield change synthesis/worldclim_data/", 
                        pattern = pat, full.names = TRUE)
  if (!length(f_files)) { message("   ⚠  none"); next }
  
  pts <- vect(cmb, geom = c("longitude", "latitude"), crs = "EPSG:4326")
  
  fut_mat <- vapply(f_files, \(f)
                    extract(rast(f, lyrs = 1), pts)[,2],   # °C already
                    numeric(nrow(cmb)))
  
  fut_mean <- if (is.null(dim(fut_mat))) fut_mat else rowMeans(fut_mat, na.rm = TRUE)
  base_val <- extract(base_r, pts)[,2]
  
  df_todo$delta_T[match(cmb$row_index, df_todo$row_index)] <-
    fut_mean - base_val                     # °C difference
}

# 5 ── overwrite only where new ΔT exists
idx    <- match(df$row_index, df_todo$row_index)
new_DT <- df_todo$delta_T[idx]

df$`Local delta T from 2005` <- ifelse(!is.na(new_DT),
                                       new_DT,
                                       df$`Local delta T from 2005`)

# 6 ── save
df <- select(df, -row_index)
write_xlsx(df, "excel_data/yield_predictions_data_with_local_dT&dP_filled_updates.xlsx")


#=========================================================
  # ── 0.  Coordinates for Chefa, Ethiopia ───────────────────────────────
lon <- 39.8167
lat <- 10.72
pt  <- vect(data.frame(lon = lon, lat = lat),
            geom = c("lon", "lat"), crs = "EPSG:4326")

# ── 1.  Baseline BIO1 (WorldClim v2.1, 1970-2000) ─────────────────────
base_r   <- rast("worldclim_data/wc2.1_2.5m_bio_1.tif")   # units already °C
base_val <- extract(base_r, pt)[, 2]                      # °C
cat("Baseline annual mean T  (1970-2000):", base_val, "°C\n\n")

# ── 2.  Helper to get future temps & ΔT for any SSP × window ─────────
get_delta <- function(ssp = "ssp585", window = "2081-2100") {
  pat   <- sprintf("wc2.1_2.5m_bioc_.*_%s_%s\\.tif$", ssp, window)
  files <- list.files("worldclim_data", pattern = pat, full.names = TRUE)
  
  if (length(files) == 0) {
    cat("⚠️  NO rasters for", ssp, window, "\n")
    return(invisible(NULL))
  }
  
  fut_vec <- vapply(files, \(f) extract(rast(f, lyrs = 1), pt)[, 2],
                    numeric(1))
  fut_mean <- mean(fut_vec, na.rm = TRUE)
  
  cat("SSP:",  ssp, "  window:", window,
      "\n  •  n GCMs      :", length(fut_vec),
      "\n  •  future mean :", round(fut_mean, 2), "°C",
      "\n  •  ΔT (fut-base):", round(fut_mean - base_val, 2), "°C\n\n")
}

# ── 3.  Examples ──────────────────────────────────────────────────────
get_delta("ssp585", "2081-2100")   # late-century high forcing
get_delta("ssp245", "2041-2060")   # mid-century moderate forcing
get_delta("ssp126", "2021-2040")   # near-term low forcing


#==============================================================================
# Adding precipitation

library(readxl)
library(writexl)
library(dplyr)
library(terra)
library(stringr)

# 1 ── Load Excel and add row ID
df <- read_excel("excel_data/yield_predictions_data_with_local_dT&dP_filled_updates.xlsx", 
                 sheet = "Sheet1") %>%
  mutate(row_index = row_number())

# 2 ── Prepare data for climate extraction
df_todo <- df %>%
  mutate(
    ssp = case_when(
      `Climate scenario` %in% c("RCP2.6", "SSP126") ~ "ssp126",
      `Climate scenario` %in% c("RCP4.5", "SSP245") ~ "ssp245",
      `Climate scenario` %in% c("RCP6.0", "SSP370") ~ "ssp370",
      `Climate scenario` %in% c("RCP8.5", "SSP585") ~ "ssp585",
      `Climate scenario` %in% c("B1", "A1B1")        ~ "ssp126",
      `Climate scenario` == "B2"                    ~ "ssp245",
      `Climate scenario` == "A1B"                   ~ "ssp370",
      `Climate scenario` == "A2"                    ~ "ssp585",
      TRUE ~ NA_character_
    ),
    win = case_when(
      between(`Future_Mid-point`, 2021, 2040) ~ "2021-2040",
      between(`Future_Mid-point`, 2041, 2060) ~ "2041-2060",
      between(`Future_Mid-point`, 2061, 2080) ~ "2061-2080",
      between(`Future_Mid-point`, 2081, 2100) ~ "2081-2100"
    )
  ) %>%
  filter(!is.na(ssp), !is.na(win))

# Create empty holders
df_todo$delta_P      <- NA_real_   # absolute change (mm)
df_todo$delta_P_pct  <- NA_real_   # relative change (%)
df_todo$baseline_precip <- NA_real_

# 3 ── Compute historical baseline BIO12 from 12 monthly rasters
prec_files <- list.files("C:/Users/JNamita/OneDrive - Wageningen University & Research/PhD- Namita/Chapter 1- Yield change synthesis/worldclim_data/prec_historical", 
                         pattern = "^wc2.1_2.5m_prec_\\d+\\.tif$", full.names = TRUE)
monthly_stack <- rast(prec_files)
base_r <- sum(monthly_stack)  # This is BIO12 (annual precipitation baseline)

# 4 ── Loop over SSP × time window and calculate ΔP (mm and %)
for (cmb in split(df_todo, list(df_todo$ssp, df_todo$win), drop = TRUE)) {
  
  ssp_id <- cmb$ssp[1]
  win_id <- cmb$win[1]
  message("▶  Processing: ", ssp_id, "  ", win_id)
  
  # Match files with 'prec' + ssp + window structure
  pat <- paste0("prec_.*", ssp_id, "_", win_id, "\\.tif$")
  f_files <- list.files(
    "C:/Users/JNamita/OneDrive - Wageningen University & Research/PhD- Namita/Chapter 1- Yield change synthesis/worldclim_data",
    pattern = pat,
    full.names = TRUE
  )
  if (!length(f_files)) {
    message("   ⚠  No future precip files found.")
    next
  }
  
  # Get study coordinates
  pts <- terra::vect(cmb, geom = c("longitude", "latitude"), crs = "EPSG:4326")
  
  # Extract and sum all layers (monthly precip → annual)
  fut_mat <- vapply(f_files, \(f) {
    values <- extract(rast(f), pts)[, -1, drop = FALSE]
    rowSums(values, na.rm = TRUE)
  }, numeric(nrow(cmb)))
  
  fut_mean <- if (is.null(dim(fut_mat))) fut_mat else rowMeans(fut_mat, na.rm = TRUE)
  
  # Historical baseline
  base_val <- extract(base_r, pts)[,2]
  
  # ΔP (absolute mm) and ΔP% (relative change)
  delta_abs <- fut_mean - base_val
  delta_pct <- (fut_mean - base_val) / base_val * 100
  
  # Write results back
  df_todo$delta_P[match(cmb$row_index, df_todo$row_index)]        <- delta_abs
  df_todo$delta_P_pct[match(cmb$row_index, df_todo$row_index)]    <- delta_pct
  df_todo$baseline_precip[match(cmb$row_index, df_todo$row_index)] <- base_val
}

# 5 ── Merge results into main df
df <- df %>%
  left_join(
    df_todo %>% select(row_index, delta_P, delta_P_pct, baseline_precip),
    by = "row_index"
  ) %>%
  mutate(
    `Annual Precipitation change  from 2005 (mm)_cal` =
      coalesce(delta_P, `Annual Precipitation change  from 2005 (mm)_cal`),
    `Annual Precipitation change  from 2005 (%)_cal` = delta_P_pct,
    `Baseline precipitation (mm)_cal` = baseline_precip
  ) %>%
  select(-delta_P, -delta_P_pct, -baseline_precip)

# 6 ── Save final file
df <- select(df, -row_index)
write_xlsx(df, "excel_data/yield_predictions_data_with_local_dT&dP_filled_update_final.xlsx")


#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# FILL "Local delta T from BASELINE YEAR" using NextGDDP
# Namita — Oct 2025
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# STEP 1: CREATE 20-YEAR ENSEMBLE MEANS (KEPT FOR REFERENCE/COMPLETENESS)
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# --- Define directories ---
clim_dir <- "C:/Users/JNamita/OneDrive - Wageningen University & Research/PhD- Namita/Chapter 1- Yield change synthesis/nextgddp_data"
hist_out_dir_20yr <- file.path(clim_dir, "hist", "ENSEMBLE_means_20yr") # Assuming this is where future files are referenced
fut_dir <- file.path(clim_dir, "future")

# --- NEW CORRECTED DIRECTORY: Location of the yearly ensemble rasters ---
# Based on your image path: .../nextgddp_data/hist/Year_means
BASE_YEAR_ENSEMBLE_DIR <- file.path(clim_dir, "hist", "Year_means")


# --- Load coordinates and baseline/future info ---
df <- read_excel("excel_data/yield_predictions_data_with_nextgddp_yearly_baseline_deltas.xlsx") %>%
  mutate(row_index = row_number())

pts <- vect(df, geom = c("longitude", "latitude"), crs = "EPSG:4326")

# --- Prepare dataframe with SSPs and time windows ---
df_todo <- df %>%
  mutate(
    ssp = case_when( # ... ssp assignments ...
      `Climate scenario` %in% c("RCP2.6", "SSP126") ~ "ssp126",
      `Climate scenario` %in% c("RCP4.5", "SSP245") ~ "ssp245",
      `Climate scenario` %in% c("RCP6.0", "SSP370") ~ "ssp370",
      `Climate scenario` %in% c("RCP8.5", "SSP585") ~ "ssp585",
      `Climate scenario` %in% c("B1", "A1B1") ~ "ssp126",
      `Climate scenario` == "B2" ~ "ssp245",
      `Climate scenario` == "A1B" ~ "ssp370",
      `Climate scenario` == "A2" ~ "ssp585",
      TRUE ~ NA_character_
    ),
    base_year = floor(`Baseline_Mid-point`), 
    fut_win = case_when( # ... fut_win assignments (unchanged) ...
      between(`Future_Mid-point`, 2021, 2040) ~ "2021-2040",
      between(`Future_Mid-point`, 2041, 2060) ~ "2041-2060",
      between(`Future_Mid-point`, 2061, 2080) ~ "2061-2080",
      between(`Future_Mid-point`, 2081, 2100) ~ "2081-2100",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(ssp), !is.na(fut_win), base_year >= 1975, base_year <= 2014) 

# Initialize outputs (remaining script logic is unchanged)
df_todo$temp_change_nextgddp <- NA_real_
df_todo$precip_change_nextgddp <- NA_real_

# --- Loop through unique SSP × Baseline_Year × Future combinations ---
for (cmb in split(df_todo, list(df_todo$ssp, df_todo$base_year, df_todo$fut_win), drop = TRUE)) {
  
  ssp_id <- as.character(cmb$ssp[1])
  base_yr <- as.integer(cmb$base_year[1]) 
  fut_win <- as.character(cmb$fut_win[1]) # This is "2021-2040", "2041-2060", etc.
  
  message("\n🔍 Processing: ", ssp_id, " | Base Year: ", base_yr, " | Fut: ", fut_win)
  
  # ---- Load HISTORICAL BASELINE YEAR ensemble means (CORRECTED DIR, SAME PATTERN) ----
  base_tavg_file <- list.files(
    BASE_YEAR_ENSEMBLE_DIR,
    pattern = paste0("ensemble_mean_TAVG_", base_yr, "\\.tif$"),
    full.names = TRUE
  )
  base_ptot_file <- list.files(
    BASE_YEAR_ENSEMBLE_DIR,
    pattern = paste0("ensemble_sum_PTOT_", base_yr, "\\.tif$"),
    full.names = TRUE
  )
  
  # ---- Load FUTURE 20-year ensemble means (CORRECTED PATTERN) ----
  # Pattern now expects: sspXXX_ENSEMBLEmean_20XX-20XX_TAVG...
  # We use fut_win (hyphenated) directly in the search pattern.
  
  fut_tavg_file <- list.files(
    fut_dir,
    pattern = paste0(ssp_id, "_ENSEMBLEmean_", fut_win, ".*TAVG-mean_mean"),
    full.names = TRUE
  )
  fut_ptot_file <- list.files(
    fut_dir,
    pattern = paste0(ssp_id, "_ENSEMBLEmean_", fut_win, ".*PTOT-sum_mean"),
    full.names = TRUE
  )
  
  message("  Found baseline TAVG: ", length(base_tavg_file),
          " | PTOT: ", length(base_ptot_file),
          " | future TAVG: ", length(fut_tavg_file),
          " | PTOT: ", length(fut_ptot_file))
  
  if (!length(base_tavg_file) | !length(base_ptot_file) | !length(fut_tavg_file) | !length(fut_ptot_file)) {
    message("⚠️ Missing files for ", ssp_id, " | Base Year ", base_yr, " → Fut ", fut_win)
    next
  }
  
  # ---- Prepare point subset ----
  pts_cmb <- pts[as.integer(cmb$row_index), , drop = FALSE]
  
  # ---- Load rasters ----
  base_tavg <- rast(base_tavg_file)
  base_ptot <- rast(base_ptot_file)
  fut_tavg <- rast(fut_tavg_file)
  fut_ptot <- rast(fut_ptot_file)
  
  # ---- Extract values ----
  base_tavg_vals <- terra::extract(base_tavg, pts_cmb)[, -1]
  base_ptot_vals <- terra::extract(base_ptot, pts_cmb)[, -1]
  fut_tavg_vals  <- terra::extract(fut_tavg,  pts_cmb)[, -1]
  fut_ptot_vals  <- terra::extract(fut_ptot,  pts_cmb)[, -1]
  
  # ---- Compute ensemble means ----
  base_tavg_mean <- base_tavg_vals 
  base_ptot_mean <- base_ptot_vals
  
  # Future is still a 20-year ensemble stack, so rowMeans is kept
  if (is.null(dim(fut_tavg_vals))) fut_tavg_mean <- fut_tavg_vals
  else fut_tavg_mean <- rowMeans(fut_tavg_vals, na.rm = TRUE)
  
  if (is.null(dim(fut_ptot_vals))) fut_ptot_mean <- fut_ptot_vals
  else fut_ptot_mean <- rowMeans(fut_ptot_vals, na.rm = TRUE)
  
  # ---- Compute ΔT (°C) and ΔP (%) ----
  temp_delta <- fut_tavg_mean - base_tavg_mean
  precip_delta <- ifelse(
    base_ptot_mean == 0, 0,
    ((fut_ptot_mean - base_ptot_mean) / base_ptot_mean) * 100
  )
  
  # ---- Store results ----
  idx <- df_todo$row_index %in% cmb$row_index
  df_todo$temp_change_nextgddp[idx] <- temp_delta
  df_todo$precip_change_nextgddp[idx] <- precip_delta
}

#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# STEP 3: SAVE RESULTS
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––

df_final <- df %>%
  left_join(
    df_todo %>%
      select(row_index, temp_change_nextgddp, precip_change_nextgddp),
    by = "row_index"
  )

write_xlsx(df_final, "excel_data/yield_predictions_data_with_climate_all crops.xlsx")

message("\n✅ Completed successfully: ΔT and ΔP using BASELINE YEAR ensembles and 20-year future ensembles saved.")
