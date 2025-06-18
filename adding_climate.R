#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––
#  FILL "Local delta T from 2005"  using WorldClim 2.1 CMIP-6
#  Namita — June 2025
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––

library(readxl)
library(writexl)
library(dplyr)
library(terra)

# 1 ── data + id column
df <- read_excel("yield_predictions_data.xlsx", sheet = "Dataset") |>
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
base_r <- rast("worldclim_data/wc2.1_2.5m_bio_1.tif")

# 4 ── loop over ssp × window
for (cmb in split(df_todo, list(df_todo$ssp, df_todo$win), drop = TRUE)) {
  
  ssp_id <- cmb$ssp[1];  win_id <- cmb$win[1]
  message("▶  ", ssp_id, "  ", win_id)
  
  pat <- paste0("wc2.1_2.5m_bioc_.*_", ssp_id, "_", win_id, "\\.tif$")
  f_files <- list.files("worldclim_data", pattern = pat, full.names = TRUE)
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
write_xlsx(df, "yield_predictions_data_with_local_dT_filled.xlsx")


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
