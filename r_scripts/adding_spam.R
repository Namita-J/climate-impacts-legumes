##--------Adding MapSPAM
library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(geosphere)     # distHaversine
library(stringr)

# ---- File paths (edit if needed) ----
xls_path  <- "excel_data/yield_predictions_data_with_local_dT&dP_filled.xlsx"
xls_sheet <- "Sheet1"
spam_csv  <- "excel_data/spam2020V1r2_SSA_Y_TL.csv"   # SPAM yield file (kg/ha)

# ---- 1. Load your data ----
dat <- read_excel(xls_path, sheet = xls_sheet) %>%
  clean_names() %>%
  mutate(row_id = row_number())

# Standardize coordinates
lat_col <- intersect(c("latitude","lat","latitud"), names(dat))[1]
lon_col <- intersect(c("longitude","lon","long","longitud"), names(dat))[1]
dat <- dat %>% rename(latitude = !!lat_col, longitude = !!lon_col)
dat <- dat %>% mutate(
  latitude  = suppressWarnings(as.numeric(latitude)),
  longitude = suppressWarnings(as.numeric(longitude))
)

# Find and check baseline column
baseline_col <- grep("baseline.*yield.*t.*ha", names(dat), ignore.case = TRUE, value = TRUE)[1]
if (is.na(baseline_col)) stop("Could not find your 'Baseline yield (t/ha)' column.")
# Find the column name
baseline_col <- grep("baseline.*yield.*t.*ha", names(dat), ignore.case = TRUE, value = TRUE)[1]
if (is.na(baseline_col)) stop("Could not find your 'Baseline yield (t/ha)' column.")

# Coerce to numeric (this converts "NA" strings and blanks to NA)
dat <- dat %>%
  mutate(
    baseline_yield = suppressWarnings(as.numeric(.data[[baseline_col]])),
    needs_fill = is.na(baseline_yield)
  )



# ---- 2. Define crops of interest ----
target_crops <- c("Cowpea", "Groundnut", "Green Grams", "Common Bean", "Chickpea", "Soybean")

# Map cleaned names to SPAM codes
crop_to_spam <- c(
  "cowpea"       = "cowp_l",
  "groundnut"    = "grou_l",
  "green grams"  = "opul_l",   # mung bean/green gram
  "common bean"  = "bean_l",
  "chickpea"     = "chic_l",
  "soybean"      = "soyb_l"
)

# Normalize function
norm <- function(x) x %>%
  tolower() %>%
  str_replace_all("[^a-z ]", "") %>%
  str_squish()

# Clean and filter data
dat <- dat %>%
  mutate(
    crop_clean = norm(crop),
    spam_crop_code = crop_to_spam[crop_clean]
  ) %>%
  filter(crop_clean %in% names(crop_to_spam))  # Keep only target crops

# ---- 3. Load SPAM and filter to the 6 crops ----
spam_raw <- read_csv(spam_csv, show_col_types = FALSE)
wanted_spam_cols <- unique(crop_to_spam)

spam_long <- spam_raw %>%
  select(x, y, unit, all_of(wanted_spam_cols)) %>%
  pivot_longer(cols = all_of(wanted_spam_cols),
               names_to = "spam_crop_code",
               values_to = "kg_ha") %>%
  mutate(
    longitude = suppressWarnings(as.numeric(x)),
    latitude  = suppressWarnings(as.numeric(y)),
    t_ha      = ifelse(tolower(unit) %in% c("kg/ha","kg ha","kg_per_ha"),
                       as.numeric(kg_ha)/1000, NA_real_)
  ) %>%
  filter(!is.na(t_ha)) %>%
  select(longitude, latitude, spam_crop_code, t_ha)

# ---- 4. Nearest-neighbour matching ----
get_nearest_yield <- function(lat, lon, code) {
  if (length(code) == 0 || is.na(code) || code == "" ||
      is.na(lat) || is.na(lon)) return(NA_real_)
  
  # Filter SPAM for that crop and drop zero-yield cells
  cand <- filter(spam_long, spam_crop_code == code, t_ha > 0)
  if (nrow(cand) == 0) return(NA_real_)  # no usable values at all
  
  # Compute Haversine distances
  dists <- geosphere::distHaversine(cbind(lon, lat), cbind(cand$longitude, cand$latitude))
  return(cand$t_ha[which.min(dists)])
}


# ---- 5. Compute fills ----
to_fill <- dat %>%
  filter(needs_fill, !is.na(spam_crop_code), !is.na(latitude), !is.na(longitude)) %>%
  select(row_id, latitude, longitude, spam_crop_code) %>%
  rowwise() %>%
  mutate(Baseline_yield_t_ha_SPAM = get_nearest_yield(latitude, longitude, spam_crop_code)) %>%
  ungroup() %>%
  select(row_id, Baseline_yield_t_ha_SPAM)

# ---- 6. Merge back and save ----
out <- dat %>%
  left_join(to_fill, by = "row_id") %>%
  rename(`Baseline yield (t/ha)_SPAM` = Baseline_yield_t_ha_SPAM)

# Save output
write_csv(out, "excel_data/yield_predictions_with_SPAM_baseline.csv")

# ---- 7. Report summary ----
cat("Final Summary:\n")
cat("Rows processed (only 6 crops): ", nrow(dat), "\n")
cat("Rows needing fill: ", sum(dat$needs_fill), "\n")
cat("Rows filled with SPAM: ", sum(!is.na(out$`Baseline yield (t/ha)_SPAM`)), "\n")
