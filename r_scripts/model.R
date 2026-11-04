# ============================================================
# GAMM Analysis: Legume Yield Response to Climate Change
# ============================================================

# --- 1. Load Packages ---
library(tidyverse)
library(readxl)
library(mgcv)
library(gamm4)
library(performance)
library(broom)
library(broom.mixed)
library(writexl)
library(janitor)

# --- 2. Data Import ---
raw <- read_excel("excel_data/yield_predictions_data_with_nextgddp_yearly_baseline_deltas.xlsx", 
                  sheet = "Sheet1") %>%
  clean_names()

# --- 3. Define Crop Lists ---
legume_crops <- c("Bambara", "Bush Bean", "Chickpea", "Climbing Bean", "Common Bean", 
                  "Cowpea", "Faba Bean", "Green grams", "Groundnut", "Soybean")

# --- 4. Filter and Recode ---
df_legumes <- raw %>%
  filter(crop %in% legume_crops) %>%
  mutate(
    dp         = as.numeric(climate_impacts_percent),
    dt         = as.numeric(temp_change_nextgddp),
    precip     = as.numeric(precip_change_nextgddp),
    adaptation = factor(if_else(adaptation_type == "No", "No-adaptation", "Adaptation"),
                        levels = c("No-adaptation", "Adaptation")),
    co2_group = factor(case_when(
      co2 == "Yes"                        ~ "With CO2 fertilization",
      co2 %in% c("No", "Unspecified")     ~ "Without CO2 fertilization",
      is.na(co2)                          ~ "Without CO2 fertilization",
      TRUE                                ~ "Without CO2 fertilization"
    ),
    levels = c("Without CO2 fertilization", "With CO2 fertilization")),
    study_id   = factor(ref_no),
    crop       = factor(crop),
    country    = factor(country)
  ) %>%
  filter(complete.cases(dp, dt, precip, study_id, crop, country, adaptation, co2_group)) %>%
  droplevels()

# --- 5. Check Sample Sizes ---
cat("N observations:", nrow(df_legumes), "\n")
cat("N unique studies:", length(unique(df_legumes$ref_no)), "\n")
cat("N unique countries:", length(unique(df_legumes$country)), "\n")

# --- 6. Test for Linearity (GAM) ---
gam_test <- gam(dp ~ s(precip) + s(dt) + adaptation + co2_group, 
                data = df_legumes, method = "REML")
summary(gam_test)
par(mfrow = c(1, 2))
plot(gam_test, residuals = TRUE, shade = TRUE)

# --- 7. Fit Final GAMM ---
# Temperature treated as parametric (linear) based on GAM test
# Random intercepts for study and country
gamm4_mod_final <- gamm4(
  dp ~ dt + s(precip) + adaptation + co2_group,
  random = ~ (1 | study_id) + (1 | country),
  data = df_legumes
)

# --- 8. Model Summaries ---
summary(gamm4_mod_final$gam)
summary(gamm4_mod_final$mer)

# --- 9. Model Evaluation ---
# Null model for AIC comparison
null_mod <- gamm4(dp ~ 1,
                  random = ~ (1 | study_id) + (1 | country),
                  data = df_legumes)

cat("AIC null model:", AIC(null_mod$mer), "\n")
cat("AIC full model:", AIC(gamm4_mod_final$mer), "\n")
cat("ΔAIC:", AIC(null_mod$mer) - AIC(gamm4_mod_final$mer), "\n")

# R-squared
r2(gamm4_mod_final$mer)

# --- 10. Export Model Results ---
# All values below are taken from the fitted model (no hard-coded numbers)
gam_param  <- summary(gamm4_mod_final$gam)$p.table
gam_smooth <- summary(gamm4_mod_final$gam)$s.table
r2_vals    <- r2(gamm4_mod_final$mer)
fmt_p      <- function(p) ifelse(p < 0.001, "<0.001", as.character(round(p, 3)))

model_metrics <- data.frame(
  Metric = c("AIC (null model)", "AIC (full model)", "ΔAIC",
             "Marginal R²", "Conditional R²", "Adjusted R²",
             "REML criterion", "N observations", "N studies", "N countries"),
  Value = c(round(AIC(null_mod$mer), 2),
            round(AIC(gamm4_mod_final$mer), 2),
            round(AIC(null_mod$mer) - AIC(gamm4_mod_final$mer), 2),
            round(r2_vals$R2_marginal, 3),
            round(r2_vals$R2_conditional, 3),
            round(summary(gamm4_mod_final$gam)$r.sq, 3),
            round(summary(gamm4_mod_final$mer)$AICtab["REML"], 2),
            nrow(df_legumes),
            length(unique(df_legumes$ref_no)),
            length(unique(df_legumes$country)))
)

fixed_effects <- data.frame(
  Term = c("Intercept", "ΔT (°C)", "Adaptation", "CO₂ fertilization", "s(ΔP) smooth"),
  Estimate = c(round(gam_param[, "Estimate"], 3), NA),
  SE = c(round(gam_param[, "Std. Error"], 3), NA),
  t_value = c(round(gam_param[, "t value"], 3), NA),
  edf = c(rep(NA, nrow(gam_param)), round(gam_smooth[1, "edf"], 3)),
  F_value = c(rep(NA, nrow(gam_param)), round(gam_smooth[1, "F"], 3)),
  p_value = fmt_p(c(gam_param[, "Pr(>|t|)"], gam_smooth[1, "p-value"]))
)

# Variance components (the s(precip) smooth's penalty term "Xr" is not a random effect)
vc <- as.data.frame(VarCorr(gamm4_mod_final$mer)) %>%
  filter(grp != "Xr") %>%
  arrange(desc(vcov))
random_effects <- data.frame(
  Group = c(vc$grp, "Total"),
  Variance = round(c(vc$vcov, sum(vc$vcov)), 1),
  Std_Dev = c(round(vc$sdcor, 2), NA),
  Pct_Variance = paste0(round(100 * c(vc$vcov, sum(vc$vcov)) / sum(vc$vcov)), "%")
)

writexl::write_xlsx(
  list(
    Model_Fit = model_metrics,
    Fixed_Effects = fixed_effects,
    Random_Effects = random_effects
  ),
  path = "excel_data/gamm_supplementary_table.xlsx"
)

message("✅ GAMM analysis complete. Results saved.")

# --- Diagnostic Plots for Final GAMM ---
par(mfrow = c(1, 2))

# Residuals vs Fitted
plot(fitted(gamm4_mod_final$mer), 
     residuals(gamm4_mod_final$mer),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)

# Q-Q Plot
qqnorm(residuals(gamm4_mod_final$mer), main = "Q-Q Residuals")
qqline(residuals(gamm4_mod_final$mer), lty = 2)

# Save as PNG for supplementary
png("Yield-projections-legumes_files/diagnostic_plots_GAMM.png", width = 1200, height = 600, res = 150)
par(mfrow = c(1, 2))
plot(fitted(gamm4_mod_final$mer), 
     residuals(gamm4_mod_final$mer),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)
qqnorm(residuals(gamm4_mod_final$mer), main = "Q-Q Residuals")
qqline(residuals(gamm4_mod_final$mer), lty = 2)
dev.off()

# OLS model for comparison
main_lm <- lm(dp ~ dt + precip + co2_group + adaptation, data = df_legumes)

# Save OLS diagnostic plots
png("Yield-projections-legumes_files/diagnostic_plots_OLS.png", width = 1200, height = 600, res = 150)
par(mfrow = c(1, 2))
plot(main_lm, which = 1, main = "Residuals vs Fitted (OLS)")
plot(main_lm, which = 2, main = "Q-Q Residuals (OLS)")
dev.off()

# After fitting your GAMM model, extract residuals and test normality
# Shapiro-Wilk test on GAMM residuals
gamm_resid <- residuals(gamm4_mod_final$mer)
shapiro.test(gamm_resid)


# --- Sensitivity analysis: CO2-inclusive simulations only ---

df_co2_only <- df_legumes %>%
  filter(co2_group == "With CO2 fertilization")

cat("N observations (CO2 only):", nrow(df_co2_only), "\n")
cat("N unique studies (CO2 only):", length(unique(df_co2_only$ref_no)), "\n")

# Fit GAMM on CO2-inclusive data only (drop co2_group term)
gamm4_mod_co2only <- gamm4(
  dp ~ dt + s(precip) + adaptation,
  random = ~ (1 | study_id) + (1 | country),
  data = df_co2_only
)

summary(gamm4_mod_co2only$gam)
summary(gamm4_mod_co2only$mer)

# Compare AIC with full model
cat("AIC full model:", AIC(gamm4_mod_final$mer), "\n")
cat("AIC CO2-only model:", AIC(gamm4_mod_co2only$mer), "\n")

# Diagnostic plots
png("Yield-projections-legumes_files/diagnostic_plots_GAMM_co2only.png", 
    width = 1200, height = 600, res = 150)
par(mfrow = c(1, 2))
plot(fitted(gamm4_mod_co2only$mer), 
     residuals(gamm4_mod_co2only$mer),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted (CO2-only)")
abline(h = 0, col = "red", lty = 2)
qqnorm(residuals(gamm4_mod_co2only$mer), main = "Q-Q Residuals (CO2-only)")
qqline(residuals(gamm4_mod_co2only$mer), lty = 2)
dev.off()


