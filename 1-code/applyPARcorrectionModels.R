# SCRIPT: APPLY CALIBRATION AND ANALYZE PAR SENSOR DATA --------------------
# Authors: M. Larade-Lounis, L. Lescroart, M. Mc Cormick, R. Perez, D. Cornet (CIRAD, June 2025)

# ==== LOAD PACKAGES =======================================================
packs <- c("lubridate", "tidyverse", "ggpmisc", "patchwork", "suncalc", 
           "yardstick")
InstIfNec <- function(pack) {
  if (!requireNamespace(pack, quietly = TRUE)) install.packages(pack)
  require(pack, character.only = TRUE)
}
invisible(lapply(packs, InstIfNec))


# ==== READ DATA ===========================================================
files <- list.files(path = "./out/", pattern = "CampbellData", full.names = TRUE)
latest_file <- files[which.max(file.info(files)$mtime)]
df <- read.csv2(latest_file, sep = ";", dec = ",")
models <- readRDS("./out/PARcorrectionModels.RDS")


# ==== DEFINE REFERENCE SENSOR =============================================
RefSensorLabel <- "T2"
ref_name <- unique(df[which(grepl(RefSensorLabel, df$sensor)), "sensor"])
TimeOfValid_initial <- ymd_hm("2025 06 05 09 48", tz = "Europe/Paris")
TimeOfValid_final  <- Inf

# ==== APPLY CALIBRATION MODELS TO LONG DATA ===============================
apply_correction_models <- function(df_long, models, out_col = "tension_cor") {
  df_long[[out_col]] <- NA_real_
  sensors <- unique(df_long$sensor)
  for (capteur in sensors) {
    if (!is.null(models[[capteur]])) {
      a <- models[[capteur]]$a
      b <- models[[capteur]]$b
      mask <- df_long$sensor == capteur
      df_long[[out_col]][mask] <- a * df_long$tension[mask] + b
      # df_long[[out_col]][mask] <- (df_long$tension[mask] - b) / a
    }
  }
  return(df_long)
}

df_corr <- apply_correction_models(df, models) %>%
  filter(!is.na(tension_cor))


# ==== FILTER TO COMPLETE TIMES FOR ALL SENSORS ============================
sensors_attendus <- unique(df_corr$sensor)
times_complets <- df_corr %>%
  group_by(time) %>%
  filter(all(sensors_attendus %in% sensor)) %>%
  pull(time) %>%
  unique()
df_corr <- df_corr %>%
  filter(time %in% times_complets)


# ==== ADD REFERENCE VALUES AND INDEX ======================================
df_corr <- df_corr %>%
  group_by(time) %>%
  mutate(tension_ref = first(tension[sensor == ref_name])) %>%
  ungroup() %>%
  group_by(sensor) %>%
  mutate(index = row_number()) %>%
  ungroup() 


# ==== FILTER PERIOD OF CALIBRATION & KEEP ONLY PAR SENSORS ================
df_valid <- df_corr %>% 
  mutate(time = lubridate::ymd_hms(time, tz = "Europe/Paris")) %>%
  filter(time > TimeOfValid_initial, time <= TimeOfValid_final) 


# ==== COMPUTE DAILY RMSE FOR EACH SENSOR (RAW & CORRECTED) ================
rmse_capteurs_jour <- df_valid %>%
  filter(sensor != ref_name) %>%
  mutate(Date = as.Date(time), hour = lubridate::hour(time),
         am_pm = ifelse(hour < 12, "AM", "PM")) %>%
  group_by(sensor, Date, am_pm) %>%
  summarise(RMSE_raw = sqrt(mean((tension - tension_ref)^2, na.rm = TRUE)),
            RMSE_cor = sqrt(mean((tension_cor - tension_ref)^2, na.rm = TRUE)),
            .groups = "drop")

print(paste0("RMSE raw: ", mean(rmse_capteurs_jour$RMSE_raw)))
print(paste0("RMSE cor: ", mean(rmse_capteurs_jour$RMSE_cor)))


# ==== RMSE PLOT: DAILY, PER SENSOR, RAW VS CORRECTED ======================
rmse_capteurs_jourl <- pivot_longer(rmse_capteurs_jour, values_to="RMSE", 
                                    names_to="Signal", cols=c(RMSE_raw, RMSE_cor))
g_rmse_ampm<-ggplot(rmse_capteurs_jourl, aes(Date, RMSE, color=Signal, group=Signal)) +
  geom_line() +
  theme_bw() +
  facet_grid(am_pm~sensor) +
  labs(title = "Daily RMSE for Each Sensor (Raw vs Corrected), AM/PM",
       y = "RMSE", x = "Date") +
  scale_color_manual("Signal", values=c(RMSE_raw="#e41a1c", RMSE_cor="#377eb8"))

print(g_rmse_ampm)
ggsave("./out/RMSE_by_day_by_sensor.png", g_rmse_ampm, width = 15, height = 6, dpi = 150)


# ==== SIGNAL PLOT: RAW / CORRECTED / REFERENCE ============================
df_corr_long <- pivot_longer(df_valid, values_to = "tension",
                             names_to = "tension_type", cols = c(tension, tension_cor, tension_ref))
g_signals <- ggplot(subset(df_corr_long, sensor != ref_name), 
                    aes(index, tension, color = tension_type, group = tension_type)) +
  geom_line() +
  facet_grid(sensor ~ .) +
  theme_bw() +
  labs(title = "Sensor Signals: Raw, Corrected, Reference",
       x = "Index (Sample)", y = "Tension") +
  theme(axis.text.x = element_blank()) +
  scale_color_manual("Signal", values = c(
    tension = "#e41a1c",
    tension_cor = "#4daf4a",
    tension_ref = "#377eb8"
  ))

print(g_signals)
ggsave("./out/Signals_by_sensor.png", g_signals, width = 15, height = 10.5, dpi = 150)


# ==== RESIDUALS: ADD TO DATAFRAME =========================================
df_resid <- df_valid %>%
  filter(sensor != ref_name) %>%
  mutate(
    Date = as.Date(time),
    hour = lubridate::hour(time),
    am_pm = ifelse(hour < 12, "AM", "PM"),
    resid_raw = tension - tension_ref,
    resid_cor = tension_cor - tension_ref
  )

# Pivot to long format for plotting both residuals
df_resid_long <- df_resid %>%
  pivot_longer(
    cols = c(resid_raw, resid_cor),
    names_to = "Signal",
    values_to = "residual"
  )

# ==== RESIDUAL PLOT: DOTS COLORED BY HOUR ================================
g_resid <- ggplot(df_resid_long, aes(Date, residual, color = hour)) +
  geom_jitter(width = 0.25, height = 0, alpha = 0.7, size = 1.6) +
  facet_grid(sensor ~ Signal, labeller = label_both) +
  theme_bw() +
  scale_color_viridis_c(option = "A", direction = -1) +
  labs(
    title = "Residuals (Raw and Corrected) by Day and Sensor",
    x = "Date",
    y = "Residual (Sensor - Reference)",
    color = "Hour"
  )

print(g_resid)
ggsave("./out/Residuals_by_day_by_sensor_by_ampm.png", g_resid, width = 15, height = 7, dpi = 150)


# ==== SAVE CORRECTED DATA AS CSV ==========================================
write.csv2(df_corr, paste0('./out/PARdata_', min(df_corr$Date), 
                           '_to_', max(df_corr$Date), "_corrected.csv"),
  row.names = FALSE, sep = ";", dec = ",")

cat("\nAll plots and corrected data saved to ./out/ folder.\n")
