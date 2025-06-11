# SCRIPT: SHADING DETECTION & VISUALIZATION FOR PAR SENSORS ---------------
# Authors: M. Larade-Lounis, L. Lescroart, M. Mc Cormick, R. Perez, D. Cornet (CIRAD, June 2025)

# ==== LOAD PACKAGES & DATA ================================================
packs <- c('tidyverse', 'lubridate', 'data.table', 'randomForest')
for (pack in packs) {
  if (!require(pack, character.only = TRUE)) install.packages(pack)
  library(pack, character.only = TRUE)
}

# Read corrected PAR data (long format)
files <- list.files(path = "./out/", pattern = "PARdata", full.names = TRUE)
latest_file <- files[which.max(file.info(files)$mtime)]
df <- read.csv2(latest_file, sep = ";", dec = ",")


# ==== SET PARAMETERS ======================================================
RefSensorLabel <- "T2"
ref_name <- unique(df$sensor[grepl(RefSensorLabel, df$sensor)])

# Remove reference sensor (we only predict shading for others)
df <- df %>% filter(sensor != ref_name)

# Set shading pattern period (adapt if needed)
TimeOfCalib_initial <- -Inf
TimeOfCalib_final   <- ymd_hm("2025 06 03 16 10", tz = "Europe/Paris")

df <- df %>% 
  mutate(time = ymd_hms(time, tz = "Europe/Paris")) %>% 
  filter(time > TimeOfCalib_initial, time <= TimeOfCalib_final) %>%
  mutate(ratio = ifelse(tension_ref == 0, 1, tension / tension_ref),
         dif   = tension_ref - tension) %>%
  mutate(index = row_number()) 


# ==== HELPER FUNCTION: SHADOW DETECTION ===================================
detect_shading_rf <- function(df, rf_mod, prob_thresh = 0.5, min_gap = 10) {
  df <- df %>%
    mutate(ratio = ifelse(tension_ref == 0, 1, tension / tension_ref),
           dif   = tension_ref - tension, can_shadow = tension_ref >= 400)
  rf_pred <- predict(rf_mod, newdata = df, type = "prob")
  df$proba_ombre <- rf_pred[, "ombrage"]
  df$ombre_pred  <- (df$proba_ombre > prob_thresh) & df$can_shadow
  
  # Index-based grouping for shading periods
  df <- df %>% arrange(sensor, index)
  dt <- as.data.table(df)
  dt[, ombre_grp := NA_integer_]
  for (s in unique(dt$sensor)) {
    idx <- which(dt$sensor == s & dt$ombre_pred)
    if (length(idx) == 0) next
    grp <- 1
    dt$ombre_grp[idx[1]] <- grp
    for (i in 2:length(idx)) {
      if ((dt$index[idx[i]] - dt$index[idx[i-1]]) > min_gap) grp <- grp + 1
      dt$ombre_grp[idx[i]] <- grp
    }
  }
  as.data.frame(dt)
}


# ==== MANUAL CALIBRATION SET (HAND-LABELLED PERIODS) ======================
df_calib <- df %>%
  mutate(Heure = hour(time) + minute(time)/60,
         Etat = case_when((Heure >= 8 + 25/60 & Heure <= 9 + 45/60) |
                            (Heure >= 18 + 05/60 & Heure <= 18 + 59/60) ~ "ombrage",
                          (Heure >= 5 & Heure <= 8+ 05/60) |
                            (Heure >= 11 & Heure <= 12 + 30/60) ~ "soleil",
                          TRUE ~ NA_character_)) %>%
  filter(!is.na(Etat))

g_scatter <- ggplot(df_calib,
                      aes(tension_ref, tension, colour = Etat)) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  geom_point(alpha = 0.7, size = 1.4) +
  facet_wrap(~ sensor, ncol = 4) +
  theme_bw() +
  labs(title = "Calibration scatter: sensor vs. reference",
       x = "Reference tension (mV)", y = "Sensor tension (mV)")
print(g_scatter)
ggsave("./out/SunVSshade.png", g_scatter, width = 11, height = 5, dpi = 150)


# ==== FIT RANDOM FOREST MODEL =============================================
set.seed(42)
rf_mod <- randomForest(factor(Etat) ~ tension + tension_ref + ratio + dif,
                       data = df_calib, ntree = 300, importance = TRUE)


# ==== APPLY SHADING DETECTION TO ALL DATA =================================
df_pred <- detect_shading_rf(df, rf_mod, prob_thresh = 0.5, min_gap = 100)


# ==== POST-PROCESS: FILL IN SHADING GROUPS (NO GAPS INSIDE GROUPS) ========
df_pred <- df_pred %>%
  group_by(sensor, ombre_grp) %>%
  mutate(ombre_filled = ifelse(
    !is.na(ombre_grp) & index >= min(index[ombre_pred]) & index <= max(index[ombre_pred]),
      TRUE, ombre_pred)) %>%
  ungroup()


# ==== SUMMARIZE SHADING PERIODS (FOR RIBBONS) =============================
ombre_periods <- df_pred %>%
  filter(!is.na(ombre_grp)) %>%
  group_by(sensor, ombre_grp) %>%
  summarise(start_idx = min(index), end_idx=max(index), .groups="drop")

ombre_ribbons <- ombre_periods %>%
  select(sensor, ombre_grp, start_idx, end_idx)


# ==== PLOT: SHADING RIBBONS OVER INDEX (ONE FACET PER SENSOR) =============
g_shade <- ggplot() +
  geom_rect(data = ombre_ribbons, fill = "orange", alpha = 0.3,
            aes(xmin = start_idx, xmax = end_idx, ymin = -Inf, ymax = Inf)) +
  geom_line(data=df_pred, aes(x=index, y=tension, color="Sensor"), size=.7) +
  geom_line(data = df_pred, aes(x=index, y=tension_ref, color="Reference"),
            linewidth = 0.7, linetype = "dashed") +
  facet_wrap(~sensor, scales = "free_y") +
  scale_color_manual(values = c("Sensor" = "blue", "Reference" = "black")) +
  labs(x = "Index", y="Voltage", color = "Signal",
       title = "Shading Detection (index-based, all observations shown)") +
  theme_bw(base_size = 14)

print(g_shade)
ggsave("./out/ShadingDetection_ByIndex.png", g_shade, width = 12, height = 5, dpi = 150)

cat("\nShading detection plot saved in ./out/ShadingDetection_ByIndex.png\n")


# ==== DAILY AM/PM SUMMARY OF SHADING ======================================
## 1.  Flag AM / PM and calculate sample‐step in minutes --------------
df_pred <- df_pred %>%
  mutate(Date = as.Date(time), hour = hour(time), 
         am_pm = if_else(hour < 12, "AM", "PM"),
         shade_pct = 100 * (1 - tension / tension_ref))            

# infer logging interval once (assumes it is constant)
step_min <- round(median(diff(sort(unique(df_pred$time)))) / 60)


## 2.  Summarise ------------------------------------------------------------
step_sec <- median(as.numeric(diff(sort(unique(df_pred$time))), units = "secs"))
step_min <- step_sec / 60                         # <-- numeric, not difftime

shade_summary <- df_pred %>%
  filter(ombre_filled) %>%
  group_by(sensor, Date, am_pm) %>%
  summarise(
    avg_shade_pct      = mean(shade_pct, na.rm = TRUE),
    PARi                = mean(tension_ref, na.rm = TRUE),  
    shade_minutes      = n() * step_min,
    n_periods          = n_distinct(ombre_grp),
    .groups            = "drop"
  )


## 3. reshape --------------------------------------------------------------
shade_long <- shade_summary %>%
  pivot_longer(cols = c(avg_shade_pct, PARi, shade_minutes),
               names_to = "measure", values_to = "value") %>%
  mutate(measure = recode(measure,
                          avg_shade_pct     = "Shading intensity (%)",
                          PARi = "Incident PAR (µmol/m²/s)",
                          shade_minutes     = "Shade minutes"))

## 4. plot -----------------------------------------------------------------
g_facet <- ggplot(shade_long,
                  aes(Date, value, colour = am_pm, group = am_pm)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_grid(measure ~ sensor, scales = "free_y") +
  scale_colour_manual(values = c(AM = "#1f78b4", PM = "#e31a1c")) +
  theme_bw() +
  labs(title  = "Shading metrics by day: facet = measure × sensor",
       x      = "Date",
       y      = NULL,
       colour = "Half-day")

print(g_facet)
ggsave("./out/ShadingPeriodsCharacteristics.png", g_facet, width = 12, height = 6, dpi = 150)


## 5.  save to disk --------------------------------------------------------
write.csv2(shade_summary, "./out/Shading_summary_by_day_ampm_sensor.csv",
           row.names = FALSE)
cat("\nShading summary saved to ./out/Shading_summary_by_day_ampm_sensor.csv\n")


# ==== SHADING RELATIVE TO INCIDENT PAR ======================================
# Bin reference PAR (tension_ref) into e.g. 200 µmol m⁻² s⁻¹ steps
df_pred <- df_pred %>%
  mutate(par_bin=cut(tension_ref, breaks=seq(0, max(tension_ref, na.rm=T)+50, by=50),
                     include.lowest = TRUE, right = FALSE))

# Summarise shading statistics per bin
par_shade_stats<-df_pred %>%
  filter(tension_ref > 0) %>%
  group_by(sensor, par_bin) %>%
  summarise(n=n(), shading_freq = mean(ombre_filled, na.rm = TRUE),
            mean_shade_pct = mean(shade_pct[ombre_filled], na.rm = TRUE),
            mean_rel_intensity=mean(100*tension[ombre_filled]/tension_ref[ombre_filled], na.rm=T), 
    .groups = "drop")

# Plot: Average shading intensity as function of incoming PAR
plot_pari<-  ggplot(par_shade_stats, aes(x = par_bin, y = mean_shade_pct, colour = sensor, group = sensor)) +
  geom_line() + geom_point() +
  theme_bw() +
  labs(title = "Average shading intensity vs. incoming PAR",
       x = "Incoming PAR (reference, binned)", y = "Shading intensity (%)")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

print(plot_pari)
ggsave("./out/ShadingvsPARi.png", plot_pari, width = 12, height = 6, dpi = 150)


