# SCRIPT: READ, FILTER AND CALIBRATE PAR SENSORS FROM LOGGER --------------
# Authors: M. Larade-Lounis, L. Lescroart, M. Mc Cormick, R. Perez, D. Cornet (CIRAD, June 2025)

# ==== LOAD PACKAGES =======================================================
required_packs <- c("lubridate", "tidyverse", "ggpmisc", "patchwork", "suncalc")
InstIfNec <- function(pack) {
  if (!requireNamespace(pack, quietly = TRUE)) install.packages(pack)
  require(pack, character.only = TRUE)
}
invisible(lapply(required_packs, InstIfNec))

# ==== USER INPUTS =========================================================
latitude  <- 43.65038816873962
longitude <- 3.8692266353798983
RefSensorLabel <- "T2"
TimeOfCalib_initial <- ymd_hm("2025 06 03 16 20", tz = "Europe/Paris")
TimeOfCalib_final  <- ymd_hm("2025 06 05 09 45", tz = "Europe/Paris")
custom_minutes_morning <- 0
custom_minutes_evening <- 120


# ==== Ploting fct =========================================================
make_pair_plot <- function(df_long, ref_name, sensor) {
  # Wide format for just the two sensors (ref and sensor)
  df_wide <- df_long %>%
    select(time, sensor, tension) %>%
    filter(sensor %in% c(ref_name, sensor)) %>%
    pivot_wider(names_from = sensor, values_from = tension)
  
  df_wide <- df_wide %>%
    mutate(hour=lubridate::hour(time) + lubridate::minute(time) / 60)
  
  df_time <- df_wide %>%
    pivot_longer(cols=c(ref_name, sensor), names_to="Type", values_to="Tension") %>%
    arrange(time) %>%
    group_by(Type) %>%
    mutate(index = row_number()) %>%
    ungroup()
  
  plot_time <- ggplot(df_time, aes(x = index, y = Tension, color = Type)) +
    geom_line() +
    labs(title=paste(sensor, "&", ref_name, "(index)"), x = "Index (diurnal sample)", y = "Tension") +
    theme_bw() +
    scale_color_manual(values = setNames(c("black", "dodgerblue"), c(ref_name, sensor)),
                       labels=c(ref_name, sensor))
  
  plot_reg <- ggplot(df_wide, aes_string(x = ref_name, y = sensor, color = "hour")) +
    geom_abline(linetype = "dashed", color = "grey50") +
    geom_point(alpha = 0.8, size = 2) +
    geom_smooth(method = "lm", color = "red", se = FALSE, inherit.aes = FALSE,
                aes_string(x = ref_name, y = sensor)) +
    ggpmisc::stat_poly_eq(aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~")),
                          formula=y ~ x, parse=T, size=4, color="black") +
    labs(title=paste(sensor, "vs", ref_name), x=ref_name, y=sensor, color="Hour") +
    scale_color_viridis_c(option = "A", direction = -1) +
    theme_bw()
  
  plot_time + plot_reg
}


# ==== READ DATA & PLOT DATA ===============================================
files <- list.files(path = "./out/", pattern = "CampbellData", full.names = TRUE)
latest_file <- files[which.max(file.info(files)$mtime)]
df <- read.csv2(latest_file, sep = ";", dec = ",")

par_names  <- unique(df$sensor)[grep("PAR", unique(df$sensor))]
ref_name   <- unique(par_names)[grep("T2", par_names)]
other_names <- setdiff(par_names, ref_name)

plots_raw <- map(other_names, ~make_pair_plot(df, ref_name, .x))
pw_raw <- wrap_plots(plots_raw, ncol = 1)
print(pw_raw)
ggsave("./out/PairPlot_step1_raw.png", pw_raw, width = 14, 
       height = 4 * length(other_names), dpi = 150)


# ==== FILTER PERIOD OF CALIBRATION & KEEP ONLY PAR SENSORS ================
df_PAR <- df %>% 
  mutate(time = lubridate::ymd_hms(time, tz = "Europe/Paris")) %>%
  filter(time > TimeOfCalib_initial, time <= TimeOfCalib_final) %>%
  filter(str_detect(sensor, "PAR"))

plots_calib <- map(other_names, ~make_pair_plot(df_PAR, ref_name, .x))
pw_calib <- wrap_plots(plots_calib, ncol = 1)
print(pw_calib)
ggsave("./out/PairPlot_step2_calib.png", pw_calib, width = 14, 
       height = 4 * length(other_names), dpi = 150)


# ==== REMOVE NIGHT PERIODS ================================================
filter_by_light_period <- function(df, latitude, longitude, 
                                   light_period = c("sunrise_sunset", "civil", "nautical", "custom_minutes"), 
                                   custom_minutes_morning, custom_minutes_evening,
                                   time_col = "time", tz = "UTC") {
  light_period <- match.arg(light_period)
  date_df <- df %>% mutate(Date = as.Date(.data[[time_col]])) %>% distinct(Date)
  
  if (light_period == "sunrise_sunset") {
    sun_cols <- c("sunrise", "sunset")
    col_start <- "sunrise"; col_end <- "sunset"
  } else if (light_period == "civil") {
    sun_cols <- c("dawn", "dusk")
    col_start <- "dawn"; col_end <- "dusk"
  } else if (light_period == "nautical") {
    sun_cols <- c("nauticalDawn", "nauticalDusk")
    col_start <- "nauticalDawn"; col_end <- "nauticalDusk"
  } else if (light_period == "custom_minutes") {
    sun_cols <- c("sunrise", "sunset")
    col_start <- "sunrise"; col_end <- "sunset"
  }
  
  sun_tab <- suncalc::getSunlightTimes(
    date = date_df$Date, lat = latitude, lon = longitude, keep = sun_cols, tz = tz
  )
  
  if (light_period == "custom_minutes") {
    sun_tab <- sun_tab %>%
      mutate(
        period_start = .data[[col_start]] - lubridate::minutes(custom_minutes_morning),
        period_end   = .data[[col_end]]   + lubridate::minutes(custom_minutes_evening)
      )
  } else {
    sun_tab <- sun_tab %>%
      mutate(
        period_start = .data[[col_start]],
        period_end   = .data[[col_end]]
      )
  }
  
  df2 <- df %>%
    mutate(Date = as.Date(.data[[time_col]])) %>%
    left_join(sun_tab %>% select(date, period_start, period_end), by = c("Date" = "date")) %>%
    filter(.data[[time_col]] >= period_start, .data[[time_col]] <= period_end)
  
  return(df2)
}

df_PAR <- filter_by_light_period(df_PAR, latitude, longitude, 
                                 light_period = "custom_minutes",
                                 custom_minutes_morning, custom_minutes_evening)

plots_day <- map(other_names, ~make_pair_plot(df_PAR, ref_name, .x))
pw_day <- wrap_plots(plots_day, ncol = 1)
print(pw_day)
ggsave("./out/PairPlot_step3_daytime.png", pw_day, width = 14, 
       height = 4 * length(other_names), dpi = 150)


# ==== REMOVE EDGE EFFECTS BY INDEX (start/end of day) =====================
# Pole shadow introduce a bias in the calibration due to shading lag between sensors 
filter_by_index_range <- function(df, index_col = "index", intervals = list(c(80, 225))) {
  if (is.numeric(intervals[[1]])) intervals <- list(intervals)
  mask <- rep(FALSE, nrow(df))
  for (iv in intervals) mask <- mask | (df[[index_col]] >= iv[1] & df[[index_col]] <= iv[2])
  df[mask, , drop = FALSE]
}

df_PAR <- df_PAR %>%
  arrange(sensor, time) %>%
  group_by(sensor) %>%             
  mutate(index = row_number()) %>%
  ungroup()
df_PAR<-filter_by_index_range(df_PAR, index_col="index", intervals=c(80, 225))

plots_final <- map(other_names, ~make_pair_plot(df_PAR, ref_name, .x))
pw_final <- wrap_plots(plots_final, ncol = 1)
print(pw_final)
ggsave("./out/PairPlot_step4_final.png", pw_final, width = 14, 
       height = 4 * length(other_names), dpi = 150)


# ==== CALIBRATE AND SAVE REGRESSION MODELS ================================
df_wide <- df_PAR %>%
  pivot_wider(names_from = sensor, values_from = tension)
save_correction_models <- function(df_wide, ref_name, file_out = NULL) {
  par_names <- names(df_wide)[str_detect(names(df_wide), "PAR")]
  sensors_to_corr <- setdiff(par_names, ref_name)
  models <- list()
  for (capteur in sensors_to_corr) {
    weights_vec <- df_wide[[ref_name]]
    mod <- lm(df_wide[[ref_name]] ~ df_wide[[capteur]],  weights = weights_vec)
    coefs <- coef(mod)
    models[[capteur]] <- list(a = coefs[2], b = coefs[1])
  }
  # Identity for ref
  models[[ref_name]] <- list(a = 1, b = 0)
  if (!is.null(file_out)) saveRDS(models, file_out)
  return(models)
}
models <- save_correction_models(df_wide, ref_name, file_out = "./out/PARcorrectionModels.RDS")

cat("\nAll plots saved to ./out/ and correction models exported.\n")
