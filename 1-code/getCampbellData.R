# SCRIPT TO READ AND VISUALIZE PAR SENSOR DATA FROM CAMPBELL LOGGER -------
# Authors: M. Larade-Lounis, L. Lescroart, M. Mc Cormick, R. Perez, D. Cornet (CIRAD, June 2025)

# ==== LOAD PACKAGES =======================================================
required_packs<-c("lubridate", "stringr", "tidyverse", "viridis", "data.table",
                  "cowplot", "plotly", "purrr")
InstIfNec <- function(pack) {
  if (!requireNamespace(pack, quietly = TRUE)) install.packages(pack)
  require(pack, character.only = TRUE)
}
invisible(lapply(required_packs, InstIfNec))


# ==== USER INPUTS =========================================================
Sensors <- c("Battery", "Temperature", rep("PAR", 5))
PATH2DATA <- "./data/"
files <- list.files(path = PATH2DATA, pattern = "DataPAR")
coefPAR <- 10.0  # Apogee sensor: µmol m-2 s-1 per mV


# ==== HELPER FUNCTIONS ====================================================
# Parse a single Campbell logger line into a named list of values
parse_campbell_row <- function(ref, variable = NULL) {
  blocks <- str_split(ref, "#")[[1]]
  blocks <- blocks[blocks != "" & !blocks %in% c("C1", "E")]
  pairs <- list()
  current_T <- NULL
  t_index <- 0
  for (b in blocks) {
    if (str_detect(b, "^T\\d+")) {
      current_T <- b
      t_index <- as.numeric(str_extract(current_T, "\\d+")) + 1
    } else if (str_detect(b, "^S\\d+")) {
      sx_part <- str_extract(b, "^S\\d+")
      val_part <- str_extract(b, "(?<=_)\\d+\\.?\\d*")
      # Sensor column name: <variable>_Tx_Sx if provided, else fallback to raw
      if (!is.null(variable) && length(variable) >= t_index) {
        colname <- paste0(variable[t_index], "_", current_T, "_", sx_part)
      } else {
        colname <- paste(current_T, sx_part, sep = "_")
      }
      pairs[[colname]] <- as.numeric(val_part)
    }
  }
  return(pairs)
}

# Parse all rows for a data.frame and expand into wide format
expand_campbell_data <- function(df, variable = NULL) {
  datalist <- lapply(df$RawData, function(x) parse_campbell_row(str_sub(x, 4, -2), variable))
  wide <- bind_rows(datalist)
  bind_cols(df, wide)
}


# ==== READ AND MERGE ALL FILES ============================================
allDat <- map_dfr(files, function(file) {
  # Extract timestamp from filename
  name <- str_remove(str_remove(file, "DataPAR_"), ".CSV")
  TimeStart <- ymd_hms(name)
  
  # Read file and compute datetime
  dat <- data.table::fread(file.path(PATH2DATA, file)) %>%
    as.data.frame() %>%
    mutate(time = ymd_hms(TimeStart + Timestamp, tz = "Europe/Paris"),
           Date = as.character(as.Date(time)),
           ref = str_sub(RawData, start = 4, end = str_length(RawData) - 1))
  
  # --- Mapping: Campbell column vs. user sensor labels
  first_blocks <- str_split(dat$ref[1], "#")[[1]]
  first_blocks <- first_blocks[first_blocks != "" & !first_blocks %in% c("C1", "E")]
  mapping <- data.frame(Campbell = character(0), Sensor = character(0), stringsAsFactors = FALSE)
  current_T <- NULL
  t_index <- 0
  for (b in first_blocks) {
    if (str_detect(b, "^T\\d+")) {
      current_T <- b
      t_index <- as.numeric(str_extract(current_T, "\\d+")) + 1
    } else if (str_detect(b, "^S\\d+")) {
      sx_part <- str_extract(b, "^S\\d+")
      campbell_name <- paste(current_T, sx_part, sep = "_")
      user_sensor <- if (!is.null(Sensors) && length(Sensors) >= t_index) Sensors[t_index] else campbell_name
      mapping<-rbind(mapping, data.frame(Campbell=campbell_name,
                                         Sensor=user_sensor, stringsAsFactors=F))
    }
  }
  cat("\n--- Mapping check for file:", file, "---\n")
  print(mapping)
  
  # --- Expand data (wide format) and pivot to long format for tidy usage
  dat_wide <- expand_campbell_data(dat, variable = Sensors)
  meta_cols <- names(dat)
  sensor_cols <- setdiff(names(dat_wide), meta_cols)
  
  dat_long <- dat_wide %>%
    pivot_longer(cols = all_of(sensor_cols), names_to = "sensor",
                 values_to = "tension")%>%
    mutate(tension = if_else(str_detect(sensor, "PAR"), tension * coefPAR,
                             tension))
  dat_long
})


# ==== WRITE OUTPUT ========================================================
output_file <- paste0("./out/CampbellData_", min(allDat$Date), "_to_",
                      max(allDat$Date), ".csv")
write.csv2(allDat, output_file, row.names = FALSE, sep = ";", dec = ",")
cat("\nAll files merged and saved as:", output_file, "\n")
