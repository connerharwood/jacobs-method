
library(tidyverse)
library(sf)

# ==== LOAD ====================================================================

bluff_raw = read_csv("Data/Raw/Utah Flux Network/Bluff_Flux_AmeriFluxFormat.dat")
desert_view_raw = read_csv("Data/Raw/Utah Flux Network/Desert View_Flux_AmeriFluxFormat.dat")
escalante_raw = read_csv("Data/Raw/Utah Flux Network/Escalante_Flux_AmeriFluxFormat.dat")
green_river_raw = read_csv("Data/Raw/Utah Flux Network/Green River_Flux_AmeriFluxFormat.dat")
pelican_lake_raw = read_csv("Data/Raw/Utah Flux Network/Pelican Lake_Flux_AmeriFluxFormat.dat")
wellington_raw = read_csv("Data/Raw/Utah Flux Network/Wellington_Flux_AmeriFluxFormat.dat")

# ==== CLEAN ===================================================================

bluff = bluff_raw |> 
  slice(-1:-3) |> 
  mutate(
    station = "Bluff",
    flux_values = str_split(Flux_AmeriFluxFormat, ","),
    et_mm = as.numeric(map_chr(flux_values, ~.x[6])),
    et_mm = ifelse(is.na(et_mm), NA, et_mm),
    timestamp = as.POSIXct(TOA5, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    year = year(timestamp),
    month = month(timestamp),
    day = day(timestamp),
    time = format(timestamp, "%H:%M:%S"),
    lat = 37.2818,
    lon = -109.5349
  ) |> 
  select(timestamp, station, lat, lon, year, month, day, time, et_mm)

desert_view = desert_view_raw |> 
  slice(-1:-3) |> 
  mutate(
    station = "Desert View",
    flux_values = str_split(Flux_AmeriFluxFormat, ","),
    et_mm = as.numeric(map_chr(flux_values, ~.x[6])),
    et_mm = ifelse(is.na(et_mm), NA, et_mm),
    timestamp = as.POSIXct(TOA5, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    year = year(timestamp),
    month = month(timestamp),
    day = day(timestamp),
    time = format(timestamp, "%H:%M:%S"),
    lat = 40.126664,
    lon = -109.954922
  ) |> 
  select(timestamp, station, lat, lon, year, month, day, time, et_mm)

escalante = escalante_raw |> 
  slice(-1:-3) |> 
  mutate(
    station = "Escalante",
    flux_values = str_split(Flux_AmeriFluxFormat, ","),
    et_mm = as.numeric(map_chr(flux_values, ~.x[6])),
    et_mm = ifelse(is.na(et_mm), NA, et_mm),
    timestamp = as.POSIXct(TOA5, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    year = year(timestamp),
    month = month(timestamp),
    day = day(timestamp),
    time = format(timestamp, "%H:%M:%S"),
    lat = 37.7359692085905,
    lon = -111.55517578125
  ) |> 
  select(timestamp, station, lat, lon, year, month, day, time, et_mm)

green_river = green_river_raw |> 
  slice(-1:-3) |> 
  mutate(
    station = "Green River",
    flux_values = str_split(Flux_AmeriFluxFormat, ","),
    et_mm = as.numeric(map_chr(flux_values, ~.x[6])),
    et_mm = ifelse(is.na(et_mm), NA, et_mm),
    timestamp = as.POSIXct(TOA5, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    year = year(timestamp),
    month = month(timestamp),
    day = day(timestamp),
    time = format(timestamp, "%H:%M:%S"),
    lat = 39.01884,
    lon = -110.1662778
  ) |> 
  select(timestamp, station, lat, lon, year, month, day, time, et_mm)

pelican_lake = pelican_lake_raw |> 
  slice(-1:-3) |> 
  mutate(
    station = "Pelican Lake",
    flux_values = str_split(Flux_AmeriFluxFormat, ","),
    et_mm = as.numeric(map_chr(flux_values, ~.x[6])),
    et_mm = ifelse(is.na(et_mm), NA, et_mm),
    timestamp = as.POSIXct(TOA5, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    year = year(timestamp),
    month = month(timestamp),
    day = day(timestamp),
    time = format(timestamp, "%H:%M:%S"),
    lat = 40.187581,
    lon = -109.658701
  ) |> 
  select(timestamp, station, lat, lon, year, month, day, time, et_mm)

wellington = wellington_raw |> 
  slice(-1:-3) |> 
  mutate(
    station = "Wellington",
    flux_values = str_split(Flux_AmeriFluxFormat, ","),
    et_mm = as.numeric(map_chr(flux_values, ~.x[6])),
    et_mm = ifelse(is.na(et_mm), NA, et_mm),
    timestamp = as.POSIXct(TOA5, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    year = year(timestamp),
    month = month(timestamp),
    day = day(timestamp),
    time = format(timestamp, "%H:%M:%S"),
    lat = 39.4453,
    lon = -110.7288
  ) |> 
  select(timestamp, station, lat, lon, year, month, day, time, et_mm)

easyflux_30min = rbind(bluff, desert_view, escalante, green_river, pelican_lake, wellington)

check_missing = easyflux_30min |> 
  group_by(station, year, month) |> 
  mutate(
    monthly_obs = n(),
    monthly_missing = sum(is.na(et_mm)),
    percent_missing = monthly_missing / monthly_obs
  ) |> 
  ungroup()

ggplot(check_missing, aes(x = percent_missing)) +
  geom_histogram(bins = 100)

easyflux_monthly = easyflux_30min |> 
  group_by(station, year, month) |> 
  summarize(
    lat = first(lat),
    lon = first(lon),
    total_obs = n(),
    missing_obs = sum(is.na(et_mm)),
    percent_missing = missing_obs / total_obs,
    et_mm_easyflux = ifelse(percent_missing <= .10, sum(et_mm, na.rm = TRUE), NA),
    .groups = "drop"
  ) |> 
  select(station, lat, lon, year, month, et_mm_easyflux) |> 
  filter(!(year == 2025 & month == 10))

# ==== SAVE ====================================================================

save(easyflux_monthly, file = "Data/Misc/EasyFlux vs. OpenET/easyflux_monthly.rda")
