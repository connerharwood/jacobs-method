
library(tidyverse)
library(httr)
library(jsonlite)

# ==== LOAD ====================================================================

load("Data/Misc/EasyFlux vs. OpenET/easyflux_daily.rda")

api_key = readLines("Data/Misc/openet_api.txt")

header = add_headers(Authorization = api_key)

# ==== PREP ====================================================================

stations = easyflux_daily |> 
  select(station, lat, lon) |> 
  distinct()

# ==== EEMETRIC LOOP ===========================================================

openet_daily_eemetric = map_dfr(1:nrow(stations), function(i) {
  
  st = stations[i, ]
  
  body = list(
    date_range = c("2023-12-04", "2025-10-09"),
    interval = "daily",
    geometry = c(st$lon, st$lat),
    model = "eeMETRIC",
    variable = "et",
    reference_et = "gridMET",
    units = "mm",
    file_format = "JSON"
  )
  
  resp = POST(
    url = "https://openet-api.org/raster/timeseries/point",
    header,
    body = body,
    encode = "json"
  )
  
  content(resp, "parsed") |> 
    bind_rows() |> 
    mutate(
      station = st$station,
      time = as.Date(time, format = "%Y-%m-%d"),
      year = year(time),
      month = month(time),
      day = day(time)
    ) |> 
    select(station, year, month, day, et_mm = et)
})

# ==== ENSEMBLE LOOP ===========================================================

openet_daily_ensemble = map_dfr(1:nrow(stations), function(i) {
  
  st = stations[i, ]
  
  body = list(
    date_range = c("2023-12-04", "2025-10-09"),
    interval = "daily",
    geometry = c(st$lon, st$lat),
    model = "ensemble",
    variable = "et",
    reference_et = "gridMET",
    units = "mm",
    file_format = "JSON"
  )
  
  resp = POST(
    url = "https://openet-api.org/raster/timeseries/point",
    header,
    body = body,
    encode = "json"
  )
  
  content(resp, "parsed") |> 
    bind_rows() |> 
    mutate(
      station = st$station,
      time = as.Date(time, format = "%Y-%m-%d"),
      year = year(time),
      month = month(time),
      day = day(time)
    ) |> 
    select(station, year, month, day, et_mm = et)
})

# ==== COMBINE AND SAVE ========================================================

openet_daily = left_join(
  openet_daily_eemetric, 
  openet_daily_ensemble,
  by = c("station", "year", "month", "day")
) |> 
  rename(et_mm_eemetric = et_mm.x, et_mm_ensemble = et_mm.y)

save(openet_daily, file = "Data/Misc/EasyFlux vs. OpenET/openet_daily.rda")
