
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

openet_monthly_eemetric = map_dfr(1:nrow(stations), function(i) {
  
  st = stations[i, ]
  
  body = list(
    date_range = c("2023-12-01", "2025-09-01"),
    interval = "monthly",
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
      month = month(time)
    ) |> 
    select(station, year, month, et_mm_eemetric = et)
})

# ==== ENSEMBLE LOOP ===========================================================

openet_monthly_ensemble = map_dfr(1:nrow(stations), function(i) {
  
  st = stations[i, ]
  
  body = list(
    date_range = c("2023-12-01", "2025-09-01"),
    interval = "monthly",
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
      month = month(time)
    ) |> 
    select(station, year, month, et_mm_ensemble = et)
})

# ==== COMBINE AND SAVE ========================================================

openet_monthly = left_join(
  openet_monthly_eemetric, 
  openet_monthly_ensemble,
  by = c("station", "year", "month")
)

save(openet_monthly, file = "Data/Misc/EasyFlux vs. OpenET/openet_monthly.rda")
