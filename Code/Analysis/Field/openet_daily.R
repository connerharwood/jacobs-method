
library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(tmap)
tmap_mode("view")

# ==== LOAD ====================================================================

api_key = readLines("Data/Misc/openet_api_secondary.txt")
header = add_headers(Authorization = api_key)

fields = st_read("Data/Raw/Fields/Utah/Repaired WRLU/2024/wrlu_2024.shp")

# ==== PREP FIELDS =============================================================

flux_fields = fields |> 
  filter(OBJECTID %in% c(
    "33198", # Desert View
    "32580", # Bluff
    "150766", "151421",# Green River
    "33190", "31612", "28251", # Escalante
    "46982", "53089", # Pelican Lake
    "27372", "27035" # Wellington
  )) |> 
  st_make_valid() |> 
  st_transform(crs = 4326) |> 
  mutate(station = case_when(
    OBJECTID == "33198" ~ "Desert View",
    OBJECTID == "32580" ~ "Bluff",
    OBJECTID %in% c("150766", "151421") ~ "Green River",
    OBJECTID %in% c("33190", "31612", "28251") ~ "Escalante",
    OBJECTID %in% c("46982", "53089") ~ "Pelican Lake",
    OBJECTID %in% c("27372", "27035") ~ "Wellington"
  )) |> 
  select(
    OBJECTID,
    station,
    land_use = Landuse,
    crop_group = CropGroup,
    crop = Descriptio,
    irr_method = IRR_Method,
    acres = Acres,
    county = County,
    basin = Basin,
    sub_area = SubArea,
    geometry
  ) |> 
  group_by(station) |> 
  summarize(
    across(OBJECTID:sub_area, first),
    .groups = "drop"
  )

# ==== EEMETRIC LOOP ===========================================================

get_coords = function(geom) {
  coords = st_coordinates(geom)[, 1:2]
  as.vector(t(coords))
}

openet_daily_eemetric = map_dfr(1:nrow(flux_fields), function(i) {
  field = flux_fields[i, ]
  
  coords = get_coords(field$geometry)
  
  body = list(
    date_range = c("2023-12-04", "2025-10-09"),
    interval = "daily",
    geometry = coords,
    model = "eemetric",
    variable = "et",
    reference_et = "gridMET",
    reducer = "mean",
    units = "mm",
    file_format = "JSON"
  )
  
  resp = POST(
    url = "https://openet-api.org/raster/timeseries/polygon",
    header,
    body = body,
    encode = "json"
  )
  
  content(resp, "parsed") |> 
    bind_rows() #|> 
    # mutate(
    #   station = field$station,
    #   time = as.Date(time, format = "%Y-%m-%d"),
    #   year = year(time),
    #   month = month(time),
    #   day = day(time)
    # ) |> 
    # select(station, year, month, day, et_mm = et)
})




body = list(
  date_range = c("2020-01-01", "2020-12-31"),
  interval = "daily",                  # "daily" is also possible
  field_ids = "4952327", # your field IDs
  models = "ensemble",                # or "eeMETRIC"
  variables = "et",                   # can include "ET" and others
  file_format = "JSON"
)

resp = POST(
  url = "https://openet-api.org/geodatabase/timeseries",
  header,
  body = body,
  encode = "json"
)

# Try reading directly as text
text_content = content(resp, as = "text", encoding = "UTF-8")
data = jsonlite::fromJSON(text_content)
str(data, max.level = 2)
# the response might be gzipped
raw_content = content(resp, as = "raw")
data = jsonlite::fromJSON(rawToChar(memDecompress(raw_content, type = "gzip")))

d = as.data.frame(data)

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
