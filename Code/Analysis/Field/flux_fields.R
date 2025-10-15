
library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")

# ==== LOAD ====================================================================

fields = st_read("Data/Raw/Fields/Utah/Repaired WRLU/2024/wrlu_2024.shp")

# ==== PROCESS =================================================================

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

# ==== SAVE ====================================================================

