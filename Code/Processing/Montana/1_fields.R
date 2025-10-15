
library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")

# ==== LOAD ====================================================================

fields_raw = st_read("Data/Raw/Fields/Montana/Montana_Statewide_Irrigation_Dataset.shp") |> 
  select(id = FID_1, county = COUNTYNAME, acres = New_Acres, irr_method = ITYPE) |> 
  st_make_valid() |> 
  st_transform(crs = 4326)

# ==== PANEL ===================================================================

fields_panel_temp = expand_grid(
  id = unique(fields_raw$id),
  year = 2008:2024
) |> 
  left_join(
    fields_raw |> select(id, county, acres, irr_method),
    by = "id"
  ) |> 
  # Uncode irrigation method
  mutate(irr_method = case_when(
    irr_method == "F" ~ "Flood",
    irr_method == "P" ~ "Center Pivot",
    irr_method == "S" ~ "Sprinkler",
    irr_method == "UNK" ~ "Unknown"
  )) |> 
  st_as_sf() |> 
  st_make_valid() |> 
  st_transform(crs = 4326)

# ==== SAVE ====================================================================

# Save as temporary fields panel (final panel will have CDL data)
st_write(fields_panel_temp, "Data/Clean/Fields/Montana/Temp/fields_panel_temp.shp", delete_layer = TRUE)
st_write(fields_panel_temp, "Data/Clean/Fields/Montana/Temp/fields_panel_temp.gpkg", delete_layer = TRUE)
