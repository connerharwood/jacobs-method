
library(tidyverse)
library(sf)
library(readxl)
library(tmap)
tmap_mode("view")

# ==== REPAIR WRLU GEOMETRY ====================================================

for (year in 2017:2024) {
  # Full path of raw shapefile to fix
  shapefile_raw = list.files(
    path = file.path("Data/Raw/Fields/Utah/Raw WRLU", year),
    pattern = paste0(".*", year, ".*\\.shp$"),
    full.names = TRUE,
    recursive = FALSE
  )
  
  # Full path to write fixed shapefile to
  shapefile_fixed = file.path("Data/Raw/Fields/Utah/Repaired WRLU", year, paste0("wrlu_", year, ".shp"))
  
  # Use ogr2ogr to validate geometry of raw shapefile, saving as new shapefile
  system(paste(
    "ogr2ogr -f 'ESRI Shapefile'",
    shQuote(shapefile_fixed),
    shQuote(shapefile_raw),
    "-makevalid"
  ))
}

# ==== LOAD ====================================================================

# Load 2024 WRLU fields
wrlu_2024 = st_read("Data/Raw/Fields/Utah/Repaired WRLU/2024/wrlu_2024.shp") |> 
  # Only include agriculture fields in Utah
  filter(Landuse == "Agricultural", State == "Utah") |> 
  # Select and rename needed variables
  select(
    id = OBJECTID,
    county = County,
    basin = Basin,
    sub_area = SubArea,
    land_use = Landuse,
    acres_2024 = Acres,
    crop_2024 = Descriptio,
    cdl_2024 = Class_Name,
    crop_group_2024 = CropGroup,
    land_use_group_2024 = LU_Group,
    irr_method_2024 = IRR_Method,
    geometry
  ) |> 
  # Make geometry valid
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912) |> 
  # Create field centroid for joining with previous years
  mutate(centroid = st_centroid(geometry))

# Load 2023 WRLU fields
wrlu_2023 = st_read("Data/Raw/Fields/Utah/Repaired WRLU/2023/wrlu_2023.shp") |> 
  # Only include agriculture fields in Utah
  filter(Landuse == "Agricultural", State == "Utah") |> 
  # Select and rename needed variables
  select(
    acres_2023 = Acres,
    crop_2023 = Descriptio,
    cdl_2023 = Class_Name,
    crop_group_2023 = CropGroup,
    land_use_group_2023 = LU_Group,
    irr_method_2023 = IRR_Method,
    geometry
  ) |> 
  # Make geometry valid
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912)

# Load 2022 WRLU fields
wrlu_2022 = st_read("Data/Raw/Fields/Utah/Repaired WRLU/2022/wrlu_2022.shp") |> 
  # Only include agriculture fields in Utah
  filter(Landuse == "Agricultural", State == "Utah") |> 
  # Select and rename needed variables
  select(
    acres_2022 = Acres,
    crop_2022 = Descriptio,
    cdl_2022 = Class_Name,
    crop_group_2022 = CropGroup,
    land_use_group_2022 = LU_Group,
    irr_method_2022 = IRR_Method,
    geometry
  ) |> 
  # Make geometry valid
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912)

# Load 2021 WRLU fields
wrlu_2021 = st_read("Data/Raw/Fields/Utah/Repaired WRLU/2021/wrlu_2021.shp") |> 
  # Only include agriculture fields in Utah
  filter(Landuse == "Agricultural", State == "Utah") |> 
  # Select and rename needed variables
  select(
    acres_2021 = Acres,
    crop_2021 = Descriptio,
    cdl_2021 = Class_Name,
    crop_group_2021 = CropGroup,
    land_use_group_2021 = LU_Group,
    irr_method_2021 = IRR_Method,
    geometry
  ) |> 
  # Make geometry valid
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912)

# Load 2020 WRLU fields
wrlu_2020 = st_read("Data/Raw/Fields/Utah/Repaired WRLU/2020/wrlu_2020.shp") |> 
  # Only include agriculture fields in Utah
  filter(Landuse == "Agricultural", State == "Utah") |> 
  # Select and rename needed variables
  select(
    acres_2020 = Acres,
    crop_2020 = Descriptio,
    cdl_2020 = Class_Name,
    crop_group_2020 = CropGroup,
    land_use_group_2020 = LU_Group,
    irr_method_2020 = IRR_Method,
    geometry
  ) |> 
  # Make geometry valid
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912)

# Load 2019 WRLU fields
wrlu_2019 = st_read("Data/Raw/Fields/Utah/Repaired WRLU/2019/wrlu_2019.shp") |> 
  # Only include agriculture fields in Utah
  filter(Landuse == "Agricultural", State == "Utah") |> 
  # Select and rename needed variables
  select(
    acres_2019 = Acres,
    crop_2019 = Descriptio,
    cdl_2019 = Class_Name,
    crop_group_2019 = CropGroup,
    land_use_group_2019 = LU_Group,
    irr_method_2019 = IRR_Method,
    geometry
  ) |> 
  # Make geometry valid
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912)

# Load 2018 WRLU fields
wrlu_2018 = st_read("Data/Raw/Fields/Utah/Repaired WRLU/2018/wrlu_2018.shp") |> 
  # Only include agriculture fields in Utah
  filter(Landuse == "Agricultural", State == "Utah") |> 
  # Select and rename needed variables
  select(
    acres_2018 = Acres,
    crop_2018 = Descriptio,
    cdl_2018 = Class_Name,
    crop_group_2018 = CropGroup,
    land_use_group_2018 = LU_Group,
    irr_method_2018 = IRR_Method,
    geometry
  ) |> 
  # Make geometry valid
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912)

# Load 2017 WRLU fields
wrlu_2017 = st_read("Data/Raw/Fields/Utah/Repaired WRLU/2017/wrlu_2017.shp") |> 
  # Only include agriculture fields in Utah
  filter(Landuse == "Agricultural", State == "Utah") |> 
  # Select and rename needed variables
  select(
    acres_2017 = Acres,
    crop_2017 = Descriptio,
    cdl_2017 = Class_Name,
    crop_group_2017 = CropGroup,
    land_use_group_2017 = LU_Group,
    irr_method_2017 = IRR_Method,
    geometry
  ) |> 
  # Make geometry valid
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912)

# Load crop rooting zone depths for WRLU crops
rooting_depth = read_excel("Data/Misc/rooting_depth.xlsx")

# ==== BUILD PANEL =============================================================

# Add each pre-2024 WRLU dataset to list
wrlu_list = list(wrlu_2017, wrlu_2018, wrlu_2019, wrlu_2020, wrlu_2021, wrlu_2022, wrlu_2023)

# Name each dataset in list by year
names(wrlu_list) = 2017:2023

# Define 2024 fields as base polygons
wrlu_base = wrlu_2024

# Loop through each year, spatially joining with 2024 fields
for (yr in names(wrlu_list)) {
  # Get current year's WRLU sf object
  wrlu_current = wrlu_list[[yr]]
  
  # Join current year's fields with 2024's field centroids
  wrlu_base = st_join(
    wrlu_base,
    wrlu_current,
    join = st_intersects # Find fields that intersect with 2024 centroids
  )
  
  # Create dynamic columns for current year
  acres_col = paste0("acres_", yr)
  crop_col = paste0("crop_", yr)
  cdl_col = paste0("cdl_", yr)
  crop_group_col = paste0("crop_group_", yr)
  land_use_group_col = paste0("land_use_group_", yr)
  irr_method_col = paste0("irr_method_", yr)
  
  wrlu_base = wrlu_base |>
    # Calculate difference in acres between current year and 2024 field
    mutate(acres_diff = abs(acres_2024 - .data[[acres_col]])) |> 
    group_by(id) |> 
    # For each field, keep the intersecting field with smallest difference in acreage
    slice_min(order_by = acres_diff, n = 1, with_ties = FALSE) |> 
    ungroup() |> 
    mutate(
      # Replace current year's entry with NA if acreage difference is too big
      !!crop_col := if_else(acres_diff > 0.01, NA, .data[[crop_col]]),
      !!cdl_col := if_else(acres_diff > 0.01, NA, .data[[cdl_col]]),
      !!crop_group_col := if_else(acres_diff > 0.01, NA, .data[[crop_group_col]]),
      !!land_use_group_col := if_else(acres_diff > 0.01, NA, .data[[land_use_group_col]]),
      !!irr_method_col := if_else(acres_diff > 0.01, NA, .data[[irr_method_col]])
    )
  
  # Pivot to long format
  wrlu_panel = wrlu_base |> 
    pivot_longer(
      cols = matches("^(crop(_group)?|cdl|land_use_group|irr_method)_"),
      names_to = c(".value", "year"),
      names_pattern = "(.*)_(\\d{4})"
    ) |> 
    # Convert year to integer and crop to title case
    mutate(year = as.integer(year), crop = str_to_title(crop))
}

# ==== ANALYZE CROP LABELING CHANGES ===========================================

# Count number of crops per year to analyze changes in labels across years
crop_count = wrlu_panel |> 
  st_drop_geometry() |> 
  group_by(crop, year) |> 
  count() |> 
  pivot_wider(names_from = year, values_from = n)

# For each WRLU crop that doens't match a CDL crop, look at most common CDL crop
beans = wrlu_panel |> filter(crop == "Beans") |> group_by(cdl) |> count()
berries = wrlu_panel |> filter(crop == "Berries") |> group_by(cdl) |> count()
dry = wrlu_panel |> filter(crop == "Dry Land/Other") |> group_by(cdl) |> count()
fallow = wrlu_panel |> filter(crop == "Fallow") |> group_by(cdl) |> count()
fallow_idle = wrlu_panel |> filter(crop == "Fallow/Idle") |> group_by(cdl) |> count()
field_un = wrlu_panel |> filter(crop == "Field Crop Unspecified") |> group_by(cdl) |> count()
grain_un = wrlu_panel |> filter(crop == "Grain/Seeds Unspecified") |> group_by(cdl) |> count()
grass_hay = wrlu_panel |> filter(crop == "Grass Hay") |> group_by(cdl) |> count()
horti = wrlu_panel |> filter(crop == "Horticulture") |> group_by(cdl) |> count()
idle = wrlu_panel |> filter(crop == "Idle") |> group_by(cdl) |> count()
idle_pasture = wrlu_panel |> filter(crop == "Idle Pasture") |> group_by(cdl) |> count()
melon = wrlu_panel |> filter(crop == "Melon") |> group_by(cdl) |> count()
onion = wrlu_panel |> filter(crop == "Onion") |> group_by(cdl) |> count()
orchard_un = wrlu_panel |> filter(crop == "Orchard Unspecified") |> group_by(cdl) |> count()
pasture = wrlu_panel |> filter(crop == "Pasture") |> group_by(cdl) |> count()
potato = wrlu_panel |> filter(crop == "Potato") |> group_by(cdl) |> count()
turf = wrlu_panel |> filter(crop == "Turfgrass") |> group_by(cdl) |> count()
turf_ag = wrlu_panel |> filter(crop == "Turfgrass Ag") |> group_by(cdl) |> count()
veg = wrlu_panel |> filter(crop == "Vegetables") |> group_by(cdl) |> count()

# ==== FINALIZE PANEL ==========================================================

# Finalize 2017-2024 fields panel
fields_2017_2024 = wrlu_panel |> 
  # Align crop names across years and with CDL labels
  mutate(crop = case_when(
    crop %in% c("Dry Land/Other", "Fallow", "Idle", "Fallow/Idle") ~ "Fallow/Idle Cropland",
    crop %in% c("Idle Pasture", "Pasture") ~ "Grass/Pasture",
    crop %in% c("Turfgrass", "Turfgrass Ag") ~ "Sod/Grass Seed",
    crop == "Beans" ~ "Dry Beans",
    crop == "Field Crop Unspecified" ~ "Other Crops",
    crop == "Grass Hay" ~ "Other Hay/Non Alfalfa",
    crop == "Onion" ~ "Onions",
    crop == "Orchard Unspecified" ~ "Other Tree Crops",
    crop == "Potato" ~ "Potatoes",
    TRUE ~ crop
  )) |> 
  # Merge with rooting depth data
  left_join(rooting_depth, by = "crop") |> 
  # Select needed variables
  select(
    id,
    year,
    county,
    basin,
    sub_area,
    land_use,
    acres = acres_2024,
    crop,
    crop_group,
    land_use_group,
    rz_in,
    irr_method,
    geometry
  ) |> 
  # Transform to WGS 84
  st_transform(crs = 4326)

# Create panel of fields from 2008-2016 using 2024 fields
fields_2008_2016 = expand_grid(
  id = unique(wrlu_2024$id),
  year = c(2008:2016)
) |> 
  left_join(
    wrlu_2024 |> select(id, county, basin, sub_area, land_use, acres = acres_2024),
    by = "id"
  ) |> 
  st_as_sf() |> 
  st_make_valid() |> 
  st_transform(crs = 4326)

# Combine 2008-2016 fields with 2017-2024 fields
fields_panel_temp = bind_rows(fields_2017_2024, fields_2008_2016)

# ==== SAVE ====================================================================

# Save as temporary fields panel (final panel will have CDL data for missing crop)
st_write(fields_panel_temp, "Data/Clean/Fields/Utah/Temp/fields_panel_temp.shp", delete_layer = TRUE)
st_write(fields_panel_temp, "Data/Clean/Fields/Utah/Temp/fields_panel_temp.gpkg", delete_layer = TRUE)
