
library(tidyverse)
library(readxl)
library(terra)
library(sf)
library(exactextractr)
library(data.table)

# ==== PROCESS CDL DATA ========================================================

# Load crop rooting zone depths for CDL crops
rooting_depth = read_excel("Data/Misc/rooting_depth.xlsx")

# Load sample rast to transform fields CRS
sample_rast = rast("Data/Raw/CDL/Montana/CDL_2024_30.tif")

# Load temporary fields panel
fields_panel_temp = st_read("Data/Clean/Fields/Montana/Temp/fields_panel_temp.gpkg") |> 
  # Align CRS with CDL raster
  st_transform(crs = crs(sample_rast))

# List of yearly CDL TIFF files
tiff_files = list.files("Data/Raw/CDL/Montana", full.names = TRUE)

# Initialize list to store each year
cdl_panel = list()

# Loop through and process each TIFF file
for (file in tiff_files) {
  # Extract year from TIFF file
  current_year = as.integer(str_extract(file, "\\d{4}"))
  
  # Filter fields to current year
  fields_current = fields_panel_temp |> filter(year == current_year)
  
  # Load CDL TIFF file
  cdl_rast = rast(file)
  
  # Extract mode crop for each field
  cdl_extract = exact_extract(
    # Extract raster values for current year's field poylgons
    cdl_rast,
    fields_current,
    
    # Area-weighted majority crop over a field's pixels
    fun = "majority",
    
    # Return output as a dataframe
    force_df = TRUE,
    
    # Max number of raster cells to load (Montana has just less than this amount)
    max_cells_in_memory = 535000000
  )
  
  # Build yearly table
  cdl_fields = cdl_extract |> 
    # Merge in field ID
    bind_cols(id = fields_current$id) |> 
    # Create year variable
    mutate(year = current_year) |> 
    # Rename extracted column
    rename(cdl_code = majority)
  
  # Store current year in list
  cdl_panel[[as.character(current_year)]] = cdl_fields
}

# Join fields panel with CDl data
fields_panel = fields_panel_temp |> 
  # Merge with unnested CDL data
  left_join(
    bind_rows(cdl_panel), 
    by = c("id", "year"),
    relationship = "one-to-one"
  ) |> 
  # Merge with rooting zone depths
  left_join(
    rooting_depth |> filter(!is.na(cdl_code)), 
    by = "cdl_code",
    relationship = "many-to-one"
  ) |> 
  # Select needed variables
  select(id, year, county, acres, irr_method, crop, rz_in, geometry = geom) |> 
  # Transform to WGS 84
  st_transform(crs = 4326)

# ==== SAVE ====================================================================

st_write(fields_panel, "Data/Clean/Fields/Montana/fields_panel.shp", delete_layer = TRUE)
st_write(fields_panel, "Data/Clean/Fields/Montana/fields_panel.gpkg", delete_layer = TRUE)
