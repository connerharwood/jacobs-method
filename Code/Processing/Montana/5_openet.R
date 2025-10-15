
library(tidyverse)
library(data.table)
library(glue)
library(sf)
library(terra)
library(exactextractr)

# ==== LOAD ====================================================================

# Sample raster for aligning CRS
sample_rast = rast("Data/Raw/OpenET/Montana/eeMETRIC/montana_eemetric_2024_10-0000000000-0000000000.tif")

# Montana WRLU ag fields
fields_sf = st_read("Data/Clean/Fields/Montana/fields_panel.shp") |> 
  # Filter to just 2024 fields
  filter(year == 2024) |> 
  # Select needed variables
  select(id, geometry) |> 
  # Align CRS with ET raster
  st_transform(crs = crs(sample_rast))

# ==== EXTRACT MONTHLY ET ======================================================

# Initialize list to store each year's data
all_et_list = list()

for (year in 2007:2024) {
  # Initalize list for current year's monthly data
  year_list = list()
  
  # Determine which months to process for current year (data is 11/2007-10/2024)
  if (year == 2007) {
    months = 11:12 # Nov-Dec for 2007
  } else if (year == 2024) {
    months = 1:10 # Jan-Oct for 2024
  } else {
    months = 1:12 # All months for 2008-2023
  }
  
  for (month in months) {
    print(glue("Extracting for year {year}, month {month}"))
    
    # List all GeoTIFF files for current year and month
    file_pattern = glue("montana_eemetric_{year}_{month}-")
    files = list.files(
      "Data/Raw/OpenET/Montana/eeMETRIC",
      pattern = file_pattern,
      full.names = TRUE
    )
    
    # Load and merge tiles
    rast_list = lapply(files, rast)
    openet_rast = do.call(merge, rast_list)
    
    # Extract each field's monthly ET
    et_extract = exact_extract(
      # Extract raster values for current month's field poylgons
      openet_rast, 
      fields_sf, 
      
      # Calculate area-weighted mean ET across each field's pixels
      fun = "mean",
      
      # Return output as a dataframe
      force_df = TRUE,
      
      # Max number of raster cells to load (Montana has just less than this amount)
      max_cells_in_memory = 1000000000
    ) |> 
      # Merge in field ID
      bind_cols(id = fields_sf$id) |> 
      mutate(
        year = year, # Create year column
        month = month, # Create month column
        et_in = mean / 25.4, # Convert ET from mm to in
        et_ft = et_in / 12, # Convert ET from in to ft
      ) |> 
      select(id, year, month, et_in, et_ft)
    
    # Set as data table for faster processing
    setDT(et_extract)
    
    # Store current month in list
    year_list[[as.character(month)]] = et_extract
  }
  
  # Combine all months for current year
  all_et_list[[as.character(year)]] = rbindlist(year_list)
}

# Append list of each year's ET data into one dataframe
all_et = rbindlist(all_et_list) |> 
  # Create water year variable
  mutate(water_year = if_else(month >= 11, year + 1, year)) |> 
  select(id, water_year, year, month, et_in, et_ft)

# Calculate winter ET for each field and water year
et_winter = all_et |> 
  # Filter to November through March
  filter(month %in% c(11, 12, 1, 2, 3)) |> 
  group_by(id, water_year) |> 
  summarize(et_win_in = sum(et_in, na.rm = FALSE), .groups = "drop")

# Calculate growing season ET for each field and water year
et_grow = all_et |> 
  # Filter to April through October
  filter(month %in% c(4, 5, 6, 7, 8, 9, 10)) |> 
  group_by(id, water_year) |> 
  summarize(et_grow_in = sum(et_in, na.rm = FALSE), .groups = "drop")

# Rejoin winter and growing season ET with monthly ET panel
openet_eemetric = all_et |> 
  left_join(et_winter, by = c("id", "water_year")) |> 
  left_join(et_grow, by = c("id", "water_year")) |> 
  select(
    id, 
    water_year,
    year, 
    month, 
    et_in, 
    et_ft, 
    et_win_in, 
    et_grow_in
  ) |> 
  setDT()

# ==== SAVE ====================================================================

# Save as RData file
save(openet_eemetric, file = "Data/Clean/Input Data/Montana/openet_eemetric.rda")
