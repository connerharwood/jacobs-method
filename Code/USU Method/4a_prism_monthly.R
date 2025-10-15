
library(tidyverse)
library(data.table)
library(glue)
library(sf)
library(terra)
library(rnaturalearth)
library(exactextractr)

# ==== LOAD ====================================================================

# Sample raster for aligning CRS
sample_rast = rast("Data/Raw/PRISM/prism_ppt_us_30s_202410.tif")

# Utah state boundary
state_sf = ne_states(country = "United States of America", returnclass = "sf") |> 
  # Filter to Utah
  filter(name == "Utah") |> 
  # Select needed variables
  select(name, geometry) |> 
  # Align CRS with ET raster
  st_transform(crs = crs(sample_rast)) |> 
  # Add buffer around boundary to include all raster tiles
  st_buffer(dist = 1000, max_cells = 100000) |> 
  # Simplify geometry for faster processing
  st_simplify()

# Utah WRLU ag fields
fields_sf = st_read("Data/Clean/Fields/Utah/fields_panel.shp") |> 
  # Filter to just 2024 fields
  filter(year == 2024) |> 
  # Select needed variables
  select(id, geometry) |> 
  # Align CRS with PRISM raster
  st_transform(crs = crs(sample_rast))

# ==== EXTRACT MONTHLY PRECIP ==================================================

# Vectorize Utah state boundary
state_vect = vect(state_sf)

# Initialize list to store each year's data
all_prcp_list = list()

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
    
    # Add 0 to beginning of single-digit months, combine with year
    yearmonth = paste0(year, sprintf("%02d", month))
    
    # Load PRISM raster for current year
    prism_rast = rast(glue("Data/Raw/PRISM/prism_ppt_us_30s_{yearmonth}.tif"))
    
    # Crop and mask to Utah state boundary
    prism_crop = crop(prism_rast, state_vect)
    prism_utah = crop(prism_crop, state_vect)
    
    # Extract each field's monthly precip
    prcp_extract = exact_extract(
      # Extract raster values for current month's field poylgons
      prism_utah, 
      fields_sf, 
      
      # Calculate area-weighted mean precip across each field's pixels
      fun = "mean",
      
      # Return output as a dataframe
      force_df = TRUE,
      
      # Max number of raster cells to load (Utah has just less than this amount)
      max_cells_in_memory = 350000000
    ) |> 
      # Merge in field ID
      bind_cols(id = fields_sf$id) |> 
      mutate(
        year = year, # Create year column
        month = month, # Create month column
        prcp_in = mean / 25.4 # Convert precip from mm to in
      ) |> 
      select(id, year, month, prcp_in)
    
    # Set as data table for faster processing
    setDT(prcp_extract)
    
    # Store current month in list
    year_list[[as.character(month)]] = prcp_extract
  }
  
  # Combine all months for current year
  all_prcp_list[[as.character(year)]] = rbindlist(year_list)
}

# Append list of each year's precip data into one dataframe
all_prcp = rbindlist(all_prcp_list) |> 
  # Create water year variable
  mutate(water_year = if_else(month >= 11, year + 1, year)) |> 
  select(id, water_year, year, month, prcp_in)

# Calculate winter precip for each field and water year
prcp_winter = all_prcp |> 
  # Filter to November through March
  filter(month %in% c(11, 12, 1, 2, 3)) |> 
  group_by(id, water_year) |> 
  summarize(prcp_win_in = sum(prcp_in, na.rm = FALSE), .groups = "drop")

# Rejoin winter precip with monthly precip panel
prism = all_prcp |> 
  left_join(prcp_winter, by = c("id", "water_year")) |> 
  select(id, water_year, year, month, prcp_in, prcp_win_in) |> 
  setDT()

# ==== SAVE ====================================================================

# Save as RData file
save(prism, file = "Data/Clean/Input Data/Utah/prism.rda")
