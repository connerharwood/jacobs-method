
library(tidyverse)
library(data.table)
library(glue)
library(sf)
library(terra)
library(exactextractr)

# ==== LOAD ====================================================================

# Sample raster for aligning CRS
sample_rast = rast("Data/USU Method/PRISM Daily/prism_ppt_us_30s_20241031.tif")

# Utah WRLU ag fields
fields_sf = st_read("Data/Clean/Fields/Utah/fields_panel.shp") |> 
  # Filter to just 2024 fields
  filter(year == 2024) |> 
  # Select needed variables
  select(id, geometry) |> 
  # Align CRS with PRISM raster
  st_transform(crs = crs(sample_rast))

# ==== EXTRACT MONTHLY PRECIP ==================================================

all_dates = seq(as.Date("2017-11-01"), as.Date("2024-10-31"), by = "day")

# Initialize list to store each year's data
all_prcp_list = list()

for (i in seq_along(all_dates)) {
  date = all_dates[i]
  date_str = format(date, "%Y%m%d")
  
  tif_path = glue("Data/USU Method/PRISM Daily/prism_ppt_us_30s_{date_str}.tif")
  
  # Load raster
  prism_rast = rast(tif_path)
  
  # Extract each field's daily precip
  prcp_extract = exact_extract(
    prism_rast,
    fields_sf,
    fun = "mean",
    force_df = TRUE,
    max_cells_in_memory = 350000000
  ) |>
    bind_cols(id = fields_sf$id) |>
    mutate(
      date = date,
      year = year(date),
      month = month(date),
      day = day(date),
      prcp_in = mean / 25.4
    ) |>
    select(id, date, year, month, day, prcp_in)
  
  setDT(prcp_extract)
  all_prcp_list[[i]] = prcp_extract
}

# Append list of each year's precip data into one dataframe
all_prcp = rbindlist(all_prcp_list) |> 
  # Create water year variable
  mutate(water_year = if_else(month >= 11, year + 1, year)) |> 
  select(id, water_year, year, month, day, prcp_in)

# Calculate winter precip for each field and water year
prcp_winter = all_prcp |> 
  # Filter to November through March
  filter(month %in% c(11, 12, 1, 2, 3)) |> 
  group_by(id, water_year) |> 
  summarize(prcp_win_in = sum(prcp_in, na.rm = FALSE), .groups = "drop")

# Rejoin winter precip with monthly precip panel
prism = all_prcp |> 
  left_join(prcp_winter, by = c("id", "water_year")) |> 
  select(id, water_year, year, month, day, prcp_in, prcp_win_in) |> 
  setDT()

# ==== SAVE ====================================================================

save(prism, file = "Data/USU Method/prism_daily.rda")
