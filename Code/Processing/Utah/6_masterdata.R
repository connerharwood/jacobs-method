
library(tidyverse)
library(sf)
library(data.table)

# ==== LOAD ====================================================================

# Load 2017-2024 Utah fields panel
fields_panel = st_read("Data/Clean/Fields/Utah/fields_panel.shp") |> 
  st_drop_geometry()

load("Data/Clean/Input Data/Utah/ssurgo.rda") # Soil data
load("Data/Clean/Input Data/Utah/prism.rda") # Monthly precipitation data
load("Data/Clean/Input Data/Utah/openet_eemetric.rda") # Monthly ET data

# ==== MERGE ===================================================================

# Create crosswalk of all fields, years, and months
full_panel = expand_grid(
  id = unique(fields_panel$id),
  year = 2007:2024,
  month = 1:12
) |> 
  mutate(water_year = if_else(month >= 11, year + 1, year)) |> 
  filter(water_year %in% 2008:2024) |> 
  setDT()

# Merge crosswalk with yearly fields
merge1 = full_panel |> 
  left_join(
    fields_panel,
    by = c("id", "water_year" = "year"),
    relationship = "many-to-one"
  )

# Join first merge with time-invariant SSURGO soil data
merge2 = merge1 |> 
  left_join(
    ssurgo,
    by = "id",
    relationship = "many-to-one"
  ) |> 
  # Ensure that rooting depth doesn't exceed water table or bedrock
  mutate(rz_in = case_when(
    is.na(rz_in) ~ rz_in, # Keep NA as is
    !is.na(rz_in) & rz_in > max_rz_in ~ max_rz_in, # Set rz to restrictive layer if it exceeds it
    TRUE ~ rz_in # Keep all other cases as is
  ))

# Join second merge with monthly PRISM precipitation data
merge3 = merge2 |> 
  left_join(
    prism,
    by = c("id", "year", "month"),
    relationship = "one-to-one"
  )

# Join third merge with monthly OpenET data
merge4 = merge3 |> 
  left_join(
    openet_eemetric,
    by = c("id", "year", "month"),
    relationship = "one-to-one"
  )

# Free up memory
rm(ssurgo, prism, full_panel, merge1, merge2, merge3)
gc()

# ==== DEPLETION INPUTS ========================================================

depletion_data1 = merge4 |> 
  select(
    id,
    water_year,
    year,
    month,
    county,
    basin,
    sub_area = sub_are,
    land_use = land_us,
    acres,
    irr_method = irr_mth,
    crop,
    crop_group = crp_grp,
    land_use_group = lnd_s_g,
    rz_in,
    awc_in_in,
    swsf,
    prcp_in,
    prcp_win_in,
    et_in,
    et_win_in
  ) |> 
  setDT()

depletion_data2 = depletion_data1 |> 
  # Calculate monthly effective precipitation
  mutate(peff_in = pmax(0, swsf * (0.70917 * prcp_in ^ 0.82416 - 0.11556) * 10 ^ (0.02426 * et_in))) |> 
  group_by(id, water_year) |> 
  # Calculate winter carryover soil moisture
  mutate(sm_co_in = pmax(0, pmin(0.67 * (prcp_win_in - 1.25 * et_win_in), 0.75 * rz_in * awc_in_in))) |> 
  ungroup() |> 
  setDT()

depletion_data3 = depletion_data2 |> 
  # Filter to April through October
  filter(month %in% c(4, 5, 6, 7, 8, 9, 10)) |> 
  group_by(id, water_year) |> 
  # Calculate total growing season effective precipitation
  summarize(peff_grow_in = sum(peff_in), .groups = "drop") |> 
  setDT()

# Join growing season effective precipitation with rest of data
masterdata = depletion_data2 |> 
  left_join(depletion_data3, by = c("id", "water_year")) |> 
  # Rejoin with land use and irrigation method
  left_join(
    fields_panel |> select(id, year), 
    by = c("id", "water_year" = "year"), 
    relationship = "many-to-one"
  ) |> 
  setDT()

# ==== SAVE ====================================================================

save(masterdata, file = "Data/Clean/Depletion/Utah/masterdata.rda")
