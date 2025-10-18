
library(tidyverse)
library(sf)
library(data.table)

# ==== LOAD ====================================================================

load("Data/Clean/Depletion/Utah/masterdata.rda")

# Load 2017-2024 Utah fields panel
fields_panel = st_read("Data/Clean/Fields/Utah/fields_panel.gpkg") |> 
  st_drop_geometry()

# ==== CALCULATE DEPLETION =====================================================

depletion_monthly = masterdata |> 
  filter(month %in% 4:10) |> 
  arrange(id, year, month) |> 
  group_by(id, year) |> 
  mutate(
    sm_start = pmax(0, sm_co_in[1] - cumsum(pmax(0, et_in - peff_in)) + pmax(0, et_in - peff_in)),
    
    sm_used = pmin(sm_start, pmax(0, et_in - peff_in)),
    
    sm_end = sm_start - sm_used,
    
    depletion_in = pmax(0, et_in - sm_start - peff_in),
    depletion_ft = depletion_in / 12,
    depletion_af = acres * depletion_ft
  ) |> 
  ungroup() |> 
  left_join(
    fields_panel,
    by = c("id", "year"),
    relationship = "many-to-one"
  )

depletion_annual = depletion_monthly |> 
  group_by(id, year) |> 
  summarize(
    depletion_in = sum(depletion_in, na.rm = FALSE), # Growing season depletion (inches)
    depletion_ft = sum(depletion_ft, na.rm = FALSE), # Growing season depletion (feet)
    depletion_af = sum(depletion_af, na.rm = FALSE), # Growing season depletion (acre-feet)
    .groups = "drop"
  )

median(depletion_annual$depletion_ft, na.rm = TRUE) # 1.493911
median(depletion_annual$depletion_af, na.rm = TRUE) # 7.462747

# ==== METHOD 3 ================================================================

tic()
depletion_monthly = masterdata[
  # Filter to growing season months
  month %in% 4:10
][
  order(id, year, month)
][, {
  # Initialize output vectors
  sm_start = numeric(.N)
  sm_end = numeric(.N)
  depletion_in = numeric(.N)
  depletion_ft = numeric(.N)
  
  # Soil moisture at start of irrigation season
  sm = sm_co_in[1]
  
  for (i in seq_len(.N)) {
    et = et_in[i]
    peff = peff_in[i]
    
    sm_start[i] = sm
    
    # depletion calculation
    depletion = max(0, et - sm - peff)
    sm_used = min(sm, max(0, et - peff))
    sm_new = max(0, sm - sm_used)
    
    sm_end[i] = sm_new
    depletion_in[i] = depletion
    depletion_ft[i] = depletion / 12
    
    sm = sm_new
  }
  
  .(month, acres, et_in, peff_in, sm_co_in, depletion_in, depletion_ft)
}, by = .(id, year)]
toc()
# 21.728 seconds

depletion_annual = depletion_monthly |> 
  mutate(
    # Convert depletion to feet and acre-feet
    depletion_ft = depletion_in / 12,
    depletion_af = acres * depletion_ft
  ) |> 
  group_by(id, year) |> 
  summarize(
    depletion_in = sum(depletion_in, na.rm = FALSE), # Growing season depletion (inches)
    depletion_ft = sum(depletion_ft, na.rm = FALSE), # Growing season depletion (feet)
    depletion_af = sum(depletion_af, na.rm = FALSE), # Growing season depletion (acre-feet)
    .groups = "drop"
  )

median(depletion_annual$depletion_ft, na.rm = TRUE) # 1.493911
median(depletion_annual$depletion_af, na.rm = TRUE) # 7.462747

# ==== SAVE ====================================================================

save(depletion_monthly, file = "Data/Clean/Depletion/Utah/depletion_monthly.rda")
save(depletion_annual, file = "Data/Clean/Depletion/Utah/depletion_annual.rda")
