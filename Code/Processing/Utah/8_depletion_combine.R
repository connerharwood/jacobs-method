
library(tidyverse)
library(data.table)
library(glue)
library(sf)

# ==== LOAD ====================================================================

# OpenET models
et_models = c(
  "ensemble",
  "eemetric",
  "geesebal",
  "ssebop",
  "ptjpl",
  "disalexi",
  "sims"
)

# Loop through and load monthly and annual depletion data from each model
for (model in et_models) {
  load(glue("Data/Depletion/depletion_monthly_{model}.rda"))
  load(glue("Data/Depletion/depletion_annual_{model}.rda"))
}

# Load 2017-2023 Utah fields panel
fields_panel = st_read("Data/Utah Fields/fields_panel.shp") |> 
  st_drop_geometry()

# ==== MONTHLY =================================================================

# Create crosswalk of all fields, years, and months
crosswalk_monthly = expand_grid(
  id = unique(fields_panel$id),
  water_year = 2017:2023,
  month = 4:10
) |> 
  # Convert month numbers to factored labels
  mutate(month = factor(
    month, 
    levels = 4:10,
    labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))
  ) |> 
  # Join with yearly field data
  left_join(
    fields_panel |> select(id, year, acres, crop, irr_method),
    by = c("id", "water_year" = "year"),
    relationship = "many-to-one"
  ) |> 
  setDT()

depletion_monthly_all_models = crosswalk_monthly |> 
  left_join(
    depletion_monthly_ensemble |> select(
      id,
      water_year,
      month,
      et_in_ensemble = et_in,
      depletion_in_ensemble = depletion_in,
    ),
    by = c("id", "water_year", "month"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    depletion_monthly_eemetric |> select(
      id,
      water_year,
      month,
      et_in_eemetric = et_in,
      depletion_in_eemetric = depletion_in,
    ),
    by = c("id", "water_year", "month"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    depletion_monthly_geesebal |> select(
      id,
      water_year,
      month,
      et_in_geesebal = et_in,
      depletion_in_geesebal = depletion_in,
    ),
    by = c("id", "water_year", "month"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    depletion_monthly_ssebop |> select(
      id,
      water_year,
      month,
      et_in_ssebop = et_in,
      depletion_in_ssebop = depletion_in,
    ),
    by = c("id", "water_year", "month"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    depletion_monthly_ptjpl |> select(
      id,
      water_year,
      month,
      et_in_ptjpl = et_in,
      depletion_in_ptjpl = depletion_in,
    ),
    by = c("id", "water_year", "month"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    depletion_monthly_disalexi |> select(
      id,
      water_year,
      month,
      et_in_disalexi = et_in,
      depletion_in_disalexi = depletion_in,
    ),
    by = c("id", "water_year", "month"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    depletion_monthly_sims |> select(
      id,
      water_year,
      month,
      et_in_sims = et_in,
      depletion_in_sims = depletion_in,
    ),
    by = c("id", "water_year", "month"),
    relationship = "one-to-one"
  ) |> 
  setDT()

save(depletion_monthly_all_models, file = "Data/Depletion/depletion_monthly_all_models.rda")

# ==== ANNUAL ==================================================================

# Create crosswalk of all fields, years, and months
crosswalk_annual = expand_grid(
  id = unique(fields_panel$id),
  water_year = 2017:2023
) |>
  # Join with yearly field data
  left_join(
    fields_panel |> select(id, year, acres, crop, irr_method),
    by = c("id", "water_year" = "year"),
    relationship = "one-to-one"
  ) |> 
  setDT()

depletion_annual_all_models = crosswalk_annual |> 
  left_join(
    depletion_annual_ensemble |> select(
      id,
      water_year,
      et_in_ensemble = et_in,
      depletion_in_ensemble = depletion_in,
    ),
    by = c("id", "water_year"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    depletion_annual_eemetric |> select(
      id,
      water_year,
      et_in_eemetric = et_in,
      depletion_in_eemetric = depletion_in,
    ),
    by = c("id", "water_year"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    depletion_annual_geesebal |> select(
      id,
      water_year,
      et_in_geesebal = et_in,
      depletion_in_geesebal = depletion_in,
    ),
    by = c("id", "water_year"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    depletion_annual_ssebop |> select(
      id,
      water_year,
      et_in_ssebop = et_in,
      depletion_in_ssebop = depletion_in,
    ),
    by = c("id", "water_year"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    depletion_annual_ptjpl |> select(
      id,
      water_year,
      et_in_ptjpl = et_in,
      depletion_in_ptjpl = depletion_in,
    ),
    by = c("id", "water_year"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    depletion_annual_disalexi |> select(
      id,
      water_year,
      et_in_disalexi = et_in,
      depletion_in_disalexi = depletion_in,
    ),
    by = c("id", "water_year"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    depletion_annual_sims |> select(
      id,
      water_year,
      et_in_sims = et_in,
      depletion_in_sims = depletion_in,
    ),
    by = c("id", "water_year"),
    relationship = "one-to-one"
  ) |> 
  setDT()

save(depletion_annual_all_models, file = "Data/Depletion/depletion_annual_all_models.rda")
