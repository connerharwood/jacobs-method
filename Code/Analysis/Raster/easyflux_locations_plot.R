
library(tidyverse)
library(sf)
library(tigris)
library(tmap)
tmap_mode("view")

# ==== LOAD ====================================================================

load("Data/Misc/EasyFlux vs. OpenET/easyflux_daily.rda")

utah_sf = states(cb = TRUE) |>
  filter(NAME == "Utah") |> 
  st_transform(4326)

# ==== PLOT ====================================================================

stations_sf = easyflux_daily |> 
  distinct(station, lat, lon) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

station_locations = ggplot() +
  geom_sf(data = utah_sf, fill = NA, alpha = 0.3, color = "black") +
  geom_sf(data = stations_sf, color = "red", size = 2) +
  geom_sf_text(data = stations_sf, aes(label = station), nudge_y = 0.1, size = 2, check_overlap = TRUE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "white", color = NA)    # White plot background
  )
station_locations

ggsave("Data/Misc/EasyFlux vs. OpenET/station_locations.png", station_locations)
