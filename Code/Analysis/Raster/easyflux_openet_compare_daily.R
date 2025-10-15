
library(tidyverse)
library(scales)
library(forcats)

# ==== LOAD ====================================================================

load("Data/Misc/EasyFlux vs. OpenET/easyflux_daily.rda")
load("Data/Misc/EasyFlux vs. OpenET/openet_daily.rda")

# ==== PREP ====================================================================

compare_daily = left_join(
  easyflux_daily,
  openet_daily,
  by = c("station", "year", "month", "day")
) |> 
  mutate(
    date = as.Date(paste(year, month, day, sep = "-")),
    
    diff_eemetric = (et_mm_eemetric - et_mm_easyflux) / 25.4,
    diff_ensemble = (et_mm_ensemble - et_mm_easyflux) / 25.4,
  ) |> 
  select(
    station, 
    date, 
    year, 
    month, 
    day, 
    et_mm_easyflux, 
    et_mm_eemetric, 
    et_mm_ensemble, 
    diff_eemetric, 
    diff_ensemble
  ) |> 
  pivot_longer(
    cols = c(et_mm_easyflux, et_mm_eemetric, et_mm_ensemble, diff_eemetric, diff_ensemble),
    names_to = "variable",
    values_to = "value"
  )

# ==== PLOTS ===================================================================

# Daily ET difference by flux tower
daily_diff_plot = compare_daily |> 
  filter(str_detect(variable, "^diff_")) |> 
  mutate(
    method = case_when(
      variable == "diff_eemetric" ~ "eeMETRIC",
      variable == "diff_ensemble" ~ "Ensemble"
    )
  ) |> 
  ggplot(aes(x = date, y = value, color = method)) +
  geom_line(size = 0.7, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(
    values = c(
      "eeMETRIC" = "blue3",
      "Ensemble" = "red3"
    )
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand = expansion(mult = 0.01)
  ) +
  labs(
    title = "Daily ET Difference by Flux Tower", 
    x = NULL, 
    y = "ET Difference (inches)",
    color = "Model"
  ) +
  facet_wrap(~station, scales = "free_x") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey90"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(2, "lines")
  )
daily_diff_plot

# Daily ET value by method
daily_et_plot = compare_daily |> 
  filter(str_detect(variable, "^et_mm_")) |> 
  mutate(
    method = case_when(
      variable == "et_mm_easyflux" ~ "EasyFlux",
      variable == "et_mm_eemetric" ~ "eeMETRIC",
      variable == "et_mm_ensemble" ~ "Ensemble"
    ),
    # Convert mm to inches for plotting
    value_inches = value / 25.4
  ) |> 
  ggplot(aes(x = date, y = value_inches, color = method)) +
  geom_line(size = 0.7, alpha = 0.8) +
  scale_color_manual(
    values = c(
      "EasyFlux" = "black",
      "eeMETRIC" = "blue3", 
      "Ensemble" = "red3"
    )
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand = expansion(mult = 0.01)
  ) +
  labs(
    title = "Daily ET by Flux Tower", 
    x = NULL, 
    y = "ET (inches)",
    color = "Model"
  ) +
  facet_wrap(~station, scales = "free_x") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey90"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(2, "lines")
  )
daily_et_plot

# ==== SAVE PLOTS ==============================================================

ggsave("Data/Misc/EasyFlux vs. OpenET/daily_et_difference.png", daily_diff_plot, width = 16, height = 10, dpi = 300)
ggsave("Data/Misc/EasyFlux vs. OpenET/daily_et_value.png", daily_et_plot, width = 16, height = 10, dpi = 300)
