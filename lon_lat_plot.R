library(ggplot2) 
library(sf)
library(readr)
library(dplyr)

kaudulla_elephants_clean_imputed <- read_csv("kaudulla_elephants_clean.csv")

df_sf <- kaudulla_elephants_clean_imputed |>
  filter(!is.na(lon), !is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  mutate(
    year = format(datetime, "%Y"),
    month = format(datetime, "%m"),
    year_month = format(datetime, "%Y-%m")
  ) |>
  arrange(name, datetime)

# Colors for elephant tracking
elephant_colors <- c(
  "#e6194b", "#3cb44b", "#ffe119", "#4363d8", "#f58231",
  "#911eb4", "#46f0f0", "#f032e6", "#bcf60c", "#fabebe",
  "#008080", "#e6beff", "#9a6324", "#fffac8", "#800000"
)

ggplot() +
  geom_sf(
    data = df_sf,
    aes(color = factor(name)),
    size = 0.75,
    alpha = 0.5
  ) +
  theme_minimal() +
  scale_color_manual(
    values = elephant_colors,
    name = "Name"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 3, alpha = 1)
    )
  ) +
  labs(
    title = "Tracking Data",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.position = "right"
  )
