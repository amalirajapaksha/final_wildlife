library(ggplot2)
library(gganimate)
library(sf)
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

levels_order <- df_sf %>%
  distinct(year_month) %>%
  arrange(year_month) %>%
  mutate(
    year_month_label = format(
      as.Date(paste0(year_month, "-01")),
      "%Y %b"
    )
  )

df_sf <- df_sf %>%
  mutate(
    year_month_label = factor(
      format(as.Date(paste0(year_month, "-01")), "%Y %b"),
      levels = levels_order$year_month_label
    )
  )

sex_colors <- c(
  "Male" = "darkblue",
  "Female" = "darkred"
)

p <- ggplot(df_sf) +
  geom_sf(
    aes(color = sex),
    size = 2.5,
    alpha = 0.6
  ) +
  facet_wrap(~ name, ncol = 7) +
  coord_sf() +
  scale_color_manual(values = sex_colors) +
  labs(
    title = "{closest_state}",
    subtitle = "Elephant GPS locations",
    x = "Longitude",
    y = "Latitude",
    color = "Sex"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    legend.position = "bottom"
  ) +
  transition_states(
    year_month_label,
    transition_length = 1,
    state_length = 30,
    wrap = FALSE
  ) +
  ease_aes("linear")

animate(
  p,
  width = 1920,
  height = 1080,
  res = 120,
  fps = 3,
  end_pause = 15,
  renderer = gifski_renderer()
)


anim_save("elephants_tracking.gif")




