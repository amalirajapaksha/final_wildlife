library(shiny)
library(shinyjs)
library(ggplot2)
library(sf)
library(dplyr)
library(readr)

# 1. Load and Preprocess Data
kaudulla_elephants_clean_imputed <- read_csv("kaudulla_elephants_clean.csv", show_col_types = FALSE)

all_elephant_names <- unique(kaudulla_elephants_clean_imputed$name)

df_sf_new <- kaudulla_elephants_clean_imputed |>
  filter(!is.na(lon), !is.na(lat)) |>
  mutate(
    name = factor(name, levels = all_elephant_names),
    sex = factor(sex, levels = c("Male", "Female")),
    year_month = format(datetime, "%Y-%m"),
    date_month = as.Date(paste0(year_month, "-01"))
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  arrange(name, datetime)

global_bbox <- st_bbox(df_sf_new)

active_months <- df_sf_new |>
  st_drop_geometry() |>
  distinct(date_month) |>
  arrange(date_month) |>
  pull(date_month)

# --- PRE-RENDERING ENGINE ---
img_dir <- file.path(tempdir(), "elephant_plots")
if (!dir.exists(img_dir)) dir.create(img_dir)

message("Pre-rendering plots...")
sex_colors <- c("Male" = "darkblue", "Female" = "darkred")

for (i in seq_along(active_months)) {
  m_date <- active_months[i]
  month_data <- df_sf_new |> filter(date_month == m_date)
  
  p <- ggplot(month_data) +
    geom_sf(
      aes(color = sex), 
      size = 4.5,          # Increased dot size for better visibility
      alpha = 0.8,
      show.legend = TRUE 
    ) +
    facet_wrap(~ name, ncol = 7, drop = FALSE) + 
    coord_sf(
      xlim = c(global_bbox["xmin"], global_bbox["xmax"]),
      ylim = c(global_bbox["ymin"], global_bbox["ymax"])
    ) +
    scale_color_manual(values = sex_colors, drop = FALSE) +
    labs(
      title = format(m_date, "%Y %b"),
      subtitle = "Elephant GPS locations",
      x = "Longitude", y = "Latitude", color = "Sex:"
    ) +
    # Boosted base size drastically to blow up text dimensions on saved files
    theme_minimal(base_size = 22) + 
    theme(
      plot.title = element_text(face = "bold", size = 32, hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 22, hjust = 0.5, color = "gray30", margin = margin(b = 10)),
      
      # Axis Labels and Coordinates
      axis.title = element_text(size = 20, face = "bold"),
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5, size = 16, face = "bold"),
      axis.text.y = element_text(size = 16, face = "bold"),
      
      # Elephant Names (Facet Strips)
      strip.text = element_text(size = 20, face = "bold", color = "black"), 
      strip.background = element_rect(fill = "#f0f2f5", color = NA),
      
      # Legend Amplification
      legend.position = "bottom",
      legend.title = element_text(size = 24, face = "bold"),
      legend.text = element_text(size = 22, face = "bold"),
      legend.key.size = unit(1.8, "cm"), 
      
      plot.margin = margin(10, 10, 10, 10)
    ) +
    # Make legend color icons larger & distinct
    guides(color = guide_legend(override.aes = list(size = 7)))
  
  # Adjusted dimensions and slightly dropped DPI to make everything appear dramatically larger relative to the frame
  ggsave(
    filename = file.path(img_dir, paste0("plot_", i, ".png")),
    plot = p, width = 20, height = 13, dpi = 96
  )
}
message("Pre-rendering complete!")


# 2. User Interface
ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  
  div(
    style = "padding: 8px 15px 0px 15px; display: flex; justify-content: space-between; align-items: center;",
    tags$h4("Kaudulla Elephant Tracking Timeline", style = "margin: 0; font-weight: bold; font-size: 1.4rem;"),
    
    div(
      style = "display: flex; align-items: center; gap: 15px; background-color: #f8f9fa; padding: 6px 14px; border-radius: 6px; border: 1px solid #e3e6f0;",
      div(
        style = "min-width: 100px; text-align: center;",
        tags$strong(textOutput("current_month_ui"), style = "font-size: 1.2rem; color: #2c3e50;")
      ),
      div(
        style = "display: flex; gap: 6px;",
        actionButton("btn_prev", "Back ⏮", class = "btn btn-sm btn-secondary", style = "padding: 4px 10px;"),
        actionButton("btn_toggle", "▶ Play", class = "btn btn-sm btn-success", style = "padding: 4px 14px;"), 
        actionButton("btn_next", "Next ⏭", class = "btn btn-sm btn-secondary", style = "padding: 4px 10px;")
      )
    )
  ),
  hr(style = "margin: 5px 0 8px 0;"),
  
  # Responsive container utilizing maximum screen estate
  div(
    style = "width: 100%; height: 85vh; display: flex; justify-content: center; align-items: center; overflow: hidden; padding: 0 5px;",
    imageOutput("elephant_plot", width = "auto", height = "100%")
  )
)


# 3. Server Logic
server <- function(input, output, session) {
  
  addResourcePath("pre_rendered", img_dir)
  
  current_idx <- reactiveVal(1)
  is_playing <- reactiveVal(FALSE)
  timer <- reactiveTimer(1000)
  
  observeEvent(input$btn_toggle, {
    is_playing(!is_playing())
    if (is_playing()) {
      updateActionButton(session, "btn_toggle", label = "⏸ Pause")
      removeClass("btn_toggle", "btn-success")
      addClass("btn_toggle", "btn-warning")
    } else {
      updateActionButton(session, "btn_toggle", label = "▶ Play")
      removeClass("btn_toggle", "btn-warning")
      addClass("btn_toggle", "btn-success")
    }
  })
  
  observe({
    if (!is_playing()) return() 
    timer()
    isolate({
      if (current_idx() < length(active_months)) {
        current_idx(current_idx() + 1)
      } else {
        current_idx(1)
      }
    })
  })
  
  observeEvent(input$btn_next, {
    if (current_idx() < length(active_months)) {
      current_idx(current_idx() + 1)
    }
  })
  
  observeEvent(input$btn_prev, {
    if (current_idx() > 1) {
      current_idx(current_idx() - 1)
    }
  })
  
  current_date <- reactive({
    active_months[current_idx()]
  })
  
  output$current_month_ui <- renderText({
    format(current_date(), "%Y %b")
  })
  
  output$elephant_plot <- renderImage({
    list(
      src = file.path(img_dir, paste0("plot_", current_idx(), ".png")),
      contentType = "image/png",
      alt = "Elephant Tracking Map",
      height = "100%",
      width = "auto"
    )
  }, deleteFile = FALSE)
}

shinyApp(ui = ui, server = server)