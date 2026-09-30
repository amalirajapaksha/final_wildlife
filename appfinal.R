library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(dplyr)
library(sf)
library(readr)
library(plotly)
library(leaflet)
library(htmltools)
library(yyjsonr)
library(shiny)
library(readxl)
library(lattice)
library(grid)
library(ggplot2)  
library(lubridate)
library(shinyjs)


#==============================================================
# 1. DATA 
#==============================================================


# ── Colour palette (one per elephant) ─────────────────────────────────────────
ELEPHANT_COLOURS <- c(
  Talatha             = "#E63946",
  Pazhani             = "#457B9D",
  `recollared female` = "#2A9D8F",
  Rahu                = "#F4A261",
  Kasun               = "#9B2226",
  Dona                = "#6A0572",
  Mina                = "#0096C7",
  Illuk               = "#52B788",
  Dewmi               = "#F77F00",
  Gothami             = "#CB4335",
  Wilmini             = "#1B4332",
  female_1            = "#B5838D",
  `Tara Devi`         = "#D4A017",
  Damien              = "#3D405B"
)

# ── Load data ──────────────────────────────────────────────────────────────────
DATA_PATH <- "kaudulla_elephants_clean.csv"

load_data <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  df$datetime    <- as.POSIXct(df$datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  df$datetime_sl <- with_tz(df$datetime, "Asia/Colombo")   # Sri Lanka time
  # FIX 1: derive the calendar date from datetime (old "%d/%m/%Y" -> all NA)
  df$date_parsed <- as.Date(df$datetime_sl)
  df <- df[!is.na(df$lat) & !is.na(df$lon), ]
  df
}
elephants_df <- load_data(DATA_PATH)

# Summary stats
n_obs      <- nrow(elephants_df)
n_animals  <- length(unique(elephants_df$name))
date_start <- format(min(elephants_df$date_parsed, na.rm = TRUE), "%d %b %Y")
date_end   <- format(max(elephants_df$date_parsed, na.rm = TRUE), "%d %b %Y")

# ── Month choices for the global "Month" filter (sorted chronologically) ────
month_lookup <- elephants_df %>%
  mutate(
    month_key   = format(date_parsed, "%Y-%m"),
    month_label = format(date_parsed, "%B %Y")
  ) %>%
  distinct(month_key, month_label) %>%
  arrange(month_key)

month_choices <- setNames(month_lookup$month_key, month_lookup$month_label)
month_choices <- c("All months" = "all", month_choices)

# ── Helper: split a track into segments at large gaps (for the MAP) ──────────
assign_track_segments <- function(d, time_col = "datetime_sl") {
  d <- d[order(d[[time_col]]), ]
  if (nrow(d) < 2) { d$seg <- 1L; return(d) }
  gaps <- as.numeric(difftime(d[[time_col]][-1], d[[time_col]][-nrow(d)], units = "secs"))
  med  <- median(gaps[gaps > 0], na.rm = TRUE)
  if (!is.finite(med)) med <- 3600
  thr  <- max(med * 4, 6 * 3600)
  d$seg <- c(1L, cumsum(gaps > thr) + 1L)
  d
}

# ── Helper: insert NA rows at large gaps (for the LINE CHARTS) ───────────────
# An NA row in the middle of a big gap makes plotly leave a blank space
# instead of joining across the missing period.
insert_gaps <- function(d, time_col = "datetime_sl", cols = c("lat", "lon")) {
  d <- d[order(d[[time_col]]), ]
  if (nrow(d) < 2) return(d)
  gaps <- as.numeric(difftime(d[[time_col]][-1], d[[time_col]][-nrow(d)], units = "secs"))
  med  <- median(gaps[gaps > 0], na.rm = TRUE)
  if (!is.finite(med)) med <- 3600
  big  <- which(gaps > max(med * 4, 6 * 3600))
  if (!length(big)) return(d)
  na_rows <- d[big, , drop = FALSE]
  na_rows[[time_col]] <- d[[time_col]][big] + gaps[big] / 2
  for (cc in cols) na_rows[[cc]] <- NA_real_
  d <- rbind(d, na_rows)
  d[order(d[[time_col]]), ]
}



kaudulla_elephants_clean_imputed <- read_csv("kaudulla_elephants_clean.csv")

df_sf <- kaudulla_elephants_clean_imputed |>
  filter(!is.na(lon), !is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  mutate(
    year = format(datetime, "%Y"),
    month = format(datetime, "%m"),
    year_month = format(datetime, "%Y-%m"),
    # Week-of-month, always 4 buckets (days 1-7, 8-14, 15-21, 22-end)
    week_of_month = factor(
      paste0("Week ", pmin(ceiling(day(datetime) / 7), 4)),
      levels = c("Week 1", "Week 2", "Week 3", "Week 4")
    )
  ) |>
  arrange(name, datetime)

# IMPORTANT: palette MUST be based on FULL dataset (not filtered)
pal <- colorFactor(
  palette = c(
    "red", # Red
    "#377EB8", # Blue
    "#4DAF4A", # Green
    "#984EA3", # Purple
    "#FF7F00", # Orange
    "#fffac8", # Yellow
    "#A65628", # Brown
    "#F781BF", # Pink
    "#17BECF", # Cyan
    "#000000", # Black
    "blue", # Sky Blue
    "#2F4F4F", # Lime Green
    "#FB9A99", # Light Red
    "#CAB2D6", # Lavender
    "#FDBF6F", # Light Orange
    "#6A3D9A", # Dark Purple
    "#B2DF8A", # Light Green
    "#FF1493", # Deep Pink
    "#00CED1", # Dark Turquoise
    "#FFD000"  # Gold
  ),
  domain = df_sf$year_month
)

# ── Week-of-month colour palette (4 fixed, highly-distinct colours) ──────────
# Since only one month is selectable at a time, there are always at most
# 4 weeks on screen (days 1-7, 8-14, 15-21, 22-end) - one clear colour each.
week_of_month_colors <- c(
  "Week 1" = "#e6194b",  # red
  "Week 2" = "#3cb44b",  # green
  "Week 3" = "#4363d8",  # blue
  "Week 4" = "#f58231"   # orange
)

pal_week <- colorFactor(
  palette = unname(week_of_month_colors),
  domain  = names(week_of_month_colors)
)
elephants <- sort(unique(df_sf$name))

# Colors for elephant tracking
elephant_colors <- c(
  "#e6194b", "#3cb44b", "#ffe119", "#4363d8", "#f58231",
  "#911eb4", "#46f0f0", "#f032e6", "#bcf60c", "#fabebe",
  "#008080", "#e6beff", "#9a6324", "#fffac8", "#800000"
)



kaudulla_elephants_clean_imputed <- read_csv("kaudulla_elephants_clean.csv", show_col_types = FALSE)

all_elephant_names <- unique(kaudulla_elephants_clean_imputed$name)

df_sf_new <- kaudulla_elephants_clean_imputed |>
  filter(!is.na(lon), !is.na(lat)) |>
  mutate(
    name = factor(name, levels = all_elephant_names),
    # Ensure sex is a factor with both levels present
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





#==============================================================
# 2. CALENDAR HEATMAP FUNCTION
#==============================================================

calendarHeat <- function(dates, 
                         values,
                         colors,
                         at = NULL,           
                         ncolors=99,
                         title,
                         date.form = "%Y-%m-%d", 
                         colorkey = FALSE,      
                         legend = NULL,         
                         ...) {
  require(lattice, quietly = TRUE)
  require(grid, quietly = TRUE)
  
  if (inherits(dates, c("character", "factor"))) {
    dates <- strptime(dates, date.form)
  }
  caldat <- data.frame(value = values, dates = dates)
  min.date <- as.Date(paste(format(min(dates), "%Y"), "-1-1", sep = ""))
  max.date <- as.Date(paste(format(max(dates), "%Y"), "-12-31", sep = ""))
  
  caldat <- data.frame(date.seq = seq(min.date, max.date, by="days"), value = NA)
  dates <- as.Date(dates) 
  caldat$value[match(dates, caldat$date.seq)] <- values
  
  caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
  caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1
  caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
  caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
  yrs <- as.character(unique(caldat$yr))
  d.loc <- as.numeric()                        
  for (m in min(yrs):max(yrs)) {
    d.subset <- which(caldat$yr == m)  
    sub.seq <- seq(1, length(d.subset))
    d.loc <- c(d.loc, sub.seq)
  }  
  caldat <- cbind(caldat, seq=d.loc)
  
  if (!is.null(at)) {
    if (missing(colors)) {
      colors <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")
      calendar.pal <- colorRampPalette(colors, space = "Lab")(length(at) - 1)
    } else {
      if(length(colors) == (length(at) - 1)) {
        calendar.pal <- colors
      } else {
        calendar.pal <- colorRampPalette(colors, space = "Lab")(length(at) - 1)
      }
    }
    my.cuts <- NULL 
  } else {
    if (missing(colors)) colors <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")
    calendar.pal <- colorRampPalette(colors, space = "Lab")(ncolors)
    my.cuts <- ncolors - 1
  }
  
  def.theme <- lattice.getOption("default.theme")
  cal.theme <- function() {  
    list(
      strip.background = list(col = "transparent"),
      strip.border = list(col = "transparent"),
      axis.line = list(col="transparent"),
      par.strip.text=list(cex=0.8))
  }
  lattice.options(default.theme = cal.theme)
  yrs <- (unique(caldat$yr))
  nyr <- length(yrs)
  #==============================================================
  # PART 2: PLOT RENDERING & POST-GRAPHICS REGION FOCUS
  #==============================================================
  print(cal.plot <- levelplot(value~woty*dotw | yr, data=caldat,
                              as.table=TRUE,
                              aspect=.14,
                              layout = c(1, nyr%%7),
                              between = list(x=0, y=c(1,1)),
                              strip=TRUE,
                              main = ifelse(missing(title), "", title),
                              scales = list(
                                x = list(
                                  at= c(seq(2.9, 52, by=4.42)),
                                  labels = month.abb,
                                  alternating = c(1, rep(0, (nyr-1))),
                                  tck=0,
                                  cex = 1.1),
                                y=list(
                                  at = c(0, 1, 2, 3, 4, 5, 6),
                                  labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                                             "Friday", "Saturday"),
                                  alternating = 1,
                                  cex = 0.85,
                                  tck=0)),
                              xlim = c(0.4, 54.6),
                              ylim = c(6.6,-0.6),
                              at = at,                 
                              cuts = my.cuts,           
                              col.regions = calendar.pal, 
                              xlab="" ,
                              ylab="",
                              colorkey = colorkey,    
                              legend = legend,        
                              subscripts=TRUE
  ) )
  
  panel.locs <- trellis.currentLayout()
  for (row in 1:nrow(panel.locs)) {
    for (column in 1:ncol(panel.locs))  {
      if (panel.locs[row, column] > 0) {
        trellis.focus("panel", row = row, column = column, highlight = FALSE)
        xyetc <- trellis.panelArgs()
        subs <- caldat[xyetc$subscripts,]
        dates.fsubs <- caldat[caldat$yr == unique(subs$yr),]
        y.start <- dates.fsubs$dotw[1]
        y.end   <- dates.fsubs$dotw[nrow(dates.fsubs)]
        dates.len <- nrow(dates.fsubs)
        adj.start <- dates.fsubs$woty[1]
        
        for (k in 0:6) {
          if (k < y.start) { x.start <- adj.start + 0.5 } else { x.start <- adj.start - 0.5 }
          if (k > y.end) { x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5 } else { x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5 }
          grid.lines(x = c(x.start, x.finis), y = c(k -0.5, k - 0.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        if (adj.start <  2) {
          grid.lines(x = c( 0.5,  0.5), y = c(6.5, y.start-0.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
          grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
          grid.lines(x = c(x.finis, x.finis), y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
          if (dates.fsubs$dotw[dates.len] != 6) {
            grid.lines(x = c(x.finis + 1, x.finis + 1), y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
          }
          grid.lines(x = c(x.finis, x.finis), y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        for (n in 1:51) {
          grid.lines(x = c(n + 1.5, n + 1.5), y = c(-0.5, 6.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        x.start <- adj.start - 0.5
        
        if (y.start > 0) {
          grid.lines(x = c(x.start, x.start + 1), y = c(y.start - 0.5, y.start -  0.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start + 1, x.start + 1), y = c(y.start - 0.5 , -0.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start, x.start), y = c(y.start - 0.5, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          if (y.end < 6  ) {
            grid.lines(x = c(x.start + 1, x.finis + 1), y = c(-0.5, -0.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis), y = c(6.5, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.start + 1, x.finis), y = c(-0.5, -0.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis), y = c(6.5, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          }
        } else {
          grid.lines(x = c(x.start, x.start), y = c( - 0.5, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
        }
        
        if (y.start == 0 ) {
          if (y.end < 6  ) {
            grid.lines(x = c(x.start, x.finis + 1), y = c(-0.5, -0.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis), y = c(6.5, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.start + 1, x.finis), y = c(-0.5, -0.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis), y = c(6.5, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          }
        }
        for (j in 1:12)  {
          last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
          x.last.m <- dates.fsubs$woty[last.month] + 0.5
          y.last.m <- dates.fsubs$dotw[last.month] + 0.5
          grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, y.last.m), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          if ((y.last.m) < 6) {
            grid.lines(x = c(x.last.m, x.last.m - 1), y = c(y.last.m, y.last.m), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.last.m - 1, x.last.m - 1), y = c(y.last.m, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.last.m, x.last.m), y = c(- 0.5, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          }
        }
      }
    }
    trellis.unfocus()
  } 
  lattice.options(default.theme = def.theme)
}

# ==============================================================================
# 3. GLOBAL SETTINGS & DATA INGESTION
# ==============================================================================
elephants <- read.csv("kaudulla_elephants_clean.csv")
elephants$datetime <- ymd_hms(elephants$datetime)
elephants$date <- as.Date(elephants$datetime)

elephant_names <- unique(elephants$name)

my_colors2 <- c("#D9D9D9", "#FEE08B", "#D9EF8B", "#91CF60", "#4DAC26", "#006400")
my_ranges <- c(0, 1, 25, 50, 75, 99.999, 100)
range_labels <- c("0 %", "1 - 25 %", "25 - 50 %", "50 - 75 %", "75 - 99 %", "100 %")

discrete_key <- list(
  space = "right",
  rectangles = list(col = my_colors2, border = "black", size = 4),
  text = list(range_labels, cex = 1.1),
  padding.text = 4,
  columns = 1
)



#==============================================================
# 4. CLIMATE DATA
#==============================================================

climate <- read_excel("daily_climate.xlsx")
climate$date <- as.Date(climate$date, origin = "1899-12-30")
dates <- climate$date

my_colors <- c(
  "#FFFF99", "#FFCC66", "#F5B27A", "#FF6F91", 
  "#9966CC", "#330066", "#000000"
)

plot_info <- list(
  "Solar Radiation" = list(
    values = climate$solar_radiation,
    breaks = c(0,15,20,22,23.5,25,26.5,28),
    labels = c("0-15", "15-20", "20-22", "22-23.5", "23.5-25", "25-26.5", "26.5-28"),
    title = "Daily Solar Radiation Calendar Heatmap"
  ),
  "Rainfall" = list(
    values = climate$rainfall,
    breaks = c(0,0.3,0.6,1.5,2.5,4,10,55),
    labels = c("0-0.3", "0.3-0.6", "0.6-1.5", "1.5-2.5", "2.5-4", "4-10", "10-55"),
    title = "Daily Rainfall Calendar Heatmap"
  ),
  "Pressure" = list(
    values = climate$pressure,
    breaks = c(0,100.8,101.0,101.1,101.2,101.3,101.4,101.5),
    labels = c("0-100.8", "100.8-101", "101-101.1", "101.1-101.2", "101.2-101.3", "101.3-101.4", "101.4-101.5"),
    title = "Daily Pressure Calendar Heatmap"
  ),
  "Maximum Temperature" = list(
    values = climate$temp_max,
    breaks = c(0,26.7,27.4,28.1,28.8,29.5,30.2,31),
    labels = c("0-26.7", "26.7-27.4", "27.4-28.1", "28.1-28.8", "28.8-29.5", "29.5-30.2", "30.2-31"),
    title = "Daily Maximum Temperature Calendar Heatmap"
  ),
  "Earth Skin Temperature" = list(
    values = climate$temp_skin,
    breaks = c(0,27.2,27.9,28.6,29.3,30,30.7,31.5),
    labels = c("0-27.2", "27.2-27.9", "27.9-28.6", "28.6-29.3", "29.3-30", "30-30.7", "30.7-31.5"),
    title = "Daily Earth Skin Temperature Calendar Heatmap"
  ),
  "Wind Speed" = list(
    values = climate$wind_speed,
    breaks = c(0,2,4,5,6,7,8,10),
    labels = c("0-2", "2-4", "4-5", "5-6", "6-7", "7-8", "8-10"),
    title = "Daily Wind Speed Calendar Heatmap"
  ),
  "Maximum Wind Speed" = list(
    values = climate$wind_speed_max,
    breaks = c(0,3,5,6,7,8,9,11),
    labels = c("0-3", "3-5", "5-6", "6-7", "7-8", "8-9", "9-11"),
    title = "Daily Maximum Wind Speed Calendar Heatmap"
  )
)



#==============================================================
# 5. UI
#==============================================================
# ==============================================================
# 4B. HOME RANGE / MOVEMENT MODULE DATA (from app (6).R)
# ==============================================================

mcp_tracking_data <- read.csv(DATA_PATH)
mcp_tracking_data$lat <- as.numeric(mcp_tracking_data$lat)
mcp_tracking_data$lon <- as.numeric(mcp_tracking_data$lon)
mcp_tracking_data$datetime <- as.POSIXct(mcp_tracking_data$datetime, tz = "UTC")

mcp_tracking_clean <- mcp_tracking_data %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  arrange(name, datetime)

mcp_unique_elephants <- sort(unique(mcp_tracking_clean$name))
mcp_unique_sexes <- sort(unique(mcp_tracking_clean$sex))

mcp_palette_colors <- c(
  "#FF5252", "#2196F3", "#4CAF50", "#FFC107", "#9C27B0",
  "#FF9800", "#00BCD4", "#8BC34A", "#E91E63", "#3F51B5",
  "#795548", "#607D8B", "#CDDC39", "#F44336"
)
mcp_elephant_colors <- setNames(
  mcp_palette_colors[seq_along(mcp_unique_elephants)],
  mcp_unique_elephants
)

mcp_min_date <- as.Date(min(mcp_tracking_clean$datetime, na.rm = TRUE))
mcp_max_date <- as.Date(max(mcp_tracking_clean$datetime, na.rm = TRUE))

# ── Helper functions ──────────────────────────────────────────────────────────

mcp_shoelace_area <- function(lons, lats) {
  n <- length(lons)
  area <- 0
  for (i in 1:n) {
    j <- ifelse(i == n, 1, i + 1)
    area <- area + lons[i] * lats[j] - lons[j] * lats[i]
  }
  abs(area) / 2
}

mcp_haversine_km <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

mcp_compute_bearing <- function(lat1, lon1, lat2, lon2) {
  to_rad <- pi / 180
  dlon <- (lon2 - lon1) * to_rad
  lat1r <- lat1 * to_rad
  lat2r <- lat2 * to_rad
  x <- sin(dlon) * cos(lat2r)
  y <- cos(lat1r) * sin(lat2r) - sin(lat1r) * cos(lat2r) * cos(dlon)
  bearing <- atan2(x, y) / to_rad
  (bearing + 360) %% 360
}

mcp_compute_hull <- function(df, ratio = 0.3) {
  # ratio: 0 = tightest/most concave fit, 1 = same as a convex hull
  # 0.2-0.4 tends to look close to a "natural" home-range boundary; tune to taste
  if (nrow(df) < 3) {
    return(NULL)
  }
  
  pts_sf <- df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  hull_sf <- tryCatch(
    st_concave_hull(st_union(pts_sf), ratio = ratio, allow_holes = FALSE),
    error = function(e) NULL
  )
  
  # fallback to convex hull if GEOS on this machine is too old for concave hulls
  if (is.null(hull_sf) || length(hull_sf) == 0 || st_is_empty(hull_sf)) {
    hull_sf <- st_convex_hull(st_union(pts_sf))
  }
  
  coords <- st_coordinates(hull_sf)
  hull_lons <- coords[, "X"]
  hull_lats <- coords[, "Y"]
  
  # geodesic area straight from sf (accounts for lat/lon curvature properly,
  # more accurate than the shoelace + flat cos(lat) approximation)
  area_km2 <- as.numeric(st_area(hull_sf)) / 1e6
  
  list(lons = hull_lons, lats = hull_lats, area_km2 = area_km2)
}

mcp_add_movement_metrics <- function(df) {
  df %>%
    arrange(name, datetime) %>%
    group_by(name) %>%
    mutate(
      prev_lat  = lag(lat),
      prev_lon  = lag(lon),
      prev_time = lag(datetime),
      step_km   = mcp_haversine_km(prev_lat, prev_lon, lat, lon),
      hours     = as.numeric(difftime(datetime, prev_time, units = "hours")),
      speed_kmh = ifelse(hours > 0, step_km / hours, NA_real_),
      bearing   = mcp_compute_bearing(prev_lat, prev_lon, lat, lon)
    ) %>%
    ungroup() %>%
    select(-prev_lat, -prev_lon, -prev_time)
}

mcp_tracking_clean <- mcp_add_movement_metrics(mcp_tracking_clean)

mcp_bin_bearings <- function(bearings, n_bins = 16) {
  bin_width <- 360 / n_bins
  bin_labels <- seq(0, 360 - bin_width, by = bin_width)
  bins <- cut(
    bearings,
    breaks = c(bin_labels, 360),
    labels = bin_labels,
    include.lowest = TRUE,
    right = FALSE
  )
  counts <- table(factor(bins, levels = as.character(bin_labels)))
  data.frame(theta = as.numeric(names(counts)), r = as.numeric(counts))
}

mcp_make_rose_plot <- function(bearings, color, name) {
  bd <- mcp_bin_bearings(bearings[is.finite(bearings)])
  plot_ly(
    bd,
    type = "barpolar",
    r = ~r, theta = ~theta,
    marker = list(color = color, line = list(color = "#ffffff", width = 0.5)),
    hovertemplate = paste0("<b>", name, "</b><br>%{theta}°: %{r} fixes<extra></extra>")
  ) %>%
    layout(
      polar = list(
        angularaxis = list(
          tickmode  = "array",
          tickvals  = c(0, 45, 90, 135, 180, 225, 270, 315),
          ticktext  = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
          direction = "clockwise",
          rotation  = 90,
          gridcolor = "rgba(0,0,0,0.1)",
          linecolor = "rgba(0,0,0,0.15)"
        ),
        radialaxis = list(
          gridcolor = "rgba(0,0,0,0.08)",
          linecolor = "rgba(0,0,0,0.1)",
          tickfont  = list(color = "#666", size = 9)
        ),
        bgcolor = "rgba(0,0,0,0)"
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      font = list(color = "#333333"),
      showlegend = FALSE,
      margin = list(l = 20, r = 20, t = 30, b = 20),
      title = list(text = name, font = list(size = 12, color = "#333333"))
    ) %>%
    config(displayModeBar = FALSE)
}

ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/1/11/Flag_of_Sri_Lanka.svg",
               height = "22px", style = "margin-right:8px; vertical-align:middle;"),
      "Kaudulla Elephant Tracker"
    ),
    titleWidth = 320
  ),
  
  dashboardSidebar(
    width = 270,
    
    tags$div(
      style = "padding:14px 16px 6px; color:#ccc; font-size:12px; line-height:1.5;",
      tags$b("Kaudulla National Park"), tags$br(),
      "North Central Province, Sri Lanka", tags$br(),
      "8\u00B008\u2032N  80\u00B054\u2032E", tags$br(),
      tags$hr(style = "border-color:#444; margin:8px 0;"),
      tags$i("GPS Collar Monitoring Programme"), tags$br(),
      tags$a("Wildlife Department of Sri Lanka",
             href = "https://wildlife.gov.lk", target = "_blank",
             style = "color:#8bc34a;"),
      tags$hr(style = "border-color:#444; margin:8px 0;")
    ),
    
    sidebarMenu(
      menuItem("Latitude vs Time",   tabName = "lat_tab",  icon = icon("chart-line")),
      menuItem("Longitude vs Time",  tabName = "lon_tab",  icon = icon("chart-line")),
      menuItem("Both Coordinates",   tabName = "both_tab", icon = icon("layer-group")),
      menuItem("Heat Maps",          tabName = "heat_tab", icon = icon("fire")),
      menuItem("Elephant Tracking", tabName = "tracking_tab",icon = icon("map")),
      menuItem("Live Elephant Path", tabName = "live_tab", icon = icon("play")),
      menuItem("Migration & Climate", tabName = "climate_tab",icon = icon("globe")),
      menuItem("Data Table",         tabName = "data_tab", icon = icon("table")),
      menuItem("Home Range & Speed", tabName = "mcp_tab",  icon = icon("compass"))
    ),
    
    tags$hr(style = "border-color:#444; margin:4px 0;"),
    
    tags$div(style = "padding:0 16px;",
             selectInput(
               "sel_elephants", "Select Elephants",
               choices  = sort(unique(elephants_df$name)),
               selected = sort(unique(elephants_df$name)),
               multiple = TRUE
             ),
             
             tags$div(style = "display:flex; gap:4px; margin:-4px 0 8px;",
                      actionButton("btn_all",   "All",     class = "btn-xs", style = "flex:1; font-size:11px;"),
                      actionButton("btn_female","Females", class = "btn-xs", style = "flex:1; font-size:11px;"),
                      actionButton("btn_male",  "Males",   class = "btn-xs", style = "flex:1; font-size:11px;"),
                      actionButton("btn_clear", "Clear",   class = "btn-xs", style = "flex:1; font-size:11px;")
             ),
             
             tags$hr(style = "border-color:#444; margin:4px 0;"),
             
             dateRangeInput(
               "date_range", "Date Range",
               start = min(elephants_df$date_parsed, na.rm = TRUE),
               end   = max(elephants_df$date_parsed, na.rm = TRUE),
               min   = min(elephants_df$date_parsed, na.rm = TRUE),
               max   = max(elephants_df$date_parsed, na.rm = TRUE),
               format = "dd M yyyy"
             ),
             
             selectInput(
               "sel_month", "Month",
               choices  = month_choices,
               selected = "all"
             ),
             
             checkboxInput("add_smooth", "Add LOESS smoother", value = FALSE),
             
             tags$hr(style = "border-color:#444; margin:4px 0;"),
             
             radioButtons(
               "agg_level", "Time Resolution",
               choices  = c("Raw (hourly)" = "raw",
                            "Daily mean"   = "day",
                            "Weekly mean"  = "week"),
               selected = "raw",
               inline   = FALSE
             )
    ),
    
    tags$div(
      style = "padding:10px 16px 4px; font-size:10px; color:#888; line-height:1.4;",
      tags$b("Key Literature"), tags$br(),
      "Fernando et al. (2008)", tags$br(),
      "Pastorini et al. (2010)", tags$br(),
      "Ratnayeke et al. (2023)"
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("

/*====================================================
  GLOBAL
====================================================*/
body{
  background:#f5f7fb;
  font-family:'Segoe UI',system-ui,sans-serif;
}

.content-wrapper,
.right-side{
  background:#f5f7fb;
}

/*====================================================
  BOXES
====================================================*/

.box{
  background:white;
  border:none;
  border-radius:14px;
  box-shadow:0 2px 12px rgba(0,0,0,.08);
}

.box-header{
  background:white;
  color:#1e293b;
  border-bottom:1px solid #e5e7eb;
  border-radius:14px 14px 0 0;
}

.box-title{
  color:#0f766e;
  font-size:18px;
  font-weight:700;
}

/*====================================================
  SIDEBAR
====================================================*/

.main-sidebar{
  background:#ffffff;
  border-right:1px solid #e5e7eb;
}

.sidebar-menu>li>a{
    color: #ffffff !important;   /* white text */
  font-weight: 700 !important; /* bold */
  font-size:14px;
  font-weight:500;
  border-radius:10px;
  margin:5px 10px;
}

.sidebar-menu>li>a:hover{
  background:#ecfeff !important;
  color:#0f766e !important;
}

.sidebar-menu>li.active>a{
  background:#0f766e !important;
  color:white !important;
}

.sidebar-menu>li.header{
  color:#64748b !important;
}

/*====================================================
  HEADER
====================================================*/

.main-header .logo{
  background:#0f766e !important;
  color:white !important;
  font-weight:bold;
}

.main-header .navbar{
  background:#0f766e !important;
}

/*====================================================
  VALUE BOXES
====================================================*/

.small-box{
  border-radius:14px;
  box-shadow:0 3px 12px rgba(0,0,0,.08);
}

.info-box{
  background:white;
  border-radius:14px;
  box-shadow:0 3px 10px rgba(0,0,0,.06);
}

/*====================================================
  TEXT
====================================================*/

h4.ref-heading{
  color:#0f766e;
  font-size:15px;
  font-weight:700;
}

p.ref-text{
  color:#475569;
  font-size:13px;
  line-height:1.7;
}

.shiny-text-output{
  color:#334155;
}

/*====================================================
  SECTION TITLES
====================================================*/

.section-title{
  font-size:32px;
  font-weight:800;
  color:#1e293b;
  text-align:center;
  margin:30px 0 20px;
  letter-spacing:-0.5px;
}

.sub-title{
  font-size:24px;
  font-weight:700;
  color:#334155;
  text-align:center;
  margin:25px 0 15px;
}

.section-description{
  font-size:15px;
  color:#64748b;
  text-align:center;
  line-height:1.7;
  max-width:900px;
  margin:0 auto 25px auto;
}

.section-box{
  background:white;
  border-radius:14px;
  padding:20px;
  margin-bottom:25px;
  box-shadow:0 2px 12px rgba(0,0,0,.08);
}


/*====================================================
  SIDEBAR PANEL (inside sidebarLayout)
====================================================*/

.well{
  background:#0f766e !important;
  border:none !important;
  border-radius:12px;
  color:white !important;
  box-shadow:0 3px 10px rgba(0,0,0,.15);
}

.well h4,
.well h5,
.well label,
.well p,
.well .help-block{
  color:white !important;
  font-weight:600;
}

/* SelectInput */

.selectize-control.single .selectize-input{
  background:white !important;
  color:#0f766e !important;
  border-radius:8px;
  border:none;
}

.selectize-dropdown{
  border-radius:8px;
}

.selectize-dropdown .option{
  color:#1e293b;
}

.selectize-dropdown .active{
  background:#0f766e !important;
  color:white !important;
}

/* Table inside sidebar */

.well table{
  color:white;
}

.well td,
.well th{
  color:white !important;
}

/* Horizontal line */

.well hr{
  border-top:1px solid rgba(255,255,255,.35);
}

/* Action buttons */

.well .btn{
  background:white;
  color:#0f766e;
  border:none;
  border-radius:8px;
}

.well .btn:hover{
  background:#ecfdf5;
}

/* Numeric inputs */

.well input{
  border-radius:8px;
}

/*====================================================
  TABLES
====================================================*/

.dataTables_wrapper{
  color:#334155;
}

.dataTables_wrapper .dataTables_filter input{
  background:white;
  border:1px solid #cbd5e1;
  border-radius:8px;
  color:#334155;
}

table.dataTable{
  border-collapse:collapse;
}

table.dataTable tbody tr{
  background:white !important;
}

table.dataTable tbody tr:nth-child(even){
  background:#f8fafc !important;
}

table.dataTable tbody tr:hover{
  background:#ecfeff !important;
}

table.dataTable td{
  padding:10px;
}

/*====================================================
  BUTTONS
====================================================*/

.btn{
  border-radius:8px;
}

.btn-default{
  background:white;
  border:1px solid #cbd5e1;
}

.btn-default:hover{
  background:#ecfeff;
}

/*====================================================
  INPUTS
====================================================*/

.form-control{
  border-radius:8px;
  border:1px solid #cbd5e1;
}

/*====================================================
  LEAFLET
====================================================*/

.leaflet-container{
  border-radius:12px;
  border:1px solid #dbe4ee;
}

/* Numbered reference badges — shared between the reference map and the
   Key Coordinates table below it, so each map feature can be matched
   to its exact row at a glance. */
.ref-badge{
  display:inline-flex; align-items:center; justify-content:center;
  width:20px; height:20px; border-radius:50%;
  font-size:11px; font-weight:700; color:#ffffff;
  box-shadow:0 0 0 2px rgba(255,255,255,0.9), 0 1px 3px rgba(0,0,0,0.35);
  line-height:1;
}
.ref-badge.core{ background:#0f766e; }
.ref-badge.boundary{ background:#c1440e; }

.leaflet-div-badge{ background:transparent !important; border:none !important; }

/*====================================================
  PLOTLY
====================================================*/

.js-plotly-plot,
.plotly,
.plot-container {
  background: #ffffff !important;   /* clean white */
  border-radius: 12px;
}

/* if plots are inside boxes */
.box-body {
  background: #ffffff !important;
}

/*====================================================
  SCROLLBAR
====================================================*/

::-webkit-scrollbar{
  width:8px;
}

::-webkit-scrollbar-thumb{
  background:#94a3b8;
  border-radius:5px;
}

::-webkit-scrollbar-track{
  background:#f1f5f9;
}


"))
    ),
    
    tabItems(
      
      # ── TAB 1 : Latitude vs Time ─────────────────────────────────────────────
      tabItem("lat_tab",
              fluidRow(
                valueBoxOutput("vbox_obs",   width = 3),
                valueBoxOutput("vbox_eleph", width = 3),
                valueBoxOutput("vbox_start", width = 3),
                valueBoxOutput("vbox_end",   width = 3)
              ),
              fluidRow(
                box(
                  title = "\U0001F4CD Latitude vs Time — GPS Collar Data, Kaudulla National Park",
                  width = 12, solidHeader = TRUE,
                  tags$p(
                    style = "color:#666; font-size:11px; margin-bottom:6px;",
                    "Kaudulla elephants range roughly between latitudes 8.10\u00B0N and 8.25\u00B0N.",
                    "The park boundary lies around 8.08\u00B0\u20138.22\u00B0N (Fernando et al. 2008).",
                    "Northward movement often corresponds to the seasonal arrival at",
                    "Kaudulla tank when Minneriya dries (Ratnayeke et al. 2023)."
                  ),
                  plotlyOutput("plot_lat", height = "460px")
                )
              ),
              fluidRow(
                box(
                  title = "\U0001F5FA Live Position — Synced with Latitude Chart",
                  width = 12, solidHeader = TRUE,
                  tags$p(
                    style = "color:#666; font-size:11px; margin-bottom:6px;",
                    "Hover over a point on the Latitude vs Time chart above:",
                    "the map below jumps to that elephant's position at that",
                    "moment and draws the path travelled up to it, so you can",
                    "see exactly where — and in which direction — it moved."
                  ),
                  leafletOutput("sync_map_lat", height = "420px")
                )
              ),
              fluidRow(
                box(
                  title = "\U0001F4DA Literature Context — Latitude & Elephant Ranging in Sri Lanka",
                  width = 12, solidHeader = TRUE,
                  tags$div(style = "display:flex; flex-wrap:wrap; gap:20px; padding:4px 0;",
                           
                           tags$div(style = "flex:1; min-width:220px;",
                                    tags$h4("Fernando et al. (2008)", class = "ref-heading"),
                                    tags$p("Home ranges of Sri Lankan elephants averaged 46\u2013103 km\u00B2,
                        with latitudinal movement of 0.1\u00B0\u20130.3\u00B0 correlated with
                        seasonal tank water levels in the dry zone.", class = "ref-text")
                           ),
                           tags$div(style = "flex:1; min-width:220px;",
                                    tags$h4("Ratnayeke et al. (2023)", class = "ref-heading"),
                                    tags$p("Kaudulla\u2013Minneriya corridor study showed elephants shift
                        northward (higher latitude) into Kaudulla from May\u2013October
                        when the Minneriya tank partially dries, with peak
                        aggregations in August\u2013September.", class = "ref-text")
                           ),
                           tags$div(style = "flex:1; min-width:220px;",
                                    tags$h4("Wildlife Department of Sri Lanka", class = "ref-heading"),
                                    tags$p("The Department's GPS collar programme in Kaudulla National Park
                        (est. 2002, 6,900 ha) monitors movement to inform HEC
                        (Human\u2013Elephant Conflict) mitigation and corridor management.",
                                           class = "ref-text"),
                                    tags$a("wildlife.gov.lk", href = "https://wildlife.gov.lk",
                                           target = "_blank", style = "color:#2e7d32; font-size:11px;")
                           ),
                           tags$div(style = "flex:1; min-width:220px;",
                                    tags$h4("Geographic Reference", class = "ref-heading"),
                                    tags$p("Latitude 8.10\u00B0\u20138.25\u00B0N (WGS84). Kaudulla tank (reservoir)
                        at ~8.14\u00B0N is a key dry-season water source. The Mahaweli
                        River floodplain at ~8.22\u00B0N forms the northern park boundary.",
                                           class = "ref-text")
                           )
                  )
                )
              )
      ),
      
      # ── TAB 2 : Longitude vs Time ────────────────────────────────────────────
      tabItem("lon_tab",
              fluidRow(
                box(
                  title = "\U0001F4CD Longitude vs Time — GPS Collar Data, Kaudulla National Park",
                  width = 12, solidHeader = TRUE,
                  tags$p(
                    style = "color:#666; font-size:11px; margin-bottom:6px;",
                    "Longitudes span 80.87\u00B0\u201380.96\u00B0E. The Kaudulla tank lies near 80.90\u00B0E.",
                    "Elephants moving eastward (higher longitude) approach the park's",
                    "eastern boundary, which borders agricultural land — a key HEC zone",
                    "(Pastorini et al. 2010; Wildlife Dept. Sri Lanka 2023 Annual Report)."
                  ),
                  plotlyOutput("plot_lon", height = "460px")
                )
              ),
              fluidRow(
                box(
                  title = "\U0001F5FA Live Position — Synced with Longitude Chart",
                  width = 12, solidHeader = TRUE,
                  tags$p(
                    style = "color:#666; font-size:11px; margin-bottom:6px;",
                    "Hover over a point on the Longitude vs Time chart above:",
                    "the map below jumps to that elephant's position at that",
                    "moment and draws the path travelled up to it, so you can",
                    "see exactly where — and in which direction — it moved."
                  ),
                  leafletOutput("sync_map_lon", height = "420px")
                )
              ),
              fluidRow(
                box(
                  title = "\U0001F4DA Literature Context — Longitude & East\u2013West Ranging",
                  width = 12, solidHeader = TRUE,
                  tags$div(style = "display:flex; flex-wrap:wrap; gap:20px; padding:4px 0;",
                           
                           tags$div(style = "flex:1; min-width:220px;",
                                    tags$h4("Pastorini et al. (2010)", class = "ref-heading"),
                                    tags$p("Genetic analysis of Sri Lankan elephants confirmed
                        east\u2013west sub-population structure partly driven by
                        the Mahaweli River. Kaudulla elephants belong to the
                        eastern dry-zone meta-population.", class = "ref-text")
                           ),
                           tags$div(style = "flex:1; min-width:220px;",
                                    tags$h4("Leimgruber et al. (2008)", class = "ref-heading"),
                                    tags$p("Longitude displacement of >0.05\u00B0 per day indicates
                        long-range foraging excursions beyond the core
                        Kaudulla\u2013Minneriya protected area, with agriculture
                        along the eastern boundary being the primary
                        conflict zone.", class = "ref-text")
                           ),
                           tags$div(style = "flex:1; min-width:220px;",
                                    tags$h4("HEC Hotspot — Eastern Boundary", class = "ref-heading"),
                                    tags$p("Longitudes >80.94\u00B0E place elephants near the
                        Giritale\u2013Hingurakgoda road and paddy fields.
                        Wildlife Dept. electric fence lines run along
                        ~80.95\u00B0E on the eastern park edge.", class = "ref-text")
                           ),
                           tags$div(style = "flex:1; min-width:220px;",
                                    tags$h4("Geographic Reference", class = "ref-heading"),
                                    tags$p("Longitude 80.87\u00B0\u201380.96\u00B0E (WGS84). Kaudulla tank
                        central axis \u224880.89\u00B0E. National Highway A11
                        (Habarana\u2013Trincomalee) crosses the corridor
                        near 80.93\u00B0E and is a major elephant crossing point.",
                                           class = "ref-text")
                           )
                  )
                )
              )
      ),
      
      # ── TAB 3 : Both coordinates ─────────────────────────────────────────────
      tabItem("both_tab",
              fluidRow(
                box(
                  title = "\U0001F4CD Latitude & Longitude vs Time (Overlaid, Dual Y-Axis)",
                  width = 12, solidHeader = TRUE,
                  tags$p(
                    style = "color:#666; font-size:11px; margin-bottom:6px;",
                    "Latitude (solid lines, circles, left axis) and longitude",
                    "(dotted lines, triangles, right axis) are plotted on the",
                    "same chart per elephant, using matching colours. Correlated",
                    "dips in latitude with rising longitude typically indicate",
                    "movement toward agricultural areas on the eastern boundary."
                  ),
                  plotlyOutput("plot_both", height = "600px")
                )
              ),
              fluidRow(
                box(
                  title = "\U0001F5FA Live Position — Synced with Lat/Lon Chart",
                  width = 12, solidHeader = TRUE,
                  tags$p(
                    style = "color:#666; font-size:11px; margin-bottom:6px;",
                    "Hover over a point on the Lat/Lon chart above: the map",
                    "below jumps to that elephant's position at that moment",
                    "and draws the path travelled up to it."
                  ),
                  leafletOutput("sync_map_both", height = "420px")
                )
              ),
              fluidRow(
                box(
                  title = "\U0001F5FA Reference Map — Kaudulla Tank & Park Boundary",
                  width = 5, solidHeader = TRUE,
                  tags$p(
                    style = "color:#666; font-size:11px; margin-bottom:6px;",
                    "Approximate park boundary (dashed) and the Kaudulla Tank",
                    "reference point used throughout the Latitude/Longitude tabs.",
                    "This is the geographic anchor for every reference line and",
                    "excursion described elsewhere in the dashboard."
                  ),
                  leafletOutput("kaudulla_ref_map", height = "420px")
                ),
                box(
                  title = "\U0001F4CD Key Coordinates",
                  width = 7, solidHeader = TRUE,
                  tags$p(
                    style = "color:#666; font-size:11px; margin-bottom:8px;",
                    "Each numbered badge below matches the same badge on the reference map",
                    "— \u25CF teal numbers are point features, \u25CF orange numbers are boundary lines."
                  ),
                  tags$table(
                    class = "table table-condensed",
                    style = "font-size:12px; color:#444;",
                    tags$thead(
                      tags$tr(
                        tags$th("#"), tags$th("Location"), tags$th("Latitude"), tags$th("Longitude"), tags$th("Relevance to elephant movement")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td(tags$span(class = "ref-badge core", "1")), tags$td(tags$b("Kaudulla Tank (core reference)")), tags$td("8.140\u00B0N"), tags$td("80.895\u00B0E"),
                              tags$td("Dry-season water source; latitudinal reference line on the Lat/Lon tabs")),
                      tags$tr(tags$td(tags$span(class = "ref-badge core", "2")), tags$td("Park entrance / safari zone"), tags$td("8.111\u00B0N"), tags$td("80.886\u00B0E"),
                              tags$td("Southwestern edge of range; low elephant density")),
                      tags$tr(tags$td(tags$span(class = "ref-badge core", "3")), tags$td("Kaudulla Wewa (mapped reservoir)"), tags$td("8.168\u00B0N"), tags$td("80.926\u00B0E"),
                              tags$td("Northeastern shoreline; frequent gathering point in dry months")),
                      tags$tr(tags$td(tags$span(class = "ref-badge boundary", "4")), tags$td("Southern park boundary"), tags$td("8.080\u00B0N"), tags$td("\u2014"),
                              tags$td("Southward range limit shown as a reference line on the Latitude tab")),
                      tags$tr(tags$td(tags$span(class = "ref-badge boundary", "5")), tags$td("Northern park boundary"), tags$td("8.220\u00B0N"), tags$td("\u2014"),
                              tags$td("Northward range limit shown as a reference line on the Latitude tab")),
                      tags$tr(tags$td(tags$span(class = "ref-badge boundary", "6")), tags$td("Eastern boundary (HEC zone)"), tags$td("\u2014"), tags$td("80.950\u00B0E"),
                              tags$td("Agricultural edge; excursions beyond this longitude flag conflict risk")),
                      tags$tr(tags$td(tags$span(class = "ref-badge boundary", "7")), tags$td("Western park boundary"), tags$td("\u2014"), tags$td("80.872\u00B0E"),
                              tags$td("Westward range limit shown as a reference line on the Longitude tab"))
                    )
                  ),
                  tags$p(style = "color:#888; font-size:10px; margin-top:8px;",
                         "Coordinates are approximate (WGS84) and are the same reference values used to draw the dashed lines on the Latitude, Longitude, and Both-Coordinates charts.")
                )
              ),
              fluidRow(
                box(
                  title = "\U0001F418 About The Gathering — Kaudulla National Park",
                  width = 8, solidHeader = TRUE,
                  tags$p(style = "color:#444; font-size:12px; line-height:1.7;",
                         tags$b("Kaudulla National Park"), " was gazetted in 2002 specifically
              to protect the elephant corridor between Minneriya and Hurulu Eco Park.
              Together these three parks form the 'Trincomalee Elephant Triangle'.",
                         tags$br(), tags$br(),
                         "Every year between July and October, up to ", tags$b("300\u2013400 elephants"),
                         " converge at the Kaudulla and Minneriya tanks in what is known as ",
                         tags$b(style = "color:#2e7d32;", "'The Gathering'"),
                         " — one of the largest aggregations of Asian elephants in the world
              (Fernando et al. 2008; BBC Wildlife Magazine 2009).",
                         tags$br(), tags$br(),
                         "This GPS collar dataset documents the movement of ",
                         tags$b(style = "color:#2e7d32;", "14 individually identified elephants"),
                         " from July 2024 to June 2026, capturing seasonal latitudinal shifts,
              boundary excursions, and corridor use."
                  )
                ),
                box(
                  title = "\U0001F3DB Wildlife Department Mandate",
                  width = 4, solidHeader = TRUE,
                  tags$p(style = "color:#444; font-size:12px; line-height:1.7;",
                         "The Department of Wildlife Conservation of Sri Lanka (DWC),
              under the Ministry of Environment, administers Kaudulla under
              the Fauna and Flora Protection Ordinance (FFPO).",
                         tags$br(), tags$br(),
                         "The GPS collar programme contributes to:", tags$br(),
                         "\u2022 Human\u2013Elephant Conflict (HEC) early warning", tags$br(),
                         "\u2022 Corridor integrity assessment", tags$br(),
                         "\u2022 Population monitoring", tags$br(), tags$br(),
                         tags$a("wildlife.gov.lk", href = "https://wildlife.gov.lk",
                                target = "_blank", style = "color:#2e7d32;")
                  )
                )
              )
      ),
      
      
      
      # ── TAB 4 : Heat Maps ────────────────────────────────────────────────────
      tabItem("heat_tab",
              fluidRow(
                box(
                  title = "\U0001F321 Average Longitude by Month — Position Heat Map (blank = no data)",
                  width = 12, solidHeader = TRUE,
                  tags$p(
                    style = "color:#bbb; font-size:11px; margin-bottom:6px;",
                    "Cell colour = mean longitude of GPS fixes for that elephant in that",
                    "month (warmer = further east, toward the agricultural boundary;",
                    "cooler = further west, toward the tank). Blank cells mean the",
                    "elephant had no GPS fixes recorded that month within the current filters."
                  ),
                  plotlyOutput("heat_lon", height = "420px")
                )
              ),
              fluidRow(
                box(
                  title = "\U0001F4CA Data Coverage by Month — GPS Fix Count Heat Map",
                  width = 12, solidHeader = TRUE,
                  tags$p(
                    style = "color:#bbb; font-size:11px; margin-bottom:6px;",
                    "Cell colour = number of GPS fixes recorded for that elephant in",
                    "that month. Darker/blank cells flag months with sparse or missing",
                    "collar data — useful for spotting collar failures or animals that",
                    "temporarily left monitoring range."
                  ),
                  plotlyOutput("heat_n", height = "420px")
                )
              )
      ),
      
      # ── TAB 5 : Elephant Tracking ────────────────────────────────────────────────────
      tabItem("tracking_tab",
              fluidRow(
                box(
                  title = "🗺️ Elephant Tracking Overview",
                  width = 12,
                  solidHeader = TRUE,
                  leafletOutput("tracking_map", height = 600)
                )
              ),
              
              fluidRow(
                box(
                  title = "📊 GPS Tracking Data by Month",
                  width = 12,
                  solidHeader = TRUE,
                  
                  tags$div(
                    style = "text-align:center;",
                    
                    tags$a(
                      href = "https://zubhp3-amali-priyanwada.shinyapps.io/elephants_by_month/",
                      target = "_blank",
                      
                      tags$button(
                        "Click here for the Month Wise GPS Tracking Data Analysis",
                        style = "
            background-color:#2E8B57;
            color:white;
            border:none;
            padding:12px 25px;
            font-size:16px;
            font-weight:bold;
            border-radius:8px;
            cursor:pointer;
          "
                      )
                    )
                  )
                )
              ),
              
              useShinyjs(), # Initialize shinyjs to handle button color swapping
              theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
              
              fluidRow(
                box(
                  title = "🐘 GPS Tracking Data by Individual Elephant",
                  width = 12,solidHeader = TRUE,
                  useShinyjs(),
                  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
                  
                  div(
                    style = "padding: 5px 15px 0px 15px; display: flex; justify-content: space-between; align-items: center;",
                    tags$h4("Kaudulla Elephant Tracking Timeline", style = "margin: 0; font-weight: bold; font-size: 1.3rem;"),
                    
                    div(
                      style = "display: flex; align-items: center; gap: 15px; background-color: #f8f9fa; padding: 4px 12px; border-radius: 6px; border: 1px solid #e3e6f0;",
                      div(
                        style = "min-width: 90px; text-align: center;",
                        tags$strong(textOutput("current_month_ui"), style = "font-size: 1.1rem; color: #2c3e50;")
                      ),
                      div(
                        style = "display: flex; gap: 5px;",
                        actionButton("btn_prev", "Back ⏮", class = "btn btn-sm btn-secondary", style = "padding: 2px 8px;"),
                        actionButton("btn_toggle", "▶ Play", class = "btn btn-sm btn-success", style = "padding: 2px 12px;"), 
                        actionButton("btn_next", "Next ⏭", class = "btn btn-sm btn-secondary", style = "padding: 2px 8px;")
                      )
                    )
                  ),
                  hr(style = "margin: 5px 0 10px 0;"),
                  
                  div(
                    style = "width: 100%; height: 83vh; display: flex; justify-content: center; align-items: center; overflow: hidden; padding: 0 10px;",
                    imageOutput("elephant_plot", width = "auto", height = "100%")
                  )
                )
              )
      ),
      
      
      
      
      
      
      # ── TAB 5b : Live Elephant Path ──────────────────────────────────────────
      tabItem("live_tab",
              fluidRow(
                box(
                  title = "\U0001F418 Choose Elephant", width = 4, solidHeader = TRUE,
                  selectInput(
                    "live_elephant", NULL,
                    choices  = sort(unique(elephants_df$name)),
                    selected = sort(unique(elephants_df$name))[1]
                  ),
                  selectInput(
                    "live_month", "Month",
                    choices  = month_choices,
                    selected = "all"
                  ),
                  tags$p(
                    style = "color:#666; font-size:11px; margin: -6px 0 10px;",
                    "Elephant + Month here are specific to this page.",
                    "The sidebar's Date Range still applies too.",
                    "Press \u25B6 on the slider to animate, or drag it."
                  ),
                  uiOutput("live_info_box")
                ),
                box(
                  title = "\U0001F3AC Playback", width = 8, solidHeader = TRUE,
                  uiOutput("live_slider_ui")
                )
              ),
              fluidRow(
                box(
                  title = "\U0001F5FA Live Map — Path Drawn in Real Time",
                  width = 12, solidHeader = TRUE,
                  tags$p(
                    style = "color:#666; font-size:11px; margin-bottom:6px;",
                    "The shaded polygon is the elephant's home-range (convex",
                    "hull) built only from the fixes seen so far — watch it",
                    "expand and reshape as more of the path is revealed."
                  ),
                  leafletOutput("live_map", height = 500)
                )
              ),
              fluidRow(
                box(
                  title = "\U0001F4D0 Home-Range (Hull) Area — Growing Live", width = 12, solidHeader = TRUE,
                  tags$p(
                    style = "color:#666; font-size:11px; margin-bottom:6px;",
                    "Convex-hull area (km\u00B2) computed from only the fixes",
                    "revealed so far. Needs at least 3 fixes to form a polygon."
                  ),
                  plotlyOutput("live_hull_plot", height = "300px")
                )
              ),
              fluidRow(
                box(
                  title = "\U0001F4CD Latitude vs Time (live)", width = 6, solidHeader = TRUE,
                  plotlyOutput("live_lat_plot", height = "320px")
                ),
                box(
                  title = "\U0001F4CD Longitude vs Time (live)", width = 6, solidHeader = TRUE,
                  plotlyOutput("live_lon_plot", height = "320px")
                )
              )
      ),
      
      
      # ── TAB 6 : Migration & Climate ────────────────────────────────────────────────────
      tabItem("climate_tab",
              
              fluidRow(
                box(
                  title = "🐘 Elephant Tracking Data Availability",
                  width = 12,  # <-- Changed from 6 to 12 for full width
                  solidHeader = TRUE,
                  
                  # Make the select input smaller and inline
                  fluidRow(
                    column(
                      width = 3,  # Elephant selector takes 1/4 of the row
                      selectInput(
                        inputId = "selected_elephant",
                        label = "Select Elephant Name:",
                        choices = elephant_names,
                        selected = elephant_names[1]
                      )
                    ),
                    column(
                      width = 9,  # Help text takes remaining 3/4
                      helpText(
                        "This heatmap shows the percentage of valid GPS records captured per day (max 24 records/day)."
                      )
                    )
                  ),
                  
                  # Full width plot
                  plotOutput("calendar_plot", height = "500px")  # Slightly taller for better visibility
                )
              ),
              
              fluidRow(
                box(
                  title = "🐘 Elephant Migration Map",
                  width = 12,
                  solidHeader = TRUE,
                  
                  sidebarLayout(
                    sidebarPanel(
                      width = 2,
                      
                      selectInput("year", "Select Year", choices = NULL),
                      
                      tags$div(
                        style = "max-width:150px;",
                        selectInput(
                          "month",
                          "Select Month",
                          # names shown to the user ("January"...) map to the
                          # "01".."12" values used everywhere else in the app
                          choices = setNames(sprintf("%02d", 1:12), month.name),
                          multiple = FALSE
                        )
                      ),
                      
                      tags$div(
                        style = "max-width:130px;",
                        selectInput(
                          "elephant",
                          "Select Elephant",
                          choices = NULL
                        )
                      ),
                      
                      tags$div(
                        style = "max-width:150px;",
                        selectInput(
                          "select_week",
                          "Select Week",
                          choices = c("All Weeks", "Week 1", "Week 2", "Week 3", "Week 4"),
                          selected = "All Weeks",
                          multiple = TRUE
                        )
                      ),
                      
                      checkboxInput(
                        "show_seq_numbers",
                        "Show point sequence numbers",
                        value = FALSE
                      )
                    ),
                    
                    mainPanel(
                      width = 10,
                      
                      tags$div(
                        style = "text-align:right; margin-bottom:8px;",
                        actionButton(
                          "open_map_newtab",
                          "🔗 Open Map in New Tab",
                          class = "btn-sm",
                          style = "background:#2E8B57; color:white; border:none;"
                        )
                      ),
                      leafletOutput("map", height = 600),
                      
                      tags$p(
                        style = "font-size:11px; color:#666; line-height:1.45; margin-top:10px;",
                        tags$em(
                          "Note: the GPS collars record ", tags$b("hourly"), " fixes, so up ",
                          "to 24 points can appear per elephant per day. However, the data has ",
                          tags$b("missing values"), " - some hours simply have no reading ",
                          "(the collar missed a fix, lost signal, etc.), so gaps in the track ",
                          "are expected, not an error. When enabled, the numbers above are ",
                          "based ", tags$b("only on the readings that are actually available"),
                          " (missing hours are skipped, not counted), showing the ",
                          tags$b("order"), " in which those available fixes occurred ",
                          "(1 = earliest fix shown, highest = most recent available fix) - ",
                          "they are ", tags$b("not"), " day numbers, hours of the day, or dates."
                        )
                      )
                    )
                  )
                )
              ),
              
              
              fluidRow(
                box(
                  title = "🌡 Climate Calendar Analysis",
                  width = 12,
                  solidHeader = TRUE,
                  
                  # --------------------------
                  # First row
                  # --------------------------
                  fluidRow(
                    
                    column(
                      width = 3,
                      
                      selectInput(
                        "variable",
                        "Climate Variable",
                        choices = names(plot_info)
                      )
                      
                    ),
                    
                    column(
                      width = 9,
                      
                      helpText(
                        "This calendar heatmap displays daily values of the selected climate variable."
                      )
                      
                    )
                    
                  ),
                  
                  # --------------------------
                  # Calendar
                  # --------------------------
                  plotOutput(
                    "calendarPlot",
                    height = "550px"
                  ),
                  
                  hr(),
                  
                  # --------------------------
                  # Summary
                  # --------------------------
                  h4("Summary Statistics"),
                  
                  tableOutput("summaryTable")
                  
                )
              )
      ),
      
      # ── TAB 7 : Data Table ───────────────────────────────────────────────────
      tabItem("data_tab",
              fluidRow(
                box(
                  title = "\U0001F4CB GPS Observation Records",
                  width = 12, solidHeader = TRUE,
                  DTOutput("data_table")
                )
              )
      ),
      tabItem(
        "mcp_tab",
        div(
          class = "section-title",
          bsicons::bs_icon("compass"), " Home Range, Movement & Speed"
        ),
        div(
          class = "section-description",
          "Minimum convex polygon (MCP) home ranges, GPS movement tracks, ",
          "and speed/direction metrics for elephants tracked at Kaudulla National Park."
        ),
        fluidRow(
          box(
            title = "Filters", width = 12, solidHeader = TRUE,
            tags$div(
              style = "display:flex; flex-wrap:wrap; gap:20px; align-items:flex-start;",
              tags$div(
                style = "min-width:220px;",
                checkboxGroupInput(
                  "mcp_sex_filter", "Sex",
                  choices = mcp_unique_sexes,
                  selected = mcp_unique_sexes
                )
              ),
              tags$div(
                style = "min-width:280px; padding-top:4px; color:#64748b; font-size:13px; line-height:1.6;",
                icon("circle-info"), " Elephant, Date Range, and Month are controlled from the ",
                tags$b("sidebar"), " on the left and apply to this tab too, so it stays in sync ",
                "with the other plots."
              )
            )
          )
        ),
        fluidRow(
          valueBoxOutput("mcp_vb_elephants", width = 3),
          valueBoxOutput("mcp_vb_points", width = 3),
          valueBoxOutput("mcp_vb_speed", width = 3),
          valueBoxOutput("mcp_vb_distance", width = 3)
        ),
        fluidRow(
          box(
            title = "Tracking Points & Minimum Convex Polygons",
            width = 12, solidHeader = TRUE,
            leafletOutput("mcp_hull_map", height = "600px")
          )
        ),
        fluidRow(
          box(
            title = "Cumulative Distance Traveled Over Time",
            width = 12, solidHeader = TRUE,
            plotlyOutput("mcp_timeline_plot", height = "550px")
          )
        ),
        fluidRow(
          box(
            title = "Home Range Area by Elephant",
            width = 12, solidHeader = TRUE,
            plotlyOutput("mcp_area_bar_chart", height = "550px")
          )
        ),
        fluidRow(
          box(
            title = "Movement Direction by Elephant (16 compass sectors)",
            width = 6, solidHeader = TRUE,
            uiOutput("mcp_rose_individual_ui")
          ),
          box(
            title = "Overall Movement Direction — All Selected Elephants Combined",
            width = 6, solidHeader = TRUE,
            plotlyOutput("mcp_rose_population", height = "600px")
          )
        ),
        fluidRow(
          box(
            title = "Per-Elephant Summary",
            width = 12, solidHeader = TRUE,
            DTOutput("mcp_summary_table")
          )
        )
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  all_names    <- sort(unique(elephants_df$name))
  female_names <- sort(unique(elephants_df$name[elephants_df$sex == "Female"]))
  male_names   <- sort(unique(elephants_df$name[elephants_df$sex == "Male"]))
  
  # ── Quick-select buttons ─────────────────────────────────────────────────────
  observeEvent(input$btn_all,    updateSelectInput(session, "sel_elephants", selected = all_names))
  observeEvent(input$btn_female, updateSelectInput(session, "sel_elephants", selected = female_names))
  observeEvent(input$btn_male,   updateSelectInput(session, "sel_elephants", selected = male_names))
  observeEvent(input$btn_clear,  updateSelectInput(session, "sel_elephants", selected = character(0)))
  
  # ── Reactive filtered dataset ────────────────────────────────────────────────
  filtered <- reactive({
    req(input$date_range)
    
    df <- elephants_df %>%
      filter(
        date_parsed >= input$date_range[1],
        date_parsed <= input$date_range[2]
      )
    
    if (length(input$sel_elephants) > 0) {
      df <- df %>% filter(name %in% input$sel_elephants)
    } else {
      df <- df[0, ]
    }
    
    if (!is.null(input$sel_month) && input$sel_month != "all") {
      df <- df %>% filter(format(date_parsed, "%Y-%m") == input$sel_month)
    }
    
    df
  })
  
  # ── Aggregated dataset ───────────────────────────────────────────────────────
  agg_data <- reactive({
    df <- filtered()
    lvl <- input$agg_level
    
    if (lvl == "raw") return(df)
    
    df %>%
      mutate(
        period = if (lvl == "day")
          as.POSIXct(floor_date(datetime_sl, "day"))
        else
          as.POSIXct(floor_date(datetime_sl, "week"))
      ) %>%
      group_by(name, sex, period) %>%
      summarise(lat = mean(lat, na.rm = TRUE),
                lon = mean(lon, na.rm = TRUE),
                .groups = "drop") %>%
      rename(datetime_sl = period)
  })
  
  # ── Value boxes ──────────────────────────────────────────────────────────────
  output$vbox_obs <- renderValueBox({
    n <- nrow(filtered())
    valueBox(format(n, big.mark = ","), "GPS Fixes (filtered)",
             icon = icon("location-dot"), color = "green")
  })
  output$vbox_eleph <- renderValueBox({
    valueBox(length(unique(filtered()$name)), "Elephants Selected",
             icon = icon("paw"), color = "olive")
  })
  output$vbox_start <- renderValueBox({
    d <- suppressWarnings(min(filtered()$date_parsed, na.rm = TRUE))
    valueBox(if (is.finite(d)) format(d, "%d %b %Y") else "\u2014", "Data From",
             icon = icon("calendar-day"), color = "teal")
  })
  output$vbox_end <- renderValueBox({
    d <- suppressWarnings(max(filtered()$date_parsed, na.rm = TRUE))
    valueBox(if (is.finite(d)) format(d, "%d %b %Y") else "\u2014", "Data To",
             icon = icon("calendar-check"), color = "teal")
  })
  
  # ── Helper: single-coordinate plotly scatter ────────────────────────────────
  make_plot <- function(df, y_col, y_title, ref_lines = NULL, plot_source = NULL) {
    elephants_in_data <- unique(df$name)
    col_map <- ELEPHANT_COLOURS[names(ELEPHANT_COLOURS) %in% elephants_in_data]
    
    p <- plot_ly(source = plot_source)
    
    for (el in elephants_in_data) {
      sub <- df %>% filter(name == el) %>% arrange(datetime_sl)
      sub <- insert_gaps(sub)                 # FIX 2: break lines at gaps
      clr <- if (el %in% names(col_map)) col_map[[el]] else "#aaaaaa"
      
      p <- p %>% add_trace(
        data = sub,
        x    = ~datetime_sl,
        y    = as.formula(paste0("~", y_col)),
        type = "scatter",
        mode = "lines+markers",
        name = el,
        connectgaps = FALSE,                  # FIX 2: do NOT join across gaps
        line    = list(color = clr, width = 1.5),
        marker  = list(
          color   = clr,
          size    = 4,
          opacity = 0.85,
          symbol  = "circle",
          line    = list(color = clr, width = 1)
        ),
        text = ~paste0(
          "<b>", name, "</b><br>",
          "Time (SL): ", format(datetime_sl, "%d %b %Y %H:%M"), "<br>",
          y_title, ": ", round(get(y_col), 5), "\u00B0<br>",
          "Sex: ", sex
        ),
        hoverinfo = "text"
      )
      
      if (input$add_smooth && nrow(sub) > 10) {
        smooth_df <- data.frame(x = as.numeric(sub$datetime_sl), y = sub[[y_col]])
        smooth_df <- smooth_df[!is.na(smooth_df$y), ]
        if (nrow(smooth_df) > 5) {
          lo <- loess(y ~ x, data = smooth_df, span = 0.3)
          smooth_df$yhat <- predict(lo)
          smooth_df$ts   <- as.POSIXct(smooth_df$x, origin = "1970-01-01", tz = "Asia/Colombo")
          p <- p %>% add_trace(
            data = smooth_df, x = ~ts, y = ~yhat,
            type = "scatter", mode = "lines",
            name = paste(el, "(smooth)"),
            line = list(color = clr, width = 2.5, dash = "dot"),
            showlegend = FALSE, hoverinfo = "skip"
          )
        }
      }
    }
    
    if (!is.null(ref_lines)) {
      for (rl in ref_lines) {
        p <- p %>% add_segments(
          x = min(df$datetime_sl, na.rm = TRUE),
          xend = max(df$datetime_sl, na.rm = TRUE),
          y = rl$val, yend = rl$val,
          line = list(color = rl$color, width = 1.5, dash = "dash"),
          name = rl$label, showlegend = TRUE, hoverinfo = "name"
        )
      }
    }
    
    p %>% layout(
      paper_bgcolor = "#ffffff",
      plot_bgcolor  = "#ffffff",
      font  = list(color = "#333333", family = "Segoe UI"),
      xaxis = list(title = "Date / Time (Asia/Colombo)", gridcolor = "#e5e5e5",
                   zerolinecolor = "#dddddd", tickformat = "%b %Y"),
      yaxis = list(title = y_title, gridcolor = "#e5e5e5", zerolinecolor = "#dddddd"),
      legend = list(bgcolor = "#ffffff", bordercolor = "#4caf50",
                    borderwidth = 1, font = list(size = 11)),
      hoverlabel = list(bgcolor = "#ffffff", font = list(color = "#333333")),
      margin = list(t = 40, b = 60, l = 70, r = 20)
    )
  }
  
  # ── Helper: dual-axis plot — latitude & longitude overlaid ──────────────────
  make_dual_axis_plot <- function(df, plot_source = NULL) {
    elephants_in_data <- unique(df$name)
    col_map <- ELEPHANT_COLOURS[names(ELEPHANT_COLOURS) %in% elephants_in_data]
    
    p <- plot_ly(source = plot_source)
    
    for (el in elephants_in_data) {
      sub <- df %>% filter(name == el) %>% arrange(datetime_sl)
      sub <- insert_gaps(sub)                 # FIX 2: break lines at gaps
      clr <- if (el %in% names(col_map)) col_map[[el]] else "#aaaaaa"
      
      # Latitude (left axis, solid, circles)
      p <- p %>% add_trace(
        data = sub, x = ~datetime_sl, y = ~lat,
        type = "scatter", mode = "lines+markers",
        name = paste(el, "\u2013 Lat"), legendgroup = el, yaxis = "y",
        connectgaps = FALSE,
        line   = list(color = clr, width = 1.5, dash = "solid"),
        marker = list(color = clr,
                      size = 4,
                      symbol = "circle",
                      line = list(color = clr, width = 1)),
        text = ~paste0("<b>", name, "</b><br>Latitude: ", round(lat, 5), "\u00B0N<br>",
                       format(datetime_sl, "%d %b %Y %H:%M"), "<br>Sex: ", sex),
        hoverinfo = "text"
      )
      
      # Longitude (right axis, dotted, triangles)
      p <- p %>% add_trace(
        data = sub, x = ~datetime_sl, y = ~lon,
        type = "scatter", mode = "lines+markers",
        name = paste(el, "\u2013 Lon"), legendgroup = el, yaxis = "y2",
        connectgaps = FALSE,
        line   = list(color = clr, width = 1.5, dash = "dot"),
        marker = list(color = clr,
                      size = 4,
                      symbol = "triangle-up",
                      line = list(color = clr, width = 1)),
        text = ~paste0("<b>", name, "</b><br>Longitude: ", round(lon, 5), "\u00B0E<br>",
                       format(datetime_sl, "%d %b %Y %H:%M"), "<br>Sex: ", sex),
        hoverinfo = "text"
      )
      
      if (input$add_smooth && nrow(sub) > 10) {
        sm_lat <- data.frame(x = as.numeric(sub$datetime_sl), y = sub$lat)
        sm_lat <- sm_lat[!is.na(sm_lat$y), ]
        if (nrow(sm_lat) > 5) {
          lo <- loess(y ~ x, data = sm_lat, span = 0.3)
          sm_lat$yhat <- predict(lo)
          sm_lat$ts   <- as.POSIXct(sm_lat$x, origin = "1970-01-01", tz = "Asia/Colombo")
          p <- p %>% add_trace(data = sm_lat, x = ~ts, y = ~yhat,
                               type = "scatter", mode = "lines",
                               name = paste(el, "Lat smooth"), legendgroup = el, yaxis = "y",
                               line = list(color = clr, width = 2.5, dash = "solid"),
                               opacity = 0.4, showlegend = FALSE, hoverinfo = "skip")
        }
        sm_lon <- data.frame(x = as.numeric(sub$datetime_sl), y = sub$lon)
        sm_lon <- sm_lon[!is.na(sm_lon$y), ]
        if (nrow(sm_lon) > 5) {
          lo2 <- loess(y ~ x, data = sm_lon, span = 0.3)
          sm_lon$yhat <- predict(lo2)
          sm_lon$ts   <- as.POSIXct(sm_lon$x, origin = "1970-01-01", tz = "Asia/Colombo")
          p <- p %>% add_trace(data = sm_lon, x = ~ts, y = ~yhat,
                               type = "scatter", mode = "lines",
                               name = paste(el, "Lon smooth"), legendgroup = el, yaxis = "y2",
                               line = list(color = clr, width = 2.5, dash = "dashdot"),
                               opacity = 0.4, showlegend = FALSE, hoverinfo = "skip")
        }
      }
    }
    
    ref_lat <- list(
      list(val = 8.140, color = "#4fc3f7", label = "Kaudulla Tank (Lat ~8.140\u00B0N)"),
      list(val = 8.080, color = "#ef9a9a", label = "S. Boundary (Lat ~8.080\u00B0N)"),
      list(val = 8.220, color = "#ef9a9a", label = "N. Boundary (Lat ~8.220\u00B0N)")
    )
    ref_lon <- list(
      list(val = 80.895, color = "#4fc3f7", label = "Kaudulla Tank (Lon ~80.895\u00B0E)"),
      list(val = 80.950, color = "#ef9a9a", label = "E. Boundary (Lon ~80.950\u00B0E)"),
      list(val = 80.872, color = "#ef9a9a", label = "W. Boundary (Lon ~80.872\u00B0E)")
    )
    x_min <- min(df$datetime_sl, na.rm = TRUE)
    x_max <- max(df$datetime_sl, na.rm = TRUE)
    
    for (rl in ref_lat) {
      p <- p %>% add_segments(x = x_min, xend = x_max, y = rl$val, yend = rl$val, yaxis = "y",
                              line = list(color = rl$color, width = 1, dash = "dash"),
                              name = rl$label, showlegend = TRUE, hoverinfo = "name")
    }
    for (rl in ref_lon) {
      p <- p %>% add_segments(x = x_min, xend = x_max, y = rl$val, yend = rl$val, yaxis = "y2",
                              line = list(color = rl$color, width = 1, dash = "dashdot"),
                              name = rl$label, showlegend = TRUE, hoverinfo = "name")
    }
    
    p %>% layout(
      paper_bgcolor = "#ffffff",
      plot_bgcolor  = "#ffffff",
      font  = list(color = "#333333", family = "Segoe UI"),
      xaxis = list(title = "Date / Time (Asia/Colombo)", gridcolor = "#e5e5e5",
                   zerolinecolor = "#dddddd", tickformat = "%b %Y", domain = c(0, 1)),
      yaxis = list(title = "Latitude (\u00B0N, WGS84)", gridcolor = "#e5e5e5",
                   zerolinecolor = "#dddddd",
                   titlefont = list(color = "#0277bd"), tickfont = list(color = "#0277bd")),
      yaxis2 = list(title = "Longitude (\u00B0E, WGS84)", overlaying = "y", side = "right",
                    showgrid = FALSE,
                    titlefont = list(color = "#ef6c00"), tickfont = list(color = "#ef6c00")),
      legend = list(bgcolor = "#ffffff", bordercolor = "#4caf50",
                    borderwidth = 1, font = list(size = 10)),
      hoverlabel = list(bgcolor = "#ffffff", font = list(color = "#333333")),
      margin = list(t = 40, b = 60, l = 70, r = 70)
    )
  }
  
  # ── Latitude plot ────────────────────────────────────────────────────────────
  output$plot_lat <- renderPlotly({
    df <- agg_data()
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    ref_lines <- list(
      list(val = 8.140, color = "#4fc3f7", label = "Kaudulla Tank (~8.140\u00B0N)"),
      list(val = 8.080, color = "#ef9a9a", label = "S. Park Boundary (~8.080\u00B0N)"),
      list(val = 8.220, color = "#ef9a9a", label = "N. Park Boundary (~8.220\u00B0N)")
    )
    make_plot(df, "lat", "Latitude (\u00B0N, WGS84)", ref_lines, plot_source = "lat_plotly")
  })
  
  # ── Longitude plot ───────────────────────────────────────────────────────────
  output$plot_lon <- renderPlotly({
    df <- agg_data()
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    ref_lines <- list(
      list(val = 80.895, color = "#4fc3f7", label = "Kaudulla Tank (~80.895\u00B0E)"),
      list(val = 80.950, color = "#ef9a9a", label = "E. Park Boundary (~80.950\u00B0E)"),
      list(val = 80.872, color = "#ef9a9a", label = "W. Park Boundary (~80.872\u00B0E)")
    )
    make_plot(df, "lon", "Longitude (\u00B0E, WGS84)", ref_lines, plot_source = "lon_plotly")
  })
  
  # ── Both coordinates (dual y-axis) ──────────────────────────────────────────
  output$plot_both <- renderPlotly({
    df <- agg_data()
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    make_dual_axis_plot(df, plot_source = "both_plotly")
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # SYNCED MAP — hovering on the Lat/Lon/Both time-series charts moves a
  # marker on the map to that exact GPS fix and draws the path travelled so
  # far, per elephant, so direction of movement is visible at a glance.
  # ══════════════════════════════════════════════════════════════════════════
  
  # ── Robustly turn whatever plotly gives back for the x-hover value into a
  #    POSIXct in Sri-Lanka time ──────────────────────────────────────────────
  parse_hover_time <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.numeric(x)) {
      # plotly sometimes reports epoch milliseconds for datetime axes
      return(as.POSIXct(x / 1000, origin = "1970-01-01", tz = "Asia/Colombo"))
    }
    t <- suppressWarnings(as.POSIXct(x, tz = "Asia/Colombo"))
    if (is.na(t)) {
      t <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Colombo"))
    }
    t
  }
  
  get_col <- function(nm) {
    if (nm %in% names(ELEPHANT_COLOURS)) unname(ELEPHANT_COLOURS[[nm]]) else "#888888"
  }
  
  # Single reactive "scrubber" position, driven by whichever chart the user
  # is hovering over (Latitude, Longitude, or the combined Both-Coordinates
  # chart) — all three synced maps below react to it.
  hover_time <- reactiveVal(NULL)
  
  observeEvent(event_data("plotly_hover", source = "lat_plotly"), {
    ed <- event_data("plotly_hover", source = "lat_plotly")
    t  <- parse_hover_time(ed$x)
    if (!is.null(t) && !is.na(t)) hover_time(t)
  })
  observeEvent(event_data("plotly_hover", source = "lon_plotly"), {
    ed <- event_data("plotly_hover", source = "lon_plotly")
    t  <- parse_hover_time(ed$x)
    if (!is.null(t) && !is.na(t)) hover_time(t)
  })
  observeEvent(event_data("plotly_hover", source = "both_plotly"), {
    ed <- event_data("plotly_hover", source = "both_plotly")
    t  <- parse_hover_time(ed$x)
    if (!is.null(t) && !is.na(t)) hover_time(t)
  })
  
  # ── Base map (context layer): full, faint tracks for every elephant that
  #    passes the current filters — redrawn only when the filters change,
  #    NOT on every hover (that's handled separately via leafletProxy) ───────
  build_base_sync_map <- function(df) {
    elephants_in_data <- sort(unique(df$name))
    
    m <- leaflet() %>%
      addProviderTiles("CartoDB.Positron", group = "Light") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("Light", "Satellite"),
        options    = layersControlOptions(collapsed = TRUE)
      )
    
    for (el in elephants_in_data) {
      sub <- df %>% filter(name == el) %>% arrange(datetime_sl)
      sub <- sub[!is.na(sub$lat) & !is.na(sub$lon), ]
      if (nrow(sub) < 2) next
      m <- m %>% addPolylines(
        data = sub, lng = ~lon, lat = ~lat,
        color = get_col(el), weight = 1.5, opacity = 0.35,
        group = "context"
      )
    }
    
    if (nrow(df) > 0) {
      m <- m %>% fitBounds(
        lng1 = min(df$lon, na.rm = TRUE), lat1 = min(df$lat, na.rm = TRUE),
        lng2 = max(df$lon, na.rm = TRUE), lat2 = max(df$lat, na.rm = TRUE)
      )
    }
    
    if (length(elephants_in_data) > 0) {
      m <- m %>% addLegend(
        "bottomright",
        colors  = vapply(elephants_in_data, get_col, character(1)),
        labels  = elephants_in_data,
        title   = "Elephant", opacity = 0.9
      )
    }
    m
  }
  
  output$sync_map_lat  <- renderLeaflet({ build_base_sync_map(agg_data()) })
  output$sync_map_lon  <- renderLeaflet({ build_base_sync_map(agg_data()) })
  output$sync_map_both <- renderLeaflet({ build_base_sync_map(agg_data()) })
  
  # ── Progress layer: on every hover, redraw (via proxy, no full re-render)
  #    each elephant's path up to the hovered time plus a bold "current
  #    position" marker, so consecutive hover points are joined by a line ──
  update_sync_progress <- function(map_id) {
    df <- agg_data()
    ht <- hover_time()
    
    proxy <- leafletProxy(map_id) %>% clearGroup("progress")
    if (is.null(ht) || nrow(df) == 0) return(invisible(NULL))
    
    elephants_in_data <- sort(unique(df$name))
    
    for (el in elephants_in_data) {
      sub <- df %>%
        filter(name == el, datetime_sl <= ht) %>%
        arrange(datetime_sl)
      sub <- sub[!is.na(sub$lat) & !is.na(sub$lon), ]
      if (nrow(sub) == 0) next
      
      clr <- get_col(el)
      
      if (nrow(sub) >= 2) {
        proxy <- proxy %>% addPolylines(
          data = sub, lng = ~lon, lat = ~lat,
          color = clr, weight = 3, opacity = 0.95,
          group = "progress"
        )
      }
      
      cur <- sub[nrow(sub), ]
      proxy <- proxy %>%
        addCircleMarkers(
          data = cur, lng = ~lon, lat = ~lat,
          radius = 7, color = "#ffffff", weight = 2,
          fillColor = clr, fillOpacity = 1,
          group = "progress",
          popup = ~paste0(
            "<b>", name, "</b><br>",
            format(datetime_sl, "%d %b %Y %H:%M"), "<br>",
            "Lat: ", round(lat, 5), "\u00B0N<br>",
            "Lon: ", round(lon, 5), "\u00B0E"
          ),
          label = ~paste0(name, " \u2014 ", format(datetime_sl, "%d %b %Y %H:%M"))
        )
    }
    invisible(NULL)
  }
  
  observeEvent(hover_time(), {
    update_sync_progress("sync_map_lat")
    update_sync_progress("sync_map_lon")
    update_sync_progress("sync_map_both")
  }, ignoreNULL = FALSE)
  
  # ══════════════════════════════════════════════════════════════════════════
  # LIVE ELEPHANT PATH — a dedicated page: pick ONE elephant, then press the
  # slider's play button to watch its GPS path get drawn frame-by-frame on
  # the map, in perfect time-sync with the Latitude/Longitude charts below.
  # ══════════════════════════════════════════════════════════════════════════
  
  # Data for the chosen elephant only, respecting the sidebar's Date Range /
  # Month filters but NOT the multi-elephant "Select Elephants" checklist
  # (so this page always shows whichever elephant you pick here).
  live_base_data <- reactive({
    req(input$live_elephant, input$date_range)
    df <- elephants_df %>%
      filter(
        name == input$live_elephant,
        date_parsed >= input$date_range[1],
        date_parsed <= input$date_range[2]
      )
    if (!is.null(input$live_month) && input$live_month != "all") {
      df <- df %>% filter(format(date_parsed, "%Y-%m") == input$live_month)
    }
    df %>% arrange(datetime_sl)
  })
  
  # ── Playback slider — rebuilt whenever the elephant/date range changes,
  #    so it always spans exactly that elephant's number of GPS fixes ───────
  output$live_slider_ui <- renderUI({
    df <- live_base_data()
    validate(need(nrow(df) > 0, "No GPS fixes for this elephant in the selected date range."))
    sliderInput(
      "live_frame",
      paste0("Fix 1 of ", nrow(df), " \u2014 drag or press \u25B6 to animate"),
      min = 1, max = nrow(df), value = 1, step = 1, width = "100%",
      animate = animationOptions(interval = 250, loop = FALSE)
    )
  })
  
  # ── Convex-hull (home-range) area at every cumulative fix count, computed
  #    once per elephant/month/date-range change (not on every frame tick) ──
  live_hull_series <- reactive({
    df <- live_base_data()
    n  <- nrow(df)
    areas <- rep(NA_real_, n)
    if (n >= 3) {
      for (i in 3:n) {
        h <- tryCatch(mcp_compute_hull(df[seq_len(i), ]), error = function(e) NULL)
        areas[i] <- if (is.null(h)) NA_real_ else h$area_km2
      }
    }
    areas
  })
  
  # ── Current-position info card ──────────────────────────────────────────
  output$live_info_box <- renderUI({
    df <- live_base_data()
    req(nrow(df) > 0, input$live_frame)
    n   <- min(input$live_frame, nrow(df))
    cur <- df[n, ]
    step_km <- if (n > 1) {
      round(mcp_haversine_km(df$lat[n - 1], df$lon[n - 1], df$lat[n], df$lon[n]), 3)
    } else NA_real_
    hull_km2 <- live_hull_series()[n]
    
    tags$div(
      style = "font-size:12px; line-height:1.9; margin-top:4px;",
      tags$p(tags$b("Elephant: "), cur$name),
      tags$p(tags$b("Time: "), format(cur$datetime_sl, "%d %b %Y %H:%M"), " (SL time)"),
      tags$p(tags$b("Latitude: "), round(cur$lat, 5), "\u00B0N"),
      tags$p(tags$b("Longitude: "), round(cur$lon, 5), "\u00B0E"),
      tags$p(tags$b("Step distance: "), if (is.na(step_km)) "\u2014" else paste0(step_km, " km")),
      tags$p(tags$b("Hull area so far: "), if (is.na(hull_km2)) "\u2014 (need \u2265 3 fixes)" else paste0(round(hull_km2, 3), " km\u00B2")),
      tags$p(tags$b("Progress: "), n, " / ", nrow(df), " fixes")
    )
  })
  
  # ── Base map: full faint track for the chosen elephant — rebuilt only
  #    when the elephant or date range changes, not on every frame ─────────
  output$live_map <- renderLeaflet({
    df <- live_base_data()
    validate(need(nrow(df) > 0, "No GPS fixes for this elephant in the selected date range."))
    clr <- get_col(input$live_elephant)
    
    m <- leaflet() %>%
      addProviderTiles("CartoDB.Positron", group = "Light") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("Light", "Satellite"),
        options    = layersControlOptions(collapsed = TRUE)
      ) %>%
      addPolylines(
        data = df, lng = ~lon, lat = ~lat,
        color = clr, weight = 1.5, opacity = 0.25, group = "context"
      ) %>%
      fitBounds(
        lng1 = min(df$lon, na.rm = TRUE), lat1 = min(df$lat, na.rm = TRUE),
        lng2 = max(df$lon, na.rm = TRUE), lat2 = max(df$lat, na.rm = TRUE)
      )
    m
  })
  
  # ── Frame-by-frame progress: fires every time the slider moves (manually
  #    or via the animate/play button), redrawing only the "in-progress"
  #    layer via leafletProxy — this is what makes the path draw live ───────
  observeEvent(input$live_frame, {
    df <- live_base_data()
    req(nrow(df) > 0)
    n   <- min(input$live_frame, nrow(df))
    sub <- df[seq_len(n), ]
    clr <- get_col(input$live_elephant)
    
    proxy <- leafletProxy("live_map") %>% clearGroup("liveprogress")
    
    # Growing convex-hull (home-range) polygon, built only from fixes so far
    if (nrow(sub) >= 3) {
      hull <- tryCatch(mcp_compute_hull(sub), error = function(e) NULL)
      if (!is.null(hull)) {
        proxy <- proxy %>% addPolygons(
          lng = hull$lons, lat = hull$lats,
          color = clr, weight = 1.5, dashArray = "4",
          fillColor = clr, fillOpacity = 0.12,
          group = "liveprogress"
        )
      }
    }
    
    if (nrow(sub) >= 2) {
      proxy <- proxy %>% addPolylines(
        data = sub, lng = ~lon, lat = ~lat,
        color = clr, weight = 3.5, opacity = 0.95, group = "liveprogress"
      )
    }
    cur <- sub[nrow(sub), ]
    proxy %>% addCircleMarkers(
      data = cur, lng = ~lon, lat = ~lat,
      radius = 8, color = "#ffffff", weight = 2,
      fillColor = clr, fillOpacity = 1, group = "liveprogress",
      popup = ~paste0(
        "<b>", name, "</b><br>",
        format(datetime_sl, "%d %b %Y %H:%M"), "<br>",
        "Lat: ", round(lat, 5), "\u00B0N<br>",
        "Lon: ", round(lon, 5), "\u00B0E"
      )
    )
  })
  
  # ── Live home-range (hull) area chart — grows as frames advance ──────────
  output$live_hull_plot <- renderPlotly({
    df <- live_base_data()
    validate(need(nrow(df) >= 3, "Need at least 3 GPS fixes to compute a home-range polygon."))
    req(input$live_frame)
    n     <- min(input$live_frame, nrow(df))
    areas <- live_hull_series()
    clr   <- get_col(input$live_elephant)
    
    full_df <- data.frame(datetime_sl = df$datetime_sl, area_km2 = areas)
    sub_df  <- full_df[seq_len(n), ]
    
    plot_ly() %>%
      add_trace(
        data = full_df, x = ~datetime_sl, y = ~area_km2,
        type = "scatter", mode = "lines", name = "Final",
        line = list(color = clr, width = 1, dash = "dot"),
        opacity = 0.25, hoverinfo = "skip", showlegend = FALSE
      ) %>%
      add_trace(
        data = sub_df, x = ~datetime_sl, y = ~area_km2,
        type = "scatter", mode = "lines+markers", name = "So far",
        line = list(color = clr, width = 2.5),
        marker = list(color = clr, size = 5),
        text = ~paste0(format(datetime_sl, "%d %b %Y %H:%M"), "<br>",
                       "Hull area: ", round(area_km2, 3), " km\u00B2"),
        hoverinfo = "text", showlegend = FALSE
      ) %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
        font  = list(color = "#333333", family = "Segoe UI"),
        xaxis = list(title = "", gridcolor = "#e5e5e5"),
        yaxis = list(title = "Home-range / hull area (km\u00B2)", gridcolor = "#e5e5e5"),
        margin = list(t = 20, b = 40, l = 60, r = 20)
      )
  })
  
  # ── Live Latitude / Longitude vs time charts — revealed up to the
  #    current frame, so the line is drawn in step with the map ────────────
  make_live_plot <- function(df, sub, y_col, y_title, clr) {
    plot_ly() %>%
      add_trace(
        data = df, x = ~datetime_sl, y = as.formula(paste0("~", y_col)),
        type = "scatter", mode = "lines", name = "Full track",
        line = list(color = clr, width = 1, dash = "dot"),
        opacity = 0.25, hoverinfo = "skip", showlegend = FALSE
      ) %>%
      add_trace(
        data = sub, x = ~datetime_sl, y = as.formula(paste0("~", y_col)),
        type = "scatter", mode = "lines+markers", name = "Travelled so far",
        line = list(color = clr, width = 2.5),
        marker = list(color = clr, size = 5),
        text = ~paste0(format(datetime_sl, "%d %b %Y %H:%M"), "<br>",
                       y_title, ": ", round(get(y_col), 5), "\u00B0"),
        hoverinfo = "text", showlegend = FALSE
      ) %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
        font  = list(color = "#333333", family = "Segoe UI"),
        xaxis = list(title = "", gridcolor = "#e5e5e5"),
        yaxis = list(title = y_title, gridcolor = "#e5e5e5"),
        margin = list(t = 20, b = 40, l = 60, r = 20)
      )
  }
  
  output$live_lat_plot <- renderPlotly({
    df <- live_base_data()
    validate(need(nrow(df) > 0, "No data."))
    req(input$live_frame)
    n   <- min(input$live_frame, nrow(df))
    make_live_plot(df, df[seq_len(n), ], "lat", "Latitude (\u00B0N)", get_col(input$live_elephant))
  })
  
  output$live_lon_plot <- renderPlotly({
    df <- live_base_data()
    validate(need(nrow(df) > 0, "No data."))
    req(input$live_frame)
    n   <- min(input$live_frame, nrow(df))
    make_live_plot(df, df[seq_len(n), ], "lon", "Longitude (\u00B0E)", get_col(input$live_elephant))
  })
  
  # ── Small static reference map: Kaudulla Tank & park boundary ──────────────
  # This is the same geographic anchor (tank + N/S/E/W boundary lines) used
  # for every dashed reference line on the Latitude, Longitude, and
  # Both-Coordinates charts above — shown here as an actual map for context.
  output$kaudulla_ref_map <- renderLeaflet({
    
    # Approximate park boundary (rectangle) — matches the reference lines
    # used in make_plot()/make_dual_axis_plot(): lat 8.080-8.220, lon 80.872-80.950
    park_boundary <- data.frame(
      lon = c(80.872, 80.950, 80.950, 80.872, 80.872),
      lat = c(8.080,  8.080,  8.220,  8.220,  8.080)
    )
    
    # Points #1-3 — same three rows at the top of the Key Coordinates table
    ref_points <- data.frame(
      num  = c("1", "2", "3"),
      name = c("Kaudulla Tank (core reference)",
               "Park entrance / safari zone",
               "Kaudulla Wewa (mapped reservoir)"),
      lat  = c(8.140, 8.111, 8.168),
      lon  = c(80.895, 80.886, 80.926),
      note = c("Dry-season water source \u2014 latitude/longitude reference lines pivot around this point.",
               "Southwestern edge of the elephants' core range; jeep safari staging area.",
               "Northeastern shoreline of the reservoir; frequent dry-season gathering point.")
    )
    
    # Boundary edges #4-7 — same four rows at the bottom of the Key Coordinates
    # table. Each is drawn as its own highlighted edge (not just the faint
    # rectangle) with a numbered badge at its midpoint, so every table row has
    # a directly matching feature on the map.
    ref_edges <- list(
      list(num = "4", name = "Southern park boundary",
           lng = c(80.872, 80.950), lat = c(8.080, 8.080),
           mid_lng = 80.911, mid_lat = 8.080),
      list(num = "5", name = "Northern park boundary",
           lng = c(80.872, 80.950), lat = c(8.220, 8.220),
           mid_lng = 80.911, mid_lat = 8.220),
      list(num = "6", name = "Eastern boundary (HEC zone)",
           lng = c(80.950, 80.950), lat = c(8.080, 8.220),
           mid_lng = 80.950, mid_lat = 8.150),
      list(num = "7", name = "Western park boundary",
           lng = c(80.872, 80.872), lat = c(8.080, 8.220),
           mid_lng = 80.872, mid_lat = 8.150)
    )
    
    badge_label <- function(num, cls) {
      htmltools::HTML(paste0("<span class='ref-badge ", cls, "'>", num, "</span>"))
    }
    
    m <- leaflet() |>
      addProviderTiles(providers$OpenStreetMap) |>
      addPolygons(
        data = park_boundary, lng = ~lon, lat = ~lat,
        color = "#2e7d32", weight = 1, dashArray = "6 4",
        fill = TRUE, fillColor = "#2e7d32", fillOpacity = 0.06,
        label = "Kaudulla National Park \u2014 approximate boundary used for all reference lines"
      )
    
    # Boundary edges (#4-7): highlighted orange segment + numbered badge
    for (e in ref_edges) {
      m <- m |>
        addPolylines(
          lng = e$lng, lat = e$lat,
          color = "#c1440e", weight = 4, opacity = 0.85, dashArray = "8 5",
          label = paste0("#", e$num, " \u2014 ", e$name)
        ) |>
        addLabelOnlyMarkers(
          lng = e$mid_lng, lat = e$mid_lat,
          label = badge_label(e$num, "boundary"),
          labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE,
                                      direction = "center", className = "leaflet-div-badge")
        )
    }
    
    # Point features (#1-3): teal circle marker + numbered badge
    m <- m |>
      addCircleMarkers(
        data = ref_points, lng = ~lon, lat = ~lat,
        radius = 9, color = "#0f766e", fillColor = "#4fc3f7",
        fillOpacity = 0.9, stroke = TRUE, weight = 2,
        popup = ~paste0("<b>#", num, " \u2014 ", name, "</b><br>", round(lat, 3), "\u00B0N, ", round(lon, 3), "\u00B0E<br>", note)
      ) |>
      addLabelOnlyMarkers(
        data = ref_points, lng = ~lon, lat = ~lat,
        label = ~lapply(num, badge_label, cls = "core"),
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE,
                                    direction = "center", className = "leaflet-div-badge")
      ) |>
      addLegend(
        position = "bottomright",
        colors = c("#0f766e", "#c1440e"),
        labels = c("Key point (badges 1\u20133)", "Boundary line (badges 4\u20137)"),
        opacity = 0.9
      ) |>
      setView(lng = 80.905, lat = 8.15, zoom = 12)
    
    m
  })
  
  # ── Monthly aggregation for heat maps ───────────────────────────────────────
  heat_data <- reactive({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    
    df <- df %>% mutate(ym = format(datetime_sl, "%Y-%m"))
    months_seq <- format(
      seq(as.Date(format(min(df$datetime_sl), "%Y-%m-01")),
          as.Date(format(max(df$datetime_sl), "%Y-%m-01")), by = "month"), "%Y-%m")
    names_seq <- sort(unique(df$name))
    
    agg <- df %>% group_by(name, ym) %>%
      summarise(mlon = mean(lon, na.rm = TRUE), n = n(), .groups = "drop")
    
    mat_lon <- matrix(NA_real_, length(names_seq), length(months_seq),
                      dimnames = list(names_seq, months_seq))
    mat_n <- mat_lon
    for (i in seq_len(nrow(agg))) {
      mat_lon[agg$name[i], agg$ym[i]] <- agg$mlon[i]
      mat_n[agg$name[i],   agg$ym[i]] <- agg$n[i]
    }
    list(months = months_seq, names = names_seq, mat_lon = mat_lon, mat_n = mat_n)
  })
  
  output$heat_lon <- renderPlotly({
    hd <- heat_data()
    plot_ly(x = hd$months, y = hd$names, z = hd$mat_lon, type = "heatmap",
            colors = colorRamp(c("#f1faee", "#2a9d8f", "#e9c46a", "#e63946")),
            hoverongaps = FALSE,
            colorbar = list(title = "Mean\nLon (\u00B0E)",
                            tickfont = list(color = "#333333"),
                            titlefont = list(color = "#333333")),
            hovertemplate = "%{y}<br>%{x}<br>Mean lon %{z:.4f}\u00B0E<extra></extra>") %>%
      layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             font = list(color = "#333333", family = "Segoe UI"),
             xaxis = list(title = "Month", tickangle = -45, gridcolor = "#e5e5e5"),
             yaxis = list(title = "", autorange = "reversed", gridcolor = "#e5e5e5"),
             margin = list(t = 20, b = 70, l = 110, r = 20)) %>%
      config(displaylogo = FALSE)
  })
  
  output$heat_n <- renderPlotly({
    hd <- heat_data()
    plot_ly(x = hd$months, y = hd$names, z = hd$mat_n, type = "heatmap",
            colors = colorRamp(c("#f1faee", "#ff9f1c", "#e63946")),
            hoverongaps = FALSE,
            colorbar = list(title = "GPS\nFixes",
                            tickfont = list(color = "#333333"),
                            titlefont = list(color = "#333333")),
            hovertemplate = "%{y}<br>%{x}<br>%{z} fixes<extra></extra>") %>%
      layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             font = list(color = "#333333", family = "Segoe UI"),
             xaxis = list(title = "Month", tickangle = -45, gridcolor = "#e5e5e5"),
             yaxis = list(title = "", autorange = "reversed", gridcolor = "#e5e5e5"),
             margin = list(t = 20, b = 70, l = 110, r = 20)) %>%
      config(displaylogo = FALSE)
  })
  
  
  # ── Elephant tracking ───────────────────────────────────────────────────────────────
  # Color palette for tracking
  color_palette_tracking <- colorFactor(
    palette = elephant_colors,
    domain = df_sf$name
  )
  
  # Tracking Map
  output$tracking_map <- renderLeaflet({
    leaflet(df_sf) %>%
      addProviderTiles(providers$OpenStreetMap, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      
      # POINTS
      addCircleMarkers(
        color = ~color_palette_tracking(name),
        radius = 4,
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~paste("<b>Date:</b>", datetime,
                       "<br><b>Gender:</b>", sex,
                       "<br><b>Name:</b>", name)
      ) %>%
      
      # LEGEND
      addLegend(
        pal = color_palette_tracking,
        values = ~name,
        title = "Elephant Name",
        position = "bottomright"
      ) %>%
      
      # LAYERS CONTROL
      addLayersControl(
        baseGroups = c("Street Map", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # GPS by elephants
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
  
  
  
  # ── Migration & Climate ───────────────────────────────────────────────────────────────
  
  gps_data_reactive <- reactive({
    req(input$selected_elephant)
    
    elephants %>%
      filter(name == input$selected_elephant) %>%
      group_by(date) %>%
      summarise(
        valid_records = sum(!is.na(lat) & !is.na(lon)),
        availability = pmin(100, 100 * valid_records / 24),
        .groups = "drop"
      )
  })
  
  output$calendar_plot <- renderPlot({
    df <- gps_data_reactive()
    
    validate(
      need(nrow(df) > 0, "No data available for the selected elephant.")
    )
    
    # We execute the function directly within the renderPlot space
    calendarHeat(
      dates = df$date,
      values = df$availability,
      at = my_ranges,
      colors = my_colors2,
      title = paste("Daily GPS Availability Calendar Heatmap -", input$selected_elephant),
      colorkey = FALSE,
      legend = list(
        right = list(
          fun = draw.key,
          args = list(key = discrete_key)
        )
      )
    )
  })
  
  # populate year dropdown
  observe({
    updateSelectInput(session, "year",
                      choices = sort(unique(df_sf$year)))
    
    updateSelectInput(
      session,
      "month",
      choices  = setNames(sprintf("%02d", 1:12), month.name),
      selected = sprintf("%02d", 1:12)[1]
    )
    
    updateSelectInput(session, "elephant",
                      choices = sort(unique(df_sf$name)))
  })
  
  # reactive filtered dataset (IMPORTANT FIX)
  df_filtered <- reactive({
    req(input$year, input$month, input$elephant, input$select_week)
    
    d <- df_sf |>
      filter(
        year == input$year,
        month %in% input$month,
        name == input$elephant
      )
    
    # If one or more SPECIFIC weeks are chosen (i.e. "All Weeks" isn't
    # among the picks), narrow down to just those weeks. Start/end markers
    # in build_migration_map() are computed from whatever is returned here,
    # so they automatically reflect the start/end of the chosen week(s).
    if (!("All Weeks" %in% input$select_week)) {
      d <- d |> filter(week_of_month %in% input$select_week)
    }
    
    d
  })
  
  # Build the migration map for a given filtered dataset (reused for
  # both the live app render AND the "open in new tab" export)
  build_migration_map <- function(dat, show_numbers = FALSE) {
    
    if (nrow(dat) == 0) {
      return(
        leaflet() |>
          addProviderTiles(providers$OpenStreetMap) |>
          addPopups(
            lng = 80.0,
            lat = 7.0,
            popup = "<b>No elephant data available for selected year & month</b>"
          ) |>
          setView(lng = 80.0, lat = 7.0, zoom = 7)
      )
    }
    
    elephant_list <- sort(unique(dat$name))
    
    m <- leaflet(dat) |>
      addProviderTiles(providers$OpenStreetMap)
    
    for (e in elephant_list) {
      
      d <- dat |>
        filter(name == e) |>
        arrange(datetime)
      
      # Points plotted ONE BY ONE in chronological order (not connected by
      # lines), all the SAME fixed size. Direction/order is conveyed via
      # the optional sequence-number labels below, not by point size.
      n_pts <- nrow(d)
      
      for (i in seq_len(n_pts)) {
        row_i <- d[i, ]
        m <- m |>
          addCircleMarkers(
            data = row_i,
            group = e,
            color = ~pal_week(week_of_month),
            radius = 5,
            stroke = FALSE,
            fillOpacity = 1,
            popup = ~paste0(
              "<b>Elephant:</b> ", name,
              "<br><b>Sequence:</b> ", i, " of ", n_pts,
              "<br><b>Date:</b> ", datetime,
              "<br><b>Week:</b> ", week_of_month,
              "<br><b>Year:</b> ", year,
              "<br><b>Month:</b> ", month
            )
          )
        
        # Optional sequence-number label right on top of each point,
        # toggled by the "Show point sequence numbers" checkbox.
        if (isTRUE(show_numbers)) {
          m <- m |>
            addLabelOnlyMarkers(
              data = row_i,
              label = as.character(i),
              labelOptions = labelOptions(
                noHide = TRUE,
                textOnly = TRUE,
                direction = "top",
                offset = c(0, -8),
                style = list(
                  "font-weight" = "bold",
                  "font-size"   = "12px",
                  "color"       = "black",
                  "text-shadow" = "-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff"
                )
              )
            )
        }
      }
      
      # ── Start / End markers ─────────────────────────────────────
      start_pt <- d[1, ]
      end_pt   <- d[nrow(d), ]
      
      m <- m |>
        addAwesomeMarkers(
          data = start_pt,
          icon = awesomeIcons(icon = "play", library = "fa",
                              markerColor = "green", iconColor = "white"),
          popup = ~paste0("<b>", e, "</b><br>Start: ", datetime)
        ) |>
        addAwesomeMarkers(
          data = end_pt,
          icon = awesomeIcons(icon = "flag-checkered", library = "fa",
                              markerColor = "red", iconColor = "white"),
          popup = ~paste0("<b>", e, "</b><br>End: ", datetime)
        )
    }
    
    m <- m |>
      addLegend(
        pal = pal_week,
        values = dat$week_of_month,
        title = "Week of Month",
        position = "bottomright"
      )
    
    m
  }
  
  output$map <- renderLeaflet({
    build_migration_map(df_filtered(), show_numbers = input$show_seq_numbers)
  })
  
  # ── Open migration map in a separate browser tab ─────────────────
  observeEvent(input$open_map_newtab, {
    m <- build_migration_map(df_filtered(), show_numbers = input$show_seq_numbers)
    export_path <- file.path(tempdir(), "migration_map_popup.html")
    htmlwidgets::saveWidget(m, export_path, selfcontained = TRUE)
    addResourcePath("mapexport", tempdir())
    runjs(sprintf("window.open('mapexport/%s', '_blank');",
                  basename(export_path)))
  })
  
  # HEATMAP
  output$summaryTable <- renderTable({
    info <- plot_info[[input$variable]]
    vals <- as.numeric(info$values)
    yrs  <- format(dates, "%Y")
    
    summary_by_year <- function(v) {
      c(
        min(v, na.rm = TRUE),
        quantile(v, 0.25, na.rm = TRUE),
        median(v, na.rm = TRUE),
        mean(v, na.rm = TRUE),
        quantile(v, 0.75, na.rm = TRUE),
        max(v, na.rm = TRUE),
        sd(v, na.rm = TRUE),
        sum(is.na(v))
      )
    }
    
    yr_levels <- sort(unique(yrs))
    
    out <- data.frame(
      Measure = c(
        "Minimum", "First Quantile (Q1)", "Median",
        "Mean", "Third Quantile (Q3)", "Maximum",
        "Standard Deviation", "Missing Values (NA)"
      )
    )
    
    for (y in yr_levels) {
      out[[y]] <- summary_by_year(vals[yrs == y])
    }
    
    out
  },
  digits = 2,
  colnames = TRUE,   # now show year headers
  striped = FALSE,
  bordered = FALSE,
  width = "100%"
  )
  
  output$calendarPlot <- renderPlot({
    info <- plot_info[[input$variable]]
    calendar_key <- list(
      space = "right",
      rectangles = list(col = my_colors, border = "black", size = 4),
      text = list(info$labels, cex = 1.1),
      padding.text = 4,
      columns = 1
    )
    
    calendarHeat(
      dates = dates, values = info$values, at = info$breaks, colors = my_colors,
      title = info$title, colorkey = FALSE,
      legend = list(right = list(fun = draw.key, args = list(key = calendar_key)))
    )
  })
  
  # ── Data table ───────────────────────────────────────────────────────────────
  output$data_table <- renderDT({
    df <- filtered() %>%
      select(name, sex, datetime_sl, lat, lon) %>%
      mutate(datetime_sl = format(datetime_sl, "%d %b %Y %H:%M"),
             lat = round(lat, 6), lon = round(lon, 6)) %>%
      rename(Elephant = name, Sex = sex, `Date/Time (SL)` = datetime_sl,
             Latitude = lat, Longitude = lon)
    
    datatable(df,
              options = list(pageLength = 20, scrollX = TRUE,
                             dom = "Bfrtip", buttons = c("csv", "excel")),
              rownames = FALSE, class = "stripe hover", extensions = "Buttons")
  })
  # ══════════════════════════════════════════════════════════════════════════
  # Home Range & Speed module (from app (6).R)
  # ══════════════════════════════════════════════════════════════════════════
  
  # ---- Filtered data (now driven by the GLOBAL sidebar: elephant, date, month) ----
  mcp_filtered_data <- reactive({
    req(input$date_range)
    
    df <- mcp_tracking_clean %>%
      filter(
        sex %in% input$mcp_sex_filter,
        as.Date(datetime) >= input$date_range[1],
        as.Date(datetime) <= input$date_range[2]
      )
    
    if (length(input$sel_elephants) > 0) {
      df <- df %>% filter(name %in% input$sel_elephants)
    } else {
      df <- df[0, ]
    }
    
    if (!is.null(input$sel_month) && input$sel_month != "all") {
      df <- df %>% filter(format(datetime, "%Y-%m") == input$sel_month)
    }
    
    df
  })
  
  # ---- Hull + summary ----
  mcp_hull_results <- reactive({
    df <- mcp_filtered_data()
    elephants_present <- sort(unique(df$name))
    hulls <- list()
    summary_rows <- list()
    
    for (elephant in elephants_present) {
      edata <- df %>% filter(name == elephant)
      h <- mcp_compute_hull(edata)
      step_dist <- edata$step_km[is.finite(edata$step_km)]
      step_speed <- edata$speed_kmh[is.finite(edata$speed_kmh)]
      
      hulls[[elephant]] <- h
      
      area_km2_val <- if (is.null(h)) NA_real_ else round(h$area_km2, 3)
      area_hectares_val <- if (is.null(h)) NA_real_ else round(h$area_km2 * 100, 0)
      
      days_tracked <- as.numeric(
        difftime(max(edata$datetime), min(edata$datetime), units = "days")
      )
      
      summary_rows[[elephant]] <- data.frame(
        name          = elephant,
        sex           = edata$sex[1],
        n_points      = nrow(edata),
        area_km2      = area_km2_val,
        area_hectares = area_hectares_val,
        total_dist_km = round(sum(step_dist), 1),
        km_per_day    = round(sum(step_dist) / max(days_tracked, 1), 2),
        max_step_km   = round(suppressWarnings(max(step_dist)), 2),
        avg_speed_kmh = round(suppressWarnings(mean(step_speed)), 2),
        max_speed_kmh = round(suppressWarnings(max(step_speed)), 2),
        days_tracked  = round(days_tracked, 0)
      )
    }
    
    list(
      hulls = hulls,
      summary = if (length(summary_rows) > 0) {
        bind_rows(summary_rows) %>% arrange(desc(area_km2))
      } else {
        data.frame()
      }
    )
  })
  
  # ---- Value boxes ----
  output$mcp_vb_elephants <- renderValueBox({
    valueBox(length(unique(mcp_filtered_data()$name)), "Elephants Shown",
             icon = icon("signs-post"), color = "green"
    )
  })
  
  output$mcp_vb_points <- renderValueBox({
    n <- nrow(mcp_filtered_data())
    txt <- if (n == 0) "\u2014" else format(n, big.mark = ",")
    valueBox(txt, "No. of GPS Fixes", icon = icon("location-dot"), color = "blue")
  })
  
  output$mcp_vb_distance <- renderValueBox({
    d <- mcp_filtered_data()$step_km
    d <- d[is.finite(d)]
    txt <- if (length(d) == 0) "\u2014" else paste0(format(round(sum(d), 1), big.mark = ","), " km")
    valueBox(txt, "Total Distance Covered", icon = icon("route"), color = "purple")
  })
  
  output$mcp_vb_speed <- renderValueBox({
    s <- mcp_filtered_data()$speed_kmh
    s <- s[is.finite(s)]
    txt <- if (length(s) == 0) "\u2014" else paste0(round(mean(s), 2), " km/h")
    valueBox(txt, "Avg. Speed", icon = icon("gauge-high"), color = "teal")
  })
  
  
  
  # ---- Map (base rendered once, contents updated via proxy) ----
  output$mcp_hull_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", group = "Light") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Street") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      setView(
        lng  = mean(mcp_tracking_clean$lon, na.rm = TRUE),
        lat  = mean(mcp_tracking_clean$lat, na.rm = TRUE),
        zoom = 12
      ) %>%
      addScaleBar(position = "bottomleft")
  })
  
  observe({
    df <- mcp_filtered_data()
    res <- mcp_hull_results()
    elephants_present <- sort(unique(df$name))
    
    # Clear every possible hull group (one per elephant) plus points/centers,
    # using their ACTUAL group names -- "hulls"/"points"/"centers" never
    # matched anything that was actually added, so old hulls from previously
    # selected elephants were never being removed.
    all_hull_groups <- paste(mcp_unique_elephants, "- Hull")
    
    proxy <- leafletProxy("mcp_hull_map") %>%
      clearGroup(all_hull_groups) %>%
      clearGroup("All GPS Points") %>%
      clearGroup("All Centers") %>%
      clearControls()
    
    if (length(elephants_present) == 0) {
      return()
    }
    
    for (elephant in elephants_present) {
      edata <- df %>% filter(name == elephant)
      color <- mcp_elephant_colors[[elephant]]
      h <- res$hulls[[elephant]]
      area_info <- res$summary %>% filter(name == elephant)
      
      if (!is.null(h) && nrow(area_info) > 0) {
        popup_text <- paste0(
          "<div style='width:220px;'>",
          "<h4>", elephant, " &mdash; MCP</h4>",
          "<b>Sex:</b> ", area_info$sex, "<br>",
          "<b>Area:</b> ", area_info$area_km2, " km&sup2; (",
          format(area_info$area_hectares, big.mark = ","), " ha)<br>",
          "<b>GPS fixes:</b> ", format(area_info$n_points, big.mark = ","), "<br>",
          "<b>Total distance:</b> ", area_info$total_dist_km, " km<br>",
          "<b>Avg speed:</b> ", area_info$avg_speed_kmh, " km/h<br>",
          "</div>"
        )
        proxy <- proxy %>%
          addPolygons(
            lng = h$lons, lat = h$lats,
            color = color, weight = 2, opacity = 1,
            fillColor = color, fillOpacity = 0.15,
            popup = popup_text,
            group = paste(elephant, "- Hull"),
            layerId = paste0("mcp_hull_", elephant)
          )
      }
      
      proxy <- proxy %>%
        addCircleMarkers(
          data = edata, lng = ~lon, lat = ~lat,
          popup = ~ paste0("<b>", name, "</b><br>", datetime),
          label = elephant, radius = 3,
          color = color, fillColor = color,
          fillOpacity = 0.6, weight = 1, stroke = TRUE,
          group = "All GPS Points"
        ) %>%
        addCircleMarkers(
          lng = mean(edata$lon), lat = mean(edata$lat),
          radius = 7, color = "#FFFFFF",
          fillColor = color, fillOpacity = 1, weight = 2,
          popup = paste("<b>Center:</b>", elephant),
          group = "All Centers"
        )
    }
    
    hull_groups <- paste(elephants_present, "- Hull")
    
    proxy %>%
      addLayersControl(
        baseGroups    = c("Light", "Street", "Satellite"),
        overlayGroups = c(hull_groups, "All GPS Points", "All Centers"),
        options       = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("All GPS Points", "All Centers")) %>%
      addLegend(
        "bottomright",
        colors  = unname(mcp_elephant_colors[elephants_present]),
        labels  = elephants_present,
        title   = "Elephant",
        opacity = 0.8
      )
  })
  
  # ---- Movement timeline ----
  output$mcp_timeline_plot <- renderPlotly({
    df <- mcp_filtered_data() %>%
      filter(!is.na(step_km)) %>%
      arrange(name, datetime) %>%
      group_by(name) %>%
      mutate(cum_dist_km = cumsum(coalesce(step_km, 0))) %>%
      ungroup()
    
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    
    plot_ly(
      df,
      x = ~datetime, y = ~cum_dist_km, color = ~name,
      colors = mcp_elephant_colors[unique(df$name)],
      type = "scatter", mode = "lines",
      hovertemplate = "<b>%{fullData.name}</b><br>%{x}<br>Cumulative: %{y:.1f} km<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "", gridcolor = "rgba(0,0,0,0.08)"),
        yaxis = list(title = "Cumulative distance (km)", gridcolor = "rgba(0,0,0,0.08)"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#333333"),
        legend = list(orientation = "h", y = -0.2)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ---- Area bar chart ----
  output$mcp_area_bar_chart <- renderPlotly({
    res <- mcp_hull_results()$summary %>% filter(!is.na(area_km2))
    validate(need(nrow(res) > 0, "No elephant in this selection has enough GPS fixes for a home range."))
    
    bar_df <- res %>% mutate(name = factor(name, levels = rev(name)))
    
    plot_ly(
      bar_df,
      x = ~area_km2, y = ~name, type = "bar", orientation = "h",
      marker = list(color = unname(mcp_elephant_colors[as.character(bar_df$name)])),
      text = ~ paste0(area_km2, " km²"),
      textposition = "auto",
      textfont = list(color = "#0D0D0D"),
      hovertemplate = "<b>%{y}</b><br>Area: %{x} km²<extra></extra>"
    ) %>%
      layout(
        xaxis = list(
          title     = "Area (km²)",
          gridcolor = "rgba(0,0,0,0.08)",
          range     = c(0, max(bar_df$area_km2, na.rm = TRUE) * 1.15)
        ),
        yaxis = list(title = "", automargin = TRUE),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#333333"),
        margin = list(l = 10, r = 20, t = 10, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ---- Individual rose plots ----
  output$mcp_rose_individual_ui <- renderUI({
    df <- mcp_filtered_data() %>% filter(is.finite(bearing))
    validate(need(nrow(df) > 0, "No movement data for the selected filters."))
    
    elephants_present <- sort(unique(df$name))
    n <- length(elephants_present)
    n_cols <- 2
    n_rows <- ceiling(n / n_cols)
    plot_height <- paste0(max(250, min(350, 900 / n_rows)), "px")
    
    rows <- lapply(seq_len(n_rows), function(row_i) {
      idx <- ((row_i - 1) * n_cols + 1):min(row_i * n_cols, n)
      fluidRow(
        lapply(elephants_present[idx], function(elephant) {
          column(6, plotlyOutput(
            outputId = paste0("mcp_rose_", gsub("[^A-Za-z0-9]", "_", elephant)),
            height   = plot_height
          ))
        })
      )
    })
    
    tagList(rows)
  })
  
  observe({
    df <- mcp_filtered_data() %>% filter(is.finite(bearing))
    elephants_present <- sort(unique(df$name))
    
    lapply(elephants_present, function(elephant) {
      local({
        el <- elephant
        col <- mcp_elephant_colors[[el]]
        output_id <- paste0("mcp_rose_", gsub("[^A-Za-z0-9]", "_", el))
        
        output[[output_id]] <- renderPlotly({
          edata <- df %>% filter(name == el)
          validate(need(nrow(edata) > 1, paste(el, ": not enough data")))
          mcp_make_rose_plot(edata$bearing, col, el)
        })
      })
    })
  })
  
  # ---- Population rose plot ----
  output$mcp_rose_population <- renderPlotly({
    df <- mcp_filtered_data() %>% filter(is.finite(bearing))
    validate(need(nrow(df) > 0, "No movement data for the selected filters."))
    
    bd <- mcp_bin_bearings(df$bearing)
    
    plot_ly(
      bd,
      type = "barpolar",
      r = ~r, theta = ~theta,
      marker = list(
        color = ~r,
        colorscale = list(
          c(0, "#1a237e"), c(0.25, "#1565C0"),
          c(0.5, "#00BCD4"), c(0.75, "#4CAF50"),
          c(1, "#FF5252")
        ),
        showscale = TRUE,
        colorbar = list(title = "Fixes", tickfont = list(color = "#333333"))
      ),
      hovertemplate = "%{theta}°: %{r} fixes<extra></extra>"
    ) %>%
      layout(
        polar = list(
          angularaxis = list(
            tickmode  = "array",
            tickvals  = c(0, 45, 90, 135, 180, 225, 270, 315),
            ticktext  = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
            direction = "clockwise",
            rotation  = 90,
            gridcolor = "rgba(0,0,0,0.12)",
            linecolor = "rgba(0,0,0,0.2)",
            tickfont  = list(color = "#333333", size = 14)
          ),
          radialaxis = list(
            gridcolor = "rgba(0,0,0,0.08)",
            linecolor = "rgba(0,0,0,0.1)",
            tickfont  = list(color = "#666", size = 10)
          ),
          bgcolor = "rgba(0,0,0,0)"
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#333333"),
        showlegend = FALSE,
        margin = list(l = 60, r = 60, t = 40, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ---- Summary table ----
  output$mcp_summary_table <- renderDT({
    res <- mcp_hull_results()$summary
    validate(need(nrow(res) > 0, "No data for the selected filters."))
    
    res %>%
      select(
        Elephant           = name,
        Sex                = sex,
        `Days Tracked`     = days_tracked,
        `Total Dist. (km)` = total_dist_km,
        `km / day`         = km_per_day,
        `Max Step (km)`    = max_step_km,
        `GPS Fixes`        = n_points,
        `Area (km²)`       = area_km2,
        `Area (ha)`        = area_hectares,
        `Avg Speed (km/h)` = avg_speed_kmh,
        `Max Speed (km/h)` = max_speed_kmh
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 14,
          dom        = "ft",
          order      = list(list(3, "desc")), # 0-based: col 3 = Total Dist.
          scrollX    = TRUE
        ),
        class = "display compact"
      )
  })
  
}

# ── Run ────────────────────────────────────────────────────────────────────────
shinyApp(ui, server)