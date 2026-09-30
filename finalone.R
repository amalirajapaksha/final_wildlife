library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(dplyr)
library(sf)
library(readr)
library(plotly)
library(adehabitatHR)
library(leaflet)
library(htmltools)
library(yyjsonr)
library(shiny)
library(readxl)
library(lattice)
library(grid)
library(ggplot2)  
library(lubridate)



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
  if ("imputed" %in% names(na_rows)) na_rows$imputed <- FALSE
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
    year_month = format(datetime, "%Y-%m")
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

elephants <- sort(unique(df_sf$name))

# Colors for elephant tracking
elephant_colors <- c(
  "#e6194b", "#3cb44b", "#ffe119", "#4363d8", "#f58231",
  "#911eb4", "#46f0f0", "#f032e6", "#bcf60c", "#fabebe",
  "#008080", "#e6beff", "#9a6324", "#fffac8", "#800000"
)





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
                              aspect=.12,
                              layout = c(1, nyr%%7),
                              between = list(x=0, y=c(0.5,0.5)),
                              strip=TRUE,
                              main = ifelse(missing(title), "", title),
                              scales = list(
                                x = list(
                                  at= c(seq(2.9, 52, by=4.42)),
                                  labels = month.abb,
                                  alternating = c(1, rep(0, (nyr-1))),
                                  tck=0,
                                  cex = 0.7),
                                y=list(
                                  at = c(0, 1, 2, 3, 4, 5, 6),
                                  labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                                             "Friday", "Saturday"),
                                  alternating = 1,
                                  cex = 0.6,
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

my_colors2 <- c("#7F0000", "#E34A33", "#FDAE61", "#FFFF99", "#78C679", "#006400")
my_ranges <- c(0, 1, 25, 50, 75, 99.999, 100)
range_labels <- c("0 %", "1 - 25 %", "25 - 50 %", "50 - 75 %", "75 - 99 %", "100 %")

discrete_key <- list(
  space = "right",
  rectangles = list(col = my_colors2, border = "black", size = 2),
  text = list(range_labels, cex = 0.8),
  padding.text = 3,
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
      menuItem("Migration & Climate", tabName = "climate_tab",icon = icon("globe")),
      menuItem("Data Table",         tabName = "data_tab", icon = icon("table"))
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
             
             checkboxInput("show_imputed", "Show imputed points", value = TRUE),
             
             tags$hr(style = "border-color:#444; margin:4px 0;"),
             
             dateRangeInput(
               "date_range", "Date Range",
               start = min(elephants_df$date_parsed, na.rm = TRUE),
               end   = max(elephants_df$date_parsed, na.rm = TRUE),
               min   = min(elephants_df$date_parsed, na.rm = TRUE),
               max   = max(elephants_df$date_parsed, na.rm = TRUE),
               format = "dd M yyyy"
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
              div(class = "panel-box",
                  
                  div(class = "section-title",
                      "🗺️ Elephant Tracking Overview"
                  ),
                  
                  div(class = "sub-title",
                      "Interactive Map with Satellite View"
                  ),
                  
                  leafletOutput("tracking_map", height = 600)
              ),
              
              div(class = "panel-box",
                  
                  div(class = "sub-title",
                      "📊 Tracking Data Visualization"
                  ),
                  
                  plotOutput("tracking_plot", height = 500, width = "100%")
              ),
              
              div(class = "panel-box",
                  
                  div(class = "sub-title",
                      "👥 GPS Tracking Data by Individual Elephant"
                  ),
                  
                  plotOutput("tracking_by_elephant", height = 800, width = "100%")
              )
            ),
      
      
      # ── TAB 6 : Migration & Climate ────────────────────────────────────────────────────
      tabItem("climate_tab",
              div(class = "panel-box",
                  
                  div(class = "section-title",
                      "🐘 Elephant Tracking Data Availability"
                  ),
                  sidebarLayout(
                    sidebarPanel(
                      selectInput(
                        inputId = "selected_elephant",
                        label = "Select Elephant Name:",
                        choices = elephant_names,
                        selected = elephant_names[1]
                      ),
                      hr(),
                      helpText("This heatmap shows the percentage of valid GPS records captured per day (max 24 records/day).")
                    ),
                    
                    mainPanel(
                      plotOutput("calendar_plot", height = "600px")
                    )
                  )
              ),
             
              
              div(class = "panel-box",
                  
                  div(class = "section-title",
                      "🐘 Elephant Migration Map"
                  ),
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("year", "Select Year", choices = NULL),
                      selectInput(
                        "month",
                        "Select Month",
                        choices = sprintf("%02d", 1:12),
                        multiple = TRUE
                      ),
                      selectInput("elephant", "Select Elephant", choices = NULL)
                    ),
                    
                    mainPanel(
                      leafletOutput("map", height = 600)
                    )
                  )
              ),
              
              
              div(class = "panel-box",
                  
                  div(class = "section-title",
                      "🌡 Climate Calendar Analysis"
                  ),
                  
                  sidebarLayout(
                    
                    sidebarPanel(
                      selectInput("variable",
                                  "Climate Variable",
                                  choices = names(plot_info)),
                      
                      h4("Summary Statistics"),
                      tableOutput("summaryTable")
                    ),
                    
                    mainPanel(
                      plotOutput("calendarPlot", height = 500)
                    )
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
    
    if (!input$show_imputed) df <- df %>% filter(!imputed)
    
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
                imputed = any(imputed),
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
  make_plot <- function(df, y_col, y_title, ref_lines = NULL) {
    elephants_in_data <- unique(df$name)
    col_map <- ELEPHANT_COLOURS[names(ELEPHANT_COLOURS) %in% elephants_in_data]
    
    p <- plot_ly()
    
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
          color   = ~ifelse(imputed, "#888888", clr),
          size    = ~ifelse(imputed, 5, 4),
          opacity = 0.85,
          symbol  = ~ifelse(imputed, "circle-open", "circle"),
          line    = list(color = clr, width = 1)
        ),
        text = ~paste0(
          "<b>", name, "</b><br>",
          "Time (SL): ", format(datetime_sl, "%d %b %Y %H:%M"), "<br>",
          y_title, ": ", round(get(y_col), 5), "\u00B0<br>",
          "Sex: ", sex, "<br>",
          "Imputed: ", imputed
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
  make_dual_axis_plot <- function(df) {
    elephants_in_data <- unique(df$name)
    col_map <- ELEPHANT_COLOURS[names(ELEPHANT_COLOURS) %in% elephants_in_data]
    
    p <- plot_ly()
    
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
        marker = list(color = ~ifelse(imputed, "#888888", clr),
                      size = ~ifelse(imputed, 5, 4),
                      symbol = ~ifelse(imputed, "circle-open", "circle"),
                      line = list(color = clr, width = 1)),
        text = ~paste0("<b>", name, "</b><br>Latitude: ", round(lat, 5), "\u00B0N<br>",
                       format(datetime_sl, "%d %b %Y %H:%M"), "<br>Sex: ", sex,
                       "<br>Imputed: ", imputed),
        hoverinfo = "text"
      )
      
      # Longitude (right axis, dotted, triangles)
      p <- p %>% add_trace(
        data = sub, x = ~datetime_sl, y = ~lon,
        type = "scatter", mode = "lines+markers",
        name = paste(el, "\u2013 Lon"), legendgroup = el, yaxis = "y2",
        connectgaps = FALSE,
        line   = list(color = clr, width = 1.5, dash = "dot"),
        marker = list(color = ~ifelse(imputed, "#888888", clr),
                      size = ~ifelse(imputed, 5, 4),
                      symbol = ~ifelse(imputed, "triangle-up-open", "triangle-up"),
                      line = list(color = clr, width = 1)),
        text = ~paste0("<b>", name, "</b><br>Longitude: ", round(lon, 5), "\u00B0E<br>",
                       format(datetime_sl, "%d %b %Y %H:%M"), "<br>Sex: ", sex,
                       "<br>Imputed: ", imputed),
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
    make_plot(df, "lat", "Latitude (\u00B0N, WGS84)", ref_lines)
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
    make_plot(df, "lon", "Longitude (\u00B0E, WGS84)", ref_lines)
  })
  
  # ── Both coordinates (dual y-axis) ──────────────────────────────────────────
  output$plot_both <- renderPlotly({
    df <- agg_data()
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    make_dual_axis_plot(df)
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
  
  # Tracking Data Plot
  output$tracking_plot <- renderPlot({
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
  })
  
  # GPS Tracking Data by Elephant
  output$tracking_by_elephant <- renderPlot({
    # Get unique elephants and their colors
    elephant_names <- unique(df_sf$name)
    n_elephants <- length(elephant_names)
    
    # Create color mapping for sex
    sex_colors <- c("Male" = "darkblue", "Female" = "darkred")
    
    # Determine number of columns (max 4)
    n_col <- min(4, n_elephants)
    
    ggplot() +
      geom_sf(
        data = df_sf,
        aes(color = sex),
        size = 3,
        alpha = 0.5
      ) +
      facet_wrap(~ name, ncol = n_col) +
      coord_sf() +
      theme_minimal() +
      scale_color_manual(
        values = sex_colors,
        name = "Sex"
      ) +
      labs(
        title = "GPS Tracking Data by Elephant",
        x = "Longitude",
        y = "Latitude"
      ) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "bottom"
      )
  })
  
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
      choices = sprintf("%02d", 1:12),
      selected = sprintf("%02d", 1:12)[1]
    )
    
    updateSelectInput(session, "elephant",
                      choices = sort(unique(df_sf$name)))
  })
  
  # reactive filtered dataset (IMPORTANT FIX)
  df_filtered <- reactive({
    req(input$year, input$month, input$elephant)
    
    df_sf |>
      filter(
        year == input$year,
        month %in% input$month,
        name == input$elephant
      )
  })
  
  output$map <- renderLeaflet({
    
    dat <- df_filtered()
    
    #==========================================================
    # CASE 1: NO DATA → SHOW EMPTY STREET MAP WITH MESSAGE
    #==========================================================
    if (nrow(dat) == 0) {
      
      leaflet() |>
        addProviderTiles(providers$OpenStreetMap) |>
        addPopups(
          lng = 80.0,
          lat = 7.0,
          popup = "<b>No elephant data available for selected year & month</b>"
        ) |>
        setView(lng = 80.0, lat = 7.0, zoom = 7)
    }
    
    #==========================================================
    # CASE 2: DATA EXISTS → PLOT MAP
    #==========================================================
    else {
      
      elephant_list <- sort(unique(dat$name))
      
      m <- leaflet(dat) |>
        addProviderTiles(providers$OpenStreetMap)  # ONLY STREET MAP
      
      for (e in elephant_list) {
        
        d <- dat |>
          filter(name == e) |>
          arrange(datetime)
        
        line_month <- d |>
          group_by(year_month) |>
          summarise(do_union = FALSE) |>
          st_cast("LINESTRING")
        
        m <- m |>
          addPolylines(
            data = line_month,
            group = ~paste0(e, year_month),
            color = ~pal(year_month),
            weight = 3,
            opacity = 0.8
          ) |>
          addCircleMarkers(
            data = d,
            group = e,
            color = ~pal(year_month),
            radius = 5,
            stroke = FALSE,
            fillOpacity = 1,
            popup = ~paste0(
              "<b>Elephant:</b> ", name,
              "<br><b>Date:</b> ", datetime,
              "<br><b>Year:</b> ", year,
              "<br><b>Month:</b> ", month
            )
          )
      }
      m
    }
  })
  
  
  # HEATMAP
  output$summaryTable <- renderTable({
    info <- plot_info[[input$variable]]
    vals <- as.numeric(info$values)
    
    data.frame(
      Measure = c(
        "Minimum", "First Quantile (Q1)", "Median", 
        "Mean", "Third Quantile (Q3)", "Maximum", 
        "Standard Deviation", "Missing Values (NA)"
      ),
      Value = c(
        min(vals, na.rm = TRUE),
        quantile(vals, 0.25, na.rm = TRUE),
        median(vals, na.rm = TRUE),
        mean(vals, na.rm = TRUE),
        quantile(vals, 0.75, na.rm = TRUE),
        max(vals, na.rm = TRUE),
        sd(vals, na.rm = TRUE),
        sum(is.na(vals))
      )
    )
  }, 
  digits = 2, 
  colnames = FALSE, 
  striped = FALSE,    # Turned off to let custom CSS handle row colors
  bordered = FALSE,   # Turned off to remove harsh borders
  width = "100%"
  )
  
  output$calendarPlot <- renderPlot({
    info <- plot_info[[input$variable]]
    discrete_key <- list(
      space = "right",
      rectangles = list(col = my_colors, border = "black", size = 2),
      text = list(info$labels, cex = 0.8),
      padding.text = 3,
      columns = 1
    )
    
    calendarHeat(
      dates = dates, values = info$values, at = info$breaks, colors = my_colors,
      title = info$title, colorkey = FALSE,
      legend = list(right = list(fun = draw.key, args = list(key = discrete_key)))
    )
  })
  
  # ── Data table ───────────────────────────────────────────────────────────────
  output$data_table <- renderDT({
    df <- filtered() %>%
      select(name, sex, datetime_sl, lat, lon, imputed) %>%
      mutate(datetime_sl = format(datetime_sl, "%d %b %Y %H:%M"),
             lat = round(lat, 6), lon = round(lon, 6)) %>%
      rename(Elephant = name, Sex = sex, `Date/Time (SL)` = datetime_sl,
             Latitude = lat, Longitude = lon, Imputed = imputed)
    
    datatable(df,
              options = list(pageLength = 20, scrollX = TRUE,
                             dom = "Bfrtip", buttons = c("csv", "excel")),
              rownames = FALSE, class = "stripe hover", extensions = "Buttons")
  })
}

# ── Run ────────────────────────────────────────────────────────────────────────
shinyApp(ui, server)
