# ============================================================
# RouteRx - ASA DataFest 2026 | "Best Use of External Data"
# Analyzing public transit accessibility for SVH patients
# in Topeka, Kansas using patient records + GTFS bus data
# ============================================================

library(dplyr)
library(ggplot2)
library(leaflet)
library(tidytransit)

# ============================================================
# STEP 1: Load & Clean Datasets
# ============================================================

# Load core hospital datasets
patients    <- read.csv("patients.csv")
encounters  <- read.csv("encounters.csv")
departments <- read.csv("departments.csv")
diagnosis   <- read.csv("diagnosis.csv")

# Load Tiger Census data (centroid lat/lng per census block)
tigercensuscodes <- read.csv("tigercensus.csv")

# Load Topeka Metro GTFS bus schedule data
gtfs       <- read_gtfs("google_transit.zip")
stop_times <- gtfs$stop_times
stops      <- gtfs$stops
trips      <- gtfs$trips

# Clean department addresses (remove trailing characters + whitespace)
departments <- departments %>%
  mutate(Address = trimws(gsub("\\s+\\S+$", "", Address))) %>%
  filter(!is.na(Address), Address != "*Unknown")

# ============================================================
# STEP 2: Find Top Hospital by Patient Volume
# ============================================================

# Join patients -> encounters -> departments, count by address
top_hospitals <- patients %>%
  inner_join(encounters, by = c("DurableKey" = "PatientDurableKey")) %>%
  inner_join(departments, by = c("DepartmentKey" = "DepartmentKey")) %>%
  filter(!is.na(Address)) %>%
  count(Address, sort = TRUE)

# Top result: Stormont Vail Health (SVH) at 1500 SW 10th Ave
print(head(top_hospitals, 3))

# ============================================================
# STEP 3: Find Top Census Blocks Sending Patients to SVH
# ============================================================

svh_census <- patients %>%
  inner_join(encounters, by = c("DurableKey" = "PatientDurableKey")) %>%
  inner_join(departments, by = c("DepartmentKey" = "DepartmentKey")) %>%
  filter(grepl("^1500 SW 10th", Address, ignore.case = TRUE)) %>%
  count(CensusBlockGroupFipsCode, sort = TRUE) %>%
  head(5)

print(svh_census)

# ============================================================
# STEP 4: Get Lat/Lng for Top Census Blocks via TigerCensus
# ============================================================

top_census_coords <- svh_census %>%
  inner_join(
    tigercensuscodes %>% mutate(GEOID = as.character(GEOID)),
    by = c("CensusBlockGroupFipsCode" = "GEOID")
  ) %>%
  select(CensusBlockGroupFipsCode, n, CENTLAT, CENTLON)

print(top_census_coords)

# ============================================================
# STEP 5: Join GTFS Bus Data to Find Nearby Stops
# ============================================================

# Join stop_times -> stops -> trips for full schedule
bus_schedule <- stop_times %>%
  inner_join(stops, by = "stop_id") %>%
  inner_join(trips, by = "trip_id") %>%
  filter(!is.na(arrival_time)) %>%
  select(stop_name, arrival_time, departure_time, route_id) %>%
  arrange(arrival_time)

# Target stops near SVH and top census blocks
target_stops <- c(
  "10th @ Garfield, Topeka, KS 66606",
  "10th @ Mulvane, Topeka, KS 66606",
  "Washburn @ 10th, Topeka, KS 66606",
  "10th @ Boswell, Topeka, KS 66604"
)

nearby_schedule <- bus_schedule %>%
  filter(stop_name %in% target_stops)

print(nearby_schedule)

# ============================================================
# STEP 6: Visualize Top Census Blocks + SVH on Map
# ============================================================

census_points <- data.frame(
  lat   = c(39.04752, 39.01000, 39.02627, 39.00424),
  lng   = c(-95.73393, -95.69136, -95.63915, -95.77054),
  count = c(9520, 8937, 9161, 9577),
  label = c("#4", "#3", "#2", "#1")
)

hospital <- data.frame(
  label = "SVH - 1500 SW 10th Ave",
  lat   = 39.0527126,
  lng   = -95.6960568
)

redIcon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png",
  iconWidth = 25, iconHeight = 41, iconAnchorX = 12, iconAnchorY = 41
)

leaflet() %>%
  addTiles() %>%
  addMarkers(
    data = census_points, lat = ~lat, lng = ~lng,
    label = ~label,
    popup = ~paste0(label, "<br>Patient Count: ", count),
    labelOptions = labelOptions(
      noHide = TRUE, direction = "bottom", textOnly = FALSE,
      style = list("font-weight" = "bold", "font-size" = "13px",
                   "background-color" = "white",
                   "border" = "1px solid gray", "padding" = "3px")
    )
  ) %>%
  addMarkers(
    data = hospital, lat = ~lat, lng = ~lng,
    label = "SVH", popup = ~label, icon = redIcon,
    labelOptions = labelOptions(
      noHide = TRUE, direction = "bottom", textOnly = FALSE,
      style = list("font-weight" = "bold", "color" = "red",
                   "font-size" = "13px", "background-color" = "white",
                   "border" = "1px solid gray", "padding" = "3px")
    )
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("blue", "red"),
    labels = c("Census Block (Top 4)", "SVH Hospital")
  )

# ============================================================
# STEP 7: Patient Journey Map (Sample Patient: 254529)
# ============================================================

# Home: 6225 SW 34th Terrace | Visits: SVH + Cotton O'Neil Croco
home <- data.frame(lat = 39.0089, lng = -95.7689)

hospitals <- data.frame(
  lat   = c(39.0527, 39.0143),
  lng   = c(-95.6961, -95.6116),
  label = c("Visit #1 - SVH", "Visit #2 - Cotton O'Neil")
)

blueIcon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png",
  iconWidth = 25, iconHeight = 41, iconAnchorX = 12, iconAnchorY = 41
)

leaflet() %>%
  addTiles() %>%
  addPolylines(lng = c(home$lng, hospitals$lng[1]),
               lat = c(home$lat, hospitals$lat[1]),
               color = "gray", weight = 2, dashArray = "5,5") %>%
  addPolylines(lng = c(home$lng, hospitals$lng[2]),
               lat = c(home$lat, hospitals$lat[2]),
               color = "gray", weight = 2, dashArray = "5,5") %>%
  addMarkers(data = home, lat = ~lat, lng = ~lng, icon = blueIcon) %>%
  addMarkers(data = hospitals, lat = ~lat, lng = ~lng, icon = redIcon,
             label = ~label,
             labelOptions = labelOptions(
               noHide = TRUE, direction = "bottom", textOnly = FALSE,
               style = list("font-weight" = "bold", "font-size" = "13px",
                            "background-color" = "white",
                            "border" = "1px solid gray", "padding" = "3px")
             ))
