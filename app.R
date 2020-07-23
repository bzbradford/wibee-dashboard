# WiBee Shiny App
# developed by Ben Bradford, UW-Madison
# currently built under R 3.6.3 for compatibility with data-viz.it.wisc.edu

library(shiny)
library(tidyverse)
library(httr)


# Load remote data --------------------------------------------------------

# check when last data refresh occurred
if(file.exists("./data/refresh_time")) {
  refresh_time <- readRDS("./data/refresh_time")
} else {
  refresh_time <- as.POSIXct("2020-01-01")
}

# update surveys at most once an hour. Writes to local csv
if(refresh_time < Sys.time() - 3600) {
  get_surveys <-
    content(
      GET(url = "https://wibee.caracal.tech/api/data/survey-summaries",
        config = add_headers(Authorization = Sys.getenv("caracal_token")))
    )
  if(is.data.frame(get_surveys)) {
    arrange(get_surveys, ended_at) %>% write_csv("./data/surveys.csv")
    refresh_time <- Sys.time()
    saveRDS(refresh_time, "./data/refresh_time")
    message("Survey data refreshed from remote database.")
  } else {
    message("Unable to refresh data from remote server.")
  }
} else {
  message("Skipping refresh, last query < 1 hr ago.")
}

# read data from local csv
wibee_in <- read_csv("./data/surveys.csv", col_types = cols())



# Define local variables --------------------------------------------------

# explanatory cols to keep
keep_cols <- c(
  "id",
  "user_id",
  "lat",
  "lng",
  "ended_at",
  "duration",
  "site_type",
  "crop",
  "management_type")

# bee count column names
bee_cols <- c(
  "honeybee",
  "bumble_bee",
  "large_dark_bee",
  "small_dark_bee",
  "greenbee",
  "non_bee"
)

# formatted bee names for ungrouped
bee_names <- c(
  "Honey bees",
  "Bumble bees",
  "Large dark bees",
  "Small dark bees",
  "Green bees",
  "Non-bees")

# formatted names for wild bee grouping
wildbee_names <- c(
  "Honey bees",
  "Wild bees",
  "Non-bees"
)

# bee crossref
bees <-
  tibble(
    bee_type = c(
      "honeybee",
      "bumble_bee",
      "large_dark_bee",
      "small_dark_bee",
      "greenbee",
      "wild_bee",
      "non_bee"),
    bee_name = fct_inorder(c(
      "Honey bees",
      "Bumble bees",
      "Large dark bees",
      "Small dark bees",
      "Green bees",
      "Wild bees",
      "Non-bees")
    ),
    bee_color = fct_inorder(c(
      "#eca500",
      "#d86d27",
      "#758BFD",
      "#AEB8FE",
      "#99b5aa",
      "#5f8475",
      "#949494")
    )
  )

# Valid habitat types
# habitat_types <- 
#   c("corn-soybeans-alfalfa",
#     "fruit-vegetable-field",
#     "orchard",
#     "road-field-edge",
#     "lawn-and-garden",
#     "prairie",
#     "woodland")

habitats <- tibble(
  type = c(
    "corn-soybeans-alfalfa",
    "fruit-vegetable-field",
    "orchard",
    "road-field-edge",
    "lawn-and-garden",
    "prairie",
    "woodland"),
  label = c(
    "Corn/soybeans/alfalfa",
    "Fruit/vegetable field",
    "Orchard",
    "Road or field edge",
    "Lawn and garden",
    "Prairie",
    "Woodland"))


# valid crop types
# crop_types <- 
#   c("apple",
#     "cherry",
#     "cranberry",
#     "other berry",
#     "cucumber",
#     "melon",
#     "squash",
#     "other",
#     "none")

crops <- tibble(
  type = c(
    "apple",
    "cherry",
    "cranberry",
    "other berry",
    "cucumber",
    "melon",
    "squash",
    "other",
    "none"),
  label = c(
    "Apple",
    "Cherry",
    "Cranberry",
    "Berries",
    "Cucumber",
    "Melon",
    "Squash",
    "Other",
    "None"))

# valid management types
# mgmt_types <- 
#   c("organic",
#     "conventional",
#     "other",
#     "unknown")

mgmt <- tibble(
  type = c(
    "organic",
    "conventional",
    "other",
    "unknown"),
  label = c(
    "Organic",
    "Conventional",
    "Other",
    "Unknown"
  )
)

# set icon for leaflet
bee_icon <- makeIcon(
  iconUrl = "map-icon.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 15, iconAnchorY = 15)

# wisconsin bounding box
wibox = c(-92.888114, 42.491983, -86.805415, 47.080621)

# Check if location is within Wisconsin's bounding box
inbox <- function(x, y, box = wibox) {
  ifelse(between(x, box[1], box[3]) & between(y, box[2], box[4]), T, F)
}


# Process survey data -----------------------------------------------------

# generate main dataset
surveys <- wibee_in %>%
  select(all_of(c(keep_cols, bee_cols))) %>%
  mutate(ended_at = as.Date(ended_at)) %>%
  rename(habitat = site_type, management = management_type, date = ended_at) %>%
  mutate_at(bee_cols, replace_na, 0) %>%
  mutate(wild_bee = bumble_bee + large_dark_bee + small_dark_bee + greenbee) %>%
  mutate(habitat = factor(habitat, levels = habitats$type)) %>%
  mutate(crop = tolower(crop)) %>%
  mutate(crop = factor(
    case_when(
      is.na(crop) ~ "none",
      crop %in% crops$type ~ crop,
      grepl("berry", crop) ~ "other berry",
      T ~ "other"), levels = crops$type)) %>%
  mutate(management = factor(
    case_when(
      management %in% mgmt$type ~ management,
      T ~ "other"), levels = mgmt$type)) %>%
  filter(duration == "5 minutes") %>%
  filter(date >= "2020-04-01") %>%
  drop_na(c(habitat, crop, management)) %>%
  mutate(
    lng_rnd = round(lng, 1),
    lat_rnd = round(lat, 1)) %>%
  mutate(grid_pt = paste(lng_rnd, lat_rnd)) %>%
  mutate(inwi = inbox(lng, lat, wibox))


# pivot longer for some data analysis
surveys_long <- surveys %>%
  pivot_longer(bees$bee_type, names_to = "bee_type", values_to = "count") %>%
  left_join(bees, by = "bee_type")


# generate grid points and summary statistics
map_pts <- surveys %>%
  drop_na(lng, lat) %>%
  mutate(
    lng = round(lng, 1),
    lat = round(lat, 1)) %>%
  group_by(lng, lat) %>%
  summarise(
    n_surveys = n(),
    hb = round(mean(honeybee)/5,1),
    wb = round(mean(wild_bee)/5,1),
    nb = round(mean(non_bee)/5,1),
    .groups = "drop") %>%
  mutate(grid_pt = paste(lng, lat)) %>%
  mutate(inwi = inbox(lng, lat, wibox))


# get list of all grid cells for initial selection
map_pts_all <- map_pts$grid_pt
map_pts_wi <- filter(map_pts, inwi == T)$grid_pt


# get date range of data
min_date <- min(surveys$date)
max_date <- max(surveys$date)


# total counts for project summary
bee_totals <- surveys_long %>%
  filter(bee_name %in% wildbee_names) %>%
  group_by(bee_name) %>%
  summarise(tot_count = sum(count), .groups = "drop") %>%
  mutate(pct_count = sprintf("%1.1f%%", tot_count / sum(.$tot_count) * 100))


# generate initial filter labels #
# habitat_labels <- {
#   surveys %>%
#     count(habitat, .drop = F) %>%
#     mutate(label = paste0(habitat, " (", n, ")")) %>%
#     .$label
# }
# crop_labels <- {
#   surveys %>%
#     count(crop, .drop = F) %>%
#     mutate(label = paste0(crop, " (", n, ")")) %>%
#     .$label
# }
# mgmt_labels <- {
#   surveys %>%
#     count(management, .drop = F) %>%
#     mutate(label = paste0(management, " (", n, ")")) %>%
#     .$label
# }




# App ---------------------------------------------------------------------

source("ui.r")
source("server.r")

shinyApp(ui, server)
