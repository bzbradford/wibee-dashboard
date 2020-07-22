#### BACKEND ####

library(tidyverse)

# check when last data refresh occurred
if(file.exists("./data/refresh_time")) {
  refresh_time <- readRDS("./data/refresh_time")
} else {
  refresh_time <- as.POSIXct("2020-01-01")
}

# update surveys at most once an hour
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
    msg <- "Survey data refreshed from remote database."
  } else {
    msg <- "Unable to refresh data from server, falling back on local cache."
  }
} else {
  msg <- "Survey data refreshed recently."
}

wibee_in <- read_csv("./data/surveys.csv", col_types = cols())

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
bee_ref <-
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
habitat_types <- 
  c("corn-soybeans-alfalfa",
    "fruit-vegetable-field",
    "orchard",
    "road-field-edge",
    "lawn-and-garden",
    "prairie",
    "woodland")

# valid crop types
crop_types <- 
  c("apple",
    "cherry",
    "cranberry",
    "other berry",
    "cucumber",
    "melon",
    "squash",
    "other",
    "none")

# valid management types
mgmt_types <- 
  c("organic",
    "conventional",
    "other",
    "unknown")


# generate main dataset
surveys <- wibee_in %>%
  select(all_of(c(keep_cols, bee_cols))) %>%
  mutate(ended_at = as.Date(ended_at)) %>%
  rename(habitat = site_type, management = management_type, date = ended_at) %>%
  mutate_at(bee_cols, replace_na, 0) %>%
  mutate(wild_bee = bumble_bee + large_dark_bee + small_dark_bee + greenbee) %>%
  mutate(habitat = factor(habitat, levels = habitat_types)) %>%
  mutate(crop = tolower(crop)) %>%
  mutate(crop = factor(
    case_when(
      is.na(crop) ~ "none",
      crop %in% crop_types ~ crop,
      grepl("berry", crop) ~ "other berry",
      T ~ "other"), levels = crop_types)) %>%
  mutate(management = factor(
    case_when(
      management %in% mgmt_types ~ management,
      T ~ "other"), levels = mgmt_types)) %>%
  filter(duration == "5 minutes") %>%
  filter(date >= "2020-04-01") %>%
  drop_na(c(habitat, crop, management)) %>%
  mutate(
    lng_rnd = round(lng, 1),
    lat_rnd = round(lat, 1)) %>%
  mutate(grid_pt = paste(lng_rnd, lat_rnd))


# pivot longer for plotting etc
surveys_long <- surveys %>%
  pivot_longer(bee_ref$bee_type, names_to = "bee_type", values_to = "count") %>%
  left_join(bee_ref, by = "bee_type")


# generate grid points and summary statistics
map_pts <- surveys %>%
  drop_na(lng, lat) %>%
  mutate(
    lng_rnd = round(lng, 1),
    lat_rnd = round(lat, 1)) %>%
  group_by(lng_rnd, lat_rnd) %>%
  summarise(
    n_surveys = n(),
    hb = round(mean(honeybee)/5,1),
    wb = round(mean(wild_bee)/5,1),
    nb = round(mean(non_bee)/5,1),
    .groups = "drop") %>%
  mutate(grid_pt = paste(lng_rnd, lat_rnd))

map_pts_all <- map_pts$grid_pt


# set icon for leaflet
bee_icon <- makeIcon(
  iconUrl = "map-icon.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 15, iconAnchorY = 15)


# get date range of data
min_date <- min(surveys$date)
max_date <- max(surveys$date)


# total counts for project summary
bee_totals <- surveys_long %>%
  filter(bee_name %in% wildbee_names) %>%
  group_by(bee_name) %>%
  summarise(tot_count = sum(count), .groups = "drop") %>%
  mutate(pct_count = sprintf("%1.1f%%", tot_count / sum(.$tot_count) * 100))


## generate initial filter labels ##
habitat_labels <- {
  surveys %>%
    count(habitat, .drop = F) %>%
    mutate(label = paste0(habitat, " (", n, ")")) %>%
    .$label
}
crop_labels <- {
  surveys %>%
    count(crop, .drop = F) %>%
    mutate(label = paste0(crop, " (", n, ")")) %>%
    .$label
}
mgmt_labels <- {
  surveys %>%
    count(management, .drop = F) %>%
    mutate(label = paste0(management, " (", n, ")")) %>%
    .$label
}
