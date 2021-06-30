#---- GLOBAL ----#

library(tidyverse)
library(httr)

kebab <- function(s) {
  gsub(" ", "-", tolower(s))
}

# Load remote data --------------------------------------------------------

# check when last data refresh occurred
if (file.exists("./data/refresh_time")) {
  refresh_time <- readRDS("./data/refresh_time")
} else {
  refresh_time <- as.POSIXct("2020-01-01")
}


# update surveys at most once an hour. Writes to local csv
if (refresh_time < Sys.time() - 3600) {
  get_surveys <-
    content(
      GET(url = "https://wibee.caracal.tech/api/data/survey-summaries",
        config = add_headers(Authorization = Sys.getenv("caracal_token")))
    )
  if (is.data.frame(get_surveys)) {
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
wibee_in <- read_csv("./data/surveys.csv", col_types = cols(), guess_max = 10000)



# Define local variables --------------------------------------------------

## column names ----
# survey attribute cols to keep
keep_cols <- c(
  "id",
  "user_id",
  "lat",
  "lng",
  "ended_at",
  "duration",
  "site_type",
  "crop",
  "management_type",
  "picture_url"
  )


# bee cols to pivot
bee_cols <- c(
  "honeybee",
  "bumble_bee",
  "large_dark_bee",
  "small_dark_bee",
  "greenbee",
  "non_bee"
  )


# bee names
bees <- read_csv("data/bees.csv", col_types = cols()) %>% mutate_all(fct_inorder)

# formatted bee names for ungrouped
bee_names <- bees %>% filter(type != "wild_bee") %>% pull(label) %>% as.character()

# formatted names for wild bee grouping
wildbee_names <- levels(bees$group)

# load habitat types
habitat_list <- read_csv("data/habitats.csv", col_types = cols()) %>% mutate_all(fct_inorder)

# load management types
management_list <- read_csv("data/managements.csv", col_types = cols()) %>% mutate_all(fct_inorder)

# load plant list
plant_list <- read_csv("data/plant-list.csv", col_types = cols()) %>% mutate_all(fct_inorder)



# content checks ----------------------------------------------------------

# wibee_in$site_type %>% unique()
# wibee_in$crop %>% unique()
# wibee_in$management_type %>% unique()



# Process survey data -----------------------------------------------------

# generate main dataset
wibee <- wibee_in %>%
  select(all_of(c(keep_cols, bee_cols))) %>%
  rename(
    date = ended_at,
    habitat = site_type,
    management = management_type) %>%
  filter(duration == "5 minutes", date >= "2020-04-01") %>%
  mutate(
    date = as.Date(date),
    year = lubridate::year(date),
    .after = "date") %>%
  mutate(across(all_of(bee_cols), replace_na, 0)) %>%
  mutate(wild_bee = bumble_bee + large_dark_bee + small_dark_bee + greenbee) %>%
  mutate(
    habitat = replace_na(habitat, "other"),
    habitat = case_when(
      habitat %in% habitat_list$type ~ habitat,
      grepl("lawn", habitat) || grepl("garden", habitat) ~ "lawn-and-garden",
      T ~ "other"),
    habitat = factor(habitat, levels = habitat_list$type)) %>%
  left_join(rename(habitat_list, habitat = type, habitat_name = label)) %>%
  mutate(
    management = replace_na(management, "none"),
    management = case_when(
      management %in% management_list$type ~ management,
      grepl("organic", management) ~ "organic",
      grepl("conventional", management) ~ "conventional",
      grepl("ipm", management) ~ "ipm",
      grepl("spray", management) && grepl("low", management) ~ "low spray",
      grepl("spray", management) && grepl("no", management) ~ "no spray",
      T ~ "other"),
    management = factor(management, levels = management_list$type)) %>%
  left_join(rename(management_list, management = type, management_name = label)) %>%
  left_join(plant_list) %>%
  mutate(
    lat_rnd = round(lat, 1),
    lng_rnd = round(lng, 1),
    grid_pt = paste(lat_rnd, lng_rnd, sep = ", "),
    inwi = between(lat, 42.49, 47.08) & between(lng, -92.89, -86.80)) %>%
  mutate(
    remote_id = id,
    id = 1:length(id)) %>%
  droplevels()


# separate surveys and ids for picture downloads
# the ids will change if the filter is changed in the block above
surveys <- wibee %>% select(-c("remote_id", "picture_url"))
images <- wibee %>%
  select(c("id", "remote_id", "picture_url")) %>%
  filter(!is.na(picture_url))


habitats <- surveys %>%
  group_by(habitat, habitat_name) %>%
  summarise(surveys = n(), .groups = "drop")

managements <- surveys %>%
  group_by(management, management_name) %>%
  summarise(surveys = n(), .groups = "drop")

plants <- surveys %>%
  group_by(plant_type, plant_id, plant_name, plant_common_name) %>%
  summarise(surveys = n(), .groups = "drop")

top_plants <- plants %>%
  group_by(plant_type) %>%
  slice_max(surveys, n = 15)


# pivot longer for some data analysis
surveys_long <- surveys %>%
  pivot_longer(cols = bees$type, names_to = "bee", values_to = "count") %>%
  left_join(
    rename(bees, bee = type, bee_name = label, bee_color = color, bee_group = group),
    by = "bee")



# Map data and other summaries --------------------------------------------

# generate grid points and summary statistics
map_pts <- surveys %>%
  drop_na(lat, lng) %>%
  mutate(
    lat = round(lat, 1),
    lng = round(lng, 1)) %>%
  group_by(lat, lng, inwi) %>%
  summarise(
    n_surveys = n(),
    n_users = n_distinct(user_id),
    hb = round(mean(honeybee)/5,1),
    wb = round(mean(wild_bee)/5,1),
    nb = round(mean(non_bee)/5,1),
    .groups = "drop") %>%
  mutate(grid_pt = paste(lat, lng, sep = ", "))


# get list of all grid cells for initial selection
map_pts_all <- map_pts$grid_pt
map_pts_wi <- filter(map_pts, inwi == T)$grid_pt


# get date range of data
min_date <- min(surveys$date)
max_date <- max(surveys$date)
years <- unique(format(surveys$date, "%Y"))


# total counts for project summary
bee_totals <- surveys_long %>%
  filter(bee_name %in% wildbee_names) %>%
  group_by(bee_name) %>%
  summarise(tot_count = sum(count), .groups = "drop") %>%
  mutate(pct_count = sprintf("%1.1f%%", tot_count / sum(.$tot_count) * 100))

