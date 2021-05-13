#---- GLOBAL ----#

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
  "centerpiece_type"
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
bees <- tibble(
  type = c("honeybee", "bumble_bee", "large_dark_bee", "small_dark_bee", "greenbee", "wild_bee", "non_bee"),
  label = c("Honey bees", "Bumble bees", "Large dark bees", "Small dark bees", "Green bees", "Wild bees", "Non-bees"),
  color = c("#eca500", "#d86d27", "#758BFD", "#AEB8FE", "#99b5aa", "#5f8475", "#949494"),
  group = c("Honey bees", "Wild bees", "Wild bees","Wild bees","Wild bees","Wild bees", "Non-bees")
) %>% mutate_all(fct_inorder)
# bees %>% write_csv("data/bees.csv")


# formatted bee names for ungrouped
bee_names <- bees %>% filter(type != "wild_bee") %>% pull(label) %>% as.character()


# formatted names for wild bee grouping
wildbee_names <- levels(bees$group)


# load other types
habitats <- read_csv("data/habitats.csv", col_types = cols()) %>% mutate_all(fct_inorder)
crops <- read_csv("data/crops.csv", col_types = cols()) %>% mutate_all(fct_inorder)
managements <- read_csv("data/managements.csv", col_types = cols()) %>% mutate_all(fct_inorder)



# content checks ----------------------------------------------------------

# wibee_in$site_type %>% unique()
# wibee_in$crop %>% unique()
# wibee_in$management_type %>% unique()



# Process survey data -----------------------------------------------------

# generate main dataset
surveys <- wibee_in %>%
  select(all_of(c(keep_cols, bee_cols))) %>%
  rename(
    date = ended_at,
    habitat = site_type,
    management = management_type,
    crop_category = centerpiece_type) %>%
  mutate(date = as.Date(date)) %>%
  filter(duration == "5 minutes", date >= "2020-04-01") %>%
  mutate(across(all_of(bee_cols), replace_na, 0)) %>%
  mutate(wild_bee = bumble_bee + large_dark_bee + small_dark_bee + greenbee) %>%
  mutate(
    habitat = replace_na(habitat, "other"),
    habitat = case_when(
      habitat %in% habitats$type ~ habitat,
      grepl("lawn", habitat) || grepl("garden", habitat) ~ "lawn-and-garden",
      T ~ "other"),
    habitat = factor(habitat, levels = habitats$type)) %>%
  mutate(
    crop = tolower(crop),
    crop = case_when(
      crop %in% crops$type ~ crop,
      grepl("berry", crop) ~ "other-berry",
      grepl("flower", crop) ~ "flowers",
      grepl("native", crop) ~ "native-flowers",
      T ~ "other"),
    crop = factor(crop, levels = crops$type)) %>%
  mutate(
    management = replace_na(management, "none"),
    management = case_when(
      management %in% managements$type ~ management,
      grepl("organic", management) ~ "organic",
      grepl("conventional", management) ~ "conventional",
      grepl("ipm", management) ~ "ipm",
      grepl("spray", management) && grepl("low", management) ~ "low spray",
      grepl("spray", management) && grepl("no", management) ~ "no spray",
      T ~ "other"),
    management = factor(management, levels = managements$type)) %>%
  left_join(
    rename(habitats, habitat = type, habitat_name = label),
    by = "habitat") %>%
  left_join(
    rename(crops, crop = type, crop_name = label),
    by = "crop") %>%
  left_join(
    rename(managements, management = type, management_name = label),
    by = "management") %>%
  mutate(
    lat_rnd = round(lat, 1),
    lng_rnd = round(lng, 1),
    grid_pt = paste(lat_rnd, lng_rnd, sep = ", "),
    inwi = between(lat, 42.49, 47.08) & between(lng, -92.89, -86.80),
    id = 1:length(id)) %>%
  droplevels()


# keep only attributes that have occurred at least once
habitats <- habitats %>% filter(type %in% levels(surveys$habitat)) %>% droplevels()
crops <- crops %>% filter(type %in% levels(surveys$crop)) %>% droplevels()
managements <- managements %>% filter(type %in% levels(surveys$management)) %>% droplevels()


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


# total counts for project summary
bee_totals <- surveys_long %>%
  filter(bee_name %in% wildbee_names) %>%
  group_by(bee_name) %>%
  summarise(tot_count = sum(count), .groups = "drop") %>%
  mutate(pct_count = sprintf("%1.1f%%", tot_count / sum(.$tot_count) * 100))

