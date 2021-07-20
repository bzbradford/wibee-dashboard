#---- GLOBAL ----#

library(tidyverse)
library(httr)



# Load remote data ----

# check when last data refresh occurred
if (file.exists("refresh_time")) {
  refresh_time <- readRDS("refresh_time")
} else {
  refresh_time <- as.POSIXct("2020-01-01")
}


# update surveys at most once an hour. Writes to local csv
if (refresh_time < Sys.time() - 3600) {
  try({
    get_surveys <- content(GET(
      url = "https://wibee.caracal.tech/api/data/survey-summaries",
      config = add_headers(Authorization = Sys.getenv("caracal_token"))))
  })
  if (is.data.frame(get_surveys)) {
    arrange(get_surveys, ended_at) %>% write_csv("private/surveys.csv")
    refresh_time <- Sys.time()
    saveRDS(refresh_time, "refresh_time")
    message("Survey data refreshed from remote database.")
  } else {
    message("Unable to refresh data from remote server.")
  }
} else {
  message("Skipping refresh, last query < 1 hr ago.")
}


# read data from local csv and copy amended data into main columns
wibee_in <- read_csv("private/surveys.csv", col_types = cols(), guess_max = 10000) %>%
  mutate(
    bumble_bee = bumble_bee_amended,
    honeybee = honeybee_amended,
    large_dark_bee = large_dark_bee_amended,
    small_dark_bee = small_dark_bee_amended,
    greenbee = greenbee_amended,
    non_bee = non_bee_amended
  )

# Check values in columns
# wibee_in$site_type %>% unique()
# wibee_in$crop %>% unique()
# wibee_in$management_type %>% unique()



# Load/create helper data ----

# bee names and colors
bees <- read_csv("data/bees.csv", col_types = cols()) %>% mutate_all(fct_inorder)

# formatted bee names for ungrouped
bee_names <- as.character(filter(bees, type != "wild_bee")$label)

# formatted names for wild bee grouping
wildbee_names <- levels(bees$group)


# load habitat and management types
habitat_list <- read_csv("data/habitats.csv", col_types = cols())
management_list <- read_csv("data/managements.csv", col_types = cols())


# load plant lists
plant_list <- read_csv("plants/known-plant-list.csv", col_types = cols())
legacy_plant_list <- read_csv("plants/legacy-plant-list.csv", col_types = cols())
focal_plant_list <- read_csv("plants/focal-plant-list.csv", col_types = cols())
plant_replace <- bind_rows(legacy_plant_list, focal_plant_list)


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
  "picture_url")

# bee cols to pivot
bee_cols <- c(
  "honeybee",
  "bumble_bee",
  "large_dark_bee",
  "small_dark_bee",
  "greenbee",
  "non_bee")



# Process survey data ----

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
      grepl("lawn", habitat) | grepl("garden", habitat) ~ "lawn-and-garden",
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
      grepl("spray", management) & grepl("low", management) ~ "low spray",
      grepl("spray", management) & grepl("no", management) ~ "no spray",
      T ~ "other"),
    management = factor(management, levels = management_list$type)) %>%
  left_join(rename(management_list, management = type, management_name = label)) %>%
  mutate(
    lat_rnd = round(lat, 1),
    lng_rnd = round(lng, 1),
    grid_pt = paste(lat_rnd, lng_rnd, sep = ", "),
    inwi = between(lat, 42.49, 47.08) & between(lng, -92.89, -86.80)) %>%
  mutate(
    remote_id = id,
    id = 1:length(id)) %>%
  left_join(plant_replace) %>%
  mutate(crop = ifelse(is.na(new_crop), crop, new_crop)) %>%
  left_join(plant_list) %>%
  mutate(
    focal = new_crop %in% focal_plant_list$new_crop,
    plant_group = ifelse(focal, "non-crop focal", plant_group)) %>%
  droplevels()



# Get habitat/management/plant lists ----

# make ranked list of habitat types
habitats <- wibee %>%
  group_by(habitat, habitat_name) %>%
  summarise(surveys = n(), .groups = "drop") %>%
  arrange(desc(surveys)) %>%
  rename(type = habitat, label = habitat_name) %>%
  mutate(label = fct_inorder(label)) %>%
  drop_na()

# make ranked list of management types
managements <- wibee %>%
  group_by(management, management_name) %>%
  summarise(surveys = n(), .groups = "drop") %>%
  arrange(desc(surveys)) %>%
  rename(type = management, label = management_name) %>%
  mutate(label = fct_inorder(label)) %>%
  drop_na()

# make ranked list of plants and reclass low-frequency ones
plant_ranks <- wibee %>%
  group_by(plant_group, plant_id, plant_label) %>%
  summarise(surveys = n(), .groups = "drop") %>%
  group_by(plant_group) %>%
  arrange(plant_group, desc(surveys)) %>%
  mutate(
    plant_rank = row_number(),
    plant_type = case_when(
      plant_id == "species:other" ~ "other-non-crop",
      plant_rank >= 15 & plant_group == "crop" ~ "other-crop",
      plant_rank >= 15 & plant_group == "non-crop" ~ "other-non-crop",
      T ~ plant_id),
    plant_label = case_when(
      plant_type == "other-crop" ~ "Other crop",
      plant_type == "other-non-crop" ~ "Other/Unknown non-crop plant",
      T ~ plant_label)) %>%
  drop_na()



# Save main survey data ----

surveys <- wibee %>%
  select(-c(remote_id, picture_url, plant_label)) %>%
  left_join(plant_ranks)

# separate surveys and ids for picture downloads. The ids will change if the filter is changed in the block above
images <- wibee %>%
  select(c("id", "remote_id", "picture_url")) %>%
  filter(!is.na(picture_url))



# Get final plant lists ----

plants <- surveys %>%
  group_by(plant_group, plant_type, plant_label) %>%
  summarise(surveys = n(), .groups = "drop") %>%
  arrange(plant_group, desc(surveys))

select_crops <- plants %>%
  filter(plant_group == "crop") %>%
  rename(type = plant_type, label = plant_label) %>%
  mutate(label = fct_inorder(label)) %>%
  drop_na()

focal_noncrops <- plants %>%
  filter(plant_group == "non-crop focal") %>%
  rename(type = plant_type, label = plant_label) %>%
  mutate(label = fct_inorder(label)) %>%
  drop_na()

select_noncrops <- plants %>%
  filter(plant_group == "non-crop") %>%
  rename(type = plant_type, label = plant_label) %>%
  mutate(label = fct_inorder(label)) %>%
  drop_na()



# Create long-form dataset ----

surveys_long <- surveys %>%
  pivot_longer(cols = bees$type, names_to = "bee", values_to = "count") %>%
  left_join(
    rename(bees, bee = type, bee_name = label, bee_color = color, bee_group = group),
    by = "bee")




# Map data and other summaries ----

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

