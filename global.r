#---- GLOBAL ----#

library(tidyverse)
library(httr)



# Load remote data --------------------------------------------------------

# check when last data refresh occurred
if(file.exists('./data/refresh_time')) {
  refresh_time <- readRDS('./data/refresh_time')
} else {
  refresh_time <- as.POSIXct('2020-01-01')
}


# update surveys at most once an hour. Writes to local csv
if(refresh_time < Sys.time() - 3600) {
  get_surveys <-
    content(
      GET(url = 'https://wibee.caracal.tech/api/data/survey-summaries',
        config = add_headers(Authorization = Sys.getenv('caracal_token')))
    )
  if(is.data.frame(get_surveys)) {
    arrange(get_surveys, ended_at) %>% write_csv('./data/surveys.csv')
    refresh_time <- Sys.time()
    saveRDS(refresh_time, './data/refresh_time')
    message('Survey data refreshed from remote database.')
  } else {
    message('Unable to refresh data from remote server.')
  }
} else {
  message('Skipping refresh, last query < 1 hr ago.')
}



# Define local variables --------------------------------------------------

# read data from local csv
wibee_in <- read_csv('./data/surveys.csv', col_types = cols())


# survey attribute cols to keep
keep_cols <- c(
  'id',
  'user_id',
  'lat',
  'lng',
  'ended_at',
  'duration',
  'site_type',
  'crop',
  'management_type')


# bee cols to pivot
bee_cols <- c(
  'honeybee',
  'bumble_bee',
  'large_dark_bee',
  'small_dark_bee',
  'greenbee',
  'non_bee')


# load types and labels
bees <- read_csv('data/bees.csv', col_types = cols()) %>% mutate_all(fct_inorder)
habitats <- read_csv('data/habitats.csv', col_types = cols()) %>% mutate_all(fct_inorder)
crops <- read_csv('data/crops.csv', col_types = cols()) %>% mutate_all(fct_inorder)
managements <- read_csv('data/managements.csv', col_types = cols()) %>% mutate_all(fct_inorder)


# formatted bee names for ungrouped
bee_names <- c(
  'Honey bees',
  'Bumble bees',
  'Large dark bees',
  'Small dark bees',
  'Green bees',
  'Non-bees')


# formatted names for wild bee grouping
wildbee_names <- c('Honey bees', 'Wild bees', 'Non-bees')



# Process survey data -----------------------------------------------------

# generate main dataset
surveys <- wibee_in %>%
  select(all_of(c(keep_cols, bee_cols))) %>%
  rename(habitat = site_type, management = management_type, date = ended_at) %>%
  mutate(date = as.Date(date)) %>%
  filter(duration == '5 minutes', date >= '2020-04-01') %>%
  mutate(across(all_of(bee_cols), replace_na, 0)) %>%
  mutate(wild_bee = bumble_bee + large_dark_bee + small_dark_bee + greenbee) %>%
  mutate(habitat = replace_na(habitat, 'other')) %>%
  mutate(habitat = case_when(
    habitat %in% habitats$type ~ habitat,
    T ~ 'other')) %>%
  mutate(habitat = factor(habitat, levels = habitats$type)) %>%
  mutate(crop = tolower(crop)) %>%
  mutate(crop = case_when(
    crop %in% crops$type ~ crop,
    grepl('berry', crop) ~ 'other-berry',
    T ~ 'other')) %>%
  mutate(crop = factor(crop, levels = crops$type)) %>%
  mutate(management = replace_na(management, 'none')) %>%
  mutate(management = case_when(
    management %in% managements$type ~ management,
    grepl('organic', management) ~ 'organic',
    grepl('conventional', management) ~ 'conventional',
    T ~ 'other')) %>%
  mutate(management = factor(management, levels = managements$type)) %>%
  left_join(
    rename(habitats, habitat = type, habitat_name = label),
    by = 'habitat') %>%
  left_join(
    rename(crops, crop = type, crop_name = label),
    by = 'crop') %>%
  left_join(
    rename(managements, management = type, management_name = label),
    by = 'management') %>%
  mutate(
    lat_rnd = round(lat, 1),
    lng_rnd = round(lng, 1),
    grid_pt = paste(lat_rnd, lng_rnd, sep = ', '),
    inwi = between(lat, 42.49, 47.08) & between(lng, -92.89, -86.80),
    id = 1:length(id)) %>%
  droplevels()


# keep only attributes that have occurred at least once
habitats <- filter(habitats, type %in% levels(surveys$habitat)) %>% droplevels()
crops <- filter(crops, type %in% levels(surveys$crop)) %>% droplevels()
managements <- filter(managements, type %in% levels(surveys$management)) %>% droplevels()


# pivot longer for some data analysis
surveys_long <- surveys %>%
  pivot_longer(cols = bees$type, names_to = 'bee', values_to = 'count') %>%
  left_join(
    rename(bees, bee = type, bee_name = label, bee_color = color, bee_category = category),
    by = 'bee')



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
    .groups = 'drop') %>%
  mutate(grid_pt = paste(lat, lng, sep = ', '))


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
  summarise(tot_count = sum(count), .groups = 'drop') %>%
  mutate(pct_count = sprintf('%1.1f%%', tot_count / sum(.$tot_count) * 100))

