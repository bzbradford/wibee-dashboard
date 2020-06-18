## process wibee survey data ##

library(tidyverse)
library(sf)
library(leaflet)


# Load and define ------------------------------------------------------------

# read csv
wibee_in <- read_csv("WiBee-SurveySummaries-05252020.csv")
glimpse(wibee_in)

# which columns to convert from char to fact
fct_cols <- c(
  "site_type",
  "cloud_cover",
  "wind_intensity",
  "temperature",
  "management_type",
  "crop"
)

# not useful
drop_cols <-c(
  "picture_id",
  "picture_url"
)

# bee data cols
bee_cols <- 
  c("bumble_bee",
    "honeybee",
    "large_dark_bee",
    "small_dark_bee",
    "greenbee",
    "non_bee")

# bee amended columns (drop for now)
bee_cols_amended <- 
  c("bumble_bee_amended",
    "honeybee_amended",
    "large_dark_bee_amended",
    "small_dark_bee_amended",
    "greenbee_amended",
    "non_bee_amended")

wi_shp <- read_sf("wibee_dashboard/shp/wi_county_500k.shp")
us_shp <- read_sf("wibee_dashboard/shp/conus_state_500k.shp")


# Process data ------------------------------------------------------------

# edit input data
surveys <- wibee_in %>%
  select(-drop_cols) %>%
  select(-bee_cols_amended) %>%
  mutate_at(fct_cols, as.factor) %>%
  mutate_at(bee_cols, replace_na, 0) %>%
  mutate(full_duration = case_when(duration == "5 minutes" ~ T, T ~ F)) %>%
  mutate(date = as.Date(ended_at))

glimpse(surveys)
unique(surveys$site_name)

# pivot longer for plotting etc
surveys_long <- surveys %>%
  pivot_longer(bee_cols, names_to = "species", values_to = "count") %>%
  mutate(species = factor(species, levels = bee_cols))


# create sf object of survey sites for plotting on map
surveys_sf <- surveys %>%
  filter(!(is.na(lat) | is.na(lng))) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326)



# Plots -------------------------------------------------------------------

# peek at duration values
surveys %>%
  mutate(implied_duration = as.integer(ended_at - started_at)) %>%
  group_by(full_duration, duration) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = duration, y = n, fill = full_duration)) +
  geom_col() +
  geom_text(aes(label = n)) +
  coord_flip()

# peek at completed surveys by user id
surveys_long %>%
  filter(full_duration) %>%
  filter(ended_at > "2020-01-01") %>%
  ggplot(aes(x = factor(user_id), y = count, fill = species, group = ended_at)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = bee_colors) +
  theme_classic()

# peek at completed surveys by user id
surveys_long %>%
  filter(full_duration) %>%
  filter(user_id %in% c(6, 24, 123)) %>%
  filter(date > "2020-01-01") %>%
  mutate(date = format(date, "%b %d")) %>%
  ggplot(aes(x = date, y = count, fill = species, group = id)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = bee_colors) +
  theme_classic() +
  facet_wrap(~ user_id)

# sites on map
ggplot() +
  geom_sf(data = wi_shp) +
  geom_sf(data = surveys_sf)

ggplot() +
  geom_sf(data = us_shp) +
  geom_sf(data = surveys_sf)
