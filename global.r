# global.R

# Required packages ----

suppressMessages({
  # core
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(RColorBrewer)
  library(httr)
  
  # shiny
  library(shiny)
  library(shinythemes)
  library(shinyWidgets)
  library(shinyBS)
  library(shinyjs)
  
  # display
  library(DT)
  library(leaflet)
  library(plotly)
})


# Refresh surveys --------------------------------------------------------------

# pull data from remote db
fetch_remote <- function(start_date = NULL) {
  endpoint <- "https://wibee.caracal.tech/api/data/survey-summaries"
  if (!is.null(start_date)) {
    endpoint <- sprintf("%s?start_date=%s", endpoint, start_date)
  }
  response <- content(
    GET(
      url = endpoint,
      config = add_headers(Authorization = Sys.getenv("caracal_token"))
    ),
    show_col_types = F
  )
  if (!is.data.frame(response)) stop("Invalid response from remote database")
  response
}

# handle pulling or merging new surveys with stored surveys
get_surveys <- function(force = FALSE) {
  
  # check when last data refresh occurred
  if (!exists("refresh_time")) refresh_time <- as.POSIXct("2020-1-1")
  
  # if we already have surveys.csv, just load recent surveys and merge with stored surveys
  if (file.exists("surveys.csv.gz")) {
    existing_surveys <- read_csv("surveys.csv.gz", show_col_types = FALSE)
    
    if ((refresh_time < Sys.time() - 60 * 60) | force) {
      tryCatch({
        max_date <- as.Date(max(existing_surveys$ended_at))
        new_surveys <- fetch_remote(max_date - 7)
        updated_surveys <- existing_surveys %>%
          bind_rows(new_surveys) %>%
          distinct(id, .keep_all = TRUE) %>%
          arrange(ended_at)
        survey_count <- nrow(updated_surveys)
        new_survey_count <- survey_count - nrow(existing_surveys)
        
        # only rewrite csv if there are new surveys
        if (new_survey_count > 0) {
          write_csv(updated_surveys, "surveys.csv.gz")
        }
        
        status <- sprintf("Survey data refreshed from remote database. %s total surveys found (%s since last refresh).", survey_count, new_survey_count)
      },
        error = function(e) {
          status <- sprintf("Unable to refresh surveys from remote database. Reason: %s.", e)
          updated_surveys <- existing_surveys
        }
      )
    } else {
      updated_surveys <- existing_surveys
      status <- sprintf(
        "Skipped data refresh, last query < 1 hr ago. %s total surveys in database, most recent completed on %s.",
        nrow(existing_surveys),
        format(max(existing_surveys$ended_at), format = "%Y-%m-%d %H:%M:%S", tz = "America/Chicago", usetz = TRUE)
      )
    }
  } else {
    updated_surveys <- fetch_remote() %>%
      arrange(ended_at)
    write_csv(updated_surveys, "surveys.csv.gz")
    status <- sprintf("Survey data refreshed from remote database. %s total surveys found.", nrow(updated_surveys))
  }
  
  # save data to global env
  assign("refresh_time", Sys.time(), envir = .GlobalEnv)
  assign("status", status, envir = .GlobalEnv)
  message(status)
  return(updated_surveys)
}

raw_surveys <- get_surveys()



# Load/create helper data ------------------------------------------------------

# bee names and colors
bees <- read_csv("data/bees.csv", show_col_types = F) %>%
  mutate_all(fct_inorder)

# formatted bee names for ungrouped
bee_names <- as.character(filter(bees, type != "wild_bee")$label)

# formatted names for wild bee grouping
wildbee_names <- levels(bees$group)


## load habitat and management types ----
habitat_list <- read_csv("data/habitats.csv", show_col_types = F)
management_list <- read_csv("data/managements.csv", show_col_types = F)


## load plant lists ----
plant_list <- read_csv("plants/known-plant-list.csv", show_col_types = F)
legacy_plant_list <- read_csv("plants/legacy-plant-list.csv", show_col_types = F)
focal_plant_list <- read_csv("plants/focal-plant-list.csv", show_col_types = F)
plant_replace <- bind_rows(legacy_plant_list, focal_plant_list)


## survey attribute cols to keep ----
keep_cols <- c(
  "id",
  "remote_id",
  "user_id",
  "lat",
  "lng",
  "ended_at",
  "duration",
  "site_type",
  "crop",
  "management_type",
  "picture_url")


## bee column names ----
bee_cols <- c(
  "honeybee",
  "bumble_bee",
  "large_dark_bee",
  "small_dark_bee",
  "greenbee",
  "non_bee")

## Get user IDs ----
user_ids <- sort(unique(raw_surveys$user_id))


# Process survey data ----------------------------------------------------------

processed_surveys <- raw_surveys %>%
  mutate(
    bumble_bee = bumble_bee_amended,
    honeybee = honeybee_amended,
    large_dark_bee = large_dark_bee_amended,
    small_dark_bee = small_dark_bee_amended,
    greenbee = greenbee_amended,
    non_bee = non_bee_amended
  ) %>%
  arrange(created_at) %>%
  mutate(remote_id = id, id = 1:length(id)) %>%
  select(all_of(c(keep_cols, bee_cols))) %>%
  rename(
    date = ended_at,
    habitat = site_type,
    management = management_type) %>%
  filter(duration == "5 minutes", date >= "2020-04-01") %>%
  mutate(
    date = as.Date(date),
    year = lubridate::year(date),
    month = lubridate::month(date),
    week = lubridate::week(date),
    day = lubridate::day(date),
    doy = lubridate::yday(date),
    .after = "date") %>%
  mutate(across(all_of(bee_cols), ~ replace_na(.x, 0))) %>%
  mutate(wild_bee = bumble_bee + large_dark_bee + small_dark_bee + greenbee) %>%
  mutate(
    habitat = replace_na(habitat, "other"),
    habitat = case_when(
      habitat %in% habitat_list$type ~ habitat,
      grepl("lawn", habitat) | grepl("garden", habitat) ~ "lawn-and-garden",
      T ~ "other"),
    habitat = factor(habitat, levels = habitat_list$type)) %>%
  left_join(
    rename(habitat_list, habitat = type, habitat_name = label),
    by = "habitat") %>%
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
  left_join(
    rename(management_list, management = type, management_name = label),
    by = "management") %>%
  mutate(
    lat_rnd = round(lat, 1),
    lng_rnd = round(lng, 1),
    grid_pt = paste(lat_rnd, lng_rnd, sep = ", "),
    inwi = between(lat, 42.49, 47.08) & between(lng, -92.89, -86.80)) %>%
  left_join(plant_replace, by = "crop") %>%
  mutate(crop = ifelse(is.na(new_crop), crop, new_crop)) %>%
  left_join(plant_list, by = "crop") %>%
  mutate(
    focal = new_crop %in% focal_plant_list$new_crop,
    plant_group = ifelse(focal, "non-crop focal", plant_group)) %>%
  droplevels()



# Get habitat/management/plant lists -------------------------------------------

# make ranked list of habitat types
habitats <- processed_surveys %>%
  group_by(habitat, habitat_name) %>%
  summarise(surveys = n(), .groups = "drop") %>%
  arrange(desc(surveys)) %>%
  rename(type = habitat, label = habitat_name) %>%
  mutate(label = fct_inorder(label)) %>%
  drop_na()

# make ranked list of management types
managements <- processed_surveys %>%
  group_by(management, management_name) %>%
  summarise(surveys = n(), .groups = "drop") %>%
  arrange(desc(surveys)) %>%
  rename(type = management, label = management_name) %>%
  mutate(label = fct_inorder(label)) %>%
  drop_na()

# make ranked list of plants and reclass low-frequency ones
plant_ranks <- processed_surveys %>%
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


# Merge plant data and save final surveys --------------------------------------

surveys <- processed_surveys %>%
  select(-c(remote_id, picture_url, plant_label)) %>%
  left_join(plant_ranks, by = c("plant_id", "plant_group"))



# Get final plant lists based on survey data -----------------------------------

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



# Create long-form dataset -----------------------------------------------------

bee_join <- bees %>%
  rename(bee = type, bee_name = label, bee_color = color, bee_group = group)

surveys_long <- surveys %>%
  pivot_longer(cols = bees$type, names_to = "bee", values_to = "count") %>%
  left_join(bee_join, by = "bee")



# Map data and other summaries -------------------------------------------------

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

date_slider_min <- as.Date(format(Sys.Date(), "%Y-01-01"))
date_slider_max <- as.Date(format(Sys.Date(), "%Y-12-31"))

year_summary <- surveys %>%
  group_by(year) %>%
  summarise(
    surveys = n(),
    users = n_distinct(user_id),
    first_date = min(date),
    last_date = max(date)
  ) %>%
  mutate(label = paste0(
    "<b>", year, ":</b> ",
    format(surveys, big.mark = ","), " surveys by ",
    format(users, big.mark = ","), " contributors. ",
    format(first_date, "%b %d"), " - ",
    format(last_date, "%b %d"), "."))


# total counts for project summary
bee_totals <- surveys_long %>%
  filter(bee_name %in% wildbee_names) %>%
  group_by(bee_name) %>%
  summarise(tot_count = sum(count), .groups = "drop") %>%
  mutate(pct_count = sprintf("%1.1f%%", tot_count / sum(.$tot_count) * 100))

