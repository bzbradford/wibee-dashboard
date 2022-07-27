# plants.R
# Handles plant lists for WiBee. Not run during app launch, must be sourced manually.

library(tidyverse)


kebab <- function(s) {
  gsub(" ", "-", tolower(s))
}

plantLabels <- function(sciName, comName) {
  names <- unlist(strsplit(comName, ", "))
  n <- length(names)
  if (n == 1) {
    label <- paste0(comName, " (", sciName, ")")
  } else if (n == 2) {
    label <- paste0(names[1], " or ", names[2], " (", sciName, ")")
  } else if (n > 2) {
    label <- paste0(names[1], ", ", names[2], ", etc (", sciName, ")")
  } else {
    label <- sciName
  }
  label
}

# crop list
crops <- read_csv("plants/crop-list.csv", show_col_types = F) %>%
  mutate(
    PlantGroup = "crop",
    Label = CommonName) %>%
  rename(PlantID = ID)

# focal non-crops
# noncrop_focal <- read_csv("plants/non-crop-focal-list.csv") %>%
#   mutate(PlantGroup = "non-crop focal") %>%
#   mutate(Label = case_when(
#     TaxonLevel == "Species" ~ paste0(CommonName, " (", Species, ")"),
#     TaxonLevel == "Genus" ~ paste0(CommonName, " (Genus ", Genus, ")"),
#     T ~ CommonName
#   ))

# noncrop_focal <- read_csv("plants/non-crop-focal-list.csv") %>%
#   mutate(Focal = TRUE)

# non-crops
noncrop_families <- 
  read_csv("plants/non-crop-family-list.csv", show_col_types = F) %>%
  mutate(
    TaxonLevel = "Family",
    ScientificName = Family,
    Label = paste0(CommonName, " (", Family, ")"))

noncrop_genera <- 
  read_csv("plants/non-crop-genus-list.csv", show_col_types = F) %>%
  mutate(
    TaxonLevel = "Genus",
    ScientificName = Genus,
    Label = mapply(plantLabels, paste0("Genus <em>", Genus, "</em>"), CommonName))

noncrop_species <- 
  read_csv("plants/non-crop-species-list.csv", show_col_types = F) %>%
  mutate(
    TaxonLevel = "Species",
    ScientificName = Species,
    Label = mapply(plantLabels, paste0("<em>", Species, "</em>"), CommonName))

noncrop_other <- 
  tibble(
    PlantGroup = "non-crop",
    PlantID = "other-non-crop",
    ScientificName = "Other",
    CommonName = "Other",
    Label = "Other non-crop plant")

noncrops <- 
  bind_rows(noncrop_families, noncrop_genera, noncrop_species) %>%
  mutate(
    PlantGroup = "non-crop",
    PlantID = kebab(paste0(TaxonLevel, ":", ScientificName))) %>%
  bind_rows(noncrop_other)


# all plant options excluding write-ins
known_plants <- bind_rows(crops, noncrops) %>%
  select(
    crop = PlantID,
    plant_id = PlantID,
    plant_group = PlantGroup,
    plant_name = ScientificName,
    plant_common_name = CommonName,
    plant_taxon_level = TaxonLevel,
    plant_family = Family,
    plant_genus = Genus,
    plant_species = Species,
    plant_label = Label
  )

known_plants %>% write_csv("plants/known-plant-list.csv")


# export values that don't match known plants (write-ins)
# wibee_in %>%
#   group_by(crop) %>%
#   summarise(surveys = n()) %>%
#   arrange(desc(surveys)) %>%
#   filter(!(crop %in% known_plants$plant)) %>%
#   write_csv("plants/unmatched-values.csv")

# Here I went through the unmatched list and added matches

# 
# # load legacy plant crossref list
# legacy_plants <- read_csv("plants/legacy-plant-list.csv") %>% 
#   left_join(known_plants)
# 
# 
# # combine and save master plant list
# all_plants <- known_plants %>%
#   mutate(crop = ifelse(is.na(AlternateID), PlantID, AlternateID)) %>%
#   bind_rows(legacy_plants) %>%
#   select(
#     crop,
#     plant_id = PlantID,
#     plant_group = PlantGroup,
#     plant_name = ScientificName,
#     plant_common_name = CommonName,
#     plant_taxon_level = TaxonLevel,
#     plant_family = Family,
#     plant_genus = Genus,
#     plant_species = Species,
#     plant_label = Label
#   )
# 
# # export plant list
# all_plants %>% write_csv("plants/plant-list.csv")
# 
