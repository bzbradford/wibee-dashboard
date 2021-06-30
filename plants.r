# Handles plant lists for WiBee

library(tidyverse)


# crop list
crops <- read_csv("plants/crop-list.csv") %>%
  mutate(PlantType = "crop") %>%
  rename(PlantID = ID)


# non-crops
noncrop_families <- 
  read_csv("plants/non-crop-family-list.csv") %>%
  mutate(
    ScientificName = Family,
    TaxonLevel = "Family")

noncrop_genera <- 
  read_csv("plants/non-crop-genus-list.csv") %>%
  mutate(
    ScientificName = Genus,
    TaxonLevel = "Genus")

noncrop_species <- 
  read_csv("plants/non-crop-species-list.csv") %>%
  mutate(
    ScientificName = Species,
    TaxonLevel = "Species")

noncrops <- 
  bind_rows(noncrop_families, noncrop_genera, noncrop_species) %>%
  mutate(PlantType = "non-crop") %>%
  mutate(PlantID = kebab(paste0(TaxonLevel, ":", ScientificName)))


# other
noncrop_other <- 
  tibble(
    PlantType = "non-crop",
    PlantID = "other",
    ScientificName = "Other",
    CommonName = "Other"
  )


# focal non-crops
noncrop_focal <- read_csv("plants/non-crop-focal-list.csv") %>%
  mutate(PlantType = "non-crop")


# all plant options excluding write-ins
known_plants <- bind_rows(crops, noncrop_focal, noncrops, noncrop_other)


# export values that don't match known plants (write-ins)
# wibee_in %>%
#   group_by(crop) %>%
#   summarise(surveys = n()) %>%
#   arrange(desc(surveys)) %>%
#   filter(!(crop %in% known_plants$plant)) %>%
#   write_csv("plants/unmatched-values.csv")

# Here I went through the unmatched list and added matches

# load legacy plant crossref list
legacy_plants <- read_csv("plants/legacy-plant-list.csv") %>% 
  left_join(known_plants)

# combine and save master plant list
plants <- known_plants %>%
  mutate(crop = PlantID) %>%
  bind_rows(legacy_plants) %>%
  select(
    crop,
    plant_id = PlantID,
    plant_type = PlantType,
    plant_name = ScientificName,
    plant_common_name = CommonName,
    plant_taxon_level = TaxonLevel,
    plant_family = Family,
    plant_genus = Genus,
    plant_species = Species
  )

# export plant list
plants %>% write_csv("data/plant-list.csv")
