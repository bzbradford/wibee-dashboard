# Handles plant lists for WiBee

library(tidyverse)

kebab <- function(s) {
  gsub(" ", "-", tolower(s))
}

# crop list
crops <- read_csv("plants/crop-list.csv") %>%
  mutate(
    PlantGroup = "crop",
    Label = CommonName)


# non-crops
noncrop_families <- 
  read_csv("plants/non-crop-family-list.csv") %>%
  mutate(
    TaxonLevel = "Family",
    ScientificName = Family,
    Label = paste0(Family, " (", CommonName, ")"))

noncrop_genera <- 
  read_csv("plants/non-crop-genus-list.csv") %>%
  mutate(
    TaxonLevel = "Genus",
    ScientificName = Genus,
    Label = paste0("Genus <i>", Genus, "</i> (", str_trunc(CommonName, 20), ")"))

noncrop_species <- 
  read_csv("plants/non-crop-species-list.csv") %>%
  mutate(
    TaxonLevel = "Species",
    ScientificName = Species,
    Label = paste0("<i>", Species, "</i> (", str_trunc(CommonName, 20), ")"))

noncrop_other <- 
  tibble(
    PlantGroup = "non-crop",
    PlantID = "other",
    ScientificName = "Other",
    CommonName = "Other",
    Label = "Other non-crop plant")

noncrops <- 
  bind_rows(noncrop_families, noncrop_genera, noncrop_species) %>%
  mutate(
    PlantGroup = "non-crop",
    PlantID = kebab(paste0(TaxonLevel, ":", ScientificName))) %>%
  bind_rows(noncrop_other)

# focal non-crops
noncrop_focal <- read_csv("plants/non-crop-focal-list.csv") %>%
  mutate(PlantGroup = "non-crop focal") %>%
  mutate(Label = case_when(
    TaxonLevel == "Species" ~ paste0(CommonName, " (<i>", Species, "</i>)"),
    TaxonLevel == "Genus" ~ paste0(CommonName, "(Genus <i>", Genus, "</i>)"),
    T ~ CommonName
  ))


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
all_plants <- known_plants %>%
  mutate(crop = PlantID) %>%
  bind_rows(legacy_plants) %>%
  select(
    crop,
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

# export plant list
all_plants %>% write_csv("data/plant-list.csv")
