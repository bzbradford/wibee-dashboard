
library(RColorBrewer)

df <- surveys %>%
  filter(inwi == T) %>%
  mutate(lat = lat_rnd, lng = lng_rnd) %>%
  group_by(grid_pt, lat, lng) %>%
  summarise(n_surveys = n(), .groups = "drop")
  
surveys_long


pal <- colorNumeric(
  palette = "YlOrRd",
  domain = df$n_surveys
)

df %>%
  leaflet() %>%
  addTiles() %>%
  addRectangles(
    lng1 = ~ lng - .05, lng2 = ~ lng + .05,
    lat1 = ~ lat - .05, lat2 = ~ lat + .05,
    layerId = ~ grid_pt,
    label = ~ paste(n_surveys, "surveys"),
    weight = 0.25,
    opacity = 1,
    color = "grey",
    fillOpacity = .75,
    fillColor = ~pal(n_surveys),
    highlight = highlightOptions(
      weight = 2,
      color = "red")
  )

