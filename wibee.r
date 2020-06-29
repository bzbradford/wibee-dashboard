library(tidyverse)
library(leaflet)
library(cowplot)

df <- surveys_long %>% filter(bee_name != "Wild bees")


# Plot of daily bee averages
df2 <- df %>%
  group_by(date, bee_name, bee_color) %>%
  summarise(visit_rate = mean(count) / 5) %>%
  droplevels()

df2 %>%
  ggplot(aes(x = date, y = visit_rate, fill = bee_name)) +
  geom_col() +
  scale_fill_manual(values = levels(df2$bee_color)) +
  labs(x = "Survey date", y = "Average visits per minute", fill = "") +
  theme_light() +
  theme(text = element_text(size = 16))



# Plot of bee activity averages by site characteristics
# create working dataset
df2 <- df %>%
  rename(`By crop` = crop, `By habitat` = habitat, `By management` = management) %>%
  pivot_longer(cols = c("By habitat", "By crop", "By management"), names_to = "category") %>%
  mutate(category = factor(category, c("By habitat", "By crop", "By management"))) %>%
  group_by(category, value, bee_name, bee_color) %>%
  summarise(visit_rate = mean(count) / 5, n = n()) %>%
  droplevels()

# get matching surveys counts by category
survey_count <- df2 %>% group_by(category, value) %>% summarise(n = mean(n))

# plot
df2 %>%
  ggplot() +
  geom_col(aes(x = value, y = visit_rate, fill = bee_name)) +
  geom_text(
    data = survey_count,
    aes(x = value, y = -.15, label = paste0("(", n, ")")),
    size = 3) +
  scale_fill_manual(values = levels(df2$bee_color)) +
  labs(x = "", y = "Average visits per minute", fill = "") +
  theme(axis.text.x = element_text(angle = 90,vjust = .5, hjust = 1)) +
  facet_grid(. ~ category, scales = "free_x") +
  theme(text = element_text(size = 16))



## histograms of total insect counts per survey
# exclude zero-counts from histograms
df2 <- df %>%
  filter(count > 0) %>%
  droplevels()

# plot
df2 %>%
  ggplot(aes(x = count, fill = bee_name)) +
  geom_histogram(bins = 20) +
  facet_wrap( ~ bee_name) +
  scale_fill_manual(values = levels(df2$bee_color)) +
  labs(x = "Number of flower visits per survey", y = "Number of surveys", fill = "")




# dustbin -----------------------------------------------------------------

# surveys_long %>%
#   filter(count > 0) %>%
#   mutate(visit_rate = count / 5) %>%
#   ggplot(aes(x = visit_rate, fill = bee_name)) +
#   geom_histogram() +
#   facet_wrap(~ bee_name, scale = "free_y") +
#   scale_fill_manual(values = bee_palette(levels(surveys_long$bee_name))) +
#   labs(x = "Visitation rate (when present)", y = "Number of surveys")
# 
# 

# df_in <- surveys_long
# if (nrow(df) > 0) {
#   df <- df_in %>%
#     droplevels() %>%
#     rename(`By crop` = crop, `By habitat` = habitat, `By management` = management) %>%
#     pivot_longer(cols = c("By crop", "By habitat", "By management")) %>%
#     group_by(name, value, bee_class, bee_name) %>%
#     summarise(visit_rate = mean(count) / 5, n = n())
#   df_labels <- df %>% group_by(name, value) %>% summarise(n = n[1])
#   
#   if (T) {
#     plt <- df %>%
#       summarise(visit_rate = sum(visit_rate)) %>%
#       ggplot() +
#       geom_col(aes(x = value, y = visit_rate, fill = bee_class)) +
#       geom_text(
#         data = df_labels,
#         aes(x = value, y = -.1, label = paste0("(", n, ")")),
#         size = 3) +
#       scale_fill_manual(values = bee_palette(levels(df$bee_class)))
#   } else {
#     plt <- df %>%
#       ggplot() +
#       geom_col(aes(x = value, y = visit_rate, fill = bee_name)) +
#       geom_text(
#         data = {group_by(., name, value) %>% summarise(n = n[1])},
#         aes(x = value, y = -.1, label = paste0("(", n, ")")),
#         size = 3) +
#       scale_fill_manual(values = bee_palette(levels(df$bee_name)))
#   }
#   
#   plt +
#     labs(x = "", y = "Average visits per minute", fill = "") +
#     theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
#     facet_grid(. ~ name, scales = "free_x") +
#     theme(text = element_text(size = 16))
# }


# observeEvent(filtered_surveys(), {
#   df <- filtered_surveys()
#   which_habitat <- input$which_habitat
#   which_crop <- input$which_crop
#   which_mgmt <- input$which_mgmt
#   
#   updateCheckboxGroupInput(
#     session,
#     "which_habitat",
#     choiceNames = {
#       filtered_surveys() %>%
#         count(habitat, .drop = F) %>%
#         mutate(label = paste0(habitat, " (", n, ")")) %>%
#         .$label
#     },
#     choiceValues = habitat_types,
#     selected = which_habitat
#   )
#   
#   updateCheckboxGroupInput(
#     session,
#     "which_crop",
#     choiceNames = {
#       filtered_surveys() %>%
#         count(crop, .drop = F) %>%
#         mutate(label = paste0(crop, " (", n, ")")) %>%
#         .$label
#     },
#     choiceValues = crop_types,
#     selected = which_crop
#   )
#   
#   updateCheckboxGroupInput(
#     session,
#     "which_mgmt",
#     choiceNames = {
#       filtered_surveys() %>%
#         count(management, .drop = F) %>%
#         mutate(label = paste0(management, " (", n, ")")) %>%
#         .$label
#     },
#     choiceValues = mgmt_types,
#     selected = which_mgmt
#   )
# })



# left_join(
#   tibble(management = as.factor(mgmt_types)),
#   surveys %>%
#     group_by(management) %>%
#     summarise(n = n()),
#   by = "management") %>%
#   replace_na(list(n = 0)) %>%
#   mutate(label = paste0(management, " (", n, ")")) %>%
#   .$label






# df_crop <- surveys_long %>%
#   mutate(type = as.character(crop)) %>%
#   group_by(type, bee_name) %>%
#   summarise(mean_count = mean(count), n = n()) %>%
#   mutate(type_label = "By crop")
# df_site <- surveys_long %>%
#   mutate(type = as.character(habitat_type)) %>%
#   group_by(type, bee_name) %>%
#   summarise(mean_count = mean(count), n = n()) %>%
#   mutate(type_label = "By site")
# df_mgmt <- surveys_long %>%
#   mutate(type = as.character(management_type)) %>%
#   group_by(type, bee_name) %>%
#   summarise(mean_count = mean(count), n = n()) %>%
#   mutate(type_label = "By management")
# df <- bind_rows(df_crop, df_site, df_mgmt) %>%
#   mutate(mean_count = mean_count / 5) %>%
#   group_by(type_label, type)
# df_labels <- df %>%
#   summarise(n = mean(n))
# 
# 
# 
# 
# df %>%
#   ggplot() +
#   geom_col(aes(x = type, y = mean_count, fill = bee_name)) +
#   geom_text(
#     data = df_labels,
#     aes(x = type, y = -.1, label = paste0("(", n, ")")),
#     size = 3) +
#   scale_fill_manual(values = bee_palette(1:6)) +
#   labs(x = "", y = "Average visits per minute", fill = "") +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
#   facet_grid(. ~ type_label, scales = "free_x")


# output$beePlotSiteType <- renderPlot({
#   df <- filtered_surveys_long()
#   if (nrow(df) > 0) {
#     df %>%
#       group_by(habitat_type, bee_name) %>%
#       summarise(mean_count = mean(count)) %>%
#       ggplot(aes(x = habitat_type, y = mean_count, fill = bee_name)) +
#       geom_bar(stat = "identity") +
#       scale_fill_manual(values = bee_palette(input$which_bees)) +
#       labs(x = "Site type", y = "Mean insect count", fill = "") +
#       coord_flip()}
# })
# 
# output$beePlotCropType <- renderPlot({
#   df <- filtered_surveys_long()
#   if (nrow(df) > 0) {
#     df %>%
#       group_by(crop, bee_name) %>%
#       summarise(mean_count = mean(count)) %>%
#       arrange(desc(mean_count)) %>%
#       ggplot(aes(x = crop, y = mean_count, fill = bee_name)) +
#       geom_col() +
#       scale_fill_manual(values = bee_palette(input$which_bees)) +
#       labs(x = "Crop type", y = "Mean insect count", fill = "") +
#       coord_flip()}
# })
# 
# output$beePlotMgmtType <- renderPlot({
#   df <- filtered_surveys_long()
#   if (nrow(df) > 0) {
#     df %>%
#       group_by(management_type, bee_name) %>%
#       summarise(mean_count = mean(count)) %>%
#       ggplot(aes(x = management_type, y = mean_count, fill = bee_name)) +
#       geom_col() +
#       scale_fill_manual(values = bee_palette(input$which_bees)) +
#       labs(x = "Management type", y = "Mean insect count", fill = "") +
#       coord_flip()}
# })

# # survey points for map, aggregated
# survey_pts1 <- surveys %>%
#   drop_na(lng, lat) %>%
#   mutate(
#     lng_rnd = round(lng, 1),
#     lat_rnd = round(lat, 1),
#     wild_bee = bumble_bee + large_dark_bee + small_dark_bee + greenbee) %>%
#   group_by(lng_rnd, lat_rnd) %>%
#   summarise(
#     n_surveys = n(),
#     lng = mean(lng),
#     lat = mean(lat),
#     hb = round(mean(honeybee)/5,1),
#     wb = round(mean(wild_bee)/5,1),
#     nb = round(mean(non_bee)/5,1)) %>%
#   ungroup() %>%
#   select(-c(lng_rnd, lat_rnd)) %>%
#   mutate(
#     lng = lng + runif(length(.$lng), -.001, .001),
#     lat = lat + runif(length(.$lat), -.001, .001)
#   )


# leaflet map of survey sites (points)
# output$surveyMap1 <- renderLeaflet({
#   leaflet(survey_pts1) %>%
#     addTiles() %>%
#     addCircles(~ lng, ~ lat, radius = 3000, opacity = 0) %>%
#     addMarkers( ~ lng, ~ lat,
#       label = ~ paste(n_surveys, "surveys"),
#       popup = ~ paste0(
#         "<strong>Total surveys: </strong>", n_surveys, "<br/>",
#         "<strong>Mean visits per minute:</strong><br/>",
#         "Honey bees: ", hb, "<br/>",
#         "Wild bees: ", wb, "<br/>",
#         "Non-bees: ", nb),)
# })


# # leaflet map
# output$surveyMap2 <- renderLeaflet({
#   leaflet(survey_pts2) %>%
#     addTiles() %>%
#     addRectangles(
#       lng1 = ~ lng - .005, lng2 = ~ lng + .005,
#       lat1 = ~ lat - .005, lat2 = ~ lat + .005,
#       label = ~ paste(n_surveys, "surveys"),
#       popup = ~ paste0(
#         "<strong>Total surveys: </strong>", n_surveys, "<br/>",
#         "<strong>Mean visits per minute:</strong><br/>",
#         "Honey bees: ", hb, "<br/>",
#         "Wild bees: ", wb, "<br/>",
#         "Non-bees: ", nb),
#       weight = 2,
#       opacity = 1,
#       fillOpacity = .25)
# })



# observe({
#   leafletProxy("surveyMap", data = survey_pts2) %>%
#     clearMarkers() %>%
#     addMarkers(~lng, ~lat, icon = bee_icon,
#       label = ~ paste(n_surveys, "surveys"),
#       popup = ~ paste0(
#         "<strong>Total surveys: </strong>", n_surveys, "<br/>",
#         "<strong>Mean visits per minute:</strong><br/>",
#         "Honey bees: ", hb, "<br/>",
#         "Wild bees: ", wb, "<br/>",
#         "Non-bees: ", nb),
#       # options = markerOptions(opacity = op),
#       clusterOptions = markerClusterOptions(showCoverageOnHover = F)
#       )
# })
