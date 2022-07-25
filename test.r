
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




# Plots -------------------------------------------------------------------

cur_year <- 2022

df <- surveys_long %>%
  mutate(date = ISOdate(max(year), month, day))

df %>%
  group_by(month, bee_name, bee_color) %>%
  summarise(count = round(mean(count), 1)) %>%
  mutate(date = ISOdate(cur_year, month, 1)) %>%
  plot_ly(
    x = ~ date,
    y = ~ count,
    type = "bar",
    xperiod = "M1",
    xperiodalignment = "middle",
    color = ~ bee_name,
    colors = ~ levels(.$bee_color),
    marker = list(line = list(color = "#ffffff", width = .25))) %>%
  plotly::layout(
    barmode = "stack",
    title = list(text = "<b>Daily average pollinator visitation rates</b>", font = list(size = 15)),
    xaxis = list(
      title = "",
      type = "date",
      tickformat = "%B",
      dtick = "M1",
      ticklabelmode = "period"),
    yaxis = list(title = "Number of insect visits per survey"),
    hovermode = "x unified",
    legend = list(orientation = "h"),
    bargap = 0
  )


df %>%
  group_by(week, bee_name, bee_color) %>%
  summarise(count = round(mean(count), 1)) %>%
  mutate(date = ISOdate(cur_year, 1, 1) + lubridate::weeks(week - 1)) %>%
  plot_ly(
    x = ~ date,
    y = ~ count,
    type = "bar",
    color = ~ bee_name,
    colors = ~ levels(.$bee_color),
    marker = list(line = list(color = "#ffffff", width = .25))) %>%
  plotly::layout(
    barmode = "stack",
    title = list(text = "<b>Daily average pollinator visitation rates</b>", font = list(size = 15)),
    xaxis = list(
      title = "",
      type = "date",
      tickformat = "%b %d"),
    yaxis = list(title = "Number of insect visits per survey"),
    hovermode = "x unified",
    legend = list(orientation = "h"),
    bargap = 0
  )



df %>%
  group_by(date, bee_name, bee_color) %>%
  summarise(visit_rate = round(mean(count), 1), .groups = "drop") %>%
  droplevels() %>%
  plot_ly(
    x = ~ date,
    y = ~ visit_rate,
    type = "bar",
    color = ~ bee_name,
    colors = ~ levels(.$bee_color),
    marker = list(line = list(color = "#ffffff", width = .25))) %>%
  plotly::layout(
    barmode = "stack",
    title = list(text = "<b>Daily average pollinator visitation rates</b>", font = list(size = 15)),
    xaxis = list(title = "", type = "date", tickformat = "%B %d"),
    yaxis = list(title = "Number of insect visits per survey"),
    hovermode = "x unified",
    legend = list(orientation = "h"),
    bargap = 0
  )


surveys %>%
  head(1000) %>%
  arrange(user_id) %>%
  mutate(user_label = fct_inorder(paste("User", user_id))) %>%
  group_by(year, week, user_label) %>%
  summarise(surveys_by_user = n(), .groups = "drop_last") %>%
  mutate(date = ISOdate(year, 1, 1) + lubridate::weeks(week - 1)) %>%
  arrange(date, desc(surveys_by_user)) %>%
  plot_ly(
    x = ~ date,
    y = ~ surveys_by_user,
    type = "bar",
    name = ~ user_label,
    xperiodalignment = "left",
    marker = list(line = list(color = "#ffffff", width = .25))) %>%
  plotly::layout(
    barmode = "stack",
    title = list(
      text = "<b>Weekly total number of completed surveys</b>",
      font = list(size = 15)),
    xaxis = list(
      title = "",
      type = "date",
      tickformat = "%b %d<br>%Y"),
    yaxis = list(title = "Number of surveys"),
    hovermode = "x unified",
    showlegend = F,
    bargap = 0
  )



# 2022 users --------------------------------------------------------------

surveys_long %>%
  filter(year == 2022, habitat == "orchard") %>%
  group_by(bee_name) %>%
  summarise(mean_count = mean(count)) %>%
  janitor::adorn_totals()


surveys %>%
  filter(year == 2022) %>%
  count(user_id)

