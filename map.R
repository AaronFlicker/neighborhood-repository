library(tidyverse)
library(tigris)
library(leaflet)
library(leafpop)
library(sf)
library(leaflegend)
library(htmlwidgets)
# library(geojsonio)
# library(geojsonsf)
# library(geojsonlint)
allocations <- read.csv(
  "~/neighborhood bg allocations.csv",
  colClasses = c(
    "character", 
    rep("NULL", 3), 
    "character", 
    "numeric", 
    "character"
    )
  )
#oki <- read.csv("oki allocations.csv")
census <- read.csv("census_data.csv") |>
  mutate(across(Municipality:Neighborhood, \(x) coalesce(x, "All")))
asthma <- read.csv("asthma.csv")

asthma_cs <- asthma |>
  group_by(State, County, Municipality) |>
  summarise(across(AsthmaRegistry:AsthmaAdmissions, sum)) |>
  mutate(Neighborhood = "All")

asthma_county <- asthma |>
  group_by(State, County) |>
  summarise(across(AsthmaRegistry:AsthmaAdmissions, sum)) |>
  mutate(
    Municipality = "All",
    Neighborhood = "All"
  )

asthma_total <- asthma |>
  summarise(across(AsthmaRegistry:AsthmaAdmissions, sum)) |>
  mutate(
    County = "All",
    State = "All",
    Neighborhood = "All",
    Municipality = "All"
  )

all_data <- filter(asthma, Neighborhood != "All") |>
  rbind(asthma_cs) |>
  rbind(asthma_county) |>
  rbind(asthma_total) |>
  inner_join(census) |>
  mutate(
    AsthmaRegistryRate = AsthmaRegistry/Children,
    AsthmaAdmissionRate = AsthmaAdmissions/Children,
    Tier = case_when(
      DeprivationIndex >= .6 ~ "Highest",
      DeprivationIndex >= .475 ~ "Higher",
      DeprivationIndex >= .35 ~ "Medium",
      DeprivationIndex >= .225 ~ "Lower",
      TRUE ~ "Lowest"
    ),
    Tier = factor(
      Tier, 
      levels = c("Lowest", "Lower", "Medium", "Higher", "Highest")
      ),
    GEOID = row_number(),
    Area = case_when(
      Neighborhood != "All" ~ Municipality,
      Municipality != "All" ~ County,
      TRUE ~ "All"
    )
  )

benchmarks <- all_data |>
  filter(
    Neighborhood == "All",
    Municipality %in% c("Cincinnati", "All")
    ) |>
  mutate(Area = ifelse(Municipality == "Cincinnati", Municipality, County)) |>
  select(Area, ends_with("Rate"), MedianHHIncome, DeprivationIndex) |>
  pivot_longer(
    cols = WhiteRate:DeprivationIndex,
    names_to = "variable",
    values_to = "Benchmark"
  )

rates <- filter(all_data, County != "All") |>
  select(
    Area,
    County,
    Municipality,
    Neighborhood, 
    ends_with("Rate"), 
    MedianHHIncome, 
    DeprivationIndex
    ) |>
  pivot_longer(
    cols = WhiteRate:DeprivationIndex,
    names_to = "variable"
  ) |>
  inner_join(benchmarks) |>
  mutate(
    diff = 100*(value-Benchmark)/Benchmark,
    variable = str_remove(variable, "Rate")
    )

areas <- distinct(rates, Area, County, Municipality, Neighborhood) |>
  mutate(
    Label = case_when(
      Neighborhood != "All" ~ Neighborhood,
      Municipality != "All" ~ Municipality,
      TRUE ~ County
    ),
    geoid = row_number()
  ) |>
  inner_join(select(all_data, County, Municipality, Neighborhood, Tier))

hoods <- areas$geoid[areas$Neighborhood != "All"]
munis <- areas$geoid[areas$Municipality != "All" & areas$Neighborhood == "All"]
counties <- areas$geoid[areas$Municipality == "All"]

demo_graph <- function(i) {
  x <- filter(
    rates,
    variable %in% c(
      "White", 
      "Black", 
      "Hispanic", 
      "Child", 
      "ChildHH",
      "OtherLanguage",
      "LimitedEnglish"
      )
  ) |>
    inner_join(areas[i,]) |>
    mutate(
      variable = factor(
        variable, 
        levels = c(
          "White", 
          "Black", 
          "Hispanic", 
          "Child", 
          "ChildHH", 
          "OtherLanguage", 
          "LimitedEnglish"
        )
      ),
      textloc = case_when(
        diff == max(diff) & diff > 0 ~ .9*diff,
        diff == min(diff) & diff < 0 ~ diff-(.05*diff),
        diff > 0 ~ 1.1*diff,
        TRUE ~ diff+(.05*diff)
      )
    )
  
  y <- ggplot(x, aes(x = variable, y = diff)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    labs(x = NULL, y = "%", title = unique(x$Label)) +
    scale_y_continuous(labels = NULL) +
    scale_x_discrete(
      labels = c(
        "White",
        "Black",
        "Hispanic",
        "Children",
        "Households\nwith Children",
        "Language other\nthan English",
        "Limited\nEnglish"
      )
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5)) + 
    geom_text(
      aes(label = paste0(round(value*100, 1), "%"), y = textloc), 
      size = 4
      ) +
    annotate("segment", x = 0.5, xend = 7.5, y = 0, yend = 0)
  y
  return(y)
} 

hood_demo_popups <- lapply(hoods, function(k) {
  demo_graph(i = k)
  })

muni_demo_popups <- lapply(munis, function(k){
  demo_graph(i = k)
  })

county_demo_popups <- lapply(counties, function(k){
  demo_graph(i = k)
  })

di_graph <- function(i) {
  x <- filter(
    rates,
    variable %in% c(
      "Uninsured", 
      "Assistance", 
      "HighSchool", 
      "Poverty", 
      "Vacancy", 
      "MedianHHIncome",
      "DeprivationIndex"
      )
    ) |>
    inner_join(areas[i,]) |>
    mutate(
      value = ifelse(
        variable %in% c("MedianHHIncome", "DeprivationIndex"),
        value, 
        value*100
      ),
      Desired = variable %in% c("MedianHHIncome", "HighSchool"),
      Direction = diff > 0,
      Positive = Desired == Direction,
      Positive = factor(Positive, levels = c(FALSE, TRUE)),
      variable = factor(
        variable,
        levels = c(
          "DeprivationIndex",
          "Poverty",
          "Assistance",
          "MedianHHIncome",
          "Uninsured",
          "HighSchool",
          "Vacancy"
        )
      ),
      label = case_when(
        variable == "DeprivationIndex" ~ as.character(round(value, 3)),
        variable == "MedianHHIncome" ~ 
          paste0("$", format(round(value), big.mark = ",", trim = TRUE)),
        TRUE ~ paste0(as.character(round(value, 1)), "%")
      ),
      textloc = case_when(
        diff == max(diff) & diff > 0 ~ .9*diff,
        diff == min(diff) & diff < 0 ~ diff-(.05*diff),
        diff > 0 ~ 1.1*diff,
        TRUE ~ diff+(.05*diff)
      )
    )
  
  y <- ggplot(x, aes(x = variable, y = diff, fill = Positive)) +
    geom_bar(stat = "identity") +
    labs(x = NULL, y = NULL, title = unique(x$Label), fill = NULL) +
    scale_y_continuous(labels = NULL) +
    scale_x_discrete(
      labels = c(
        "Deprivation\nIndex",
        "Poverty\nRate",
        "On Public\nAssistance",
        "Median\nHousehold\nIncome",
        "No Health\nInsurance",
        "High School\nGraduation",
        "Housing\nVacancies"
      )
    ) +
    scale_fill_manual(
      breaks = c(FALSE, TRUE), 
      values = c("orange", "lightblue"),
      labels = c("Worse than average", "Better than average")
      ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = .5),
      legend.position = "bottom"
      ) + 
    geom_text(aes(label = label, y = textloc), size = 4) +
    annotate("segment", x = 0.5, xend = 7.5, y = 0, yend = 0)
  y
  return(y)
}

hood_di_popups <- lapply(hoods, function(k) {
  di_graph(i = k)
  })

muni_di_popups <- lapply(munis, function(k){
  di_graph(i = k)
  })

county_di_popups <- lapply(counties, function(k){
  di_graph(i = k)
  })

health_graph <- function(i) {
  x <- rates |>
    filter(
      variable %in% c("AsthmaRegistry", "AsthmaAdmission")) |>
    inner_join(areas[i,]) |>
    mutate(
      Positive = diff < 0,
      Positive = factor(Positive, levels = c(FALSE, TRUE)),
      variable = factor(
        variable, 
        levels = c("AsthmaRegistry", "AsthmaAdmission")
        ),
      textloc = case_when(
        diff == max(diff) & diff > 0 ~ .9*diff,
        diff == min(diff) & diff < 0 ~ diff-(.05*diff),
        diff > 0 ~ 1.1*diff,
        TRUE ~ diff+(.05*diff)
      )
    )
  
  y <- ggplot(x, aes(x = variable, y = diff, fill = Positive)) +
    geom_bar(stat = "identity") +
    labs(x = NULL, y = "Per 100 children", title = unique(x$Label), fill = NULL) +
    scale_y_continuous(labels = NULL) +
    scale_x_discrete(
      labels = c(
        "Asthma\n Registry",
        "Asthma\nAdmissions"
      )
    ) +
    scale_fill_manual(
      breaks = c(FALSE, TRUE), 
      values = c("orange", "lightblue"),
      labels = c("Worse than average", "Better than average")
      ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = .5),
      legend.position = "bottom"
      ) + 
    geom_text(aes(label = round(value*100, 1), y = textloc), size = 4) +
    annotate("segment", x = 0.5, xend = 2.5, y = 0, yend = 0)
  y
  return(y)
}

hood_health_popups <- lapply(hoods, function(k) {
  health_graph(i = k)
  })

muni_health_popups <- lapply(munis, function(k){
  health_graph(i = k)
  })

county_health_popups <- lapply(counties, function(k){
  health_graph(i = k)
  })

hood_lines <- block_groups(
  state = "OH",
  county = "Hamilton"
  ) |>
  inner_join(allocations, multiple = "all") |>
  filter(Municipality == "Cincinnati") |>
  group_by(Neighborhood) |>
  summarise(geometry = st_union(geometry)) |>
  rename(Area = Neighborhood) |>
  mutate(
    State = "Ohio",
    County = "Hamilton",
    Centroid = st_centroid(geometry)
  ) |>
  inner_join(select(all_data, Neighborhood, Tier) |> rename(Area = Neighborhood))

pal <- colorFactor("Blues", domain = all_data$Tier)
#previewColors(colorFactor("Blues", domain = NULL), values = unique(all_data$Tier))

citymap <- leaflet() |>
  addTiles() |>
  addPolygons(
    data = hood_lines,
    stroke = TRUE,
    weight = 1,
    smoothFactor = .5,
    opacity = 1,
    fillOpacity = .7,
    fillColor = ~pal(Tier)
    ) |>
  addLegendFactor(
    pal = pal,
    values = hood_lines$Tier,
    title = "Deprivation Index",
    position = "bottomleft"
  ) |>
  addLayersControl(
    baseGroups = c(
      "Demographics", 
      "Deprivation indicators",
      "Health indicators"
      ),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
    ) |>
  addCircleMarkers(
    data = hood_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(hood_demo_popups, height = 300, width = 700),
    group = "Demographics"
  ) |>
  addCircleMarkers(
    data = hood_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(hood_di_popups, height = 300, width = 700),
    group = "Deprivation indicators"
  ) |>
  addCircleMarkers(
    data = hood_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(hood_health_popups, height = 300, width = 700),
    group = "Health indicators"
  ) 

saveWidget(citymap, "city map.html")
  
oh_lines <- county_subdivisions(
  state = "OH",
  county = c("Hamilton", "Warren", "Clermont", "Butler")
) |>
  mutate(
    County = case_when(
      NAME == "Loveland" ~ "Hamilton",
      NAMELSAD == "Fairfield city" ~ "Butler",
      NAMELSAD == "Milford city" ~ "Clermont",
      COUNTYFP == "061"  ~ "Hamilton",
      COUNTYFP == "017" ~ "Butler",
      COUNTYFP == "025" ~ "Clermont",
      TRUE ~ "Warren"
    )
  ) |>
  group_by(County, COUSUBFP, NAMELSAD) |>
  summarise(geometry = st_union(geometry)) |>
  ungroup() |>
  mutate(
    Municipality = str_remove(NAMELSAD, " city"),
    Municipality = str_remove(Municipality, " village"),
    Municipality = str_remove(Municipality, "The Village of "),
    Municipality = str_to_title(Municipality)
  ) |>
  select(County, Municipality, geometry)

ky_lines <- county_subdivisions(
  state = "KY",
  county = c("Kenton", "Campbell", "Boone") 
) |>
  rename(Municipality = NAME) |>
  mutate(
    County = case_when(
      COUNTYFP == "037" ~ "Campbell",
      COUNTYFP == "117" ~ "Kenton",
      TRUE ~ "Boone"
    )
  ) |>
  select(County, Municipality, geometry)

in_lines <- county_subdivisions(
  state = "IN",
  county = "Dearborn"
) |>
  mutate(
    County = "Dearborn",
    Municipality = str_to_title(NAMELSAD)
  ) |>
  select(County, Municipality, geometry)

muni_lines <- rbind(oh_lines, ky_lines) |>
  rbind(in_lines) |>
  mutate(
    Centroid = st_centroid(geometry),
    Neighborhood = "All"
  ) |>
  inner_join(areas) |>
  arrange(geoid)

muni_map <- leaflet() |>
  addTiles() |>
  addPolygons(
    data = muni_lines,
    stroke = TRUE,
    weight = 1,
    smoothFactor = .5,
    opacity = 1,
    fillOpacity = .7,
    fillColor = ~pal(Tier)
  ) |>
  addLegendFactor(
    pal = pal,
    values = hood_lines$Tier,
    title = "Deprivation Index",
    position = "bottomleft"
  ) |>
  addLayersControl(
    baseGroups = c(
      "Demographics", 
      "Deprivation indicators",
      "Health indicators"
    ),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addCircleMarkers(
    data = muni_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(muni_demo_popups, height = 300, width = 700),
    group = "Demographics"
  ) |>
  addCircleMarkers(
    data = muni_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(muni_di_popups, height = 300, width = 700),
    group = "Deprivation indicators"
  ) |>
  addCircleMarkers(
    data = muni_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(muni_health_popups, height = 300, width = 700),
    group = "Health indicators"
  ) 

saveWidget(muni_map, "muni map.html")

county_lines <- counties(
  state = c("OH", "KY", "IN")
) |>
  filter(
    (STATEFP == "39" & NAME %in% c("Hamilton", "Butler", "Warren", "Clermont")) |
      (STATEFP == "21" & NAME %in% c("Kenton", "Boone", "Campbell")) |
      (STATEFP == "18" & NAME == "Dearborn")
    ) |>
  rename(County = NAME) |>
  select(County, geometry) |>
  mutate(Centroid = st_centroid(geometry)) |> 
  inner_join(filter(areas, Area == "All")) |>
  arrange(geoid)

county_map <- leaflet() |>
  addTiles() |>
  addPolygons(
    data = county_lines,
    stroke = TRUE,
    weight = 1,
    smoothFactor = .5,
    opacity = 1,
    fillOpacity = .7,
    fillColor = ~pal(Tier)
  ) |>
  addLegendFactor(
    pal = pal,
    values = county_lines$Tier,
    title = "Deprivation Index",
    position = "bottomleft"
  ) |>
  addLayersControl(
    baseGroups = c(
      "Demographics", 
      "Deprivation indicators",
      "Health indicators"
    ),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addCircleMarkers(
    data = county_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(county_demo_popups, height = 300, width = 700),
    group = "Demographics"
  ) |>
  addCircleMarkers(
    data = county_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(county_di_popups, height = 300, width = 700),
    group = "Deprivation indicators"
  ) |>
  addCircleMarkers(
    data = muni_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(county_health_popups, height = 300, width = 700),
    group = "Health indicators"
  ) 
saveWidget(county_map, "county map.html")
