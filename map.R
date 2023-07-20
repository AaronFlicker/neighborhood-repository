library(tidyverse)
library(tigris)
library(leaflet)
library(leafpop)
library(sf)
library(leaflegend)
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
      )
  )

hood_benchmarks <- all_data |>
  filter(
    Municipality == "Cincinnati",
    Neighborhood == "All"
  ) |>
  select(c(ends_with("Rate"), MedianHHIncome, DeprivationIndex)) |>
  pivot_longer(
    cols = WhiteRate:DeprivationIndex,
    names_to = "variable",
    values_to = "Cincinnati"
  )

hood_rates <- filter(all_data, Neighborhood != "All") |>
  select(c(ends_with("Rate"), MedianHHIncome, DeprivationIndex, Neighborhood)) |>
  pivot_longer(
    cols = WhiteRate:DeprivationIndex,
    names_to = "variable"
  ) |>
  left_join(hood_benchmarks) |>
  mutate(
    diff = 100*(value-Cincinnati)/Cincinnati,
    variable = str_remove(variable, "Rate")
  )

hoods <- unique(hood_rates$Neighborhood)

demo_graph <- function(hood) {
  x <- filter(
    hood_rates,
    variable %in% c(
      "White", 
      "Black", 
      "Hispanic", 
      "Child", 
      "ChildHH",
      "OtherLanguage",
      "LimitedEnglish"
      ),
    Neighborhood == hood
  ) |>
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
      )
    )
  
  y <- ggplot(x, aes(x = variable, y = diff)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    labs(x = NULL, y = "%", title = hood) +
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
      aes(label = paste0(round(value*100, 1), "%")), 
      vjust = -.5, 
      size = 3.5
      ) +
    annotate("segment", x = 0.5, xend = 7.5, y = 0, yend = 0)
  y
  return(y)
} 

demo_popups <- lapply(1:length(hoods), function(i) {
  demo_graph(hood = hoods[i])
  })

di_graph <- function(hood) {
  x <- filter(
    hood_rates,
    variable %in% c(
      "Uninsured", 
      "Assistance", 
      "HighSchool", 
      "Poverty", 
      "Vacancy", 
      "MedianHHIncome",
      "DeprivationIndex"
      ),
    Neighborhood == hood
    ) |>
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
      )
    )
  
  y <- ggplot(x, aes(x = variable, y = diff, fill = Positive)) +
    geom_bar(stat = "identity") +
    labs(x = NULL, y = NULL, title = hood, fill = NULL) +
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
    geom_text(aes(label = label), vjust = -.5, size = 3.5) +
    annotate("segment", x = 0.5, xend = 7.5, y = 0, yend = 0)
  y
  return(y)
}

di_popups <- lapply(1:length(hoods), function(i) {
  di_graph(hood = hoods[i])
})

health_graph <- function(hood) {
  x <- hood_rates |>
    filter(
      variable %in% c("AsthmaRegistry", "AsthmaAdmission"),
      Neighborhood == hood
      ) |>
    mutate(
      Positive = diff < 0,
      Positive = factor(Positive, levels = c(FALSE, TRUE)),
      variable = factor(variable, levels = c("AsthmaRegistry", "AsthmaAdmission"))
    )
  
  y <- ggplot(x, aes(x = variable, y = diff, fill = Positive)) +
    geom_bar(stat = "identity") +
    labs(x = NULL, y = "Per 100 children", title = hood, fill = NULL) +
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
    geom_text(
      aes(label = round(value*100, 1)), 
      vjust = -.5, 
      size = 3.5
    ) +
    annotate("segment", x = 0.5, xend = 2.5, y = 0, yend = 0)
  y
  return(y)
}

health_popups <- lapply(1:length(hoods), function(i) {
  health_graph(hood = hoods[i])
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

leaflet() |>
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
    options = layersControlOptions(collapsed = FALSE)) |>
  addCircleMarkers(
    data = hood_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(demo_popups, height = 300, width = 700),
    group = "Demographics"
  ) |>
  addCircleMarkers(
    data = hood_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(di_popups, height = 400, width = 400),
    group = "Deprivation indicators"
  ) |>
  addCircleMarkers(
    data = hood_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(health_popups, height = 400, width = 400),
    group = "Health indicators"
  ) 
  
  





oh_lines <- county_subdivisions(
  state = "OH",
  county = c("Hamilton", "Butler", "Warren", "Clermont")
  ) |>
  mutate(
    County = case_when(
      COUNTYFP == "017" ~ "Butler",
      COUNTYFP == "061" ~ "Hamilton",
      COUNTYFP == "025" ~ "Clermont",
      TRUE ~ "Warren"
      ),
    County = case_when(
      NAMELSAD == "Loveland city" ~ "Hamilton",
      NAMELSAD == "Fairfield city" ~ "Butler",
      NAMELSAD == "Milford city" ~ "Clermont",
      TRUE ~ County
      ),
    State = "Ohio"
    ) |>
  separate_wider_delim(
    NAMELSAD, delim = " city", names = "Area", too_many = "drop"
  ) |>
  separate_wider_delim(
    Area, delim = " village", names = "Area", too_many = "drop"
  ) |>
  mutate(
    Area = ifelse(Area == "The Village of Indian Hill", "Indian Hill", Area)
    ) |>
  group_by(State, County, Area) |>
  summarise(geometry = st_union(geometry))

ky_lines <- county_subdivisions(
  state = "KY",
  county = c("Boone", "Campbell", "Kenton")
  ) |>
  rename(Area = NAME) |>
  mutate(
    County = case_when(
      COUNTYFP == "015" ~ "Boone",
      COUNTYFP == "037" ~ "Campbell",
      TRUE ~ "Kenton"
      ),
    State = "Kentucky"
    ) |>
  select(State, County, Area, geometry)

in_lines <- county_subdivisions(
  state = "IN",
  county = "Dearborn"
  ) |>
  mutate(
    County = "Dearborn",
    State = "Indiana"
  ) |>
  rename(Area = NAMELSAD) |>
  select(State, County, Area, geometry)

county_lines <- counties(state = c("OH", "KY", "IN")) |>
  filter(
    (NAME %in% c("Hamilton", "Butler", "Clermont", "Warren") & STATEFP == "39") |
      NAME %in% c("Campbell", "Boone", "Kenton") & STATEFP == "21" |
      NAME == "Dearborn"
    ) |>
  mutate(
    State = case_when(
      STATEFP == "39" ~ "Ohio",
      STATEFP == "21" ~ "Kentucky",
      TRUE ~ "Indiana"
    )
  ) |>
  rename(Area = NAME) |>
  select(Area, State)
  
metro_data <- filter(all_data, County == "All")

