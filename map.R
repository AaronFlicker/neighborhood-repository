library(tidyverse)
library(tigris)
library(leaflet)
library(sf)
library(geojsonio)
library(geojsonsf)
library(geojsonlint)
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
    AsthmaAdmissionRate = AsthmaAdmissions/Children
    )

hood_benchmarks <- all_data |>
  filter(
    Municipality == "Cincinnati",
    Neighborhood == "All"
  ) |>
  select(c(ends_with("Rate"), Municipality)) |>
  rename(Area = Municipality)

hoods <- all_data$Neighborhood[all_data$Neighborhood != "All"]

hood_data <- filter(all_data, Neighborhood == hoods[i]) |>
  select(c(ends_with("Rate"), Neighborhood)) |>
  rename(Area = Neighborhood) |>
  rbind(hood_benchmarks) |>
  pivot_longer(
    WhiteRate:AsthmaAdmissionRate,
    names_to = "variable"
  ) |>
  mutate(
    variable = str_remove(variable, "Rate"),
    variable = factor(variable, levels = c(
      "AsthmaAdmission", "AsthmaRegistry",
      "LimitedEnglish", "OtherLanguage", 
      "Vacancy", "Uninsured", "HighSchool", "Assistance", "Poverty",
      "Child", "Hispanic", "White", "Black"
      )
     ),
    Area = factor(Area, levels = c("Cincinnati", hoods[i]))
    )

ggplot(hood_data, aes(x = variable, y = value, fill = Area)) +
  geom_bar(stat = "identity", position = position_dodge(.9)) +
  coord_flip() +
  labs(x = NULL, y = "%", title = hoods[i], fill = NULL) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, .1),
    labels = seq(0, 100, 10)
  ) +
  scale_fill_manual(
    values = c("darkblue", "lightblue"),
    breaks = c(hoods[i], "Cincinnati")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = .5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_text(aes(label = round(value*100, 1)), hjust = -.5, size = 3.5, position = position_dodge(.9))



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
    County = "Hamilton"
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

