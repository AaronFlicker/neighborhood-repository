library(tidyverse)
library(tigris)
library(leaflet)
library(sf)
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
census <- read.csv("census_data.csv")
asthma <- read.csv("asthma.csv")
all_data <- inner_join(census, asthma) |>
  mutate(Area = coalesce(Neighborhood, Municipality)) |>
  select(-Municipality)
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

oki_lines <- rbind(oh_lines, ky_lines) |>
  rbind(in_lines) |>
  mutate(Area = str_to_title(Area)) |>
  rbind(hood_lines) |>
  full_join(all_data) |>
  arrange(Area)
write_csv(oki_lines, "for map.csv")
