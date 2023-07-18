library(tidyverse)
library(tidycensus)

vars <- load_variables(2021, "acs5")
bg_vars <- filter(vars, geography == "block group")
tract_vars <- filter(vars, geography == "tract") |>
  left_join(bg_vars, join_by(label, concept))

bg_var_list <-c(
  paste0("B01001_00", c(1, 3:6)),
  paste0("B01001_0", 27:30),
  paste0("B02001_00", 2:3),
  "B03002_012",
  paste0("B11005_00", 1:2),
  "B15003_001",
  paste0("B15003_0", 17:25),
  paste0("B16004_00", c(1, 3, 7, 8)),
  paste0("B16004_0", c(12, 13, 17, 18, 22, 23, 25, 29, 30, 34, 35, 39, 
                       40, 44, 45, 47, 51, 52, 56, 57, 61, 62, 66, 67)),
  paste0("B17010_00", 1:2),
  "B19049_001",
  paste0("B25002_00", c(1, 3)),
  paste0("B27010_0", c(17, 33, 50, 66))
  )

#tract_var_list <- c("B19058_001", "B19058_002")

cat_frame <- data.frame(
  variable = sort(c(bg_var_list, "B19058_002")),
  category = c(
    "Population",
    rep("Children", 8),
    "White",
    "Black",
    "Hispanic",
    "Households",
    "HHWithChildren",
    "Age25",
    rep("HighSchool", 9),
    "Age5",
    "English",
    rep("LimitedEnglish", 8),
    "English",
    rep("LimitedEnglish", 8),
    "English",
    rep("LimitedEnglish", 8),
    "PovertyTotal",
    "Poverty",
    "MedianHHIncome",
    "Assistance",
    "HousingUnits",
    "Vacant",
    rep("Uninsured", 4)
    )
  )

## List of census tract deprivation indexes
di <- read.csv(
  "deprivation index all tracts 2021.csv",
  colClasses = c("numeric", "character", rep("numeric", 6))
  )

## Allocation of Hamilton County census block groups
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

## Allocation of 8 county census tracts
oki <- read.csv(
  "oki allocations.csv",
  col.names = c(
    "CountyCode",
    "Tract",
    NA,
    "County",
    "Municipality",
    NA,
    "Allocation"
  ),
  colClasses = c(
    "character", 
    "character", 
    "NULL", 
    "character", 
    "character", 
    "NULL", 
    "numeric"
    )
  ) |>
  filter(
    County %in% c(
    "Dearborn IN",
    "Boone KY",
    "Campbell KY",
    "Kenton KY",
    "Butler OH",
    "Clermont OH",
    "Hamilton OH",
    "Warren OH"
    )
    ) |>
  separate_wider_delim(
    Municipality, delim = "CCD", names = "Municipality", too_many = "drop"
    ) |>
  separate_wider_delim(
    Municipality, delim = " city", names = "Municipality", too_many = "drop"
  ) |>
  separate_wider_delim(
    Municipality, delim = ",", names = "Municipality", too_many = "drop"
  )|>
  separate_wider_delim(
    Municipality, delim = "village", names = "Municipality", too_many = "drop"
  ) |>
  separate_wider_delim(
    Tract, 
    delim = ".", 
    names = c("TractA", "TractB"),
    too_few = "align_start"
    ) |>
  mutate(
    Municipality = str_remove(Municipality, "The Village of"),
    Municipality = str_to_title(Municipality),
    Municipality = str_trim(Municipality),
    Tract = NA
    ) |>
  separate_wider_delim(County, delim = " ", names = c("County", "State")) |>
  mutate(
    State = case_when(
      State == "OH" ~ "Ohio",
      State == "KY" ~ "Kentucky",
      TRUE ~ "Indiana"
      ),
    TractB = coalesce(TractB, "")
    )

for (i in 1:nrow(oki)){
  if (str_length(oki$TractA[i]) < 4){
    prefix_length <- 4-str_length(oki$TractA[i])
    prefix <- str_flatten(rep("0", prefix_length))
  }else{
    prefix_length = 0
    prefix = ""
  }
  suffix_length <-
    6-prefix_length-str_length(oki$TractA[i])-str_length(oki$TractB[i])
  suffix <- str_flatten(rep("0", suffix_length))
  oki$Tract[i] <- paste0(prefix, oki$TractA[i], oki$TractB[i], suffix)
}
oki$GEOID <- paste0(oki$CountyCode, oki$Tract)

hamco_bg <- get_acs(
  geography = "block group",
  variables = bg_var_list,
  county = "Hamilton",
  state = "OH"
  ) |>
  inner_join(cat_frame) |>
  group_by(GEOID, category) |>
  summarise(estimate = sum(estimate)) |>
  pivot_wider(
    id_cols = GEOID,
    names_from = category,
    values_from = estimate
  ) |>
  mutate(Income = MedianHHIncome*Households) |>
  inner_join(allocations, multiple = "all") |>
  mutate(across(c(Age25:LimitedEnglish, Population:Income), \(x) x*Allocation))

hood_inc <- hamco_bg |>
  filter(
    Municipality == "Cincinnati",
    !is.na(Income)
    ) |>
  group_by(Neighborhood) |>
  summarise(
    Income = sum(Income),
    HH = sum(Households)
    ) |>
  mutate(MedianHHIncome = Income/HH)

cinci_hood <- filter(hamco_bg, Municipality == "Cincinnati") |>
  ungroup() |>
  select(Age25:LimitedEnglish, Population:White, Municipality, Neighborhood) |>
  group_by(Municipality, Neighborhood) |>
  summarise(across(Age25:White, sum)) |>
  left_join(hood_inc)

hh_tract <- filter(hamco_bg, Municipality == "Cincinnati") |>
  mutate(Tract = str_trunc(GEOID, 11, "right", ellipsis = "")) |>
  group_by(Tract, Neighborhood) |>
  summarise(Households = sum(Households)) |>
  group_by(Tract) |>
  mutate(
    TractHH = sum(Households),
    Allocation = Households/TractHH
    )

hood_assistance <- get_acs(
  geography = "tract",
  variables = "B19058_002",
  county = "Hamilton",
  state = "OH"
  ) |>
  inner_join(
    select(hh_tract, -Households), 
    by = c("GEOID" = "Tract"),
    multiple = "all"
    ) |>
  mutate(
    Assistance = estimate*Allocation,
    HH = TractHH*Allocation
    ) |>
  group_by(Neighborhood) |>
  summarise(
    Assistance = sum(Assistance),
    HH = sum(HH),
    AssistanceRate = Assistance/HH,
    Assistance = round(Assistance, 0)
  ) |>
  select(Neighborhood, Assistance, AssistanceRate)

cinci_hood <- inner_join(cinci_hood, hood_assistance) |>
  mutate(
    County = "Hamilton",
    State = "Ohio"
    ) |>
  select(-c(HH, Income))

oh_cs <- get_acs(
  geography = "county subdivision",
  variables = c(bg_var_list, "B19058_002"),
  state = "OH",
  county = c("Clermont", "Warren", "Butler", "Hamilton"),
  year = 2021
  )

ky_cs <- get_acs(
  geography = "county subdivision",
  variables = c(bg_var_list, "B19058_002"),
  state = "KY",
  county = c("Boone", "Campbell", "Kenton"),
  year = 2021
  )

in_cs <- get_acs(
  geography = "county subdivision",
  variables = c(bg_var_list, "B19058_002"),
  state = "IN",
  county = "Dearborn",
  year = 2021
  )

oki_cs <- rbind(oh_cs, ky_cs) |>
  rbind(in_cs) |>
  inner_join(cat_frame) |>
  separate_wider_delim(
    NAME, 
    delim = ", ", 
    names = c("Municipality", "County", "State")
    ) |>
  mutate(
    Municipality = str_remove(Municipality, " city"),
    Municipality = str_remove(Municipality, " village"),
    Municipality = str_remove(Municipality, " CCD"),
    Municipality = str_remove(Municipality, "The Village of "),
    Municipality = str_to_title(Municipality),
    County = str_remove(County, " County"),
    County = case_when(
      Municipality == "Loveland" ~ "Hamilton",
      Municipality == "Milford" ~ "Clermont",
      TRUE ~ County
    )
  ) |>
  filter(
    !is.na(estimate),
    estimate > 0
    )

cs_income <- filter(oki_cs, category %in% c("MedianHHIncome", "Households")) |>
  distinct(GEOID, Municipality, County, State, estimate, category) |>
  pivot_wider(
    id_cols = c(GEOID, County, State, Municipality),
    names_from = category,
    values_from = estimate
  ) |>
  mutate(Income = MedianHHIncome*Households) |>
  group_by(Municipality, County, State) |>
  summarise(
    Income = sum(Income),
    Households = sum(Households)
    ) |>
  mutate(MedianHHIncome = Income/Households) |>
  filter(!is.na(MedianHHIncome)) |>
  select(-Income)
  
cs_demo <- filter(oki_cs, category != "MedianHHIncome") |>
  distinct(GEOID, Municipality, County, State, estimate, category) |>
  group_by(Municipality, County, State, category) |>
  summarise(estimate = sum(estimate)) |>
  pivot_wider(
    id_cols = c(Municipality, County, State),
    names_from = category,
    values_from = estimate,
    values_fill = 0
  ) |>
  left_join(cs_income) |>
  mutate(AssistanceRate = Assistance/Households,
         Neighborhood = NA) |>
  rbind(cinci_hood) |>
  mutate(
    HighSchoolRate = HighSchool/Age25,
    OtherLanguage = Age5-English,
    OtherLanguageRate = OtherLanguage/Age5,
    BlackRate = Black/Population,
    ChildRate = Children/Population,
    ChildHHRate = HHWithChildren/Households,
    HispanicRate = Hispanic/Population,
    VacancyRate = Vacant/HousingUnits,
    LimitedEnglishRate = LimitedEnglish/Population,
    PovertyRate = Poverty/PovertyTotal,
    UninsuredRate = Uninsured/Population,
    WhiteRate = White/Population,
    across(Age25:MedianHHIncome, round)
    ) |>
  select(State, County, Municipality, Neighborhood, Population, 
         Black, BlackRate, Hispanic, HispanicRate, White, WhiteRate,
         Children, ChildRate, Uninsured, UninsuredRate, 
         Households, HHWithChildren, ChildHHRate, 
         Assistance, AssistanceRate, MedianHHIncome,
         Age5, OtherLanguage, OtherLanguageRate, 
         LimitedEnglish, LimitedEnglishRate,
         Age25, HighSchool, HighSchoolRate,
         PovertyTotal, Poverty, PovertyRate,
         HousingUnits, Vacant, VacancyRate
         )

oh_tract <- get_acs(
  geography = "tract",
  variables = "B01001_001",
  state = "OH",
  county = c("Clermont", "Warren", "Butler", "Hamilton"),
  year = 2021
  )

ky_tract <- get_acs(
  geography = "tract",
  variables = "B01001_001",
  state = "KY",
  county = c("Kenton", "Campbell", "Boone"),
  year = 2021
  )

in_tract <- get_acs(
  geography = "tract",
  variables = "B01001_001",
  state = "IN",
  county = c("Dearborn"),
  year = 2021
  )

cs_di <- rbind(oh_tract, ky_tract) |>
  rbind(in_tract) |>
  full_join(select(oki, County:Allocation, GEOID), multiple = "all") |>
  mutate(Population = estimate*Allocation) |>
  inner_join(
    select(di, census_tract_fips, dep_index), 
    by = c("GEOID" = "census_tract_fips")
    ) |>
  mutate(
    County = case_when(
      Municipality == "Loveland" ~ "Hamilton",
      Municipality == "Milford" ~ "Clermont",
      TRUE ~ County
    )
  ) |>
  group_by(Municipality, County, State) |>
  mutate(
    TownPop = sum(Population),
    TractShare = Population/TownPop,
    DIWeight = TractShare*dep_index
    ) |>
  summarise(DeprivationIndex = sum(DIWeight))

hood_di <- filter(hamco_bg, Municipality == "Cincinnati") |>
  mutate(census_tract_fips = str_trunc(GEOID, 11, "right", ellipsis = "")) |>
  group_by(census_tract_fips, Neighborhood, Municipality) |>
  summarise(Population = sum(Population)) |>
  group_by(Municipality, Neighborhood) |>
  mutate(
    HoodPop = sum(Population),
    TractWeight = Population/HoodPop
    ) |>
  inner_join(di) |>
  mutate(DIWeight = TractWeight*dep_index) |>
  summarise(DeprivationIndex = sum(DIWeight)) |>
  mutate(
    County = "Hamilton",
    State = "Ohio"
    ) |>
  rbind(cs_di)

all <- full_join(cs_demo, hood_di)

write_csv(all, "census_data.csv")


