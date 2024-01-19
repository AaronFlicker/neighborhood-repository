library(cincy)
library(cowplot)
library(DBI)
library(glue)
library(htmlwidgets)
library(leaflegend)
library(leaflet)
library(leafpop)
library(lubridate)
library(odbc)
library(readxl)
library(sf)
library(stringi)
library(tidycensus)
library(tidygeocoder)
library(tidyverse)
library(tigris)
library(sf)
options(tigris_use_cache = TRUE)

source("~/cchmc_colors.R")

bg_var_list <-c(
  paste0("B01001_00", 1:9),
  paste0("B01001_0", 10:49),
  paste0("B02001_00", 2:9),
  "B02001_010",
  "B03002_012",
  paste0("B07201_00", c(1, 2, 4)),
  "B07201_014",
  paste0("B11012_00", 1:9),
  paste0("B11012_0", 10:17),
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

bg_vars <- load_variables("acs5", year = 2022) |>
  filter(name %in% bg_var_list) 

oh_muni <- get_acs(
  geography = "county subdivision",
  variables = c(bg_var_list, "B19058_002"),
  county = c("Hamilton", "Clermont", "Butler", "Warren"),
  state = "OH",
  year = 2022,
  geometry = TRUE
) 

ky_muni <- get_acs(
  geography = "county subdivision",
  variables = c(bg_var_list, "B19058_002"),
  county = c("Boone", "Campbell", "Kenton"),
  state = "KY",
  year = 2022,
  geometry = TRUE
) 

in_muni <- get_acs(
  geography = "county subdivision",
  variables = c(bg_var_list, "B19058_002"),
  county = "Dearborn",
  state = "IN",
  year = 2022,
  geometry = TRUE
) 

oki_muni <- rbind(oh_muni, ky_muni) |>
  rbind(in_muni) |>
  separate_wider_delim(
    NAME,
    names = c("Municipality", "County", "State"),
    delim = ", "
  ) |>
  mutate(
    Municipality = str_remove(Municipality, " city"),
    Municipality = str_remove(Municipality, " CCD"),
    Municipality = str_remove(Municipality, " village"),
    Municipality = str_remove(Municipality, "The Village of "),
    Municipality = str_to_title(Municipality),
    County = str_remove(County, " County")
  )|>
  select(-State)

oki_muni2 <- as_tibble(oki_muni) |>
  filter(variable != "B19049_001") |> 
  mutate(County = case_when(
    Municipality == "Loveland" ~ "Hamilton",
    Municipality == "Milford" ~ "Clermont",
    Municipality == "Fairfield" ~ "Butler",
    TRUE ~ County
    )
  ) |> 
  select(Municipality:estimate) |>
  left_join(bg_vars, by = c("variable" = "name")) |>
  group_by(Municipality, County, variable, label, concept) |>
  summarise(estimate = sum(estimate)) |>
  mutate(Neighborhood = Municipality) |>
  ungroup()

allocations <- read.csv(
  "neighborhood bg allocations.csv",
  colClasses = c(
    "character", 
    rep("NULL", 3), 
    "character", 
    "numeric", 
    "character"
  )
) |>
  mutate(
    Neighborhood = ifelse(
      Neighborhood %in% c("North Avondale", "Paddock Hills"),
      "North Avondale-Paddock Hills",
      Neighborhood
    )
  )

hamco_bg <- get_acs(
  geography = "block group",
  variables = bg_var_list,
  county = "Hamilton",
  state = "OH",
  year = 2022
) |>
  inner_join(bg_vars, by = c("variable" = "name"))

hamco_bg2 <- filter(hamco_bg, variable != "B19049_001") |>
  inner_join(allocations, multiple = "all") |>
  mutate(estimate = estimate*Allocation) |>
  filter(!is.na(estimate))

cinci_bg <- filter(hamco_bg2, Municipality == "Cincinnati")

cinci_hood <- cinci_bg |>
  as_tibble() |>
  group_by(Municipality, Neighborhood, variable, label, concept) |>
  summarise(estimate = sum(estimate)) |>
  mutate(County = "Hamilton")

hood_muni <- rbind(cinci_hood, oki_muni2) |>
  separate_wider_delim(
    label,
    names = paste0("Group", 1:5),
    delim = ":!!",
    too_few = "align_start"
  ) |>
  mutate(Neighborhood = coalesce(Neighborhood, Municipality))

areas <- hood_muni |>
  ungroup() |>
  distinct(County, Municipality, Neighborhood) |>
  arrange(str_to_title(Neighborhood), County) |>
  mutate(HoodID = row_number())

pops <- hood_muni |> 
  filter(
    str_ends(variable, "_001"),
    str_detect(concept, "Income", negate = TRUE)
  ) |>
  mutate(
    Measure = case_when(
      str_starts(concept, "Sex") ~ "Population",
      str_starts(concept, "Geographical") ~ "Mobility",
      str_starts(concept, "Households") ~ "HH",
      str_starts(concept, "Educational") ~ "Over25",
      str_starts(concept, "Age") ~ "Over5",
      str_starts(concept, "Poverty") ~ "Poverty",
      TRUE ~ "HousingUnits"
    )
  ) |>
  pivot_wider(
    id_cols = c(County, Municipality, Neighborhood),
    names_from = Measure,
    names_prefix = "Pop",
    values_from = estimate
  )

race <- filter(hood_muni, str_detect(concept, "Race")) |>
  mutate(
    Race = case_when(
      str_detect(Group2, "White") ~ "White",
      str_detect(Group2, "Black") ~ "Black",
      str_detect(Group2, "Hispanic") ~ "Hispanic",
      TRUE ~ "Other"
    )
  ) |>
  group_by(County, Municipality, Neighborhood, Race) |>
  summarise(Total = sum(estimate)) |>
  filter(Race != "Other") |>
  pivot_wider(
    id_cols = c(County, Municipality, Neighborhood),
    names_from = Race,
    names_prefix = "Total",
    values_from = Total
  ) |>
  inner_join(pops) |>
  mutate(
    RateBlack = TotalBlack/PopPopulation,
    RateWhite = TotalWhite/PopPopulation,
    RateHispanic = TotalHispanic/PopPopulation
  ) |>
  select(County: PopPopulation, RateBlack:RateHispanic)

lang <- filter(hood_muni, str_detect(concept, "Language")) |>
  mutate(
    Language = case_when(
      is.na(Group2) ~ "Total",
      str_detect(Group3, "English") ~ "English",
      TRUE ~ "NotWell"
    )
  ) |>
  filter(Language != "Total") |>
  group_by(Neighborhood, Municipality, County, Language) |>
  summarise(estimate = sum(estimate)) |>
  pivot_wider(
    id_cols = c(Neighborhood, Municipality, County),
    names_from = Language,
    names_prefix = "Total",
    values_from = estimate
  ) |>
  inner_join(pops) |>
  mutate(
    TotalOtherThanEnglish = PopOver5 - TotalEnglish,
    RateOtherThanEnglish = TotalOtherThanEnglish/PopOver5,
    RateNotWell = TotalNotWell/PopOver5
  ) |>
  select(
    Neighborhood:County, 
    TotalNotWell, 
    PopOver5, 
    TotalOtherThanEnglish:RateNotWell
  )

benchmarks <- function(k){
  x <- filter(k, Municipality == Neighborhood) |>
    ungroup()
  baseline <- select(x, starts_with("Pop")) 
  baseline <- sum(baseline[1])
  means <- select(x, starts_with("Total")) |>
    summarise(across(everything(), sum)) |>
    mutate(across(everything(), \(x) x/baseline)) |>
    pivot_longer(cols = everything()) |>
    mutate(
      name = str_remove(name, "Total"),
      Measure = "Mean"
    )
  
  mins <- ungroup(k) |>
    select(starts_with("Rate")) |>
    summarise(across(everything(), min)) |>
    pivot_longer(cols = everything()) |>
    mutate(
      name = str_remove(name, "Rate"),
      Measure = "Min"
    )
  
  maxes <- ungroup(k) |>
    select(starts_with("Rate")) |>
    summarise(across(everything(), max)) |>
    pivot_longer(cols = everything()) |>
    mutate(
      name = str_remove(name, "Rate"),
      Measure = "Max"
    ) 
  
  y <- rbind(means, mins) |>
    rbind(maxes) |>
    pivot_wider(
      id_cols = name,
      names_from = Measure,
      values_from = value
    )
  
  z <- select(k, c(County:Neighborhood, starts_with("Rate"))) |>
    pivot_longer(
      cols = starts_with("Rate"),
    ) |>
    mutate(name = str_remove(name, "Rate")) |>
    inner_join(y) |>
    mutate(
      Shade = ifelse(
        value > Mean, 
        (value-Mean)/(Max-Mean), 
        (value-Mean)/(Mean-Min)
      ),
      Shade = ifelse(is.infinite(Shade), NA, Shade),
      Textloc = ifelse(value > .9, value - .025, value + .04)
    )
}

race_rates <- benchmarks(race)

lang_rates <- benchmarks(lang)

race_lang <- rbind(race_rates, lang_rates) |>
  mutate(Neighborhood = coalesce(Neighborhood, Municipality)) |>
  inner_join(areas)

demo_graph <- function(k) {
  x <- filter(race_lang, HoodID == k) |>
    mutate(
      name = factor(
        name, 
        levels = c("White", "Black", "Hispanic",  "OtherThanEnglish", "NotWell")
      )
    )
  
  ggplot(x, aes(x = name, y = value, fill = Shade)) +
    geom_bar(stat = "identity", color = "black") +
    labs(x = NULL, y = "%", title = unique(x$Neighborhood), fill = NULL) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, .2),
      labels = seq(0, 100, 20)
    ) +
    scale_x_discrete(
      labels = c(
        "White",
        "Black",
        "Hispanic",
        "Language other\nthan English",
        "Limited\nEnglish"
      )
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = .5),
      legend.position = c(.9, .8)
    ) + 
    geom_text(
      aes(label = paste0(round(value*100, 1), "%"), y = Textloc), 
      size = 4
    ) +
    scale_fill_gradient2(
      limits = c(-1, 1), 
      high = cchmcdarkpurple, 
      low = cchmcdarkgreen, 
      mid = cchmclightblue,
      breaks = waiver(),
      n.breaks = 3,
      labels = c("Lowest", "Average", "Highest")
    )
}

muninums <- areas$HoodID[areas$Neighborhood == areas$Municipality]

hoodnums <- areas$HoodID[areas$Municipality%in% c(
  "Cincinnati", 
  "Norwood", 
  "St. Bernard", 
  "Elmwood Place"
) & areas$Neighborhood != "Cincinnati"]

demo_popup_muni <- lapply((muninums), function(k) {
  demo_graph(k)
})

demo_popup_hood <- lapply((hoodnums), function(k) {
  demo_graph(k)
})

di <- read.csv(
  "deprivation index all tracts 2022.csv",
  colClasses = c("numeric", "character", rep("NULL", 6))
)

hood_di <- filter(cinci_bg, variable == "B01001_001") |>
  as_tibble() |>
  mutate(census_tract_fips = str_trunc(GEOID, 11, "right", ellipsis = "")) |>
  group_by(census_tract_fips, Neighborhood, Municipality) |>
  summarise(Population = sum(estimate)) |>
  group_by(Municipality, Neighborhood) |>
  mutate(
    HoodPop = sum(Population),
    TractWeight = Population/HoodPop
  ) |>
  inner_join(di) |>
  mutate(DIWeight = TractWeight*dep_index) |>
  summarise(DeprivationIndex = sum(DIWeight)) |>
  mutate(County = "Hamilton")

oh_tract <- get_acs(
  geography = "tract",
  variables = "B01001_001",
  state = "OH",
  county = c("Clermont", "Warren", "Butler", "Hamilton"),
  year = 2022
)

ky_tract <- get_acs(
  geography = "tract",
  variables = "B01001_001",
  state = "KY",
  county = c("Kenton", "Campbell", "Boone"),
  year = 2022
)

in_tract <- get_acs(
  geography = "tract",
  variables = "B01001_001",
  state = "IN",
  county = "Dearborn",
  year = 2022
)

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

dep_index <- rbind(oh_tract, ky_tract) |>
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
  group_by(Municipality, County) |>
  mutate(
    TownPop = sum(Population),
    TractShare = Population/TownPop,
    DIWeight = TractShare*dep_index
  ) |>
  summarise(DeprivationIndex = sum(DIWeight)) |>
  mutate(Neighborhood = Municipality) |>
  rbind(hood_di) |>
  rename(value = DeprivationIndex) |>
  mutate(name = "DI")

cinci_hh <- filter(hamco_bg2, variable == "B11012_001") |>
  mutate(TractID = str_trunc(GEOID, 11, "right", ellipsis = "")) |>
  group_by(TractID) |>
  mutate(TractHH = sum(estimate)) |>
  group_by(Neighborhood, Municipality, TractID) |>
  mutate(HoodTractHH = sum(estimate)) |>
  distinct(TractID, Neighborhood, TractHH, HoodTractHH) |>
  mutate(Allocation = HoodTractHH/TractHH) |>
  select(Allocation) |>
  ungroup() |>
  filter(Municipality == "Cincinnati")

hood_assistance <- get_acs(
  geography = "tract",
  variables = c("B19058_002", "B11012_001"),
  county = "Hamilton",
  state = "OH"
) |>
  mutate(
    Category = ifelse(variable == "B11012_001", "PopHH", "TotalAssistance")
  ) |>
  pivot_wider(
    id_cols = GEOID,
    names_from = Category,
    values_from = estimate
  ) |>
  inner_join(cinci_hh, by = c("GEOID" = "TractID"), multiple = "all") |>
  mutate(across(PopHH:TotalAssistance, \(x) x*Allocation)) |>
  group_by(Municipality, Neighborhood) |>
  summarise(across(PopHH:TotalAssistance, sum)) |>
  mutate(County = "Hamilton")

assistance <- filter(hood_muni, variable %in% c("B19058_002", "B11012_001")) |>
  filter(Neighborhood == Municipality) |>
  mutate(
    Category = ifelse(variable == "B11012_001", "PopHH", "TotalAssistance")
  ) |>
  pivot_wider(
    id_cols = c(County, Municipality, Neighborhood),
    names_from = Category,
    values_from = estimate
  ) |>
  rbind(hood_assistance) |>
  mutate(
    value = TotalAssistance/PopHH,
    name = "Assistance"
  ) |>
  select(County:Neighborhood, value, name)

hood_income <- filter(hamco_bg, variable %in% c("B11012_001", "B19049_001")) |>
  as_tibble() |>
  mutate(Category = ifelse(variable == "B19049_001", "MedianIncome", "HH")) |>
  inner_join(allocations, multiple = "all") |>
  mutate(estimate = ifelse(Category == "HH", estimate*Allocation, estimate)) |>
  pivot_wider(
    id_cols = c(Municipality, Neighborhood, GEOID),
    names_from = Category,
    values_from = estimate
  ) |>
  filter(
    Municipality == "Cincinnati",
    !is.na(MedianIncome)
  ) |>
  mutate(Income = MedianIncome*HH) |>
  group_by(Municipality, Neighborhood) |>
  summarise(
    Income = sum(Income),
    HH = sum(HH)
  ) |>
  mutate(
    value = Income/HH,
    County = "Hamilton"
  ) |>
  select(County, Neighborhood, value)

oki_income <- oki_muni |>
  filter(variable %in% c("B11012_001", "B19049_001")) |>
  mutate(Category = ifelse(variable == "B19049_001", "MedianIncome", "HH")) |>
  pivot_wider(
    id_cols = c(GEOID, Municipality, County),
    names_from = Category,
    values_from = estimate
  ) |>
  filter(!is.na(MedianIncome)) |>
  mutate(
    Income = MedianIncome*HH,
    County = case_when(
      Municipality == "Loveland" ~ "Hamilton",
      Municipality == "Fairfield" ~ "Butler",
      Municipality == "Milford" ~ "Clermont",
      TRUE ~ County
    )
  ) |>
  group_by(Municipality, County) |>
  summarise(
    HH = sum(HH),
    Income = sum(Income)
  ) |>
  mutate(
    value = Income/HH,
    Neighborhood = Municipality
  ) |>
  select(-c(HH, Income)) |>
  rbind(hood_income) |>
  mutate(name = "MedianIncome")

di_meas <- hood_muni |>
  filter(variable %in% c(paste0("B15003_0", 17:25), "B17010_002", "B25002_003") |
           str_detect(variable, "B27010")) |>
  mutate(
    Category = case_when(
      str_starts(concept, "Educational") ~ "NoHighSchool",
      str_starts(concept, "Poverty") ~ "Poverty",
      str_starts(concept, "Occupancy") ~ "Vacant",
      TRUE ~ "Uninsured"
    )
  ) |>
  group_by(Municipality, County, Neighborhood, Category) |>
  summarise(estimate = sum(estimate)) |>
  pivot_wider(
    id_cols = c(Municipality, County, Neighborhood),
    names_from = Category,
    names_prefix = "Total",
    values_from = estimate
  ) |>
  inner_join(pops)

pov <- select(di_meas, ends_with("Poverty")) |>
  mutate(
    value = TotalPoverty/PopPoverty,
    name = "Poverty"
  ) |>
  select(-c(TotalPoverty, PopPoverty))

ins <- select(di_meas, TotalUninsured, PopPopulation) |>
  mutate(
    value = TotalUninsured/PopPopulation,
    name = "Uninsured"
  ) |>
  select(value, name)

hs <- select(di_meas, Municipality:TotalNoHighSchool, PopOver25) |>
  mutate(
    value = TotalNoHighSchool/PopOver25,
    name = "NoHS"
  ) |>
  select(Municipality:Neighborhood, name, value)

vac <- di_meas |>
  mutate(
    value = TotalVacant/PopHousingUnits,
    name = "Vacancy"
  ) |>
  select(Municipality:Neighborhood, name, value)

digraph_rates <- function(k, var){
  z <- filter(hood_muni, variable == var) |>
    inner_join(k) |>
    filter(Neighborhood == Municipality) |>
    mutate(Weight = value*estimate) |>
    ungroup() |>
    summarise(
      Weight = sum(Weight),
      Pop = sum(estimate)
    ) |>
    mutate(
      Mean = Weight/Pop,
      Min = min(k$value),
      Max = max(k$value),
      SD = sd(k$value)
    ) |>
    select(Mean:SD) |>
    cross_join(k) |>
    mutate(
      ZScore = (value-Mean)/SD,
      Scale = ifelse(
        ZScore > 0, 
        ((ZScore/max(ZScore))+1)/2, 
        (-ZScore/min(ZScore)+1)/2
      ),
      Textloc = ifelse(Scale > .9, Scale - .025, Scale + .04)
    )
}

di_rates <- digraph_rates(dep_index, "B01001_001") |>
  mutate(Shade = -(Scale-1))

income_rates <- digraph_rates(oki_income, "B11012_001") |>
  mutate(Shade = Scale)

pov_rates <- digraph_rates(pov, "B17010_001") |>
  mutate(Shade = -(Scale-1))

ins_rates <- digraph_rates(ins, "B01001_001") |>
  mutate(Shade = -(Scale-1))

assist_rates <- digraph_rates(assistance, "B11012_001") |>
  mutate(Shade = -(Scale-1))

hs_rates <- digraph_rates(hs, "B15003_001") |>
  mutate(Shade = Scale)

vac_rates <- digraph_rates(vac, "B25002_001") |>
  mutate(Shade = -(Scale-1))

di_all <- rbind(di_rates, pov_rates) |>
  rbind(assist_rates) |>
  rbind(income_rates) |>
  rbind(ins_rates) |>
  rbind(hs_rates) |>
  rbind(vac_rates) |>
  inner_join(areas)

di_graph <- function(k){
  x <- filter(di_all, HoodID == k) 
  if (nrow(x) < 7){
    x <- rbind(
      x, 
      c(
        as.numeric(rep(NA, 4)), 
        as.character(x[1, 5:6]),
        NA,
        as.character(x[1, 8]),
        "MedianIncome",
        NA,
        0,
        .04,
        0,
        mean(x$HoodID)
      )
    )
    x <- x |>
      mutate(across(c(Mean:SD, value, ZScore:Shade), as.numeric))
  } 
  x <- x |>
    mutate(
      name = factor(
        name,
        levels = c(
          "DI",
          "Poverty",
          "Assistance",
          "MedianIncome",
          "Uninsured",
          "NoHS",
          "Vacancy"
        )
      ),
      Label = case_when(
        is.na(value) ~ "NA",
        name == "DI" ~ as.character(round(value, 3)),
        name == "MedianIncome" ~ 
          paste0("$", format(as.integer(value), big.mark = ",")),
        TRUE ~ paste0(round(value*100, 1), "%")
      )
    )
  
  ggplot(x, aes(x = name, y = Scale, fill = Shade)) +
    geom_bar(stat = "identity") +
    labs(x = NULL, y = "%", title = unique(x$Neighborhood), fill = NULL) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, .5, 1),
      labels = c("Lowest", "Average", "Highest")
    ) +
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
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = .5),
      legend.position = "bottom"
    ) + 
    geom_text(aes(label = Label, y = Textloc), size = 4) +
    scale_fill_gradient2(
      limits = c(0, 1), 
      high = cchmcdarkgreen, 
      low = cchmcdarkpurple,
      mid = cchmclightblue,
      midpoint = .5,
      breaks = waiver(),
      n.breaks = 3,
      labels = c("Worst", "Average", "Best")
    )
}

di_popup_muni <- lapply((muninums), function(k) {
  di_graph(k)
})

di_popup_hood <- lapply((hoodnums), function(k) {
  di_graph(k)
})

pyramid <- filter(hood_muni, concept == "Sex by Age") |>
  ungroup() |>
  filter(!is.na(Group3)) |>
  rename(Gender = Group2) |>
  mutate(
    Age = str_remove(Group3, " years"),
    Age = case_when(
      Age %in% c("15 to 17", "18 and 19") ~ "15 to 19",
      Age %in% c("20", "21", "22 to 24") ~ "20 to 24",
      Age %in% c("60 and 61", "62 to 64") ~ "60 to 64",
      Age %in% c("65 and 66", "67 to 69") ~ "65 to 69",
      TRUE ~ Age
    ),
    Age = factor(
      Age, 
      levels = c(
        "Under 5",
        "5 to 9",
        "10 to 14",
        "15 to 19",
        "20 to 24",
        "25 to 29",
        "30 to 34",
        "35 to 39",
        "40 to 44",
        "45 to 49",
        "50 to 54",
        "55 to 59",
        "60 to 64",
        "65 to 69",
        "70 to 74",
        "75 to 79",
        "80 to 84",
        "85 and over"
      )
    )
  ) |>
  group_by(County, Municipality, Neighborhood, Gender, Age) |>
  summarise(Population = sum(estimate)) |>
  inner_join(areas) |>
  group_by(County, Municipality, Neighborhood) |>
  mutate(
    Total = sum(Population),
    Share = round(100*Population/Total, 1)
  ) 

pop_pyr <- function(k){
  x <- filter(pyramid, HoodID == k) |>
    mutate(Neighborhood = coalesce(Neighborhood, Municipality))
  
  ggplot(
    x, 
    aes(
      x = Age, 
      y = ifelse(Gender == "Male", -Share, Share), 
      fill = Gender
    )
  ) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      x = NULL, 
      y = "% of population", 
      fill = NULL, 
      title = x$Neighborhood[1]
    ) +
    scale_y_continuous(
      limits = c(-30, 30), 
      breaks = seq(-25, 25, 5),
      labels = c(25, 20, 15, 10, 5, seq(0, 25, 5))
    ) +
    scale_fill_manual(values = c(cchmcpink, cchmclightblue)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5))
}

pyr_popup_muni <- lapply((muninums), function(k) {
  pop_pyr(k)
})

pyr_popup_hood <- lapply((hoodnums), function(k) {
  pop_pyr(k)
})

hh <- filter(
  hood_muni, 
  variable %in% c(
    paste0("B11012_00", c(3, 4, 6, 7, 9)), 
    paste0("B11012_0", c(10:12, 14:17))
  )
) |>
  mutate(
    HHType = case_when(
      str_detect(Group2, "couple") ~ 
        ifelse(str_starts(Group3, "With children"), "TwoParent", "Couple"),
      TRUE ~ 
        ifelse(str_starts(Group3, "With children"), "SingleParent", "Single")
    )
  ) |>
  group_by(County, Municipality, Neighborhood, HHType) |>
  summarise(HH = sum(estimate)) |>
  ungroup() |>
  pivot_wider(
    id_cols = c(County, Municipality, Neighborhood),
    names_from = HHType,
    values_from = HH,
    names_prefix = "Total"
  ) |>
  inner_join(pops) |>
  mutate(
    RateCouple = TotalCouple/PopHH,
    RateSingle = TotalSingle/PopHH,
    RateSingleParent = TotalSingleParent/PopHH,
    RateTwoParent = TotalTwoParent/PopHH
  ) |>
  select(County:TotalTwoParent, PopHH, RateCouple:RateTwoParent)

hh_rates <- benchmarks(hh) |>
  inner_join(areas)

hh_graph <- function(k) {
  x <- filter(hh_rates, HoodID == k) |>
    mutate(
      name = factor(
        name, 
        levels = c("TwoParent", "SingleParent", "Couple", "Single")
      )
    )
  
  ggplot(x, aes(x = name, y = value, fill = Shade)) +
    geom_bar(stat = "identity", color = "black") +
    labs(x = NULL, y = "%", title = unique(x$Neighborhood), fill = NULL) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, .2),
      labels = seq(0, 100, 20)
    ) +
    scale_x_discrete(
      labels = c(
        "Children with\ntwo adults",
        "Children with\none adult",
        "Adults,\nno children",
        "Single adult"
      )
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5)) + 
    geom_text(
      aes(label = paste0(round(value*100, 1), "%"), y = Textloc), 
      size = 4
    ) +
    scale_fill_gradient2(
      limits = c(-1, 1), 
      high = cchmcdarkpurple, 
      low = cchmcdarkgreen, 
      mid = cchmclightblue,
      breaks = waiver(),
      n.breaks = 3,
      labels = c("Lowest", "Average", "Highest")
    )
} 

hh_popup_muni <- lapply((muninums), function(k) {
  hh_graph(k)
})

hh_popup_hood <- lapply((hoodnums), function(k) {
  hh_graph(k)
})

mobile <- hood_muni |>
  filter(variable %in% c("B07201_002", "B07201_004", "B07201_014")) |>
  mutate(
    Category = case_when(
      str_starts(Group2, "Same") ~ "SameHome",
      !is.na(Group3) ~ "SameCity",
      TRUE ~ "Abroad"
    )
  ) |>
  group_by(Municipality, County, Neighborhood, Category) |>
  summarise(estimate = sum(estimate)) |>
  pivot_wider(
    id_cols = c(Municipality, County, Neighborhood),
    names_from = Category,
    names_prefix = "Total",
    values_from = estimate
  ) |>
  inner_join(pops) |>
  mutate(
    TotalOtherUS = PopMobility-TotalSameHome-TotalSameCity-TotalAbroad,
    RateSameHome = TotalSameHome/PopMobility,
    RateSameCity = TotalSameCity/PopMobility,
    RateOtherUS = TotalOtherUS/PopMobility,
    RateAbroad = TotalAbroad/PopMobility
  ) |>
  select(Municipality:TotalSameHome, PopMobility, TotalOtherUS:RateAbroad)

mobile_rates <- benchmarks(mobile) |>
  inner_join(areas)

mobile_graph <- function(k){
  x <- filter(mobile_rates, HoodID == k) |>
    mutate(
      name = factor(
        name, 
        levels = c("SameHome", "SameCity", "OtherUS",  "Abroad")
      )
    )
  
  ggplot(x, aes(x = name, y = value, fill = Shade)) +
    geom_bar(stat = "identity", color = "black") +
    labs(x = NULL, y = "%", title = unique(x$Neighborhood), fill = NULL) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, .2),
      labels = seq(0, 100, 20)
    ) +
    scale_x_discrete(
      labels = c(
        "Same Home",
        "Same metro area",
        "Elsewhere\nin U.S.",
        "Other Country"
      )
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5)) + 
    geom_text(
      aes(label = paste0(round(value*100, 1), "%"), y = Textloc), 
      size = 4
    ) +
    scale_fill_gradient2(
      limits = c(-1, 1), 
      high = cchmcdarkpurple, 
      low = cchmcdarkgreen, 
      mid = cchmclightblue,
      breaks = waiver(),
      n.breaks = 3,
      labels = c("Lowest", "Average", "Highest")
    )
}

mobile_popup_muni <- lapply((muninums), function(k) {
  mobile_graph(k)
})

mobile_popup_hood <- lapply((hoodnums), function(k) {
  mobile_graph(k)
})

con <- dbConnect(odbc::odbc(), "ClarityProd")

registry <- dbGetQuery(con, "
  SELECT DISTINCT p.pat_id
                ,CAST(p.birth_date AS DATE) AS birth_date
				        ,p.add_line_1
				        ,p.add_line_2
				        ,p.city
				        ,p.state
				        ,p.zip
				        ,p.county
				        ,c.registry_id
				        ,c.registry_name
				        ,g.X
				        ,g.Y
				        ,g.foster
				        ,g.rmh
				        ,g.pobox
				        ,g.cchmc
				        ,g.stjoe
				        ,g.unknown_address
				        ,g.foreign_address
				        ,g.geocode_attempted
  FROM hpceclarity.bmi.registry_config c
    INNER JOIN hpceclarity.bmi.reg_data_hx_membership m
      ON c.registry_id = m.registry_id
    INNER JOIN hpceclarity.bmi.registry_data_info d
      ON m.record_id = d.record_id
    INNER JOIN hpceclarity.bmi.patient p
      ON p.pat_id = d.networked_id
    LEFT JOIN temptable.dbo.full_list_geocode g
      ON (p.add_line_1 = g.add_line_1 
          OR (p.add_line_1 IS NULL AND g.add_line_1 IS NULL))
        AND (p.add_line_2 = g.add_line_2 
          OR (p.add_line_2 IS NULL AND g.add_line_2 IS NULL))
        AND (p.city = g.city OR (p.city IS NULL AND g.city IS NULL))
        AND (p.state = g.state OR (p.state IS NULL AND g.state IS NULL))
        AND (p.zip = g. zip OR (p.zip IS NULL AND g.zip IS NULL))
  WHERE (c.registry_name LIKE '%asthma%' OR c.registry_id = '210652454')
    AND p.add_line_1 NOT LIKE '%222 E%'
    AND m.status_c = 1
    AND g.foster != 1
    AND g.rmh != 1
    AND g.pobox != 1
    AND g.cchmc != 1
    AND g.stjoe != 1
    AND g.unknown_address != 1
    AND g.foreign_address != 1
    AND g.state in ('Ohio', 'Kentucky', 'Indiana')
                  ") |>
  mutate(
    asthma_admission = 0,
    t1d_registry_admission = 0,
    mental_health_admission = 0,
    county = str_to_title(county),
    contact_date = today()
  ) |>
  mutate(Age = as.numeric(today()-birth_date)/365.25) |>
  filter(Age <= 18) 

asthma_exceptions <- dbGetQuery(con, "
  SELECT DISTINCT ha.hsp_account_id
                ,peh.pat_enc_csn_id
    FROM hpceclarity.bmi.hsp_acct_dx_list ha
      INNER JOIN hpceclarity.bmi.edg_current_icd10 edg
        ON ha.dx_id = edg.dx_id
      INNER JOIN hpceclarity.bmi.pat_enc_hsp peh
        ON ha.hsp_account_id = peh.hsp_account_id
    WHERE edg.code in ('E94.9', 'E84.8', 'E84.19', 'E84.0',
                       'E84.11', 'Z93.0', 'Z99.11', 'Z99.1') 
      OR (edg.code like 'D57.%' AND edg.code <> 'D57.3')
      OR edg.code like 'J96.1%'
      OR edg.code like 'J96.2%'
      OR edg.code like 'Q20.%'
      OR edg.code like 'Q24.%'
      OR edg.code like 'Q25.%'
      OR edg.code like 'Q89.3%'
                                ") |>
  mutate(AsthmaException = 1)

admits <- dbGetQuery(con, "
  SELECT DISTINCT r.pat_enc_csn_id
                  ,r.pat_id
                  ,CAST(r.birth_date AS DATE) AS birth_date
                  ,CAST(r.hosp_admsn_time AS DATE) AS contact_date
                  ,a.addr_hx_line1 as add_line_1 
                  ,a.addr_hx_line2 as add_line_2
                  ,a.city_hx as city
                  ,a.state
                  ,a.zip_hx as zip
                  ,a.county
                  ,r.disch_icd_1
		              ,r.disch_icd_2
		              ,r.adt_pat_class
		              ,department_id
		              ,department_name
		              ,g.X
		              ,g.Y
  FROM hpceclarity.bmi.readmissions r
		INNER JOIN hpceclarity.dbo.chmc_adt_addr_hx a
		  ON r.pat_id = a.pat_id
		LEFT JOIN temptable.dbo.full_list_geocode g
		  ON (a.addr_hx_line1 = g.add_line_1 
		      OR (a.addr_hx_line1 IS NULL AND g.add_line_1 IS NULL))
		    AND (a.addr_hx_line2 = g.add_line_2 
		        OR (a.addr_hx_line2 IS NULL AND g.add_line_2 IS NULL))
		    AND (a.city_hx = g.city OR (a.city_hx IS NULL AND g.city IS NULL))
		    AND (a.state = g.state OR (a.state IS NULL AND g.state IS NULL))
		    AND (a.zip_hx = g.zip OR (a.zip_hx IS NULL AND g.zip IS NULL))
  WHERE r.hosp_admsn_time >= a.eff_start_date
		AND (a.eff_end_date IS NULL OR r.hosp_admsn_time < a.eff_end_date)
		AND year(r.hosp_admsn_time) in ('2022', '2023')
		AND a.state IN ('Ohio', 'Indiana', 'Kentucky')
		AND g.foster != 1
    AND g.rmh != 1
    AND g.pobox != 1
    AND g.cchmc != 1
    AND g.stjoe != 1
    AND g.unknown_address != 1
    AND g.foreign_address != 1
                     ") |>
  mutate(
    Asthma = ifelse(
      disch_icd_1 %in% c(
        "J45.21",
        "J45.22", 
        "J45.31", 
        "J45.32", 
        "J45.41", 
        "J45.42", 
        "J45.51", 
        "J45.52", 
        "J45.901", 
        "J45.902" 
      ),
      1,
      0
    ),
    Diabetes = ifelse(
      disch_icd_1 %in% c(
        "E08.10", 
        "E10.10",
        "E10.11",
        "E10.65",
        "E10.9",
        "E11.10",
        "E11.649",
        "E11.65",
        "E11.69",
        "E11.9",
        "E13.10",
        "E13.65",
        "E15",
        "E16.1",
        "E16.2",
        "R73.01",
        "R73.02",
        "R73.03",
        "R73.09"
      ),
      1,
      0
    ),
    MH = ifelse(department_id %in% c(
      20026290, 
      10401085,	
      10401086,	
      10401087,	
      10401088,	
      10401089,	
      10401090,	
      10401091,	
      10401092,	
      10401093,	
      10401094
    ) | str_starts(disch_icd_1, "F") | str_starts(disch_icd_1, "R45"),
    1,
    0
    )
  ) |>
  left_join(asthma_exceptions) |>
  mutate(
    AsthmaException = coalesce(AsthmaException, 0),
    Asthma = ifelse(Asthma == 1 & AsthmaException == 0, 1, 0),
    county = str_to_title(county)
  )

geocoded1 <- filter(registry, !is.na(X)) |>
  select(X, Y) |>
  rbind(filter(admits, !is.na(X)) |> select(X, Y)) |>
  unique() 

to_geocode <- filter(registry, is.na(X)) |>
  select(add_line_1:zip) |>
  rbind(filter(admits, is.na(X)) |> select(add_line_1:zip)) |>
  unique() |> 
  mutate(
    state = case_when(
      state == "Ohio" ~ "OH",
      state == "Kentucky" ~ "KY",
      TRUE ~ "IN"
    ),
    address = paste(add_line_1, city, state, sep = ", "),
    address = paste(address, zip),
    ID = row_number()
  ) |>
  select(ID, address)

#write_csv(to_geocode, "for_degauss.csv")
#docker run --rm -v ${PWD}:/tmp ghcr.io/degauss-org/geocoder:3.0.2 for_degauss.csv

muni_lines <- oki_muni |>
  mutate(
    County = case_when(
      Municipality == "Loveland" ~ "Hamilton",
      Municipality == "Milford" ~ "Clermont",
      Municipality == "Fairfield" ~ "Butler",
      TRUE ~ County
    )
  ) |>
  group_by(Municipality, County) |>
  summarise(geometry = st_union(geometry)) |>
  st_as_sf()

hood_lines <- neigh_sna |>
  rename(Neighborhood = neighborhood) |>
  mutate(
    Neighborhood = case_when(
      Neighborhood == "Villages at Roll Hill" ~ "Roll Hill",
      Neighborhood %in% c("Lower Price Hill", "Queensgate") ~ 
        "Lower Price Hill-Queensgate",
      Neighborhood == "North Avondale - Paddock Hills" ~
        "North Avondale-Paddock Hills",
      TRUE ~ Neighborhood
    ),
    Municipality = "Cincinnati"
  ) |>
  group_by(Municipality, Neighborhood) |>
  summarise(geometry = st_union(SHAPE)) |>
  mutate(geometry = st_transform(geometry, crs = "NAD83"))

geocoded <- read.csv("for_degauss_geocoded_v3.0.2.csv") |>
  filter(
    precision == "range",
    geocode_result != "imprecise_geocode"
  ) |>
  rename(X = lon, Y = lat) |>
  select(X, Y) |>
  rbind(geocoded1) |>
  unique() |>
  mutate(GeocodeID = row_number()) |>
  st_as_sf(coords = c("X", "Y"), crs = 'NAD83', remove = FALSE) |>
  st_join(muni_lines) |>
  filter(!is.na(Municipality)) |>
  group_by(GeocodeID) |>
  mutate(count = n()) |>
  filter(count == 1) |>
  select(-count)

cinci <- filter(geocoded, Municipality == "Cincinnati") |>
  select(-Municipality) |>
  st_join(hood_lines) |>
  filter(!is.na(Neighborhood))

hooded <- geocoded |>
  mutate(Neighborhood = Municipality) |>
  filter(Municipality != "Cincinnati") |>
  rbind(cinci)

registry_totals <- registry |>
  inner_join(hooded) |>
  distinct(pat_id, registry_name, County, Municipality, Neighborhood) |>
  group_by(County, Municipality, Neighborhood, registry_name) |>
  summarise(Patients = n()) |>
  separate_wider_delim(
    cols = registry_name,
    names = c("x", "Disease", "y"),
    delim = " "
  ) |>
  mutate(
    Disease = str_to_title(Disease),
    Disease = paste(Disease, "Registry", sep = "_")
  ) |>
  pivot_wider(
    id_cols = c(County, Municipality, Neighborhood),
    names_from = Disease,
    values_from = Patients,
    values_fill = 0
  )

admit_totals <- inner_join(admits, hooded) |>
  mutate(Year = year(contact_date)) |>
  distinct(
    pat_id, 
    pat_enc_csn_id, 
    Year,
    County, 
    Municipality, 
    Neighborhood,
    Asthma,
    Diabetes,
    MH
  ) |>
  group_by(County, Municipality, Neighborhood, Year) |>
  summarise(
    Asthma_Admits = sum(Asthma),
    Asthma_Admitted = length(unique(pat_id[Asthma == 1])),
    Diabetes_Admits = sum(Diabetes),
    Diabetes_Admitted = length(unique(pat_id[Diabetes == 1])),
    MH_Admits = sum(MH),
    MH_Admitted = length(unique(pat_id[MH == 1])),
    Total_Admits = n(),
    Total_Admitted = length(unique(pat_id))
  ) |>
  mutate(across(Asthma_Admits:Total_Admitted, \(x) coalesce(x, 0))) |>
  group_by(County, Municipality, Neighborhood) |>
  summarise(across(Asthma_Admits:Total_Admitted, mean))

hospital <- full_join(admit_totals, registry_totals) |>
  mutate(across(Asthma_Admits:Diabetes_Registry, \(x) coalesce(x, 0)))

cinci_hospital <- filter(hospital, Municipality == "Cincinnati") |> 
  group_by(County, Municipality) |>
  summarise(across(Asthma_Admits:Diabetes_Registry, sum)) |>
  mutate(Neighborhood = "Cincinnati")

children <- filter(
  hood_muni, 
  Group3 %in% c(
    "Under 5 years",
    "5 to 9 years",
    "10 to 14 years",
    "15 to 17 years"
  )
) |>
  group_by(County, Municipality, Neighborhood) |>
  summarise(Children = sum(estimate))

hospital_children <- rbind(hospital, cinci_hospital) |>
  inner_join(children) |>
  pivot_longer(
    cols = Asthma_Admits:Diabetes_Registry,
    names_to = "Measure"
  ) |>
  mutate(
    Rate = value*100/Children,
    Rate = ifelse(is.infinite(Rate), NA, Rate)
  ) |>
  separate_wider_delim(
    Measure,
    delim = "_",
    names = c("Condition", "Measure")
  )

hospital_means <- ungroup(hospital_children) |>
  filter(Municipality == Neighborhood) |>
  group_by(Condition, Measure) |>
  summarise(value = sum(value)) |>
  cross_join(
    filter(children, Neighborhood == Municipality) |> 
      ungroup() |> 
      summarise(Children = sum(Children))
  ) |>
  mutate(Mean = 100*value/Children) |>
  select(Condition, Measure, Mean)

hospital_rates <- ungroup(hospital_children) |>
  group_by(Condition, Measure) |>
  summarise(
    Min = min(Rate, na.rm = TRUE),
    Max = max(Rate, na.rm = TRUE),
    SD = sd(Rate, na.rm = TRUE)
  ) |>
  inner_join(hospital_means)

hospital_all <- inner_join(hospital_children, hospital_rates) |>
  mutate(
    Rate = ifelse(is.nan(Rate), NA, Rate),
    Shade = (Rate-Mean)/SD
  ) |>
  inner_join(areas)

admit_graph <- function(k){
  x <- hospital_all |>
    filter(
      HoodID == k,
      Condition == "Total"
    ) |>
    mutate(
      Measure = factor(Measure, levels = c("Admits", "Admitted")),
      Textloc = ifelse(Rate > 13, Rate - 1.5, Rate + 1.5)
    )
  
  shadelimits <- c(
    min(hospital_all$Shade[hospital_all$Condition == "Total"]), 
    max(hospital_all$Shade[hospital_all$Condition == "Total"])
  )
  
  ggplot(x, aes(x = Measure, y = Rate, fill = Shade)) +
    geom_bar(stat = "identity", color = "black") +
    labs(
      x = NULL, 
      y = "Per 100 children per year (2022-23)", 
      title = "Total",
      fill = NULL
      ) +
      scale_y_continuous(limits = c(0, 17), breaks = seq(0, 16, 2)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = .5)) + 
      geom_text(aes(label = round(Rate, 1), y = Textloc), size = 4) +
      geom_text(
        aes(
          label = ifelse(
            value > 0, 
            paste("n =", format(value, big.mark = ","), "per year"), 
            ""
            ), 
          y = ifelse(Rate > 13, Textloc - 2, Textloc + 2)
        ),
        size = 4
      ) +
      scale_fill_gradient2(
        limits = shadelimits, 
        high = cchmcdarkpurple, 
        low = cchmcdarkgreen, 
        mid = cchmclightblue,
        midpoint = 0,
        breaks = sort(c(shadelimits, mean(shadelimits))),
        labels = c("Lowest", "", "Highest")
      ) +
      scale_x_discrete(labels = c("Admissions", "Patients\nAdmitted")) 
}

asthma_admit_graph <- function(k){
  x <- hospital_all |>
    filter(
      HoodID == k,
      Condition == "Asthma",
      Measure != "Registry"
      ) |>
    mutate(
      Measure = factor(Measure, levels = c("Admits", "Admitted")),
      Textloc = ifelse(Rate > 2.5, Rate - .2, Rate + .2)
    )
  
  shadelimits <- c(
    min(
      hospital_all$Shade[hospital_all$Condition == "Asthma" & 
                           hospital_all$Measure != "Registry"]
      ),
    max(
      hospital_all$Shade[hospital_all$Condition == "Asthma" & 
                           hospital_all$Measure != "Registry"]
      )
    )

  ggplot(x, aes(x = Measure, y = Rate, fill = Shade)) +
    geom_bar(stat = "identity", color = "black") +
    labs(
      x = NULL, 
      y = "Per 100 children per year (2022-23)", 
      title = "Asthma",
      fill = NULL
      ) +
    scale_y_continuous(limits = c(0, 3)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5)) + 
    geom_text(aes(label = round(Rate, 1), y = Textloc), size = 4) +
    geom_text(
      aes(
        label = ifelse(
          value > 0, 
          paste("n =", format(value, big.mark = ","), "per year"), 
          ""
        ), 
        y = ifelse(Rate > 2.5, Textloc - .3, Textloc + .3)
      ),
      size = 4
    ) +
    scale_fill_gradient2(
      limits = shadelimits, 
      high = cchmcdarkpurple, 
      low = cchmcdarkgreen, 
      mid = cchmclightblue,
      midpoint = 0,
      breaks = sort(c(shadelimits, mean(shadelimits))),
      labels = c("Lowest", "", "Highest")
    ) +
    scale_x_discrete(labels = c("Admissions", "Patients\nAdmitted"))
}

diabetes_admit_graph <- function(k){
  x <- hospital_all |>
    filter(
      HoodID == k,
      Condition == "Diabetes",
      Measure != "Registry"
      ) |>
    mutate(
      Measure = factor(Measure, levels = c("Admits", "Admitted")),
      Textloc = ifelse(Rate > 4, Rate - .3, Rate + .3)
    )
  
  shadelimits <- c(
    min(
      hospital_all$Shade[hospital_all$Condition=="Diabetes" & 
                           hospital_all$Measure != "Registry"]
    ),
    max(
      hospital_all$Shade[hospital_all$Condition == "Diabetes" &
                           hospital_all$Measure != "Registry"]
    )
  )
  
  ggplot(x, aes(x = Measure, y = Rate, fill = Shade)) +
    geom_bar(stat = "identity", color = "black") +
    labs(
      x = NULL, 
      y = "Per 100 children per year (2022-23)", 
      title = "Diabetes",
      fill = NULL
      ) +
      scale_y_continuous(limits = c(0, 5)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = .5)) + 
      geom_text(aes(label = round(Rate, 1), y = Textloc), size = 4) +
      geom_text(
        aes(
          label = ifelse(value > 0, paste("n =", value, "per year"), ""), 
          y = ifelse(Rate > 4, Textloc - .4, Textloc + .4)
        ),
        size = 4
      ) +
      scale_fill_gradient2(
        limits = shadelimits, 
        high = cchmcdarkpurple, 
        low = cchmcdarkgreen, 
        mid = cchmclightblue,
        midpoint = 0,
        breaks = sort(c(shadelimits, mean(shadelimits))),
        labels = c("Lowest", "", "Highest")
      ) +
      scale_x_discrete(labels = c("Admissions", "Patients\nAdmitted"))
}

mh_graph <- function(k){
  x <- hospital_all |>
    filter(
      HoodID == k,
      Condition == "MH"
      ) |>
    mutate(
      Measure = factor(Measure, levels = c("Admits", "Admitted")),
      Textloc = ifelse(Rate > 3, Rate - .3, Rate + .3)
    )
  
  shadelimits <- c(
    min(hospital_all$Shade[hospital_all$Condition == "MH"]),
    max(hospital_all$Shade[hospital_all$Condition == "MH"])
    )
  
  ggplot(x, aes(x = Measure, y = Rate, fill = Shade)) +
    geom_bar(stat = "identity", color = "black") +
    labs(
      x = NULL, 
      y = "Per 100 children per year (2022-23)", 
      title = "Psychiatric",
      fill = NULL
      ) +
    scale_y_continuous(limits = c(0, 4)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5)) + 
    geom_text(aes(label = round(Rate, 1), y = Textloc), size = 4) +
    geom_text(
      aes(
        label = ifelse(value > 0, paste("n =", value, "per year"), ""), 
        y = ifelse(Rate > 3, Textloc - .4, Textloc + .4)
        ),
      size = 4
      ) +
    scale_fill_gradient2(
      limits = shadelimits, 
      high = cchmcdarkpurple, 
      mid = cchmclightblue,
      low = cchmcdarkgreen,
      midpoint = 0,
      breaks = sort(c(shadelimits, mean(shadelimits))),
      labels = c("Lowest", "", "Highest")
      ) +
    scale_x_discrete(labels = c("Admissions", "Patients\nAdmitted"))
}

admit_grid <- function(k){
  p1 <- admit_graph(k) + theme(legend.position = "none")
  p2 <- asthma_admit_graph(k) + theme(legend.position = "none")
  p3 <- diabetes_admit_graph(k) + theme(legend.position = "none")
  p4 <- mh_graph(k) + theme(legend.position = "none")
  plots <- plot_grid(p1, p2, p3, p4, ncol = 2)
  title <- ggdraw() +
    draw_label(areas$Neighborhood[areas$HoodID == k])
  legend <- get_legend(p1 + theme(legend.position = "bottom"))
  plot_grid(title, plots, legend, ncol = 1, rel_heights = c(0.08, 1, .1))
}

admit_popup_muni <- lapply((muninums), function(k) {
  admit_grid(k)
})

admit_popup_hood <- lapply((hoodnums), function(k) {
  admit_grid(k)
})

registry_graph <- function(k){
  x <- hospital_all |>
    filter(
      Measure == "Registry",
      HoodID == k
      ) |>
    mutate(Textloc = ifelse(Rate > 38, Rate - 1, Rate + 1))
  
  shadelimits <- c(
    min(hospital_all$Shade[hospital_all$Measure == "Registry"]),
    max(hospital_all$Shade[hospital_all$Condition == "MH"])
  )
  
  ggplot(x, aes(x = Condition, y = Rate, fill = Shade)) +
    geom_bar(stat = "identity", color = "black") +
    labs(
      x = NULL, 
      y = glue("Per 100 children (as of ", {format(today(), "%b %d, %Y")}, ")"), 
      title = unique(x$Neighborhood),
      fill = NULL
    ) +
    scale_y_continuous(limits = c(0, 40)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5)) + 
    geom_text(aes(label = round(Rate, 1), y = Textloc), size = 4) +
    geom_text(
      aes(
        label = ifelse(
          value > 0, 
          paste("n =", format(value, big.mark = ",")), 
          ""
          ), 
        y = ifelse(Rate > 38, Textloc - 2, Textloc + 2)
      ),
      size = 4
    ) +
    scale_fill_gradient2(
      limits = shadelimits, 
      high = cchmcdarkpurple, 
      mid = cchmclightblue,
      low = cchmcdarkgreen,
      midpoint = 0,
      breaks = sort(c(shadelimits, mean(shadelimits))),
      labels = c("Lowest", "", "Highest")
    )
}

registry_popup_muni <- lapply((muninums), function(k){
  registry_graph(k)
})

registry_popup_hood <- lapply((hoodnums), function(k){
  registry_graph(k)
})

schools <- read_delim("cps.txt", delim = " ")

colnames(schools) <- c(LETTERS[1:5], LETTERS[7:13])

schools2 <- schools |>
  mutate(
    Num1 = parse_number(C),
    Num2 = parse_number(D),
    Num3 = parse_number(E),
    Num4 = parse_number(G),
    Num5 = parse_number(H),
    Num6 = parse_number(I),
    Num7 = parse_number(J),
    Number = coalesce(Num1, Num2),
    Number = coalesce(Number, Num3),
    Number = coalesce(Number, Num4),
    Number = coalesce(Number, Num5),
    Number = coalesce(Number, Num6),
    Number = coalesce(Number, Num7),
    Number = ifelse(B == "Promise", "5425", Number),
    Name = paste(A, B),
    Name = ifelse(C == Number, Name, paste(Name, C)),
    Name = ifelse(Number == C | Number == D, Name, paste(Name, D)),
    Name  = ifelse(
      Number == C | Number == D | Number == E,
      Name,
      paste(Name, E)
    ),
    Name  = ifelse(
      Number == C | Number == D | Number == E | Number == G,
      Name,
      paste(Name, G)
    ),
    Name  = ifelse(
      Number == C | Number == D | Number == E | Number == G | Number == H,
      Name,
      paste(Name, H)
    ),
    Combo = paste(A, B, C, D, E, G, H, I, J, K, L, M),
    Combo = str_remove(Combo, Name),
    Combo = str_remove(Combo, Number),
    Combo = str_trim(Combo)
  ) |>
  separate_wider_delim(
    Combo, 
    delim = ",", 
    names = c("Street", "City", "Rest"), 
    too_few = "align_start"
  ) |>
  mutate(Rest = str_trim(Rest)) |>
  separate_wider_delim(
    Rest, 
    delim = " ", 
    names = c("State", "Zip"), 
    too_many = "drop"
  ) |>
  mutate(
    City = "Cincinnati",
    State = "OH",
    Street = ifelse(B == "Promise", "Winton Ridge Lane", Street),
    Zip = ifelse(Name == "Mt. Washington School", "45230", Zip),
    Name = ifelse(B == "Promise", "The Promise Center", Name),
    Name = ifelse(
      str_detect(Name, "Gamble Montessori Elementary"),
      "Gamble Montessori Elementary School",
      Name
    ),
    Name = ifelse(str_detect(Name, "Aiken"), "Aiken High School", Name)
  ) |>
  distinct(Name, Number, Street, City, State, Zip) |>
  filter(
    !Name %in% c(
      "Cincinnati Digital Academy", 
      "Virtual High School",
      "Hospital/Satellite Program Office"
    )
  ) |>
  mutate(
    Address = paste(Number, Street),
    Type = "School"
  ) |>
  select(-c(Street, Number))

pharms <- read_excel("pharmacies.xlsx")

to_geocode <- pharms |>
  separate_wider_delim(
    pharmacy_name, 
    delim = "-", 
    names = c("Name", "name2", "name3"), 
    too_many = "merge",
    too_few = "align_start"
  ) |>
  mutate(
    name2 = ifelse(name2 == "", name3, name2),
    comma = str_detect(name2, ","),
    city = str_trim(ifelse(comma, name2, name3))
  ) |>
  separate_wider_delim(
    city,
    delim = "-",
    names = c("city", "add", "add2", "add3"),
    too_few = "align_start",
    too_many = "merge"
  ) |>
  mutate(
    comma = str_detect(city, ","),
    city = ifelse(comma, city, str_trim(add)),
    comma = str_detect(city, ","),
    city = ifelse(comma, city, str_trim(add2)),
    comma = str_detect(city, ",")
    ) |>
  filter(comma) |>
  separate_wider_delim(
    city,
    delim = ",",
    names = c("city", "state", "state2", "state3"),
    too_few = "align_start"
  ) |>
  mutate(
    state = case_when(
      !is.na(state3) ~ state3,
      !is.na(state2) ~ state2,
      TRUE ~ state
    ),
    state = str_trim(state),
    length = str_length(state)
  ) |>
  filter(
    length == 2,
    state %in% c("OH", "KY", "IN")
  ) |>
  mutate(
    add3 = ifelse(str_detect(add3, "NON E-RX"), NA, str_trim(add3))
  ) |>
  separate_wider_delim(
    add2, 
    names = "add2", 
    delim = "SUITE", 
    too_many = "drop"
    ) |>
  separate_wider_delim(add3, names = "add3", delim = ",", too_many = "drop") |>
  separate_wider_delim(add2, names = "add2", delim = " STE", too_many = "drop") |>
  separate_wider_delim(add2, names = "add2", delim = " ROOM", too_many = "drop") |>
  separate_wider_delim(add2, names = "add2", delim = " PO ", too_many = "drop") |>
  separate_wider_delim(add2, names = "add2", delim = " RM ", too_many = "drop") |> 
  mutate(
    add2 = str_trim(add2),
    add = ifelse(
      str_trunc(add2, 1, "right", ellipsis = "") %in% 1:9 | is.na(add2), 
      add, 
      paste(add, add2, sep = " ")
      ),
    add2 = ifelse(
      !str_trunc(add2, 1, "right", ellipsis = "") %in% 1:9, 
      NA, 
      add2
      ),
    add2 = ifelse(str_length(add2) < 4, NA, add2),
    add = ifelse(!is.na(add2), NA, str_trim(add)),
    add = coalesce(add3, add),
    add = coalesce(add2, add),
    add = coalesce(add, name3)
    ) |>
  separate_wider_delim(add, names = "add", delim = " AT ", too_many = "drop") |>
  separate_wider_delim(add, names = "add", delim = " STE", too_many = "drop") |>
  separate_wider_delim(add, names = "add", delim = " SUITE ", too_many = "drop") |>
  mutate(
    add = str_trim(add),
    Zip = NA,
    Type = "Pharmacy"
    ) |>
  select(Name, add, city, state, Zip, Type) |>
  rename(
    Address = add,
    City = city,
    State = state
    ) |>
  rbind(schools2)

geocoded_points <- geocode(
  to_geocode,
  street = Address,
  city = City,
  state = State,
  postalcode = Zip,
  method = "census"
) 

undone <- filter(geocoded_points, is.na(lat)) |>
  select(-c(lat, long)) |>
  geocode(
    street = Address,
    city = City,
    state = State,
    postalcode = Zip,
    method = "geoapify"
  )

points_all <- undone |>
  select(Name, Type, lat, long, Address) |>
  rbind(select(geocoded_points, Name, lat, long, Type, Address)) |>
  filter(!is.na(lat))

points_all2 <- points_all |>
  st_as_sf(coords = c("long", "lat"), crs = "NAD83") |>
  inner_join(points_all, multiple = "all") |>
  unique()

points <- muni_lines |>
  st_join(points_all2, left = FALSE)

cinci_points <- filter(
  points, 
  Municipality %in% c("Cincinnati", "Norwood", "St. Bernard", "Elmwood Place") | 
    Type == "School"
)

map_lines <- muni_lines |> 
  mutate(Neighborhood = Municipality) |>
  rbind(hood_lines |> mutate(County = "Hamilton")) |>
  inner_join(di_rates) |>
  inner_join(areas) |>
  mutate(
    Tier = case_when(
      value >= .6 ~ "Highest",
      value >= .475 ~ "Higher",
      value >= .35 ~ "Medium",
      value >= .225 ~ "Lower",
      TRUE ~ "Lowest"
    ),
    Tier = factor(
      Tier, 
      levels = c("Lowest", "Lower", "Medium", "Higher", "Highest")
    ),
    Centroid = st_centroid(geometry)
  ) |>
  arrange(HoodID) |>
  select(geometry, Neighborhood, Municipality, Tier, Centroid, HoodID)

centroids <- st_coordinates(map_lines$Centroid) |>
  as_tibble() |>
  mutate(
    Neighborhood = map_lines$Neighborhood,
    X = case_when(
      Neighborhood == "Sycamore Township" ~ -84.3788,
      Neighborhood == "Columbia Township" ~ -84.40098,
      TRUE ~ X
    ),
    Y = case_when(
      Neighborhood == "Sycamore Township" ~ 39.20386,
      Neighborhood == "Columbia Township" ~ 39.17114,
      Neighborhood == "East End" ~ 39.122,
      Neighborhood == "Riverside" ~ 39.07735,
      TRUE ~ Y
    ),
    HoodID = map_lines$HoodID
  ) |>
  st_as_sf(coords = c("X", "Y"), crs = "NAD83") |>
  rename(Centroid = geometry)

map_lines <- as_tibble(select(map_lines, -Centroid)) |>
  inner_join(as_tibble(centroids)) |>
  st_as_sf()

cinci_map_lines <- map_lines |>
  filter(Neighborhood != Municipality |
           Municipality %in% c("Norwood", "St. Bernard", "Elmwood Place")
  )

muni_map_lines <- filter(map_lines, Neighborhood == Municipality)

pal <- colorFactor(
  c(
    cchmcdarkgreen, 
    cchmclightgreen, 
    cchmclightblue, 
    cchmclightpurple, 
    cchmcdarkpurple
  ), 
  domain = map_lines$Tier
)

muni_icons <- icons(
  iconUrl = ifelse(
    points$Type == "School", 
    "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
    "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-yellow.png"
  ),
  iconWidth = 20,
  iconHeight = 30
)

cinci_icons <- icons(
  iconUrl = ifelse(
    cinci_points$Type == "School", 
    "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
    "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-yellow.png"
  ),
  iconWidth = 20,
  iconHeight = 30
)

city_map <- leaflet() |>
  addTiles() |>
  addPolygons(
    data = cinci_map_lines,
    stroke = TRUE,
    weight = 1,
    smoothFactor = .5,
    opacity = 1,
    fillOpacity = .7,
    fillColor = ~pal(Tier)
  ) |>
  addLegendFactor(
    pal = pal,
    values = cinci_map_lines$Tier,
    title = "Deprivation Index",
    position = "topleft"
  ) |>
  addLayersControl(
    baseGroups = c(
      "Demographics", 
      "Deprivation indicators",
      "Age and gender",
      "Household types",
      "Geographic mobility",
      "Hospital admissions",
      "Medical registries",
      "Schools and pharmacies"
    ),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addCircleMarkers(
    data = cinci_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(demo_popup_hood, height = 300, width = 700),
    group = "Demographics"
  ) |>
  addCircleMarkers(
    data = cinci_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(di_popup_hood, height = 300, width = 700),
    group = "Deprivation indicators"
  ) |>
  addCircleMarkers(
    data = cinci_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(pyr_popup_hood, height = 300, width = 700),
    group = "Age and gender"
  ) |>
  addCircleMarkers(
    data = cinci_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(hh_popup_hood, height = 300, width = 700),
    group = "Household types"
  ) |>
  addCircleMarkers(
    data = cinci_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(mobile_popup_hood, height = 300, width = 700),
    group = "Geographic mobility"
  ) |>
  addCircleMarkers(
    data = cinci_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(admit_popup_hood, height = 500, width = 700),
    group = "Hospital admissions"
  ) |>
  addCircleMarkers(
    data = cinci_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(registry_popup_hood, height = 300, width = 600),
    group = "Health registries"
  ) |>
  addMarkers(
    data = cinci_points,
    ~long,
    ~lat,
    icon = cinci_icons,
    popup = ~Name,
    group = "Schools and pharmacies"
  ) 

saveWidget(city_map, "neighborhood maps/city map.html")

muni_map <- leaflet() |>
  addTiles() |>
  addPolygons(
    data = muni_map_lines,
    stroke = TRUE,
    weight = 1,
    smoothFactor = .5,
    opacity = 1,
    fillOpacity = .7,
    fillColor = ~pal(Tier)
  ) |>
  addLegendFactor(
    pal = pal,
    values = muni_map_lines$Tier,
    title = "Deprivation Index",
    position = "topleft"
  ) |>
  addLayersControl(
    baseGroups = c(
      "Demographics", 
      "Deprivation indicators",
      "Age and gender",
      "Household types",
      "Geographic mobility",
      "Hospital admissions",
      "Medical registries",
      "Schools and pharmacies"
    ),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addCircleMarkers(
    data = muni_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(demo_popup_muni, height = 300, width = 700),
    group = "Demographics"
  ) |>
  addCircleMarkers(
    data = muni_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(di_popup_muni, height = 300, width = 700),
    group = "Deprivation indicators"
  ) |>
  addCircleMarkers(
    data = muni_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(pyr_popup_muni, height = 300, width = 700),
    group = "Age and gender"
  ) |>
  addCircleMarkers(
    data = muni_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(hh_popup_muni, height = 300, width = 700),
    group = "Household types"
  ) |>
  addCircleMarkers(
    data = muni_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(mobile_popup_muni, height = 300, width = 700),
    group = "Geographic mobility"
  ) |>
  addCircleMarkers(
    data = muni_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(admit_popup_muni, height = 500, width = 700),
    group = "Hospital admissions"
  ) |>
  addCircleMarkers(
    data = muni_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(registry_popup_muni, height = 300, width = 700),
    group = "Medical registries"
  ) |>
  addMarkers(
    data = points,
    ~long,
    ~lat,
    icon = muni_icons,
    popup = ~Name,
    group = "Schools and pharmacies"
  ) 

saveWidget(muni_map, "neighborhood maps/area map.html")

assist_totals <- filter(hood_muni, variable %in% c("B19058_002", "B11012_001")) |>
  filter(Neighborhood == Municipality) |>
  mutate(
    Category = ifelse(variable == "B11012_001", "PopHH", "TotalAssistance")
  ) |>
  pivot_wider(
    id_cols = c(County, Municipality, Neighborhood),
    names_from = Category,
    values_from = estimate
  ) |>
  rbind(hood_assistance) |>
  mutate(
    value = TotalAssistance/PopHH,
    name = "Assistance"
  ) |>
  rename(AssistancePct = value) |>
  select(County:AssistancePct)

dataset <- inner_join(race, lang) |>
  inner_join(dep_index |> rename(DI = value) |> select(-name)) |>
  inner_join(di_meas) |>
  left_join(assist_totals) |>
  inner_join(hh) |>
  inner_join(mobile) |>
  left_join(hospital) |>
  left_join(
    hospital_children |> distinct(County, Municipality, Neighborhood, Children)
    ) |>
  left_join(oki_income |> rename(MedianHHIncome = value) |> select(-name)) |>
  rename(
    Black = TotalBlack,
    Hispanic = TotalHispanic,
    White = TotalWhite,
    Population = PopPopulation,
    BlackRate = RateBlack,
    WhiteRate = RateWhite,
    HispanicRate = RateHispanic,
    EnglishNotWell = TotalNotWell,
    Over5 = PopOver5,
    OtherThanEnglish = TotalOtherThanEnglish,
    OtherThanEnglishRate = RateOtherThanEnglish,
    EnglishNotWellRate = RateNotWell,
    DeprivationIndex = DI,
    HSGraduates = TotalNoHighSchool,
    PovertyFamilies = TotalPoverty,
    Uninsured = TotalUninsured,
    VacantHousing = TotalVacant,
    Over1 = PopMobility,
    Households = PopHH,
    Over25 = PopOver25,
    Families = PopPoverty,
    HousingUnits = PopHousingUnits,
    CoupleNoKids = TotalCouple,
    SingleNoKids = TotalSingle,
    SingleParentHH = TotalSingleParent,
    TwoParentHH = TotalTwoParent,
    CoupleNoKidsRate = RateCouple,
    SingleNoKidsRate = RateSingle,
    SingleParentHHRate = RateSingleParent,
    TwoParentHHRate = RateTwoParent,
    AbroadLastYear = TotalAbroad,
    SameMetroLastYear = TotalSameCity,
    SameHomeLastYear = TotalSameHome,
    USLastYear = TotalOtherUS,
    SameHomeRate = RateSameHome,
    SameMetroRate = RateSameCity,
    USLastYearRate = RateOtherUS,
    AbroadLastYearRate = RateAbroad,
    AsthmaAdmissions = Asthma_Admits,
    AsthmaPatientsAdmitted = Asthma_Admitted,
    DiabetesAdmissions = Diabetes_Admits,
    DiabetesPatientsAdmitted = Diabetes_Admitted,
    MHAdmissions = MH_Admits,
    MHPatientsAdmitted = MH_Admitted,
    HospitalAdmissions = Total_Admits,
    HospitalPatients = Total_Admitted,
    AsthmaRegistry = Asthma_Registry,
    DiabetesRegistry = Diabetes_Registry,
    AssistanceHH = TotalAssistance,
    AssistanceRate = AssistancePct
  ) |>
  mutate(
    PovertyRate = PovertyFamilies/Families,
    UninsuredRate = Uninsured/Population,
    GraduationRate = HSGraduates/Over25,
    VacancyRate = VacantHousing/HousingUnits,
    HospitalAdmissionRate = 100*HospitalAdmissions/Children,
    HospitalPatientRate = 100*HospitalPatients/Children,
    AsthmaAdmissionRate = 100*AsthmaAdmissions/Children,
    AsthmaPatientRate = 100*AsthmaPatientsAdmitted/Children,
    AsthmaRegistryRate = 100*AsthmaRegistry/Children,
    DiabetesAdmissionRate = 100*DiabetesAdmissions/Children,
    DiabetesPatientRate = 100*DiabetesPatientsAdmitted/Children,
    DiabetesRegistryRate = 100*DiabetesRegistry/Children,
    MHAdmissionRate = 100*MHAdmissions/Children,
    MHPatientRate = 100*MHPatientsAdmitted/Children
    ) |>
  select(
    County:Neighborhood,
    Population,
    Households,
    Families,
    HousingUnits,
    Children,
    Over1,
    Over5,
    Over25,
    White,
    WhiteRate,
    Black,
    BlackRate,
    Hispanic,
    HispanicRate,
    OtherThanEnglish,
    OtherThanEnglishRate,
    EnglishNotWell,
    EnglishNotWellRate,
    DeprivationIndex,
    PovertyFamilies,
    PovertyRate,
    AssistanceHH,
    AssistanceRate,
    MedianHHIncome,
    Uninsured,
    UninsuredRate,
    HSGraduates,
    GraduationRate,
    VacantHousing,
    VacancyRate,
    TwoParentHH,
    TwoParentHHRate, 
    SingleParentHH,
    SingleParentHHRate,
    CoupleNoKids,
    CoupleNoKidsRate,
    SingleNoKids,
    SingleNoKidsRate,
    SameHomeLastYear,
    SameHomeRate,
    SameMetroLastYear,
    SameMetroRate,
    USLastYear,
    USLastYearRate,
    AbroadLastYear,
    AbroadLastYearRate,
    HospitalAdmissions,
    HospitalAdmissionRate,
    HospitalPatients,
    HospitalPatientRate,
    AsthmaRegistry,
    AsthmaRegistryRate,
    AsthmaAdmissions,
    AsthmaAdmissionRate,
    AsthmaPatientsAdmitted,
    AsthmaPatientRate,
    DiabetesRegistry,
    DiabetesRegistryRate,
    DiabetesAdmissions,
    DiabetesAdmissionRate,
    DiabetesPatientsAdmitted,
    DiabetesPatientRate,
    MHAdmissions,
    MHAdmissionRate,
    MHPatientsAdmitted,
    MHPatientRate
  ) |>
  mutate(
    across(
      c(
        Population:White, 
        Black, 
        Hispanic, 
        OtherThanEnglish, 
        EnglishNotWell,
        PovertyFamilies,
        AssistanceHH,
        MedianHHIncome,
        Uninsured,
        HSGraduates,
        VacantHousing,
        TwoParentHH,
        SingleParentHH,
        CoupleNoKids,
        SingleNoKids,
        SameHomeLastYear,
        SameMetroLastYear,
        USLastYear,
        AbroadLastYear
        ),
      \(x) round(x, 0)
      ),
    across(
      c(
        WhiteRate,
        BlackRate,
        HispanicRate,
        OtherThanEnglishRate,
        EnglishNotWellRate,
        DeprivationIndex,
        PovertyRate,
        AssistanceRate,
        UninsuredRate,
        GraduationRate,
        VacancyRate,
        TwoParentHHRate,
        SingleParentHHRate,
        CoupleNoKidsRate,
        SingleNoKidsRate,
        SameHomeRate,
        SameMetroRate,
        USLastYearRate,
        AbroadLastYearRate
      ),
      \(x) round(x, 3)
      ),
    across(
      c(
        HospitalAdmissionRate,
        HospitalPatientRate,
        AsthmaRegistryRate,
        AsthmaAdmissionRate,
        AsthmaPatientRate,
        DiabetesRegistryRate,
        DiabetesAdmissionRate,
        DiabetesPatientRate,
        MHAdmissionRate,
        MHPatientRate
      ),
      \(x) round(x, 1)
      )
    )

# write_csv(dataset, "neighborhood maps/Neighborhood dataset.csv")

agedata <- select(pyramid, County:Population, Share) |>
  rename(Pct = Share)
# write_csv(agedata, "neighborhood maps/Neighborhood Age and Gender.csv")
