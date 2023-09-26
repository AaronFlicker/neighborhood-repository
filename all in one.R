library(DBI)
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

## List of census tract deprivation indexes
di <- read.csv(
  "input data/deprivation index all tracts 2021.csv",
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

bg_vars <- load_variables("acs5", year = 2021) |>
  filter(name %in% bg_var_list)

hamco_bg <- get_acs(
  geography = "block group",
  variables = bg_var_list,
  county = "Hamilton",
  state = "OH",
  geometry = TRUE
  ) |>
  inner_join(bg_vars, by = c("variable" = "name")) |>
  inner_join(allocations, multiple = "all") |>
  mutate(estimate = estimate*Allocation) 

cinci_bg <- filter(hamco_bg, Municipality == "Cincinnati")
  
cinci_bg2 <- cinci_bg |>
  as_tibble() |>
  group_by(Municipality, Neighborhood, variable, label, concept) |>
  summarise(estimate = sum(estimate)) |>
  mutate(County = "Hamilton")

oh_muni <- get_acs(
  geography = "county subdivision",
  variables = c(bg_var_list, "B19058_002"),
  county = c("Hamilton", "Clermont", "Butler", "Warren"),
  state = "OH",
  geometry = TRUE
) 

ky_muni <- get_acs(
  geography = "county subdivision",
  variables = c(bg_var_list, "B19058_002"),
  county = c("Boone", "Campbell", "Kenton"),
  state = "KY",
  geometry = TRUE
) 

in_muni <- get_acs(
  geography = "county subdivision",
  variables = c(bg_var_list, "B19058_002"),
  county = "Dearborn",
  state = "IN",
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
  ) |>
  select(-State)

oki_muni2 <- as_tibble(oki_muni) |>
  select(Municipality:estimate) |>
  left_join(bg_vars, by = c("variable" = "name")) |>
  mutate(
    County = case_when(
      Municipality == "Loveland" ~ "Hamilton",
      Municipality == "Milford" ~ "Clermont",
      Municipality == "Fairfield" ~ "Butler",
      TRUE ~ County
      )
    ) |>
  group_by(Municipality, County, variable, label, concept) |>
  summarise(estimate = sum(estimate)) |>
  mutate(Neighborhood = NA) |>
  ungroup() |>
  rbind(cinci_bg2) |>
  separate_wider_delim(
    label,
    names = paste0("Group", 1:5),
    delim = ":!!",
    too_few = "align_start"
  ) |>
  mutate(Neighborhood = coalesce(Neighborhood, Municipality))

pyramid <- filter(oki_muni2, concept == "SEX BY AGE") |>
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
  summarise(Population = sum(estimate)) 

areas <- pyramid |>
  ungroup() |>
  distinct(County, Municipality, Neighborhood) |>
  arrange(str_to_title(Neighborhood), County) |>
  mutate(HoodID = row_number())

hoodnums <- areas$HoodID[areas$Municipality%in% c(
  "Cincinnati", 
  "Norwood", 
  "St. Bernard", 
  "Elmwood Place"
  ) & areas$Neighborhood != "Cincinnati"]

muninums <- areas$HoodID[areas$Neighborhood == areas$Municipality]
  
pyramid <- inner_join(pyramid, areas) |>
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

pops <- oki_muni2 |> 
  filter(
    str_ends(variable, "_001"),
    str_detect(concept, "INCOME", negate = TRUE)
    ) |>
  mutate(
    Measure = case_when(
      str_starts(concept, "SEX") ~ "Population",
      str_starts(concept, "GEOGRAPHICAL") ~ "Mobility",
      str_starts(concept, "HOUSEHOLDS") ~ "HH",
      str_starts(concept, "EDUCATIONAL") ~ "Over25",
      str_starts(concept, "AGE") ~ "Over5",
      str_starts(concept, "POVERTY") ~ "Poverty",
      TRUE ~ "HousingUnits"
    )
  ) |>
  pivot_wider(
    id_cols = c(County, Municipality, Neighborhood),
    names_from = Measure,
    names_prefix = "Pop",
    values_from = estimate
  )

race <- filter(oki_muni2, str_detect(concept, "RACE")) |>
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

lang <- filter(oki_muni2, str_detect(concept, "LANGUAGE")) |>
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

hood_baselines <- function(k){
  x <- filter(k, !is.na(Neighborhood) | Municipality != "Cincinnati") |>
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
  
  mins <- select(x, starts_with("Rate")) |>
    summarise(across(everything(), min)) |>
    pivot_longer(cols = everything()) |>
    mutate(
      name = str_remove(name, "Rate"),
      Measure = "Min"
      )
  
  maxes <-select(x, starts_with("Rate")) |>
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
  
  select(k, c(County:Neighborhood, starts_with("Rate"))) |>
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

race_rates <- hood_baselines(race)

lang_rates <- hood_baselines(lang)

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
    labs(x = NULL, y = "%", title = unique(x$Neighborhood)) +
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
    guides(fill = "none") +
    theme(plot.title = element_text(hjust = .5)) + 
    geom_text(
      aes(label = paste0(round(value*100, 1), "%"), y = Textloc), 
      size = 4
    ) +
    scale_fill_gradient2(
      limits = c(-1, 1), 
      high = cchmcdarkpurple, 
      low = cchmcdarkgreen, 
      mid = cchmclightblue
    )
} 

demo_popup_muni <- lapply((muninums), function(k) {
  demo_graph(k)
})

demo_popup_hood <- lapply((hoodnums), function(k) {
  demo_graph(k)
})

hh <- filter(oki_muni2, 
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

hh_rates <- hood_baselines(hh) |>
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
    labs(x = NULL, y = "%", title = unique(x$Neighborhood)) +
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
    guides(fill = "none") +
    theme(plot.title = element_text(hjust = .5)) + 
    geom_text(
      aes(label = paste0(round(value*100, 1), "%"), y = Textloc), 
      size = 4
    ) +
    scale_fill_gradient2(
      limits = c(0, 1), 
      high = cchmcdarkpurple, 
      low = cchmcdarkgreen, 
      mid = cchmclightblue
    )
} 

hh_popup_muni <- lapply((muninums), function(k) {
  hh_graph(k)
})

hh_popup_hood <- lapply((hoodnums), function(k) {
  hh_graph(k)
})

cinci_hh <- filter(hamco_bg, variable == "B11012_001") |>
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

assistance <- filter(oki_muni2, variable %in% c("B19058_002", "B11012_001")) |>
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

digraph_rates <- function(k, var){
  z <- filter(oki_muni2, variable == var) |>
    inner_join(k) |>
    filter(Neighborhood == Municipality) |>
    mutate(Weight = value*estimate) |>
    summarise(
      Weight = sum(Weight),
      Pop = sum(estimate)
    ) |>
    mutate(
      Mean = Weight/Pop,
      Min = min(k$value),
      Max = max(k$value)
      ) |>
    select(Mean:Max) |>
    cross_join(k) |>
    mutate(
      Scale = (value-Min)/(Max-Min),
      Textloc = ifelse(Scale > .9, Scale - .025, Scale + .04)
      )
  z
}

di_rates <- digraph_rates(dep_index, "B01001_001", "down") |>
  mutate(Shade = 1-Scale)

hood_income <- filter(cinci_bg, variable %in% c("B11012_001", "B19049_001")) |>
  as_tibble() |>
  mutate(Category = ifelse(variable == "B19049_001", "MedianIncome", "HH")) |>
  pivot_wider(
    id_cols = c(Municipality, Neighborhood, GEOID),
    names_from = Category,
    values_from = estimate
  ) |>
  filter(!is.na(MedianIncome)) |>
  mutate(Income = MedianIncome*HH) |>
  group_by(Municipality, Neighborhood) |>
  summarise(
    Income = sum(Income),
    HH = sum(HH)
    ) |>
  mutate(
    value = Income/HH,
    County = "Hamilton"
    )

oki_income <- rbind(oh_muni, ky_muni) |>
  rbind(in_muni) |>
  filter(variable %in% c("B11012_001", "B19049_001")) |>
  separate_wider_delim(
    NAME,
    delim = ", ",
    names = c("Municipality", "County", "State")
  ) |>
  mutate(
    Municipality = str_remove(Municipality, " city"),
    Municipality = str_remove(Municipality, " village"),
    Municipality = str_remove(Municipality, "The Village of "),
    Municipality = str_remove(Municipality, "CCD"),
    Municipality = str_to_title(Municipality),
    County = str_remove(County, " County"),
    County = case_when(
      Municipality == "Loveland" ~ "Hamilton",
      Municipality == "Fairfield" ~ "Butler",
      Municipality == "Milford" ~ "Clermont",
      TRUE ~ County
    )
  ) |>
  mutate(Category = ifelse(variable == "B19049_001", "MedianIncome", "HH")) |>
  pivot_wider(
    id_cols = c(GEOID, Municipality, County),
    names_from = Category,
    values_from = estimate
  ) |>
  filter(!is.na(MedianIncome)) |>
  mutate(Income = HH*MedianIncome) |>
  group_by(Municipality, County) |>
  summarise(
    HH = sum(HH),
    Income = sum(Income)
  ) |>
  mutate(value = Income/HH) |>
  rbind(hood_income) |>
  select(-c(HH, Income)) |>
  mutate(
    Municipality = str_trim(Municipality),
    name = "MedianIncome",
    Neighborhood = coalesce(Neighborhood, Municipality)
    )

income_rates <- digraph_rates(oki_income, "B11012_001") |>
  mutate(Shade = Scale)

di_meas <- oki_muni2 |>
  filter(variable %in% c(paste0("B15003_0", 17:25), "B17010_002", "B25002_003") |
           str_detect(variable, "B27010")) |>
  mutate(
    Category = case_when(
      str_starts(concept, "EDUCATIONAL") ~ "NoHighSchool",
      str_starts(concept, "POVERTY") ~ "Poverty",
      str_starts(concept, "OCCUPANCY") ~ "Vacant",
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

pov_rates <- digraph_rates(pov, "B17010_001") |>
  mutate(Shade = 1-Scale)

ins <- select(di_meas, TotalUninsured, PopPopulation) |>
  mutate(value = TotalUninsured/PopPopulation) |>
  select(value)

ins_rates <- digraph_rates(ins, "B01001_001") |>
  mutate(
    name = "Uninsured",
    Shade = 1-Scale
  )

assist_rates <- digraph_rates(assistance, "B11012_001") |>
  mutate(Shade = 1-Scale) 

hs <- select(di_meas, Municipality:TotalNoHighSchool, PopOver25) |>
  mutate(
    value = TotalNoHighSchool/PopOver25,
    name = "NoHS"
  ) |>
  select(Municipality:Neighborhood, name, value)

hs_rates <- digraph_rates(hs, "B15003_001") |>
  mutate(Shade = Scale)

vac <- di_meas |>
  mutate(
    value = TotalVacant/PopHousingUnits,
    name = "Vacancy"
    ) |>
  select(Municipality:Neighborhood, name, value)

vac_rates <- digraph_rates(vac, "B25002_001") |>
  mutate(Shade = 1-Scale)

di_all <- rbind(di_rates, pov_rates) |>
  rbind(assist_rates) |>
  rbind(income_rates) |>
  rbind(ins_rates) |>
  rbind(hs_rates) |>
  rbind(vac_rates) |>
  inner_join(areas)

di_graph <- function(k){
  x <- filter(di_all, HoodID == k) |>
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
        name == "DI" ~ as.character(round(value, 3)),
        name == "MedianIncome" ~ 
          paste0("$", format(as.integer(value), big.mark = ",")),
        TRUE ~ paste0(round(value*100, 1), "%")
        )
      )
  
  ggplot(x, aes(x = name, y = Scale, fill = Shade)) +
    geom_bar(stat = "identity") +
    labs(x = NULL, y = "%", title = unique(x$Neighborhood)) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, .2),
      labels = NULL
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
    guides(fill = "none") +
    theme(plot.title = element_text(hjust = .5)) + 
    geom_text(aes(label = Label, y = Textloc), size = 4) +
    scale_fill_gradient2(
      limits = c(0, 1), 
      high = cchmcdarkpurple, 
      low = cchmcdarkgreen,
      mid = cchmclightblue,
      midpoint = .5
    )
}

di_popup_muni <- lapply((muninums), function(k) {
  di_graph(k)
})

di_popup_hood <- lapply((hoodnums), function(k) {
  di_graph(k)
})

mobile <- oki_muni2 |>
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

mobile_rates <- hood_baselines(mobile) |>
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
    labs(x = NULL, y = "%", title = unique(x$Neighborhood)) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, .2),
      labels = seq(0, 100, 20)
    ) +
    scale_x_discrete(
      labels = c(
        "Same Home",
        "Same City",
        "Elsewhere\nin U.S.",
        "Other Country"
      )
    ) +
    theme_minimal() +
    guides(fill = "none") +
    theme(plot.title = element_text(hjust = .5)) + 
    geom_text(
      aes(label = paste0(round(value*100, 1), "%"), y = Textloc), 
      size = 4
    ) +
    scale_fill_gradient2(
      limits = c(-1, 1), 
      high = cchmcdarkpurple, 
      low = cchmcdarkgreen, 
      mid = cchmclightblue
    )
}

mobile_popup_muni <- lapply((muninums), function(k) {
  mobile_graph(k)
})

mobile_popup_hood <- lapply((hoodnums), function(k) {
  mobile_graph(k)
})

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
  
hood_lines <- hamco_bg |>
  filter(Municipality == "Cincinnati") |>
  group_by(Municipality, Neighborhood) |>
  summarise(geometry = st_union(geometry)) |>
  st_as_sf()

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
  FROM hpceclarity.bmi.registry_config c
    INNER JOIN hpceclarity.bmi.reg_data_hx_membership m
      ON c.registry_id = m.registry_id
    INNER JOIN hpceclarity.bmi.registry_data_info d
      ON m.record_id = d.record_id
    INNER JOIN hpceclarity.bmi.patient p
      ON p.pat_id = d.networked_id
  WHERE (c.registry_name LIKE '%asthma%' OR c.registry_id = '210652454')
    AND p.add_line_1 NOT LIKE '%222 E%'
    AND m.status_c = 1
                  ") |>
  mutate(
    asthma_admission = 0,
    t1d_registry_admission = 0,
    mental_health_admission = 0,
    county = str_to_title(county),
    contact_date = today()
  ) 

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
  FROM hpceclarity.bmi.readmissions r
		INNER JOIN hpceclarity.dbo.chmc_adt_addr_hx a
		  ON r.pat_id = a.pat_id
  WHERE r.hosp_admsn_time >= a.eff_start_date
		AND (a.eff_end_date IS NULL OR r.hosp_admsn_time < a.eff_end_date)
		AND year(r.hosp_admsn_time) = 2022
		AND a.state IN ('Ohio', 'Indiana', 'Kentucky')
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
    ),
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

adds <- select(registry, pat_id:county, contact_date) |>
  rbind(select(admits, pat_id:zip, county)) |>
  mutate(Age = as.numeric(contact_date-birth_date)/365.25) |>
  filter(
    Age <= 18,
    (state == "Ohio" & 
       county %in% c("Hamilton", "Butler", "Clermont", "Warren")) |
      (state == "Kentucky" & county %in% c("Boone", "Campbell", "Kenton")) |
      (state == "Indiana" & county == "Dearborn")
  ) |>
  unique()

adds_unique <- adds |>
  distinct(add_line_1, add_line_2, city, state, zip) |>
  mutate(AddID1 = row_number())

adds <- inner_join(adds, adds_unique)

adds_cleaned <- adds_unique |>
  mutate(
    Add1 = case_when(
      str_detect(add_line_1, "4933 Allen") ~ "4933 Allens Ridge Dr",
      str_detect(add_line_1, "4231 Sophia") ~ "4231 Sophias Way",
      str_detect(add_line_1, "5583 Jamie") ~ "5583 Jamies Oak Ct",
      str_detect(add_line_1, "8681 Harper") ~ "8681 Harpers Point Drive Apt A",
      TRUE ~ add_line_1
    ),
    Add1 = ifelse(str_starts(Add1, "#"), str_remove(Add1, "#"), Add1),
    Add1 = str_trim(str_to_upper(Add1)),
    First = str_trunc(Add1, 3, "right", ellipsis = ""),
    Add1 = ifelse(is.na(parse_number(First)), "", Add1),
    Add1 = ifelse(
      str_sub(Add1, 1, 1) %in% LETTERS, 
      str_sub(Add1, 2, str_length(Add1)), 
      Add1
    ),
    Add1 = ifelse(
      str_sub(Add1, 1, 1) %in% LETTERS, 
      str_sub(Add1, 2, str_length(Add1)), 
      Add1
    ),
    Add2 = str_trim(str_to_upper(add_line_2)),
    Add2 = coalesce(Add2, ""),
    Address = case_when(
      str_sub(Add1, 1, 1) %in% 1:9 ~ Add1,
      str_sub(Add2, 1, 1) %in% 1:9 ~ Add2,
      TRUE ~ NA
    ),
    City = str_to_title(city),
    Zip = str_trunc(zip, 5, "right", ellipsis = "")
  ) |>
  filter(Address != "") |>
  separate_wider_delim(
    Address, delim = "APT", names = "Address", too_many = "drop"
  ) |>
  separate_wider_delim(
    Address, delim = "#", names = "Address", too_many = "drop"
  ) |>
  separate_wider_delim(
    Address, delim = "UNIT", names = "Address", too_many = "drop"
  ) |>
  mutate(Address = str_trim(Address))

cleaned_unique <- adds_cleaned |>
  distinct(City, state, Zip, Address) |>
  mutate(AddID2 = row_number())

adds_cleaned <- inner_join(adds_cleaned, cleaned_unique)
adds <- left_join(adds, select(adds_cleaned, AddID1, AddID2))

for_geocoder <- cleaned_unique |>
  mutate(
    state = case_when(
      state == "Ohio" ~ "OH",
      state == "Kentucky" ~ "KY",
      TRUE ~ "IN"
    ),
    address = paste(Address, City, state, Zip, sep = ", ")
  ) |>
  select(AddID2, address) |>
  rename(ID = AddID2)
#write_csv(for_geocoder, "for_degauss.csv")

#cd "C:\Users\FLI6SH\OneDrive - cchmc\ACT_Neighborhood\Repository\neighborhood-repository"
#docker run --rm -v ${PWD}:/tmp ghcr.io/degauss-org/geocoder:3.0.2 for_degauss.csv

geocoded <- read.csv("for_degauss_geocoded_v3.0.2.csv") |>
  filter(
    precision == "range", 
    geocode_result != "imprecise_geocode"
  ) |>
  rename(AddID2 = ID) |>
  st_as_sf(coords = c("lon", "lat"), crs = 'NAD83', remove = FALSE) |>
  st_join(muni_lines, left = FALSE) |>
  as_tibble() 

geocoded2 <- geocoded |>
  distinct(AddID2, County, Municipality) |>
  group_by(AddID2) |>
  mutate(count = n())

muni_single <- filter(geocoded2, count == 1) |>
  select(-count)

muni_multi <- filter(geocoded2, count > 1) |>
  inner_join(adds, multiple = "all") |>
  filter(
    add_line_1 == "6901 Ken Arbre Dr" & Municipality == "Sycamore Township" |
      add_line_1 == "6500 Ridge Rd" & Municipality == "Columbia Township"
  ) |>
  select(AddID2, Municipality, County)

muni_add <- rbind(muni_single, muni_multi)

hooded1 <- filter(muni_add, Municipality != "Cincinnati") |>
  mutate(Neighborhood = NA) 
  
cinci <- filter(muni_add, Municipality == "Cincinnati") |>
  inner_join(geocoded) |>
  st_as_sf() |>
  st_join(hood_lines) |>
  group_by(AddID2) |>
  mutate(count = n()) |>
  rename(Municipality = Municipality.x)

hooded2 <- filter(cinci, count == 1) |>
  as_tibble() |>
  select(AddID2, County, Municipality, Neighborhood)

hooded_all <- filter(cinci, count > 1) |>
  filter(Neighborhood == "College Hill") |>
  as_tibble() |>
  select(AddID2, County, Municipality, Neighborhood) |>
  rbind(hooded1) |>
  rbind(hooded2) |>
  inner_join(select(adds, pat_id, add_line_1, AddID2), multiple = "all")

registry_totals <- registry |>
  mutate(Age = as.numeric(today()-birth_date)/365.25) |>
  filter(Age <= 18) |>
  inner_join(hooded_all, multiple = "all") |>
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

admit_totals <- inner_join(admits, hooded_all, multiple = "all") |>
  distinct(
    pat_id, 
    pat_enc_csn_id, 
    County, 
    Municipality, 
    Neighborhood,
    Asthma,
    Diabetes,
    MH
    ) |>
  group_by(County, Municipality, Neighborhood) |>
  summarise(
    Asthma_Admits = sum(Asthma),
    Asthma_Admitted = length(unique(pat_id[Asthma == 1])),
    Diabetes_Admits = sum(Diabetes),
    Diabetes_Admitted = length(unique(pat_id[Diabetes == 1])),
    MH_Admits = sum(MH),
    MH_Admitted = length(unique(pat_id[MH == 1]))
    )

hospital <- full_join(admit_totals, registry_totals) |>
  mutate(
    across(Asthma_Admits:MH_Admitted, \(x) coalesce(x, 0)),
    Neighborhood = coalesce(Neighborhood, Municipality)
    )

cinci_hospital <- filter(hospital, Municipality == "Cincinnati") |>
  group_by(County, Municipality) |>
  summarise(across(Asthma_Admits:Diabetes_Registry, sum)) |>
  mutate(Neighborhood = NA)

children <- filter(
  oki_muni2, 
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
  filter(Municipality != "Cincinnati" | !is.na(Neighborhood)) |>
  group_by(Condition, Measure) |>
  summarise(value = sum(value, na.rm = TRUE)) |>
  cross_join(
    filter(children, Municipality != "Cincinnati" | !is.na(Neighborhood)) |> 
      ungroup() |> 
      summarise(Children = sum(Children))
    ) |>
  mutate(Mean = 100*value/Children) |>
  select(Condition, Measure, Mean)
  
hospital_rates <- ungroup(hospital_children) |>
  filter(Municipality != "Cincinnati" | !is.na(Neighborhood)) |>
  group_by(Condition, Measure) |>
  summarise(
    Min = min(Rate, na.rm = TRUE),
    Max = max(Rate, na.rm = TRUE)
  ) |>
  inner_join(hospital_means)

hospital_all <- inner_join(hospital_children, hospital_rates) |>
  mutate(
    Scale = (Rate-Min)/Max,
    Textloc = ifelse(Rate > 100, Rate-3, Rate+3)
    ) |>
  inner_join(areas)

asthma_graph <- function(k){
  x <- filter(hospital_all, HoodID == k) |>
    filter(Condition == "Asthma") |>
    mutate(
      Measure = factor(
        Measure, 
        levels = c("Registry", "Admits", "Admitted")
      )
    )
  
  ggplot(x, aes(x = Measure, y = Rate, fill = Scale)) +
    geom_bar(stat = "identity", color = "black") +
    labs(x = NULL, y = "Per 100 children", title = unique(x$Neighborhood)) +
    scale_y_continuous(
      limits = c(0, 111),
      breaks = seq(0, 110, 10)
    ) +
    theme_minimal() +
    guides(fill = "none") +
    theme(plot.title = element_text(hjust = .5)) + 
    geom_text(aes(label = round(Rate, 1), y = Textloc), size = 4) +
    geom_text(
      aes(
        label = ifelse(value > 0, paste("n", value, sep = " = "), ""), 
        y = ifelse(Rate > 100, Textloc-5, Textloc+5)
        ),
      size = 4
    ) +
    scale_fill_gradient2(
      limits = c(0, 1), 
      high = cchmcdarkpurple, 
      low = cchmcdarkgreen, 
      mid = cchmclightblue,
      midpoint = .6
    ) +
    scale_x_discrete(labels = c("Registry", "Admits", "Patients\nAdmitted"))
}

asthma_popup_muni <- lapply((muninums), function(k) {
 asthma_graph(k)
})

asthma_popup_hood <- lapply((hoodnums), function(k) {
  asthma_graph(k)
})

diabetes_graph <- function(k){
  x <- filter(hospital_all, HoodID == k) |>
    filter(Condition == "Diabetes") |>
    mutate(
      Measure = factor(
        Measure, 
        levels = c("Registry", "Admits", "Admitted")
      )
    )
  
  ggplot(x, aes(x = Measure, y = Rate, fill = Scale)) +
    geom_bar(stat = "identity", color = "black") +
    labs(x = NULL, y = "Per 100 children", title = unique(x$Neighborhood)) +
    scale_y_continuous(
      limits = c(0, 7),
      breaks = 0:7
    ) +
    theme_minimal() +
    guides(fill = "none") +
    theme(plot.title = element_text(hjust = .5)) + 
    geom_text(aes(label = round(Rate, 2), y = Textloc-2.8), size = 4) +
    geom_text(
      aes(
        label = ifelse(value > 0, paste("n", value, sep = " = "), ""), 
        y = ifelse(Rate > 6, Textloc-3.5, Textloc-2.5)
      ),
      size = 4
    ) +
    scale_fill_gradient2(
      limits = c(-1, 1), 
      high = cchmcdarkpurple, 
      low = cchmcdarkgreen, 
      mid = cchmclightblue
    ) +
    scale_x_discrete(labels = c("Registry", "Admits", "Patients\nAdmitted"))
}

diabetes_popup_muni <- lapply((muninums), function(k) {
  diabetes_graph(k)
})

diabetes_popup_hood <- lapply((hoodnums), function(k) {
  diabetes_graph(k)
})

mh_graph <- function(k){
  x <- filter(hospital_all, HoodID == k) |>
    filter(Condition == "MH") |>
    mutate(Measure = factor(Measure, levels = c("Admits", "Admitted")))
  
  ggplot(x, aes(x = Measure, y = Rate, fill = Scale)) +
    geom_bar(stat = "identity", color = "black") +
    labs(x = NULL, y = "Per 100 children", title = unique(x$Neighborhood)) +
    scale_y_continuous(
      limits = c(0, 7),
      breaks = seq(0, 7, 1)
    ) +
    theme_minimal() +
    guides(fill = "none") +
    theme(plot.title = element_text(hjust = .5)) + 
    geom_text(aes(label = round(Rate, 2), y = Textloc-2.8), size = 4) +
    geom_text(
      aes(
        label = ifelse(value > 0, paste("n", value, sep = " = "), ""), 
        y = ifelse(Rate > 6, Textloc-3, Textloc-2.5)
      ),
      size = 4
    ) +
    scale_fill_gradient2(
      limits = c(-1, 1), 
      high = cchmcdarkpurple, 
      low = cchmcdarkgreen, 
      mid = cchmclightblue
    ) +
    scale_x_discrete(labels = c("Admissions", "Patients\nAdmitted"))
}

mh_popup_muni <- lapply((muninums), function(k) {
  mh_graph(k)
})

mh_popup_hood <- lapply((hoodnums), function(k) {
  mh_graph(k)
})

pharms <- dbGetQuery(con, "
  SELECT pharmacy_id
        ,pharmacy_name
        ,record_state_name
    FROM hpceclarity.bmi.rx_phr
    WHERE record_state_name is null or record_state_name = 'Active'
                     ") 

to_geocode <- pharms |>
  separate_wider_delim(
    pharmacy_name, 
    delim = "-", 
    names = c("Name", "name2", "name3"), 
    too_many = "merge",
    too_few = "align_start"
  ) |>
  separate_wider_delim(
    name3, 
    delim = "--", 
    names = c("name3", "name4", "name5"), 
    too_few = "align_start",
    too_many = "merge"
  ) |>
  mutate(
    name2 = ifelse(name2 == "", name3, name2),
    comma = str_detect(name2, ","),
    city = str_trim(ifelse(comma, name2, name3))
  ) |>
  separate_wider_delim(
    city,
    delim = "-",
    names = c("city", "city2", "city3", "city4", "city5"),
    too_few = "align_start"
  ) |>
  mutate(
    comma = str_detect(city, ","),
    city = str_trim(ifelse(comma, city, city2)),
    comma = str_detect(city, ","),
    city = case_when(
      comma ~ city,
      str_detect(city3, ",") ~ city3,
      str_detect(city4, ",") ~ city4,
      str_detect(city5, ",") ~ city5,
      TRUE ~ city
    )
  ) |>
  filter(str_detect(city, ",")) |>
  separate_wider_delim(
    city,
    delim = ",",
    names = c("City", "State"),
    too_many = "drop"
  ) |>
  mutate(State = str_trim(State)) |>
  filter(State %in% c("OH", "IN", "KY")) |>
  separate_wider_delim(
    name3,
    delim = " AT ",
    names = c("Address", "Address2"),
    too_few = "align_start",
    too_many = "merge"
  ) |>
  mutate(
    Address = str_trim(Address),
    num = parse_number(Address),
    num = as.character(num),
    added = str_starts(Address, num),
    Address = ifelse(added, Address, str_trim(name4)),
    num = as.character(parse_number(Address)),
    added = str_starts(Address, num),
    Address = ifelse(is.na(added), str_trim(city2), Address),
    num = as.character(parse_number(Address)),
    added = str_starts(Address, num),
    Address = ifelse(added, Address, str_trim(city3)),
    num = as.character(parse_number(Address)),
    added = str_starts(Address, num),
    Type = "Pharmacy",
    Zip = NA
  ) |>
  filter(added, !is.na(added)) |>
  select(Name, Address, City, State, Type, Zip) |>
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
  geocode(
    street = Address,
    city = City,
    state = State,
    postalcode = Zip,
    method = "geoapify"
  )

points_all <- undone |>
  rename(
    lat = lat...9,
    long = long...10
    ) |>
  select(Name, Type, lat, long, Address) |>
  rbind(select(geocoded_points, Name, lat, long, Type, Address)) |>
  filter(!is.na(lat))
  
points_all2 <- points_all |>
  st_as_sf(
    coords = c("long", "lat"),
    crs = "NAD83"
  ) |>
  inner_join(points_all, multiple = "all") |>
  unique()

points <- muni_lines |>
  st_join(points_all2, left = FALSE)

cinci_points <- filter(
  points, 
  Municipality %in% c("Cincinnati", "Norwood", "St. Bernard", "Elmwood Place") | 
    Type == "School"
  )

cinci_map_lines <- hamco_bg |>
  mutate(Allocation = ifelse(Neighborhood == "California", 1, Allocation)) |>
  group_by(GEOID) |>
  filter(Allocation == max(Allocation)) |>
  ungroup() |>
  filter(
    Municipality %in% c("Cincinnati", "Norwood", "St. Bernard", "Elmwood Place")
    ) |>
  group_by(Neighborhood) |>
  summarise(geometry = st_union(geometry)) |>
  inner_join(di_rates |> 
               mutate(Neighborhood = coalesce(Neighborhood, Municipality))
             ) |>
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
  ) 

pal <- colorFactor(
  c(
    cchmcdarkgreen, 
    cchmclightgreen, 
    cchmclightblue, 
    cchmclightpurple, 
    cchmcdarkpurple
    ), 
  domain = cinci_map_lines$Tier
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
      "Asthma",
      "Diabetes",
      "Mental health",
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
    popup = popupGraph(asthma_popup_hood, height = 300, width = 700),
    group = "Asthma"
  ) |>
  addCircleMarkers(
    data = cinci_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(diabetes_popup_hood, height = 300, width = 700),
    group = "Diabetes"
  ) |>
  addCircleMarkers(
    data = cinci_map_lines$Centroid,
    color = cchmcpink,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(mh_popup_hood, height = 300, width = 700),
    group = "Mental health"
  ) |>
  addMarkers(
    data = cinci_points,
    ~long,
    ~lat,
    icon = muni_icons,
    popup = ~Name,
    group = "Schools and pharmacies"
  ) 

saveWidget(city_map, "city map.html")


