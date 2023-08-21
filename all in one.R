library(DBI)
library(htmlwidgets)
library(leaflegend)
library(leaflet)
library(leafpop)
library(lubridate)
library(odbc)
library(readxl)
library(sf)
library(tidycensus)
library(tidygeocoder)
library(tidyverse)
library(tigris)
library(sf)
options(tigris_use_cache = TRUE)

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
    "Families",
    "Poverty",
    "MedianHHIncome",
    "Assistance",
    "HousingUnits",
    "Vacant",
    rep("Uninsured", 4)
  )
)

hamco_bg <- get_acs(
  geography = "block group",
  variables = bg_var_list,
  county = "Hamilton",
  state = "OH",
  geometry = TRUE
  ) |>
  inner_join(cat_frame)

hamco_bg2 <- hamco_bg |>
  as_tibble() |>
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

hood_inc <- hamco_bg2 |>
  filter(
    Municipality == "Cincinnati",
    !is.na(Income)
  ) |>
  group_by(Neighborhood) |>
  summarise(
    Income = sum(Income),
    IncomeHH = sum(Households)
  )

cinci_hood <- filter(hamco_bg2, Municipality == "Cincinnati") |>
  ungroup() |>
  select(Age25:LimitedEnglish, Population:White, Municipality, Neighborhood) |>
  group_by(Municipality, Neighborhood) |>
  summarise(across(Age25:White, sum)) |>
  left_join(hood_inc)

hh_tract <- filter(hamco_bg2, Municipality == "Cincinnati") |>
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
  summarise(Assistance = sum(Assistance))

cinci_hood <- inner_join(cinci_hood, hood_assistance) |>
  mutate(
    County = "Hamilton",
    State = "Ohio",
    across(Age25:Assistance, round)
  ) 

oh_cs <- get_acs(
  geography = "county subdivision",
  variables = c(bg_var_list, "B19058_002"),
  state = "OH",
  county = c("Clermont", "Warren", "Butler", "Hamilton"),
  year = 2021,
  geometry = TRUE
  )

ky_cs <- get_acs(
  geography = "county subdivision",
  variables = c(bg_var_list, "B19058_002"),
  state = "KY",
  county = c("Boone", "Campbell", "Kenton"),
  year = 2021,
  geometry = TRUE
  )

in_cs <- get_acs(
  geography = "county subdivision",
  variables = c(bg_var_list, "B19058_002"),
  state = "IN",
  county = "Dearborn",
  year = 2021,
  geometry = TRUE
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

oki_no_income <- filter(oki_cs, category != "MedianHHIncome") |>
  as_tibble() |>
  group_by(Municipality, County, State, category) |>
  summarise(estimate = sum(estimate)) |>
  pivot_wider(
    id_cols = c(Municipality, State, County),
    names_from = category,
    values_from = estimate
  ) |>
  mutate(
    across(c(Age25:LimitedEnglish, Population:White), \(x) coalesce(x, 0)),
    Neighborhood = NA
  )

oki_income <- filter(oki_cs, category %in% c("MedianHHIncome", "Households")) |>
  pivot_wider(
    id_cols = c(GEOID, Municipality, County, State),
    names_from = category,
    values_from = estimate
  ) |>
  mutate(Income = MedianHHIncome*Households) |>
  group_by(Municipality, County, State) |>
  summarise(
    Income = sum(Income),
    IncomeHH = sum(Households)
  ) |>
  inner_join(oki_no_income)

oh_county <- get_acs(
  geography = "county",
  variables = c(bg_var_list, "B19058_002"),
  state = "OH",
  county = c("Clermont", "Warren", "Butler", "Hamilton"),
  year = 2021,
  geometry = TRUE
  )

ky_county <- get_acs(
  geography = "county",
  variables = c(bg_var_list, "B19058_002"),
  state = "KY",
  county = c("Campbell", "Kenton", "Boone"),
  year = 2021,
  geometry = TRUE
  )

in_county <- get_acs(
  geography = "county",
  variables = c(bg_var_list, "B19058_002"),
  state = "IN",
  county = "Dearborn",
  year = 2021,
  geometry = TRUE
  )

oki_county <- rbind(oh_county, ky_county) |>
  rbind(in_county) |>
  inner_join(cat_frame) |>
  separate_wider_delim(
    cols = NAME, 
    delim = ", ", 
    names = c("County", "State")
  ) |>
  mutate(County = str_remove(County, " County"))

oki_county2 <- oki_county |>
  as_tibble() |>
  group_by(County, State, category) |>
  summarise(estimate = sum(estimate)) |>
  pivot_wider(
    id_cols = c(County, State), 
    names_from = category, 
    values_from = estimate
  ) |>
  mutate(
    Income = MedianHHIncome*Households,
    IncomeHH = Households,
    Municipality = NA,
    Neighborhood = NA
  ) |>
  select(-MedianHHIncome)

metro <- oki_county2 |>
  ungroup() |>
  summarise(across(Age25:IncomeHH, sum)) |>
  mutate(
    County = "All",
    State = "All",
    Municipality = NA,
    Neighborhood = NA
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

hood_di <- filter(hamco_bg2, Municipality == "Cincinnati") |>
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
  summarise(DeprivationIndex = sum(DIWeight)) |>
  mutate(Neighborhood = NA)

county_di <- select(oki_income, Municipality, County, State, Population) |>
  inner_join(cs_di) |>
  mutate(DIWeight = DeprivationIndex*Population) |>
  group_by(State, County) |>
  summarise(
    DIWeight = sum(DIWeight),
    Population = sum(Population)
  ) |>
  mutate(
    DeprivationIndex = DIWeight/Population,
    DIWeight = DeprivationIndex*Population,
    Municipality = NA,
    Neighborhood = NA
  ) |>
  select(-DIWeight)

metro_di <- county_di |>
  mutate(DIWeight = DeprivationIndex*Population) |>
  ungroup() |>
  summarise(
    Population = sum(Population),
    DIWeight = sum(DIWeight)
  ) |>
  mutate(
    DeprivationIndex = DIWeight/Population,
    State = "All",
    County = "All",
    Municipality = NA,
    Neighborhood = NA
  ) |>
  select(DeprivationIndex:Neighborhood)

di_all <- rbind(hood_di, cs_di) |>
  rbind(select(county_di, -Population)) |>
  rbind(metro_di)

census <- rbind(cinci_hood, oki_income) |>
  rbind(oki_county2) |>
  ungroup() |>
  rbind(metro) |>
  inner_join(di_all) |>
  mutate(
    AssistanceRate = Assistance/Households,
    BlackRate = Black/Population,
    ChildRate = Children/Population,
    OtherLanguage = Age5-English,
    OtherLanguageRate = OtherLanguage/Age5,
    ChildHHRate = HHWithChildren/Households,
    HighSchoolRate = HighSchool/Age25,
    HispanicRate = Hispanic/Population,
    LimitedEnglishRate = LimitedEnglish/Age5,
    PovertyRate = Poverty/Families,
    UninsuredRate = Uninsured/Population,
    VacancyRate = Vacant/HousingUnits,
    WhiteRate = White/Population,
    MedianHHIncome = Income/IncomeHH
  ) |>
  select(
    State, County, Municipality, Neighborhood,
    Population, White, WhiteRate, Black, BlackRate, Hispanic, HispanicRate,
    Children, ChildRate, Uninsured, UninsuredRate,
    Households, HHWithChildren, ChildHHRate, Assistance, AssistanceRate, 
    MedianHHIncome,
    Age25, HighSchool, HighSchoolRate,
    Families, Poverty, PovertyRate,
    HousingUnits, Vacant, VacancyRate,
    Age5, OtherLanguage, OtherLanguageRate, LimitedEnglish, LimitedEnglishRate
  ) |>
  full_join(di_all)

con <- dbConnect(odbc::odbc(), "ClarityProd")

asthma_reg <- dbGetQuery(con, "
  SELECT DISTINCT p.pat_id
                ,CAST(p.birth_date AS DATE) AS birth_date
				        ,p.add_line_1
				        ,p.add_line_2
				        ,p.city
				        ,p.state
				        ,p.zip
				        ,p.county
  FROM hpceclarity.bmi.registry_config c
    INNER JOIN hpceclarity.bmi.reg_data_hx_membership m
      ON c.registry_id = m.registry_id
    INNER JOIN hpceclarity.bmi.registry_data_info d
      ON m.record_id = d.record_id
    INNER JOIN hpceclarity.bmi.patient p
      ON p.pat_id = d.networked_id
  WHERE c.registry_name like '%asthma%'
    AND p.add_line_1 not like '%222 E%'
    AND m.status_c = 1
                  ") |>
  mutate(
    Registry = 1,
    Admission = 0,
    Age = as.numeric(today()-birth_date)/385.25
  ) |>
  filter(Age < 18)

asthma_admits <- dbGetQuery(con, "
  SELECT pat_id
        ,pat_addr_1 as add_line_1
        ,pat_addr_2 as add_line_2
        ,pat_city as city
        ,pat_state as state
        ,pat_zip as zip
        ,countyfp
    FROM temptable.dbo.health_equity_network_encounters
    WHERE year(hosp_admsn_time) = 2022
      AND asthma_admission = 1
      AND add_line_1 not like '%222 E%'
                            ") |>
  mutate(
    Admission = 1,
    Registry = 0,
    county = case_when(
      state == "Ohio" ~ case_when(
        countyfp == "061" ~ "Hamilton",
        countyfp == "017" ~ "Butler",
        countyfp == "025" ~ "Clermont",
        countyfp == "165" ~ "Warren",
        TRUE ~ NA
      ),
      state == "Kentucky" ~ case_when(
        countyfp == "015" ~ "Boone",
        countyfp == "037"~ "Campbell",
        countyfp == "117" ~ "Kenton",
        TRUE ~ NA
      ),
      state == "Indiana" ~ ifelse(countyfp == "029", "Dearborn", NA),
      TRUE ~ NA
    )
  )

adds <- asthma_admits |>
  filter(
    !is.na(county) | 
      (state %in% c("Ohio", "Indiana", "Kentucky") & is.na(countyfp))
  ) |>
  select(-countyfp) |>
  rbind(
    asthma_reg |>
      select(-c(birth_date, Age)) |>
      filter(
        (state == "Ohio" & 
           county %in% c("HAMILTON", "CLERMONT", "BUTLER", "WARREN")) |
          (state == "Kentucky" & county %in% c("BOONE", "CAMPBELL", "KENTON")) |
          (state == "Indiana" & county == "DEARBORN") |
          (state %in% c("Ohio", "Indiana", "Kentucky") & is.na(county))
      )
  ) |>
  mutate(
    City = str_to_upper(city),
    Zip = str_trunc(zip, 5, "right", ellipsis = ""),
    Address = coalesce(add_line_1, add_line_2),
    Address = str_to_upper(Address),
  ) |>
  separate_wider_delim(
    Address, delim = "APT", names = "Address", too_many = "drop"
  ) |>
  separate_wider_delim(
    Address, delim = "#", names = "Address", too_many = "drop"
  ) |>
  separate_wider_delim(
    Address, delim = "UNIT", names = "Address", too_many = "drop"
  )

unique_adds <- adds |>
  distinct(City, state, Zip, Address) |>
  mutate(ID = row_number())

for_geocoder <- unique_adds |>
  mutate(
    state = case_when(
      state == "Ohio" ~ "OH",
      state == "Kentucky" ~ "KY",
      TRUE ~ "IN"
    ),
    address = paste(Address, City, state, Zip, sep = ", ")
  ) |>
  select(ID, address)
#write_csv(for_geocoder, "for_degauss.csv")

#cd "C:\Users\FLI6SH\OneDrive - cchmc\ACT_Neighborhood\Repository\neighborhood-repository
#docker run --rm -v ${PWD}:/tmp ghcr.io/degauss-org/geocoder:3.0.2 for_degauss.csv

geocoded <- read.csv("for_degauss_geocoded_v3.0.2.csv") |>
  filter(
    precision == "range", 
    geocode_result != "imprecise_geocode"
    ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 'NAD83', remove = FALSE)

oki_munis <- oki_cs |>
  distinct(Municipality, County, State, geometry) |>
  st_as_sf() |>
  st_join(geocoded) |>
  group_by(ID) |>
  mutate(count = n())

muni_singles <- filter(oki_munis, count == 1)
cinci1 <- filter(muni_singles, Municipality == "Cincinnati") |>
  as_tibble() |>
  select(ID) |>
  inner_join(geocoded) |>
  select(ID, geometry) |>
  st_as_sf()

hooded1 <- filter(muni_singles, Municipality != "Cincinnati") |>
  as_tibble() |>
  select(ID, State, County, Municipality) |>
  mutate(Neighborhood = NA)

cinci_bg <- st_join(cinci1, distinct(hamco_bg, GEOID, geometry)) |>
  inner_join(filter(allocations, Municipality == "Cincinnati")) |>
  distinct(ID, Municipality, Neighborhood) |>
  group_by(ID) |>
  mutate(count = n()) |>
  arrange(-count, ID)

hooded2 <- filter(cinci_bg, count == 1) |>
  select(-count) |>
  mutate(
    County = "Hamilton",
    State = "Ohio"
  )

asthma <- rbind(hooded1, hooded2) |>
  inner_join(unique_adds) |>
  inner_join(adds, multiple = "all") |>
  group_by(State, County, Municipality, Neighborhood) |>
  summarise(
    AsthmaRegistry = sum(Registry),
    AsthmaAdmissions = sum(Admission)
  )

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
  right_join(census) |>
  mutate(
    across(AsthmaRegistry:AsthmaAdmissions, \(x) coalesce(x, 0)),
    AsthmaRegistryRate = AsthmaRegistry/Children,
    AsthmaRegistryRate = ifelse(
      is.infinite(AsthmaRegistryRate), 
      NA, 
      AsthmaRegistryRate
    ),
    AsthmaAdmissionRate = AsthmaAdmissions/Children,
    AsthmaAdmissionRate = ifelse(
      is.nan(AsthmaAdmissionRate), 
      NA, 
      AsthmaAdmissionRate
    ),
    AsthmaRegistryRate = coalesce(AsthmaRegistryRate, 0),
    AsthmaAdmissionRate = coalesce(AsthmaAdmissionRate, 0),
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
    Level = case_when(
      Neighborhood != "All" ~ "Neighborhood",
      Municipality != "All" ~ "Municipality",
      County != "All" ~ "County",
      TRUE ~ "All"
    ),
    Label = case_when(
      Neighborhood != "All" ~ Neighborhood,
      Municipality != "All" ~ Municipality,
      TRUE ~ County
    )
  )

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

points <- oki_cs |>
  st_as_sf() |>
  distinct(Municipality, County, State, geometry) |>
  st_join(points_all2, left = FALSE)

cinci_points <- filter(
  points, 
  Municipality %in% c("Cincinnati", "Norwood") | Type == "School"
  )

benchmarks <- all_data |>
  ungroup() |>
  select(
    County:Neighborhood, 
    ends_with("Rate"), 
    Level, 
    MedianHHIncome, 
    DeprivationIndex
  )

bench_min <- benchmarks |>
  filter(Level != "All") |>
  group_by(Level) |>
  summarise(across(WhiteRate:DeprivationIndex, \(x) min(x, na.rm = TRUE))) |>
  pivot_longer(
    cols = WhiteRate:DeprivationIndex,
    names_to = "variable",
    values_to = "Minimum"
  )

bench_max <- benchmarks |>
  filter(Level != "All") |>
  group_by(Level) |>
  summarise(across(WhiteRate:DeprivationIndex, \(x) max(x, na.rm = TRUE))) |>
  pivot_longer(
    cols = WhiteRate:DeprivationIndex,
    names_to = "variable",
    values_to = "Maximum"
  )

bench_mean <- benchmarks |>
  ungroup() |>
  filter(
    County == "All" | (Municipality == "Cincinnati" & is.na(Neighborhood))
  ) |>
  mutate(Level = ifelse(Level == "Municipality", "Neighborhood", "County")) |>
  select(WhiteRate:DeprivationIndex) |>
  pivot_longer(
    cols = c(WhiteRate:AsthmaAdmissionRate, MedianHHIncome, DeprivationIndex),
    names_to = "variable",
    values_to = "Mean"
  )

benchmarks_all <- filter(bench_mean, Level == "County") |>
  mutate(Level = "Municipality") |>
  rbind(bench_mean) |>
  inner_join(bench_max) |>
  inner_join(bench_min)

rates <- filter(all_data, County != "All") |>
  select(
    Level,
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
  inner_join(benchmarks_all) |>
  mutate(
    Shade = ifelse(
      value > Mean, 
      (value-Mean)/(Maximum-Mean), 
      (value-Mean)/(Mean-Minimum)
    ),
    Shade = ifelse(is.infinite(Shade), NA, Shade),
    Scale = ifelse(
      variable %in% c(
        "WhiteRate", 
        "BlackRate", 
        "HispanicRate", 
        "ChildRate", 
        "ChildHHRate",
        "OtherLanguageRate",
        "LimitedEnglishRate",
        "AsthmaRegistryRate",
        "AsthmaAdmissionRate"
      ), 
      value,
      (value-Minimum)/(Maximum-Minimum)
    ),
    Textloc = ifelse(Scale > .9, Scale - .025, Scale + .04)
  )

inner_subs <- filter(
  rates, 
  Municipality %in% c("Norwood", "St. Bernard", "Elmwood Place")
  ) |>
  select(County:value) |>
  mutate(
    Level = "Neighborhood",
    Neighborhood = Municipality
  ) |>
  inner_join(benchmarks_all) |>
  mutate(
    Shade = ifelse(
      value > Mean, 
      (value-Mean)/(Maximum-Mean), 
      (value-Mean)/(Mean-Minimum)
    ),
    Scale = ifelse(
      variable %in% c(
        "WhiteRate", 
        "BlackRate", 
        "HispanicRate", 
        "ChildRate", 
        "ChildHHRate",
        "OtherLanguageRate",
        "LimitedEnglishRate",
        "AsthmaRegistryRate",
        "AsthmaAdmissionRate"
      ),
      value,
      (value-Minimum)/(Maximum-Minimum)
    ),
    Textloc = ifelse(Scale > .9, Scale - .025, Scale + .04)
  )

rates <- rbind(rates, inner_subs) |>
  mutate(variable = str_remove(variable, "Rate"))

areas <- distinct(rates, Level, County, Municipality, Neighborhood) |>
  mutate(
    Label = case_when(
      !is.na(Neighborhood) ~ Neighborhood,
      !is.na(Municipality) ~ Municipality,
      TRUE ~ County
    )
  ) |>
  inner_join(select(all_data, County, Label, Tier)) |>
  mutate(
    Label = ifelse(
      is.na(Neighborhood) & is.na(Municipality),
      paste(Label, "County", sep = " "),
      Label
    )
  ) |>
  arrange(Level, Label) |>
  ungroup() |>
  mutate(geoid = row_number())

hoods <- areas$geoid[areas$Level == "Neighborhood"]
munis <- areas$geoid[areas$Level == "Municipality"]
counties <- areas$geoid[areas$Level == "County"]

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
      )
    )
  
  y <- ggplot(x, aes(x = variable, y = Scale, fill = Shade)) +
    geom_bar(stat = "identity", color = "black") +
    labs(x = NULL, y = "%", title = unique(x$Label)) +
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
        "Children",
        "Households\nwith Children",
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
      high = "#83286B", 
      low = "#76BC44", 
      mid = "#9BD3DD"
    )
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
        TRUE ~ paste0(as.character(round(value*100, 1)), "%")
      ),
      Shade = ifelse(
        variable %in% c("MedianHHIncome", "HighSchool"), 
        Shade * -1, 
        Shade
      )
    )
  
  y <- ggplot(x, aes(x = variable, y = Scale, fill = Shade)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient2(
      limits = c(-1, 1), 
      high = "#83286B", 
      low = "#76BC44", 
      mid = "#9BD3DD"
    ) +
    labs(x = NULL, y = NULL, title = unique(x$Label), fill = NULL) +
    scale_y_continuous(labels = NULL, limits = c(0, 1)) +
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
    guides(fill = "none") + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5)) +
    geom_text(aes(label = label, y = Textloc), size = 4)
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
    inner_join(
      select(
        all_data, 
        County, 
        Municipality, 
        AsthmaRegistry, 
        AsthmaAdmissions, 
        Children, 
        Label
      )
    ) |>
    mutate(
      variable = factor(
        variable, 
        levels = c("AsthmaRegistry", "AsthmaAdmission")
      ),
      Scale = ifelse(Scale > .6, .6, Scale),
      Textloc = ifelse(Textloc > .6, Textloc-.6, Textloc)
    )
  
  y <- ggplot(x, aes(x = variable, y = Scale, fill = Shade)) +
    geom_bar(stat = "identity") +
    labs(x = NULL, y = "Per 100 children", title = unique(x$Label), fill = NULL) +
    scale_y_continuous(
      limits = c(0, .63), 
      breaks = seq(0, .6, .1), 
      labels = seq(0, 60, 10)
    ) +
    scale_x_discrete(labels = c("Asthma\n Registry", "Asthma\nAdmissions")) +
    scale_fill_gradient2(
      limits = c(-1, 1), 
      high = "#83286B", 
      low = "#76BC44", 
      mid = "#9BD3DD"
    ) +
    guides(fill = "none") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5)) +
    geom_text(aes(label = round(value*100, 1), y = Textloc), size = 4) +
    annotate(
      "text", 
      x = 1, 
      y = x$Textloc[x$variable == "AsthmaRegistry"] + .03,
      label = paste0("n = ", mean(x$AsthmaRegistry))
    ) +
    annotate(
      "text", 
      x = 2, 
      y = x$Textloc[x$variable == "AsthmaAdmission"] + .03,
      label = paste0("n = ", mean(x$AsthmaAdmissions))
    ) +
    annotate(
      "text",
      x = 1.5,
      y = .6,
      label = paste0("Children = ", format(mean(x$Children), big.mark = ","))
    )
  y
  return(y)
}

hood_health_popups <- lapply(hoods, function(k){
  health_graph(i = k)
})

muni_health_popups <- lapply(munis, function(k){
  health_graph(i = k)
})

county_health_popups <- lapply(counties, function(k){
  health_graph(i = k)
})

hood_lines <- hamco_bg |>
  inner_join(allocations, multiple = "all") |>
  filter(
    Municipality %in% c("Cincinnati", "Norwood", "St. Bernard", "Elmwood Place")
  ) |>
  group_by(Neighborhood) |>
  summarise(geometry = st_union(geometry)) |>
  #rename(Area = Neighborhood) |>
  mutate(
    State = "Ohio",
    County = "Hamilton",
    Centroid = st_centroid(geometry),
    Level = "Neighborhood"
  ) |>
  inner_join(areas) |>
  arrange(geoid)

pal <- colorFactor(
  c("#76BC44", "#A1CA3C", "#9BD3DD", "#CA5699", "#83286B"), 
  domain = all_data$Tier
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

muni_icons <- icons(
  iconUrl = ifelse(
    points$Type == "School", 
    "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
    "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-yellow.png"
  ),
  iconWidth = 20,
  iconHeight = 30
)

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
      "Health indicators",
      "Schools and Pharmacies"
    ),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addCircleMarkers(
    data = hood_lines$Centroid,
    color = "#E64479",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(hood_demo_popups, height = 300, width = 700),
    group = "Demographics"
  ) |>
  addCircleMarkers(
    data = hood_lines$Centroid,
    color = "#E64479",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(hood_di_popups, height = 300, width = 700),
    group = "Deprivation indicators"
  ) |>
  addCircleMarkers(
    data = hood_lines$Centroid,
    color = "#E64479",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(hood_health_popups, height = 300, width = 700),
    group = "Health indicators"
  ) |>
  addMarkers(
    data = cinci_points,
    ~long,
    ~lat,
    icon = cinci_icons,
    popup = ~Name,
    group = "Schools and Pharmacies"
  ) 

#saveWidget(citymap, "city map.html")

muni_lines <- oki_cs |>
  distinct(Municipality, County, State, geometry)|>
  st_as_sf() |>
  group_by(Municipality, County, State) |>
  summarise(geometry = st_union(geometry)) |>
  mutate(
    Centroid = st_centroid(geometry),
    Neighborhood = "All"
  ) |>
  inner_join(areas |> mutate(Neighborhood = coalesce(Neighborhood, "All"))) |>
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
      "Health indicators",
      "Schools and Pharmacies"
    ),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addCircleMarkers(
    data = muni_lines$Centroid,
    color = "#E64479",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(muni_demo_popups, height = 300, width = 700),
    group = "Demographics"
  ) |>
  addCircleMarkers(
    data = muni_lines$Centroid,
    color = "#E64479",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(muni_di_popups, height = 300, width = 700),
    group = "Deprivation indicators"
  ) |>
  addCircleMarkers(
    data = muni_lines$Centroid,
    color = "#E64479",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(muni_health_popups, height = 300, width = 700),
    group = "Health indicators"
  ) |>
  addMarkers(
    data = points,
    ~long,
    ~lat,
    icon = muni_icons,
    popup = ~Name,
    group = "Schools and Pharmacies"
  ) 

#saveWidget(muni_map, "muni map.html")

county_lines <- oki_county |>
  distinct(County, State, geometry) |>
  st_as_sf() |>
  mutate(Centroid = st_centroid(geometry)) |> 
  inner_join(filter(areas, Level == "County")) |>
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
      "Health indicators",
      "Schools and Pharmacies"
    ),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addCircleMarkers(
    data = county_lines$Centroid,
    color = "#E64479",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(county_demo_popups, height = 300, width = 700),
    group = "Demographics"
  ) |>
  addCircleMarkers(
    data = county_lines$Centroid,
    color = "#E64479",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(county_di_popups, height = 300, width = 700),
    group = "Deprivation indicators"
  ) |>
  addCircleMarkers(
    data = county_lines$Centroid,
    color = "#E64479",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(county_health_popups, height = 300, width = 700),
    group = "Health indicators"
  ) |>
  addMarkers(
    data = points,
    ~long,
    ~lat,
    icon = muni_icons,
    popup = ~Name,
    group = "Schools and Pharmacies"
  ) 

#saveWidget(county_map, "county map.html")


