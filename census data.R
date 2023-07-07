library(tidyverse)
library(tidycensus)

##Demographic variables
demo_var_list <- c(
  paste0("B01001_00", c(1, 3:6)),
  paste0("B01001_0", 27:30),
  paste0("B02001_00", 2:3),
  "B03002_012",
  "B03002_013",
  "B11005_001",
  "B11005_002",
  "B19049_001",
  paste0("C16002_00", 2:9),
  paste0("C16002_0", 10:14)
)

## Variables needed for deprivation index
di_vars <- c(
  "B01001_001", 
  "B17001_001",
  "B17001_002", 
  "B19013_001", 
  paste0('B15003_0',17:25),
  "B15003_001",
  paste0('B27010_0',c(17, 33, 50, 66)),
  "B19058_001",
  "B19058_002",
  "B25001_001",
  "B25002_003"
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

vars <- load_variables(2021, "acs5")

oh_cs <- get_acs(
  geography = "county subdivision",
  variables = unique(c(demo_var_list, di_vars)),
  state = "OH",
  county = c("Clermont", "Warren", "Butler", "Hamilton"),
  year = 2021
  )

ky_cs <- get_acs(
  geography = "county subdivision",
  variables = unique(c(demo_var_list, di_vars)),
  state = "KY",
  county = c("Boone", "Campbell", "Kenton"),
  year = 2021
  )

in_cs <- get_acs(
  geography = "county subdivision",
  variables = unique(c(demo_var_list, di_vars)),
  state = "IN",
  county = "Dearborn",
  year = 2021
  )

oki_cs <- rbind(oh_cs, ky_cs) |>
  rbind(in_cs) |>
  mutate(
    Category = case_when(
      variable == "B01001_001" ~ "Total",
      str_detect(variable, "B01001_0") ~ "Children",
      variable == "B02001_002" ~ "White",
      variable == "B02001_003" ~ "Black",
      variable == "B03002_012" ~ "Hispanic",
      variable == "B03002_013" ~ "WhiteHispanic",
      variable == "B11005_001" ~ "Households",
      variable == "B11005_002" ~ "ChildHH",
      variable == "B19049_001" ~ "MedianHHIncome", 
      variable == "C16002_002" ~ "English",
      variable == "C16002_003" ~ "Spanish",
      variable == "C16002_004" ~ "SpanishLtd",
      variable == "C16002_005" ~ "SpanishUL",
      variable == "C16002_006" ~ "IE",
      variable == "C16002_007" ~ "IELtd",
      variable == "C16002_008" ~ "IEUL",
      variable == "C16002_009" ~ "Asian",
      variable == "C16002_010" ~ "AsianLtd",
      variable == "C16002_011" ~ "AsianUL",
      variable == "C16002_012" ~ "Other",
      variable == "C16002_013" ~ "OtherLtd",
      variable == "C16002_014" ~ "OtherUL",
      variable == "B17001_001" ~ "PovertyPop",
      variable == "B17001_002" ~ "Poverty",
      variable == "B19013_001" ~ "MedianHHIncome",
      variable == "B15003_001" ~ "AtLeast25",
      str_detect(variable, "B15003_0") ~ "HighSchool",
      str_detect(variable, "B27010_0") ~ "Uninsured",
      variable == "B19058_001" ~ "Households",
      variable == "B19058_002" ~ "PublicAssistance",
      variable == "B25001_001" ~ "HousingUnits",
      variable == "B25002_003" ~ "Vacant",
      TRUE ~ NA
    )
  ) |>
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

cs_income <- filter(oki_cs, Category %in% c("MedianHHIncome", "Households")) |>
  distinct(GEOID, Municipality, County, State, estimate, Category) |>
  pivot_wider(
    id_cols = c(GEOID, County, State, Municipality),
    names_from = Category,
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
  select(Municipality, County, State, MedianHHIncome)
  
cs_demo <- filter(oki_cs, Category != "MedianHHIncome") |>
  distinct(GEOID, Municipality, County, State, estimate, Category) |>
  group_by(Municipality, County, State, Category) |>
  summarise(estimate = sum(estimate)) |>
  pivot_wider(
    id_cols = c(Municipality, County, State),
    names_from = Category,
    values_from = estimate,
    values_fill = 0
  ) |>
  mutate(
    NonEnglishHH = (Households-English)/Households,
    LimitedEnglishHH = (SpanishLtd+AsianLtd+IELtd+OtherLtd)/Households,
    HighSchoolRate = HighSchool/AtLeast25,
    BlackRate = Black/Total,
    ChildHHRate = ChildHH/Households,
    ChildRate = Children/Total,
    HispanicRate = Hispanic/Total,
    VacancyRate = Vacant/HousingUnits,
    PovertyRate = Poverty/PovertyPop,
    PublicAssistanceRate = PublicAssistance/Households,
    UninsuredRate = Uninsured/Total,
    NonHispanicWhiteRate = (White-WhiteHispanic)/Total
    ) |>
  left_join(cs_income) |>
  select(Municipality, State, County, NonEnglishHH:MedianHHIncome, Total)

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
  summarise(DeprivationIndex = sum(DIWeight)) |>
  full_join(cs_demo) |>
  mutate(Neighborhood = NA)
  
hamco_bg <- get_acs(
  geography = "block group",
  variables = unique(c(demo_var_list), di_vars),
  state = "OH",
  county = "Hamilton",
  year = 2021
  ) |>
  mutate(
    Description = case_when(
      variable == "B01001_001" ~ "Total",
      str_detect(variable, "B01001_0") ~ "Children",
      variable == "B02001_002" ~ "White",
      variable == "B02001_003" ~ "Black",
      variable == "B03002_012" ~ "Hispanic",
      variable == "B03002_013" ~ "WhiteHispanic",
      variable == "B11005_001" ~ "Households",
      variable == "B11005_002" ~ "ChildHH",
      variable == "B19049_001" ~ "MedianHHIncome", 
      variable == "C16002_002" ~ "English",
      variable == "C16002_003" ~ "Spanish",
      variable == "C16002_004" ~ "SpanishLtd",
      variable == "C16002_005" ~ "SpanishUL",
      variable == "C16002_006" ~ "IE",
      variable == "C16002_007" ~ "IELtd",
      variable == "C16002_008" ~ "IEUL",
      variable == "C16002_009" ~ "Asian",
      variable == "C16002_010" ~ "AsianLtd",
      variable == "C16002_011" ~ "AsianUL",
      variable == "C16002_012" ~ "Other",
      variable == "C16002_013" ~ "OtherLtd",
      variable == "C16002_014" ~ "OtherUL",
      variable == "B17001_001" ~ "PovertyPop",
      variable == "B17001_002" ~ "Poverty",
      variable == "B19013_001" ~ "MedianHHIncome",
      variable == "B15003_001" ~ "AtLeast25",
      str_detect(variable, "B15003_0") ~ "HighSchool",
      str_detect(variable, "B27010_0") ~ "Uninsured",
      variable == "B19058_001" ~ "Households",
      variable == "B19058_002" ~ "PublicAssistance",
      variable == "B25001_001" ~ "HousingUnits",
      variable == "B25002_003" ~ "Vacant",
      TRUE ~ NA
    )
  ) |>
  group_by(GEOID, Description) |>
  summarise(value = sum(estimate)) |>
  pivot_wider(
    id_cols = GEOID,
    names_from = Description,
    values_from = value
    ) 

cinci_hood <- select(hamco_bg, -MedianHHIncome) |>
  inner_join(filter(allocations, Municipality == "Cincinnati")) |>
  mutate(across(Asian:WhiteHispanic, \(x) x*Allocation)) |>
  group_by(Neighborhood) |>
  summarise(across(Asian:WhiteHispanic, sum)) |>
  mutate(
    NonEnglishHH = (Households-English)/Households,
    LimitedEnglishHH = (SpanishLtd+IELtd+AsianLtd+OtherLtd)/Households,
    NonHispanicWhiteRate = (White-WhiteHispanic)/Total,
    BlackRate = Black/Total,
    ChildHHRate = ChildHH/Households,
    ChildRate = Children/Total,
    HispanicRate = Hispanic/Total,
  ) |>
  select(Neighborhood, NonEnglishHH:HispanicRate, Total)

cinci_di <- select(hamco_bg, GEOID, Total) |>
  inner_join(allocations, multiple = "all") |>
  filter(Municipality == "Cincinnati") |>
  mutate(
    census_tract_fips = str_trunc(GEOID, 11, "right", ellipsis = ""),
    Population = Total*Allocation
  ) |>
  left_join(di) |>
  filter(!is.na(dep_index)) |>
  group_by(census_tract_fips, Neighborhood) |>
  mutate(TractPop = sum(Population)) |>
  group_by(Neighborhood) |>
  mutate(
    HoodPop = sum(Population),
    TractShare = TractPop/HoodPop
  ) |>
  distinct(census_tract_fips, Neighborhood, TractShare) |>
  inner_join(di, multiple = "all") |>
  mutate(across(dep_index:fraction_vacant_housing, \(x) x*TractShare)) |>
  group_by(Neighborhood) |>
  summarise(across(dep_index:fraction_vacant_housing, sum)) |>
  mutate(
    County = "Hamilton",
    State = "Ohio"
  ) |>
  rename(
    DeprivationIndex = dep_index,
    PublicAssistanceRate = fraction_assisted_income,
    HighSchoolRate = fraction_high_school_edu,
    MedianHHIncome = median_income,
    UninsuredRate = fraction_no_health_ins,
    PovertyRate = fraction_poverty,
    VacancyRate = fraction_vacant_housing
  ) |>
  inner_join(cinci_hood) |>
  mutate(Municipality = "Cincinnati")

all <- rbind(cs_di, cinci_di)
write_csv(all, "census_data.csv")


