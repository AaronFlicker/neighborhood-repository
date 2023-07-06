library(tidyverse)
library(tidycensus)
di <- read.csv(
  "deprivation index all tracts 2021.csv",
  colClasses = c("numeric", "character", rep("numeric", 6))
  )
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
    LengthA = str_length(TractA),
    LengthB = ifelse(is.na(TractB), 0, str_length(TractB))
    )

for (i in 1:nrow(oki)){
  if (str_length(oki$TractA[i]) < 4){
    prefix_length <- 4-str_length(oki$TractA[i])
    prefix <- str_flatten(rep("0", prefix_length))
  }else{
    prefix_length <- 0
    prefix <- ""
  }
  if (is.na(oki$TractB[i])){
    suffix_length <- 6-str_length(oki$TractA[i])-prefix_length
    suffix <- str_flatten(rep("0", suffix_length))
    oki$Tract[i] <- str_flatten(c(prefix, oki$TractA[i], suffix))
  }else{
    suffix_length <- 6-str_length(oki$TractA[i])-prefix_length-str_length(oki$TractB[i])
    suffix <- str_flatten(rep("0", suffix_length))
    oki$Tract[i] <- str_flatten(c(prefix, oki$TractA[i], oki$TractB[i], suffix))
  }
}
oki$GEOID <- paste0(oki$CountyCode, oki$Tract)

vars <- load_variables(2021, "acs5")
vars_bg <- filter(vars, geography == "block group")

bg_var_list <- c(
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

hamco_bg <- get_acs(
  geography = "block group",
  variables = bg_var_list,
  state = "OH",
  county = "Hamilton",
  year = 2021
  ) |>
  inner_join(vars, by = c("variable" = "name")) |>
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
      TRUE ~ NA
    )
  ) |>
  group_by(GEOID, Description) |>
  summarise(value = sum(estimate)) |>
  pivot_wider(
    id_cols = GEOID,
    names_from = Description,
    values_from = value
    ) |>
  mutate(
    LimitedEnglish = SpanishLtd+IELtd+AsianLtd+OtherLtd,
    NonHispanicWhite = White-WhiteHispanic
    )

cinci_hood <- select(hamco_bg, -MedianHHIncome) |>
  inner_join(filter(allocations, Municipality == "Cincinnati")) |>
  mutate(across(Asian:NonHispanicWhite, \(x) x*Allocation)) |>
  group_by(Neighborhood) |>
  summarise(across(Asian:NonHispanicWhite, sum))

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
  inner_join(oki, multiple = "all") |>
  mutate(Pop = estimate*Allocation) |>
  inner_join(di, by = c("GEOID" = "census_tract_fips"))  |>
  mutate(
    County = case_when(
      Municipality == "Loveland" ~ "Hamilton",
      Municipality == "Milford" ~ "Clermont",
      TRUE ~ County
    )
  ) |>
  group_by(Municipality, County) |>
  mutate(
    CityPop = sum(Pop),
    PopShare = Pop/CityPop,
    DIWeight = dep_index*PopShare
    ) |>
  summarise(dep_index = sum(DIWeight))

cs_vars <- c(
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

oh_cs <- get_acs(
  geography = "county subdivision",
  variables = cs_vars,
  state = "OH",
  county = c("Clermont", "Warren", "Butler", "Hamilton"),
  year = 2021
)

ky_cs <- get_acs(
  geography = "county subdivision",
  variables = cs_vars,
  state = "KY",
  county = c("Boone", "Campbell", "Kenton"),
  year = 2021
)

in_cs <- get_acs(
  geography = "county subdivision",
  variables = cs_vars,
  state = "IN",
  county = "Dearborn",
  year = 2021
)

oki_cs <- rbind(oh_cs, ky_cs) |>
  rbind(in_cs) |>
  inner_join(vars, by = c("variable" = "name")) |>
  mutate(
    Category = case_when(
      variable == "B01001_001" ~ "Population",
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
  group_by(NAME, Category) |>
  summarise(estimate = sum(estimate)) |>
  pivot_wider(id_cols = NAME, names_from = Category, values_from = estimate) |>
  separate_wider_delim(
    NAME,
    delim = ", ",
    names = c("Municipality", "County", "State")
  ) |>
  mutate(
    County = case_when(
      Municipality == "Loveland" ~ "Hamilton",
      Municipality == "Milford" ~ "Clermont",
      TRUE ~ Municipality
    ),
    Income = MedianHHIncome*Population
  ) |>
  group_by(Municipality, County, State) |>
  summarise(
    across(AtLeast25:HousingUnits, sum),
    across(Population:Income, sum)
    
    )
  mutate(
    Municipality = str_remove(Municipality, " city"),
    Municipality = str_remove(Municipality, " CCD"),
    Municipality = str_remove(Municipality, " village"),
    Municipality = str_remove(Municipality, "The Village of "),
    Municipality = str_to_title(Municipality),
    PovertyRate = Poverty/PovertyPop,
    HighSchoolRate = HighSchool/AtLeast25,
    UninsuredRate = Uninsured/Population,
    AssistanceRate = PublicAssistance/Households,
    VacancyRate = Vacant/HousingUnits
    )
  
test <- oki |>
  group_by(Municipality) |>
  summarise(counties = length(unique(County)))
  



