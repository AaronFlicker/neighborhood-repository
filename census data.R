library(tidyverse)
library(tidycensus)
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
