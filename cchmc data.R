library(tidyverse)
library(lubridate)
library(odbc)
library(DBI)
library(readxl)
library(tidygeocoder)
library(tigris)
library(sf)

con <- dbConnect(odbc::odbc(), "ClarityProd")

# births <- read_excel(
#   "births.xlsx",
#   col_types = c("text", "date", "skip", "numeric", "numeric", rep("skip", 10))
#   ) |>
#   filter(year(calendar_dt) == 2022) |>
#   group_by(Neighborhood) |>
#   summarise(
#     Births = sum(NumBirths, na.rm = TRUE),
#     PrematureBirths = sum(NumGestAgeLT37, na.rm = TRUE)
#     )

## Allocation of Hamilton County census block groups
allocations <- read.csv(
  "input data/neighborhood bg allocations.csv",
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
  "input data/oki allocations.csv",
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
  mutate(
    ID = row_number(),
    Address = ifelse(
      str_detect(Address, "4231 SOPHIA"), "4231 SOPHIAS Way", Address)
    )

batch1 <- unique_adds[1:10000,]

geocoded <- geocode(
  batch1,
  street = Address,
  city = City,
  state = state,
  postalcode = Zip,
  method = "census",
  full_results = TRUE,
  api_options = list(census_return_type = 'geographies')
  )

to_geocode <- unique_adds[10001:nrow(unique_adds),]
while (nrow(to_geocode) > 0){
  batch <- to_geocode[1:(min(10000, nrow(to_geocode))),]
  x <- geocode(
    batch,
    street = Address,
    city = City,
    state = state,
    postalcode = Zip,
    method = "census",
    full_results = TRUE,
    api_options = list(census_return_type = 'geographies')
  )
  geocoded <- rbind(geocoded, x)
  to_geocode <- anti_join(to_geocode, batch, join_by(ID))
}

geocoded2 <- filter(geocoded, !is.na(lat)) |>
  select(ID, lat, long, state_fips, county_fips, census_tract, census_block) |>
  mutate(GEOID = paste0(state_fips, county_fips, census_tract)) |>
  inner_join(oki, multiple = "all") |>
  group_by(ID) |>
  mutate(count = n())

singles <- filter(geocoded2, count == 1)
hooded1 <- filter(singles, Municipality == "Cincinnati") |>
  mutate(
    GEOID = paste0(GEOID, str_trunc(census_block, 1, "right", ellipsis = ""))
    ) |>
  inner_join(allocations) |>
  select(ID, Municipality, County, State, Neighborhood)
hooded2 <- filter(singles, Municipality != "Cincinnati") |>
  select(ID, Municipality, County, State) |>
  mutate(Neighborhood = NA)

multi <- filter(geocoded2, count > 1)

oh_munis <- county_subdivisions(
  state = "OH",
  county = c("Hamilton", "Butler", "Clermont", "Warren"),
  year = 2021
  )

ky_munis <- county_subdivisions(
  state = "KY",
  county = c("Boone", "Campbell", "Kenton"),
  year = 2021
)

in_munis <- county_subdivisions(
  state = "IN",
  county = "Dearborn",
  year = 2021
)

oki_munis <- rbind(oh_munis, ky_munis) |>
  rbind(in_munis) |>
  separate_wider_delim(
    NAMELSAD, delim = "village", names = "Municipality", too_many = "drop"
  ) |>
  separate_wider_delim(
    Municipality, delim = "city", names = "Municipality", too_many = "drop"
  ) |>
  separate_wider_delim(
    Municipality, delim = "CCD", names = "Municipality", too_many = "drop"
  ) |>
  separate_wider_delim(
    Municipality, delim = ",", names = "Municipality", too_many = "drop"
  ) |>
  mutate(
    Municipality = str_to_title(Municipality),
    Municipality = str_trim(Municipality),
    Municipality = ifelse(
      str_detect(Municipality, "Indian Hill"), "Indian Hill", Municipality
    )
  ) |>
  st_as_sf()

points_muni <- multi %>%
  st_as_sf(coords = c("long", "lat"),
           crs = st_crs(oki_munis))
intersected <- st_intersects(points_muni, oki_munis) |> 
  as.character()
hooded3 <- data.frame(points_muni) %>%
  mutate(
    intersection = as.integer(intersected),
    Municipality = ifelse(
      is.na(intersection), NA, oki_munis$Municipality[intersection]
    )
  ) |>
  inner_join(multi) |>
  mutate(
    GEOID = paste0(GEOID, str_trunc(census_block, 1, "right", ellipsis = ""))
    ) |>
  select(ID, Municipality, GEOID, State, County) |>
  left_join(allocations) |>
  mutate(Neighborhood = ifelse(Municipality == "Cincinnati", Neighborhood, NA)) |>
  select(ID, Municipality, Neighborhood, State, County)
hooded_all <- rbind(hooded1, hooded2) |>
  rbind(hooded3) |>
  inner_join(unique_adds) |>
  inner_join(adds, multiple = "all") |>
  ungroup() |>
  mutate(
    County = case_when(
      Municipality == "Loveland" ~ "Hamilton",
      Municipality == "Milford" ~ "Clermont",
      TRUE ~ County
      )
    ) |>
  group_by(State, County, Municipality, Neighborhood) |>
  summarise(
    AsthmaRegistry = sum(Registry),
    AsthmaAdmissions = sum(Admission)
    )
