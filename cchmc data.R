library(tidyverse)
library(lubridate)
library(odbc)
library(DBI)
library(readxl)
library(tidygeocoder)
library(tigris)
library(sf)

con <- dbConnect(odbc::odbc(), "ClarityProd")

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
write_csv(for_geocoder, "for_degauss.csv")

#cd "C:\Users\FLI6SH\OneDrive - cchmc\ACT_Neighborhood\Repository\neighborhood-repository
#docker run --rm -v ${PWD}:/tmp ghcr.io/degauss-org/geocoder:3.0.2 for_degauss.csv
  
geocoded <- read.csv("for_degauss_geocoded_v3.0.2.csv") |>
  filter(precision == "range", 
         geocode_result != "imprecise_geocode") |>
  st_as_sf(coords = c("lon", "lat"), crs = 'NAD83', remove = FALSE)

oh_munis <- county_subdivisions(
  state = "OH",
  county = c("Hamilton", "Butler", "Clermont", "Warren"),
  year = 2021
  ) |>
  mutate(State = "Ohio")

ky_munis <- county_subdivisions(
  state = "KY",
  county = c("Boone", "Campbell", "Kenton"),
  year = 2021
  ) |>
  mutate(State = "Kentucky")

in_munis <- county_subdivisions(
  state = "IN",
  county = "Dearborn",
  year = 2021
  ) |>
  mutate(State = "Indiana")

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
    ),
    County = case_when(
      COUNTYFP == "017" ~ "Butler",
      COUNTYFP == "025" ~ "Clermont",
      COUNTYFP == "061" ~ "Hamilton",
      COUNTYFP == "165" ~ "Warren",
      COUNTYFP == "015" ~ "Boone",
      COUNTYFP == "037" ~ "Campbell",
      COUNTYFP == "117" ~ "Kenton",
      COUNTYFP == "029" ~ "Dearborn",
      TRUE ~ NA
    )
  ) |>
  select(Municipality, County, State, geometry) |>
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
muni_multi <- filter(oki_munis, count > 1) |>
  select(Municipality:ID) |>
  inner_join(oki, multiple = "all")

oh_tracts <- tracts(
  state = "OH",
  county = c("Hamilton", "Warren", "Clermont", "Butler"),
  year = 2021
  )

ky_tracts <- tracts(
  state = "KY",
  county = c("Kenton", "Boone", "Campbell"),
  year = 2021
  )

in_tracts <- tracts(
  state = "IN",
  county = "Dearborn",
  year = 2021
  )

hooded2 <- rbind(oh_tracts, ky_tracts) |>
  rbind(in_tracts) |>
  st_join(distinct(muni_multi, ID, geometry)) |>
  distinct(ID, geometry, GEOID) |>
  as_tibble() |>
  inner_join(muni_multi) |>
  select(ID, State, County, Municipality) |>
  mutate(Neighborhood = NA)
  
hamco_bg <- block_groups(
  state = "OH",
  county = "Hamilton",
  year = 2021
  )

cinci_bg <- st_join(cinci1, hamco_bg) |>
  inner_join(filter(allocations, Municipality == "Cincinnati")) |>
  distinct(ID, Municipality, Neighborhood) |>
  group_by(ID) |>
  mutate(count = n()) |>
  arrange(-count, ID)

hooded3 <- filter(cinci_bg, count == 1) |>
  select(-count) |>
  mutate(
    County = "Hamilton",
    State = "Ohio"
    )

asthma <- rbind(hooded1, hooded2) |>
  rbind(hooded3) |>
  inner_join(unique_adds) |>
  inner_join(adds, multiple = "all") |>
  group_by(State, County, Municipality, Neighborhood) |>
  summarise(
    AsthmaRegistry = sum(Registry),
    AsthmaAdmissions = sum(Admission)
  )
write_csv(asthma, "asthma.csv")
