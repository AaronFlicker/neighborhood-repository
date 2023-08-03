library(htmlwidgets)
library(leaflet)
library(leaflegend)
library(leafpop)
library(sf)
library(tidyverse)
library(tidygeocoder)
library(tigris)

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

census <- read.csv("census_data.csv") |>
  mutate(across(Municipality:Neighborhood, \(x) coalesce(x, "All")))
asthma <- read.csv("asthma.csv")

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
  mutate(StreetNum = paste(Number, Street))

geocoded <- geocode(
  schools2,
  street = StreetNum,
  city = City,
  state = State,
  postalcode = Zip,
  method = "census"
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

benchmarks <- all_data |>
  select(
    County:Neighborhood, 
    ends_with("Rate"), 
    Level, 
    MedianHHIncome, 
    DeprivationIndex
    )

bench_min <- benchmarks |>
  group_by(Level) |>
  summarise(across(WhiteRate:DeprivationIndex, \(x) min(x, na.rm = TRUE))) |>
  pivot_longer(
    cols = WhiteRate:DeprivationIndex,
    names_to = "variable",
    values_to = "Minimum"
  )

bench_max <- benchmarks |>
  group_by(Level) |>
  summarise(across(WhiteRate:DeprivationIndex, \(x) max(x, na.rm = TRUE))) |>
  pivot_longer(
    cols = WhiteRate:DeprivationIndex,
    names_to = "variable",
    values_to = "Maximum"
  )

bench_mean <- benchmarks |>
  filter(
    County == "All" | (Municipality == "Cincinnati" & Neighborhood == "All")
    ) |>
  mutate(Level = ifelse(Level == "Municipality", "Neighborhood", "All")) |>
  select(WhiteRate:DeprivationIndex) |>
  pivot_longer(
    cols = c(WhiteRate:AsthmaAdmissionRate, MedianHHIncome, DeprivationIndex),
    names_to = "variable",
    values_to = "Mean"
  )

bench_mean2 <- filter(bench_mean, Level == "All") |>
  mutate(Level = "County")

benchmarks_all <- bench_mean2 |>
  mutate(Level = "Municipality") |>
  rbind(bench_mean) |>
  rbind(bench_mean2) |>
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
      Neighborhood != "All" ~ Neighborhood,
      Municipality != "All" ~ Municipality,
      TRUE ~ County
    )
  ) |>
  inner_join(select(all_data, County, Label, Tier)) |>
  arrange(Level, Label) |>
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
      high = "red", 
      low = "blue", 
      mid = "grey"
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
      high = "red", 
      low = "blue", 
      mid = "grey"
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
      high = "red", 
      low = "blue", 
      mid = "grey"
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

hood_lines <- block_groups(
  state = "OH",
  county = "Hamilton"
  ) |>
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

pal <- colorFactor("Blues", domain = all_data$Tier)

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
      "Schools"
      ),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
    ) |>
  addCircleMarkers(
    data = hood_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(hood_demo_popups, height = 300, width = 700),
    group = "Demographics"
  ) |>
  addCircleMarkers(
    data = hood_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(hood_di_popups, height = 300, width = 700),
    group = "Deprivation indicators"
  ) |>
  addCircleMarkers(
    data = hood_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(hood_health_popups, height = 300, width = 700),
    group = "Health indicators"
  ) |>
  addMarkers(
    data = geocoded,
    ~long,
    ~lat,
    popup = ~Name,
    group = "Schools"
  )

#saveWidget(citymap, "city map.html")
  
oh_lines <- county_subdivisions(
  state = "OH",
  county = c("Hamilton", "Warren", "Clermont", "Butler")
  ) |>
  mutate(
    County = case_when(
      NAME == "Loveland" ~ "Hamilton",
      NAMELSAD == "Fairfield city" ~ "Butler",
      NAMELSAD == "Milford city" ~ "Clermont",
      COUNTYFP == "061"  ~ "Hamilton",
      COUNTYFP == "017" ~ "Butler",
      COUNTYFP == "025" ~ "Clermont",
      TRUE ~ "Warren"
    )
  ) |>
  group_by(County, COUSUBFP, NAMELSAD) |>
  summarise(geometry = st_union(geometry)) |>
  ungroup() |>
  mutate(
    Municipality = str_remove(NAMELSAD, " city"),
    Municipality = str_remove(Municipality, " village"),
    Municipality = str_remove(Municipality, "The Village of "),
    Municipality = str_to_title(Municipality)
  ) |>
  select(County, Municipality, geometry)

ky_lines <- county_subdivisions(
  state = "KY",
  county = c("Kenton", "Campbell", "Boone") 
) |>
  rename(Municipality = NAME) |>
  mutate(
    County = case_when(
      COUNTYFP == "037" ~ "Campbell",
      COUNTYFP == "117" ~ "Kenton",
      TRUE ~ "Boone"
    )
  ) |>
  select(County, Municipality, geometry)

in_lines <- county_subdivisions(
  state = "IN",
  county = "Dearborn"
) |>
  mutate(
    County = "Dearborn",
    Municipality = str_to_title(NAMELSAD)
  ) |>
  select(County, Municipality, geometry)

muni_lines <- rbind(oh_lines, ky_lines) |>
  rbind(in_lines) |>
  mutate(
    Centroid = st_centroid(geometry),
    Neighborhood = "All"
  ) |>
  inner_join(areas) |>
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
      "Health indicators"
    ),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addCircleMarkers(
    data = muni_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(muni_demo_popups, height = 300, width = 700),
    group = "Demographics"
  ) |>
  addCircleMarkers(
    data = muni_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(muni_di_popups, height = 300, width = 700),
    group = "Deprivation indicators"
  ) |>
  addCircleMarkers(
    data = muni_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(muni_health_popups, height = 300, width = 700),
    group = "Health indicators"
  ) 

saveWidget(muni_map, "muni map.html")

county_lines <- counties(
  state = c("OH", "KY", "IN")
  ) |>
  filter(
    (STATEFP == "39" & NAME %in% c("Hamilton", "Butler", "Warren", "Clermont")) |
      (STATEFP == "21" & NAME %in% c("Kenton", "Boone", "Campbell")) |
      (STATEFP == "18" & NAME == "Dearborn")
    ) |>
  rename(County = NAME) |>
  select(County, geometry) |>
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
      "Health indicators"
    ),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addCircleMarkers(
    data = county_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(county_demo_popups, height = 300, width = 700),
    group = "Demographics"
  ) |>
  addCircleMarkers(
    data = county_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(county_di_popups, height = 300, width = 700),
    group = "Deprivation indicators"
  ) |>
  addCircleMarkers(
    data = muni_lines$Centroid,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    radius = 4,
    popup = popupGraph(county_health_popups, height = 300, width = 700),
    group = "Health indicators"
  ) 

saveWidget(county_map, "county map.html")
