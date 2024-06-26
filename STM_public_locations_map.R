## MONTREAL PUBLIC SPACES - Map location ##

install.packages("dplyr")
install.packages("readr")
install.packages("leaflet")
install.packages("magrittr")
install.packages("ggmap")
install.packages("randomcoloR")

library(dplyr)
library(readr)
library(leaflet)
library(magrittr)
library(ggmap)
library(randomcoloR)

# 2022 dataset

MTL_publ_loc <- read.csv("STM/STM_Montreal_Public_Places_2022.csv",encoding = "UTF-8")

dplyr::glimpse(MTL_publ_loc)

# renaming variables

MTL_publ_loc <- MTL_publ_loc %>%
  rename(
    NO_RVI = `NO.RVI`,
    PUBLIC_PLACE_ID = `PUBLIC.PLACE.ID`,
    ADDRESS = ADDRESSE,
    MORE_INFORMATION = `More.Information`)

# data cleaning

MTL_publ_loc_clean <- MTL_publ_loc %>%
  filter(!is.na(TYPE) & !is.na(DESCRIPTION) & !is.na(ADDRESS))

# geo locating data

register_google(key = "AIzaSyCUa_u6ePnxuQL9iG98GW-9QV2_Ut5yQWw")

geo_MTL_publ_loc <- MTL_publ_loc_clean %>%
  mutate_geocode(ADDRESS)

head(geo_MTL_publ_loc)

# filtering wrong addresses

montreal_bounds <- list(
  west = -73.935242,
  east = -73.480734,
  north = 45.705011,
  south = 45.410041
)


geo_MTL_publ_loc <- geo_MTL_publ_loc %>%
  filter(lon >= montreal_bounds$west & lon <= montreal_bounds$east &
           lat >= montreal_bounds$south & lat <= montreal_bounds$north)

# setting the TYPE colors

num_types <- length(unique(geo_MTL_publ_loc$TYPE))

pal_colors <- distinctColorPalette(num_types)

type_levels <- unique(geo_MTL_publ_loc$TYPE)

pal <- setNames(pal_colors, unique(geo_MTL_publ_loc$TYPE))

color_pal <- function(type) {
  return(pal[type])
}

# plot the map

MTL_publ_loc_map <- leaflet(geo_MTL_publ_loc) %>%
  addTiles() %>%
  addCircleMarkers(
    ~lon, ~lat,
    color = ~color_pal(TYPE),
    popup = ~paste("Description:", DESCRIPTION, "<br>", "Address:", ADDRESS),
    stroke = FALSE,
    fillOpacity = 0.7
  ) %>%
  addLegend(
    "bottomright",
    colors = pal_colors,
    labels = type_levels,
    title = "Type of Public Place",
    opacity = 1
  )

MTL_publ_loc_map
