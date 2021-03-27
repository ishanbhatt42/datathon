
library(tidyverse)
library(sf)
library(sp)
library(data.table)
library(rgdal)
library(rgeos)

# download cleaned property data
property <- fread('~/Desktop/Datathon/property_clean.csv') %>%
  rowwise() %>%
  mutate(`Full Address` = unlist(strsplit(address, '\n'))[1])

addresses <- fread('~/Desktop/Datathon/Master_Addresses_List.csv') %>% select(
  `Full Address`, `Building ID`
)

buildings <- st_read('~/Desktop/Datathon/GEO/BASEMAP_Buildings.shp') %>% select(geometry, BldgID)

merged <- property %>% merge(addresses, by = 'Full Address') %>% merge(buildings, by.x = 'Building ID', by.y = 'BldgID')
merged <- st_as_sf(merged)
merged <- st_transform(merged, crs = 4236)

# read in zones and overlay zones
zones <- st_read('~/Desktop/Datathon/GEO/CDD_ZoningDistricts.shp')
overlay_zones <- st_read('~/Desktop/Datathon/GEO/CDD_ZoningOverlayDistricts.shp')

zones <- zones %>%
  rowwise() %>%
  mutate(zone = unlist(str_split(ZONE_TYPE, '-'))[1]) %>%
  filter(zone != 'SD') %>%
  ungroup()
zones <- st_as_sf(zones)
zones <- st_transform(zones, crs = 4236)

test <- st_join(merged, zones, join=st_within)
