
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
  filter(!grepl('SD', ZONE_TYPE))
zones <- st_as_sf(zones)
zones <- st_transform(zones, crs = 4236)

overlay_zones <- st_transform(overlay_zones, crs = 4236)

bad_overlay <- overlay_zones %>%
  filter(grepl('20.11|20.12|20.80|20.90', Article) | grepl('Lesley Porter', Name))
good_overlay <- overlay_zones %>%
  filter(grepl('20.20|20.40|20.60', Article) | grepl('Basement', Name))

zoned_data <- st_join(merged, zones, join=st_within)
zoned_data <- zoned_data %>% 
  filter(!is.na(ZONE_TYPE))

bad_list <- st_join(merged, bad_overlay, join = st_within)
bad_list <- bad_list %>% filter(!is.na(Shape_area))
bad_list <- bad_list$`Full Address`
bad_list <- bad_list[!duplicated(bad_list)]

good_list <- st_join(merged, good_overlay, join=st_within)
good_list <- good_list %>%
  filter(!is.na(Shape_area))
good_list <- good_list$`Full Address`
good_list <- good_list[!duplicated(good_list)]

zoned_data <- zoned_data %>%
  rowwise() %>%
  mutate(inOverlayZone = case_when(
    `Full Address` %in% bad_list ~ 'BAD',
    `Full Address` %in% good_list ~ 'GOOD',
    TRUE ~ 'NEUTRAL'
  ))

zoned_data <- as.data.frame(zoned_data)

output <- zoned_data %>% select(-geometry)

write.csv(output, '~/Desktop/Datathon/zoned_data.csv')

# trees

trees <- st_read('~/Desktop/Datathon/GEO/ENVIRONMENTAL_StreetTrees.shp')
trees <- st_transform(trees, crs = 4236)
trees_data <- st_join(trees, zones, join=st_within)
trees_data <- trees_data %>% 
  filter(!is.na(ZONE_TYPE))
trees_data <- as.data.frame(trees_data)
trees_data <- trees_data %>% select(-geometry)

write.csv(trees_data, '~/Desktop/Datathon/trees.csv')

dev <- st_read('~/Desktop/Datathon/GEO/development_log.shp') %>% filter(Residentia != 0) %>% select(
  units = Residentia,
  geometry
)
dev <- st_transform(dev, crs = 4236)
zoned_dev <- st_join(dev, zones, join=st_within)
zoned_dev <- zoned_dev %>% 
  filter(!is.na(ZONE_TYPE))
zoned_dev <- as.data.frame(zoned_dev)
zoned_dev <- zoned_dev %>% select(-geometry)

zoned_dev <- zoned_dev %>% group_by(ZCODE) %>%
  summarize(units = sum(as.numeric(units)))

write.csv(zoned_dev, '~/Desktop/Datathon/zoned_development.csv')

















