library(sf)
library(leaflet)
library(tidyverse)

# Load dataframes
phone_log<-'https://raw.githubusercontent.com/ccnd13/East3DataVizFinalProject/main/FinalProject/311_Phone_Call_Log_Mod.csv'
df_311_phone_log<-read.csv(phone_log)

census <- '../FinalProject/2010_CensusData/2010_CensusData.shp'
df_2010_census <- st_read(census, stringsAsFactors = FALSE)

#bus_license<-'https://raw.githubusercontent.com/ccnd13/East3DataVizFinalProject/main/FinalProject/Business_Licenses_geocoded.csv'
#df_bus_license<-read.csv(bus_license)

contact_mgmt<-'https://raw.githubusercontent.com/ccnd13/East3DataVizFinalProject/main/FinalProject/311_Contact_Management_Cases.csv'
df_311_contact_mgmt<-read.csv(contact_mgmt)

city_council <- '../FinalProject/City_Council_Districts/City_Council_Districts.shp'
df_city_council <- st_read(city_council, stringsAsFactors = FALSE)

public_facilities<-'https://raw.githubusercontent.com/ccnd13/East3DataVizFinalProject/main/FinalProject/Public_Facilities.csv'
df_public_facilities<-read.csv(public_facilities)

spatial_public_facilities <- 
  df_public_facilities %>%
  st_as_sf(coords = c("Lon","Lat")) %>% 
  st_set_crs(value = 4326)

school_boundaries <- '../FinalProject/School_Boundaries/School_Boundaries.shp'
df_school_boundaries <- st_read(school_boundaries, stringsAsFactors = FALSE)

park_locations<-'https://raw.githubusercontent.com/ccnd13/East3DataVizFinalProject/main/FinalProject/Parks_Locations_and_Features.csv'
df_park_locations<-read.csv(park_locations)

spatial_park_locations <- 
  df_park_locations %>%
  st_as_sf(coords = c("Lon","Lat")) %>% 
  st_set_crs(value = 4326)

abandoned_property <- '../FinalProject/Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp'
df_abandoned_property <- st_read(abandoned_property, stringsAsFactors = FALSE)

street_lights<-'https://raw.githubusercontent.com/ccnd13/East3DataVizFinalProject/main/FinalProject/Street_Lights.csv'
df_street_lights<-read.csv(street_lights)

spatial_street_lights <- 
  df_street_lights %>%
  st_as_sf(coords = c("Lon","Lat")) %>% 
  st_set_crs(value = 4326)

###############################################################################
# Lights plot

leaflet()  %>%
  addProviderTiles("CartoDB.DarkMatter") %>% 
  addCircles(data = spatial_street_lights,
             color = "yellow",
             stroke = 0,
             fillOpacity = 0.01,
             radius = 100)
  

