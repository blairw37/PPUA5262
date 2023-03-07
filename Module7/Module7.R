library(tidyverse)
library(stringr)

# Importing Data
craigslist_listings<-read.csv('CRAIGSLIST.Listings.csv')
census_tract_boston<-read.csv('../census-tract-data-boston.csv')
census_tract_other<-read.csv('../census-tract-other-cities.csv')

# Append 25025 to census tract IDs for other census tracts
census_tract_other$CENSUS_TRACT_ID = paste('25025', census_tract_other$CENSUS_TRACT_ID, sep="")

# Create & append lists
census_tract_list_bos<-as.list(census_tract_boston$GEOCODE)
census_tract_list_other<-as.list(census_tract_other$CENSUS_TRACT_ID)
census_tract_list<-append(census_tract_list_bos, census_tract_list_other)

# Filter by census tract ID's
craigslist_listings<-craigslist_listings %>% filter(CT_ID_10 %in% census_tract_list) %>% select(LISTING_ID,LISTING_YEAR,LISTING_MONTH,LISTING_DAY,BODY,LOCATION,AREA_SQFT,PRICE,CT_ID_10)
