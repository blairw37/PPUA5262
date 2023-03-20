library(tidyverse)
library(stringr)
library(sf)
library(ggplot2)
library(ggmap)
library(patchwork)
library(gridExtra)

# Importing Data
craigslist_listings<-read.csv('../CRAIGSLIST.Listings.csv')
census_tract_boston<-read.csv('../census-tract-data-boston.csv')

# Create & append lists
census_tract_list_bos<-as.list(census_tract_boston$GEOCODE)

# Filter by census tract ID's
craigslist_listings<-craigslist_listings %>% filter(CT_ID_10 %in% census_tract_list_bos) %>% select(LISTING_ID,LISTING_YEAR,LISTING_MONTH,LISTING_DAY,BODY,LOCATION,AREA_SQFT,PRICE,CT_ID_10)

# AGGREGATIONS

# Number of listings per CT
ct_listings<-data.frame(table(craigslist_listings$CT_ID_10))
colnames(ct_listings)<-c("CT_ID_10", "COUNT")

# Price
ct_price_avg<-craigslist_listings %>%
  group_by(CT_ID_10) %>%
  summarise(AVG_PRICE=mean(PRICE))

ct_price_min<-craigslist_listings %>%
  group_by(CT_ID_10) %>%
  summarise(MIN_PRICE=min(PRICE))

ct_price_max<-craigslist_listings %>%
  group_by(CT_ID_10) %>%
  summarise(MAX_PRICE=max(PRICE))

ct_price<-craigslist_listings %>%
  group_by(CT_ID_10) %>%
  summarise(AVG_PRICE=mean(PRICE),
            MIN_PRICE=min(PRICE),
            MAX_PRICE=max(PRICE),
            PRICE_OUTLIERS=(sum(PRICE < 300)))

# Square Ft
ct_sqft<-filter(craigslist_listings, !is.na(AREA_SQFT))
ct_sqft<-ct_sqft %>%
  group_by(CT_ID_10) %>%
  summarise(AVG_SQFT=mean(AREA_SQFT, na.rm = TRUE),
            MIN_SQFT=min(AREA_SQFT, na.rm = TRUE),
            MAX_SQFT=max(AREA_SQFT, na.rm = TRUE),
            SQFT_outliers=(sum((AREA_SQFT > 3000) | (AREA_SQFT < 150))))

# Section 8
craigslist_listings$SECTION_8 <- as.integer(grepl('section 8', craigslist_listings$BODY, fixed = FALSE, ignore.case = TRUE) | grepl('voucher', craigslist_listings$BODY, fixed = FALSE, ignore.case = TRUE))
craigslist_listings$NO_SECTION_8 <- as.integer(grepl('No section 8', craigslist_listings$BODY, fixed = TRUE) | grepl('no section 8', craigslist_listings$BODY, fixed = TRUE))

ct_sec8<-craigslist_listings %>%
  group_by(CT_ID_10) %>%
  summarise(NO_SECTION_8=sum(NO_SECTION_8),
            SECTION_8_MENTIONS=sum(SECTION_8))

ct_sec8$YES_SECTION_8<-(ct_sec8$SECTION_8_MENTIONS - ct_sec8$NO_SECTION_8)

# Missing meta data
craigslist_listings$MISSING_DATA <- ifelse(is.na(craigslist_listings$AREA_SQFT) | (craigslist_listings$LOCATION == ''), 1, 0)

ct_missing_data<-craigslist_listings %>%
  group_by(CT_ID_10) %>%
  summarise(MISSING_DATA=sum(MISSING_DATA))

# Body length
craigslist_listings$BODY_STRING_COUNT <- str_count(craigslist_listings$BODY, '\\w+')
craigslist_listings$BODY_STR_COUNT_LESS <- ifelse(craigslist_listings$BODY_STRING_COUNT < 15, 1, 0)
craigslist_listings$BODY_STR_COUNT_OVER <- ifelse(craigslist_listings$BODY_STRING_COUNT > 1500, 1, 0)
ct_body_str_count<-craigslist_listings %>%
  group_by(CT_ID_10) %>%
  summarise(BODY_STR_COUNT_LESS=sum(BODY_STR_COUNT_LESS),
            BODY_STR_COUNT_OVER=sum(BODY_STR_COUNT_OVER),
            AVG_BODY_STR=mean(BODY_STRING_COUNT))

# Cash only
craigslist_listings$CASH_ONLY <- as.integer(grepl('cash only', craigslist_listings$BODY, fixed = FALSE, ignore.case = TRUE))
ct_cash_only<-craigslist_listings %>%
  group_by(CT_ID_10) %>%
  summarise(CASH_ONLY=sum(CASH_ONLY))

# Merge
ct_stats<-merge(ct_listings, ct_price, by = 'CT_ID_10')
ct_stats<-merge(ct_stats, ct_sqft, by ='CT_ID_10')
ct_stats<-merge(ct_stats, ct_sec8, by ='CT_ID_10')
ct_stats<-merge(ct_stats, ct_missing_data, by = 'CT_ID_10')
ct_stats<-merge(ct_stats, ct_body_str_count, by = 'CT_ID_10')
ct_stats<-merge(ct_stats, ct_cash_only, by = 'CT_ID_10')

# Importing low to medium income data & filtering by Boston census tracts
ltm<-st_read('tracts/Low_to_Moderate_Income_Population_by_Tract/Low_to_Moderate_Income_Population_by_Tract.shp')
ltm_boston<-ltm %>% filter(GEOID %in% census_tract_list_bos)

# Importing HCV & filtering by Boston census tracts
hcv<-st_read('tracts/Housing_Choice_Vouchers_by_Tract/HOUSING_CHOICE_VOUCHERS_BY_TRACT.shp')
hcv_boston<-hcv %>% filter(GEOID %in% census_tract_list_bos)

# Importing LIHTC Properties
lihtc<-st_read('tracts/Low-Income_Housing_Tax_Credit_Properties/LOW_INCOME_HOUSING_TAX_CREDITS.shp')
lihtc_boston<-lihtc %>% filter(TRACT_LEVE %in% census_tract_list_bos)

# Importing Redlining Maps
redline<-st_read('tracts/RedliningMaps/Boston_1938_HOLC/Boston_1938_HOLC.shp')

# Importing Urban Renewal Map
urban_renewal<-st_read('tracts/Other_Important_Planning_Boundaries_layers/Other_Important_Planning_Boundaries_layers.shp')

# Importing census tracts
census_tracts<-st_read('tracts/Census2020_Tracts/Census2020_Tracts.shp')

# Creating Boston Map
boston<-get_map(location=c(left = -71.193799, 
                           bottom = 42.22, 
                           right = -70.985746, 
                           top = 42.43),
                source="stamen")
boston_map<-ggmap(boston)
boston_map

boston_zoom<-get_map(location=c(left = -71.14, 
                           bottom = 42.3, 
                           right = -71.07, 
                           top = 42.35),
                source="stamen")
boston_map_zoom<-ggmap(boston_zoom)
boston_map_zoom

# Low to medium income percentage map
ltm_map<-boston_map + geom_sf(data=ltm_boston, aes(fill=LOWMODPCT), inherit.aes = FALSE) +
  scale_fill_gradient(high = "purple", low = "orange") + 
  labs(fill = "Percentage of \nLow to Moderate \nIncome Families")

# Redline map
redline_map<-boston_map + geom_sf(data=redline, aes(fill=holc_grade), inherit.aes = FALSE) +
  scale_fill_manual(values = c("green", "blue", "yellow", "red")) +
  labs(fill = "Grades Assigned \nby the HOLC in 1938")
redline_map

# HCV Percent Occupied map
per_hcv_map<-boston_map + geom_sf(data=hcv_boston[hcv_boston$HCV_PCT_RE>0,], aes(fill=HCV_PCT_RE), inherit.aes = FALSE) +
  scale_fill_gradient(high = "purple", low = "orange") + 
  labs(fill = "Percentage Renter \nOccupied Housing Units \nwith HCV")

# Section 8 Map
colnames(hcv_boston)[colnames(hcv_boston) == 'GEOID'] = "CT_ID_10"
ct_stats<-merge(hcv_boston, ct_stats, by = "CT_ID_10")

num_sec8_mentions_map<-boston_map + geom_sf(data=ct_stats, aes(fill=SECTION_8_MENTIONS), inherit.aes = FALSE) + 
  scale_fill_gradient(high = "purple", low = "orange") + 
  labs(fill = "Number of Section 8 Mentions")

# LIHTC Properties map
lihtc_map<-boston_map + geom_point(data=lihtc_boston, aes(x=LON, y=LAT)) +
  labs(fill = "Percentage Renter \nOccupied Housing Units \nwith HCV")
lihtc_map

lihtc_ct<-filter(lihtc_boston, TRACT_LEVE == "25025081400")
lihtc_ct_map<-boston_map_zoom + geom_point(data=lihtc_ct, aes(x=LON, y=LAT)) +
  labs(fill = "LIHTC Properties \nin Census Tract \n081400")
lihtc_ct_map


# Average price map
avg_price_map<-boston_map + geom_sf(data=ct_stats, aes(fill=AVG_PRICE), inherit.aes = FALSE) + 
  scale_fill_gradient(high = "purple", low = "orange") +
  labs(fill = "Average Prices \nCensus Tracts")
avg_price_map

# Census tract map
census_tract_map<-boston_map + geom_sf(data = census_tracts, aes(fill))

# Urban renewal
roxbury_urban_renewal<-filter(urban_renewal, NAME == "Kittredge Square" | NAME == "Campus High School")
urban_renewal_map<-boston_map + geom_sf(data=urban_renewal, aes(fill=STATUS), inherit.aes = FALSE) +
  scale_fill_manual(values = c("navy")) +
  labs(fill = "Urban Renewal \nProjects") + theme(legend.position = "none")
urban_renewal_map

roxbury_urban_renewal_map<-urban_renewal_map + geom_sf(data = roxbury_urban_renewal, aes(fill=STATUS), inherit.aes = FALSE) + scale_fill_manual(values = c("maroon", "navy"))
roxbury_urban_renewal_map
roxbury_urban_renewal_map