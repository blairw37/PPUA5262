library(tidyverse)
library(stringr)
library(sf)
library(ggplot2)
library(ggmap)
library(patchwork)
library(gridExtra)
library(forcats)
library(modelsummary)
library(formattable)

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

# Calculate Price Score
calculate_price_score<-function(PRICE) {
  if (PRICE >= 1577 && PRICE <= 2023) {
    score<-3
  } else if (PRICE >= 1315 && PRICE < 1577) {
    score<-2
  } else if (PRICE > 2023) {
    score<-1
  } else if (PRICE >= 500 && PRICE < 1315) {
    score<-1
  } else {
    score<-0
  }
  return(score)
}

craigslist_listings$PRICE_SCORE<-sapply(craigslist_listings$PRICE, calculate_price_score)

# Calculate location score
calculate_location_score<-function(LOCATION) {
  if (LOCATION == "") {
    score<-0
  } else {
    score<-1
  }
  return(score)
}

craigslist_listings$LOCATION_SCORE<-sapply(craigslist_listings$LOCATION, calculate_location_score)

# Calculate Square footage score
calculate_sqft_score<-function(AREA_SQFT) {
  if (is.na(AREA_SQFT)) {
    score<-0
  } else {
    if (AREA_SQFT >= 500 && AREA_SQFT <= 3000) {
      score<-1
    } else {
      score<-0
    }
  }
  return(score)
}
craigslist_listings$AREA_SQFT_SCORE<-sapply(craigslist_listings$AREA_SQFT, calculate_sqft_score)

# Calculate listing description score
calculate_body_score<-function(BODY_STRING_COUNT) {
  if (BODY_STRING_COUNT > 15 && BODY_STRING_COUNT < 1500) {
    score<-1
  } else {
    score<-0
  }
}
craigslist_listings$BODY_SCORE<-sapply(craigslist_listings$BODY_STRING_COUNT, calculate_body_score)

# Calculate section 8 score
calculate_sec8_score<-function(SECTION_8, NO_SECTION_8) {
  if (SECTION_8 == 1 && NO_SECTION_8==1) {
    score<-0
  } else if (SECTION_8 == 1 && NO_SECTION_8 == 0) {
    score<-2
  } else {
    score<-1
  }
}
craigslist_listings$SEC_8_SCORE<-mapply(calculate_sec8_score, craigslist_listings$SECTION_8, craigslist_listings$NO_SECTION_8)

# Calculate listing quality index
calculate_listing_quality_index<-function(PRICE_SCORE, AREA_SQFT_SCORE, LOCATION_SCORE, BODY_SCORE, SEC_8_SCORE) {
  index<-PRICE_SCORE + AREA_SQFT_SCORE + LOCATION_SCORE + BODY_SCORE + SEC_8_SCORE
  return(index)
}
craigslist_listings$LISTING_QUALITY_INDEX<-mapply(calculate_listing_quality_index, craigslist_listings$PRICE_SCORE, craigslist_listings$AREA_SQFT_SCORE, craigslist_listings$LOCATION_SCORE, craigslist_listings$BODY_SCORE, craigslist_listings$SEC_8_SCORE)

# Calculate summary table
datasummary_skim(craigslist_listings[c('PRICE_SCORE', 'AREA_SQFT_SCORE', 'LOCATION_SCORE', 'BODY_SCORE', 'SEC_8_SCORE', 'LISTING_QUALITY_INDEX')])

# Aggregate Listing Quality Index
ct_listing_quality_index<-craigslist_listings %>%
  group_by(CT_ID_10) %>%
  summarise(CT_LISTING_QUALITY_INDEX=mean(LISTING_INDEX))

# Histogram

ggplot(craigslist_listings, aes(x=LISTING_QUALITY_INDEX)) + geom_histogram(binwidth = 1, colour="black", fill="light blue") + ggtitle("Listing Quality Index Distribution")

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
ct_stats<-merge(ct_stats, ct_listing_quality_index, by = 'CT_ID_10')

# Summary table for aggregate
datasummary_skim(ct_stats[c('CT_LISTING_QUALITY_INDEX')])

ggplot(ct_stats, aes(x=CT_LISTING_QUALITY_INDEX)) + geom_histogram(binwidth = 1, colour="black", fill="light blue", bins = 10) + ggtitle("Listing Quality Index Distribution \nat the aggregate level")


# Listing quality index map
boston<-get_map(location=c(left = -71.193799, 
                           bottom = 42.22, 
                           right = -70.985746, 
                           top = 42.43),
                source="stamen")
boston_map<-ggmap(boston)
boston_map

hcv<-st_read('tracts/Housing_Choice_Vouchers_by_Tract/HOUSING_CHOICE_VOUCHERS_BY_TRACT.shp')
hcv_boston<-hcv %>% filter(GEOID %in% census_tract_list_bos)

colnames(hcv_boston)[colnames(hcv_boston) == 'GEOID'] = "CT_ID_10"
ct_stats<-merge(hcv_boston, ct_stats, by = "CT_ID_10")

# Listing quality index map

listing_quality_index_map<-boston_map + geom_sf(data=ct_stats, aes(fill=CT_LISTING_QUALITY_INDEX), inherit.aes = FALSE) + 
  scale_fill_gradient(high = "purple", low = "orange") + 
  labs(fill = "Listing Quality Index")
listing_quality_index_map
