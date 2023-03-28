library(tidyverse)
library(ggplot2)
library(GGally)
library(Hmisc)

# Importing Data
craigslist_listings<-read.csv('CRAIGSLIST.Listings.csv')
census_tract_boston<-read.csv('census-tract-data-boston.csv')
census_tract_other<-read.csv('census-tract-other-cities.csv')

# Append 25025 to census tract IDs for other census tracts
census_tract_other$CENSUS_TRACT_ID = paste('25025', census_tract_other$CENSUS_TRACT_ID, sep="")

# Create & append lists
census_tract_list_bos<-as.list(census_tract_boston$GEOCODE)
census_tract_list_other<-as.list(census_tract_other$CENSUS_TRACT_ID)
census_tract_list<-append(census_tract_list_bos, census_tract_list_other)

# Filter by census tract ID's
craigslist_listings<-craigslist_listings %>% filter(CT_ID_10 %in% census_tract_list) %>% select(LISTING_ID,LISTING_YEAR,LISTING_MONTH,LISTING_DAY,BODY,LOCATION,AREA_SQFT,PRICE,CT_ID_10)

#Aggregations

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

ct_sec8$PER_YES_SECTION_8<-ct_sec8$YES_SECTION_8 / ct_listings$COUNT
ct_sec8$PER_NO_SECTION_8<-ct_sec8$NO_SECTION_8 / ct_listings$COUNT

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

# Merge
ct_stats<-merge(ct_listings, ct_price, by = 'CT_ID_10')
ct_stats<-merge(ct_stats, ct_sqft, by ='CT_ID_10')
ct_stats<-merge(ct_stats, ct_sec8, by ='CT_ID_10')
ct_stats<-merge(ct_stats, ct_missing_data, by = 'CT_ID_10')
ct_stats<-merge(ct_stats, ct_body_str_count, by = 'CT_ID_10')

# Variables analyze
# COUNT
# AVG_PRICE
# PER_YES_SECTION_8
# PER_NO_SECTION_8
# AVG_BODY_STR
# MISSING_DATA

ct_stats %>%
  select(-'CT_ID_10') %>%
  cor()

cor.test(ct_stats$AVG_PRICE, ct_stats$PER_YES_SECTION_8)
cor.test(ct_stats$AVG_PRICE, ct_stats$MISSING_DATA)
cor.test(ct_stats$AVG_PRICE, ct_stats$AVG_BODY_STR)

ct_stats<-ct_stats %>%
  select('CT_ID_10', 'COUNT', 'AVG_PRICE', 'PER_YES_SECTION_8', 'PER_NO_SECTION_8', 'AVG_BODY_STR', 'MISSING_DATA')

cor.test(ct_stats$COUNT, ct_stats$MISSING_DATA)
cor.test(ct_stats$COUNT, ct_stats$PER_YES_SECTION_8)
cor.test(ct_stats$AVG_PRICE, ct_stats$PER_YES_SECTION_8)

ct_stats %>%
  select(-'CT_ID_10') %>%
  cor()

rcorr_matrix<-ct_stats %>%
  select(-'CT_ID_10') %>%
  as.matrix() %>%
  rcorr()

rcorr_matrix[1]
rcorr_matrix[2]
rcorr_matrix[3]

# Plotting correlation
ggpairs(data=ct_stats, columns=2:7)
