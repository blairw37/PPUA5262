library(tidyverse)
library(stringr)

# Importing Data
craigslist_listings<-read.csv('CRAIGSLIST.Listings.csv')
census_tract_boston<-read.csv('../census-tract-data-boston.csv')
census_tract_other<-read.csv('../census-tract-other-cities.csv')

# Convert Census Tract ID to characters
craigslist_listings$CT_ID_10<-as.character(craigslist_listings$CT_ID_10)
census_tract_other$CENSUS_TRACT_ID<-as.character(census_tract_other$CENSUS_TRACT_ID)
census_tract_boston$GEOCODE<-as.character(census_tract_boston$GEOCODE)

# Create substring column in craigslist_listings and census_tract_boston
craigslist_listings$CT_ID_10_SUB<-substring(craigslist_listings$CT_ID_10,6,11)
census_tract_boston$GEOCODE_SUB<-substring(census_tract_boston$GEOCODE,6,11)

# Create & append lists
census_tract_list_bos<-as.list(census_tract_boston$GEOCODE_SUB)
census_tract_list_other<-as.list(census_tract_other$CENSUS_TRACT_ID)
census_tract_list<-append(census_tract_list_bos, census_tract_list_other)

# Filter by census tract ID's
craigslist_listings_subset<-craigslist_listings %>% filter(CT_ID_10_SUB %in% census_tract_list) %>% select(LISTING_ID,LISTING_YEAR,LISTING_MONTH,LISTING_DAY,BODY,LOCATION,AREA_SQFT,PRICE,CT_ID_10_SUB)

# Filter out outliers
# craigslist_listings_subset <- filter(craigslist_listings_subset, PRICE > 400)
# craigslist_listings_subset <- filter(craigslist_listings_subset, AREA_SQFT < 3000)
# craigslist_listings_subset <- filter(craigslist_listings_subset, AREA_SQFT > 500)

# Append column for if in Boston
# craigslist_listings_subset$IN_BOSTON<-ifelse(craigslist_listings_subset$CT_ID_10_SUB %in% census_tract_list_bos, 1, 0)

# Calculate PRICE_PER_SQ_FT
# craigslist_listings_subset<-craigslist_listings_subset %>% drop_na(AREA_SQFT)
# craigslist_listings_subset<-craigslist_listings_subset %>% mutate(PRICE_PER_SQ_FT=PRICE/AREA_SQFT)

# Create LISTING_DATE
craigslist_listings_subset$LISTING_MONTH_NUM<-match(craigslist_listings_subset$LISTING_MONTH, month.name)
craigslist_listings_subset<-unite(craigslist_listings_subset, col="LISTING_YR_MONTH", c('LISTING_YEAR', 'LISTING_MONTH_NUM'), sep='-')
craigslist_listings_subset$LISTING_DATE<-as.Date(paste(craigslist_listings_subset$LISTING_YR_MONTH,"-01",sep=""))

# AGGREGATIONS

# # Number of listings per CT
# ct_listings<-data.frame(table(craigslist_listings_subset$CT_ID_10_SUB))
# colnames(ct_listings)<-c("CT_ID_10_SUB", "COUNT")

# # Price aggregations
# ct_max_price<-aggregate(PRICE~CT_ID_10_SUB, data=craigslist_listings_subset, max)
# ct_avg_price<-aggregate(PRICE~CT_ID_10_SUB, data=craigslist_listings_subset, mean)
# ct_min_price<-aggregate(PRICE~CT_ID_10_SUB, data=craigslist_listings_subset, min)

# # Sq ft aggregations
# ct_max_sqft<-aggregate(AREA_SQFT~CT_ID_10_SUB, data=craigslist_listings_subset, max, na.action = na.omit)
# ct_avg_sqft<-aggregate(AREA_SQFT~CT_ID_10_SUB, data=craigslist_listings_subset, mean, na.action = na.omit)
# ct_min_sqft<-aggregate(AREA_SQFT~CT_ID_10_SUB, data=craigslist_listings_subset, min, na.action = na.omit)

# # Listings that mention section 8
# craigslist_listings_subset$SECTION_8 <- as.integer(grepl('section 8', craigslist_listings_subset$BODY, fixed = FALSE, ignore.case = TRUE) | grepl('voucher', craigslist_listings_subset$BODY, fixed = FALSE, ignore.case = TRUE))
# section_8<-filter(craigslist_listings_subset, SECTION_8 == 1)

# ct_sec8_mention<-aggregate(SECTION_8~CT_ID_10_SUB, data=craigslist_listings_subset, sum)

# # No section 8 allowed
# craigslist_listings_subset$NO_SECTION_8 <- as.integer(grepl('No section 8', craigslist_listings_subset$BODY, fixed = TRUE) | grepl('no section 8', craigslist_listings_subset$BODY, fixed = TRUE))
# no_section_8<-filter(craigslist_listings_subset, NO_SECTION_8 == 1)
# ct_no_sec8<-aggregate(NO_SECTION_8~CT_ID_10_SUB, data=craigslist_listings_subset, sum)

# # Missing meta data
# craigslist_listings_subset$MISSING_DATA <- ifelse(is.na(craigslist_listings_subset$AREA_SQFT) | (craigslist_listings_subset$LOCATION == ''), 1, 0)
# missing_data<-aggregate(MISSING_DATA~CT_ID_10_SUB, data=craigslist_listings_subset, sum)

# # Body length
# craigslist_listings_subset$BODY_STRING_COUNT <- str_count(craigslist_listings_subset$BODY, '\\w+')
# body_string_count_avg<-aggregate(BODY_STRING_COUNT~CT_ID_10_SUB, data=craigslist_listings_subset, mean)
# body_string_count_max<-aggregate(BODY_STRING_COUNT~CT_ID_10_SUB, data=craigslist_listings_subset, max)
# body_string_count_min<-aggregate(BODY_STRING_COUNT~CT_ID_10_SUB, data=craigslist_listings_subset, min)

# body_string_count_less<-aggregate(BODY_STRING_COUNT<30~CT_ID_10_SUB, data=craigslist_listings_subset, sum)
# body_string_count_more<-aggregate(BODY_STRING_COUNT>1500~CT_ID_10_SUB, data=craigslist_listings_subset, sum)

# # Cash only
# craigslist_listings_subset$CASH_ONLY <- as.integer(grepl('cash only', craigslist_listings_subset$BODY, fixed = FALSE, ignore.case = TRUE))
# cash_only<-filter(craigslist_listings_subset, CASH_ONLY == 1)
# ct_cash_only<-aggregate(CASH_ONLY~CT_ID_10_SUB, data=craigslist_listings_subset, sum)

# Using group by

# Price
ct_price_avg<-craigslist_listings_subset %>%
  group_by(CT_ID_10_SUB) %>%
  summarise(AVG_PRICE=mean(PRICE))

ct_price_min<-craigslist_listings_subset %>%
  group_by(CT_ID_10_SUB) %>%
  summarise(MIN_PRICE=min(PRICE))

ct_price_max<-craigslist_listings_subset %>%
  group_by(CT_ID_10_SUB) %>%
  summarise(MAX_PRICE=max(PRICE))

ct_price<-craigslist_listings_subset %>%
  group_by(CT_ID_10_SUB) %>%
  summarise(AVG_PRICE=mean(PRICE),
            MIN_PRICE=min(PRICE),
            MAX_PRICE=max(PRICE),
            PRICE_OUTLIERS=(sum(PRICE < 300)))

# Square Ft
ct_sqft<-filter(craigslist_listings_subset, !is.na(AREA_SQFT))
ct_sqft<-ct_sqft %>%
  group_by(CT_ID_10_SUB) %>%
  summarise(AVG_SQFT=mean(AREA_SQFT, na.rm = TRUE),
            MIN_SQFT=min(AREA_SQFT, na.rm = TRUE),
            MAX_SQFT=max(AREA_SQFT, na.rm = TRUE),
            SQFT_outliers=(sum((AREA_SQFT > 3000) | (AREA_SQFT < 150))))

# Section 8
craigslist_listings_subset$SECTION_8 <- as.integer(grepl('section 8', craigslist_listings_subset$BODY, fixed = FALSE, ignore.case = TRUE) | grepl('voucher', craigslist_listings_subset$BODY, fixed = FALSE, ignore.case = TRUE))
craigslist_listings_subset$NO_SECTION_8 <- as.integer(grepl('No section 8', craigslist_listings_subset$BODY, fixed = TRUE) | grepl('no section 8', craigslist_listings_subset$BODY, fixed = TRUE))

ct_sec8<-craigslist_listings_subset %>%
  group_by(CT_ID_10_SUB) %>%
  summarise(NO_SECTION_8=sum(NO_SECTION_8),
            SECTION_8_MENTIONS=sum(SECTION_8))

ct_sec8$YES_SECTION_8<-(ct_sec8$SECTION_8_MENTIONS - ct_sec8$NO_SECTION_8)

# Missing meta data
craigslist_listings_subset$MISSING_DATA <- ifelse(is.na(craigslist_listings_subset$AREA_SQFT) | (craigslist_listings_subset$LOCATION == ''), 1, 0)

ct_missing_data<-craigslist_listings_subset %>%
  group_by(CT_ID_10_SUB) %>%
  summarise(MISSING_DATA=sum(MISSING_DATA))

# Body length
craigslist_listings_subset$BODY_STRING_COUNT <- str_count(craigslist_listings_subset$BODY, '\\w+')
craigslist_listings_subset$BODY_STR_COUNT_LESS <- ifelse(craigslist_listings_subset$BODY_STRING_COUNT < 15, 1, 0)
craigslist_listings_subset$BODY_STR_COUNT_OVER <- ifelse(craigslist_listings_subset$BODY_STRING_COUNT > 1500, 1, 0)
ct_body_str_count<-craigslist_listings_subset %>%
  group_by(CT_ID_10_SUB) %>%
  summarise(BODY_STR_COUNT_LESS=sum(BODY_STR_COUNT_LESS),
           BODY_STR_COUNT_OVER=sum(BODY_STR_COUNT_OVER),
            AVG_BODY_STR=mean(BODY_STRING_COUNT))

# Cash only
craigslist_listings_subset$CASH_ONLY <- as.integer(grepl('cash only', craigslist_listings_subset$BODY, fixed = FALSE, ignore.case = TRUE))
ct_cash_only<-craigslist_listings_subset %>%
  group_by(CT_ID_10_SUB) %>%
  summarise(CASH_ONLY=sum(CASH_ONLY))

# Merge
ct_stats<-merge(ct_listings, ct_price, by = 'CT_ID_10_SUB')
ct_stats<-merge(ct_stats, ct_sqft, by ='CT_ID_10_SUB')
ct_stats<-merge(ct_stats, ct_sec8, by ='CT_ID_10_SUB')
ct_stats<-merge(ct_stats, ct_missing_data, by = 'CT_ID_10_SUB')
ct_stats<-merge(ct_stats, ct_body_str_count, by = 'CT_ID_10_SUB')
ct_stats<-merge(ct_stats, ct_cash_only, by = 'CT_ID_10_SUB')

# Plots

# Missing Data & Body length
missing_data_body_len<-ggplot(ct_stats, aes(x=AVG_BODY_STR, y=MISSING_DATA))
missing_data_body_len + geom_point()

# No section 8 vs missing data
missing_data_sec8<-ggplot(ct_stats, aes(x=MISSING_DATA, y=NO_SECTION_8))
missing_data_sec8 + geom_point()

# section 8 mentions vs avg price
sec_8_avg_price<-ggplot(ct_stats, aes(x=AVG_PRICE, y=SECTION_8_MENTIONS))
sec_8_avg_price + geom_point()

# Missing data & min price - USE THIS
missing_data_min_price<-ggplot(ct_stats, aes(x=MIN_PRICE, y=MISSING_DATA))
missing_data_min_price + geom_point() + stat_smooth(method = lm)

model<-lm(MISSING_DATA ~ MIN_PRICE, data=ct_stats)

missing_data_max_price<-ggplot(ct_stats, aes(x=MAX_PRICE, y=MISSING_DATA))
missing_data_max_price + geom_point()

model<-lm(MISSING_DATA ~ MIN_PRICE, data=ct_stats)

# Missing data & avg price
missing_data_avg_price<-ggplot(ct_stats, aes(x=AVG_PRICE, y=MISSING_DATA))
missing_data_avg_price + geom_point()

# Avg price & sqft outliers - USE THIS
sqft_outliers<-filter(ct_stats, SQFT_outliers>5)
avg_price_outliers<-ggplot(sqft_outliers, aes(x=AVG_PRICE, y=SQFT_outliers))
avg_price_outliers + geom_point()

# Avg price & price outliers - USE THIS
price_outliers<-filter(ct_stats, PRICE_OUTLIERS>0)
avg_price_outliers<-ggplot(price_outliers, aes(x=AVG_PRICE, y=SQFT_outliers))
avg_price_outliers + geom_point()

# Section 8 mentions and avg price
ct_sec8<-filter(ct_stats, SECTION_8_MENTIONS > 0)
sec8_avg_price_2<-ggplot(ct_sec8, aes(x=AVG_PRICE, y=SECTION_8_MENTIONS))
sec8_avg_price_2 + geom_point()

# section 8 and min price
ct_sec8<-filter(ct_stats, SECTION_8_MENTIONS > 0)
sec8_min_price_2<-ggplot(ct_sec8, aes(x=MIN_PRICE, y=SECTION_8_MENTIONS))
sec8_min_price_2 + geom_point()

# sec 8 missing data
ct_sec8<-filter(ct_stats, SECTION_8_MENTIONS > 0)
sec8_missing_data<-ggplot(ct_sec8, aes(x=SECTION_8_MENTIONS, y=MISSING_DATA))
sec8_missing_data + geom_point()

# Sec 8 missing data no filter
sec8_missing_data_nofilter<-ggplot(ct_stats, aes(x=SECTION_8_MENTIONS, y=MISSING_DATA))
sec8_missing_data_nofilter + geom_point()

# No sec 8 missing data
ct_nosec8<-filter(ct_stats, NO_SECTION_8 > 0)
nosec8_missing_data<-ggplot(ct_nosec8, aes(x=MISSING_DATA, y=NO_SECTION_8))
nosec8_missing_data + geom_point()

# Max sq ft & missing data
max_sqft_missing_data<-ggplot(ct_stats, aes(x=MAX_SQFT, y=MISSING_DATA))
max_sqft_missing_data + geom_point()

# sqft outliers & missing data
ct_outliers<-filter(ct_stats, SQFT_outliers > 0)
outlier_sqft_missing_data<-ggplot(ct_outliers, aes(x=SQFT_outliers, y=MISSING_DATA))
outlier_sqft_missing_data + geom_point() + stat_smooth(method=lm)

# sqft outliers & avg price
sqft_outliers_avg_price<-ggplot(ct_stats, aes(x=AVG_PRICE, y=SQFT_outliers))
sqft_outliers_avg_price + geom_point() + stat_smooth(method=lm)

# Missing data & body length
missing_data_body_len<-ggplot(ct_stats, aes(x=AVG_BODY_STR, y=MISSING_DATA))
missing_data_body_len + geom_point() + stat_smooth(method=lm)
