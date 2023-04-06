library(tidyverse)
library(ggplot2)
library(sf)
library(QuantPsyc)

# Importing Data
craigslist_listings<-read.csv('CRAIGSLIST.Listings.csv')
census_tract_boston<-read.csv('census-tract-data-boston.csv')
census_tract_other<-read.csv('census-tract-other-cities.csv')
redlining<-read.csv('Module11/Redlining/Boston_Tracts_2010_HOLC.csv')
uhi<-read.csv('Module11/UrbanLandCoverAndHeatIndex/_Tract_Level_Variables.csv')
income<-st_read('Module11/Low_to_Moderate_Income_Population_by_Tract/Low_to_Moderate_Income_Population_by_Tract.shp')

# Append 25025 to census tract IDs for other census tracts
census_tract_other$CENSUS_TRACT_ID = paste('25025', census_tract_other$CENSUS_TRACT_ID, sep="")

# Create & append lists
census_tract_list_bos<-as.list(census_tract_boston$GEOCODE)
census_tract_list_other<-as.list(census_tract_other$CENSUS_TRACT_ID)
census_tract_list<-append(census_tract_list_bos, census_tract_list_other)

# Filter by census tract ID's
craigslist_listings<-craigslist_listings %>% filter(CT_ID_10 %in% census_tract_list) %>% dplyr::select(LISTING_ID,LISTING_YEAR,LISTING_MONTH,LISTING_DAY,BODY,LOCATION,AREA_SQFT,PRICE,CT_ID_10)

# Calculate price per square foot
craigslist_listings<-craigslist_listings %>% filter(!is.na(AREA_SQFT) & AREA_SQFT != 0)
craigslist_listings$PRICE_PER_SQFT<-craigslist_listings$PRICE / craigslist_listings$AREA_SQFT

# Removing outliers
craigslist_listings<-craigslist_listings %>% filter(PRICE_PER_SQFT<5)

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

# Price per sqft
ct_price_per_sqft<-craigslist_listings %>%
  group_by(CT_ID_10) %>%
  summarise(AVG_PRICE_PER_SQFT=mean(PRICE_PER_SQFT))

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
ct_sec8$PER_SECTION_8_MENTIONS<-ct_sec8$SECTION_8_MENTIONS / ct_listings$COUNT

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
ct_stats<-merge(ct_stats, ct_price_per_sqft, by= 'CT_ID_10')

# Independent variable: PER_YES_SECTION_8

# Merge
ct_stats<-merge(ct_stats, redlining, by = 'CT_ID_10')
ct_stats<-merge(ct_stats, uhi, by = 'CT_ID_10', all.x = TRUE)
ct_stats<-merge(ct_stats, income, by.x = 'CT_ID_10', by.y = 'GEOID')

# Correlation
cor.test(ct_stats$PER_YES_SECTION_8, ct_stats$COUNT) # t = -3.0369 p-value = 0.002808 cor = -0.2377074 
cor.test(ct_stats$PER_YES_SECTION_8, ct_stats$AVG_PRICE) # t = -2.3287 p-value = 0.02118 cor = -0.1844291 
cor.test(ct_stats$PER_YES_SECTION_8, ct_stats$AVG_BODY_STR) # t = -4.2045 p-value = 4.422e-05 cor = -0.3208916 
cor.test(ct_stats$PER_YES_SECTION_8, ct_stats$MISSING_DATA) # t = -2.801 p-value = 0.005749 cor = -0.2201701
cor.test(ct_stats$PER_YES_SECTION_8, ct_stats$redline) # t = -3.3406 p-value = 0.001049 cor = -0.2599398
cor.test(ct_stats$PER_YES_SECTION_8, ct_stats$LST_CT) # t = -2.2836 p-value = 0.02376 cor = -0.1809763
cor.test(ct_stats$PER_YES_SECTION_8, ct_stats$LOWMODPCT) # t = 1.5417 p-value = 0.1252 cor = 0.1232898
cor.test(ct_stats$PER_YES_SECTION_8, ct_stats$AVG_PRICE_PER_SQFT) # t = -2.1259 p-value = 0.03511 cor = -0.1688503 

ct_stats_correlation<-ct_stats %>% dplyr::select('CT_ID_10', 'AVG_PRICE_PER_SQFT', 'redline', 'LOWMODPCT')

ct_stats_correlation %>%
  dplyr::select(-'CT_ID_10') %>%
  cor()

rcorr_matrix<-ct_stats_correlation %>%
  dplyr::select(-'CT_ID_10') %>%
  as.matrix() %>%
  rcorr()

rcorr_matrix[1]
rcorr_matrix[2]
rcorr_matrix[3]

ggpairs(data=ct_stats_correlation, columns=2:4)

# Regression

ct_stats_filtered<-ct_stats %>%
  filter(!is.na(PER_YES_SECTION_8) & !is.na(COUNT) & !is.na(AVG_PRICE) & !is.na(AVG_BODY_STR) & !is.na(MISSING_DATA) & !is.na(redline) & !is.na(LST_CT))

# COUNT
reg_count<-lm(scale(PER_YES_SECTION_8)~scale(COUNT), data=ct_stats_filtered)
summary(reg_count)

# AVG_PRICE
reg_avg_price<-lm(scale(PER_YES_SECTION_8)~scale(AVG_PRICE), data=ct_stats_filtered)
summary(reg_avg_price)

# AVG_BODY_STR
reg_avg_body_str<-lm(scale(PER_YES_SECTION_8)~scale(AVG_BODY_STR), data=ct_stats_filtered)
summary(reg_avg_body_str)

# MISSING_DATA
reg_missing_data<-lm(scale(PER_YES_SECTION_8)~scale(MISSING_DATA), data=ct_stats_filtered)
summary(reg_missing_data)

# redline
reg_redline<-lm(scale(PER_YES_SECTION_8)~scale(redline), data=ct_stats_filtered)
summary(reg_redline)

reg_grade<-lm(PER_YES_SECTION_8~Grade, data=ct_stats)
summary(reg_grade)

# LST_CT
reg_lst<-lm(scale(PER_YES_SECTION_8)~scale(LST_CT), data=ct_stats_filtered)
summary(reg_lst)

# AVG_PRICE_PER_SQFT
reg_avg_price_per_sqft<-lm(scale(PER_YES_SECTION_8)~scale(AVG_PRICE_PER_SQFT), data=ct_stats_filtered)
summary(reg_avg_price_per_sqft)

# Multi
reg_multi<-lm(PER_YES_SECTION_8~COUNT + AVG_PRICE + AVG_BODY_STR + MISSING_DATA + redline + LST_CT, data=ct_stats)
summary(reg_multi)

lm.beta(reg_multi)

# Visualization

# COUNT
count_plot<-ggplot(data=ct_stats, aes(x=COUNT, y=PER_YES_SECTION_8)) +
  geom_point() + xlab("Number of Craigslist Listings") + ylab("Percentage of Section 8 Listings")
count_plot + geom_smooth(method=lm)

# AVG_PRICE
avg_price_plot<-ggplot(data=ct_stats, aes(x=AVG_PRICE, y=PER_YES_SECTION_8)) +
  geom_point() + xlab("Average Price") + ylab("Percentage of Section 8 Listings")
avg_price_plot + geom_smooth(method=lm)

# AVG_BODY_STR
avg_body_str_plot<-ggplot(data=ct_stats, aes(x=AVG_BODY_STR, y=PER_YES_SECTION_8)) +
  geom_point() + xlab("Average Length of Body") + ylab("Percentage of Section 8 Listings")
avg_body_str_plot + geom_smooth(method=lm)

# LST_CT
lst_plot<-ggplot(data=ct_stats, aes(x=LST_CT, y=PER_YES_SECTION_8)) +
  geom_point() + xlab("Land Surface Temperature") + ylab("Percentage of Section 8 Listings")
lst_plot + geom_smooth(method=lm)

# Redline
redline_plot<-ggplot(data = ct_stats, aes(x=redline, y=PER_YES_SECTION_8)) +
  geom_point() + xlab("Redline") + ylab("Percentage of Section 8 Listings")
redline_plot + geom_smooth(method=lm)

# Price per square ft
price_per_sqft_plot<-ggplot(data = ct_stats, aes(x=AVG_PRICE_PER_SQFT, y=PER_YES_SECTION_8)) +
  geom_point() + xlab("Price per Square Ft") + ylab("Percentage of Section 8 Listings")
price_per_sqft_plot + geom_smooth(method=lm)
