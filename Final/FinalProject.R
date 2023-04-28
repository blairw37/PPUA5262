library(tidyverse)
library(ggplot2)
library(sf)
library(ggmap)
library(forcats)
library(modelsummary)
library(formattable)

# Importing Data
craigslist_listings<-read.csv('CRAIGSLIST.Listings.csv')
census_tract_boston<-read.csv('census-tract-data-boston.csv')
census_tract_other<-read.csv('census-tract-other-cities.csv')
census_indicators<-read.csv('Final/census_indicators/ACS_1519_TRACT.csv')

# Importing maps
redline<-st_read('Final/RedliningMaps/Boston_1938_HOLC/Boston_1938_HOLC.shp')
ltm_income<-st_read('Final/Low_to_Moderate_Income_Population_by_Tract/Low_to_Moderate_Income_Population_by_Tract.shp')
urban_renewal<-st_read('Final/Other_Important_Planning_Boundaries_layers/Other_Important_Planning_Boundaries_layers.shp')

# Append 25025 to census tract IDs for other census tracts
census_tract_other$CENSUS_TRACT_ID = paste('25025', census_tract_other$CENSUS_TRACT_ID, sep="")

# Create & append lists
census_tract_list_bos<-as.list(census_tract_boston$GEOCODE)
census_tract_list_other<-as.list(census_tract_other$CENSUS_TRACT_ID)
census_tract_list<-append(census_tract_list_bos, census_tract_list_other)

# Filter by census tract ID's
craigslist_listings<-craigslist_listings %>% filter(CT_ID_10 %in% census_tract_list) %>% dplyr::select(LISTING_ID,LISTING_YEAR,LISTING_MONTH,LISTING_DAY,BODY,LOCATION,AREA_SQFT,PRICE,CT_ID_10)
ltm_income<-ltm_income %>% filter(GEOID %in% census_tract_list)
census_indicators<-census_indicators %>% filter(CT_ID_10 %in% census_tract_list)

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

ct_price$PRICE_OUTLIERS<-ifelse(ct_price$PRICE_OUTLIERS>0, 1,0)

# Square Ft
ct_sqft<-filter(craigslist_listings, !is.na(AREA_SQFT) & AREA_SQFT<3000)
ct_sqft<-ct_sqft %>%
  group_by(CT_ID_10) %>%
  summarise(AVG_SQFT=mean(AREA_SQFT, na.rm = TRUE),
            MIN_SQFT=min(AREA_SQFT, na.rm = TRUE),
            MAX_SQFT=max(AREA_SQFT, na.rm = TRUE),
            SQFT_outliers=(sum((AREA_SQFT > 3000) | (AREA_SQFT < 150))))

ct_sqft$SQFT_outliers<-ifelse(ct_sqft$SQFT_outliers>0,1,0)

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


# ANOVA with redline
# Calculate price per sqft
ct_stats$PRICE_PER_SQFT<-ct_stats$AVG_PRICE/ct_stats$AVG_SQFT

# Summary tables
datasummary_skim(ct_stats[c('AVG_PRICE', 'AVG_SQFT', 'PER_YES_SECTION_8', 'PER_NO_SECTION_8', 'MISSING_DATA', 'AVG_BODY_STR', 'PRICE_PER_SQFT')])


# Split into quartiles
ct_stats<-ct_stats %>% mutate(quartile = ntile(PRICE_PER_SQFT, 4))
names(ct_stats)[21]<-'PRICE_PER_SQFT_QUARTILE'

# Anova
aov_sec8<-aov(PER_YES_SECTION_8~as.factor(PRICE_PER_SQFT_QUARTILE), data=ct_stats)
summary(aov_sec8)

# TukeyHSD
TukeyHSD(aov_sec8)

# Visualizing
means<-aggregate(PER_YES_SECTION_8~PRICE_PER_SQFT_QUARTILE,data=ct_stats,mean)

# names(means)[1]='PRICE_PER_SQFT_QUARTILE'

ses<-aggregate(PER_YES_SECTION_8~PRICE_PER_SQFT_QUARTILE,data=ct_stats,
               function(x) sd(x, na.rm=TRUE/sqrt(length(!is.na(x)))))

names(ses)[2]<-'se_PER_YES_SECTION_8'
# names(ses)[1]<-'PRICE_PER_SQFT_QUARTILE'

means<-merge(means,ses,by='PRICE_PER_SQFT_QUARTILE')

means<-transform(means, lower=PER_YES_SECTION_8-1.96*se_PER_YES_SECTION_8, upper=PER_YES_SECTION_8+1.96*se_PER_YES_SECTION_8)

bar<-ggplot(data=means,aes(x=PRICE_PER_SQFT_QUARTILE, y=PER_YES_SECTION_8))
bar + geom_bar(stat="identity",position="dodge",fill="blue") +
  ylab('Percentage of Listings \nThat Welcome Section 8 Vouchers') +
  geom_errorbar(aes(ymax=upper, ymin=lower),
                position=position_dodge(.9))


# Boston map
boston<-get_map(location=c(left = -71.193799, 
                           bottom = 42.22, 
                           right = -70.985746, 
                           top = 42.43),
                source="stamen")
boston_map<-ggmap(boston)
boston_map

# Redline map
redline_map<-boston_map + geom_sf(data=redline, aes(fill=holc_grade), inherit.aes = FALSE) +
  scale_fill_manual(values = c("green", "blue", "yellow", "red")) +
  labs(fill = "Grades Assigned \nby the HOLC in 1938")
redline_map

# Low to moderate income
ltm_income_map<-boston_map + geom_sf(data=ltm_income[ltm_income$LOWMODPCT>0,], aes(fill=LOWMODPCT), inherit.aes = FALSE) +
  scale_fill_gradient(high = "purple", low = "orange") + 
  labs(fill = "Percentage of Low to Moderate Income")
ltm_income_map

# Urban renewal
urban_renewal_map<-boston_map + geom_sf(data=urban_renewal, aes(fill=STATUS), inherit.aes = FALSE) +
  scale_fill_manual(values = c("navy")) +
  labs(fill = "Urban Renewal \nProjects")
urban_renewal_map

# Demographics
colnames(ltm_income)[colnames(ltm_income) == 'GEOID'] = 'CT_ID_10'
census_indicators<-merge(ltm_income, census_indicators, by = "CT_ID_10")
census_indicators$Black<-census_indicators$Black * 100
census_indicators$White<-census_indicators$White * 100

census_indicators_map_bl<-boston_map + geom_sf(data=census_indicators, aes(fill=Black), inherit.aes = FALSE) +
  scale_fill_gradient(high = "purple", low = "orange") +
  labs(fill = "Percentage Black Population")
census_indicators_map_bl

census_indicators_map_wh<-boston_map + geom_sf(data=census_indicators, aes(fill=White), inherit.aes = FALSE) +
  scale_fill_gradient(high = "purple", low = "orange") +
  labs(fill = "Percentage White Population")
census_indicators_map_wh

# Percentage of section 8 holders map
ct_stats<-merge(census_indicators, ct_stats, by = "CT_ID_10")

per_sec8_map<-boston_map + geom_sf(data=ct_stats, aes(fill=PER_YES_SECTION_8), inherit.aes = FALSE) +
  scale_fill_gradient(high = "purple", low = "orange") +
  labs(fill = "Percentage of Sec8 Approval")
per_sec8_map

# Section 8 stats
per_yes_sec_8_avg<-mean(ct_stats$PER_YES_SECTION_8)
per_yes_sec_8_avg

per_no_sec_8_avg<-mean(ct_stats$PER_NO_SECTION_8)
per_no_sec_8_avg
