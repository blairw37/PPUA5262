library(tidyverse)
library(forcats)
library(modelsummary)
library(formattable)

craigslist_listings<-read.csv('dataverse_files/CRAIGSLIST.Listings.csv')
census_tract_boston<-read.csv('census-tract-data-boston.csv')
census_tract_other<-read.csv('census-tract-other-cities.csv')

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
craigslist_listings_subset<-craigslist_listings %>% filter(CT_ID_10_SUB %in% census_tract_list) %>% select(LISTING_ID,LISTING_YEAR,LISTING_MONTH,LISTING_DAY,LOCATION,AREA_SQFT,PRICE,CT_ID_10_SUB,BODY)

# Filter out outliers
craigslist_listings_subset <- filter(craigslist_listings_subset, PRICE > 400)
craigslist_listings_subset <- filter(craigslist_listings_subset, AREA_SQFT < 3000)
craigslist_listings_subset <- filter(craigslist_listings_subset, AREA_SQFT > 500)

# Append column for if in Boston
craigslist_listings_subset$IN_BOSTON<-ifelse(craigslist_listings_subset$CT_ID_10_SUB %in% census_tract_list_bos, 1, 0)

# Calclulate PRICE_PER_SQ_FT
craigslist_listings_subset<-craigslist_listings_subset %>% drop_na(AREA_SQFT)
craigslist_listings_subset<-craigslist_listings_subset %>% mutate(PRICE_PER_SQ_FT=PRICE/AREA_SQFT)

# Create LISTING_DATE
craigslist_listings_subset$LISTING_MONTH_NUM<-match(craigslist_listings_subset$LISTING_MONTH, month.name)
craigslist_listings_subset<-unite(craigslist_listings_subset, col="LISTING_YR_MONTH", c('LISTING_YEAR', 'LISTING_MONTH_NUM'), sep='-')
craigslist_listings_subset$LISTING_DATE<-as.Date(paste(craigslist_listings_subset$LISTING_YR_MONTH,"-01",sep=""))

# Calculate average price for a 500 sq foot apartment in JP
craigslist_listings_500<-craigslist_listings_subset %>% filter(CT_ID_10_SUB=="120400") %>% filter(AREA_SQFT > 700) %>% filter (AREA_SQFT < 900)
avg_rent<-mean(craigslist_listings_500$PRICE)

# Calculating the average price per square ft for each census tract ID
avg_price_per_sq_ft_df<-craigslist_listings_subset %>%
  group_by(CT_ID_10_SUB,LISTING_DATE,IN_BOSTON) %>%
  summarise(AVG_PRICE_PER_SQ_FT = mean(PRICE_PER_SQ_FT))

# Filtering by census tract in Cambridge
avg_price_per_sq_ft_df_filtered1<-avg_price_per_sq_ft_df %>% filter(CT_ID_10_SUB=="354100") %>% select(CT_ID_10_SUB,LISTING_DATE,AVG_PRICE_PER_SQ_FT)

# Filtering by census tract in Jamaica Plain
avg_price_per_sq_ft_df_filtered2<-avg_price_per_sq_ft_df %>% filter(CT_ID_10_SUB=="120400") %>% select(CT_ID_10_SUB,LISTING_DATE,AVG_PRICE_PER_SQ_FT)

# Creating a time series plot to compare CT in Cambridge and CT in JP
time_series_plot<-ggplot() + geom_line(data=avg_price_per_sq_ft_df_filtered1, aes(x=LISTING_DATE, y=AVG_PRICE_PER_SQ_FT, color="354100 - Cambridge")) + geom_line(data=avg_price_per_sq_ft_df_filtered2, aes(x=LISTING_DATE, y=AVG_PRICE_PER_SQ_FT, color="120400 - Jamaica Plain")) + labs(title="Average Price Per Square Ft Over Time in Two Census Tracts") + xlab("Listing Date") + ylab("Average Price per Square Ft") + scale_color_manual(values = c("354100 - Cambridge" = "darkblue", "120400 - Jamaica Plain" = "red")) + labs(color = "Census Tracts")
time_series_plot

# Calculating average prices for listings in Boston and outside of Boston
avg_price_per_sq_ft_bos<-craigslist_listings_subset %>%
  group_by(IN_BOSTON,LISTING_DATE) %>%
  summarise(AVG_PRICE_PER_SQ_FT = mean(PRICE_PER_SQ_FT))

# Filter by CTs in Boston
avg_price_per_sq_ft_df_bos<-avg_price_per_sq_ft_bos %>% filter(IN_BOSTON==1) %>% select(LISTING_DATE,AVG_PRICE_PER_SQ_FT,IN_BOSTON)

# Filter by CTs outside of Boston
avg_price_per_sq_ft_df_other<-avg_price_per_sq_ft_bos %>% filter(IN_BOSTON==0) %>% select(LISTING_DATE,AVG_PRICE_PER_SQ_FT,IN_BOSTON)

# Creating a time series plot to compare in Boston to outside of Boston
time_series_plot_bos<-ggplot() + geom_line(data=avg_price_per_sq_ft_df_bos, aes(x=LISTING_DATE, y=AVG_PRICE_PER_SQ_FT, color="In Boston")) + geom_line(data=avg_price_per_sq_ft_df_other, aes(x=LISTING_DATE, y=AVG_PRICE_PER_SQ_FT, color="Outside of Boston")) + labs(title="Average Price Per Square Ft Over Time in Boston vs. Outside Boston") + xlab("Listing Date") + ylab("Average Price per Square Ft") + scale_color_manual(values = c("In Boston" = "darkblue", "Outside of Boston" = "red")) + labs(color = "Census Tracts")

time_series_plot_bos

# Creating a time series plot to compare JP with census tracts outside of Boston
time_series_plot_jp<-ggplot() + geom_line(data=avg_price_per_sq_ft_df_filtered2, aes(x=LISTING_DATE, y=AVG_PRICE_PER_SQ_FT, color="010104 - Back Bay")) + geom_line(data=avg_price_per_sq_ft_df_other, aes(x=LISTING_DATE, y=AVG_PRICE_PER_SQ_FT, color="Outside of Boston")) + labs(title="Average Price Per Square Ft Over Time in 010104 - Back Bay vs. Outside Boston") + xlab("Listing Date") + ylab("Average Price per Square Ft") + scale_color_manual(values = c("010104 - Back Bay" = "darkblue", "Outside of Boston" = "red")) + labs(color = "Census Tracts")
time_series_plot_jp

# Density Plot
craigslist_listings_bos <- filter(craigslist_listings_subset, IN_BOSTON == 1)
density_df<- as.data.frame(table(craigslist_listings_bos$CT_ID_10_SUB))
names(density_df)[names(density_df) == 'Var1'] <- 'CT_ID_10_SUB'
density_df<-density_df %>% arrange(desc(Freq))
View(head(density_df,10))

