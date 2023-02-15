library(tidyverse)
library(forcats)
library(modelsummary)
library(formattable)
library(lubridate)

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
craigslist_listings_subset<-craigslist_listings %>% filter(CT_ID_10_SUB %in% census_tract_list) %>% select(LISTING_ID,LISTING_YEAR,LISTING_MONTH,LISTING_DAY,LOCATION,AREA_SQFT,PRICE,CT_ID_10_SUB,BODY,ALLOWS_CATS,ALLOWS_DOGS)
craigslist_listings_subset<-craigslist_listings_subset %>% filter(CT_ID_10_SUB != '010206')

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

# Density
density_listing_date_df<- as.data.frame(table(craigslist_listings_bos$LISTING_DATE))

# Calculating average prices for listings in Boston and outside of Boston
avg_price_per_sq_ft<-craigslist_listings_subset %>%
  group_by(CT_ID_10_SUB) %>%
  summarise(AVG_PRICE_PER_SQ_FT = mean(PRICE_PER_SQ_FT))

# Number of listings in Boston vs outside of Boston
craigslist_listings_subset_in_Boston <- filter(craigslist_listings_subset, IN_BOSTON == 1)
craigslist_listings_subset_outside_Boston <- filter(craigslist_listings_subset, IN_BOSTON == 0)

# Pets
craigslist_listings_subset_dogs <- filter(craigslist_listings_subset, ALLOWS_DOGS == 1)
craigslist_listings_subset_cats <- filter(craigslist_listings_subset, ALLOWS_CATS == 1)
craigslist_listings_subset_pets <- filter(craigslist_listings_subset, ALLOWS_DOGS == 0, ALLOWS_CATS == 0)

# Average rent
avg_rent_bos<-mean(craigslist_listings_subset_in_Boston$PRICE)
avg_rent_out<-mean(craigslist_listings_subset_outside_Boston$PRICE)

# Avg square foot
avg_sq_ft<-mean(craigslist_listings_subset$AREA_SQFT)

# Calculating the average price per square ft for each census tract ID
avg_price_per_sq_ft_df<-craigslist_listings_subset %>%
  group_by(CT_ID_10_SUB,LISTING_DATE) %>%
  summarise(AVG_PRICE_PER_SQ_FT = mean(PRICE_PER_SQ_FT))

# Plot - Number of listings over date

date_func<-function(x) {
  switch(x,
         '2020-02-01' = 'Feb 2020',
         '2020-03-01' = 'Mar 2020',
         '2020-04-01' = 'Apy 2020',
         '2020-05-01' = 'May 2020',
         '2020-06-01' = 'Jun 2020',
         '2020-07-01' = 'Jul 2020',
         '2020-08-01' = 'Aug 2020',
         '2020-09-01' = 'Sep 2020',
         '2020-10-01' = 'Oct 2020',
         '2020-11-01' = 'Nov 2020',
         '2020-12-01' = 'Dec 2020',
         '2021-01-01' = 'Jan 2021',
         '2021-02-01' = 'Fen 2021',
         '2021-03-01' = 'Mar 2021',
         '2021-04-01' = 'Apr 2021',
         '2021-05-01' = 'May 2021',
         '2021-06-01' = 'Jun 2021',
         '2021-07-01' = 'Jul 2021',
         '2021-08-01' = 'Aug 2021',
         '2021-09-01' = 'Sep 2021',
         '2021-10-01' = 'Oct 2021',
         '2021-11-01' = 'Nov 2021',
         '2021-12-01' = 'Dec 2021'
  )
}

craigslist_listings_subset_2<-sapply(craigslist_listings_subset$LISTING_DATE, date_func)

date_freq <- as.data.frame(table(craigslist_listings_subset$LISTING_DATE))
names(date_freq)[1] = "LISTING_DATE"
names(date_freq)[2] = "FREQ"
date_freq_plot <- ggplot(data = date_freq, aes(x=LISTING_DATE, y=FREQ)) + geom_col() + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) 

time_price_plot <- ggplot() + geom_line(data = avg_price_per_sq_ft_df, aes(x = LISTING_DATE, y = AVG_PRICE_PER_SQ_FT))

# Density plot
ct_freq <- as.data.frame(table(craigslist_listings_subset$CT_ID_10_SUB))
names(ct_freq)[1] = "CT_ID_10_SUB"
names(ct_freq)[2] = "FREQ"
ct_freq_plot <- ggplot(ct_freq, aes(x = CT_ID_10_SUB)) + geom_density()

# Time series plot
# Creating a time series plot to compare CT in Cambridge and CT in JP
time_series_plot<-ggplot() + geom_line(data=avg_price_per_sq_ft_df_filtered1, aes(x=LISTING_DATE, y=AVG_PRICE_PER_SQ_FT, color="354100 - Cambridge")) + geom_line(data=avg_price_per_sq_ft_df_filtered2, aes(x=LISTING_DATE, y=AVG_PRICE_PER_SQ_FT, color="120400 - Jamaica Plain")) + labs(title="Average Price Per Square Ft Over Time in Two Census Tracts") + xlab("Listing Date") + ylab("Average Price per Square Ft") + scale_color_manual(values = c("354100 - Cambridge" = "darkblue", "120400 - Jamaica Plain" = "red")) + labs(color = "Census Tracts")
time_series_plot

# Histogram
histogram_plot<-ggplot(avg_price_per_sq_ft_df, aes(x = LISTING_DATE)) + geom_histogram(bins = 10)

# Date freq 2
date_freq_2<-ggplot(craigslist_listings_subset, aes(x = LISTING_DATE)) + geom_bar()

# Get top 20 most expensive census tracts
avg_price_per_sq_ft_df_2<-craigslist_listings_subset %>%
  group_by(CT_ID_10_SUB) %>%
  summarise(AVG_PRICE_PER_SQ_FT = mean(PRICE_PER_SQ_FT))

avg_price_per_sq_ft_df_2<-avg_price_per_sq_ft_df_2 %>% arrange(desc(AVG_PRICE_PER_SQ_FT))
nrow(avg_price_per_sq_ft_df_2)
box_dot_plot<-ggplot(head(avg_price_per_sq_ft_df_2,10), aes(CT_ID_10_SUB, AVG_PRICE_PER_SQ_FT)) + geom_boxplot()

craigslist_listings_subset<-craigslist_listings_subset %>% arrange(desc(PRICE_PER_SQ_FT))
box_dot_plot_2<-ggplot(head(craigslist_listings_subset, 20), aes(CT_ID_10_SUB, PRICE_PER_SQ_FT)) + geom_boxplot()

# Box plots again
craigslist_listings_subset_box<-filter(craigslist_listings_subset, CT_ID_10_SUB == "010104" | CT_ID_10_SUB =="353102" | CT_ID_10_SUB == "070402" | CT_ID_10_SUB == "010600" | CT_ID_10_SUB == "010701")
box_dot_plot_3<-ggplot(craigslist_listings_subset_box, aes(CT_ID_10_SUB, PRICE_PER_SQ_FT)) + geom_boxplot() + geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .5, fill='red', binwidth = 0.15) + theme(axis.text.x = element_text(angle=45, vjust=0.5)) + labs(title= "Box Plot + Dot Plot")
box_dot_plot_3

