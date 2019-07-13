#===================================================================
# 1. Loading all required libraries
#===================================================================
#miscellaneous
library('plyr')
library('caTools')
library('MASS')
library('car') # vif
library('rpart') # decision tree
library('rpart.plot') #plotting decision tree
library('relaimpo') #find relative importance of linear regression model variables
# general visualisation
library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
# general data manipulation
library('dplyr')
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
# specific visualisation
library('ggrepel') # visualisation
library('ggridges') # visualisation
library('ggExtra') # visualisation
library('ggforce') # visualisation
library('viridis') # visualisation
# specific data manipulation
library('lazyeval') # data wrangling
library('broom') # data wrangling
library('purrr') # string manipulation
# Date plus forecast
library('lubridate') # date and time
library('timeDate') # date and time
library('tseries') # time series analysis
library('forecast') # time series analysis
library('prophet') # time series analysis
library('timetk') # time series analysis
# Maps / geospatial
library('geosphere') # geospatial locations
library('leaflet') # maps
library('leaflet.extras') # maps
library('maps') # maps
#syntax
library('magrittr')

#===================================================================
# 2. Setting up
#===================================================================
setwd("C:/Users/howar/Documents/School/Analytics Files/Project2")
rm(list=ls())
options(scipen = 999)

#-------------------------------------------------------------------
# 2.1 Reading in the data
#-------------------------------------------------------------------
av <- fread("air_visit_data.csv", encoding = "UTF-8")
as <- fread("air_store_info.csv", encoding = "UTF-8")
hs <- fread("hpg_store_info.csv", encoding = "UTF-8")
ar <- fread("air_reserve.csv", encoding = "UTF-8")
hr <- fread("hpg_reserve.csv", encoding = "UTF-8")
id <- fread("store_id_relation.csv", encoding = "UTF-8")
hol <- fread("date_info.csv", encoding = "UTF-8")
weather_stations.dt <-fread("weather_stations.csv", encoding = "UTF-8")
airstore_nearest.dt <-fread("air_store_info_with_nearest_active_station.csv", encoding = "UTF-8")
tokyo_weather <- fread("tokyo__tokyo-kana__tonokyo.csv")
#===================================================================
# 3. Data Exploration
#===================================================================
#-------------------------------------------------------------------
# 3.1 Exploring visitor data
#-------------------------------------------------------------------
summary(av)
# visitor: min 1 max 877 mean 20 
# date: character
summary(as)
summary(hs)
summary(ar)
summary(hr)
summary(id)
summary(hol)

## changing visit_date class to date
av$visit_date <- lubridate::ymd(av$visit_date)

## Boxplot of visitor data to observe outliers
boxplot(av$visitors)
## Identifies outliers in the boxplot (Press esc to finish)
identify(rep(1, length(av$visitors)), av$visitors, labels = seq_along(av$visitors))

## random large no of visitor to check if the large no is an outlier or not
av[visitors == 409]
rest409 <- av[air_store_id == "air_9828505fefc77d75"]
plot(rest409$visit_date, rest409$visitors)
# from the scatter plot, we conclude that it is an outlier

#===================================================================
# 4. Data Cleaning
#===================================================================
#-------------------------------------------------------------------
# 4.1 Cleaning visitor data
#-------------------------------------------------------------------
## cleaning unrealistic values of visitors
# getting median of every restaurant to determine the most popular restaurant
median <- av[, median(visitors), by = air_store_id]
which.max(median$V1)
median[40]
# restaurant with air_store_id air_1c0b150f9e696a5f is the most popular restaurant
av[air_store_id == "air_1c0b150f9e696a5f", max(visitors)]
# trim at 166 which is the largest no of visitors of the most popular restaurant

## checking to see if 166 is an outlier in the visitors of most popular restaurant
most_pop_rest <- av[air_store_id == "air_1c0b150f9e696a5f"]
plot(most_pop_rest$visit_date, most_pop_rest$visitors)
# not an outlier, threshold to trim data set at 166

## trimming visitors above 166
cleaned_visits <- av[visitors <= 166]

## Finding the no. of unique stations used
length(unique(airstore_nearest.dt$station_id))
# returns 38 stations

## view the no. of restaurants under each weather station
table(airstore_nearest.dt$station_id)
# tokyo__tokyo-kana__tonokyo returns highest no of 305
# will use tokyo__tokyo-kana__tonokyo weather data for subsequent analysis with visitor count

## looking at no. of rows of visitors data for each store
cleaned_visits[,.N,by=air_store_id]
# different restaurants have different no. of visitor data

#===================================================================
# 5. Data Preparation
#===================================================================
#-------------------------------------------------------------------
# 5.1 Area & Genre
#-------------------------------------------------------------------
#setting up the map for air restaurants
air_map <- addProviderTiles(addTiles(leaflet(as)), provider = "Esri.NatGeoWorldMap") 
air_map <- addMarkers(air_map, lng=as$longitude, lat=as$latitude,popup=as$air_store_id, label = as$air_genre_name,clusterOptions = markerClusterOptions(air_map))
#Setting up p1 which shows the number of restaurants against the genre name for air restaurants
p1 <- tally(group_by(as, air_genre_name))
p1$air_genre_name <- factor(p1$air_genre_name, levels = p1$air_genre_name[order(p1$n)])
#Preparing p2 which shows the top 15 areas with the most number of visitors for air restaurants
p2 <- tally(group_by(as, air_area_name))
p2$air_area_name <- factor(p2$air_area_name, levels = p2$air_area_name[order(p2$n)])
p2 <- top_n(p2, 15, n)
#setting up the map for hpg restaurants
hpg_map <- addProviderTiles(addTiles(leaflet(hs)), provider = "Esri.NatGeoWorldMap")
hpg_map <- addMarkers(hpg_map, lng=hs$longitude, lat=hs$latitude,popup=hs$hpg_store_id, label = hs$hpg_genre_name,clusterOptions = markerClusterOptions(hpg_map))
#Prepare p3 which shows the number of restaurants against the genre name for hpg restaurants
p3 <- tally(group_by(hs, hpg_genre_name))
p3$hpg_genre_name <- factor(p3$hpg_genre_name, levels = p3$hpg_genre_name[order(p3$n)])
#Prepare p4 which shows the top 15 areas with the most number of visitors for hpg restaurants
p4 <- tally(group_by(hs, hpg_area_name))
p4$hpg_area_name <- factor(p4$hpg_area_name, levels = p4$hpg_area_name[order(p4$n)])
p4 <- top_n(p4, 15, n)

#Create table that combines air visits and air store info data
foo <- left_join(av, as, by = "air_store_id")
#Find the sum of restaurants per genre & average number of visitors per restaurants
num_restaurant <- av[, .N, by = air_store_id]
num_visitors <- av[, sum(visitors), by = air_store_id]
foo <- left_join(foo, num_restaurant, by = "air_store_id")
foo <- left_join(foo, num_visitors, by = "air_store_id")
#Add average number of visitors to the table for plotting
foo <- as.data.table(foo)
avg_visitors_per_genre <- foo[, "avg_visitors" := V1/N, by=air_store_id]

foo <- group_by(foo, N, air_genre_name) 
foo <- ungroup(foo)
#Prepare visits_day which shows the number of visitors across the week by genre
visits_day <- mutate(foo, wday = wday(visit_date, label = TRUE))
visits_day <- group_by(visits_day, wday, air_genre_name)
visits_day <- dplyr::summarise(visits_day, mean_visitors = mean(visitors)) 

#-------------------------------------------------------------------
# 5.2 Weather
#-------------------------------------------------------------------
## merging airvisit & airstore_nearest using airstoreid
table1 <- merge(cleaned_visits, airstore_nearest.dt, by = 'air_store_id', all = T)

#drop other columns
table1$latitude <- NULL
table1$latitude_str <- NULL
table1$longitude <- NULL
table1$longitude_str <- NULL
table1$station_latitude <- NULL
table1$station_longitude <- NULL
table1$station_vincenty <- NULL
table1$station_great_circle <- NULL
table1$air_area_name <- NULL

#subset out the tokyo__tokyo-kana__tonokyo data
tokyo_visits <- table1[station_id == "tokyo__tokyo-kana__tonokyo"]

##changing the column name so that the data tables can be merged
setnames(tokyo_weather, "calendar_date", 'visit_date')
tokyo_weather$visit_date <- lubridate::ymd(tokyo_weather$visit_date)
tokyo_visits$visit_date <- lubridate::ymd(tokyo_visits$visit_date)

## merging weather columns and visitors column
tokyo_merge <- merge(tokyo_visits, tokyo_weather, by = 'visit_date', all = T)
#drop stationid because we have subsetted a weather station, and all data belongs to the same station.
tokyo_merge$station_id <- NULL

setorder(tokyo_merge, air_store_id, visit_date)
# weather data present on some days but visitors data not available
sum(is.na(tokyo_merge$air_store_id) & is.na(tokyo_merge$visitors))
# 39 of such cases

## removing data that has no restaurants corresponding to the weather data
tokyo_merge_remove <- tokyo_merge[!is.na(air_store_id) & !is.na(visitors)]

sum(!is.na(tokyo_merge_remove$deepest_snowfall))
#only 543/92129 rows have data on deepest snowfall

sum(!is.na(tokyo_merge_remove$total_snowfall))
#only 76/92129 rows have data on total snowfall

## final datatable to be used -----------------------------
tokyo_final <- tokyo_merge_remove

## changing rain to binary 0,1
tokyo_final<-tokyo_final[precipitation>0, rain:= 1]
tokyo_final<-tokyo_final[precipitation == 0, rain:= 0]

sum(tokyo_final$rain == 1, na.rm = TRUE)
# 30315 entries of 1 for rain column
sum(tokyo_final$rain == 0, na.rm = TRUE)
# 18613 entries of 0 for rain column
sum(is.na(tokyo_final$rain))
# 43201 NA for rain column

## proportion of rain column, excluding na values
sum(tokyo_final$rain == 1, na.rm = TRUE)/(sum(tokyo_final$rain == 1, na.rm = TRUE)+sum(tokyo_final$rain == 0, na.rm = TRUE))
# 0.61958 of rain column is 1
sum(tokyo_final$rain == 0, na.rm = TRUE)/(sum(tokyo_final$rain == 1, na.rm = TRUE)+sum(tokyo_final$rain == 0, na.rm = TRUE))
# 0.38042 of rain column is 0 

tokyo_final <- tokyo_final[, mean_visitors := mean(visitors), by = c("air_genre_name","visit_date")]

#-------------------------------------------------------------------
# 5.3 Date, time, holidays
#-------------------------------------------------------------------
#Splitting visit_date into more columns and converting into date format
av$visit_date <- lubridate::ymd(av$visit_date)
av$WDay <- factor(lubridate::wday(av$visit_date, label = TRUE))
av$Day <- factor(lubridate::day(av$visit_date))
av$Month <- factor(lubridate::month(av$visit_date))
av$Year <- factor(lubridate::year(av$visit_date))
av$visit_date <- lubridate::date(av$visit_date)
#Creating a list of dates where holiday_flg==1
#and converting them into Date format
holidays <- c(which(hol$holiday_flg==1))
holiday_dates <- c(hol[holidays]$calendar_date)
holiday_dates <- lubridate::ymd(holiday_dates)
#Adding another column holiday_flg which will be True on holiday dates based 
#on the previously created list of holiday dates
for (i in 1:length(holiday_dates)) {
  av[c(which(av$visit_date==holiday_dates[i])), holiday_flg:= T]
}
av[c(which(is.na(av$holiday_flg))), holiday_flg:=F]
av$holiday_flg <- factor(av$holiday_flg)
#Creating a new column where if the Day of the Week is either Friday, Saturday
#or Sunday, Weekend is set as True
av[av$WDay=="Fri"|av$WDay=="Sat"|av$WDay=="Sun", Weekend := T]
av[c(which(is.na(av$Weekend))), Weekend := F]
#Summary of air visit
summary(av)
#Determine number of entries with visitors greater than 100
av <- av[!(visitors > 166)] #Remove outliers since we have determined previously that rows having visitors greater than 166 are outliers
#Check for incomplete cases
which(!complete.cases(av)) #0
#Summary of the hpg reserve
summary(hr)
#Converting the visit date time into date data type and separating it into
#even more columns for day of the month, month, year, hour, date and day of the week
hr$visit_datetime <- lubridate::ymd_hms(hr$visit_datetime)
hr$VisitDay <- factor(lubridate::day(x = hr$visit_datetime))
hr$VisitMonth <- factor(lubridate::month(x = hr$visit_datetime))
hr$VisitYear <- factor(lubridate::year(x = hr$visit_datetime))
hr$VisitHour <- factor(as.numeric(substr(hr$visit_datetime, 12, 13)))
hr$VisitDate <- date(substr(hr$visit_datetime, 1, 10))
hr$VisitWDay <- factor(lubridate::wday(hr$VisitDate, label = TRUE))
#Converting the reserve date time into date data type and separating it into
#even more columns for day of the month, month, year, hour, date and day of the week
hr$reserve_datetime <- lubridate::ymd_hms(hr$reserve_datetime)
hr$ReserveDay <- factor(lubridate::day(x = hr$reserve_datetime))
hr$ReserveMonth <- factor(lubridate::month(x = hr$reserve_datetime))
hr$ReserveYear <- factor(lubridate::year(x = hr$reserve_datetime))
hr$ReserveHour <- factor(lubridate::hour(hr$reserve_datetime))
hr$ReserveDate <- lubridate::date(substr(hr$reserve_datetime, 1, 10))
hr$ReserveWDay <- factor(lubridate::wday(hr$ReserveDate, label = TRUE))
#Creating two columns for the the number of days and hours between visit date and reserve date 
hr$DateDifference <- hr$VisitDate - hr$ReserveDate
hr$HourDifference <- time_length(hr$visit_datetime - hr$reserve_datetime, unit = "hour")
#Adding another column holiday_flg which will be True on holidays based 
#on the previously created list of holiday dates
for (i in 1:length(holiday_dates)) {
  hr[c(which(hr$VisitDate==holiday_dates[i])), holiday_flg:= T]
}
hr[c(which(is.na(hr$holiday_flg))), holiday_flg:=F]
hr$holiday_flg <- factor(hr$holiday_flg)
#Check for incomplete cases
which(!complete.cases(hr)) #0
#Convert into date data type
ar$visit_date <- lubridate::date(ar$visit_datetime)
#For each unique air store id and visit date pair, we count the total number of reservation visitors
airreserve <- as.data.table(dplyr::summarise(group_by(ar, air_store_id, visit_date), reserve_visitors_air = sum(reserve_visitors)))
#For each unique hpg store id and visit date pair, we count the total number of reservation visitors
hpg_reserve <- as.data.table(dplyr::summarise(group_by(hr, hpg_store_id, VisitDate), reserve_visitors_hpg = sum(reserve_visitors)))
#Merge hpg reserve data with store relation data
hpg_reserve <- merge(hpg_reserve, id, by = "hpg_store_id")
#Change the name of one of the columns for ease of subsequent merge
hpg_reserve$visit_date <- hpg_reserve$VisitDate
hpg_reserve[, VisitDate := NULL]
#Merge into a table containing information about reservations and visits
all_reserve <- merge(av, ar, by = c("air_store_id", "visit_date"))
all_reserve <- merge(all_reserve, hpg_reserve, by = c("air_store_id", "visit_date"))
all_reserve <- mutate(all_reserve, reserve_visitors = reserve_visitors + reserve_visitors_hpg)
all_reserve <- as.data.table(all_reserve)

#-------------------------------------------------------------------
# 5.4 Data Preparation for model building
#-------------------------------------------------------------------
#Changing the name of one of the columns for ease of merge later
hol$visit_date <- hol$calendar_date
hol[, calendar_date:= NULL]
#Reading in the data for ar and av again so so as to reduce the number of independent
#variables used for model building
arr <- fread("air_reserve.csv", encoding = "UTF-8")
avv <- fread("air_visit_data.csv", encoding = "UTF-8")
#Converting visit_datetime into date data type
arr$visit_datetime <- lubridate::ymd_hms(arr$visit_datetime)
arr$visit_date <- lubridate::date(arr$visit_datetime)
#Dropping the visit_datetime column
arr[, visit_datetime:= NULL]
#Convert reserve_datetime into date data type
arr$reserve_datetime <- lubridate::ymd_hms(arr$reserve_datetime)
arr$reserve_datetime <- lubridate::date(arr$reserve_datetime)
arr$reserve_datetime_diff <- arr$visit_date - arr$reserve_datetime
avv <- avv[!(visitors > 166)] #Remove outliers
avv$visit_date <- lubridate::ymd(avv$visit_date)
avv$dow <- lubridate::wday(avv$visit_date)
avv$year <- lubridate::year(avv$visit_date)
avv$month <- lubridate::month(avv$visit_date)
#For each set of unique air_store_id and dow pair, we obtain the minimum number of visitors
tmp <- as.data.table(dplyr::summarise(group_by(avv, air_store_id, dow) ,min_visitors = min(visitors)))
stores <- tmp
#For each set of unique air_store_id and dow pair, we obtain the mean number of visitors
tmp <- as.data.table(dplyr::summarise(group_by(avv, air_store_id, dow) ,mean_visitors = mean(visitors)))
stores <- merge(stores, tmp, by = c("air_store_id", "dow"))
#For each set of unique air_store_id and dow pair, we obtain the median number of visitors
tmp <- as.data.table(dplyr::summarise(group_by(avv, air_store_id, dow) ,median_visitors = median(visitors)))
stores <- merge(stores, tmp, by = c("air_store_id", "dow"))
#For each set of unique air_store_id and dow pair, we obtain the maximum number of visitors
tmp <- as.data.table(dplyr::summarise(group_by(avv, air_store_id, dow) ,max_visitors = max(visitors)))
stores <- merge(stores, tmp, by = c("air_store_id", "dow"))
#For each set of unique air_store_id and dow pair, we obtain the number of occurence of each unique pair
tmp <- as.data.table(dplyr::summarise(group_by(avv, air_store_id, dow) ,count_observations = n()))
stores <- merge(stores, tmp, by = c("air_store_id", "dow"))
#Merge with air store info
stores <- merge(stores, as, by = "air_store_id")
#Convert visit_date into date data type
hol$visit_date <- lubridate::ymd(hol$visit_date)
#Merge air visit date with date info
final <- merge(avv, hol, by = "visit_date")
#Merge with stores obtained from previous steps
final <- merge(final, stores, by = c("air_store_id", "dow"))
#Merge with air reserve
final <- merge(final, arr, by = c("air_store_id", "visit_date"))
#Convert final values into a data table and 
#drop unnecessary columns
final.dt <- as.data.table(final)
final.dt[, air_store_id:= NULL]
final.dt[, visit_date:= NULL]
final.dt[, count_observations := NULL]
#Convert datetime_diff into numeric from date data type for ease of use in models
final.dt$reserve_datetime_diff <- as.numeric(final.dt$reserve_datetime_diff)
final.dt$reserve_date <- final.dt$reserve_datetime
final.dt[, reserve_datetime := NULL]
#Making a copy for use in random forest model
h2o.dt <- final.dt

########################################
# 5.4.1 Preparing for Correlation plot #
########################################
corr <- final.dt #created to remove non-numeric variables from final.dt
#Finding the columns which are not numeric
sapply(corr, is.numeric) 
#Removing columns that cannot be converted into numeric type
corr$day_of_week <- NULL
corr$air_genre_name <- NULL
corr$air_area_name <- NULL
corr$reserve_date <- NULL
#Converting the remaining columns into numeric
sapply(corr, as.numeric)

#################################
# 5.4.2 Factoring the variables #
#################################
final.dt$year <- factor(final.dt$year)
final.dt$month <- factor(final.dt$month)
final.dt$day_of_week <- factor(final.dt$day_of_week)
final.dt$holiday_flg <- factor(final.dt$holiday_flg)
final.dt$air_genre_name <- factor(final.dt$air_genre_name)
final.dt$air_area_name <- factor(final.dt$air_area_name)
final.dt$latitude <- factor(final.dt$latitude)
final.dt$longitude <- factor(final.dt$longitude)
#Removing dow as there already is a day_of_week column
final.dt[, dow := NULL]

#-------------------------------------------------------------------
# 5.5 Train-test split
#-------------------------------------------------------------------
set.seed(2004)
#Setting split ratio as 80% 
train <- sample.split(Y = final.dt$visitors, SplitRatio = 0.8)
trainset <- subset(final.dt, train == T)
testset <- subset(final.dt, train == F)

summary(trainset$visitors)
summary(testset$visitors)

train.dt <- as.data.table(trainset)
test.dt <- as.data.table(testset)

#-------------------------------------------------------------------
# 5.6 CART
#-------------------------------------------------------------------
#Copying a new train data set
cart.train <- train.dt
#Converting air area name and air genre name into numeric as rpart
#appears to be unable to handle utf-8 encoding
cart.train$air_genre_name <- factor(as.numeric(factor(cart.train$air_genre_name)))
cart.train$air_area_name <- factor(as.numeric(factor(cart.train$air_area_name)))

#===================================================================
# 6. Feature Engineering
#===================================================================
#-------------------------------------------------------------------
# 6.1 Visits & Area/Genre
#-------------------------------------------------------------------
###########################
# 6.1.1 air area features #
###########################
air_map
#-------------------------------------------------------------------------
#Plot numbers of different types of cuisine alongside the areas with the most air restaurants
ggplot(data = p1, aes(x = air_genre_name, y = n, fill = air_genre_name)) + 
  geom_col() + 
  theme(legend.position = "none") + 
  coord_flip() + 
  labs(x = "Type of cuisine (air_genre_name)", y = "Number of air restaurants")

#Top 15 areas with the most number of restaurants
ggplot(data = p2, aes(x = air_area_name, y = n, fill = air_area_name)) + 
  geom_col() + 
  theme(legend.position = "none") + 
  coord_flip() + 
  labs(x = "Top 15 areas (air_area_name)", y = "Number of air restaurants")

#Frequency of air_genre in each area
ggplot(data = mutate(as, area = str_sub(as$air_area_name, 1, 10)), aes(x = area, y = air_genre_name)) +
  geom_count(colour = "blue") +
  theme(legend.position = "bottom", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))

###########################
# 6.1.2 hpg area features #
###########################
hpg_map
#-------------------------------------------------------------------------
#Plot numbers of different types of cuisine (or air_genre_names) alongside the areas with the most air restaurants
ggplot(data = p3, aes(x = hpg_genre_name, y = n, fill = hpg_genre_name)) + 
  geom_col() + 
  theme(legend.position = "none") + 
  coord_flip() + 
  labs(x = "Type of cuisine (hpg_genre_name)", y = "Number of hpg restaurants")


ggplot(data = p4, aes(x = hpg_area_name, y = n, fill = hpg_area_name)) + 
  geom_col() + 
  theme(legend.position = "none") + 
  coord_flip() + 
  labs(x = "Top 15 areas (hpg_area_name)", y = "Number of hpg restaurants")

#Frequency of hpg_genre in each area
ggplot(data = mutate(hs, area = str_sub(hs$hpg_area_name, 1, 10)), aes(x = area, y = hpg_genre_name)) +
  geom_count(colour = "red") +
  theme(legend.position = "bottom", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))

#############################
# 6.1.3 Clusltering Effects #
#############################
#------------------------------------------------------------------
# Investigate if clustering effect exists for the different genres.
#------------------------------------------------------------------
# What is clustering effect?
# Clustering effect refers to the better business performance across the category in one area
# when there are more businesses in this category. More visitors may be attracted to these businesses
# as the area would have a reputation for the category.

#-------------------------------------------------------------------------
# For air restaurants (No plots for hpg restaurants since no hpg visits data)
#-------------------------------------------------------------------------
#Plot relationship of avg.visitors and no.of restaurants of the genre
ggplot(data = as.data.frame(foo), mapping = aes(N, avg_visitors, color = air_genre_name)) +
  geom_line() +
  labs(y = "Average number of visitors to 'air' restaurants", x = "Total number of restaurants of the genre in each area") +
  theme(legend.position = "none") +
  facet_wrap(~ air_genre_name)

#-------------------------------------------------------------------------
# 6.2 Visits & Day of the Week/Genre
#-------------------------------------------------------------------------
# Plotting visits throughout the week, by genre
ggplot(data = visits_day, mapping = aes(air_genre_name, mean_visitors, color = wday)) +
  geom_point(size = 4) +
  theme(legend.position = "left", axis.text.y = element_blank(),
        plot.title = element_text(size = 14)) +
  coord_flip() +
  labs(x = "") +
  scale_x_discrete(position = "top") +
  ggtitle("air_genre_name") +
  scale_color_hue()

# Density plot for no. of visitors across genre
ggplot(data = foo, mapping = aes(visitors, 
  air_genre_name, fill = air_genre_name)) +
  geom_density_ridges(bandwidth = 0.1) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(y = "") +
  scale_fill_cyclical(values = c("dark blue", "light blue"))

#-------------------------------------------------------------------------
# 6.3 Visits & Weather
#-------------------------------------------------------------------------
#Exploring tokyo weather columns -------------------------
#Average temperature
ggplot(data = tokyo_weather, aes(x = visit_date, y = avg_temperature)) + 
  geom_line() +
  labs(title = "Average temperature across the date")
#Sunlight
ggplot(data = tokyo_weather, aes(x = visit_date, y = hours_sunlight)) + 
  geom_line() +
  labs(title = "Hours of sunlight across the date")
#Solar radiation
ggplot(data = tokyo_weather, aes(x = visit_date, y = solar_radiation)) + 
  geom_line() +
  labs(title = "Solar radiation across the date")
#Average windspeed
ggplot(data = tokyo_weather, aes(x = visit_date, y = avg_wind_speed)) + 
  geom_line() +
  labs(title = "Average wind speed across the date")
#Average vapour pressure
ggplot(data = tokyo_weather, aes(x = visit_date, y = avg_vapor_pressure)) + 
  geom_line() +
  labs(title = "Average vapor pressure across the date")
#Average local pressure
ggplot(data = tokyo_weather, aes(x = visit_date, y = avg_local_pressure)) + 
  geom_line() +
  labs(title = "Average local pressure across the date")
#Average humidity
ggplot(data = tokyo_weather, aes(x = visit_date, y = avg_humidity)) + 
  geom_line() +
  labs(title = "Average humidity across the date")
#Average sea pressure
ggplot(data = tokyo_weather, aes(x = visit_date, y = avg_sea_pressure)) + 
  geom_line() +
  labs(title = "Average sea pressure across the date")
#Cloud cover
ggplot(data = tokyo_weather, aes(x = visit_date, y = cloud_cover)) + 
  geom_line() +
  labs(title = "cloud cover across the date")


summary(tokyo_final)
# precipitation: min 0 max 106.5 NAs: 43201

## finding correlation between visitors and each weather column
cor(tokyo_final$visitors, tokyo_final$avg_temperature)
cor(tokyo_final$visitors, tokyo_final$high_temperature)
cor(tokyo_final$visitors, tokyo_final$low_temperature)
cor(tokyo_final$visitors, tokyo_final$precipitation, use = "complete.obs")
cor(tokyo_final$visitors, tokyo_final$hours_sunlight)
cor(tokyo_final$visitors, tokyo_final$solar_radiation)
cor(tokyo_final$visitors, tokyo_final$high_temperature)
cor(tokyo_final$visitors, tokyo_final$avg_wind_speed)
cor(tokyo_final$visitors, tokyo_final$avg_vapor_pressure, use = "complete.obs")
cor(tokyo_final$visitors, tokyo_final$avg_local_pressure)
cor(tokyo_final$visitors, tokyo_final$avg_humidity, use = "complete.obs")
cor(tokyo_final$visitors, tokyo_final$avg_sea_pressure)
cor(tokyo_final$visitors, tokyo_final$cloud_cover)
# all variables result in low correlation with visitors

cor(tokyo_final$visitors, tokyo_final$rain, use = "complete.obs")
# low correlation of -0.046945

#Exploration on weather variables and visitors across genre
a <- ggplot(data = tokyo_final, aes(x=precipitation, y=mean_visitors)) + geom_line() +
  facet_wrap(~air_genre_name)
a

b <- ggplot(data = tokyo_final, aes(x=avg_temperature, y=mean_visitors)) + geom_line() +
  facet_wrap(~air_genre_name)
b

c <- ggplot(data = tokyo_final, aes(x=hours_sunlight, y=mean_visitors)) + geom_line() +
  facet_wrap(~air_genre_name)
c

d <- ggplot(data = tokyo_final, aes(x=solar_radiation, y=mean_visitors)) + geom_line() +
  facet_wrap(~air_genre_name)
d

e <- ggplot(data = tokyo_final, aes(x=avg_wind_speed, y=mean_visitors)) + geom_line() +
  facet_wrap(~air_genre_name)
e

f <- ggplot(data = tokyo_final, aes(x=avg_vapor_pressure, y=mean_visitors)) + geom_line() +
  facet_wrap(~air_genre_name)
f

g <- ggplot(data = tokyo_final, aes(x=avg_local_pressure, y=mean_visitors)) + geom_line() +
  facet_wrap(~air_genre_name)
g

h <- ggplot(data = tokyo_final, aes(x=avg_humidity, y=mean_visitors)) + geom_line() +
  facet_wrap(~air_genre_name)
h

i <- ggplot(data = tokyo_final, aes(x=avg_sea_pressure, y=mean_visitors)) + geom_line() +
  facet_wrap(~air_genre_name)
i

j <- ggplot(data = tokyo_final, aes(x=cloud_cover, y=mean_visitors)) + geom_line() +
  facet_wrap(~air_genre_name)
j

#-------------------------------------------------------
# 6.4 Visits & Date, Time, Holidays
#-------------------------------------------------------
#Median vsitors against date on holidays
ggplot(dplyr::summarise(group_by(av[av$holiday_flg==TRUE], visit_date), med_visitors = median(visitors)), aes(visit_date, med_visitors)) + geom_point(size = 3, colour = "#FF4444") + labs(x = "Visit Date", y = "Median visitors")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Median visitors against date (holiday)") + theme(plot.title = element_text(hjust = 0.5))
#Mean visitors against date on holidays
ggplot(dplyr::summarise(group_by(av[av$holiday_flg==TRUE], visit_date), mean_visitors = mean(visitors)), aes(visit_date, mean_visitors)) + geom_point(size = 3, colour = "#FF4444") + labs(x = "Visit Date", y = "Mean visitors")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Mean visitors against date (holiday)") + theme(plot.title = element_text(hjust = 0.5))
#Mean visitors against day of week on holidays
ggplot(dplyr::summarise(group_by(av[av$holiday_flg==TRUE], WDay), mean_visitors = mean(visitors)), aes(WDay, mean_visitors)) + geom_point(size = 3, colour = "#FF4444") + labs(x = "Day of the week", y = "Mean visitors")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Mean visitors against day of the week (holiday)") + theme(plot.title = element_text(hjust = 0.5))
#Median visitors against day of week on holidays
ggplot(dplyr::summarise(group_by(av[av$holiday_flg==TRUE], WDay), median_visitors = median(visitors)), aes(WDay, median_visitors)) + geom_point(size = 3, colour = "#FF4444") + labs(x = "Day of the week", y = "Median visitors")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Median visitors against day of the week (holiday)") + theme(plot.title = element_text(hjust = 0.5))
#Mean of all visitors in all restaurants of a particular day of the week against the day of week for non-holidays
combined_mean <- av[holiday_flg == F,.(mean_visitors = mean(visitors)),keyby = WDay]
ggplot(combined_mean, aes(x = WDay, y = mean_visitors)) + geom_point(size = 3, colour = "#FF4444") + labs(x = "Day of the week", y = "Mean visitors")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Mean of total visitors against day of the week") + theme(plot.title = element_text(hjust = 0.5))
#Median of all visitors in all restaurants of a particular day of the week against the day of week for non-holidays
combined_median <- av[holiday_flg == F,.(median_visitors = median(visitors)),keyby = WDay]
ggplot(combined_median, aes(x = WDay, y = median_visitors)) + geom_point(size = 3, colour = "#FF4444") + labs(x = "Day of the week", y = "Median visitors")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Median of total visitors against day of the week") + theme(plot.title = element_text(hjust = 0.5))
#Sum of all visitors in all restaurants against month
ggplot(dplyr::summarise(group_by(av, Month), all_visitors = sum(visitors)), aes(Month, all_visitors)) + geom_point(size = 3, colour = "#FF4444") + labs(x = "Month", y = "All visitors")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Sum of all visitors against month") + theme(plot.title = element_text(hjust = 0.5))

#Sum of all visitors in all restaurant against date
ggplot(dplyr::summarise(group_by(av, visit_date),all_visitors = sum(visitors)), aes(visit_date,all_visitors)) + geom_line(col = "red") + labs(y = "All visitors", x = "Date") + ggtitle("All visitors against date") + theme(plot.title = element_text(hjust = 0.5)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Sum of all visitors against month") + theme(plot.title = element_text(hjust = 0.5))
#Median of visitors in all restaurants against month
ggplot(dplyr::summarise(group_by(av, Month), med_visitors = median(visitors)), aes(Month, med_visitors)) + geom_point(size = 3, colour = "#FF4444") + labs(x = "Month", y = "Median visitors")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Median of total visitors against month") + theme(plot.title = element_text(hjust = 0.5))
#Mean of visitors in all restaurants against month
ggplot(dplyr::summarise(group_by(av, Month), mean_visitors = mean(visitors)), aes(Month, mean_visitors)) + geom_point(size = 3, colour = "#FF4444") + labs(x = "Month", y = "Mean visitors")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Mean of total visitors against month") + theme(plot.title = element_text(hjust = 0.5))

#Comparison between holiday and non-holiday
ggplot(data = av, aes(x = av$holiday_flg, y = av$visitors, color = holiday_flg)) + geom_boxplot() + scale_y_continuous(trans='log10') + labs(x = "Holiday", y = "Visitors")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Visitors (Holiday vs non-holiday)") + theme(plot.title = element_text(hjust = 0.5))
#Comparison between weekend and non-weekend
ggplot(data = av, aes(x = av$Weekend, y = av$visitors, color = Weekend )) + geom_boxplot() + scale_y_continuous(trans='log10') + labs(x = "Weekend", y = "Visitors")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Visitors (Weekend vs non-weekend)") + theme(plot.title = element_text(hjust = 0.5))
#Comparison between mean number of visitors between holiday and non-holidays for the different days of the week
ggplot(dplyr::summarise(group_by(av, WDay, holiday_flg), mean_visitors = mean(visitors)), aes(WDay, mean_visitors, color = holiday_flg)) + geom_point(size = 4) + theme(legend.position = "right") + labs(y = "Average number of visitors", x = "Day of the week") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Average visitors against day of the week") + theme(plot.title = element_text(hjust = 0.5))

#--------------------------------------------------------
# 6.5 Reservations
#--------------------------------------------------------
ggplot(dplyr::summarise(group_by(hr, VisitDate), all_visitors = sum(reserve_visitors)), aes(VisitDate, all_visitors)) + geom_line(colour = "#FF4444") + labs(x = "hpg visit date", y = 'Sum of all reserve visitors') + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Sum of reserve visitors against date") + theme(plot.title = element_text(hjust = 0.5))
#Sum of all visitors for all reservations for a particular hour against hour
ggplot(dplyr::summarise(group_by(hr, VisitHour), all_visitors = sum(reserve_visitors)), aes(VisitHour, all_visitors)) + geom_col(fill = "#FF4444") + labs(x = "Hour", y = 'Sum of all reserve visitors') + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Sum of reserve visitors against hour") + theme(plot.title = element_text(hjust = 0.5))
#Sum of all visitors against the hour difference between reservation and visit date_time
ggplot(dplyr::summarise(group_by(filter(hr, HourDifference < 24*5), HourDifference), all_visitors = sum(reserve_visitors)), aes(HourDifference, all_visitors)) + geom_col(fill = "#FF4444") + labs(x = "Time from reservation to visit [hours]", y = "Sum of all reserve visitors") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Sum of reserve visitors against hour difference") + theme(plot.title = element_text(hjust = 0.5))
#Plot of total number of reservation visitors and number of actual visitors
ggplot(all_reserve[reserve_visitors < 120], aes(reserve_visitors, visitors)) + geom_point(color = "black", alpha = 0.5) + geom_abline(slope = 1, intercept = 0, color = "#FF4444") + geom_smooth(method = "lm", color = "blue") + labs(x = "Reserve visitors", y = "Actual visitors") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Actual visitors against reserve visitors") + theme(plot.title = element_text(hjust = 0.5))

#============================================================
# 7. Models
#============================================================
#--------------------------------------------------------
# 7.1 Correlation Plot
#--------------------------------------------------------
# Corrplot
corrplot(cor(corr), type = "upper")  
#correlation plot of all the numeric variables in trainset

corrplot(cor(corr[,1:12])[1:12,2, drop=FALSE])  
#correlation of just visitors with the other numeric variables in trainset

#--------------------------------------------------------
# 7.2 Linear Regression Model
#--------------------------------------------------------
options(scipen = 10)
m1 <- lm(visitors ~ . , data = train.dt)
summary(m1)
m2 <- step(m1)
summary(m2)
#Good adjusted R square of 0.6326

par(mfrow = c(2,2))
plot(m2)  # Plot model 2 diagnostics
par(mfrow = c(1,1))

m3 <- stepAIC(m1, direction = "forward", trace = FALSE)

summary(m3)
AIC(m2) #543143.2
AIC(m3) #543143.2
#Both models m2 and m3 have the same AIC and differs only by one variable (longitude)

vif(m1)
#Issues of collinearity:
#Some variables have very high adjusted gvif values and
#may be removed in the next model
#Remove variables with high GVIF
m4 <- lm(visitors ~ . - year - mean_visitors - median_visitors - reserve_date, data = train.dt)
summary(m4)
#Lower adjusted R square of 0.5636
par(mfrow = c(2,2))
plot(m4)  # Plot model 4 diagnostics
par(mfrow = c(1,1))

vif(m4)
#Error message of aliased coefficients in the model, which is likely due to perfect multicollinearity
#between longitude and latitude
#Remove latitude and longitude from the model
m4 <- lm(visitors ~ . - year - mean_visitors - median_visitors - reserve_date - latitude - longitude, data = train.dt)
summary(m4)
#Lower adjusted R square of 0.5632
par(mfrow = c(2,2))
plot(m4)  # Plot model 4 diagnostics
par(mfrow = c(1,1))

vif(m4)
#No multicollinearity issues
residuals(m4)

RMSE.m4.train <- sqrt(mean(residuals(m4)^2))  # RMSE on trainset based on m4 model.
summary(abs(residuals(m4)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.m4.test <- predict(m4, newdata = test.dt)
testset.error <- test.dt$visitors - predict.m4.test

# Testset Errors
RMSE.m4.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))
RMSE.m4.test/mean(testset$visitors) #0.3947617

#Finding relative importance of variables in m4
relaimpo::calc.relimp.lm(m2)

#--------------------------------------------------------
# 7.3 CART
#--------------------------------------------------------
options(digits = 3)

#Creating our first tree using the entire data set
cart1 <- rpart(visitors ~ ., data = cart.train, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
print(cart1)
printcp(cart1, digits = 3)
plotcp(cart1)

cp.opt <- cart1$cptable[which.min(cart1$cptable[,"xerror"]),"CP"]
cp.opt

cart1a <- prune(cart1, cp = cp.opt)
print(cart1a)
printcp(cart1a, digits = 3)
plotcp(cart1a)

cart1a$variable.importance

#Tree is be too big
#make cp slightly greater than 0, to prevent the tree from growing right to the end

cart2 <- rpart(visitors ~ ., data = cart.train, method = 'anova', control = rpart.control(minsplit = 2, cp = 0.01))
print(cart2)
printcp(cart2, digits = 3)
plotcp(cart2)

cp.opt <- cart2$cptable[which.min(cart2$cptable[,"xerror"]),"CP"]

#Prune cart2 to obtain cart3
cart3 <- prune(cart2, cp = cp.opt)
print(cart3)
printcp(cart3, digits = 3)

cart3$variable.importance

#including only the variables used in the final lin reg model earlier
cart4 <- rpart(visitors ~ month + day_of_week + holiday_flg + min_visitors + max_visitors + air_genre_name + air_area_name + reserve_visitors + reserve_datetime_diff, data = cart.train, method = 'anova', control = rpart.control(minsplit = 2, cp = 0.01))

print(cart4)
printcp(cart4, digits = 3)
plotcp(cart4)

cp.opt2 <- cart4$cptable[which.min(cart4$cptable[,"xerror"]),"CP"]

#Prune cart4 to obtain cart5
cart5 <- prune(cart4, cp = cp.opt2)
print(cart5)
printcp(cart5, digits = 3)

# Plotting the final tree cart 5
prp(cart5, type=2, nn=T, fallen.leaves=T, branch.lty=3, nn.box.col = 'light blue', min.auto.cex = 0.1, nn.cex = 0.6, split.cex = 1.1, shadow.col="grey")

cart5$variable.importance

#--------------------------------------------------------
# 7.4 Random Forest using h2o package
#--------------------------------------------------------
# Install h2o package
if(!require(h2o)) {
  install.packages("h2o")
}
##Things to note
# 1. You will have to install the latest version of Java RE 1.8.x or JDK 1.8.x 
#    for your OS in order to run the local H2O server instance
# 2. H2O is not compatible with the latest Java Runtime platform (JRE 1.9.x or JDK 1.9.x)

library(h2o)
# start the local h20 instance
# set nthreads=-1 if not sure multithreading works well on your computer
# adjust max memory allocation as per your hardware configuration
h2o.init(nthreads=4, max_mem_size="4G")  
h2o.removeAll() ## clean slate - just in case the cluster was already running

#Remove day_of_week column since there is already a dow column
h2o.dt$day_of_week <- NULL

#Split into 70% train and 30% test
train.h2o <- sample.split(Y = h2o.dt$visitors, SplitRatio = 0.7)
trainset.h2o <- subset(h2o.dt, train.h2o == T)
testset.h2o <- subset(h2o.dt, train.h2o == F)

# import pre-processed training and testing sets into h2O
trainHex <-as.h2o(trainset.h2o)
testHex  <-as.h2o(testset.h2o)

# further split trainHex into purely training and validation parts
#to perform cross validation on random forest and deep learning model
#so as to reduce over-fitting
splits <- h2o.splitFrame(trainHex, c(0.70), seed=278)
train  <- h2o.assign(splits[[1]], "train.hex") # 70%
valid  <- h2o.assign(splits[[2]], "valid.hex") # 30% 

response <- "visitors"
predictors <- setdiff(names(train), response)

# Number of CV folds
nfolds <- 5

my_rf <- h2o.randomForest(x = predictors,
                          y = response,
                          training_frame = train,
                          validation_frame = valid,
                          ntrees = 50,
                          nfolds = nfolds,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)
h2o.performance(my_rf, train=T)          ## sampled training data 
h2o.performance(my_rf, valid=T)          ## sampled validation data 
h2o.performance(my_rf, newdata=train)    ## full training data
h2o.performance(my_rf, newdata=valid)    ## full validation data
h2o.performance(my_rf, newdata=testHex)  ## full test data
summary(my_rf)
#Find the relative importance of the variables
h2o.varimp(my_rf)
h2o.rmse(my_rf)/mean(h2o.dt$visitors) #0.289
pred <- h2o.predict(my_rf, newdata = valid)
#Shut down h2o
h2o.shutdown()
y