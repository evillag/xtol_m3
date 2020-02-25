#install.packages("RMySQL")
#install.packages("hrbrthemes")
#install.packages("extrafontdb")
#install.packages("Rttf2pt1")
library(RMySQL)
library(dplyr)
library(ggplot2)
library(lubridate)
library(hrbrthemes)
library(viridis)
library(plotly)

con = dbConnect(
  MySQL(),
  user = 'deepAnalytics',
  password = 'Sqltask1234!',
  dbname = 'dataanalytics2018',
  host = 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com'
)

## List the tables contained in the database
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con, 'yr_2010')

## Use asterisk to specify all attributes for download
#power_2006 <- dbGetQuery(con, "SELECT * FROM yr_2006")
power_2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
power_2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
power_2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
#power_2010 <- dbGetQuery(con, "SELECT * FROM yr_2010")

str(power_2007)
str(power_2008)
str(power_2009)

summary(power_2007)
summary(power_2008)
summary(power_2009)

dim(power_2007) # approx 521k records
dim(power_2008) # approx 526k records
dim(power_2009) # approx 521k records

## Check for NA values
anyNA(power_2007)
anyNA(power_2008)
anyNA(power_2009)

# Merge datasets
power_df <-
  bind_rows(power_2007, power_2008, power_2009)

# Create DateTime Column
## Combine Date and Time attribute values in a new attribute column
power_df <-cbind(power_df,paste(power_df$Date,power_df$Time), stringsAsFactors=FALSE)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
summary(power_df)
colnames(power_df)[11] <-"DateTime"
glimpse(power_df)

power_df <- power_df[,c(ncol(power_df), 1:(ncol(power_df)-1))]
glimpse(power_df)

## Convert DateTime from character to POSIXct 
power_df$DateTime <- as.POSIXct(power_df$DateTime, "%Y/%m/%d %H:%M:%S")
## Add the time zone
attr(power_df$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(power_df)

## Create "year" attribute with lubridate
power_df$year <- year(power_df$DateTime)
power_df$quarter <- quarter(power_df$DateTime)
power_df$month <- month(power_df$DateTime)
power_df$week <- week(power_df$DateTime)
power_df$weekday <- weekdays(power_df$DateTime, abbreviate = T)
power_df$day <- day(power_df$DateTime)
power_df$hour <- hour(power_df$DateTime)
power_df$minute <- minute(power_df$DateTime)

glimpse(power_df)
str(power_df)

## Subset the second week of 2008 - All Observations
houseWeek <- filter(power_df, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)

# Visualize a Single Day with Plotly

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(power_df, year == 2008 & month == 1 & day == 9)
houseDay
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$Time, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines') 


print("Sub_metering_1: dishwasher, an oven and a microwave")
print("Sub_metering_2: washing-machine, a tumble-drier, a refrigerator and a light")
print("Sub_metering_3: dishwasherwater-heater and an air-conditioner")





##########################################################################
##########################################################################

sum_year <- power_all %>%
  filter(year > 2006) %>%
  group_by(year, week_day) %>%
  summarize(
    median_gap = median(Global_active_power),
    total_gap = sum(Global_active_power)
  )

sum_year

ggplot(sum_year, aes(y = total_gap, x = week_day, color = week_day)) +
  geom_boxplot()

by_hour <- power_all %>%
  filter(year > 2006) %>%
  group_by(week_day, hour) %>%
  summarize(
    median_gap = median(Global_active_power),
    total_gap = sum(Global_active_power)
  )

ggplot(by_hour,
       aes(
         y = week_day,
         x = hour,
         size = total_gap,
         color = total_gap
       )) +
  geom_point() +
  scale_color_gradient(low = "yellow", high = "red")

########### Submeters behavior ###########
sub_meters <- power_all %>%
  filter(year > 2006) %>%
  group_by(year) %>%
  mutate(
    Sub_metering_rest = (Global_active_power * 1000 / 60) - Sub_metering_1 - Sub_metering_2 - Sub_metering_3
  ) %>%
  summarize(
    totalMetering_1 = sum(Sub_metering_1),
    totalMetering_2 = sum(Sub_metering_2),
    totalMetering_3 = sum(Sub_metering_3),
    totalMetering_rest = sum(Sub_metering_rest)
  )
sub_meters

# Consumption per year
ggplot(sub_meters, aes(x = year)) +
  geom_line(aes(y = totalMetering_1), color = "firebrick") +
  geom_line(aes(y = totalMetering_2), color = "navyblue") +
  geom_line(aes(y = totalMetering_3), color = "orange") +
  geom_line(aes(y = totalMetering_rest), color = "black")
###########

## Consuption of A/C, Heater per month ###
## energy sub-metering No. 3 
ac_per_month <- power_all %>%
  group_by(month) %>%
  summarize(Total_AC_Heater = sum(Sub_metering_3))

ac_per_month$month_factor <- factor(ac_per_month$month, levels = c(1:12), ordered = T)

ggplot(ac_per_month, aes(x=month_factor, y=Total_AC_Heater)) +
  geom_bar(stat="identity", color="yellow", fill="firebrick") + 
  scale_x_discrete(name ="Month", limits=c(1:12)) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  theme_ipsum()  +
  theme(legend.position="none") +
  ggtitle("Historical A/C and Heater Power Consumption per month")

## Consuption ofhe kitchen, containing mainly a dishwasher, 
## an oven and a microwave (hot plates are not electric but gas powered).  
## energy sub-metering No. 1 
kitchen_per_month <- power_all %>%
  group_by(month) %>%
  summarize(Total_Kitchen = sum(Sub_metering_1))

kitchen_per_month$month_factor <- factor(kitchen_per_month$month, levels = c(1:12), ordered = T)

ggplot(kitchen_per_month, aes(x=month_factor, y=Total_Kitchen)) +
  geom_bar(stat="identity", color="orange", fill="blue") + 
  scale_x_discrete(name ="Month", limits=c(1:12)) +
  labs(y="Total power consumed")+
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  theme_ipsum()  +
  theme(legend.position="none") +
  ggtitle("Historical Kitchen Power Consumption per month")


## Consuption of the laundry room, containing a washing-machine,
## a tumble-drier, a refrigerator and a light. 
## energy sub-metering No. 2 
laundry_per_month <- power_all %>%
  group_by(month) %>%
  summarize(Total_Laundry_Room = sum(Sub_metering_2))

laundry_per_month$month_factor <- factor(laundry_per_month$month, levels = c(1:12), ordered = T)

ggplot(laundry_per_month, aes(x=month_factor, y=Total_Laundry_Room)) +
  geom_bar(stat="identity", color="black", fill="deepskyblue3") + 
  scale_x_discrete(name ="Month", limits=c(1:12)) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  theme_ipsum()  +
  labs(y="Total power consumed")+
  theme(legend.position="none") +
  ggtitle("Historical Laundry Room Power Consumption per month")

##########################################################################



