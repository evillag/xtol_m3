library(dplyr)
library(lubridate)

ds_pwr <- read.csv(file = "data/household_power_consumption.txt", sep = ";")

dim(ds_pwr)

glimpse(ds_pwr)

anyNA(ds_pwr)

str(ds_pwr)

summary(ds_pwr)

head(ds_pwr, n=20)
tail(ds_pwr, n=20)

# Fix datatypes
ds_pwr$Date <- dmy(ds_pwr$Date)
ds_pwr$Time <- hms(ds_pwr$Time)

# Handle NA's
dim(ds_pwr %>% filter(Global_active_power == "?"))

ds_pwr$Global_active_power[ds_pwr$Global_active_power == "?"] <- NA
ds_pwr$Global_active_power <- as.numeric(ds_pwr$Global_active_power)

ds_pwr$Global_reactive_power[ds_pwr$Global_reactive_power == "?"] <- NA
ds_pwr$Global_reactive_power <- as.numeric(ds_pwr$Global_reactive_power)

ds_pwr$Global_intensity[ds_pwr$Global_intensity == "?"] <- NA
ds_pwr$Global_intensity <- as.numeric(ds_pwr$Global_intensity)

ds_pwr$Voltage[ds_pwr$Voltage == "?"] <- NA
ds_pwr$Voltage <- as.numeric(ds_pwr$Voltage)

dim(ds_pwr %>% filter(Sub_metering_1 == "?"))
ds_pwr$Sub_metering_1[ds_pwr$Sub_metering_1 == "?"] <- NA
ds_pwr$Sub_metering_1 <- as.numeric(ds_pwr$Sub_metering_1)

ds_pwr$Sub_metering_2[ds_pwr$Sub_metering_2 == "?"] <- NA
ds_pwr$Sub_metering_2 <- as.numeric(ds_pwr$Sub_metering_2)

ds_pwr$Sub_metering_2[ds_pwr$Sub_metering_3 == "?"] <- NA
ds_pwr$Sub_metering_2 <- as.numeric(ds_pwr$Sub_metering_3)

summary(ds_pwr)
glimpse(ds_pwr)
summary(ds_pwr$Sub_metering_1)
summary(ds_pwr$Sub_metering_2)
summary(ds_pwr$Sub_metering_3)


# Lets start plotting
hist(ds_pwr$Sub_metering_1)
hist(ds_pwr$Sub_metering_2)
hist(ds_pwr$Sub_metering_3)
hist(ds_pwr$Global_active_power)

hist(ds_pwr$Date, "years", freq = TRUE)
hist(ds_pwr$Date, "months", freq = TRUE, )

ds_pwr$year <- year(ds_pwr$Date)
glimpse(ds_pwr)

ds_pwr %>%
  filter(year == 2006) %>%
  dim()
  #arrange(desc(Global_active_power))

ds_pwr %>%
  filter(year == 2007) %>%
  dim()

ds_pwr %>%
  filter(year == 2008) %>%
  dim()

ds_pwr %>%
  filter(year == 2009) %>%
  dim()

ds_pwr %>%
  filter(year == 2010) %>%
  dim()


# Create a new variable to store the energy not accounted for by the sub-meters
head(ds_pwr %>%
  filter(year > 2006 & year < 2010) %>%
  mutate(rest_of_house_metering = Global_active_power*1000/60)

glimpse(ds_pwr)


