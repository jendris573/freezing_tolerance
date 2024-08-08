#calculating thermal safety margins based on LT50 and minimum temperatures
#experienced +/- 7 days around each sampling date

library(ggplot2)
library(readxl)
library(tidyverse)
library(caTools)#moving window calculation

tenn_clim<-read.csv("data/Tennessee_climate.csv")

#keep only sewage plant
tenn_clim <- tenn_clim%>%filter(STATION=="USC00401790")
#change date to date
tenn_clim$DATE<-mdy(tenn_clim$DATE)

#create column for year
tenn_clim <- mutate(tenn_clim, year=year(tenn_clim$DATE))

#create column for month
tenn_clim <- mutate(tenn_clim, month=month(tenn_clim$DATE))

## create column for julian date##
tenn_clim$julian_date <- yday(tenn_clim$DATE)

#omit NA in TMIN recordings 
tenn_clim<-tenn_clim[complete.cases(tenn_clim[,9]),]

#filter for 1980-2022
tenn1980 <- tenn_clim %>%
  filter(year>1979) %>%
  filter(year<2024)

#Calculate the minimum temperature for each day and year on a rolling +/- 7 day average
temp<-tenn1980%>%
  group_by(year)%>%
  mutate(what=runmin(TMIN,14))

#import LT50 values
LT50<-read_excel("data/LT50 master.xlsx")
