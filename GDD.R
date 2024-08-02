#calculate accumulated growing degree day for each day of year for each year

#Created July 31 2024 by Evan Rehm
#load packages
library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(writexl)

#Load NOAA Climate Data Online data
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

#calculate GDD as (Tmax-Tmin)/2 - base
#in our case we will use a base of 5C

#keep just pre July 1 dates
tenn1980<-tenn1980%>%
  filter(julian_date<213)

#calculate GDD from first step
tenn1980$GDDmaxmin<-((tenn1980$TMAX+tenn1980$TMIN)/2)-5
#change negatives to zero
tenn1980$GDDmaxmin<-ifelse(tenn1980$GDDmaxmin<0,0,tenn1980$GDDmaxmin)

#calculate cumulative GDD - most common method
tenn1980<-tenn1980%>%
  group_by(year)%>%
  mutate(GDDcumsum=cumsum(GDDmaxmin))

#werite the GDD data
write_xlsx(tenn1980, "data/climate_GDD.xlsx")
