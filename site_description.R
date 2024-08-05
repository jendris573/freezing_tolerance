### Statistical analysis for Clarksville climate
### Written by Joe Endris

# # # # # # # # #
## Libraries ----
# # # # # # # # #

library(readxl)
library(writexl)
library(fitdistrplus)
library(lubridate)
library(MuMIn)
library(dplyr)
library(pracma)
library(multcomp)
library(ggplot2)
library(gridExtra)

# # # # # # # # # # # # #
## Data Preparation ----
# # # # # # # # # # # # #

#Load NOAA Climate Data Online data
tenn_clim<-read_excel("data/tenn1980.xlsx")

#create column for year
tenn_clim <- mutate(tenn_clim, year=year(tenn_clim$DATE))

#create column for month
tenn_clim <- mutate(tenn_clim, month=month(tenn_clim$DATE))

## create column for julian date##
tenn_clim$julian_date <- yday(tenn_clim$DATE)

#omit NA in precipitation recordings 
tenn_precip<-tenn_clim[complete.cases(tenn_clim[,4]),]
#omit NA in TMAX recordings 
tenn_TMAX<-tenn_clim[complete.cases(tenn_clim[,5]),]
#omit NA in TMIN recordings 
tenn_TMIN<-tenn_clim[complete.cases(tenn_clim[,6]),]

# # # # # # # # # # # # # #
## Climate data points ----
# # # # # # # # # # # # # #

#determine annual precipitation values
precip <- tenn_precip %>%
  group_by(year) %>%
  dplyr::summarise(annual_precip = sum(PRCP))

#average annual TMAX
TMAX <- tenn_TMAX %>%
  group_by(year) %>%
  dplyr::summarise(annual_TMAX = mean(TMAX))

#average annual TMIN
TMIN <- tenn_TMIN %>%
  group_by(year) %>%
  dplyr::summarise(annual_TMIN = mean(TMIN))

#create one data frame with all the data
climate <- cbind(precip, TMAX$annual_TMAX, TMIN$annual_TMIN) %>%
  rename("TMAX" = "TMAX$annual_TMAX",
         "TMIN" = "TMIN$annual_TMIN")

#calculate the mean high temperature
mean_TMAX <-   climate %>%
  dplyr::summarise(annual_TMAX = mean(TMAX))
#calculate the mean low temperature
mean_TMIN <-   climate %>%
  dplyr::summarise(annual_TMIN = mean(TMIN))
#calculate the mean precipitation
mean_precip <-   climate %>%
  dplyr::summarise(mean_precip = mean(annual_precip))
