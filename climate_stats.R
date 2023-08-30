### Statistical analysis for Clarksville Climate
### Written by Joe Endris
### With input from Dr Evan Rehm

library(readxl)
library(fitdistrplus)
library(lubridate)
library(MuMIn)
library(dplyr)
library(pracma)
library(multcomp)
library(ggplot2)

########################
### Data Preparation ###
########################

#Load NOAA Climate Data Online data
tenn_clim<-read.csv("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/Tennessee_climate.csv")

#keep only sewage plant
tenn_clim <- tenn_clim%>%filter(NAME=="CLARKSVILLE SEWAGE PLANT, TN US")

#omit NA in temperature recordings 
tenn_clim<-tenn_clim[complete.cases(tenn_clim[,8]),]

#create column for year
tenn_clim <- mutate(tenn_clim, year=year(tenn_clim$DATE))

#create column for month
tenn_clim <- mutate(tenn_clim, month=month(tenn_clim$DATE))

## create column for julian date##
tenn_clim$julian_date <- yday(tenn_clim$DATE)

##########################
### Statistical Models ###
##########################

clim_mod1 <- glm(julian_date ~ TMIN, data=tenn_clim)

summary(clim_mod1)
dredge(clim_mod1)


