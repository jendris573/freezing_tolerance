#No need to run this script again unless changing the phenology code or GDD
#Data and cleaning script
#created and ran on 8/2/24
#combine LT50, GDD and julian date for all individuals

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readxl)

########################
### data preparation ###
########################

#read in raw data for LT50 values
outputs<-read_excel("data/LT50 master.xlsx")
#order by date and species
outputs<-outputs[with(outputs,
  order( year,julian_date)),
]

#create column for julian date
outputs$julian_date <- yday(outputs$Date)

#create column for month
outputs <- mutate(outputs, month=month(outputs$Date))

#create column for year
outputs <- mutate(outputs, year=year(outputs$Date))

outputs<-outputs%>%
  filter(State=="TN")

#import growing degree days and phenology estimates
phen<-read.csv("data/DOY_phenology.csv")
#calculate mean values per group
phen<-phen%>%
  group_by(species,year)%>%
  summarize(mean1=mean(ph1),
            mean2=mean(ph2),
            mean3=mean(ph3),
            mean4=mean(ph4))

#manually give phenology scores
outputs$phen<-NA
outputs$phen<-ifelse(outputs$julian_date<76,0,outputs$phen)
#now tough part
#need to add phenology score for each species for each period based on mean values
#only count as next phenology score if they have passed that day for that phenology score
outputs$phen[55:66]<-1
outputs$phen[67:72]<-2
outputs$phen[73:84]<-2
outputs$phen[85:108]<-3
#2023
outputs$phen[163:180]<-2
outputs$phen[181:186]<-3
outputs$phen[187:192]<-2
outputs$phen[193:216]<-3

outputs<-outputs[,-c(13:14)]
#import GDD and add this as a column as well
GDD<-read_excel("data/climate_GDD.xlsx")
GDD<-GDD[,c(12,14,16)]

outputs<-merge(outputs,GDD,by.x=c("julian_date","year"),by.y=c("julian_date","year"),all.x=TRUE)

write_xlsx(outputs,"data/LT50 master.xlsx")
