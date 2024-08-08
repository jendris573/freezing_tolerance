#No need to run this script again unless changing the phenology code or GDD
#Data and cleaning script
#created and ran on 8/2/24
#Ran again 8/8/2024 after changing phenology data for 2023 to subsample only certain dates
#to match sampling intensity of 2022
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
outputs$phen<-ifelse(outputs$julian_date<71,0,outputs$phen)
#now tough part
#need to add phenology score for each species for each period based on mean values
#only count as next phenology score if they have passed that day for that phenology score
outputs$phen[37:48]<-0

outputs$phen[49:66]<-1
outputs$phen[67:72]<-2
outputs$phen[73:84]<-2
outputs$phen[85:102]<-3
outputs$phen[103:108]<-4
#2023
outputs$phen[145:156]<-0
outputs$phen[157:162]<-1
outputs$phen[163:180]<-2
outputs$phen[181:216]<-3

outputs<-outputs[,-c(13)]
#import GDD and add this as a column as well
GDD<-read_excel("data/climate_GDD.xlsx")
GDD<-GDD[,c(12,14,16)]

outputs<-merge(outputs,GDD,by.x=c("julian_date","year"),by.y=c("julian_date","year"),all.x=TRUE)

write_xlsx(outputs,"data/LT50 master.xlsx")
