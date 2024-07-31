#Code to import climate data from three weather stations - Clarksville TN, Tuscaloosa AL, and Tell City IN
#Climate stations have long-term records and are near collection sites for study investigating latitudinal differences in freezing
#tolerance

#Created January 21, 2022 by Evan Rehm
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
#import datasets - file path will be unique to your computer
#load data
#clarksville - Sewage plant
cla<-read.csv("~/OneDrive - Austin Peay State University/Rehm lab/Trees/Data/Climate data/Clarksville station/2850632.csv")
#Tuscaloosa - Bankhead Dam
tus<-read.csv("~/OneDrive - Austin Peay State University/Rehm lab/Trees/Data/Climate data/Tuscaloosa station/2850724.csv")
#Tell City - Turn this one off and look at Williams IN which is ~70 miles further north
#tel<-read.csv("~/OneDrive - Austin Peay State University/Rehm lab/Trees/Data/Climate data/Tell City Indiana/2850677.csv")
#Williams IN
tel<-read.csv("~/OneDrive - Austin Peay State University/Rehm lab/Trees/Data/Climate data/Williams IN/2852795.csv")

#Tuscaloosa has the shortest record - Use Tell City to cut down the records from the other stations
#Where is the first row with an actual temperature for Tell City
tus<-tus[complete.cases(tus$TOBS)!=0,]
#The earliest date across all datasets
early<-tus$DATE[1]

#limit the clarksville and tell city data to only that that overlaps Tuscaloosa
cla<-cla[which(cla$DATE>=early),]
tel<-tel[which(tel$DATE>=early),]

#remove unneccessary columns
#tel<-tel[,c(2,3,11,12,13)]
tel<-tel[,c(2,3,9,10,11)]
cla<-cla[,c(2,3,9,10,11)]
tus<-tus[,c(2,3,9,10,11)]

#Put the Date column into date format instead of character
tel$DATE<-as.Date(tel$DATE)
cla$DATE<-as.Date(cla$DATE)
tus$DATE<-as.Date(tus$DATE)

#add a column for easier site ID
tel$site<-'IN'
cla$site<-'TN'
tus$site<-'AL'

#keep only those dates that match across all data.frames
date<-as.data.frame(as.Date(tus$DATE))#tus has the fewest days so need to restrict based on this
colnames(date)[1]<-'DATE'

tel <- merge(date, tel, by.y = "DATE", by.x = "DATE", all.x = TRUE)
cla <- merge(date, cla, by.y = "DATE", by.x = "DATE", all.x = TRUE)

#Switch from wide to long format and then join all the dataframes together
tus<-gather(tus,temp,celcius,TMAX:TOBS)
tel<-gather(tel,temp,celcius,TMAX:TOBS)
cla<-gather(cla,temp,celcius,TMAX:TOBS)

temp<-rbind(tus,tel,cla)

#make unique columns for day month and year
temp = temp %>% 
  mutate(date = ymd(DATE)) %>% #ymd command from lubridate package
  mutate_at(vars(DATE), list(year=year, month=month, day=day))

temp$Julian<-yday(temp$DATE)

#NAs are in the NAME column, need to remove these rows
temp<-temp %>% drop_na(NAME)

#look at lowest temperature per year over time at each site
low<-subset(temp,temp=="TMIN")%>%#uses only the TMIN data
  group_by(site,year)%>%
  summarise(low=min(celcius,na.rm=TRUE))

#make plot of lowest temperature per year for each of the sites
ggplot(low,aes(year,low,group=site,color=site))+
  geom_line()+
  theme_classic()+
  ylab("Lowest temperature per year")
#Alabama is obviously less severe but TN and IN are pretty close together

#investigate the date of the last frost in spring
#getting last day of last frost below 0, -2,-5 and -8
lowest<-subset(temp,temp=="TMIN")#create a dataframe of only minimum temperatures
spring<-lowest%>%#use only the minimum temperature data
  filter(month<=6)%>%#use only dates before July 1
  #filter(year>=1990)%>%#use only years after 1990
  filter(celcius<=-1)%>%#can change this to whatever temperature you like, currently I am taking only temperature less than 0C
  group_by(year,site)%>%#group within a year and by site so we get the latest temp per year
  slice(tail(row_number(),1))%>%#slicing out the last date when temperature is below 0C
  summarise(last_spring=last(celcius),
            month=month,#keeping the month variable in the new dataframe
            day=day,#keeping the day variable in the new dataframe
            DATE=DATE,#keeping the DATE variable in the new dataframe
            Julian=Julian)#keeping the Julian variable in the new dataframe

#Remove 2022 as data are incomplete
spring<-spring[which(spring$year!=2022),]

#plot last freezing date over time
ggplot(spring,aes(year,Julian,group=site,color=site))+
  geom_line()+
  theme_classic()+
  ylab("Julian date of last -1C temperature")

#Average date of last freezing date
spring%>%
  group_by(site)%>%
  summarise(ave=mean(Julian))

#Average date of last freezing date since 2000
spring%>%
  filter(year>1999)%>%
  group_by(site)%>%
  summarise(ave=mean(Julian))

ggplot(spring,aes(year,dayofyear))+
  geom_point()+
  geom_line()+
  geom_smooth(method=lm)

#calculate last spring frost as a moving average (5-year moving average)
move_ave<-spring%>%
  select(year,dayofyear)%>%
  mutate(srate=rollmean(dayofyear,k=5,fill=NA))#change k to determine interval
#remove 2021 as data are not complete
