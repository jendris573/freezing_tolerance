### Specific code for analyzing and extracting phenology values
#Code developed in response to reviewers from Scientific Reports
#Created 8/1/24 by Evan Rehm

library(dplyr)
library(tidyr)
library(ggplot2)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(gridExtra)
library(MuMIn)

#edit the below code when its time to save actual figures
##ggsave(filename, plot = last_plot(),device = png(),path = NULL, scale = 1, +
#width = NA, height = NA, units = c("in"), dpi = 300, limitsize = TRUE, bg = NULL)

########################
### data preparation ###
########################
#read in phenology observations
phenology<-read_excel("data/phenology_check.xlsx")

#create column for year
phenology <- mutate(phenology, year=year(date))

#create column for julian date
phenology$julian_date <- yday(phenology$date)

#filter out 2021 data since there is no corresponding LT50 data for 2021
phenology <- filter(phenology, year > "2021")

#omit any blank spots in the mean_phenology column
phenology <- phenology[complete.cases(phenology[,4]),]

ggplot(data=phenology,aes(x=julian_date,y=phenology))+
         geom_point()+
         facet_wrap(~species)

#match phenology with GDD
clim<-read_excel("data/climate_GDD.xlsx")
slim_sub<-clim[,c(12,14,16)]

#add GDD data to phenology data
phenology<-merge(phenology,slim_sub,by.x=c('year','julian_date'),by.y=c('year','julian_date'),all.x=TRUE)
#fill missing values as midpoint
phenology[c(151:175),9]<-122.15

ggplot(data=phenology,aes(x=GDDcumsum,y=phenology))+
  geom_point()+
  facet_wrap(~species)

############loop to fit sigmoid curve to each individual in each year and extract###############
#the GDD value for that specific combination###########################
#remove unneeded species
phenology<-phenology%>%filter(species!="Ostrya virginiana")
phenology<-phenology%>%filter(species!="Quercus alba")

phenology$id<-paste(phenology$species,phenology$year,phenology$number,sep='_')
phenology<-phenology[
  order( phenology[,10], phenology[,9] ),
]
#empty dataframe to write prediction values to
out<-data.frame(matrix(ncol = 5, nrow = 0))
colnames(out) <- c("id", "ph1", "ph2","ph3","ph4")

#acer23<-acer%>%filter(year==2023)#filter to a single year
#acer23<-acer23%>%filter(number==1)
for(i in 1:length(unique(phenology$id))){
  who<-unique(phenology$id)[i]
  temp<-phenology[which(phenology$id==who),]
fit <- nls(phenology ~ SSlogis(GDDcumsum, Asym, xmid, scal), data = temp,
           control=nls.control(maxiter=500,warnOnly=TRUE,tol=10))#fit model
#summary(fit)
newdata = data.frame(GDDcumsum = seq(35, 600, length.out = 1000))#new data for which to fit

newdata$prediction<-predict(fit, newdata )
out[i,1]=temp$id[1]
out[i,2]<-newdata[which.min(abs(newdata$prediction - 1)),][1]#predict GDD for bud stage 1
out[i,3]<-newdata[which.min(abs(newdata$prediction - 2)),][1]#stage 2
out[i,4]<-newdata[which.min(abs(newdata$prediction - 3)),][1]#stage 3
out[i,5]<-newdata[which.min(abs(newdata$prediction - 3.9)),][1]#stage 4 - note this number is invalid at the curve can never reach 4
}

#make some figures of the outputs
out<-out%>%separate_wider_delim(id,"_",names=c('species','year','individual'))

write.csv(out,"data/GDD_phenology.csv",row.names=FALSE)
GDDph2<-ggplot(out,aes(x=year,y=ph2))+
         geom_boxplot()+
  ylab("GDD for phase 2")+
         facet_wrap(~species)
GDDph3<-ggplot(out,aes(x=year,y=ph3))+
  geom_boxplot()+
  ylab("GDD for phase 3")+
  facet_wrap(~species)
########################################################
############loop to fit sigmoid curve to each individual in each year and extract###############
#the GDD value for that specific combination###########################
#####DOY ~~~~
#remove unneeded species
#empty dataframe to write prediction values to
out2<-data.frame(matrix(ncol = 5, nrow = 0))
colnames(out2) <- c("id", "ph1", "ph2","ph3","ph4")

#acer23<-acer%>%filter(year==2023)#filter to a single year
#acer23<-acer23%>%filter(number==1)
for(i in 1:length(unique(phenology$id))){
  who<-unique(phenology$id)[i]
  temp<-phenology[which(phenology$id==who),]
  fit <- nls(phenology ~ SSlogis(julian_date, Asym, xmid, scal), data = temp,
             control=nls.control(maxiter=500,warnOnly=TRUE,tol=10))#fit model
  #summary(fit)
  newdata = data.frame(julian_date = seq(1, 150, length.out = 150))#new data for which to fit
  
  newdata$prediction<-predict(fit, newdata )
  out2[i,1]=temp$id[1]
  out2[i,2]<-newdata[which.min(abs(newdata$prediction - 1)),][1]#predict GDD for bud stage 1
  out2[i,3]<-newdata[which.min(abs(newdata$prediction - 2)),][1]#stage 2
  out2[i,4]<-newdata[which.min(abs(newdata$prediction - 3)),][1]#stage 3
  out2[i,5]<-newdata[which.min(abs(newdata$prediction - 3.9)),][1]#stage 4 - note this number is invalid at the curve can never reach 4
}

#make some figures of the outputs
out2<-out2%>%separate_wider_delim(id,"_",names=c('species','year','individual'))

write.csv(out2,"data/DOY_phenology.csv",row.names=FALSE)
DOYph2<-ggplot(out2,aes(x=year,y=ph2))+
  geom_boxplot()+
  ylab("Julian Day for phase 2")+
  facet_wrap(~species)
DOYph3<-ggplot(out2,aes(x=year,y=ph3))+
  geom_boxplot()+
  ylab("Julian Day for phase 3")+
  facet_wrap(~species)

grid.arrange(GDDph2,GDDph3,DOYph2,DOYph3,ncol=2)
###################################################
###################################################
###################################################
#read in raw data for LT50 values
outputs<-read_excel("data/LT50 master.xlsx")
outputs<-outputs%>%filter(State=="TN")

#create column for julian date
outputs$julian_date <- yday(outputs$Date)

#create column for month
outputs <- mutate(outputs, month=month(outputs$Date))

#create column for year
outputs <- mutate(outputs, year=year(outputs$Date))


###################################################
###################################################
###################################################
#read in NOAA Climate Data data
TN<-read.csv("data/Tennessee_climate.csv")

#keep only sewage plant
TN <- TN%>%filter(STATION=="USC00401790")

#omit NA in temperature recordings 
TN<-TN[complete.cases(TN[,10]),]

#create column for year
TN <- mutate(TN, year=year(TN$DATE))

#create column for month
TN <- mutate(TN, month=month(TN$DATE))

## create column for julian date##
TN$julian_date <- yday(TN$DATE)

