### Statisical analysis

library(readxl)
library(fitdistrplus)
library(lubridate)
library(MuMIn)

LT50_data<-read_excel("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/LT50 master.xlsx")

LT50_data <- filter(LT50_data, State=="TN")

#add in column for julian date
LT50_data$julian_date <- yday(LT50_data$Date)

#add in column for year
LT50_data <- mutate(LT50_data, year=year(LT50_data$Date))

hist(LT50_data$LT50)

#look at distribution of LT50s without considering treatment type
descdist(LT50_data$LT50,discrete=FALSE)

mod1<-glm(LT50~Species*julian_date*year,data=LT50_data,na.action="na.fail")
summary(mod1)

mod2<-glm(LT50~(Species+julian_date+year)^2,data=LT50_data)
summary(mod2)

mod3<-glm(LT50~Species*julian_date+Species*year,data=LT50_data)
summary(mod3)

mod4<-glm(LT50~Species+julian_date+year,data=LT50_data)
summary(mod4)

dredge(mod1)

mod5<- glm(LT50~Species*julian_date+year, data=LT50_data)
summary(mod5)

