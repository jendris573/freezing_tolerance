### Statisical analysis

library(readxl)
library(fitdistrplus)
library(lubridate)
library(MuMIn)
library(dplyr)
library(pracma)

LT50_data<-read_excel("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/LT50 master.xlsx")

LT50_data$Species <- as.factor(LT50_data$Species)

LT50_data <- filter(LT50_data, State=="TN")

#add in column for julian date
LT50_data$julian_date <- yday(LT50_data$Date)

#add in column for year
LT50_data <- mutate(LT50_data, year=year(LT50_data$Date))

hist(LT50_data$LT50)

#look at distribution of LT50s without considering treatment type
descdist(LT50_data$LT50,discrete=FALSE)

#force any LT50 values below -11 to be treated as -11
LT50_data$LT50mod <- ifelse(LT50_data$LT50< -11, -11, LT50_data$LT50)

hist(LT50_data$LT50mod)

#look at distribution of LT50mods without considering treatment type
<<<<<<< HEAD
descdist(LT50_data$LT50,discrete=FALSE)

mod1<-glm(LT50~Species*julian_date*year,data=LT50_data,na.action="na.fail")
=======
descdist(LT50_data$LT50mod,discrete=FALSE)

mod1<-glm(LT50mod~Species*julian_date*year,data=LT50_data,na.action="na.fail")
>>>>>>> 0531baf62ca0e1330071dee31d1b043bb32d9a93
summary(mod1)
dredge(mod1)

mod2<-glm(LT50mod~(Species+julian_date+year)^2,data=LT50_data)
summary(mod2)

mod3<-glm(LT50mod~Species*julian_date+Species*year,data=LT50_data)
summary(mod3)

mod4<-glm(LT50mod~Species+julian_date,data=LT50_data)
summary(mod4)

summary(glht(mod4, mcp(Species="Tukey")))

mod5<- glm(LT50~Species*julian_date+year, data=LT50_data)
summary(mod5)

<<<<<<< HEAD




=======
>>>>>>> 0531baf62ca0e1330071dee31d1b043bb32d9a93
#convert LT50mod to positive integers
LT50_data$LT50mod2 <- LT50_data$LT50mod + 11.1
LT50_data$LT50mod2 <- log(LT50_data$LT50mod2)

mod1b<-glm(LT50mod2~Species*julian_date*year,data=LT50_data,na.action="na.fail", family=Gamma)
summary(mod1b)

dredge(mod1b)  

descdist(LT50_data$LT50mod2,discrete=FALSE)
<<<<<<< HEAD
hist(LT50_data$LT50mod2, breaks=20)
=======
hist(LT50_data$LT50mod2)
>>>>>>> 0531baf62ca0e1330071dee31d1b043bb32d9a93


###transform data####

#create new column to create square root for LT50
LT50_data$LT50trans <- LT50_data$LT50mod/mean(LT50_data$LT50mod)

hist(LT50_data$LT50trans)




