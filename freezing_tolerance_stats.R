### Statistical analysis

library(readxl)
library(fitdistrplus)
library(lubridate)
library(MuMIn)
library(dplyr)
library(pracma)
library(multcomp)
library(ggplot2)

LT50_data<-read_excel("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/LT50 master.xlsx")
LT50_data<-read_excel("C:/R/freezing_tolerance/LT50 master.xlsx")

LT50_data$Species <- as.factor(LT50_data$Species)

LT50_data <- filter(LT50_data, State=="TN")

#add in column for julian date
LT50_data$julian_date <- yday(LT50_data$Date)

#add in column for year
LT50_data <- mutate(LT50_data, year=year(LT50_data$Date))

#bring in phenology data
pheno<-read_excel("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/Phenology and Bud Forcing/phenology check.xlsx")
pheno<-read_excel("C:/R/freezing_tolerance/phenology check.xlsx")

#add in column for julian date
pheno$julian_date <- yday(pheno$date)

#add in column for year
pheno <- mutate(pheno, year=year(date))

#filter for core three species
pheno <- filter(pheno, species != "Ostrya virginiana")
pheno <- filter(pheno, species != "Quercus alba")

#filter out 2021 data (incomplete year)
#pheno <- filter(pheno, year != "2021")

pheno$species <- as.factor(pheno$species)

##################################
### Begin statistical analysis ###
##################################

hist(LT50_data$LT50)

#look at distribution of LT50s without considering treatment type
descdist(LT50_data$LT50,discrete=FALSE)

#force any LT50 values below -11 to be treated as -11
#LT50_data$LT50mod <- ifelse(LT50_data$LT50< -11, -11, LT50_data$LT50)

#hist(LT50_data$LT50mod)

#look at distribution of LT50mods without considering treatment type
descdist(LT50_data$LT50,discrete=FALSE)

#Three way interaction (no significance)
#mod1<-glm(LT50~Species*julian_date*year,data=LT50_data,na.action="na.fail")

#three way interaction with -11 curtail
#mod1<-glm(LT50mod~Species*julian_date*year,data=LT50_data,na.action="na.fail")

#summary(mod1)
#dredge(mod1)
#dredge is being used for model selection to see all possible subsets

mod2<-glm(LT50~(Species+julian_date+year)^2,data=LT50_data, na.action="na.fail")
summary(mod2)
dredge(mod2)

mod2a <- glm(LT50 ~ Species + julian_date, data=LT50_data)
summary(mod2a)

summary(glht(mod2a, mcp(Species= "Tukey")))

pheno_cut <- filter(pheno, year != "2021")
mod2b <- glm(phenology ~ species * date * year, data=pheno_cut, family = poisson, na.action="na.fail")
summary(mod2b)
dredge(mod2b)

mod2c <- glm(phenology ~ date + year, data=pheno_cut, family = poisson, na.action="na.fail" )
summary(mod2c)

#need to find the average phenology for each julian date before making the figure
pheno_mean<-pheno_cut%>%
  group_by(year,species,julian_date)%>%
  summarize(mean_pheno=mean(phenology),
            sd_pheno=sd(phenology))

summary(glht(mod2c, mcp(species= "Tukey")))#not relevant since Species isn't used as a predictor

#need to find the average phenology for each julian date before making the figure
pheno_mean<-pheno%>%
  group_by(year,species,julian_date)%>%
  summarize(mean_pheno=mean(phenology),
            sd_pheno=sd(phenology))

mod_plot<-ggplot(data=pheno_mean,aes(x = julian_date, y=mean_pheno,group=species,colour=species)) +
  geom_point()+
  geom_line(aes(group=species))+
  ylab("pheno Code")+
  xlab("Julian Date")+
  ylim(-1, 5)+
  theme_bw()+
  facet_wrap(~year)

mod_plot

##############################################
### Excess models that are not the best fit###
##############################################
mod3<-glm(LT50mod~Species*julian_date+Species*year,data=LT50_data)
summary(mod3)

mod4<-glm(LT50mod~Species+julian_date,data=LT50_data)
summary(mod4)

summary(glht(mod4, mcp(Species="Tukey")))

mod5<- glm(LT50~Species*julian_date+year, data=LT50_data)
summary(mod5)

#convert LT50mod to positive integers
LT50_data$LT50mod2 <- LT50_data$LT50mod + 11.1
LT50_data$LT50mod2 <- log(LT50_data$LT50mod2)

mod1b<-glm(LT50mod2~Species*julian_date*year,data=LT50_data,na.action="na.fail", family=Gamma)
summary(mod1b)

dredge(mod1b)  

descdist(LT50_data$LT50mod2,discrete=FALSE)

hist(LT50_data$LT50mod2, breaks=20)

hist(LT50_data$LT50mod2)

###transform data####

#create new column to create square root for LT50
LT50_data$LT50trans <- LT50_data$LT50mod/mean(LT50_data$LT50mod)

hist(LT50_data$LT50trans)




