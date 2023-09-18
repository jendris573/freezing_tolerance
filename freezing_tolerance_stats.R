### Statistical analysis

library(readxl)
library(fitdistrplus)
library(lubridate)
library(MuMIn)
library(dplyr)
library(pracma)
library(multcomp)
library(ggplot2)


#read in data
LT50_data<-read_excel("data/LT50 master.xlsx")
#LT50_data<-read_excel("C:/R/freezing_tolerance/LT50 master.xlsx") #Evan

#species as a factor
LT50_data$Species <- as.factor(LT50_data$Species)

#filter for only TN data
LT50_data <- filter(LT50_data, State=="TN")

#add in column for julian date
LT50_data$julian_date <- yday(LT50_data$Date)

#add in column for year
LT50_data <- mutate(LT50_data, year=year(LT50_data$Date))

#bring in phenology data
pheno<-read_excel("data/phenology_check.xlsx")
pheno<-read_excel("C:/R/freezing_tolerance/phenology check.xlsx") #Evan

#add in column for julian date
pheno$julian_date <- yday(pheno$date)

#add in column for year
pheno <- mutate(pheno, year=year(date))

#filter for core three species
pheno <- filter(pheno, species != "Ostrya virginiana")
pheno <- filter(pheno, species != "Quercus alba")

#filter out 2021 data (incomplete year)
pheno <- filter(pheno, year != "2021")

#species as a factor for phenology data
pheno$species <- as.factor(pheno$species)

#################################
### LT50 statistical analysis ###
#################################

#histogram to view LT50 data
hist(LT50_data$LT50)

#look at distribution of LT50s without considering treatment type
descdist(LT50_data$LT50,discrete=FALSE)

mod2<-glm(LT50~(Species+julian_date+year)^2,data=LT50_data, na.action="na.fail")
summary(mod2)
dredge(mod2)

#final model for LT50 
mod2a <- glm(LT50 ~ Species + julian_date, data=LT50_data)
summary(mod2a)

summary(glht(mod2a, mcp(Species= "Tukey")))

######################################
### Phenology statistical analysis ###
######################################

#filter out 2021 data since there is no corresponding LT50 data for 2021
pheno_cut <- filter(pheno, year != "2021")

#final model for phenology
pheno_mod2c <- glm(phenology ~ date + year, data=pheno_cut, family = poisson, na.action="na.fail" )
summary(pheno_mod2c)

summary(glht(pheno_mod2c, mcp(species= "Tukey")))#not relevant since Species isn't used as a predictor

#need to find the average phenology for each julian date before making the figure
pheno_mean<-pheno_cut%>%
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

#Three way interaction (no significance)
mod1<-glm(LT50~Species*julian_date*year,data=LT50_data,na.action="na.fail")

#three way interaction with -11 curtail
mod1<-glm(LT50mod~Species*julian_date*year,data=LT50_data,na.action="na.fail")

summary(mod1)
dredge(mod1) #dredge is being used for model selection to see all possible subsets

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


mod2b <- glm(phenology ~ species * date * year, data=pheno_cut, family = poisson, na.action="na.fail")
summary(mod2b)
dredge(mod2b)

