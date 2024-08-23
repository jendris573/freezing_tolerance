#stats comparing if phenology varied by GDD, year, and species
#make plots too

library(ggplot2)
library(tidyverse)
library(MuMIn)

DOY<-read.csv("data/DOY_phenology.csv")
GDD<-read.csv("data/GDD_phenology.csv")

#did phenology vary with GDD, year, and species
GDD2<-GDD%>%
  pivot_longer(4:7)
colnames(GDD2)[4]<-"phenology"

ggplot(data=GDD2,aes(x=phenology,y=value))+
  geom_boxplot()+
  facet_wrap(~species+year)
#Does it really make sense to model all phenology stages through time and year?
#stage 4 is problematic as the sigmoidal curve never asymptotes so values all tend to bunch near the asymptope
#stage 0 isn't very helpful since that is when they are considered dormant

#model just stages 2 for GDD/DOY, year, and species
mod<-glm(ph2~year*species,data=GDD,na.action="na.fail")
summary(mod)
dredge(mod)
mod<-glm(ph2~year+species,data=GDD)
summary(mod)

#for DOY
mod<-glm(ph2~year*species,data=DOY,na.action="na.fail")
summary(mod)
dredge(mod)
mod<-glm(ph2~year+species,data=DOY)
summary(mod)

#ph3
mod<-glm(ph3~year*species,data=GDD,na.action="na.fail")
summary(mod)
dredge(mod)
mod<-glm(ph2~year+species,data=GDD)
summary(mod)

#for DOY
mod<-glm(ph3~year*species,data=DOY)
summary(mod)
mod<-glm(ph3~year,data=DOY)
summary(mod)



GDDph2<-ggplot(out,aes(x=year,y=ph2))+
  geom_boxplot()+
  ylab("GDD for phase 2")+
  facet_wrap(~species)
GDDph3<-ggplot(out,aes(x=year,y=ph3))+
  geom_boxplot()+
  ylab("GDD for phase 3")+
  facet_wrap(~species)

DOYph2<-ggplot(out2,aes(x=year,y=ph2))+
  geom_boxplot()+
  ylab("Julian Day for phase 2")+
  facet_wrap(~species)
DOYph3<-ggplot(out2,aes(x=year,y=ph3))+
  geom_boxplot()+
  ylab("Julian Day for phase 3")+
  facet_wrap(~species)