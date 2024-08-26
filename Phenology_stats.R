#stats comparing if phenology varied by GDD, year, and species
#make plots too

library(ggplot2)
library(tidyverse)
library(MuMIn)
library(gridExtra)
library(multcomp)

DOY<-read.csv("data/DOY_phenology.csv")
GDD<-read.csv("data/GDD_phenology.csv")

DOY$species<-as.factor(DOY$species)
GDD$species<-as.factor(GDD$species)

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
#best model
mod_GDDph2<-glm(ph2~year+species,data=GDD)
summary(mod_GDDph2)
summary(glht(mod_GDDph2, mcp(species="Tukey")))
#for DOY
mod<-glm(ph2~year*species,data=DOY,na.action="na.fail")
summary(mod)
dredge(mod)
mod_DOYph2<-glm(ph2~year*species,data=DOY)
summary(mod_DOYph2)
summary(glht(mod_DOYph2, mcp(species="Tukey")))

#ph3
mod<-glm(ph3~year*species,data=GDD,na.action="na.fail")
summary(mod)
dredge(mod)
mod_GDDph3<-glm(ph3~year*species,data=GDD)
summary(mod_GDDph3)
summary(glht(mod_GDDph3, mcp(species="Tukey")))

#for DOY
mod<-glm(ph3~year*species,data=DOY,na.action="na.fail")
summary(mod)
dredge(mod)
mod_DOYph3<-glm(ph3~year+species,data=DOY)
summary(mod_DOYph3)
summary(glht(mod_DOYph3, mcp(species="Tukey")))


GDDph2<-ggplot(GDD,aes(x=species,y=ph2,fill=as.factor(year)))+
  geom_boxplot(width=0.5,position = position_dodge(width = 0.55))+
  scale_fill_manual(values=c('grey50','grey80'),labels=c('2022','2023'),name='Year')+
  ylab("AGDD at phenology stage 2")+
  #facet_wrap(~species)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.8, 0.85),legend.key=element_blank(),
        text = element_text(size = 16),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylim(270,560)
GDDph2
DOYph2<-ggplot(DOY,aes(x=species,y=ph2,fill=as.factor(year)))+
  geom_boxplot(width=0.5,position = position_dodge(width = 0.55))+
  scale_fill_manual(values=c('grey50','grey80'),guide="none")+
  ylab("Julian Date at phenology stage 2")+
  #facet_wrap(~species)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.8, 0.85),legend.key=element_blank(),
        text = element_text(size = 16),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylim(75,126)
DOYph2

GDDph3<-ggplot(GDD,aes(x=species,y=ph3,fill=as.factor(year)))+
  geom_boxplot(width=0.5,position = position_dodge(width = 0.55))+
  scale_fill_manual(values=c('grey50','grey80'),guide="none")+
  ylab("AGDD at phenology stage 3")+
  #facet_wrap(~species)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.8, 0.85),legend.key=element_blank(),
        text = element_text(size = 16),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylim(270,560)
GDDph3
DOYph3<-ggplot(DOY,aes(x=species,y=ph3,fill=as.factor(year)))+
  geom_boxplot(width=0.5,position = position_dodge(width = 0.55))+
  scale_fill_manual(values=c('grey50','grey80'),guide="none")+
  ylab("Julian Date at phenology stage 3")+
  #facet_wrap(~species)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.8, 0.85),legend.key=element_blank(),
        text = element_text(size = 16),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylim(75,126)
DOYph3

grid.arrange(GDDph2,GDDph3,DOYph2,DOYph3,ncol=2)
