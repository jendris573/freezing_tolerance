### Code to plot phenology from the APSU farm
### written by Joe Endris

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(gridExtra)

phenology<-read_excel("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/Phenology and Bud Forcing/phenology check.xlsx")

#data prep
phenology <- mutate(phenology, year=year(date))

phenology$julian_date <- yday(phenology$date)

#calculate mean phenology by julian date
phenology <- phenology%>%
  group_by(species, year, julian_date) %>%
  dplyr::summarise(mean_phenology=mean(phenology))

#omit any blank spots in the mean_phenology column
phenology <- phenology[complete.cases(phenology[,4]),]

###############################################
### Plot to show three core species by year ###
###############################################

maple_phenology<-ggplot(data=subset(phenology, species=="Acer saccharum"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_line()+
  ylab("Phenology Code")+
  xlab("Julian Date")+
  ylim(-1, 5)+
  theme_bw()+
  ggtitle("Acer saccharum")
maple_phenology

poplar_phenology<-ggplot(data=subset(phenology, species=="Liriodendron tulipifera"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_line()+
  ylab("Phenology Code")+
  xlab("Julian Date")+
  ylim(-1, 5)+
  theme_bw()+
  ggtitle("Liriodendron tulipifera")
poplar_phenology

beech_phenology<-ggplot(data=subset(phenology, species=="Fagus grandifolia"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_line()+
  ylab("Phenology Code")+
  xlab("Julian Date")+
  ylim(-1, 5)+
  theme_bw()+
  ggtitle("Fagus grandifolia")
beech_phenology

grid.arrange(maple_phenology, poplar_phenology, beech_phenology, nrow=3)

##############################################
### Plot to show two other species by year ###
##############################################

hornbeam_phenology<-ggplot(data=subset(phenology, species=="Ostrya virginiana"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_line()+
  ylab("Phenology Code")+
  xlab("Julian Date")+
  ylim(-1, 5)+
  theme_bw()+
  ggtitle("Ostrya virginiana")

hornbeam_phenology

oak_phenology<-ggplot(data=subset(phenology, species=="Quercus alba"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_line()+
  ylab("Phenology Code")+
  xlab("Julian Date")+
  ylim(-1, 5)+
  theme_bw()+
  ggtitle("Quercus alba")

oak_phenology

###############################################################
### Plot to show phenology by year with all 5 species shown ###
###############################################################

###code for six panel version
year1<-ggplot(data=subset(phenology, year==2021), aes(x = julian_date, y=mean_phenology, color=species)) +
  geom_line()+
  ylab("Phenology Code")+
  xlab("Julian Date")+
  ylim(-1, 5)+
  theme_bw()+
  ggtitle("2021")
year1

year2<-ggplot(data=subset(phenology, year==2022), aes(x = julian_date, y=mean_phenology, color=species)) +
  geom_line()+
  ylab("Phenology Code")+
  xlab("Julian Date")+
  ylim(-1, 5)+
  theme_bw()+
  ggtitle("2022")
year2

year3<-ggplot(data=subset(phenology, year==2023), aes(x = julian_date, y=mean_phenology, color=species)) +
  geom_line()+
  ylab("Phenology Code")+
  xlab("Julian Date")+
  ylim(-1, 5)+
  theme_bw()+
  ggtitle("2023")
year2

grid.arrange(year1, year2, year3, nrow=3)

