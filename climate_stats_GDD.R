#climate stats and GDD comparison
### simplified look at comparing GDD with climate stats
#climate data from PRISM

library(readxl)
library(writexl)
library(fitdistrplus)
library(lubridate)
library(MuMIn)
library(dplyr)
library(pracma)
library(multcomp)
library(ggplot2)
library(gridExtra)

########################
### Data Preparation ###
########################

#Load NOAA Climate Data Online data
tenn_clim<-read_excel("data/climate_GDD.xlsx")

###################
### Last freeze ###
###################

#calculate last day below -2 for each year since 1980
last_freeze <- tenn_clim%>%
  filter(TMIN< -2)%>%
  filter(julian_date<180)%>%
  group_by(year)%>%
  filter(row_number()==n())

#plot GDDcumsum by last freeze date
ggplot(data=last_freeze,aes(x=year,y=GDDcumsum))+
  geom_point()+
  ylab("Accumulated Growing Degree Days at last freeze date")+
  xlab("Year")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.2, 0.8),legend.key=element_blank(),
        text = element_text(size = 12))
#ggsave("figures/GDD_freeze_year.png",units="cm",width=13,height=10)
mod<-lm(GDDcumsum~year,data=last_freeze)
summary(mod)
#calculate mean last freeze for TN since 1980
mean(as.numeric(last_freeze$julian_date))

#statistical model for changes in last freeze date
last_freeze_mod <- lm(julian_date~year, data=last_freeze)
summary(last_freeze_mod)
