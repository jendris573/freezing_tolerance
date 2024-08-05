### code for freezing tolerance plots and stats
### written by Joe Endris
### with input from Evan Rehm

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(ggtext)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(gridExtra)
library(MuMIn)
library(fitdistrplus)
library(pracma)
library(gtsummary)

# # # # # # # # # # # # #
### data preparation ----
# # # # # # # # # # # # #

#read in raw data for LT50 values
outputs<-read_excel("data/LT50 master.xlsx")

#create column for julian date
outputs$julian_date <- yday(outputs$Date)

#create column for month
outputs <- mutate(outputs, month=month(outputs$Date))

#create column for year
outputs <- mutate(outputs, year=year(outputs$Date))

#read in PRISM Climate Data data (1981-2023)
TN<-read.csv("data/PRISM_climate.csv")

#force R to view dates as dates
TN<-as.Date(TN$DATE, "%m/%d/%y")

#create column for year
TN <- mutate(TN, year=year(TN$DATE))

#create column for month
TN <- mutate(TN, month=month(TN$DATE))

## create column for julian date##
TN$julian_date <- yday(TN$DATE)

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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
## plot with historic coldest temp by julian date and coldest day of study year ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

dbl_panel <- filter(outputs, State == "TN")

dbl_panel <- dbl_panel%>%
  group_by(year, Species, julian_date) %>%
  dplyr::summarise(LT15.m=mean(LT15), LT50mod=mean(LT50), LT95.m=mean(LT95),
                   LT50mod_sd=sd(LT50),
                   LT50mod_se=sd(LT50)/sqrt(6))

jdate_TMIN <- TN %>%
  filter(year >1979) %>%
  group_by(julian_date) %>%
  summarise(absol_TMIN = min(TMIN)) 

TMIN_2022 <- TN %>%
  filter(year==2022) %>%
  mutate(absol_TMIN = TMIN) %>%
  select(julian_date, absol_TMIN)

TMIN_2023 <- TN %>%
  filter(year==2023) %>%
  mutate(absol_TMIN = TMIN) %>%
  select(julian_date, absol_TMIN)

#stack jdate_TMIN and TMIN_2022 into a single dataframe
jdate_TMIN$year="1980"
TMIN_2022$year="2022"
TMIN_2023$year="2023"
new<-rbind(jdate_TMIN,TMIN_2022,TMIN_2023)

plot22 <-ggplot() +
  geom_point(data=subset(dbl_panel, year=="2022"), aes(x = julian_date, y=LT50mod, color= Species), position = position_dodge(width = 2))+
  geom_errorbar(data=subset(dbl_panel, year=="2022"), aes(x= julian_date, ymax=LT50mod+LT50mod_se,ymin=LT50mod-LT50mod_se, color= Species), width= 2, position = position_dodge(width = 2))+
  geom_line(data=subset(new,year!="2023"), aes(x=julian_date, y=absol_TMIN, group=year,linetype=year))+
  scale_color_manual(values = c("Acer saccharum" = "red", "Liriodendron tulipifera" = "blue", "Fagus grandifolia" = "black"))+
  scale_linetype_manual("Minimum temperature",values = c("2022"=1,"1980"=2),labels=c("Since 1980","2022"))+
  xlim(40,130) +
  ylim(-20,10)+
  labs(y=expression("LT"["50"]/"Temperature (°C)"))+
  xlab("Julian Date")+
  theme_bw()+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),legend.spacing.y = unit(0, "cm"))+
  #legend.position=c("0.1","0.85"), legend.box = "vertical")+
  ggtitle(2022)
plot22

plot23 <-ggplot() +
  geom_point(data=subset(dbl_panel, year=="2023"), aes(x = julian_date, y=LT50mod, color= Species), position = position_dodge(width = 2))+
  geom_errorbar(data=subset(dbl_panel, year=="2023"), aes(x= julian_date, ymax=LT50mod+LT50mod_se,ymin=LT50mod-LT50mod_se, color= Species), width = 2, position = position_dodge(width = 2))+
  geom_line(data=subset(new,year!="2022"), aes(x=julian_date, y=absol_TMIN, group=year,linetype=year))+
  scale_linetype_manual("Minimum temperature",values = c("2023"=1,"1980"=2),labels=c("Since 1980","2023"))+
  scale_color_manual(values = c("Acer saccharum" = "red", "Liriodendron tulipifera" = "blue", "Fagus grandifolia" = "black"),guide="none")+
  xlim(40,130) +
  ylim(-20,10)+
  labs(y=expression("LT"["50"]/"Temperature (°C)"))+
  xlab("Julian Date")+
  theme_bw()+
  theme(axis.title.x = element_markdown())+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.box.background = element_blank(),legend.spacing.y = unit(0, "cm"))+
  #legend.position=c("0.1","0.9"))+
  ggtitle(2023)
plot23

LT50_plot <- grid.arrange(plot22, plot23,nrow=2)