## R code to manipulate and plot climate data relating to the APSU Farm ##
## Written by Joe Joe Endris ##

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(gridExtra)
library(MuMIn)
library(readr)

##################################
### Data entry and preparation ###
##################################

#Load NOAA Climate Data Online data
tenn_clim<-read.csv("data/Tennessee_climate.csv")

#keep only sewage plant
tenn_clim <- tenn_clim%>%filter(STATION=="USC00401790")

#create column for year
tenn_clim <- mutate(tenn_clim, year=year(tenn_clim$DATE))

#create column for month
tenn_clim <- mutate(tenn_clim, month=month(tenn_clim$DATE))

## create column for julian date##
tenn_clim$julian_date <- yday(tenn_clim$DATE)

###########################
### Last freeze by year ###
###########################

#calculate last day below freezing for each year
last_freeze <- TN%>%
  filter(TMIN< -2)%>%
  filter(year(DATE)>1979)%>%
  filter(julian_date<180)%>%
  group_by(year(DATE))%>%
  filter(row_number()==n())

#calculate mean last freeze for TN since 1980
mean(as.numeric(last_freeze$julian_date))

#calculate last day below freezing for 2022
last_freeze_2022 <- TN%>%
  filter(TMIN< -2)%>%
  filter(year(DATE)==2022)%>%
  filter(julian_date<180)%>%
  group_by(year(DATE))%>%
  filter(row_number()==n())
last_freeze_2022

#calculate last day below freezing for 2023
last_freeze_2023 <- TN%>%
  filter(TMIN< -2)%>%
  filter(year(DATE)==2023)%>%
  filter(julian_date<180)%>%
  group_by(year(DATE))%>%
  filter(row_number()==n())
last_freeze_2023

#################################
### Absolute Low Temp by Year ###
#################################

#Determine absolute coldest day by year
TN$DATE <- as.Date(TN$DATE)
class(TN$DATE)

yearly_TMIN <- TN %>%
  group_by(year) %>%
  summarise(temp = min(TMIN, na.rm = TRUE))

#####################################
### Mean Low Temps by Julian Date ###
#####################################

#calculate mean monthly low
TN_monthly_low <- TN %>%
  group_by(julian_date) %>%
  filter(julian_date<152) %>%
  summarise(temp=mean(TMIN))

## create graph for mean low temps by Julian date ##
TN_TMIN_plot <-
  ggplot(TN_monthly_low, aes(x = julian_date, y = temp)) +
  geom_line() +
  labs(title = "Mean Lowest Temperature by Julian Date",
       y= "Temperature (Celcius)",
       x= "Julian Date") + 
  theme_bw(base_size = 15)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

TN_TMIN_plot

#############################################
### Determine the number of days below -2 ###
#############################################

#Number of Days Below -2
TN_freeze <- TN %>%
  group_by(year) %>%
  filter(month <6) %>%
  summarise(total_days=sum(TMIN < -2))

#plot Number of Days Below -2 since 1980
TN_freeze_plot <- TN_freeze %>%
  filter(year > 1980)%>%
  ggplot(aes(x = year, y = total_days)) +
  geom_point(color="black") +
  geom_smooth(method="loess")+
  labs(title = "Number of Days Below -2(C)",
       subtitle = "Clarksville, TN",
       y= "Number of Days",
       x= "Year") + 
  theme_bw(base_size = 15)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

TN_freeze_plot

mod_neg2 <- lm(total_days~year, data=TN_freeze)
summary(mod_neg2)


#determine number of days below -2



#######################################
### Absolute Low by year since 1980 ###
#######################################

yearly_TMIN_1980 <-  yearly_TMIN %>%
  filter(year>1979)

TMIN_1980 <- ggplot(yearly_TMIN_1980, aes(x=year, y=temp))+
  geom_point()+
  geom_smooth(method="lm")
TMIN_1980

mod1 <- lm(temp~year, data=yearly_TMIN_1980)
summary(mod1)

#########################################
### Four panel temperature comparison ###
#########################################

# Plot mean low temp for March and April since 1980
UL <- TN %>%
  filter(year>1980) %>%
  group_by(year) %>%
  filter(julian_date>59) %>%
  filter(julian_date<121) %>%
  summarise(temp=mean(TMIN))

UL_panel <- ggplot(UL, aes(x=year, y=temp))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title = "Mean low in March and April by year",
       y= "Temperature (C)",
       x= "Year") + 
  theme_bw(base_size = 15)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

UL_panel

# plot last freeze by Julian date since 1980

UR_panel <- ggplot(last_freeze, aes(x=year, y=julian_date))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title = "Last Freeze by Julian Date",
       y= "Julian Date",
       x= "Year") + 
  theme_bw(base_size = 15)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

UR_panel

mod2 <- lm(julian_date~year, data = last_freeze)
summary(mod2)

# Comparison of 2007 and 2022/2023

BL <- TN%>%
  filter(year == 2007|year==2023|year==2022)%>%
  filter(month == 3 | month == 4) %>%
  filter(julian_date<121)

BL$year <- as.factor(BL$year)

BL_plot <- ggplot(BL, aes(x=julian_date, y=TMIN, color=year, group=year))+
  geom_line()+
  #geom_smooth(method="lm")+
  scale_color_manual(breaks= c("2007", "2022", "2023"),
                     values = c("black", "blue", "red"))+
  labs(title = "Mean low temperature by Julian date for 2007, 2022, and 2023",
       y= "Temperature (C)",
       x= "Julian Date") + 
  theme_bw(base_size = 15)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

BL_plot

# Mean temp by Julian Date for 2007, 2022, and 2023

BR <- TN_PRISM %>%
  filter(year == 2007|year==2023|year==2022)%>%
  filter(julian_date<121)

BR$year <- as.factor(BR$year)

BR_plot <- ggplot(BR, aes(x=julian_date, y=tmean, color=year, group=year))+
  geom_line()+
  geom_smooth(method="lm", se= FALSE)+
  scale_color_manual(breaks= c("2007", "2022", "2023"),
                   values = c("black", "blue", "red"))+
  labs(title = "Mean temperature by Julian date for 2007, 2022, and 2023",
       y= "Temperature (C)",
       x= "Julian Date") + 
  theme_bw(base_size = 15)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

BR_plot


#################################################
### Mean low temperatuers for March and April ###
#################################################

March_mean_tmin <- TN %>%
  filter(year>=1980) %>%
  group_by(month, year) %>%
  filter(julian_date>60) %>%
  filter(julian_date<90) %>%
  summarise(temp=mean(TMIN))

mod3 <- lm(temp ~ year, data = March_mean_tmin)
summary(mod3)

April_mean_tmin <- TN %>%
  filter(year>=1980) %>%
  group_by(year) %>%
  filter(julian_date>91) %>%
  filter(julian_date<121) %>%
  summarise(temp=mean(TMIN))

mod4 <- lm(temp ~ year, data = April_mean_tmin)
summary(mod4)





###############################################
### plot monthly mean low temps for Jan-May ###
###############################################

as.Date(TN$DATE)

TN_month_mean <- TN %>%
  group_by(month=lubridate::floor_date(DATE, "month")) %>%
  summarise(mean_low = mean(TMIN))

TN_month_mean$month2<-month(TN_month_mean$month)
sub<-TN_month_mean%>%
  filter(month2%in%c(1,2,3,4,5))

ggplot(sub,aes(x=month,y=mean_low))+
  geom_point()+
  facet_wrap(~month2,scales="free")+
  geom_smooth(method="lm")

TN_monthly_mean_plot <- ggplot(TN_month_mean, aes(x= month, y=mean_low))+
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Monthly Mean Low Temperture",
       subtitle = "Clarksville, TN",
       y= "Temperature (C)",
       x= "Month") + theme_bw(base_size = 15)

TN_monthly_mean_plot
