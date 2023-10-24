### Statistical analysis for Clarksville Climate
### Written by Joe Endris
### With input from Evan Rehm

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
tenn_clim<-read.csv("data/Tennessee_climate.csv")

#keep only sewage plant
tenn_clim <- tenn_clim%>%filter(STATION=="USC00401790")

#create column for year
tenn_clim <- mutate(tenn_clim, year=year(tenn_clim$DATE))

#create column for month
tenn_clim <- mutate(tenn_clim, month=month(tenn_clim$DATE))

## create column for julian date##
tenn_clim$julian_date <- yday(tenn_clim$DATE)

#omit NA in precipitation recordings 
tenn_clim<-tenn_clim[complete.cases(tenn_clim[,6]),]
#omit NA in TMAX recordings 
tenn_clim<-tenn_clim[complete.cases(tenn_clim[,9]),]
#omit NA in TMIN recordings 
tenn_clim<-tenn_clim[complete.cases(tenn_clim[,10]),]

#filter for 1980-2022
tenn1980 <- tenn_clim %>%
  filter(year>1979) %>%
  filter(year<2023)

###########################
### Climate data points ###
###########################

#determine annual precipitation values
precip <- tenn1980 %>%
  group_by(year) %>%
  dplyr::summarise(annual_precip = sum(PRCP))

#average annual TMAX
TMAX <- tenn1980 %>%
  group_by(year) %>%
  dplyr::summarise(annual_TMAX = mean(TMAX))

#average annual TMIN
TMIN <- tenn1980 %>%
  group_by(year) %>%
  dplyr::summarise(annual_TMIN = mean(TMIN))

#create one data frame with all the data
climate <- cbind(precip, TMAX$annual_TMAX, TMIN$annual_TMIN) %>%
  rename("TMAX" = "TMAX$annual_TMAX",
         "TMIN" = "TMIN$annual_TMIN")

#calculate the mean high temperature
mean_TMAX <-   climate %>%
  dplyr::summarise(annual_TMAX = mean(TMAX))
#calculate the mean low temperature
mean_TMIN <-   climate %>%
  dplyr::summarise(annual_TMIN = mean(TMIN))
#calculate the mean precipitation
mean_precip <-   climate %>%
  dplyr::summarise(mean_precip = mean(annual_precip))

#filter for 1980-2022 (2023 has incomplete data)
climate1980 <- climate %>%
  filter(year>1979) %>%
  filter(year<2023)

#Plot for climate since 1980
climate_plot <- ggplot() +
  geom_point(data = climate1980, aes(x= year, y=TMIN, color = TMIN))+
  labs(y=expression("Temperature (°C)"), x="Year")+
  xlab("Year")+
  theme_bw()+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.x = element_blank(),
       # axis.text.x=element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),legend.spacing.y = unit(0, "cm"))+
  ggtitle("Clarksville, TN climate since 1980")
  
climate_plot

##########################
### Statistical Models ###
##########################

#basic model to deterimine 
clim_mod1 <- glm(TMIN ~ julian_date + year , data=tenn1980)

summary(clim_mod1)


###########################
### Last freeze by year ###
#Include this in the paper#
###########################

#calculate last day below freezing for each year
last_freeze <- tenn_clim%>%
  filter(TMIN< -2)%>%
  filter(year(DATE)>1979)%>%
  filter(julian_date<180)%>%
  group_by(year(DATE))%>%
  filter(row_number()==n())

#calculate mean last freeze for TN since 1980
mean(as.numeric(last_freeze$julian_date))

#statisical model for changes in last freeze date
last_freeze_mod <- lm(julian_date~year, data=last_freeze)
summary(last_freeze_mod)

#calculate last day below freezing for 2022
last_freeze_2022 <- tenn_clim%>%
  filter(TMIN< -2)%>%
  filter(year(DATE)==2022)%>%
  filter(julian_date<180)%>%
  group_by(year(DATE))%>%
  filter(row_number()==n())
last_freeze_2022

#calculate last day below freezing for 2023
last_freeze_2023 <- tenn_clim%>%
  filter(TMIN< -2)%>%
  filter(year(DATE)==2023)%>%
  filter(julian_date<180)%>%
  group_by(year(DATE))%>%
  filter(row_number()==n())
last_freeze_2023

##############################################
### The number of days below -2 since 1980 ###
##############################################

#determine number of spring days below -2
spring_1980 <- tenn_clim %>%
  group_by(year) %>%
  filter(month <6) %>%
  filter(year>1979) %>%
  summarise(total_days=sum(TMIN < -2))

mean(spring_1980$total_days)

#plot Number of Days Below -2 since 1980
TN_freeze_plot <- spring_1980 %>%
  ggplot(aes(x = year, y = total_days)) +
  geom_point(color="black") +
  geom_smooth(method="lm")+
  labs(y= "Number of Days",
       x= "Year") + 
  theme_bw(base_size = 15)+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank())

TN_freeze_plot

mod_neg2 <- glm(total_days~year, data=spring_1980)
summary(mod_neg2)

#######################################
### Absolute Low by year since 1980 ###
#######################################

#Determine absolute coldest day by year
tenn_clim$DATE <- as.Date(tenn_clim$DATE)
class(tenn_clim$DATE)

yearly_TMIN <- tenn1980 %>%
  group_by(year) %>%
  summarise(temp = min(TMIN, na.rm = TRUE))

TMIN_1980 <- ggplot(yearly_TMIN, aes(x=year, y=temp))+
  geom_point()+
  geom_smooth(method="lm")
TMIN_1980

absolute_TMIN <- lm(temp~year, data=yearly_TMIN)
summary(absolute_TMIN)

###########################################################
### Mean low temperatures for February, March and April ###
###########################################################

February_mean_tmin <- tenn1980 %>%
  group_by(julian_date, year) %>%
  filter(julian_date>31) %>%
  filter(julian_date<60) %>%
  dplyr::summarise(temp=mean(TMIN))

february_model <- glm(temp ~ julian_date + year, data = February_mean_tmin, na.action="na.fail")
summary(february_model)

february_TMIN_plot <- ggplot(February_mean_tmin, aes(x= year, y=temp))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_bw()+
  xlab('Year')+
  ylab('Daily minimum temperature')+
  annotate(geom="text",x=1981,y=17,label="February",size=5)

february_TMIN_plot

March_mean_tmin <- tenn1980 %>%
  group_by(julian_date, year) %>%
  filter(julian_date>59) %>%
  filter(julian_date<91) %>%
  summarise(temp=mean(TMIN))

#colnames(March_mean_tmin)[2] <- "new_col2"

march_model <- glm(temp ~ julian_date + year, data = March_mean_tmin, na.action="na.fail")
summary(march_model)

march_TMIN_plot <- ggplot(March_mean_tmin, aes(x= year, y=temp))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_bw()+
  xlab('Year')+
  ylab('Daily minimum temperature')+
  annotate(geom="text",x=1981,y=19,label="March",size=5)

march_TMIN_plot

April_mean_tmin <- tenn1980 %>%
   group_by(julian_date, year) %>%
  filter(julian_date>90) %>%
  filter(julian_date<121) %>%
  summarise(temp=mean(TMIN))

april_model <- glm(temp ~ julian_date + year, data = April_mean_tmin, na.action="na.fail")
summary(april_model)

april_TMIN_plot <- ggplot(April_mean_tmin, aes(x= year, y=temp))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_bw()+
  xlab('Year')+
  ylab('Daily minimum temperature')+
  annotate(geom="text",x=1981,y=21,label="April",size=5)
april_TMIN_plot

grid.arrange(february_TMIN_plot,march_TMIN_plot,april_TMIN_plot,ncol=2)
###########################################################################################
                                #Unused code
###########################################################################################

#########################################
### Four panel temperature comparison ###
#########################################

# Plot mean low temp for March and April since 1980
UL <- tenn_clim %>%
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

BL <- tenn_clim%>%
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

BR <- tenn_clim_PRISM %>%
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


###############################################
### plot monthly mean low temps for Jan-May ###
###############################################

as.Date(tenn_clim$DATE)

TN_month_mean <- tenn_clim %>%
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



### 2010 looks funky

clim2010 <- tenn_clim %>%
  filter(year==2010)

clim2010 <- clim2010 %>%
  summarise(average= mean(TMIN))
