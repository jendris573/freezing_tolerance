##code to start developing figures for cold tolerance##
##written by Joe Endris##

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(gridExtra)

outputs<-read_excel("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/LT50 master.xlsx")

#############################################################
###generic garbage plot just to see where everything plots###
#############################################################

ggplot(outputs, aes(x = State, y = LT50, shape = Species, color = Species)) +
  geom_point(size = 2) +
  xlab("Location") +
  ylab("Temperature (°C)") +
  theme_bw()

#edit the below code when its time to save actual plots
##ggsave(filename, plot = last_plot(),device = png(),path = NULL, scale = 1, +
#width = NA, height = NA, units = c("in"), dpi = 300, limitsize = TRUE, bg = NULL)

###################################################
###plot with mean LT50 values grouped by species###
###################################################

thresholds <- outputs%>%
  group_by(Species)%>%
  dplyr::summarise(across(LT15:LT95,list(mean=~mean(.),sd=~sd(.),se=~sd(./sqrt(6)))))

ggplot(data=thresholds,aes(x=Species))+
  geom_point(aes(y=LT50_mean))+
  geom_errorbar(aes(ymin=LT50_mean-LT50_se,ymax=LT50_mean+LT50_se))+
  ylab(bquote("Mean LT50 Temperature (°C)"))+
  theme_bw()

###########################################
##Before and After Mean last freeze plots##
###########################################

#omit any blank spots in the last_freeze column
outputs_LF <- outputs[complete.cases(outputs[,8]),]

outputs_LF <- filter(outputs_LF, State == "TN", Date >= "2022-01-01")

outputs_LF <- outputs_LF%>%
  group_by(Species, last_freeze)%>%
  dplyr::summarise(across(LT15:LT95,list(mean=~mean(.),sd=~sd(.),se=~sd(./sqrt(6)))))

outputs_LF$last_freeze<-factor(outputs_LF$last_freeze,levels=c("before","after"))

BA_species_plot <- ggplot(outputs_LF, aes(x=Species, y=LT50_mean, color=last_freeze))+
  geom_point(position=position_dodge(0.5))+
  geom_errorbar(aes(ymax=LT50_mean+LT50_se,ymin=LT50_mean-LT50_se), position=position_dodge(0.5))+
  xlab ("Species") +
  ylab ("Temperature (°C)")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))

BA_species_plot

#################################
###Tolerance at TN across time###
#################################

TN_outputs <- filter(outputs, State == "TN", Date >= "2022-01-01")

TN_outputs$Species <- as.factor(TN_outputs$Species)
is.factor(TN_outputs$Species)

TN_mean <- TN_outputs%>%
  group_by(Species,Date)%>%
  dplyr::summarise(LT50mean= mean(LT50),
                   LT50sd=sd(LT50),
                   LT50se=sd(LT50)/sqrt(6))

TN_plot <- ggplot(TN_outputs, aes(x=Date, y=LT50, color=Species))+
  geom_point(position=position_dodge(0.5))+
  xlab ("Date") +
  ylab ("Temperature (°C)")+
  theme_bw()

TN_plot

####################################
###Plot LT50 in TN by Julian date###
####################################

outputs$julian_date <- yday(outputs$Date)

outputs_julian <- filter(outputs, State == "TN")

outputs_julian <- outputs%>%
  group_by(Species, julian_date) %>%
  dplyr::summarise(LT15.m=mean(LT15), LT50.m=mean(LT50), LT95.m=mean(LT95),
                   LT50.m_sd=sd(LT50),
                   LT50.m_se=sd(LT50)/sqrt(6))

ggplot(outputs_julian, aes(x= julian_date,y=LT50.m, color= Species)) +
  geom_point()+
  geom_errorbar(aes(ymax=LT50.m+LT50.m_se,ymin=LT50.m-LT50.m_se))+
  xlab ("Julian Date")+
  ylab ("Temperature (°C)")+
  xlim(40,130) +
  theme_bw()

jd1<-ggplot(outputs_julian, aes(x = julian_date, y=LT50.m)) +
  geom_point(data=subset(outputs_julian, Species=="Acer saccharum"))+
  geom_errorbar(data=subset(outputs_julian, Species=="Acer saccharum"), aes(ymax=LT50.m+LT50.m_se,ymin=LT50.m-LT50.m_se))+
  geom_smooth(stat="smooth",method="loess")+
  ylab("Temperature (°C)")+
  xlab("Julian Date")+
  xlim(40,130) +
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Acer saccharum")

jd2<-ggplot(outputs_julian, aes(x = julian_date, y=LT50.m)) +
  geom_point(data=subset(outputs_julian, Species=="Fagus grandifolia"))+
  geom_errorbar(data=subset(outputs_julian, Species=="Fagus grandifolia"), aes(ymax=LT50.m+LT50.m_se,ymin=LT50.m-LT50.m_se))+
  geom_smooth(stat="smooth",method="loess")+
  ylab("Temperature (°C)")+
  xlab("Julian Date")+
  xlim(40,130) +
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Fagus grandifolia")

jd3<- ggplot(outputs_julian, aes(x = julian_date, y=LT50.m)) +
  geom_point(data=subset(outputs_julian, Species=="Liriodendron tulipifera"))+
  geom_errorbar(data=subset(outputs_julian, Species=="Liriodendron tulipifera"), aes(ymax=LT50.m+LT50.m_se,ymin=LT50.m-LT50.m_se))+
  geom_smooth(stat="smooth",method="loess")+
  ylab("Temperature (°C)")+
  xlab("Julian Date")+
  xlim(40,130) +
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Liriodendron tulipifera")

grid.arrange(jd1,jd2,jd3,nrow=3)

#########################################
###Plot Comparing LT50 in 2022 vs 2023###
###        six panel version          ###
#########################################

#data prep
outputs_year <- filter(outputs, State == "TN", Date >= "2022-01-01")

outputs_year <- mutate(outputs_year, year=year(outputs_year$Date))

outputs_year$julian_date <- yday(outputs_year$Date)

outputs_year <- outputs_year%>%
  group_by(Species, year, julian_date) %>%
  dplyr::summarise(LT50.m=mean(LT50),
                   LT50.m_sd=sd(LT50),
                   LT50.m_se=sd(LT50)/sqrt(6))

###code for six panel version
ASyear1<-ggplot(data=subset(outputs_year, Species=="Acer saccharum"& year==2022), aes(x = julian_date, y=LT50.m)) +
  geom_point()+
  geom_errorbar(aes(ymax=LT50.m+LT50.m_se,ymin=LT50.m-LT50.m_se))+
  geom_smooth(stat="smooth",method="lm")+
  xlim(40,130) +
  ylim(-20,-5)+
  ylab("Temperature (°C)")+
  xlab("Julian Date")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Acer saccharum - 2022")
ASyear1

ASyear2<-ggplot(data=subset(outputs_year, Species=="Acer saccharum"& year==2023), aes(x = julian_date, y=LT50.m)) +
  geom_point()+
  geom_errorbar(aes(ymax=LT50.m+LT50.m_se,ymin=LT50.m-LT50.m_se))+
  geom_smooth(stat="smooth",method="lm")+
  xlim(40,130) +
  ylim(-20,-5)+
  ylab("Temperature (°C)")+
  xlab("Julian Date")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Acer saccharum - 2023")
ASyear2

LTyear1<-ggplot(data=subset(outputs_year, Species=="Liriodendron tulipifera"& year==2022), aes(x = julian_date, y=LT50.m)) +
  geom_point()+
  geom_errorbar(aes(ymax=LT50.m+LT50.m_se,ymin=LT50.m-LT50.m_se))+
  geom_smooth(stat="smooth",method="lm")+
  xlim(40,130) +
  ylim(-20,-5)+
  ylab("Temperature (°C)")+
  xlab("Julian Date")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Liriodendron tulipifera - 2022")
LTyear1

LTyear2<-ggplot(data=subset(outputs_year, Species=="Liriodendron tulipifera"& year==2023), aes(x = julian_date, y=LT50.m)) +
  geom_point()+
  geom_errorbar(aes(ymax=LT50.m+LT50.m_se,ymin=LT50.m-LT50.m_se))+
  geom_smooth(stat="smooth",method="lm")+
  xlim(40,130) +
  ylim(-20,-5)+
  ylab("Temperature (°C)")+
  xlab("Julian Date")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Liriodendron tulipifera - 2023")
LTyear2

FGyear1<-ggplot(data=subset(outputs_year, Species=="Fagus grandifolia"& year==2022), aes(x = julian_date, y=LT50.m)) +
  geom_point()+
  geom_errorbar(aes(ymax=LT50.m+LT50.m_se,ymin=LT50.m-LT50.m_se))+
  geom_smooth(stat="smooth",method="lm")+
  xlim(40,130) +
  ylim(-20,-5)+
  ylab("Temperature (°C)")+
  xlab("Julian Date")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Fagus grandifolia - 2022")
FGyear1

FGyear2<-ggplot(data=subset(outputs_year, Species=="Fagus grandifolia"& year==2023), aes(x = julian_date, y=LT50.m)) +
  geom_point()+
  geom_errorbar(aes(ymax=LT50.m+LT50.m_se,ymin=LT50.m-LT50.m_se))+
  geom_smooth(stat="smooth",method="lm")+
  xlim(40,130) +
  ylim(-20,-5)+
  ylab("Temperature (°C)")+
  xlab("Julian Date")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Fagus grandifolia - 2023")
FGyear2

grid.arrange(ASyear1,ASyear2, FGyear1, FGyear2, LTyear1, LTyear2,nrow=3)

#########################################
###Plot Comparing LT50 in 2022 vs 2023###
###       three panel version         ###
#########################################

two_years <- filter(outputs, State == "TN", Date >= "2022-01-01")

two_years <- mutate(two_years, year=year(two_years$Date))

two_years$julian_date <- yday(two_years$Date)

two_years <- two_years%>%
  group_by(Species, julian_date, year) %>%
  dplyr::summarise(LT50.m=mean(LT50),
                   LT50.m_sd=sd(LT50),
                   LT50.m_se=sd(LT50)/sqrt(6))

ASyears<-ggplot(data=subset(two_years, Species=="Acer saccharum"), aes(x = julian_date, y=LT50.m, color=factor(year))) +
  geom_point()+
  geom_errorbar(aes(ymax=LT50.m+LT50.m_se,ymin=LT50.m-LT50.m_se))+
  geom_smooth(stat="smooth",method="lm")+
  xlim(40,130) +
  ylim(-20,-5)+
  ylab("Temperature (°C)")+
  xlab("Julian Date")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Acer saccharum")
ASyears

FGyears<-ggplot(data=subset(two_years, Species=="Fagus grandifolia"), aes(x = julian_date, y=LT50.m, color=factor(year))) +
  geom_point()+
  geom_errorbar(aes(ymax=LT50.m+LT50.m_se,ymin=LT50.m-LT50.m_se))+
  geom_smooth(stat="smooth",method="lm")+
  xlim(40,130) +
  ylim(-20,-5)+
  ylab("Temperature (°C)")+
  xlab("Julian Date")+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Fagus grandifolia")
FGyears

LTyears<-ggplot(data=subset(two_years, Species=="Liriodendron tulipifera"), aes(x = julian_date, y=LT50.m, color=factor(year))) +
  geom_point()+
  geom_errorbar(aes(ymax=LT50.m+LT50.m_se,ymin=LT50.m-LT50.m_se))+
  geom_smooth(stat="smooth",method="lm")+
  xlim(40,130) +
  ylim(-20,-5)+
  ylab("Temperature (°C)")+
  xlab("Julian Date")+
  theme_bw()+
  ggtitle("Liriodendron tulipifera")+
  guides(col= guide_legend(title= "Year"))
LTyears

grid.arrange(ASyears, FGyears, LTyears,nrow=3)

#newtest2