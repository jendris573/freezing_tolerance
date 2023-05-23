#plot to produce a two y-axis plot for freezing tolerance
#comparing LT50 values and phenology across Julian date
#written by Joe Endris

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

##################################
### Data entry and preparation ###
##################################

LT50_2y <-read_excel("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/LT50 master.xlsx")

phenology<-read_excel("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/Phenology and Bud Forcing/phenology check.xlsx")

#Filter for 2022 & 2023 data only
LT50_2y <- filter(LT50_2y, State == "TN", Date >= "2022-01-01")

#create column for Julian date
LT50_2y$julian_date <- yday(LT50_2y$Date)

#create column for year
LT50_2y <- mutate(LT50_2y, year=year(LT50_2y$Date))

LT50_2y <- LT50_2y %>%
  group_by(Species, julian_date) %>%
  dplyr::mutate(LT50mod=mean(LT50),
                   LT50mod_sd=sd(LT50),
                   LT50mod_se=sd(LT50)/sqrt(6))

#phenology data preparation

#create column for Julian date
phenology$julian_date <- yday(phenology$date)

#create column for year
phenology <- mutate(phenology, year=year(date))

#create column for mean phonology by Julian date
phenology <- phenology %>%
  group_by(julian_date, species) %>%
  mutate(avg_pheno = mean(phenology))


###############################################
### Plot LT50 and Phenology with two Y axes ###
###############################################

#Filter non sugar maple species out of both data sets
ASt <- filter(LT50_2y, Species=="Acer saccharum")
ASp <- filter(phenology, species=="Acer saccharum")

ymax <- 5

AS_plot<-ggplot(ASt, aes(x = julian_date, y=LT50mod, color=factor(year))) +
  geom_smooth(se = FALSE)+
  geom_smooth(ASp, mapping = aes(x=julian_date, y= avg_pheno, color=factor(year)), se= FALSE)+
  scale_color_manual(values = c("2022" = "red", "2023" = "blue"))+
  xlab("Julian Date")+
  ylab("LT50 (°C)")+
  xlim(45,130) +
  scale_y_continuous(sec.axis = sec_axis(~., name="Phenology", breaks=seq(0,5,1)))+
  coord_cartesian(ylim = c(-20, ymax))+
  theme(axis.line.y.right = element_line(color = "purple"), 
        axis.ticks.y.right = element_line(color = "purple"),
        axis.text.y.right = element_text(color = "purple"), 
        axis.title.y.right = element_text(color = "purple"))+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  guides(col= guide_legend(title= "Year"))+
  ggtitle("Acer saccharum")

AS_plot
 