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

LT50_2y <-read_excel("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/LT50 master.xlsx")

phenology<-read_excel("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/Phenology and Bud Forcing/phenology check.xlsx")

#LT50 data preparation
LT50_2y <- filter(LT50_2y, State == "TN", Date >= "2022-01-01")
LT50_2y$julian_date <- yday(LT50_2y$Date)
LT50_2y <- mutate(LT50_2y, year=year(LT50_2y$Date))

LT50_2y <- LT50_2y %>%
  group_by(Species, julian_date) %>%
  dplyr::mutate(LT50mod=mean(LT50),
                   LT50mod_sd=sd(LT50),
                   LT50mod_se=sd(LT50)/sqrt(6))

#phenology data preparation
phenology$julian_date <- yday(phenology$date)
phenology <- mutate(phenology, year=year(date))
phenology <- phenology %>%
  group_by(julian_date, species) %>%
  mutate(avg_pheno = mean(phenology))


###############################################
### Plot LT50 and Phenology with two Y axes ###
###############################################

#sugar maple plot
ASt <- filter(LT50_2y, Species=="Acer saccharum")
ASp <- filter(phenology, species=="Acer saccharum")

ymax <- 5

AS_plot<-ggplot(ASt, aes(x = julian_date, y=LT50mod, color=factor(year))) +
  geom_smooth(se = FALSE)+
  geom_smooth(ASp, mapping = aes(x=julian_date, y= avg_pheno, color= "purple"), se= FALSE)+
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

AS_plot