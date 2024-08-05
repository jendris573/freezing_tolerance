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

#read in NOAA Climate Data data (1980-2023)
TN<-read_excel("data/tenn1980.xlsx")

#omit NA in temperature recordings 
TN<-TN[complete.cases(TN[,6]),]

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


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
### Plot to show phenology of three core species by year ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#calculate mean phenology by julian date
phenology <- phenology%>%
  group_by(species, year, julian_date) %>%
  dplyr::mutate(mean_phenology=mean(phenology))

#calculate SD for phenology
phenology <- phenology%>%
  group_by(species, year, julian_date) %>%
  mutate(pheno_sd = sd(phenology, na.rm=TRUE))

phenology[,10][phenology[,10]==0] <- NA

maple_phenology<-ggplot(data=subset(phenology, species=="Acer saccharum"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_point()+
  geom_errorbar(aes(y = mean_phenology, ymin = mean_phenology - pheno_sd, ymax = mean_phenology + pheno_sd), alpha = .2) +
  geom_line()+
  labs(x="", y="", colour = "Year")+
  scale_color_manual(values = c("2022" = "grey50", "2023" = "black"))+
  ylim(-1, 5)+
  theme_bw()+
  theme(axis.title.x = element_markdown())+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=14),
        legend.background = element_blank(),
        legend.box.background = element_blank(),legend.spacing.y = unit(0, "cm"),
        legend.position=c("0.08","0.65"))+
  annotate("text", x=40,y=4.5,label= expression(italic("Acer saccharum")), hjust=0, size=5)

maple_phenology

beech_phenology<-ggplot(data=subset(phenology, species=="Fagus grandifolia"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_point()+
  geom_errorbar(aes(y = mean_phenology, ymin = mean_phenology - pheno_sd, ymax = mean_phenology + pheno_sd), alpha = .2) +
  geom_line()+
  labs(x="", y="Phenology Code", colour = "Year")+
  scale_color_manual(values = c("2022" = "grey", "2023" = "black"))+
  ylim(-1, 5)+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=14))+
  annotate("text", x=40,y=4,label= expression(italic("Fagus grandifolia")), hjust=0, size=5)

beech_phenology

poplar_phenology<-ggplot(data=subset(phenology, species=="Liriodendron tulipifera"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_point()+
  geom_errorbar(aes(y = mean_phenology, ymin = mean_phenology - pheno_sd, ymax = mean_phenology + pheno_sd), alpha = .2) +
  geom_line()+
  labs(x="Julian Date", y="", colour = "Year")+
  scale_color_manual(values = c("2022" = "grey", "2023" = "black"))+
  ylim(-1, 5)+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=14))+
  annotate("text", x=40,y=4.5,label= expression(italic("Liriodendron tulipifera")), hjust=0, size=5)

poplar_phenology

grid.arrange(maple_phenology, beech_phenology, poplar_phenology, nrow=3)


# # # # # # # # # # # # # # # # # # # # # # #
### Does LT50 vary with phenology status ---
# # # # # # # # # # # # # # # # # # # # # # # 

outputs$phen<-as.factor(outputs$phen)
outputs$year<-as.factor(outputs$year)
outputs$Species<-as.factor(outputs$Species)
ggplot(outputs,aes(x=phen,y=LT50,fill=year))+
  geom_boxplot()+
  facet_wrap(~Species)

#Does LT50 vary by phenology stage?
kruskal.test(LT50 ~ phen, data = outputs)
#significant difference but now look at pairwise
pairwise.wilcox.test(outputs$LT50, outputs$phen,
                     p.adjust.method = "BH")
#only 0 is different than stage 2 and 3

#a better model that incorprates year, species and phenology stage
mod<-glm(LT50~(phen+year+Species)^2,data=outputs)
summary(mod)
stepAIC(mod)
#best model
mod1<-glm(LT50~phen*Species+year,data=outputs)
summary(mod1)

#What about a simpler model
mods<-glm(LT50~phen+Species+year,data=outputs)
summary(mods)

#comparing specific factor levels
summary(glht(mods, mcp(phen="Tukey")))
summary(glht(mods,mcp(Species="Tukey")))

# # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # #
# Statistical analyses start here ----
# # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # #


#species as a factor
outputs$Species <- as.factor(outputs$Species)

#filter for only TN data
outputs <- filter(outputs, State=="TN")

#filter for core three species
phenology <- filter(phenology, species != "Ostrya virginiana")
phenology <- filter(phenology, species != "Quercus alba")

#species as a factor for phenology data
phenology$species <- as.factor(phenology$species)

# # # # # # # # # # # # # # # # # #
### LT50 statistical analysis ----
# # # # # # # # # # # # # # # # # #

#global model
LT50_model<-glm(LT50~(Species+julian_date+year)^2,data=outputs, na.action="na.fail")
summary(LT50_model)
dredge(LT50_model)

#final model for LT50 
LT50_final_model <- glm(LT50 ~ Species + julian_date, data=outputs)
summary(LT50_final_model)

summary(glht(LT50_final_model, mcp(Species= "Tukey")))

# # # # # # # # # # # # # # # # # # # #
### Phenology statistical analysis ----
# # # # # # # # # # # # # # # # # # # #

#global model
phenology_model <- glm(phenology ~ species * date * year, data=phenology, family = poisson, na.action="na.fail")
summary(phenology_model)
dredge(phenology_model)

#final model for phenology
pheno_mod <- glm(phenology ~ date + year, data=phenology, family = poisson, na.action="na.fail" )
summary(pheno_mod)

#summary(glht(phenology, mcp(species= "Tukey")))#not relevant since Species isn't used as a predictor

# # # # # # # # # # # # #
## Gtsummmary tables ----
# # # # # # # # # # # # #

tenn <- outputs %>%
  filter(State == "TN")

table1 <- tenn %>%
  #select(LT50, year, Species) %>%
  tbl_summary(include = c(LT50, year, Species, julian_date),
              by = Species,
              missing= "no",
              digits = all_continuous() ~1,
              label = list(LT50 ~ "LT50 (°C)"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n}")) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>% #number of digits displayed for p-values
  modify_caption("Table 1. LT50 (°C) values for three hardwood tree species") %>%
  #modify_footnote(everything() ~ NA) %>%
  modify_header(
    update = list(
      label ~ '',
      stat_1 ~ '***Acer saccharum***', #is markdown **bold** formatting
      stat_2 ~ '***Fagus grandifolia***',
      stat_3 ~ '***Liriodendron tulipifera***',
      p.value ~ '**P-value**')) %>%
  as_gt() %>%
  gt::tab_options(heading.align = "left")
table1










