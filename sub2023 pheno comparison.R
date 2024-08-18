#Comparing the full 2023 DOY and GDD phenology estimates to those from just the 
#sub 2023 data

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(gridExtra)

DOY<-read.csv("data/DOY_phenology.csv")
DOYs<-read.csv("data/DOY_phenology_sub2023.csv")
GDD<-read.csv("data/GDD_phenology.csv")
GDDs<-read.csv("data/GDD_phenology_sub2023.csv")

DOY<-DOY%>%filter(year==2023)
GDD<-GDD%>%filter(year==2023)

#compare difference in GDD
GDD[,4:7]<-GDD[,4:7]-GDDs[,4:7]
DOY[,4:7]-DOYs[,4:7]
