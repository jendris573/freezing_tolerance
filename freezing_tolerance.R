##R code to to analyze freezing resistance##
##In line with Lim et al 1998##
##by Joe Endris##

library(dplyr)
library(dtplyr)
library(data.table)
library(ggplot2)
library(ggfortify)
library(tidyr)
library(minpack.lm)#contains the nlsLM function we need for the gompertz
library(readxl)
library(writexl)

#read in the raw data
raw_data<-read_excel("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/Freezing Data.xlsx",sheet=2)

#create an unique ID for each tree species/number/temp/date
freeze_data <- mutate(raw_data, Unique_ID = paste(Date_Collected, Spec,State,Num, sep = "."))
freeze_data<-freeze_data%>%
  filter(Date_Collected>"2021-12-31")

#calculate leakage (initial leakage/final leakage) x 100
freeze_data<-mutate(freeze_data, leakage = ((adjusted_elec1/adjusted_elec2)*100))

#looping over each unique ID for injury
injury_percent<-data.frame()
for(j in 1:length(unique(freeze_data$Unique_ID))){
  id<-unique(freeze_data$Unique_ID)[j]
  test<-freeze_data[which(freeze_data$Unique_ID==id),]
  for(i in 1:6){
    test$injury[i]<-((test$leakage[i]-test$leakage[1])/(100-test$leakage[1]))*100
  }
  injury_percent<-rbind(injury_percent,test)
}
injury_percent$injury<-as.numeric(injury_percent$injury)
#changing all negatives to zero before calculating the adjusted injury
#only do this for the non -40 readings. if the -40 is turned into a 0 then the next calculation
#spits errors
injury_percent$injury<-ifelse(injury_percent$injury<0&injury_percent$Temp!=-40,0,injury_percent$injury)

#calculate adjusted injury (Injury[temperature]-/injury[-40])x 100
adj_injury_percent<-data.frame()
for(j in 1:length(unique(injury_percent$Unique_ID))){
  id<-unique(injury_percent$Unique_ID)[j]
  test<-injury_percent[which(injury_percent$Unique_ID==id),]
  for(i in 1:6){
    test$adjInj[i]<-((test$injury[i]/test$injury[6]))*100
  }
  adj_injury_percent<-rbind(adj_injury_percent,test)
}

#round the numbers in the adjusted injury column to two digits past decimal point
injury_percent$adjInj<-round(adj_injury_percent$adjInj,2)

#negative numbers in the adjusted injury column come about because the -40C control injury value
#for that individual was negative
#so these negative values just need to be multiplied by -1 to turn them positive
injury_percent$adjInj <- if_else (injury_percent$adjInj<0,injury_percent$adjInj*-1,injury_percent$adjInj)

#values that are >100% in the adjInj column mean the injury of that leaf was greater than the -40C
#meaning they were even more dead than the -40C
#must convert these to 100% injury
injury_percent$adjInj <- if_else (injury_percent$adjInj>99.99,100,injury_percent$adjInj)

################################################################
################################################################
dat.test<-injury_percent
#add in a tiny bit of jitter to avoid perfect fits and non-model convergence
dat.test$adjInj<-ifelse(dat.test$adjInj==0&dat.test$Temp!=4, dat.test$adjInj+abs(jitter(1)),dat.test$adjInj)
#for the nlsLM far down below
index <- function(x, index = 1) { return(x[index])}

#Determine the potential starting values for b and k for each individual
#model based on Lim et al 1998 - Comparing Gompertz and Richards functions to estimate freezing injury in Rhododendron using electrolyte leakage
#Journal of American Society of Horticulture Science
test <- split(dat.test, list(dat.test$Unique_ID))#the length of this list should be equal to the

#number of rows of your data/6
isTRUE(length(test)==nrow(injury_percent)/6)#they are the same so that is great

#determine starting values for b
b.start.test <- unlist(lapply(lapply(lapply(test, lm, formula = (log(100) - log(adjInj + 100)) ~ Temp), coefficients), index), exp) # calculate starting values for b

#when the b value is positive the starting value is very far away from the true value
#an easy fix is to turn all those positive to negatives
b.start.test<-ifelse(b.start.test>0,b.start.test*-1,b.start.test)

#determine starting values for k
k.start.test <- unlist(lapply(lapply(lapply(test, lm, formula = (log(100) - log(adjInj + 100)) ~ Temp), coefficients), index,index=2)) # calculate starting values for k
b.start.test#check to make sure there is a value for each individuals
k.start.test#check to make sure there is a value for each individuals

##########################################################################
#use those starting b and k values to now make predictions at a bunch of temperatures
new_dat<-list()#initiate new list to write predicted data into
gomp.mod.list<-list()

#########################################
#########################################
for (i in 1:length(test)){
  temp<-as.data.frame(test[[i]])
  gomp.mod.list[[i]]<-
    nlsLM(adjInj ~ 100 * exp(-b * exp(-k * Temp)), 
          start = list(b = b.start.test[i], k = k.start.test[i]),
          control = nls.control(maxiter = 10000,tol = 1e-05),data=temp) # calculate gompertz models per species
  new_dat[[i]]<-data.frame(Temp=seq(min(-40), max(4), length.out = 50))
  # Create a data set of fitted data
  new_dat[[i]]$adjInj = predict(gomp.mod.list[[i]], new_dat[[i]])
}

#########################################
#########################################
#this code needed only to trouble shoot individuals
#   temp<-as.data.frame(test[[2]])
# ggplot(temp,aes(x=Temp,y=adjInj))+
#   geom_point()
#   gomp.mod.list[[2]]<-
#     nlsLM(adjInj ~ 100 * exp(-b * exp(-k * Temp)), 
#           start = list(b = -b.start.test[2], k = k.start.test[2]),
#           control = nls.control(maxiter = 1000,tol = 1e-05),data=temp) # calculate gompertz models per species
#   new_dat[[2]]<-data.frame(Temp=seq(min(-40), max(4), length.out = 50))
#   # Create a data set of fitted data
#   new_dat[[2]]$adjInj = predict(gomp.mod.list[[2]], new_dat[[2]])
#########################################
#########################################
b <- unlist(lapply(lapply(gomp.mod.list, coefficients),index,index=1))
k <- unlist(lapply(lapply(gomp.mod.list, coefficients), index,index=2))
#extract LT10, 50 and 90 values from the gompertz model
#First the LT15
LT15.temp<-0
for (i in 1:length(b)){
  LT15.temp[i] <- -log( (log(100)-log(15)) / b[i] ) / k[i]
}
LT15.temp

#pull in the unique ID names to match these estimates too
ID<-data.frame(ID=unique(injury_percent$Unique_ID))
LT<-separate(data = ID, col = ID, into = c("Date", "Species","State","Individual"), sep = "\\.")

LT$LT15<-LT15.temp 
#add in LT50
LT50.temp<-0
for (i in 1:length(b)){
  LT50.temp[i] <- -log( (log(100)-log(50)) / b[i] ) / k[i]
}
LT$LT50<-LT50.temp
#add in LT90
LT95.temp<-0
for (i in 1:length(b)){
  LT95.temp[i] <- -log( (log(100)-log(95)) / b[i] ) / k[i]
}
LT$LT95<-LT95.temp

LT$last_freeze <- NA

LT$last_freeze <- ifelse(LT$State=="AL"&LT$Date<"2022-03-01", "before",LT$last_freeze)
LT$last_freeze <- ifelse(LT$State=="AL"&LT$Date>"2022-03-01", "after",LT$last_freeze)
LT$last_freeze <- ifelse(LT$State=="IN"&LT$Date=="2022-04-05", "before",LT$last_freeze)
LT$last_freeze <- ifelse(LT$State=="IN"&LT$Date=="2022-04-26", "after",LT$last_freeze)
LT$last_freeze <- ifelse(LT$State=="TN"&LT$Date=="2022-04-07", "before",LT$last_freeze)
LT$last_freeze <- ifelse(LT$State=="TN"&LT$Date=="2022-04-19", "after",LT$last_freeze)

#write LT values to the file
write_xlsx(LT, "~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/LT50 master.xlsx")

#testfor github
