#code specifically comparing LT50 differences among phenology stages
#we have no LT50 for stage 4 phenology
#potential focus on just stage 3 phenology since we have data for that for all species and years

#read in raw data for LT50 values
outputs<-read_excel("data/LT50 master.xlsx")

#######################################
### Does LT50 vary with phenology status ###
#######################################
outputs$phen<-as.factor(outputs$phen)
outputs$year<-as.factor(outputs$year)
outputs$Species<-as.factor(outputs$Species)
ggplot(outputs,aes(x=phen,y=LT50,fill=year))+
  geom_boxplot()+
  facet_wrap(~Species)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.1, 0.85),legend.key=element_blank(),
        text = element_text(size = 14))

# #Does LT50 vary by phenology stage?
# kruskal.test(LT50 ~ phen, data = outputs)
# #significant difference but now look at pairwise
# pairwise.wilcox.test(outputs$LT50, outputs$phen,
#                      p.adjust.method = "BH")
# #only 0 is different than stage 2 and 3

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

#look at just pheno levels 2 and 3

#Phenology level 2
mods<-glm(LT50~Species+year,data=outputs%>%filter(phen==2))
summary(mods)

#Phenology level 3
mods<-glm(LT50~Species+year,data=outputs%>%filter(phen==3))
summary(mods)
