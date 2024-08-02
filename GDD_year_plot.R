#make plot of GDD for just 2022 and 2023

library(ggplot)
library(readxl)

#werite the GDD data
outputs<-read_xlsx("data/climate_GDD.xlsx")

outputs<-outputs%>%
  filter(year==2022|year==2023)
outputs$year<-as.factor(outputs$year)
#limit to data up to June 15
outputs<-outputs%>%
  filter(julian_date<166)

g1<-ggplot(outputs,aes(x=julian_date,y=GDDcumsum,colour=year,group=year))+
  geom_line(aes(linetype=year,linewidth=year))+
  xlab("Julian Date")+
  ylab("Accumulated growing degree days")+
  scale_color_manual(values=c('black','black'))+
  scale_linetype_manual(values=c("solid", "dashed"))+
  scale_linewidth_manual(values=c(1,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.2, 0.8),legend.key=element_blank(),
        text = element_text(size = 16))
g1
ggsave("figures/GDD_year.png",units="cm",width=13,height=10)
