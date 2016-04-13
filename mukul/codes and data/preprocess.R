library(ggplot2)
#library(ggalt)
library(ggthemes)
library(readxl)
library(dplyr)
library(grid)
library(tokenizers)

#setwd("/home/mukul/Desktop/Mukul Backup/github repos stuff/52 vis/2016-13/")
xl1 <- read_excel("UAS_Sightings_report_21Aug-31Jan.xlsx")
xl2 <- read_excel("UASEventsNov2014-Aug2015.xls")

drones <- setNames(bind_rows(xl2[,1:4],
                             xl1[,c(1,3,4,2)]), 
                   c("ts", "city", "state","event"))
drones <- mutate(drones, 
                 year=format(ts, "%Y"), 
                 year_mon=format(ts, "%Y%m"), 
                 ymd=as.Date(ts), 
                 yw=format(ts, "%Y%V"))
by_week <- mutate(count(drones, yw), wk=as.Date(sprintf("%s1", yw), "%Y%U%u")-7,count(drones2,class))
#by_week <- arrange(filter(by_week, wk>=as.Date("2014-11-10")), wk)

#########################################################
data=array(NA,nrow(drones))

for(i in 1:nrow(drones))
{
  print(i)
  d=tokenize_sentences(drones$event[i])[[1]]
  if(length(grep("NOTIFIED",d,ignore.case = F)) != 0)
  { 
    data[i]=d[grep("NOTIFIED",d,ignore.case = F)]
  }
}
data=gsub("(NOTIFIED).*","\\1",data)
data=gsub(" WAS","",data)
data=gsub(" WERE","",data)

#write.csv(data,"department2.csv")
d=read.csv("departments.csv",header = F)
colnames(d)="department"

drones=cbind(drones,d)

#write.csv(drones,"new_drones_data.csv",row.names = F)
