
#################VISUALISE DATA###########################
drones=read.csv("new_drones_data.csv",header=T)
drones$class="MISSING_DATA"
drones$department=as.character(drones$department)
drones$class[drones$department=="LEO NOT NOTIFIED"] ="NOT_NOTIFIED"
drones$class[drones$department=="UNKN IF LEOS NOTIFIED"] ="UNKNOWN"
drones$class[!(drones$department=="LEO NOT NOTIFIED" 
             | drones$department=="UNKN IF LEOS NOTIFIED" 
             | is.na(drones$department)) ]="NOTIFIED"
drones$class=as.factor(drones$class)

drones2=drones[drones$class!="MISSING_DATA",]



ggplot(data = drones2) + 
  geom_bar(mapping = aes(x = class,fill=class))+ggtitle("Whether UAS was notified to LEO or not")

ggplot(data = drones2) + 
  geom_bar(mapping = aes( x=factor(1),fill = class), width = 1) + 
  coord_polar(theta = "y")+xlab("")+ggtitle("Whether UAS was notified to LEO or not (pie chart)")

####UASEventsNov2014-Aug2015
ggplot(data = drones2[1:nrow(xl2),]) + 
  geom_bar(mapping = aes(x = class,fill=class)) +ggtitle("UASEventsNov2014-Aug2015")

ggplot(data = drones2[(nrow(xl2)+1):nrow(drones2),]) + 
  geom_bar(mapping = aes(x = class,fill=class)) +ggtitle("UAS_Sightings_report_21Aug-31Jan")

########

state=d$state[as.data.frame(count(drones2,state))$n>27]

ggplot(data=drones2[drones2$state %in% state,]) +
  geom_bar(mapping = aes(x=state,fill=class))+ ggtitle("Stateweise classification")


ggplot(data=drones2[drones2$state %in% state,]) +
  geom_bar(mapping = aes(x=state,fill=class),width=1)+coord_polar()+ggtitle("Stateweise classification (polar coords)")

ggplot(data=drones2) + 
  geom_bar(mapping=aes(x=year,fill=class))+ggtitle("yearwise classification")

###############
ggplot(data=drones2[drones2$yw>=201500 & drones2$yw<=201570,]) + 
  geom_bar(mapping=aes(x=yw,fill=class))+ggtitle("Weekly classification in year 2015")


