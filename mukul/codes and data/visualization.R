library(ggplot2)
#library(ggalt)
library(ggthemes)
library(readxl)
library(dplyr)
library(grid)
library(ggmap)
library(gganimate)
library(tokenizers)

##############################DATA#####################################
#setwd("/home/mukul/Desktop/Mukul Backup/github repos stuff/52 vis/2016-13/vis/codes and data/")
xl1 <- read_excel("UAS_Sightings_report_21Aug-31Jan.xlsx")
xl2 <- read_excel("UASEventsNov2014-Aug2015.xls")
drones=read.csv("new_drones_data.csv",header=T)

### to download coordinates of cities
d=NULL
for(i in 1:nrow(drones2))
{
  d=tryCatch(
    {
      rbind(d,geocode(as.character(drones2$city[i])))  
    },
    error=function(cond)
    {
      rbind(d,c(NA,NA))
    }
    
  )
}
#write.csv(d,"geocodes.csv",row.names = F)

########################PREPROCESSING#####################################
drones=read.csv("new_drones_data.csv",header=T)
drones$class="MISSING_DATA"
drones$department=as.character(drones$department)
drones$class[drones$department=="LEO NOT NOTIFIED"] ="NOT_NOTIFIED"
drones$class[drones$department=="UNKN IF LEOS NOTIFIED"] ="UNKNOWN"
drones$class[!(drones$department=="LEO NOT NOTIFIED" 
             | drones$department=="UNKN IF LEOS NOTIFIED" 
             | is.na(drones$department)) ]="NOTIFIED"
drones$class=as.factor(drones$class)
hour=as.numeric(format(as.POSIXct(drones$ts, format="%Y-%m-%d %H:%M:%S"), format="%H"))
drones$hour=hour
month=as.numeric(format(as.POSIXct(drones$ts, format="%Y-%m-%d %H:%M:%S"), format="%m"))
drones$month=month
drones$month[drones$month==1]="January"
drones$month[drones$month==2]="February"
drones$month[drones$month==3]="March"
drones$month[drones$month==4]="April"
drones$month[drones$month==5]="May"
drones$month[drones$month==6]="June"
drones$month[drones$month==7]="July"
drones$month[drones$month==8]="August"
drones$month[drones$month==9]="September"
drones$month[drones$month==10]="October"
drones$month[drones$month==11]="November"
drones$month[drones$month==12]="December"
drones$month=as.factor(drones$month)
drones$month = factor(drones$month,levels(drones$month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
#levels(drones$month)=c("January","February","March","April","May","June","July","August","September","October","November","December")

drones$hour_class="Day"
drones$hour_class[drones$hour <= 6 | drones$hour>18]="Night"

drones2=drones[drones$class!="MISSING_DATA",]

drones2$region=tolower(drones2$state)
d1=read.csv("geocodes.csv")
drones2=cbind(drones2,d1)
drones2=as.data.frame(drones2)

#################VISUALISE DATA###########################

p=ggplot(data=drones) + 
  geom_bar(mapping=aes(x=hour,fill="red"))+ggtitle("Hourly classification of UAS")+ylab("# UAS sightings")+ scale_fill_discrete("",labels="",breaks="")
plot(p)

p=ggplot(data = drones[!is.na(drones$month),]) + 
  geom_bar(mapping = aes(x = month,fill=hour_class),position="fill")+ggtitle("Classify Monthly UAS sightings on the basis of day and night time")+xlab("# UAS sightings")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(p)

p=ggplot(data=drones2[drones2$yw>=201500 & drones2$yw<=201570,]) + 
  geom_bar(mapping=aes(x=yw,fill=class))+ggtitle("Weekly classification in year 2015")
plot(p)

p=ggplot() + 
  geom_map(aes(x = long, y = lat, map_id = region), data = us,
           map = us,fill="gray",color="white") + 
  geom_point(mapping = aes(x=lon,y=lat,color=hour_class,frame=month),data=subset(drones2,lon>= min(us$long) & lon<=max(us$long) &   lat>= min(us$lat) & lat<=max(us$lat)  ),size=5,alpha=0.7)+
  ggtitle("UAS occurences month : ")+theme_light()

ani.options(interval = 1, ani.width = 800, ani.height = 800)
gg_animate(p,"monthly UAS occurences.gif")


p=ggplot() +
  geom_map(aes(x = long, y = lat, map_id = region), data = us,
           map = us, fill = "#ffffff", color = "#ffffff", size = 0.15) +
  geom_map(aes(fill = count, map_id = region),
           map = us, data = by_state,
           color = "#ffffff", size = 0.15)+ggtitle("UAS map")
plot(p)

p=ggplot() + 
  geom_map(aes(x = long, y = lat, map_id = region), data = us,
           map = us,fill="gray",color="white") + 
  geom_point(mapping = aes(x=lon,y=lat,color=class,frame=yw),data=subset(drones2,lon>= min(us$long) & lon<=max(us$long) &   lat>= min(us$lat) & lat<=max(us$lat)  ),size=7)+theme_light()+ggtitle("Weekly UAS occurences")

ani.options(interval = 0.5, ani.width = 800, ani.height = 800)
gg_animate(p,"weekly UAS occurences.gif")
