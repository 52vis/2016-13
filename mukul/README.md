

<b>Approach:</b>

I have extracted the names of Law Enforcement officer from text data(EventREPORTNARRATIVE column). 

I have used <i>tokenizers</i> package to tokenize text.

Most of the reports (not all) follows a specific pattern :" LEOS NOTIFIED." , "PHOENIX PD NOTIFIED." etc.
So I used [tokenizers](https://github.com/lmullen/tokenizers) package to split these text data and get sentences which have 'NOTIFIED' word in it. All LEO are not extracted using this method as all reports doesn't follow this pattern. I was able to extract 1007 LEO names out of 1347(total data size). 
Resulting LEO names are still very unstructured. I had to manually change few entries to make data more consistent.(aim is to decrease factor levels.)

```r
##loading libraries
library(ggplot2)
library(ggthemes)
library(readxl)
library(dplyr)
library(grid)
library(ggmap)
library(gganimate)
library(animation)

xl1 <- read_excel("UAS_Sightings_report_21Aug-31Jan.xlsx")
xl2 <- read_excel("UASEventsNov2014-Aug2015.xls")
drones=read.csv("drones.csv")
drones$month = factor(drones$month,levels(drones$month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])

drones2=read.csv("drones2.csv")
drones2$month = factor(drones2$month,levels(drones$month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])

by_state <- drones2[drones2$class=="NOTIFIED",] %>%
  group_by(state) %>%
  summarize(count = n()) %>%  mutate(region = tolower(state))

missing_states <- data_frame(
  region = c("wyoming", "vermont", "nebraska", "iowa","idaho","north dakota","south dakota"),
  count=c(0,0,0,0,0,0,0)
)
by_state <- bind_rows(by_state, missing_states)

us <- map_data("state")

mid <- mean(by_state$count, na.rm = TRUE) 
```

```r
p=ggplot(data=drones) + 
 geom_bar(mapping=aes(x=hour,fill="red"))+ggtitle("Hourly classification of UAS")+ylab("# UAS sightings")+ scale_fill_discrete("",labels="",breaks="")
plot(p)
```
![](https://github.com/mukul13/2016-13/blob/master/mukul/images/hourly%20classification%20of%20uas.jpeg)

 drones sightingds are very high at midnight (near 12 AM) and very low in early morining.

```r
p=ggplot(data = drones[!is.na(drones$month),]) + 
  geom_bar(mapping = aes(x = month,fill=hour_class),position="fill")+ggtitle("Classify Monthly UAS sightings on the basis of day and night time")+xlab("# UAS sightings")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(p)
```
![](https://github.com/mukul13/2016-13/blob/master/mukul/images/Classify%20Monthly%20UAS%20sightings%20on%20the%20basis%20of%20day%20and%20night%20time.jpeg)

I have considered 7 AM to 6PM as Day time and 7 Pm to 6 AM as Night time. The reason sightings in august and September is very high in night time due to spring (that's my guess). 

```r
p=ggplot() + 
  geom_map(aes(x = long, y = lat, map_id = region), data = us,
           map = us,fill="gray",color="white") + 
  geom_point(mapping = aes(x=lon,y=lat,color=hour_class,frame=month),data=subset(drones2,lon>= min(us$long) & lon<=max(us$long) &   lat>= min(us$lat) & lat<=max(us$lat)  ),size=5,alpha=0.7)+
  ggtitle("UAS occurences month : ")+theme_light()

ani.options(interval = 1, ani.width = 800, ani.height = 800)
gg_animate(p,"monthly UAS occurences.gif")
```

![](https://github.com/mukul13/2016-13/blob/master/mukul/images/monthly%20UAS%20occurences.gif)

```r
p=ggplot(data = drones2) + 
  geom_bar(mapping = aes(x = class,fill=class))+ggtitle("Whether UAS was notified to LEO or not")
plot(p)
```
![](https://github.com/mukul13/2016-13/blob/master/mukul/images/whether%20uas%20was%20notified%20or%20not.jpeg)

Large number of cases are notified to nearby LEOs. Very few cases are not notified. 

```r
####UASEventsNov2014-Aug2015
p=ggplot(data = drones2[1:nrow(xl2),]) + 
  geom_bar(mapping = aes(x = class,fill=class)) +ggtitle("UASEventsNov2014-Aug2015")
plot(p)
```

![](https://github.com/mukul13/2016-13/blob/master/mukul/images/nov-aug.jpeg)

```r
p=ggplot(data = drones2[(nrow(xl2)+1):nrow(drones2),]) + 
  geom_bar(mapping = aes(x = class,fill=class)) +ggtitle("UAS_Sightings_report_21Aug-31Jan")
plot(p)
```
![](https://github.com/mukul13/2016-13/blob/master/mukul/images/aug-jan.jpeg)

```r
p=ggplot(data=drones2) +
  geom_bar(mapping = aes(x=state,fill=class))+ ggtitle("Stateweise classification")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(p)
```
![](https://github.com/mukul13/2016-13/blob/master/mukul/images/statewise%20classification.jpeg)
California has very high number of drone sightings.Unknown cases (don't know whther they are notified or not) and not notified cases are also high in california. Other than California, Florida and New York has high drone sightings. 

```r
p=ggplot(data=drones2[drones2$yw>=201500 & drones2$yw<=201570,]) + 
  geom_bar(mapping=aes(x=yw,fill=class))+ggtitle("Weekly classification in year 2015")
plot(p)
```

![](https://github.com/mukul13/2016-13/blob/master/mukul/images/Weekly%20classification%20in%20year%202015.jpeg)

```r
p=ggplot() +
  geom_map(aes(x = long, y = lat, map_id = region), data = us,
           map = us, fill = "#ffffff", color = "#ffffff", size = 0.15) +
  geom_map(aes(fill = count, map_id = region),
           map = us, data = by_state,
           color = "#ffffff", size = 0.15)+ggtitle("UAS map")
plot(p)
```

![](https://github.com/mukul13/2016-13/blob/master/mukul/images/UAS%20map.jpeg)

```r
p=ggplot() + 
geom_map(aes(x = long, y = lat, map_id = region), data = us,
         map = us,fill="gray",color="white") + 
    geom_point(mapping = aes(x=lon,y=lat,color=class,frame=yw),data=subset(drones2,lon>= min(us$long) & lon<=max(us$long) &   lat>= min(us$lat) & lat<=max(us$lat)  ),size=7)+theme_light()+ggtitle("Weekly UAS occurences")

ani.options(interval = 0.5, ani.width = 800, ani.height = 800)
gg_animate(p,"weekly UAS occurences.gif")
```
![](https://github.com/mukul13/2016-13/blob/master/mukul/images/weekly%20UAS%20occurences.gif)
