Nov2014Aug2015 <- read.csv("~/R/dataviz/UASEventsNov2014-Aug2015.csv")
Aug2015Jan2016 <- read.csv("~/R/dataviz/UAS_Sightings_report_21Aug-31Jan.csv")

Nov2014Aug2015$EventREPORTNARRATIVE <- NULL
Nov2014Aug2015$Item.Type <- NULL
Nov2014Aug2015$Path <- NULL

Aug2015Jan2016$EventREPORTNARRATIVE <- NULL

sightings <- rbind(Nov2014Aug2015, Aug2015Jan2016)
sightings$city <- as.character(sightings$LocationCITY)
sightings$datetime <- as.character(sightings$EventDATETIME)
sightings$datetime <- as.POSIXct(sightings$datetime,  format = "%d.%m.%Y %H:%M")
sightings$datetime <- as.POSIXct(sightings$datetime, tz="US/Central")
sightings$weekdays <- weekdays(sightings$datetime)
sightings$months <- months(sightings$datetime)
library(lubridate)
sightings$hours <- hour(sightings$datetime)
sightings$weekend <- ifelse(sightings$weekdays %in% c("Samstag", "Sonntag"),1,0)
sightings$weekend <- as.factor(sightings$weekend)

library(ggplot2)
gg <- ggplot(sightings, aes(x = hours, group = weekend, fill = weekend)) + 
  geom_bar() +
  scale_x_continuous(breaks = 0:23, expand = c(0,0.3)) + 
  scale_y_continuous(limits = c(0,170), breaks = seq(0,200,20), expand = c(0,0)) + 
  scale_fill_manual(labels = c("workdays", "weekend"), values = c("turquoise4", "turquoise")) +
  theme(panel.background = element_rect(fill = "honeydew"),
        panel.grid.major = element_line(colour = "darkseagreen", size=0.1),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(family = "sans", face = "bold", colour = "grey50"),
        axis.text.y = element_text(family = "sans", face = "bold", colour = "grey50"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(face = "bold", family = "NimbusSan", colour = "grey30", size=9),
        legend.text = element_text(family = "NimbusSan", face = "bold", colour = "grey50"),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "honeydew2", colour = "grey30"),
        plot.title = element_text(family = "NimbusSan"),
        plot.caption = element_text(family = "mono", face = "bold", colour = "grey30"),
        plot.subtitle = element_text(colour = "grey50", face = "italic"),
        legend.key = element_blank()) +
  labs(x = "time of day", y = "", fill = NULL,
       title = "Drones - A weekend activity?",
       subtitle = "Reports of unmanned aircraft (UAS) sightings in the US by time of day.\nNovember 2014 - January 2016.\n#52Vis",
       caption = "\nData: UAS Sighting Reports, FAA")
gg

#ggsave("./ottlngr/drones_ottlngr.png", gg)
