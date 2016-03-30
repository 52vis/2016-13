library(ggplot2)
library(ggalt)
library(ggthemes)
library(readxl)
library(dplyr)
library(hrbrmisc)
library(grid)

URL1 <- "http://www.faa.gov/uas/media/UAS_Sightings_report_21Aug-31Jan.xlsx"
URL2 <- "http://www.faa.gov/uas/media/UASEventsNov2014-Aug2015.xls"

fil1 <- basename(URL1)
fil2 <- basename(URL2)

if (!file.exists(fil1)) download.file(URL1, fil1)
if (!file.exists(fil2)) download.file(URL2, fil2)

xl1 <- read_excel(fil1)
xl2 <- read_excel(fil2)

drones <- setNames(bind_rows(xl2[,1:3],
                             xl1[,c(1,3,4)]), 
                   c("ts", "city", "state"))
drones <- mutate(drones, 
                 year=format(ts, "%Y"), 
                 year_mon=format(ts, "%Y%m"), 
                 ymd=as.Date(ts), 
                 yw=format(ts, "%Y%V"))

by_week <- mutate(count(drones, yw), wk=as.Date(sprintf("%s1", yw), "%Y%U%u")-7)
by_week <- arrange(filter(by_week, wk>=as.Date("2014-11-10")), wk)

gg <- ggplot(by_week, aes(wk, n))
gg <- gg + geom_bar(stat="identity", fill="#937206")
gg <- gg + annotate("text", by_week$wk[1], 49, label="# reports", 
                    hjust=0, vjust=1, family="Cabin-Italic", size=3)
gg <- gg + scale_x_date(expand=c(0,0))
gg <- gg + scale_y_continuous(expand=c(0,0))
gg <- gg + labs(y=NULL,
                title="Weekly U.S. UAS (drone) sightings",
                subtitle="As reported to the Federal Aviation Administration",
                caption="Data from: http://www.faa.gov/uas/law_enforcement/uas_sighting_reports/")
gg <- gg + theme_hrbrmstr(grid="Y", axis="X")
gg <- gg + theme(axis.title.x=element_text(margin=margin(t=-6)))
gg

