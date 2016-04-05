# ---------------------------------------------------------------------------------
# Unmanned aircraft sightings (UAS) visualizations
# ---------------------------------------------------------------------------------
# Andrew Heiss
# @andrewheiss | github.com/andrewheiss
# 2016-04-03
#
# Week 13 of @hrbrmstr's weekly visualization challenge
# https://rud.is/b/2016/03/30/introducing-a-weekly-r-python-js-etc-vis-challenge/
#
# Main data: https://www.faa.gov/uas/law_enforcement/uas_sighting_reports/
# ---------------------------------------------------------------------------------

# ----------------
# Load libraries
# ----------------
library(acs)       # Census data; ACS loads plyr behind the scenes, so load it first
library(dplyr)     # Magic dataframe manipulation
library(magrittr)  # Piping
library(tidyr)     # Tidy data
library(lubridate) # Deal with dates
library(readr)     # Read CSV files
library(readxl)    # Read Excel files
library(httr)      # Make HTTP calls
library(jsonlite)  # Use JSON
library(rvest)     # Scrape websites
library(ggplot2)   # Plotting
library(ggstance)  # Horizontal geoms
library(ggalt)     # geom_stateface()
library(Cairo)     # Embed fonts in PDFs and export nicer PNGs


# ----------
# API keys
# ----------
search.api.key <- "API_KEY_HERE"  # Mapzen API
api.key.install("API_KEY_HERE")  # ACS API


# ---------------------
# Load and munge data
# ---------------------
# -----
# FAA UAS sightings
# -----
faa.url <- "www.faa.gov/uas/law_enforcement/uas_sighting_reports/"

# UAS sightings, November 2014-August 2015
faa1.url <- "http://www.faa.gov/uas/media/UASEventsNov2014-Aug2015.xls"
faa1.tmp <- file.path(tempdir(), basename(faa1.url))
download.file(faa1.url, faa1.tmp)

# UAS sightings, August 2015-January 2016
faa2.url <- "http://www.faa.gov/uas/media/UAS_Sightings_report_21Aug-31Jan.xlsx"
faa2.tmp <- file.path(tempdir(), basename(faa2.url))
download.file(faa2.url, faa2.tmp)

# Combine the two dataframes
# Capture output to get rid of all the DEFINEDNAME output that read_excel makes
# See https://github.com/hadley/readxl/issues/82
capture.output({
  drones.raw <- bind_rows(read_excel(faa1.tmp), read_excel(faa2.tmp)) %>%
    select(1:4) %>%
    set_colnames(c("date.seen", "city", "state", "narrative"))
}, file="/dev/null")


# -----
# States and their abbreviations
# http://www.fonz.net/blog/archives/2008/04/06/csv-of-states-and-state-abbreviations/
# -----
states.abbr.url <- "http://www.fonz.net/blog/wp-content/uploads/2008/04/states.csv"
states.abbr <- read_csv(states.abbr.url)


# -----
# State population
# -----
acs.url <- "www.census.gov/programs-surveys/acs/"
# Get 2011 state population from the Census's ACS
state.population.raw <- acs.fetch(endyear=2011, geography=geo.make(state="*"),
                                  table.number="B01003", col.names="pretty")

# ACS returns an S4 object with the data as a matrix (@estimate) and state
# names as a dataframe (@geography)
state.population <- bind_cols(as.data.frame(state.population.raw@estimate), 
                              state.population.raw@geography) %>%
  select(population = 1, state = 2)


# -----
# Geoencode cities
# -----
# Simplemaps has an open source list of ≈30,000 pre-geocded cities
# http://simplemaps.com/resources/us-cities-data
cities.url <- "http://simplemaps.com/files/cities.csv"
cities.geocoded <- read_csv(cities.url)

# Match drone cities with pre-geocoded list
drones.geocoded.pre <- drones.raw %>% 
  left_join(states.abbr, by=c("state"="State")) %>%
  left_join(cities.geocoded, by=c("city", "Abbreviation"="state"))

# But this doesn't get everything; ≈200 are missing
drones.geocoded.pre %>% filter(is.na(lat)) %>% summarise(n())

# So geocode the remaining cities with Mapzen

# Send an address to the Mapzen API and return the top geocoded result
#
# Parameters:
#   address_id: row number
#   location: text of address to geocode
#
# Returns: data frame with geocoded data and row number for later left_join()ing
#
geocode <- function(row_id, location) {
  # Pause since Mapzen only allows 2 queries per second
  Sys.sleep(0.55)
  
  request <- GET("https://search.mapzen.com/v1/search", 
                 query=list(text=location,
                            api_key=search.api.key))
  json <- content(request, as="text")
  results <- fromJSON(json)$features
  results.df <- results$properties %>% bind_cols(results$geometry) %>%
    slice(1) %>%        # Get the top result
    mutate(row_id) %>%  # Add the row number
    select(row_id, coordinates)  # Only keep the row number and coordinates
  return(results.df)
}

# Select just the missing cities
drones.missing <- drones.geocoded.pre %>%
  filter(is.na(lat)) %>%
  mutate(row_id = row_number()) %>%
  select(-c(lat, lng))

# Use the Mapzen API to geocode based on "city, state"
# This takes a while (half a second per row...)
drones.geocoded <- drones.missing %>%
  mutate(search.string = paste(city, state, sep=", ")) %>%
  rowwise() %>%
  do(geocode(.$row_id, .$search.string)) %>%
  mutate(lat = unlist(coordinates[2]),  # Split the coordinates column
         lng = unlist(coordinates[1]))

# Add the lat/lon data to the dataframe of all missing cities
drones.geocoded.joined <- drones.missing %>%
  left_join(drones.geocoded, by="row_id") %>%
  select(-c(row_id, coordinates))


# -----
# Merge all this data into one big final dataframe
# -----
drones.geocoded.all <- drones.geocoded.pre %>%
  filter(!is.na(lat)) %>%
  bind_rows(drones.geocoded.joined) %>%
  mutate(year.seen = year(date.seen),
         month.seen = month(date.seen, label=TRUE, abbr=FALSE),
         my.seen = paste(month.seen, year.seen)) %>%
  arrange(date.seen) %>%
  mutate(my.seen = factor(my.seen, levels=unique(my.seen))) %>%
  # Even with Mapzen some of these sightings are way off, like in Europe or
  # Antarctica, so limit the sightings to ones roughly in lower 48 states
  filter(lat < 49, lat > 24, lng < 0)


# ----------------------------------------------------
# Are these military drones or just hobbyist drones?
# ----------------------------------------------------
# Get a list of geocoded Air Force bases
usaf.url <- "https://en.wikipedia.org/wiki/List_of_United_States_Air_Force_installations"

# The coordinates from Wikipedia export like this:
#   38°59′25″N 104°51′30″W / 38.99028°N 104.85833°W / 38.99028; -104.85833 (USAF Academy)
# We just care about the last set of decimal numbers, so use tidyr::extract + 
# a regex to extract them
usaf.bases <- read_html(usaf.url) %>%
  # Select the third table in the #mv-content-text div
  html_nodes(xpath='//*[@id="mw-content-text"]/table[3]') %>%
  html_table() %>% bind_rows() %>%
  extract(Coordinates, c("lat", "lng"), "(\\d+\\.\\d+); (-?\\d+\\.\\d+)",
          convert=TRUE) %>%
  filter(lat < 49, lat > 24, lng < 0)

# Map sightings and Air Force bases
p.map <- ggplot() +
  # Add states
  geom_polygon(data=map_data("state"), aes(x=long, y=lat, group=group),
               fill="white", colour="black", size=0.25) +
  # Add bases
  geom_point(data=usaf.bases, aes(x=lng, y=lat, colour="Air Force base"), 
             shape=15, size=2) +
  # Add drone sightings
  geom_point(data=drones.geocoded.all, aes(x=lng, y=lat, colour="UAS sighting"),
             shape=19, size=0.75, alpha=0.5) +
  # Add legend
  scale_colour_manual(name=NULL, values=c("#004259", "#BFDB3B"),
                      guide=guide_legend(override.aes=list(shape=c(15, 16)))) +
  # Make the plot fill the whole x axis
  scale_x_continuous(expand=c(0, 0)) +
  # Use an Albers projection focused just on the lower 48 states
  coord_map(projection="albers", lat0=45.5, lat1=29.5) + 
  # Labels
  labs(title="Military or hobbyist drones?", 
       subtitle="November 2014–January 2016",
       caption=paste0("Data from the FAA (", faa.url, ")")) +
  # Theme stuff
  theme_bw(base_size=10, base_family="Source Sans Pro Light") + 
  theme(panel.background = element_rect(fill="grey92", colour=NA),
        panel.border=element_blank(), axis.line=element_blank(),
        panel.grid=element_blank(), axis.ticks=element_blank(),
        axis.title=element_blank(), axis.text=element_blank(),
        legend.key = element_blank(), legend.position=c(0.1, 0.1),
        title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
        plot.title=element_text(size=rel(1.5)),
        plot.subtitle=element_text(family="Source Sans Pro"),
        plot.caption=element_text(family="Source Sans Pro Light",
                                  lineheight=0.5, size=rel(0.7)))
p.map

# Save a PDF and PNG
ggsave(p.map, filename="drones_af_map.pdf",
       width=6, height=4, units="in", device=cairo_pdf)
ggsave(p.map, filename="drones_af_map.png",
       width=6, height=4, units="in", type="cairo", dpi=300)


# --------------------------------------------------
# Which states see more drone activity per capita?
# --------------------------------------------------
# Summarize drone sightings by state
drones.state <- drones.geocoded.all %>%
  group_by(state) %>%
  summarise(sightings = n()) %>%
  left_join(state.population, by="state") %>%
  mutate(sightings.per.cap = sightings / population * 1000000) %>%
  # Add abbreviations for geom_stateface()
  left_join(states.abbr, by=c("state"="State")) %>%
  # Sort by per capita measure, take top ten states (reversed) and make an
  # ordered factor
  arrange(desc(sightings.per.cap)) %>%
  slice(10:1) %>%
  mutate(state = factor(state, levels=state, ordered=TRUE),
         sightings.per.cap.clean = round(sightings.per.cap, 0),
         state.fill = ifelse(Abbreviation == "DC", "#F28730", "#8A9C0F"))


p.per.capita <- ggplot(drones.state, aes(x=sightings.per.cap, y=state)) +
  # Horizontal bar plot
  geom_barh(aes(fill=state.fill), stat="identity") + 
  # State shapes from ProPublica: https://propublica.github.io/stateface/
  geom_stateface(aes(y=state, x=35, label=Abbreviation), colour="white", size=8) +
  # Add text
  geom_text(aes(label=sprintf("%.0f", sightings.per.cap)),
            hjust=0, color="black", nudge_x=10,
            family="Source Sans Pro Light", size=3.5) +
  # Make plot fill full x and y axes
  scale_x_continuous(expand=c(0, 0)) + scale_y_discrete(expand=c(0, 0)) +
  # Use color names as actual colors
  scale_fill_identity() +
  coord_cartesian(xlim=c(0, 900)) +
  # Labels
  labs(x=NULL, y=NULL,
       title="Drones will watch the watchmen",
       subtitle="Unmanned aircraft sightings per 1,000,000 residents\nNovember 2014–January 2016",
       caption=paste0("Data from the American Community Survey (", 
                      acs.url, ") \nand the FAA (", faa.url, ")")) +
  # Theme stuff
  theme_bw(base_size=10, base_family="Source Sans Pro Light") +
  theme(panel.background = element_rect(fill="#ffffff", colour=NA),
        panel.border=element_blank(), axis.line=element_blank(),
        panel.grid=element_blank(), axis.ticks=element_blank(),
        axis.title=element_blank(), axis.text.x=element_blank(),
        title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
        plot.title=element_text(size=rel(1.5)),
        plot.subtitle=element_text(family="Source Sans Pro"),
        plot.caption=element_text(family="Source Sans Pro Light",
                                  lineheight=0.8, size=rel(0.7)))
p.per.capita

# Save a PDF and PNG
ggsave(p.per.capita, filename="drones_states.pdf",
       width=6, height=4, units="in", device=cairo_pdf)
ggsave(p.per.capita, filename="drones_states.png",
       width=6, height=4, units="in", type="cairo", dpi=300)
