## TweetsOut_01_ClassifyByState.R
#' This script is intended to load a data frame of tweets from an
#' SQLite database generated with the script SearchAndStoreTweets.R
#' and figure out what state each user is from.

rm(list=ls())

# path to git directory
git.dir <- "C:/Users/Sam/WorkGits/AgroStream/"

# load packages
require(twitteR)
require(lubridate)
require(ggmap)
require(stringr)
require(maptools)
require(DBI)
require(ROAuth)
require(dplyr)
require(sp)
require(maptools)
require(maps)
require(rgdal)
require(viridis)
require(ggthemes)

# function for state abbreviation - function from https://gist.github.com/ligyxy/acc1410041fe2938a2f5
abb2state <- function(name, convert = F, strict = F){
  data(state)
  # state data doesn't include DC
  state = list()
  state[['name']] = c(state.name,"District Of Columbia")
  state[['abb']] = c(state.abb,"DC")
  
  if(convert) state[c(1,2)] = state[c(2,1)]
  
  single.a2s <- function(s){
    if(strict){
      is.in = tolower(state[['abb']]) %in% tolower(s)
      ifelse(any(is.in), state[['name']][is.in], NA)
    }else{
      # To check if input is in state full name or abb
      is.in = rapply(state, function(x) tolower(x) %in% tolower(s), how="list")
      state[['name']][is.in[[ifelse(any(is.in[['name']]), 'name', 'abb')]]]
    }
  }
  sapply(name, single.a2s)
}

# path to database
path.out <- paste0(git.dir, "TweetsOut.sqlite")

# connect to database
db <- dbConnect(RSQLite::SQLite(), path.out)

# read in table
df <- dbReadTable(db, "tweets")

# when you're done, disconnect from database (this is when the data will be written)
dbDisconnect(db)

## convert lat/long to states
## based on StackOverflow answer here: https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r
# read in shapefile of states
states.shp <- readOGR(paste0(git.dir, "US_States"), "US_States")

# make SpatialPoints from database
locations.pts <- SpatialPoints(data.frame(lon = df$lon.location,
                                          lat = df$lat.location))

locations.geotag <- data.frame(lon=as.numeric(df$longitude),
                               lat=as.numeric(df$latitude))

locations.geotag.pts <- SpatialPoints(locations.geotag[is.finite(locations.geotag$lon), ])

# make sure same projection
proj4string(locations.pts) <- proj4string(states.shp)
proj4string(locations.geotag.pts) <- proj4string(states.shp)

# add state name to data frame
df$state <- as.character(over(locations.pts, states.shp)$name)
df$state.geotag[is.finite(locations.geotag$lon)] <- as.character(over(locations.geotag.pts, states.shp)$name)

# get rid of tweets without a state (out of the US)
df <- df[!is.na(df$state),]

# convert state names to lowercase with column name 'region' for merge with map data
df$region <- str_to_lower(df$state)

# summarize by state
df.state <- dplyr::summarize(group_by(df, region),
                             n.tweets = sum(is.finite(as.numeric(id))))
df.state$state.abb <- abb2state(df.state$region, convert=T)

# prepare polygon for ggplot
data.maps <- map_data("state")
df.map <- left_join(data.maps, df.state, by="region")
df.map <- df.map[order(df.map$order),]

# bar chart of states
p.tweets.state.bar <- 
  ggplot(df.state, aes(x=state.abb, y=n.tweets)) +
  geom_bar(stat="identity") +
  scale_y_continuous(name="Tweets") +
  scale_x_discrete(name="State") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.text.x=element_text(angle=90,hjust=1, vjust=0.5))

p.tweets.state.hist <-
  ggplot(df.state, aes(x=n.tweets)) +
  geom_histogram(binwidth=50) +
  scale_x_continuous(name="Tweets") +
  theme_bw() +
  theme(panel.grid=element_blank())

p.tweets.map <-
  ggplot(df.map, aes(x=long, y=lat, group=group, fill=n.tweets)) +
  geom_polygon(color="white") +
  scale_fill_viridis(name="log(Tweets)") +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  coord_map() +
  theme_bw() +
  theme(panel.grid=element_blank(),
        panel.border=element_blank(),
        legend.position="bottom")

## compare geotagged to location-based states
# data frame for geotagged only
df.geotag <- subset(df, !is.na(df$state.geotag))

# total number of geotagged
dim(df.geotag)[1]

# percent geotagged
round(100*dim(df.geotag)[1]/dim(df)[1], 2)

# number of geotagged and location that match states
sum(df.geotag$state==df.geotag$state.geotag)

# percent geotagged and location that match states
round(100*sum(df.geotag$state==df.geotag$state.geotag)/dim(df.geotag)[1], 2)
