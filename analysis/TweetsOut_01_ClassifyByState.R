## TweetsOut_01_ClassifyByState.R
#' This script is intended to load a data frame of tweets from an
#' SQLite database generated with the script SearchAndStoreTweets.R
#' and figure out what state each Tweet corresponds to, then put it
#' back in the SQLite database.

rm(list=ls())

# path to git directory
git.dir <- "C:/Users/Sam/WorkGits/AgroStream/"

# load packages
require(twitteR)
require(lubridate)
require(ggmap)
require(stringr)
require(stringi)
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
require(zoo)
require(reshape2)
require(hydroGOF)
source(paste0(git.dir, "analysis/plots/plot_colors.R"))
source(paste0(git.dir, "analysis/interp.R"))

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

## load tweet database
# path to database
path.out <- paste0(git.dir, "TweetsOut.sqlite")

# connect to database
db <- dbConnect(RSQLite::SQLite(), path.out)

# read in table
df <- dbReadTable(db, "tweets")

# subset based on time
df$date <- as.Date(ymd_hms(df$created))
df$DOY <- yday(df$date)
df$week <- week(df$date-days(1))  # -1 to match with NASS week endings

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

## add state name from profile location to data frame
df$state.loc <- as.character(over(locations.pts, states.shp)$name)

## add state name from geotag to data frame
df$state.geotag[is.finite(locations.geotag$lon)] <- as.character(over(locations.geotag.pts, states.shp)$name)

## add state name if mentioned in tweet
# figure out if a state is mentioned in the text
df$state.mention <- NA   # make empty column

states.all <- as.character(states.shp$name)
states.abb.all <- abb2state(as.character(states.shp$name), convert=T)

f.state.mention <- function(x, states.all, states.abb.all){
  # first, find any state names
  state.mention <- states.all[str_detect(x, states.all)]
  state.oneword.all <- states.all[str_detect(x, gsub(" ", "", states.all))]
  state.lc.mention <- states.all[str_detect(x, str_to_lower(states.all))]
  state.caps.mention <- states.all[str_detect(x, str_to_upper(states.all))]

  # now, search for abbreviations as whole words only
  words <- unique(stri_extract_all_words(x, simplify=T))
  state.abb.mention <- words[words %in% states.abb.all]

  # if it is mentioned in multiple cases, condense to single mention per state
  state.mention <- unique(str_to_title(c(state.mention, state.oneword.all, state.lc.mention, state.caps.mention, abb2state(state.abb.mention))))
  
  if (length(state.mention)==1){
    out <- abb2state(state.mention, convert=T)
  } else {
    out <- "XX"
  }
  
  return(out)
}

state.mention <- sapply(df$text, f.state.mention, states.all=states.all, states.abb.all=states.abb.all)

# add to data frame
df$state.mention <- unlist(state.mention) 

## fill in state column with hierarchy: mention > geotag > location
df$state <- df$state.loc
df$state[which(df$state.geotag != "NA")] <- df$state.geotag[which(df$state.geotag != "NA")]
df$state[which(df$state.mention != "XX")] <- abb2state(df$state.mention[which(df$state.mention != "XX")])

# get rid of tweets without a state (out of the US)
df <- df[!is.na(df$state),]

# make state name abbreviation
df$state.abb <- abb2state(df$state, convert=T)

## write new data frame to database
# add data frame to database (if it doesn't exist, it will be created)
dbWriteTable(db, "tweetsWithStates", df, append=T)

# when you're done, disconnect from database (this is when the data will be written)
dbDisconnect(db)

## make some plots
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
