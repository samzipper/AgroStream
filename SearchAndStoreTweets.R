## SearchAndStoreTweets.R
#' This script is intended to:
#'  (1) search Twitter for a keyword or set of keywords
#'  (2) download all matching Tweets
#'  (3) extract the location of the tweeter via Google Maps
#'  (4) save the output as a CSV file

rm(list=ls())

# path to git directory
git.dir <- "C:/Users/Sam/WorkGits/AgroStream/"

# load packages
require(rtweet)
require(lubridate)
require(ggmap)
require(stringr)
require(maptools)
require(DBI)
require(ROAuth)
require(dplyr)

# get today/yesterday dates
date_today <- as.Date(Sys.time())
date_yesterday <- date_today-days(1)

# search string: what will you search twitter for?
search.str.1 <- paste0("((corn OR soy OR wheat) AND (plant OR planting OR planted OR plants OR #plant17 OR #plant2017 OR #plant18 OR #plant2018 OR harvest OR harvesting OR harvested OR harvests OR #harvest17 OR #harvest2017 OR #harvest18 OR #harvest2018) since:", as.character(date_yesterday), " until:", as.character(date_today))
search.str.2 <- paste0("#corn17 OR #corn2017 OR #corn18 OR #corn2018 OR #corn19 OR #corn2019 OR #soy17 OR #soy2017 OR #soy18 OR #soy2018 OR #soy19 OR #soy2019 OR #wheat17 OR #wheat2017 OR #wheat18 OR #wheat2018 OR #wheat19 OR #wheat2019  since:", as.character(date_yesterday), " until:", as.character(date_today))

# output directory: save to Dropbox, not git repository, so it's automatically backed up
# this is also where authentication info is stored
out.dir <- "C:/Users/gsas/OneDrive - The University of Kansas/Research/Twitter/AgroStream/"
#out.dir <- "C:/Users/Sam/Dropbox/Work/Twitter/AgroStream/"
#out.dir <- "D:/Dropbox/Work/Twitter/AgroStream/"

# path to save output data
path.out <- paste0(out.dir, "rTweetsOut.sqlite")

# path to save the screen output
path.sink <- paste0(out.dir, "rTweetsOut_Screen_", format(Sys.time(), "%Y%m%d-%H%M"), ".txt")

# read in token which was created with script rtweet_SetUpToken.R
r.token <- readRDS(file.path(out.dir, "twitter_token.Rds"))

## launch sink file, which will store screen output 
# this is useful when automating, so it can be double-checked later
# to make sure nothing weird happened
s <- file(path.sink, open="wt")
sink(s, type="message")

# status update
print(paste0("starting, from ", date_yesterday, " to ", date_today))

# load existing tweet SQLite database
db <- dbConnect(RSQLite::SQLite(), path.out)
df.in <- dbReadTable(db, "tweets")

# path to a CSV file with a list of all countries 
# (downloaded from: http://blog.plsoucy.com/2012/04/iso-3166-country-code-list-csv-sql/ )
path.countries <- paste0(git.dir, "AllCountries.csv")

# search twitter!
tweets <- search_tweets2(c(search.str.1, search.str.2),
                         n=10000, 
                         geocode='39.833333,-98.583333,1500mi',
                         type="recent",
                         include_rts=F,
                         retryOnRateLimit=T,
                         token=r.token)

# subset to yesterday only, just in case...
df <- subset(tweets, created_at >= date_yesterday & created_at < date_today)

# get rid of duplicates just in case
df <- unique(df)

## using Google Maps API, get estimated geographic coordinates based on user location
# limit of 2500/day! so, get clean location as much as possible first to minimize calls to API

# get user location
df.users <- lookup_users(df$screen_name,
                         token=r.token)

# trim to only users with location info
df.users <- df.users[df.users$location != "",]

# replace % and # in user location with blank so geocode doesn't get messed up
df.users$location <- gsub("%", " ",df.users$location)
df.users$location <- gsub("#", " ",df.users$location)
df.users$location <- gsub("$", " ",df.users$location)
df.users$location <- gsub("&", "and",df.users$location)

# deal with emojis and other weird characters
df.users$location <- iconv(df.users$location, "UTF-8", "ASCII", sub="")

# trim leading/trailing white space
df.users$location <- trimws(df.users$location)

## filter locations by US only (partial string matching)
# load countries
df.countries <- read.csv(path.countries, stringsAsFactors=F)
df.countries <- subset(df.countries, code != "US")   # get rid of US from list

# add some more countries/other things to filter
countries <- c(df.countries$name, 
               "Netherlands", "M?xico", "Guam", 
               "Alberta", "Saskatchewan", "British Columbia", "Yukon Territories", "Ontario", "Quebec",
               "Nunavut", "Northwest Territories", "Yukon Territory", "Prince Edward Island", "Newfoundland",
               "Alaska", "Hawaii", "Africa", "Asia", "Europe", "Australia")

# eliminate for any location that includes a country name that's not the US
df.users <- df.users[
  unlist(lapply(X=df.users$location, 
                FUN=function(x) sum(str_detect(tolower(x), tolower(countries))))
  )==0, ]

## filter locations to eliminate any that are just a large geographic region name (exact matching)
big.geo <- c("United Nations", "Earth", "United States", "USA", "US", "America", "United States of America",
             "North America", "South America")

# get rid of locations that are just a state name
df.users <- df.users[!(df.users$location %in% big.geo), ]

# get unique locations
locations <- unique(df.users$location)

# get rid of any locations that are empty
locations <- locations[!(locations %in% c(" ", ""))]

# figure out which locations have already been geocoded
locations.exist <- locations[str_to_lower(locations) %in% str_to_lower(df.in$location)]
df.locations.exist <- 
  data.frame(location = locations.exist,
             lat.location = df.in$lat.location[match(str_to_lower(locations.exist), str_to_lower(df.in$location))],
             lon.location = df.in$lon.location[match(str_to_lower(locations.exist), str_to_lower(df.in$location))])
df.locations.exist <- subset(df.locations.exist, is.finite(lat.location))
locations <- locations[!(locations %in% df.locations.exist$location)]

# status update
print(paste0(length(locations), " locations to geocode"))

# make vector to hold empty locations
lat.location <- rep(NaN, length(locations))
lon.location <- rep(NaN, length(locations))
success <- rep(F, length(locations))

# call geocode for each location
maxtries <- 5
for (l in 1:length(locations)){
  check.geocode <- F
  check.status <- F
  tries <- 0
  while (!check.geocode & tries <= maxtries){
    # count number of tries
    tries <- tries + 1
    
    # geocode
    l.geo <- ggmap::geocode(locations[l], source="google", output="all")
    
    # check if success
    if (l.geo$status != "OVER_QUERY_LIMIT") check.geocode <- T
    if (l.geo$status == "OK") check.status <- T
  }
  
  if (check.status){
    # check if location not ambiguous
    check.ambig <- if (length(l.geo$results)==1) T else F
    
    # check if location resolved to state level
    #   acceptable google address component codes, from https://developers.google.com/maps/documentation/geocoding/intro
    add.comp.state <- c("locality", "postal_code", "neighborhood", "park", "sublocality", "locality",
                        paste0("administrative_area_level_", seq(1,5)))
    add.comps <- unlist(l.geo$results[[1]]$address_components)
    if (sum(add.comps[which(names(add.comps)=="types1")] %in% add.comp.state) > 0){
      check.state <- T
    } else {
      check.state <- F
    }
    
    # figure out: is this a good geocode?
    if (check.status & check.ambig & check.state){
      success[l] <- T
      lat.location[l] <- l.geo$results[[1]]$geometry$location$lat
      lon.location[l] <- l.geo$results[[1]]$geometry$location$lng
    }
    
  }
  
}

## make final locations data frame
df.locations <- rbind(df.locations.exist,
                      data.frame(
                        location = locations[success],
                        lat.location = lat.location[success],
                        lon.location = lon.location[success]
                      ))

# status update
print(paste0(sum(success), " locations successfully geocoded"))

# add location info back to user data frame
df.users <- left_join(df.users[c("location", "description", "screen_name")], df.locations, by="location", all.x=T)

# make output data frame including tweet, user, location, etc.
df.out <- left_join(df, df.users, by="screen_name", all.x=T)

# put in order
df.out <- df.out[order(df.out$status_id), ]

# convert dates to character string for database
df.out$created_at <- as.character(df.out$created_at)

## convert columns that are lists to text strings separated by _<>_
# find list columns
cols.list <- which(lapply(df.out, class) == "list")

for (col in cols.list){
  df.out[,col] <- apply(df.out[,col], 1, function(x) as.character(paste(x, collapse="_<>_")))
}

## put into database
# add data frame to database (if it doesn't exist, it will be created)
dbWriteTable(db, "tweets", df.out, append=T)

# if you want to read in a data frame from your db to check...
#df.test <- dbReadTable(db, "tweets")
#dbWriteTable(db, "tweets", df.test, overwrite=T)

# when you're done, disconnect from database (this is when the data will be written)
dbDisconnect(db)

# print status update
print(paste0(dim(df.out)[1], " tweets added to database"))

# close sink
close(s)
sink()
sink(type="message")
close(s)

# # make a plot
# state_map <- map_data("state")
# p.map <-
#   ggplot(data=df.out, aes(x=lon.location, y=lat.location)) +
#   geom_path(data=state_map, color="blue", aes(x=long, y=lat, group=factor(region))) +
#   geom_point(shape=21) +
#   coord_map() +
#   theme_bw() +
#   theme(panel.grid=element_blank())
