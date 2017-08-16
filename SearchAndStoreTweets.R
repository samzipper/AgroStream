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
require(twitteR)
require(lubridate)
require(ggmap)
require(stringr)
require(maptools)
require(DBI)
require(ROAuth)

# search string: what will you search twitter for?
search.str.1 <- "((corn OR soy OR wheat) AND (plant OR planting OR planted OR plants OR #plant17 OR #plant2017 OR #plant18 OR #plant2018 OR harvest OR harvesting OR harvested OR harvests OR #harvest17 OR #harvest2017 OR #harvest18 OR #harvest2018))"
search.str.2 <- "#corn17 OR #corn2017 OR #corn18 OR #corn2018 OR #corn19 OR #corn2019 OR #soy17 OR #soy2017 OR #soy18 OR #soy2018 OR #soy19 OR #soy2019 OR #wheat17 OR #wheat2017 OR #wheat18 OR #wheat2018 OR #wheat19 OR #wheat2019"

# output directory: save to Dropbox, not git repository, so it's automatically backed up
# this is also where authentication info is stored
#out.dir <- "C:/Users/Sam/Dropbox/Work/Twitter/AgroStream/"
out.dir <- "D:/Dropbox/Work/Twitter/AgroStream/"

# path to save output data
path.out <- paste0(out.dir, "TweetsOut.sqlite")

# path to save the screen output
path.sink <- paste0(out.dir, "TweetsOut_Screen_", format(Sys.time(), "%Y%m%d-%H%M"), ".txt")

## launch sink file, which will store screen output 
# this is useful when automating, so it can be double-checked later
# to make sure nothing weird happened
s <- file(path.sink, open="wt")
sink(s, type="message")

# path to a CSV file with a list of all countries 
# (downloaded from: http://blog.plsoucy.com/2012/04/iso-3166-country-code-list-csv-sql/ )
path.countries <- paste0(git.dir, "AllCountries.csv")

# relative path to authentication info (this is in .gitignore
# so not shared publicly). these are obtained from twitter/google
# when you create your app.
path.auth.t <- paste0(out.dir, "TwitterAuth.txt")

# read in authentication info - file has three lines of 
# comments followed by:
#   [1] consumer key (API key)
#   [2] consumer secret (API secret)
#   [3] access token
#   [4] access secret
auth.t <- read.table(path.auth.t, skip=3, stringsAsFactors=F)[,1]    # read in as vector

# set up authentication
options(httr_oauth_cache=T)   # this will store authentication as a local file
setup_twitter_oauth(auth.t[1], auth.t[2], auth.t[3], auth.t[4])

# get today/yesterday dates
date_today <- as.Date(Sys.time())
date_yesterday <- date_today-days(1)

# search twitter!
tweets.1 <- searchTwitter(search.str.1, 
                          n=10000, 
                          geocode='39.833333,-98.583333,1500mi',
                          resultType="recent",
                          since=as.character(date_yesterday),
                          until=as.character(date_today),
                          retryOnRateLimit=5000)

tweets.2 <- searchTwitter(search.str.2, 
                          n=10000, 
                          geocode='39.833333,-98.583333,1500mi',
                          resultType="recent",
                          since=as.character(date_yesterday),
                          until=as.character(date_today),
                          retryOnRateLimit=5000)

tweets <- append(tweets.1, tweets.2)

# get rid of retweets
tweets <- strip_retweets(tweets, strip_manual=T, strip_mt=T)

# put into data frame (only categories we care about)
df <- twListToDF(tweets)[,c("text", "created", "id", "screenName", "isRetweet", "longitude", "latitude")]

# get rid of duplicates just in case
df <- unique(df)

# convert text to UTF-8 to deal with weird characters
df$text <- sapply(df$text, function(row) iconv(row, to='UTF-8'))
df$text <- gsub("\n", " ", df$text)

# get rid of tweets referring to The Tribez (an android game where people plant things)
df <- subset(df, !grepl(tolower("The Tribez"), tolower(df$text)))

## using Google Maps API, get estimated geographic coordinates based on user location
# limit of 2500/day! so, get clean location as much as possible first to minimize calls to API

# get user location
userInfo <- lookupUsers(df$screenName)
df.users <- twListToDF(userInfo)

# trim to only users with location info
df.users <- df.users[df.users$location != "",]

# replace % and # in user location with blank so geocode doesn't get messed up
df.users$location <- gsub("%", " ",df.users$location)
df.users$location <- gsub("#", " ",df.users$location)

# trim leading/trailing white space
df.users$location <- trimws(df.users$location)

## filter locations by US only (partial string matching)
# load countries
df.countries <- read.csv(path.countries, stringsAsFactors=F)
df.countries <- subset(df.countries, code != "US")   # get rid of US from list

# add some more countries/other things to filter
countries <- c(df.countries$name, 
               "Netherlands", "México", "Guam", 
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

# call geocode
geo.out <- geocode(locations, source="google", output="all")

## filter output
# status check: did geocode find a location?
check.status <- sapply(geo.out, function(x) x["status"]=="OK" & length(x["status"])>0)
check.status[is.na(check.status)] <- F
geo.out <- geo.out[check.status]
locations <- locations[check.status]

# status check: is location ambiguous?
check.ambig <- sapply(lapply(geo.out, lapply, length), function(x) x["results"]=="1")
geo.out <- geo.out[check.ambig]
locations <- locations[check.ambig]

# status check: is location resolved to state level?
# acceptable google address component codes, from https://developers.google.com/maps/documentation/geocoding/intro
add.comp.state <- c("locality", "postal_code", "neighborhood", "park", "sublocality", "locality",
                    paste0("administrative_area_level_", seq(1,5)))

state.resolved <- function(i.location, geocodes=geo.out){
  # custom function to determine if any subcounty address component exists
  #   input, i.location, is the index of a point in the geo.out list
  #   output will be a logical (T/F)
  
  # check if it found any results
  if (length(geocodes[[i.location]]$results)>0){
    
    # figure out number of address components returned for this location
    n.add.comp <- length(geocodes[[i.location]]["results"]$results[[1]]$address_components)
    
    # extract address component types for this location
    add.comp.types <- unlist(sapply(1:n.add.comp, function(x) unlist(geocodes[[i.location]]["results"]$results[[1]]$address_components[[x]]$types)))
    
    # see if any address component types are at subcounty level
    return(sum(add.comp.types %in% add.comp.state)>0)
    
  } else {
    # if no results found, output is false
    
    return(FALSE)
    
  }
} 

check.state <- unlist(lapply(1:length(locations), FUN=state.resolved))   # apply state check function
geo.out <- geo.out[check.state]
locations <- locations[check.state]

## make final locations data frame
df.locations <- data.frame(
  location = locations,
  lat.location = sapply(geo.out, function(x) x["results"]$results[[1]]$geometry$location$lat),
  lon.location = sapply(geo.out, function(x) x["results"]$results[[1]]$geometry$location$lng)
)

# add location info back to user data frame
df.users <- merge(df.users[c("location", "description", "screenName")], df.locations, by="location", all.x=T)

# make output data frame including tweet, user, location, etc.
df.out <- merge(df, df.users, by="screenName", all.x=T)

# trim output data frame to only those with locations (either geotagged or from google)
df.out <- df.out[(is.finite(df.out$latitude) & is.finite(df.out$longitude)) | 
                   (is.finite(df.out$lat.location) & is.finite(df.out$lon.location)), ]

# get rid of commas
df.out$text <- gsub(",", " ", df.out$text)
df.out$location <- gsub(",", " ", df.out$location)
df.out$description <- gsub(",", " ", df.out$description)

# get rid of line breaks
df.out$text <- gsub("\n", " ", df.out$text)
df.out$location <- gsub("\n", " ", df.out$location)
df.out$description <- gsub("\n", " ", df.out$description)

# get rid of URLs
removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x)
df.out$text <- unlist(lapply(df.out$text, removeURL))

# trim leading/trailing white space
df.out$text <- trimws(df.out$text)

# get rid of duplicate tweets
df.out <- df.out[!duplicated(df.out[c("screenName", "text")]), ]

# put in order
df.out <- df.out[order(df.out$id), ]

# convert dates to character string for database
df.out$created <- as.character(df.out$created)

## put into database
# create/connect to database
db <- dbConnect(RSQLite::SQLite(), path.out)

# add data frame to database (if it doesn't exist, it will be created)
dbWriteTable(db, "tweets", df.out, append=T)

# if you want to read in a data frame from your db to check...
#df.test <- dbReadTable(db, "tweets")

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
