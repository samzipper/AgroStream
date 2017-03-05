## SearchAndStoreTweets_CornPlanting.R
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

# search string: what will you search twitter for?
search.str <- "((corn OR soy OR wheat) AND (plant OR planting OR planted OR plants OR #plant17 OR #plant2017)) OR #corn17 OR #corn2017 OR #soy17 OR #soy2017 OR #wheat17 OR #wheat2017"

# path to save output CSV
path.out <- paste0(git.dir, "TweetsOut.csv")

# path to save the last tweet ID
path.lastID <- paste0(git.dir, "TweetsOut_LastID.txt")

# path to a CSV file with a list of all countries 
# (downloaded from: http://blog.plsoucy.com/2012/04/iso-3166-country-code-list-csv-sql/ )
path.countries <- paste0(git.dir, "AllCountries.csv")

# relative path to authentication info (this is in .gitignore
# so not shared publicly). these are obtained from twitter/google
# when you create your app.
path.auth.t <- paste0(git.dir, "TwitterAuth.txt")

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

# check if lastID file exists
if (file.exists(path.lastID)){
  lastID <- read.csv(path.lastID)$id
  
  # search twitter!
  tweets <- searchTwitter(search.str, 
                          n=10000, 
                          geocode='39.833333,-98.583333,1500mi',
                          resultType="recent",
                          sinceID=lastID,
                          retryOnRateLimit=5000)
  
} else {
  # search twitter using last week
  date_today <- as.Date(Sys.time())
  date_1week <- date_today - days(7)
  
  tweets <- searchTwitter(search.str, 
                          n=10000, 
                          geocode='39.833333,-98.583333,1500mi',
                          resultType="recent",
                          since=as.character(date_1week),
                          retryOnRateLimit=5000)
}

# get rid of retweets
tweets <- strip_retweets(tweets, strip_manual=T, strip_mt=T)

# put into data frame (only categories we care about)
df <- twListToDF(tweets)[,c("text", "created", "id", "screenName", "isRetweet", "longitude", "latitude")]

# convert text to UTF-8 to deal with weird characters
df$text <- sapply(df$text, function(row) iconv(row, to='UTF-8'))
df$text <- gsub("\n", " ", df$text)

## using Google Maps API, get estimated geographic coordinates based on user location
# limit of 2500/day! so, get clean location as much as possible first to minimize calls to API

# get user location
userInfo <- lookupUsers(df$screenName)
df.users <- twListToDF(userInfo)

# trim to only users with location info
df.users <- df.users[df.users$location != "",]

# replace % in user location with blank so geocode doesn't get messed up
df.users$location <- gsub("%", " ",df.users$location)

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
big.geo <- c("United Nations", "Earth", "United States", "USA", "US", "America", 
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

# status check: is location ambiguous?
check.ambig <- sapply(lapply(geo.out, lapply, length), function(x) x["results"]=="1")

# status check: is location resolved to subcounty level?
# acceptable subcounty codes, from https://developers.google.com/maps/documentation/geocoding/intro
add.comp.subcounty <- c("locality", "postal_code", "neighborhood", "park", "sublocality", "locality",
                        paste0("administrative_area_level_", seq(2,5)))

subcounty <- function(i.location, geocodes=geo.out){
  # custom function to determine if any subcounty address component exists
  #   input, i.location, is the index of a point in the geo.out list
  #   output will be a logical (T/F)
  
  # check if it found any results
  if (length(geocodes[[i.location]]$results)>0){
    
    # figure out number of address components returned for this location
    n.add.comp <- length(geocodes[[i.location]]["results"]$results[[1]]$address_components)
    
    # extract address component types for this location
    add.comp.types <- c(sapply(1:n.add.comp, function(x) unlist(geocodes[[i.location]]["results"]$results[[1]]$address_components[[x]]$types)))
    
    # see if any address component types are at subcounty level
    return(sum(add.comp.types %in% add.comp.subcounty)>0)
    
  } else {
    # if no results found, output is false
    
    return(FALSE)
    
  }
}

check.subcounty <- unlist(lapply(1:length(locations), FUN=subcounty))   # apply subcounty function

# combine all checks into a single logical
check.all <- check.status & check.ambig & check.subcounty

# trim geo.out
geo.out <- geo.out[check.all]

## make final locations data frame
df.locations <- data.frame(
  location = locations[check.all],
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

# save output
if (file.exists(path.out)){
  write.table(df.out, path.out, sep=",", row.names=F, col.names=F, append=T)
} else {
  write.table(df.out, path.out, sep=",", row.names=F)
}

# write a text file with the last ID found, which will be used for future runs
write.table(data.frame(id=max(df.out$id)), path.lastID, row.names=F)

# # make a plot
# state_map <- map_data("state")
# p.map <-
#   ggplot(data=df.out, aes(x=lon.location, y=lat.location)) +
#   geom_path(data=state_map, color="blue", aes(x=long, y=lat, group=factor(region))) +
#   geom_point(shape=21) +
#   coord_map() +
#   theme_bw() +
#   theme(panel.grid=element_blank())
