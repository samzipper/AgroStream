
rm(list=ls())

# path to git directory
git.dir <- "C:/Users/Sam/WorkGits/AgroStream/"

# load packages
require(twitteR)
require(lubridate)
require(ggmap)
require(stringr)

# path to a CSV file with a list of all countries 
# (downloaded from: http://blog.plsoucy.com/2012/04/iso-3166-country-code-list-csv-sql/ )
path.countries <- paste0(git.dir, "AllCountries.csv")

# relative path to authentication info (this is in .gitignore
# so not shared publicly). these are obtained from twitter/google
# when you create your app.
path.auth.t <- paste0(git.dir, "TwitterAuth.txt")
path.auth.g <- paste0(git.dir, "GoogleAuth.txt")

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

# get only tweets from yesterday
date.today <- as.Date(Sys.time())      # today's date
date.yesterday <- date.today-days(1)   # yesterday's date

# search twitter!
tweets <- searchTwitter("#drought", n=10000, 
                        geocode='39.833333,-98.583333,1500mi',
                        resultType="recent",
                        since=as.character(date.yesterday),
                        until=as.character(date.today),
                        retryOnRateLimit=500)

tweets <- strip_retweets(tweets, strip_manual=T, strip_mt=T)

# put into data frame
df <- twListToDF(tweets)

# trim to only categories we care about
df <- df[,c("text", "created", "id", "screenName", "isRetweet", "longitude", "latitude")]

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
               "Alaska", "Hawaii")

# eliminate for any location that includes a country name that's not the US
df.users <- df.users[
  unlist(lapply(X=df.users$location, 
                FUN=function(x) sum(str_detect(tolower(x), tolower(countries))))
         )==0, ]

## filter locations to eliminate any that are just a state name (exact matching)
# load states dataset
data("state")
states <- c(state.abb, state.name, "United Nations", "Earth", "United States", 
            "USA", "North America", "Europe", "Africa", "Australia", "Asia", "South America", "America")

# get rid of locations that are just a state name
df.users <- df.users[!(df.users$location %in% states), ]

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
  
  # figure out number of address components returned for this location
  n.add.comp <- length(geocodes[[i.location]]["results"]$results[[1]]$address_components)
  
  # extract address component types for this location
  add.comp.types <- c(sapply(1:n.add.comp, function(x) unlist(geocodes[[i.location]]["results"]$results[[1]]$address_components[[x]]$types)))
  
  # see if any address component types are at subcounty level
  return(sum(add.comp.types %in% add.comp.subcounty)>0)
}

check.subcounty <- unlist(lapply(1:length(locations), FUN=subcounty))   # apply subcounty function

# combine all checks into a single logical
check.all <- check.status & check.ambig & check.subcounty

## make final locations data frame
df.locations <- data.frame(
  location = locations[check.all],
  lat.location = sapply(geo.out, function(x) x["results"]$results[[1]]$geometry$location$lat)[check.all],
  lon.location = sapply(geo.out, function(x) x["results"]$results[[1]]$geometry$location$lng)[check.all]
)

# add location info back to user data frame
df.users <- merge(df.users[c("location", "description", "screenName")], df.locations, by="location", all.x=T)

# make output data frame including tweet, user, location, etc.
df.out <- merge(df, df.users, by="screenName", all.x=T)

# trim output data frame to only those with locations (either geotagged or from google)
df.out <- df.out[(is.finite(df.out$latitude) & is.finite(df.out$longitude)) | 
                   (is.finite(df.out$lat.location) & is.finite(df.out$lon.location)), ]
