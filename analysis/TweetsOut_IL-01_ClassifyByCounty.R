## TweetsOut_IL-01_ClassifyByCounty.R
#' This script is intended to conduct a detailed analysis of Illinois tweets
#' at county resolution. Illinois was selected because it had the most tweets.
#' The output of TweetsOut_01_ClassifyByState.R is required.

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

## script control variables
# which state?
s <- "Illinois"

# which crops?
crop.list <- c("corn", "soy")

# start/end date for period of interest (20 weeks total)
date.start <- ymd("2017-02-27")  # start of week 9: 2017-02-27
date.end <- ymd("2017-07-16")    # end of week 28: 2017-07-16
dates.all <- seq(date.start, date.end, by="day")

## load tweet database
# path to database
path.out <- paste0(git.dir, "TweetsOut.sqlite")

# connect to database
db <- dbConnect(RSQLite::SQLite(), path.out)

# read in table
df.in <- dbReadTable(db, "tweetsWithStates")

# when you're done, disconnect from database (this is when the data will be written)
dbDisconnect(db)

## subset data
# subset to only unique tweets - some appear to have been stored twice
df <- unique(df.in)

# subset based on time
df$date <- as.Date(ymd_hms(df$created))
df <- subset(df, date>=date.start & date <= date.end)
df$DOY <- yday(df$date)
df$week <- week(df$date-days(1))  # -1 to match with NASS week endings

# subset to data that uses specific hashtags
df <- df[str_detect(str_to_lower(df$text), "#corn") | str_detect(str_to_lower(df$text), "#soy") | str_detect(str_to_lower(df$text), "#plant17"), ]

# subset to data in crop list
df <- df[str_detect(str_to_lower(df$text), crop.list[1]) | str_detect(str_to_lower(df$text), crop.list[2]), ]

# subset to state of IL (location, geotag, or mention)
df <- subset(df, state.loc==s | state.geotag==s | state.mention==abb2state(s, convert=T))

# get rid of entries which have a state.mention other than illinois
df <- subset(df, state.mention %in% c("IL", "XX"))

## figure if counties are mentioned in tweets
# read in shapefile of Illinois
IL.shp <- readOGR(dsn=paste0(git.dir, "IL_Counties"), layer="IL_BNDY_County_Py")

# get list of county names
IL.names <- as.character(IL.shp@data$COUNTY_NAM)

# figure out if county name is mentioned in any tweet
f.county.mention <- function(x, county.names){
  # first, find any state names
  county.mention <- county.names[str_detect(str_to_lower(paste0(x, " co")), str_to_lower(paste0(county.names, " co")))]
  
  if (length(county.mention)==1){
    out <- county.mention
  } else {
    out <- "XX"
  }
  
  return(out)
}

df$county.mention <- sapply(df$text, f.county.mention, county.names=IL.names)

## figure out which locations are resolved to county level using geocode
# get unique locations
locations <- unique(str_to_lower(df$location))

# call geocode
geo.out <- geocode(locations, source="google", output="all")

# status check: did geocode find a location?
check.status <- sapply(geo.out, function(x) x["status"]=="OK" & length(x["status"])>0)
check.status[is.na(check.status)] <- F
geo.out <- geo.out[check.status]
locations <- locations[check.status]

# status check: is location ambiguous?
check.ambig <- sapply(lapply(geo.out, lapply, length), function(x) x["results"]=="1")
geo.out <- geo.out[check.ambig]
locations <- locations[check.ambig]

# status check: is location resolved to county level? (administrative_area_level_2)
# acceptable google address component codes, from https://developers.google.com/maps/documentation/geocoding/intro
add.comp.state <- c("administrative_area_level_2")

county.resolved <- function(i.location, geocodes=geo.out){
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

check.county <- unlist(lapply(1:length(locations), FUN=county.resolved))   # apply county check function
geo.out <- geo.out[check.county]
locations <- locations[check.county]

## make final locations data frame
df.locations <- data.frame(
  location = locations,
  n.components = sapply(geo.out, function(x) length(x["results"]$results[[1]]$address_components)),
  county = "XX",
  state = "XX",
  stringsAsFactors=F)

# figure out which component corresponds to county
f.which.county <- function(n.components, geocode=geo.out){
  i.county <- which(sapply(seq(1, n.components), function(x) geocode["results"]$results[[1]]$address_components[[x]]$types[1])=="administrative_area_level_2")
  if (length(i.county)>0){
    county <- geocode["results"]$results[[1]]$address_components[[i.county]]$long_name
    county <- gsub(" County", "", county)
  } else {
    county <- "XX"
  }
  return(county)
}

f.which.state <- function(n.components, geocode=geo.out){
  i.state <- which(sapply(seq(1, n.components), function(x) geocode["results"]$results[[1]]$address_components[[x]]$types[1])=="administrative_area_level_1")
  if (length(i.state)>0){
    state <- geocode["results"]$results[[1]]$address_components[[i.state]]$short_name
  } else {
    state <- "XX"
  }
  return(state)
}

df$county.location <- "XX"
for (i in 1:dim(df.locations)[1]){
  df.locations$county[i] <- f.which.county(n.components=df.locations$n.components[i], geocode=geo.out[[i]])
  df.locations$state[i] <- f.which.state(n.components=df.locations$n.components[i], geocode=geo.out[[i]])
  if (df.locations$state[i]==abb2state(s, convert=T)){
    df$county.location[str_to_lower(df$location)==df.locations$location[i]] <- df.locations$county[i]
  }
}

## clean up data by county
df$county.location <- str_to_upper(df$county.location)

# subset to places that have either county mention or county location
df <- subset(df, county.location != "XX" | county.mention != "XX")

# finalize county, prioritizing mention over location
df$county <- df$county.location
df$county[df$county.mention != "XX"] <- df$county.mention[df$county.mention != "XX"]

# add to SQlite database
# connect to database
db <- dbConnect(RSQLite::SQLite(), path.out)

# read in table
dbWriteTable(db, "ILtweetsWithCounties", df)

# when you're done, disconnect from database (this is when the data will be written)
dbDisconnect(db)
