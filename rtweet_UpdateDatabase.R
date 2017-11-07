## rtweet_UpdateDatabase.R
#' This script is intended to update the database to match the column names from the rtweet package,
#' which is replacing the deprecated twitteR package. It will create a new SQLite database names rTweetsOut.sql
#' 
#' This reduces the number of tweets from 16,239 to 15,698, presumably due to tweets that were deleted

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
require(dbplyr)

# output directory: save to Dropbox, not git repository, so it's automatically backed up
# this is also where authentication info is stored
out.dir <- "C:/Users/Sam/Dropbox/Work/Twitter/AgroStream/"
#out.dir <- "D:/Dropbox/Work/Twitter/AgroStream/"

# path to save output data
path.in <- paste0(out.dir, "TweetsOut.sqlite")
path.out <- paste0(out.dir, "rTweetsOut.sqlite")

# load existing tweet SQLite database
db.in <- dbConnect(RSQLite::SQLite(), path.in)
db.out <- dbConnect(RSQLite::SQLite(), path.out)

# read in existing tweet database
df.in <- dbReadTable(db.in, "tweets")

# pieces of old data frame to save
df.old <- data.frame(status_id = df.in$id, 
                     location = df.in$location,
                     description = df.in$description,
                     lat.location = df.in$lat.location,
                     lon.location = df.in$lon.location)

# re-look up
df.new <- lookup_statuses(df.old$status_id)

# add location/description, which was previously found using SearchAndStoreTweets 
# when these tweets were originally added to the database
df.out <- left_join(df.new, df.old, by="status_id")

## convert columns that are lists to text strings separated by _<>_
# find list columns
cols.list <- which(lapply(df.out, class) == "list")

for (col in cols.list){
  df.out[,col] <- apply(df.out[,col], 1, function(x) as.character(paste(x, collapse="_<>_")))
}

# convert dates to character string for database
df.out$created_at <- as.character(df.out$created_at)

# empty query column
df.out$query <- "NA"

# add data frame to database (if it doesn't exist, it will be created)
dbWriteTable(db.out, "tweets", df.out, overwrite=T)

# test
#df.test <- dbReadTable(db.out, "tweets")

# when you're done, disconnect from database (this is when the data will be written)
dbDisconnect(db.in)
dbDisconnect(db.out)
