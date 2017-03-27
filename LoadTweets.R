## LoadTweets.R
#' This script is intended to load a data frame of tweets from an
#' SQLite database generated with the script SearchAndStoreTweets.R

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

# output directory: this is where the SQLite database is
#out.dir <- "C:/Users/Sam/Dropbox/Work/AgroStream/"
out.dir <- "D:/Dropbox/Work/AgroStream/"

# path to database
path.out <- paste0(out.dir, "TweetsOut.sqlite")

# connect to database
db <- dbConnect(RSQLite::SQLite(), path.out)

# read in table
df <- dbReadTable(db, "tweets")

# when you're done, disconnect from database (this is when the data will be written)
dbDisconnect(db)

# plot of tweets by day
df$created <- ymd_hms(df$created)
df$DOY <- yday(df$created)
df.d <- summarize(group_by(df, DOY),
                  tweets = sum(is.finite(lat.location)))

# print most recent tweet
print(paste0("Last tweet: ", df$created[dim(df)[1]]))

p.bar.tweets.DOY <-
  ggplot(df.d, aes(x=DOY, y=tweets)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(panel.grid=element_blank())