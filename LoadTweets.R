## LoadTweets.R
#' This script is intended to load a data frame of tweets from an
#' SQLite database generated with the script SearchAndStoreTweets.R

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

# output directory: this is where the SQLite database is
out.dir <- "C:/Users/gsas/OneDrive - The University of Kansas/Research/Twitter/AgroStream/"
#out.dir <- "C:/Users/Sam/Dropbox/Work/Twitter/AgroStream/"
#out.dir <- "D:/Dropbox/Work/Twitter/AgroStream/"

# path to database
path.out <- paste0(out.dir, "rTweetsOut.sqlite")

# connect to database
db <- dbConnect(RSQLite::SQLite(), path.out)

# read in table
df <- dbReadTable(db, "tweets")

# trim to unique and rewrite
df <- unique(df)
dbWriteTable(db, "tweets", df, overwrite=T)

# when you're done, disconnect from database (this is when the data will be written)
dbDisconnect(db)

# plot of tweets by day
df$created_at <- ymd_hms(df$created_at)
df$DOY <- yday(df$created_at)
df$Date <- as.Date(df$created_at)
df.d <- summarize(group_by(df, Date),
                  tweets = sum(is.finite(created_at)))

# list of missing days
missing <- seq(df.d$Date[1], Sys.Date()-1, by="day")[!(seq(df.d$Date[1], Sys.Date()-1, by="day") %in% df.d$Date)]
print(missing)
#print(yday(missing))
#print(week(missing))

# print most recent tweet
print(paste0("Last tweet: ", df$created_at[which.max(df$status_id)]))

p.bar.tweets.DOY <-
  ggplot(df.d, aes(x=Date, y=tweets)) +
  geom_bar(stat="identity") +
  labs(title=paste0(sum(df.d$tweets), " tweets")) +
  theme_bw() +
  theme(panel.grid=element_blank())
p.bar.tweets.DOY
