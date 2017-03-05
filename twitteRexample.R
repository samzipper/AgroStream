## twitteRexample.R
#' This example is intended to download some Tweets using the twitteR package.
#' 
#' Tutorial on setting up twitteR is here: http://bigcomputing.blogspot.ca/2016/02/the-twitter-r-package-by-jeff-gentry-is.html
#' Tutorial on extracting geocode info is here: https://www.r-bloggers.com/mapping-twitter-followers-in-r/

rm(list=ls())

# path to git directory
git.dir <- "C:/Users/Sam/WorkGits/AgroStream/"

# load packages
require(twitteR)
require(lubridate)
require(ggmap)

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

## some examples
# get tweets with #drought hashtag
tweets <- searchTwitter("#drought", n=100)

# get tweets within the contiguous US (in twitteR, searches
# are defined using a center and radius of a circle)
# the US domain is defined by: geocode='39.833333,-98.583333,1500mi'
# note that there are no spaces allowed in defining the geocode
tweets <- searchTwitter("#drought", n=100, 
                        geocode='39.833333,-98.583333,1500mi',
                        resultType="recent")

# get only tweets from yesterday
date.today <- as.Date(Sys.time())      # today's date
date.yesterday <- date.today-days(1)   # yesterday's date

tweets <- searchTwitter("#drought", n=100, 
                        geocode='39.833333,-98.583333,1500mi',
                        resultType="recent",
                        since=as.character(date.yesterday),
                        until=as.character(date.today))

# get rid of retweets
strip_retweets(tweets, strip_manual=T, strip_mt=T)

# put into data frame
df <- twListToDF(tweets)

# trim to only categories we care about
df <- df[,c("text", "created", "id", "screenName", "isRetweet", "longitude", "latitude")]

## extract user location info using google maps API
# read in google API authorization (obtain from 
# console.developers.google.com --> Credentials)
auth.g <- read.table(path.auth.g, stringsAsFactors=F)[,1]

# get location of users
user <- getUser(df$screenName[1])    # select a user screen name
location(user)                       # extract user info from their profile
df$location <- unlist(lapply(X=df$screenName, FUN=function(x) location(getUser(x))))   # do all users in list simultaneously


## working with databases using RSQLite (load it with DBI)
require(DBI)

# path to database
path.db <- paste0(git.dir, "ExampleDB.sqlite")

# create database
db <- dbConnect(RSQLite::SQLite(), path.db)

# add data frame to database
dbWriteTable(db, "tweets", df)

# list tables in database
dbListTables(db)

# search/extract from database
dbGetQuery(db, 'SELECT * FROM tweets WHERE "retweetCount" > 0 & "isRetweet"==0')   # put column names in quotes

# when you're done, disconnect from database (this is when the data will be written)
dbDisconnect(db)
#unlink(path.db)  # unlinking will delete the database

## automatically put tweets into db
# connect to database
register_sqlite_backend(path.db)

# get tweets
tweets <- searchTwitter("#drought", n=5000)

# put into database
store_tweets_db(tweets)

# load tweets from database
from_db <- load_tweets_db()
