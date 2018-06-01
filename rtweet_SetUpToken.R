## rtweet_SetUpToken.R
#' This script is intended to set up the token necessary to access the
#' Twitter API, so it does not need to be completed each time.
#' 
#' Tutorial: http://rtweet.info/articles/auth.html

rm(list=ls())

require(rtweet)

## first: create app, get API key and secret from apps.twitter.com

## whatever name you assigned to your created app
appname <- "AgroStream"

## api key
key <- "AAA"

## api secret
secret <- "BBB"

## create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

# output directory: save to Dropbox, not git repository, so it's automatically backed up
# this is also where authentication info is stored
out.dir <- "C:/Users/Sam/Dropbox/Work/Twitter/AgroStream/"

## combine with name for token
file_name <- file.path(out.dir, "twitter_token.Rds")

## save token to home directory
saveRDS(twitter_token, file = file_name)
