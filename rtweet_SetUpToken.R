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

## path of home directory
home_directory <- path.expand("~/")

## combine with name for token
file_name <- file.path(home_directory, "twitter_token.Rds")

## save token to home directory
saveRDS(twitter_token, file = file_name)

## On my mac, the .Renviron text looks like this:
##     TWITTER_PAT=/Users/mwk/twitter_token.rds

## assuming you followed the procodures to create "file_name"
##     from the previous code chunk, then the code below should
##     create and save your environment variable.
cat(paste0("TWITTER_PAT=", file_name),
    file = file.path(home_directory, ".Renviron"),
    append = TRUE)
