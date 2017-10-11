## Figure_Context_Sentiment.R
#' This script is intended to analyze the sentiment of tweets.
#' It requires output from TweetsOut_01_ClassifyBystate.R
#' 
#' Good tutorial: http://tidytextmining.com/tidytext.html

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
require(tidytext)
source(paste0(git.dir, "analysis/plots/plot_colors.R"))
source(paste0(git.dir, "analysis/interp.R"))

# plot directory
plot.dir <- paste0(git.dir, "analysis/Figures+Tables/")

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
# which crops to analyze?
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

# get rid of useless columns
df <- subset(df, select=c("screenName", "text", "id", "date", "DOY", "week", "state", "state.abb"))

## transform into long-form data frame
df.token <- unnest_tokens(df, word, text)

## add sentiment data
# using AFINN dataset which assigns each word a score from -5 to +5 (for negative to positive sentiment)
df.sentiments <- get_sentiments("afinn")

# add to token data frame
df.token <- left_join(df.token, df.sentiments, by=c("word"))

# calculate number of sentiment words in each tweet
df.tweet.sentiment <- dplyr::summarize(group_by(df.token, id, DOY),
                                       sentiment = mean(score, na.rm=T),
                                       n.sentiment = sum(is.finite(score)))
df.tweet.sentiment <- subset(df.tweet.sentiment, is.finite(sentiment))
df.tweet.sentiment.time <- dplyr::summarize(group_by(df.tweet.sentiment, DOY),
                                            sentiment.mean = mean(sentiment, na.rm=T),
                                            sentiment.n = sum(is.finite(DOY)))

# for all states, sentiment through time
df.sentiment.time <- dplyr::summarize(group_by(df.token, DOY),
                                      sentiment.mean = mean(score, na.rm=T),
                                      sentiment.n = sum(is.finite(score), na.rm=T))

## make plot
# overall sentiment timeseries of mean
# old version: uses average of all sentiment words for a given DOY
# p.sentiment.time <-
#   ggplot(df.sentiment.time, aes(x=DOY, y=sentiment.mean, size=sentiment.n, weight=sentiment.n)) +
#   geom_hline(yintercept=0, color="gray65") +
#   geom_point(alpha=0.5) +
#   stat_smooth(method="loess", show.legend=F) +
#   scale_y_continuous(limits=c(-max(df.sentiment.time$sentiment.mean, na.rm=T), max(df.sentiment.time$sentiment.mean, na.rm=T))) +
#   theme_SCZ()

# new version: first averages sentiment words by tweets, then averages mean tweet score by DOY
p.sentiment.time <-
  ggplot(df.tweet.sentiment.time, aes(x=DOY, y=sentiment.mean, size=sentiment.n, weight=sentiment.n)) +
  geom_hline(yintercept=0, color="gray65") +
  geom_point(alpha=0.5) +
  stat_smooth(method="loess", show.legend=F) +
  scale_y_continuous(limits=c(-max(df.tweet.sentiment.time$sentiment.mean, na.rm=T), max(df.tweet.sentiment.time$sentiment.mean, na.rm=T))) +
  theme_SCZ()

pdf(paste0(plot.dir, "Figure_Context_Sentiment_NoText.pdf"), width=(77/25.4), height=(77/25.4))
p.sentiment.time + theme(text=element_blank(), plot.margin=unit(c(0.5,0.5,0,0), "mm"),
                         legend.position=c(0,0), legend.justification=c(0,0), 
                         legend.direction="horizontal", legend.background=element_blank())
dev.off()

sum(df.tweet.sentiment$n.sentiment)
table(df.tweet.sentiment$n.sentiment)
