## TweetsOut_03_Sentiment.R
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
require(wordcloud)
require(tidytext)
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

# count occurrences of each word
df.count <- dplyr::count(df.token, word, sort=T)

## add sentiment data
# using AFINN dataset which assigns each word a score from -5 to +5 (for negative to positive sentiment)
df.sentiments <- get_sentiments("afinn")

# add to token data frame
df.token <- left_join(df.token, df.sentiments, by=c("word"))

# average sentiment by state
df.state.sentiment <-
  dplyr::summarize(group_by(df.token, state, state.abb),
                   sentiment.mean = mean(score, na.rm=T),
                   sentiment.n = sum(is.finite(score)))

# get rid of states with < 10 sentiment words
df.state.sentiment <- subset(df.state.sentiment, sentiment.n >= 10)

# which state has the most sentiment words?
state.sentiment.most <- df.state.sentiment$state.abb[which.max(df.state.sentiment$sentiment.n)]

# for state with most sentiment words, sentiment through time
df.most.time <- dplyr::summarize(group_by(subset(df.token, state.abb==state.sentiment.most), DOY),
                                 sentiment.mean = mean(score, na.rm=T),
                                 sentiment.n = sum(is.finite(score)))

# fill in missing days
missing.DOY <- seq(min(df.most.time$DOY), max(df.most.time$DOY))[which(!(seq(min(df.most.time$DOY), max(df.most.time$DOY)) %in% df.most.time$DOY))]
df.most.time <- rbind(df.most.time, data.frame(DOY=missing.DOY, sentiment.mean=NaN))

# for all states, sentiment through time
df.sentiment.time <- dplyr::summarize(group_by(df.token, DOY),
                                 sentiment.mean = mean(score, na.rm=T),
                                 sentiment.n = sum(is.finite(score)))

# wet-to-dry ratio by state
df.state.wet.dry <- dplyr::summarize(group_by(df.token, state, state.abb),
                                     n.wet = sum(word=="wet"),
                                     n.dry = sum(word=="dry"),
                                     wet.dry = n.wet/n.dry)
df.state.wet.dry <- subset(df.state.wet.dry, (n.wet+n.dry)>=5)

# overall dry wet mentions per tweet
df.DOY.wet.dry <- dplyr::summarize(group_by(df.token, DOY),
                               wet = sum(word=="wet")/sum(is.finite(DOY)),
                               dry = sum(word=="dry")/sum(is.finite(DOY)))
df.DOY.wet.dry.melt <- melt(df.DOY.wet.dry, id=c("DOY"))

# mentions of no tille
df$no.till <- str_detect(df$text, "no till")|str_detect(df$text, "no-till")|str_detect(df$text, "notill")

# mentions of replant
df$replant <- str_detect(str_to_lower(df$text), "replant")

# mentions of rain or wet
df$rain.wet <- str_detect(str_to_lower(df$text), "rain")|str_detect(str_to_lower(df$text), "wet")
df$wet <- str_detect(str_to_lower(df$text), "wet")

# sum by state replant and rain
df.state.replant <- dplyr::summarize(group_by(df, state.abb),
                                     replant = sum(replant),
                                     rain.wet = sum(rain.wet))

# sum replant and rain for IL
df.IL.replant <- dplyr::summarize(group_by(subset(df, state.abb=="IL"), DOY),
                                  n.tweets = sum(is.finite(DOY)),
                                  n.rain = sum(rain.wet),
                                  n.replant = sum(replant))
df.IL.replant$rain.tweets <- df.IL.replant$n.rain/df.IL.replant$n.tweets
df.IL.replant$replant.tweets <- df.IL.replant$n.replant/df.IL.replant$n.tweets

# sum replant and rain for all DOY, all states
df.DOY.replant <- dplyr::summarize(group_by(df, DOY),
                                   n.tweets = sum(is.finite(DOY)),
                                   n.rain = sum(rain.wet),
                                   n.replant = sum(replant))
df.DOY.replant$rain.tweets <- df.DOY.replant$n.rain/df.DOY.replant$n.tweets
df.DOY.replant$replant.tweets <- df.DOY.replant$n.replant/df.DOY.replant$n.tweets

# daily mentions of replant and total negative words
df.token$replant <- str_detect(df.token$word, "replant")
df.replant.sentiment <- dplyr::summarize(group_by(df.token, DOY),
                                         n.replant = sum(replant),
                                         n.score.neg = sum(score<0, na.rm=T),
                                         n.score.tot = sum(is.finite(score)),
                                         mean.score = mean(score, na.rm=T))

## make map
# prepare polygon for ggplot
df.state.sentiment$region <- str_to_lower(df.state.sentiment$state)
df.state.wet.dry$region <- str_to_lower(df.state.wet.dry$state)
data.maps <- map_data("state")
df.map <- left_join(data.maps, df.state.sentiment, by="region")
df.map <- left_join(data.maps, df.state.wet.dry, by="region")
df.map <- df.map[order(df.map$order),]

# map of mean sentiment
p.map.sentiment <-
  ggplot(df.map, aes(x=long, y=lat, group=group, fill=sentiment.mean)) +
  geom_polygon() +
  scale_fill_viridis(name="log(Tweets)") +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  coord_map() +
  theme_bw() +
  theme(panel.grid=element_blank(),
        panel.border=element_blank(),
        legend.position="bottom")

# map of wet/dry ratio
p.map.wet.dry <-
  ggplot(df.map, aes(x=long, y=lat, group=group, fill=wet.dry)) +
  geom_polygon() +
  scale_fill_gradient2(name="wet/dry", midpoint=1) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  coord_map() +
  theme_bw() +
  theme(panel.grid=element_blank(),
        panel.border=element_blank(),
        legend.position="bottom")

# wet/dry ratio through time
p.time.wet.dry <-
  ggplot(subset(df.DOY.wet.dry.melt, value!=0), aes(x=DOY, y=value, color=variable)) +
  geom_point() +
  stat_smooth(method="loess") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        panel.border=element_blank(),
        legend.position="bottom")

# timeseries for state with most sentiment words
p.sentiment.most.time <-
  ggplot(df.most.time, aes(x=DOY, y=sentiment.mean)) +
  geom_hline(yintercept=0, color="gray65") +
  geom_point() +
  stat_smooth(method="loess") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        panel.border=element_blank())

# overall sentiment timeseries of mean
p.sentiment.time <-
  ggplot(df.sentiment.time, aes(x=DOY, y=sentiment.mean, color=sentiment.n, weight=sentiment.n)) +
  geom_hline(yintercept=0, color="gray65") +
  geom_point() +
  stat_smooth(method="loess") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        panel.border=element_blank())

# IL replant/rain
p.IL.replant.norm <-
  ggplot(melt(df.IL.replant, id=c("DOY", "n.tweets", "n.rain", "n.replant")), aes(x=DOY, y=value, color=variable)) +
  geom_line()

p.IL.replant <-
  ggplot(melt(df.IL.replant, id=c("DOY", "n.tweets", "rain.tweets", "replant.tweets")), aes(x=DOY, y=value, color=variable)) +
  geom_line()

# all states replant/rain
p.DOY.replant.norm <- 
  ggplot(melt(df.DOY.replant, id=c("DOY", "n.tweets", "n.rain", "n.replant")), aes(x=DOY, y=value, color=variable)) +
  geom_line()

p.DOY.replant <-
  ggplot(melt(df.DOY.replant, id=c("DOY", "n.tweets", "rain.tweets", "replant.tweets")), aes(x=DOY, y=value, color=variable)) +
  geom_line()

# replant with sentiment
p.replant.sentiment <-
  ggplot(melt(df.replant.sentiment, id=c("DOY", "n.score.neg", "n.score.tot")), aes(x=DOY, y=value, color=variable)) +
  geom_line()

# wordcloud
p.wordcloud <- 
  wordcloud(df.count$word, df.count$n, max.words=100)