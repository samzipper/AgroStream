## Figure_Context_Wordcloud.R
#' This script is intended to make a wordcloud of all the words in all the tweets.

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

# get all text in lower case
text.all <- str_to_lower(df$text)

# get rid of hashtags so it's just words
text.words <- gsub("#", " ", text.all)

# paste into one big thing
text.words <- paste(text.words, collapse=" ")

# remove weird symbols
remove.symbols <- c("â", "í")
for (s in remove.symbols){
  text.words <- gsub(s, " ", text.words)
}

# get rid of some common/boring words as well as search words and derivatives
remove.words <- c("plant17", "planting", "plant", "plants", "planted", "planter", "corn", "corn17", 
                  "soy", "soy17", "soybean", "soybeans", "beans", "bean", "wheat", "wheat17", 
                  "of", "on", "the", "amp", "may", "if", "in", "like", "still", "see", "get", "g/e",
                  "day", "days", "can", "will", "just", "from", "to", "co", "and", "for", "at", "by", 
                  "a", "i", "is", "s", "it", "it's", "be", "we", "you", "or", "so", "my", "has", "was",
                  "us", "that", "not", "as", "than", "with", "but", "this", "are", "how", "this", "no", 
                  "w", "into")
for (b in remove.words){
  text.words <- gsub(paste0('\\<', b, '\\>'), " ", text.words)
}

# make no till and cover crop into one word
text.words <- gsub("no till", "no-till", text.words)
text.words <- gsub("strip till", "strip-till", text.words)
text.words <- gsub("cover crop", "cover-crop", text.words)

# save as a text file so that you can plug it into a wordcloud thing
write.table(text.words, paste0(plot.dir, "Figure_Context_Wordcloud.txt"), quote=F, sep=" ", col.names=F)

# get rid of useless columns
df <- subset(df, select=c("screenName", "text", "id", "date", "DOY", "week", "state", "state.abb"))

## transform into long-form data frame
df.token <- unnest_tokens(df, word, text)

# count occurrences of each word
df.count <- dplyr::count(df.token, word, sort=T)

# get rid of numbers
df.count <- df.count[is.na(as.numeric(df.count$word)), ]

# remove weird symbols
remove.symbols <- c("â", "í")
for (s in remove.symbols){
  df.count <- subset(df.count, word != s)
}

# get rid of some common/boring words as well as search words and derivatives
remove.words <- c("plant17", "planting", "plant", "plants", "planted", "planter", "corn", "corn17", 
                  "soy", "soy17", "soybean", "soybeans", "beans", "bean", "wheat", "wheat17", 
                  "of", "on", "the", "amp", "may", "if", "in", "like", "still", "see", "get", "g/e",
                  "day", "days", "can", "will", "just", "from", "to", "co", "and", "for", "at", "by", 
                  "a", "i", "is", "s", "it", "it's", "be", "we", "you", "or", "so", "my", "has", "was",
                  "us", "that", "not", "as", "than", "with", "but", "this", "are", "how", "this", "no", 
                  "w", "into")
for (b in remove.words){
  df.count <- subset(df.count, word != b)
}

# get rid of anything with plant in it
df.count <- subset(df.count, !str_detect(word, "plant17"))

# wordcloud
wordcloud(df.count$word, df.count$n, max.words=100, rot.per=0.5)

df.count$word[1:100]
