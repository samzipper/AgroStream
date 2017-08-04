## TweetsOut_IL-02_CountyAnalysis.R
#' This script is intended to conduct a detailed analysis of Illinois tweets
#' at county resolution. Illinois was selected because it had the most tweets.
#' The output of TweetsOut_IL-01_ClassifyByCounty.R is required.

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
require(broom)
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

## load tweet database
# path to database
path.out <- paste0(git.dir, "TweetsOut.sqlite")

# connect to database
db <- dbConnect(RSQLite::SQLite(), path.out)

# read in table
df <- dbReadTable(db, "ILtweetsWithCounties")

# when you're done, disconnect from database (this is when the data will be written)
dbDisconnect(db)

# make date column
df$date <- as.Date(ymd_hms(df$created))

# read in shapefile of Illinois
IL.shp <- readOGR(dsn=paste0(git.dir, "IL_Counties"), layer="IL_BNDY_County_Py")

# get list of county names
IL.names <- as.character(IL.shp@data$COUNTY_NAM)

# count number of tweets by county
IL.df <- tidy(IL.shp, region="COUNTY_NAM")
IL.df$county <- IL.df$id

## tweets/day/county
df.c.DOY <- dplyr::summarize(group_by(df, county, DOY),
                             tweets.day = sum(is.finite(as.numeric(id))))

## tweets/county
df.c <- dplyr::summarize(group_by(df.c.DOY, county),
                         n.tweets = sum(tweets.day),
                         max.tweets.DOY = max(tweets.day))

## DOY of max tweets for each county
# must be counties with >= 5 tweets, and max.tweets.DOY > 1
max.county.list <- df.c$county[df.c$n.tweets >= 5 & df.c$max.tweets.DOY > 1]
df.max.county.DOY <- subset(df.c, n.tweets >= 5 & max.tweets.DOY>1)
df.max.county.DOY$DOY.max <- NaN
for (c in df.max.county.DOY$county){
  df.c.sub <- subset(df.c.DOY, county==c)
  DOY.max <- df.c.sub$DOY[which.max(df.c.sub$tweets.day)]
  df.max.county.DOY$DOY.max[df.max.county.DOY$county==c] <- mean(DOY.max)
}

## join county data with polygon data frame
df.map <- left_join(IL.df, df.c, by="county")
df.map <- left_join(df.map, df.max.county.DOY[,c("county", "DOY.max")], by="county")

## make map
p.IL.tweets <-
  ggplot(df.map, aes(x=long, y=lat, group=group, fill=log10(n.tweets))) +
  geom_polygon()

p.IL.DOY.max <-
  ggplot(df.map, aes(x=long, y=lat, group=group, fill=DOY.max)) +
  geom_polygon()