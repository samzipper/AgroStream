## TweetsOut_02_CompareToNASS.R
#' This script is intended to load a data frame of tweets from an
#' SQLite database generated with the script SearchAndStoreTweets.R,
#' figure out what state each user is from, and make an eCDF for each state.

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
source(paste0(git.dir, "analysis/plot_colors.R"))

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

## load crop progress report (downloaded from USDA NASS QuickStats)
df.NASS <- read.csv(paste0(git.dir, "analysis/CropProgress_USDA-NASS.csv"), stringsAsFactors=F)

# make some useful columns
df.NASS$state.abb <- abb2state(df.NASS$State, convert=T)
df.NASS$date <- ymd(df.NASS$Week.Ending)
df.NASS$week <- week(df.NASS$date)-1  # -1 to make sure it matches with lubridate-calculated week

# trim to only useful columns
df.NASS <- df.NASS[,c("date", "state.abb", "week", "Commodity", "Value")]
colnames(df.NASS) <- c("date", "state.abb", "week", "Commodity", "progress.NASS")

## load tweet database
# path to database
path.out <- paste0(git.dir, "TweetsOut.sqlite")

# connect to database
db <- dbConnect(RSQLite::SQLite(), path.out)

# read in table
df <- dbReadTable(db, "tweets")

# when you're done, disconnect from database (this is when the data will be written)
dbDisconnect(db)

## convert lat/long to states
## based on StackOverflow answer here: https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r
# read in shapefile of states
states.shp <- readOGR(paste0(git.dir, "US_States"), "US_States")

# make SpatialPoints from database
locations.pts <- SpatialPoints(data.frame(lon = df$lon.location,
                                          lat = df$lat.location))

# make sure same projection
proj4string(locations.pts) <- proj4string(states.shp)

# add state name to data frame
df$state <- as.character(over(locations.pts, states.shp)$name)

# get rid of tweets without a state (out of the US)
df <- df[!is.na(df$state),]

# make state abbreviate
df$state.abb <- abb2state(df$state, convert=T)

# do some date calculations
df$date <- as.Date(ymd_hms(df$created))
df$DOY <- yday(df$date)
df$week <- week(df$date-days(1))  # -1 to match with NASS week endings

## cycle through crops and states
crop.list <- c("corn")         # eventually: do both corn and soy
state.list <- c("IL")          # eventually: select state list based on some tweet threshold

for (s in state.list){
  # subset to data from this state
  df.state <- subset(df, state.abb==s)
  df.NASS.state <- subset(df.NASS, state.abb==s)
  
  for (crop in crop.list){
    # subset to data for this crop
    df.state.crop <- df.state[str_detect(df.state$text, crop), ]
    df.NASS.state.crop <- subset(df.NASS.state, str_to_lower(Commodity)==crop)
    
    ## summarize by day
    df.d <- summarize(group_by(df.state.crop, date),
                      tweets = sum(is.finite(lat.location)))
    
    # linearly interpolate for missing dates: 2017-04-04, 2017-04-07, 2017-04-17
    df.d <- rbind(df.d, data.frame(date=ymd(c("2017-04-04", "2017-04-07", "2017-04-17")), tweets=NaN))
    df.d <- df.d[order(df.d$date), ]
    df.d$gapfill <- is.na(df.d$tweets)
    df.d$tweets <- round(na.approx(df.d$tweets))
    df.d$week <- week(df.d$date-days(1))  # -1 to match with NASS week endings
    
    ## summarize by week
    df.w <- summarize(group_by(df.d, week),
                      tweets = sum(tweets))
    df.w <- df.w[2:(dim(df.w)[1]-1), ]   # get rid of first and last week because not full 7 days
    
    ## define baseline weekly value
    wk.baseline <- min(df.w$tweets)  # eventually: more refined technique necessary?
    df.w$tweets.norm <- df.w$tweets - wk.baseline
    df.w$tweets.cum <- cumsum(df.w$tweets)
    df.w$progress.twitter <- 100*df.w$tweets.cum/max(df.w$tweets.cum)
    
    ## fit logistic curve to cumulative tweets
    m1.est <- max(df.w$tweets.cum)
    m2.est <- 0.5
    m3.est <- mean(df.w$week)
    
    #df.test <- data.frame(week=seq(min(df.w$week),max(df.w$week)))
    #df.test$tweets.test <- m1.est/(1+exp(-m2.est*(df.test$week-m3.est)))
    #qplot(x=week, y=tweets.test, data=df.test)
    
    log.fit <- nls(tweets.cum ~ (m1.est/(1+exp(-m2*(week-m3)))),
                   data = df.w,
                   start = list(#m1 = m1.est, 
                                m2 = m2.est, 
                                m3 = m3.est),
                   trace=T)
    
    # prediction using fitted function
    df.w$tweets.fit <- predict(log.fit, list(week=df.w$week))
    m2 <- coef(log.fit)[1]
    m3 <- coef(log.fit)[2]
    
    # calculate second derivate of fit, from wolfram alpha second derivative calculator: y = (m1/(1+exp(-m2*(x-m3))))
    df.w$fit.deriv2 <- (m1.est*(m2^2)*exp(m2*(m3+df.w$week))*(exp(m2*m3)-exp(m2*df.w$week)))/
      ((exp(m2*m3)+exp(m2*df.w$week))^3)
    
    # calculate start and end week as max/min of second derivative
    wk.start <- df.w$week[which.max(df.w$fit.deriv2)]
    wk.end <- df.w$week[which.min(df.w$fit.deriv2)]
    
    # calculate progress based on wk.start and wk.end
    df.w <- subset(df.w, week>=(wk.start) & week <= (wk.end))
    df.w$tweets.cum.sub <- cumsum(df.w$tweets)
    df.w$progress.twitter <- 100*df.w$tweets.cum.sub/max(df.w$tweets.cum.sub)
    
    ## merge with NASS data
    df.w <- merge(df.w, df.NASS.state.crop, by=c("week"), all=T)
    df.w.melt <- melt(df.w[,c("week", "tweets", "tweets.norm", "tweets.cum", "progress.twitter", "progress.NASS")], 
                      id=c("week", "tweets", "tweets.norm", "tweets.cum"))
    
    ## diagnostic plots
    p.bar.d <- 
      ggplot(df.d, aes(x=date, y=tweets, fill=gapfill)) +
      geom_bar(stat="identity")
    
    p.bar.w <- 
      ggplot(df.w[2:(dim(df.w)[1]-1), ], aes(x=week, y=tweets)) +
      geom_bar(stat="identity")
    
    p.tweets.w <- 
      ggplot(df.w, aes(x=week, y=tweets.cum)) +
      geom_line() + geom_point()
    
    p.tweets.fit.w <- 
      ggplot(df.w, aes(x=week, y=tweets.cum)) +
      geom_point() + 
      geom_line(data=df.w, aes(x=week, y=tweets.fit))
    
    p.tweets.prc.w <- 
      ggplot(df.w, aes(x=week, y=progress.twitter)) +
      geom_line() + geom_point()
    
    p.NASS.w <-
      ggplot(df.w, aes(x=week, y=progress.NASS)) +
      geom_line() + geom_point()
    
    p.comp.w <-
      ggplot(df.w.melt, aes(x=week, y=value, color=variable)) +
      geom_line() + 
      geom_point() +
      scale_color_manual(name="Source", labels=c("progress.twitter"="Twitter", "progress.NASS"="NASS"), 
                         values=c("progress.twitter"=col.blue, "progress.NASS"=col.red)) +
      scale_x_continuous(name="Week") +
      scale_y_continuous(name="Percent Planted") +
      ggtitle(paste0(s, " ", crop)) +
      theme_SCZ()
    
  }
}