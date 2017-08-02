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
require(hydroGOF)
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

# minimum activity threshold?
tweet.per.week.thres <- 2    # only analyze states averaging this many tweets per week

# max user percentage threshold? (% of tweets coming from a single user)
max.user.prc.thres <- 25

# start/end date for period of interest (20 weeks total)
date.start <- ymd("2017-02-27")  # start of week 9: 2017-02-27
date.end <- ymd("2017-07-16")    # end of week 28: 2017-07-16

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

## subset data
# subset to data that uses specific hashtags
df <- df[str_detect(df$text, "#corn17") | str_detect(df$text, "#soy17") | str_detect(df$text, "#plant17"), ]

# subset to data in crop list
df <- df[str_detect(df$text, crop.list[1]) | str_detect(df$text, crop.list[2]), ]

# subset based on time
df$date <- as.Date(ymd_hms(df$created))
df <- subset(df, date>=date.start & date <= date.end)
df$DOY <- yday(df$date)
df$week <- week(df$date-days(1))  # -1 to match with NASS week endings

## convert lat/long to states
## based on StackOverflow answer here: https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r
# read in shapefile of states
states.shp <- readOGR(paste0(git.dir, "US_States"), "US_States")

# make SpatialPoints from database
locations.pts <- SpatialPoints(data.frame(lon = df$lon.location,
                                          lat = df$lat.location))

locations.geotag <- data.frame(lon=as.numeric(df$longitude),
                               lat=as.numeric(df$latitude))

locations.geotag.pts <- SpatialPoints(locations.geotag[is.finite(locations.geotag$lon), ])

# make sure same projection
proj4string(locations.pts) <- proj4string(states.shp)
proj4string(locations.geotag.pts) <- proj4string(states.shp)

## add state name from profile location to data frame
df$state.loc <- as.character(over(locations.pts, states.shp)$name)

## add state name from geotag to data frame
df$state.geotag[is.finite(locations.geotag$lon)] <- as.character(over(locations.geotag.pts, states.shp)$name)

## add state name if mentioned in tweet
# figure out if a state is mentioned in the text
df$state.mention <- NA   # make empty column

states.all <- as.character(states.shp$name)
states.abb.all <- abb2state(states.all, convert=T)

f.state.mention <- function(x, states.all, states.abb.all){
  # search for state names and/or abbreviation in text
  state.mention <- states.all[str_detect(x, states.all)]
  state.lc.mention <- states.all[str_detect(x, str_to_lower(states.all))]
  state.caps.mention <- states.all[str_detect(x, str_to_upper(states.all))]
  state.abb.mention <- states.abb.all[str_detect(x, states.abb.all)]
  
  # if it is mentioned in multiple cases, condense to single mention per state
  state.mention <- unique(str_to_title(c(state.mention, state.lc.mention, state.caps.mention, abb2state(state.abb.mention))))
  
  if (length(state.mention)==1){
    out <- abb2state(state.mention, convert=T)
  } else {
    out <- "XX"
  }
  
  return(out)
}

df$state.mention <- sapply(df$text, f.state.mention, states.all=states.all, states.abb.all=states.abb.all)

## fill in state column with hierarchy: mention > geotag > location
df$state <- df$state.loc
df$state[which(df$state.geotag != "NA")] <- df$state.geotag[which(df$state.geotag != "NA")]
df$state[which(df$state.mention != "XX")] <- abb2state(df$state.mention[which(df$state.mention != "XX")])

# get rid of tweets without a state (out of the US)
df <- df[!is.na(df$state),]

# make state abbreviate
df$state.abb <- abb2state(df$state, convert=T)

## summarize by state
df.state.stats <- dplyr::summarize(group_by(df, state.abb),
                                   n.tweets = sum(is.finite(as.numeric(id))))
df.state.stats$tweets.per.week <- df.state.stats$n.tweets/length(unique(df$week))

## cycle through crops and states
state.list <- df.state.stats$state.abb[df.state.stats$tweets.per.week >= tweet.per.week.thres & df.state.stats$state.abb %in% unique(df.NASS$state.abb)]

for (s in state.list){
  # subset to data from this state
  df.state <- subset(df, state.abb==s)
  df.NASS.state <- subset(df.NASS, state.abb==s)
  for (crop in crop.list){
    
    # subset to data for this crop
    df.state.crop <- df.state[str_detect(df.state$text, crop), ]
    df.NASS.state.crop <- subset(df.NASS.state, str_detect(str_to_lower(Commodity), crop))
    
    # calculate number of tweets per user
    df.state.crop.tweets.user <- dplyr::summarize(group_by(df.state, screenName),
                                             tweets = sum(is.finite(date)))
    max.user.prc <- 100*max(df.state.crop.tweets.user$tweets)/sum(df.state.crop.tweets.user$tweets)
    
    # for corn:
    # IA: max user is 34/203  tweets (16.7%), mjwiegand
    # IL: max user is 10/185  tweets (5.41%), jwolf3447
    # KS: max user is 213/255 tweets (83.5%), mjwiegand
    # MN: max user is 45/135  tweets (33.3%), BRNAgNews_Mark
    # NE: max user is 255/346 tweets (73.7%), mjwiegand
    # OH: max user is 94/162  tweets (58.0%), andersonsgrain
    
    if (max.user.prc <= max.user.prc.thres){
      
      ## summarize by day
      df.d <- summarize(group_by(df.state.crop, date),
                        tweets = sum(is.finite(lat.location)))
      
#      # linearly interpolate for missing dates: 2017-04-04, 2017-04-07, 2017-04-17
#      df.d <- rbind(df.d, data.frame(date=ymd(c("2017-04-04", "2017-04-07", "2017-04-17")), tweets=NaN))
#      df.d <- df.d[order(df.d$date), ]
#      df.d$gapfill <- is.na(df.d$tweets)
#      df.d$tweets <- round(na.approx(df.d$tweets))

      # define week for comparison with NASS
      df.d$week <- week(df.d$date-days(1))  # -1 to match with NASS week endings
      
      ## summarize by week
      df.w <- summarize(group_by(df.d, week),
                        tweets = sum(tweets))
      df.w$tweets.cum <- cumsum(df.w$tweets)
      
      ## fit logistic curve to cumulative tweets
      m1.est <- max(df.w$tweets.cum)
      m2.est <- 0.5
      m3.est <- mean(df.w$week)
      
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
      
      # add a buffer on either side
      wk.buffer <- 2
      wk.start <- wk.start-wk.buffer
      wk.end <- wk.end+wk.buffer
      
      # use entire period of record
      #    wk.start <- min(df.w$week)
      #    wk.end <- max(df.w$week)
      
      # calculate progress based on wk.start and wk.end
      df.w <- subset(df.w, week>=(wk.start) & week <= (wk.end))
      df.w$tweets[1] <- 0
      df.w$tweets.cum.sub <- cumsum(df.w$tweets)
      df.w$progress.twitter <- 100*df.w$tweets.cum.sub/max(df.w$tweets.cum.sub)
      
      ## merge with NASS data
      df.w <- merge(df.w, df.NASS.state.crop, by=c("week"), all=T)
      
      ## add info and make overall data frame
      df.w$crop <- crop
      df.w$state.abb <- s
      if (exists("df.all")){
        df.all <- rbind(df.all, df.w)
      } else {
        df.all <- df.w
      }
      
    }  # end of max.user.prc.thres if statement
  }  # end of crop loop
}  # end of state loop

## make plot
df.melt <- melt(df.all[,c("week", "crop", "state.abb", "progress.twitter", "progress.NASS")],
                id=c("week", "crop", "state.abb"))

# calculate date of 10%, 50%, 90% planted
df.plant <- dplyr::summarize(group_by(df.melt[complete.cases(df.melt),], crop, state.abb, variable),
                             plant.10 = interp(x=value, y=week, x.in=10),
                             plant.25 = interp(x=value, y=week, x.in=25),
                             plant.50 = interp(x=value, y=week, x.in=50),
                             plant.75 = interp(x=value, y=week, x.in=75),
                             plant.90 = interp(x=value, y=week, x.in=90))
df.plant.melt <- melt(df.plant, id=c("crop", "state.abb", "variable"), value.name="progress", variable.name="prc")
df.plant.cast <- dcast(df.plant.melt, crop + state.abb + prc ~ variable, value.var="progress")

p.facet.tweets <-
  ggplot(df.all, aes(x=week, y=tweets)) +
  geom_hline(yintercept=0, color="gray65") +
  geom_line() +
  geom_point() +
  facet_grid(crop ~ state.abb, scales="free_y") +
  scale_x_continuous(name="Week") +
  scale_y_continuous(name="Tweets") +
  theme_SCZ()

p.facet.prc <-
  ggplot(df.melt, aes(x=week, y=value, color=variable)) +
  geom_line() +
  geom_point() +
  facet_grid(crop ~ state.abb) +
  scale_color_manual(name="Source", labels=c("progress.twitter"="Twitter", "progress.NASS"="NASS"),
                     values=c("progress.twitter"=col.blue, "progress.NASS"=col.red)) +
  scale_x_continuous(name="Week") +
  scale_y_continuous(name="% Planted") +
  theme_SCZ() +
  theme(legend.position="bottom")
ggsave(paste0(git.dir, "analysis/plots/TweetsOut_02_CompareToNASS_p.facet.prc.png"),
       p.facet.prc, width=12, height=8, units="in")

p.facet.scatter <-
  ggplot(df.all, aes(x=progress.NASS, y=progress.twitter)) +
  geom_abline(intercept=0, slope=1, color="gray65") +
  geom_point(aes(color=state.abb)) +
  stat_smooth(method="lm") +
  facet_grid(.~crop) +
  scale_x_continuous(name="% Planted, NASS", limits=c(0,100)) +
  scale_y_continuous(name="% Planted, Twitter", limits=c(0,100)) +
  scale_color_discrete(name="State") +
  theme_SCZ() +
  theme(legend.position="bottom")
ggsave(paste0(git.dir, "analysis/plots/TweetsOut_02_CompareToNASS_p.facet.scatter.png"),
       p.facet.scatter, width=12, height=8, units="in")

p.plant.facet.scatter <-
  ggplot(df.plant.cast, aes(x=progress.NASS, y=progress.twitter)) +
  geom_abline(intercept=0, slope=1, color="gray65") +
  geom_point(aes(color=state.abb)) +
  facet_grid(crop ~ prc, scales="free",
             labeller=as_labeller(c("plant.10"="10%", "plant.25"="25%", "plant.50"="50%", "plant.75"="75%", "plant.90"="90%",
                                    "corn"="Corn", "soy"="Soy"))) +
  scale_x_continuous(name="Week, NASS") +
  scale_y_continuous(name="Week, Twitter") +
  scale_color_discrete(name="State") +
  theme_SCZ() +
  theme(legend.position="bottom")
ggsave(paste0(git.dir, "analysis/plots/TweetsOut_02_CompareToNASS_p.plant.facet.scatter.png"),
       p.plant.facet.scatter, width=12, height=8, units="in")

## summarize by state and crop
# comparing progress (units are %)
df.fit <- dplyr::summarize(group_by(df.all, state.abb, crop),
                           RMSE = rmse(progress.twitter, progress.NASS),
                           NSE = NSE(progress.twitter, progress.NASS),
                           KGE = KGE(progress.twitter, progress.NASS),
                           bias.prc = pbias(progress.twitter, progress.NASS))
