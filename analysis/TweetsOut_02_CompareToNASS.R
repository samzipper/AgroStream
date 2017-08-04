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
# make plots?
make.plots <- T  # logical

# which crops to analyze?
crop.list <- c("corn", "soy")

# minimum activity threshold?
tweets.per.week.thres <- 1    # only analyze states/crop combos averaging this many tweets per week

# max user percentage threshold? (% of tweets coming from a single user)
max.user.prc.thres <- 25

# start/end date for period of interest (20 weeks total)
date.start <- ymd("2017-02-27")  # start of week 9: 2017-02-27
date.end <- ymd("2017-07-16")    # end of week 28: 2017-07-16
dates.all <- seq(date.start, date.end, by="day")

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

## summarize by crop
for (crop in crop.list){
  df.crop <- df[str_detect(df$text, crop), ]
  df.crop$crop <- crop
  
  if (exists("df.crop.all")){
    df.crop.all <- rbind(df.crop.all, df.crop)
  } else {
    df.crop.all <- df.crop
  }
}

# stats: how many corn, soy, and both
length(unique(df.crop.all$state.abb))
dim(subset(df.crop.all, crop=="corn"))[1]
dim(subset(df.crop.all, crop=="soy"))[1]
length(intersect(subset(df.crop.all, crop=="corn")$id, subset(df.crop.all, crop=="soy")$id))

## determine list of states for analysis
# summarize by state, crop, and user
df.state.crop.user <- dplyr::summarize(group_by(df.crop.all, crop, state.abb, screenName),
                                       tweets = sum(is.finite(as.numeric(id))))

# summarize by state and crop
df.state.stats <- dplyr::summarize(group_by(df.state.crop.user, crop, state.abb),
                                   n.tweets = sum(tweets),
                                   max.user.tweets = max(tweets))

# calculate mean tweets per week
df.state.stats$tweets.per.week <- df.state.stats$n.tweets/length(unique(df$week))

# calculate proportion of tweets represented by max user
df.state.stats$max.user.prc <- 100*df.state.stats$max.user.tweets/df.state.stats$n.tweets

# subset to only states meeting criteria:
#  NASS data available
#  sufficient rate of tweets (tweets.per.week >= tweets.per.week.thres)
#  no single user is disproportionaly tweeting (max.user.prc <= max.user.prc.thres)
df.state.crop.all <- subset(df.state.stats, state.abb %in% unique(df.NASS$state.abb) & 
                           tweets.per.week >= tweets.per.week.thres & 
                           max.user.prc <= max.user.prc.thres)

# get rid of texas, because it planted before AgroStream started collecting data
df.state.crop.all <- subset(df.state.crop.all, state.abb != "TX")

## cycle through crops and states
for (i in 1:dim(df.state.crop.all)[1]){
  
  # get crop and state
  c <- df.state.crop.all$crop[i]
  s <- df.state.crop.all$state.abb[i]
  
  # subset to data for this state and crop
  df.state.crop <- subset(df.crop.all, state.abb==s & crop==c)
  df.NASS.state.crop <- subset(df.NASS, state.abb==s & str_detect(str_to_lower(Commodity), c))
  
  
  ## summarize by day
  df.d <- summarize(group_by(df.state.crop, date),
                    tweets = sum(is.finite(lat.location)))
  
  # fill in with all other dates, with tweets set to 0
  df.d <- rbind(df.d, data.frame(date = dates.all[!(dates.all %in% df.d$date)],
                                 tweets = 0))
  df.d <- df.d[order(df.d$date), ]
  
  # define week for comparison with NASS
  df.d$week <- week(df.d$date-days(1))  # -1 to match with NASS week endings
  
  ## summarize by week
  df.w <- summarize(group_by(df.d, week),
                    tweets = sum(tweets))
  df.w$tweets.cum <- cumsum(df.w$tweets)
  
  ## merge with NASS data
  df.w <- merge(df.w, df.NASS.state.crop, by=c("week"), all=T)
  
  ## fit logistic curve to cumulative tweets
  tweets.tot <- max(df.w$tweets.cum)
  df.w$tweets.tot <- tweets.tot
  m2.est <- 0.5
  m3.est <- mean(df.w$week)
  
  log.fit <- nls(tweets.cum ~ (tweets.tot/(1+exp(-m2*(week-m3)))),
                 data = df.w,
                 start = list(m2 = m2.est, m3 = m3.est),
                 trace=T)
  
  # prediction using fitted function
  df.w$tweets.fit <- predict(log.fit, list(week=df.w$week))
  m2 <- coef(log.fit)[1]
  m3 <- coef(log.fit)[2]
  
  # calculate second derivate of fit, from wolfram alpha second derivative calculator: y = (m1/(1+exp(-m2*(x-m3))))
  df.w$fit.deriv2 <- (tweets.tot*(m2^2)*exp(m2*(m3+df.w$week))*(exp(m2*m3)-exp(m2*df.w$week)))/
    ((exp(m2*m3)+exp(m2*df.w$week))^3)
  
  for (wk.buffer.start in seq(0,5)){
    for (wk.buffer.end in seq(0,5)){
      
      # calculate start and end week as max/min of second derivative
      wk.start <- df.w$week[which.max(df.w$fit.deriv2)]
      wk.end <- df.w$week[which.min(df.w$fit.deriv2)]
      
      # add a buffer on either side
      wk.start <- wk.start-wk.buffer.start
      wk.end <- wk.end+wk.buffer.end
      
      ## simple: calculate progress based on cumulative through time
      # calculate progress based on wk.start and wk.end
      df.w.buffer <- subset(df.w, week>=(wk.start) & week <= (wk.end))
      df.w.buffer$tweets[1] <- 0
      df.w.buffer$tweets.cum.sub <- cumsum(df.w.buffer$tweets)
      df.w.buffer$progress.twitter <- 100*df.w.buffer$tweets.cum.sub/max(df.w.buffer$tweets.cum.sub)
      
      # ## more complex: calculate progress based on another logistic fit
      # # fit logistic curve, now based on the subset of data using this buffer
      # tweets.tot.buffer <- max(df.w.buffer$tweets.cum.sub)
      # m2.est.buffer <- 0.5
      # m3.est.buffer <- mean(df.w.buffer$week)
      # 
      # log.fit.buffer <- nls(tweets.cum.sub ~ (tweets.tot.buffer/(1+exp(-m2*(week-m3)))),
      #                data = df.w.buffer,
      #                start = list(m2 = m2.est.buffer, m3 = m3.est.buffer),
      #                trace=T)
      # 
      # # prediction using fitted function
      # df.w.buffer$tweets.fit <- predict(log.fit.buffer, list(week=df.w.buffer$week))
      # 
      # # estimate progress based on fit
      # df.w.buffer$progress.fit <- 100*df.w.buffer$tweets.fit/tweets.tot.buffer
      
      ## add info and make overall data frame
      df.w.buffer$crop <- c
      df.w.buffer$state.abb <- s
      df.w.buffer$wk.buffer.start <- wk.buffer.start
      df.w.buffer$wk.buffer.end <- wk.buffer.end
      if (exists("df.all")){
        df.all <- rbind(df.all, df.w.buffer)
      } else {
        df.all <- df.w.buffer
      }
    }
  }
}  # end of state/crop loop

## save output
write.csv(df.all, paste0(git.dir, "analysis/TweetsOut_02_CompareToNASS_df.all.csv"), row.names=F)

if (make.plots){
  
  ## summarize by state and crop
  # comparing progress between NASS and cumsum approaches (units are %)
  df.fit <- dplyr::summarize(group_by(df.all, wk.buffer.start, wk.buffer.end, state.abb, crop),
                             RMSE = rmse(progress.twitter, progress.NASS),
                             KGE = KGE(progress.twitter, progress.NASS),
                             bias.prc = pbias(progress.twitter, progress.NASS),
                             tweets.tot = mean(tweets.tot))
  
  # summarize fit by buffer period and crop
  df.fit.buffer <- dplyr::summarize(group_by(df.fit, wk.buffer.start, wk.buffer.end, crop),
                                    RMSE.mean = mean(RMSE),
                                    KGE.min = min(KGE),
                                    KGE.mean = mean(KGE),
                                    KGE.max = max(KGE),
                                    bias.min = min(bias.prc),
                                    bias.mean = mean(bias.prc),
                                    bias.max = max(bias.prc))
  
  # sum of the absolute value of biases
  df.fit.buffer$bias.sum <- abs(df.fit.buffer$bias.min) + abs(df.fit.buffer$bias.mean) + abs(df.fit.buffer$bias.max)
  
  # select best buffers
  i.best.corn <- which(df.fit.buffer$RMSE.mean==min(subset(df.fit.buffer, crop=="corn")$RMSE.mean))
  wk.buffer.start.best.corn <- df.fit.buffer$wk.buffer.start[i.best.corn]
  wk.buffer.end.best.corn <- df.fit.buffer$wk.buffer.end[i.best.corn]
  
  i.best.soy <- which(df.fit.buffer$RMSE.mean==min(subset(df.fit.buffer, crop=="soy")$RMSE.mean))
  wk.buffer.start.best.soy <- df.fit.buffer$wk.buffer.start[i.best.soy]
  wk.buffer.end.best.soy <- df.fit.buffer$wk.buffer.end[i.best.soy]
  
  # subset to best
  df.all <- subset(df.all, (crop=="corn" & wk.buffer.start==wk.buffer.start.best.corn & wk.buffer.end==wk.buffer.end.best.corn) | 
                     (crop=="soy" & wk.buffer.start==wk.buffer.start.best.soy & wk.buffer.end==wk.buffer.end.best.soy))
  
  df.fit.best <- subset(df.fit, (crop=="corn" & wk.buffer.start==wk.buffer.start.best.corn & wk.buffer.end==wk.buffer.end.best.corn) | 
                          (crop=="soy" & wk.buffer.start==wk.buffer.start.best.soy & wk.buffer.end==wk.buffer.end.best.soy))
  
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
  
  p.RMSE.tweets.scatter <-
    ggplot(df.fit.best, aes(x=tweets.tot, y=RMSE)) +
    geom_point(aes(color=state.abb, shape=crop)) +
    stat_smooth(method="lm") +
    scale_color_discrete(name="State") +
    theme_SCZ() +
    theme(legend.position="bottom")
  
  
  # report mean RMSE and prc.bias
  mean(df.fit.best$RMSE)
  mean(df.fit.best$bias.prc)
  
}
