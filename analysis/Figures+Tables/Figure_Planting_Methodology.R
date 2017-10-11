## Figure_Planting_Methodology.R
#' This script is intended to make a figure which demonstrates the methodology for 
#' estimating planting progress.
#' 
#' Output from TweetsOut_01_ClassifyByState.R is required.

rm(list=ls())

# path to git directory
git.dir <- "C:/Users/Sam/WorkGits/AgroStream/"

# load packages
require(twitteR)
require(lubridate)
require(ggmap)
require(gridExtra)
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

# which crop and state to analyze?
c <- "corn"
s <- "IL"

# start/end date for period of interest (20 weeks total)
date.start <- ymd("2017-02-27")  # start of week 9: 2017-02-27
date.end <- ymd("2017-07-16")    # end of week 28: 2017-07-16
dates.all <- seq(date.start, date.end, by="day")

# buffer weeks on front & end? (can be determined from script Figure_Planting_Comparison.R)
wk.buffer.start <- 0
wk.buffer.end <- 4

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
df <- df[str_detect(str_to_lower(df$text), c), ]

## summarize by crop
for (crop in c){
  df.crop <- df[str_detect(df$text, crop), ]
  df.crop$crop <- crop
  
  if (exists("df.crop.all")){
    df.crop.all <- rbind(df.crop.all, df.crop)
  } else {
    df.crop.all <- df.crop
  }
}

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

## calculate first/second differences
df.w$diff.first <- NaN
df.w$diff.second <- NaN
df.w$diff.first[2:dim(df.w)[1]] <- diff(df.w$tweets.cum, differences=1)
df.w$diff.second[3:dim(df.w)[1]] <- diff(df.w$tweets.cum, differences=2)

## spline smoothing
fit.spline <- smooth.spline(df.w$week, df.w$tweets.cum, spar=0.5)
df.w$fit.spline <- predict(fit.spline, x=df.w$week)$y
df.w$fit.spline.deriv1 <- predict(fit.spline, x=df.w$week, deriv=1)$y
df.w$fit.spline.deriv2 <- predict(fit.spline, x=df.w$week, deriv=2)$y

# calculate start and end week as max/min of second derivative
wk.start <- df.w$week[which.max(df.w$fit.deriv2)]
wk.end <- df.w$week[which.min(df.w$fit.deriv2)]

wk.start.spline <- df.w$week[which.max(df.w$fit.spline.deriv2)]
wk.end.spline <- df.w$week[which.min(df.w$fit.spline.deriv2)]

# add a buffer on either side
wk.start <- wk.start-wk.buffer.start
wk.end <- wk.end+wk.buffer.end

wk.start.spline <- wk.start.spline-wk.buffer.start
wk.end.spline <- wk.end.spline+wk.buffer.end

## simple: calculate progress based on cumulative through time
# calculate progress based on wk.start and wk.end
df.w.buffer <- subset(df.w, week>=(wk.start) & week <= (wk.end))
df.w.buffer$tweets[1] <- 0
df.w.buffer$tweets.cum.sub <- cumsum(df.w.buffer$tweets)
df.w.buffer$progress.twitter <- 100*df.w.buffer$tweets.cum.sub/max(df.w.buffer$tweets.cum.sub)

df.w.buffer.spline <- subset(df.w, week>=(wk.start.spline) & week <= (wk.end.spline))
df.w.buffer.spline$tweets[1] <- 0
df.w.buffer.spline$tweets.cum.sub <- cumsum(df.w.buffer.spline$tweets)
df.w.buffer.spline$progress.twitter <- 100*df.w.buffer.spline$tweets.cum.sub/max(df.w.buffer.spline$tweets.cum.sub)

## find weeks before/after planting with NASS data; set to 0% and 100%
df.w.NASS <- subset(df.w, is.finite(progress.NASS))

if (sum(!(df.w.NASS$week %in% df.w.buffer$week))>0){
  df.w.NASS.log <- subset(df.w.NASS, !(week %in% df.w.buffer$week))
  df.w.NASS.log$tweets <- NaN
  df.w.NASS.log$tweets.cum.sub <- NaN
  df.w.NASS.log$progress.twitter <- NaN
  df.w.NASS.log$progress.twitter[df.w.NASS.log$week<min(df.w.buffer$week)] <- 0
  df.w.NASS.log$progress.twitter[df.w.NASS.log$week>max(df.w.buffer$week)] <- 100
  df.w.buffer <- rbind(df.w.buffer, df.w.NASS.log)
}

if (sum(!(df.w.NASS$week %in% df.w.buffer.spline$week))>0){
  df.w.NASS.spline <- subset(df.w.NASS, !(week %in% df.w.buffer.spline$week))
  df.w.NASS.spline$tweets <- NaN
  df.w.NASS.spline$tweets.cum.sub <- NaN
  df.w.NASS.spline$progress.twitter <- NaN
  df.w.NASS.spline$progress.twitter[df.w.NASS.spline$week<min(df.w.buffer.spline$week)] <- 0
  df.w.NASS.spline$progress.twitter[df.w.NASS.spline$week>max(df.w.buffer.spline$week)] <- 100
  df.w.buffer.spline <- rbind(df.w.buffer.spline, df.w.NASS.spline)
}

df.melt <- melt(df.w.buffer.spline[,c("week", "progress.twitter", "progress.NASS")],
                id=c("week"))

## goal is stacked plots:
# 1. barplot of tweets/week
# 2. point plot of cumulative tweets through time with line for logistic fit
# 3. line plot of second derivative of logistic fit
# 4. line+point plot of cumulative tweets for growing period
# 5. line+point plot of planting progress for Twitter and NASS

p.bar.tweets.week <-
  ggplot(df.w, aes(x=week, y=tweets)) +
  geom_hline(yintercept=0, color="gray65") +
  geom_bar(stat="identity", fill=col.blue) +
  scale_x_continuous(breaks=seq(10, 25, 5)) +
  theme_SCZ()

p.point.tweets.cum.fit <-
  ggplot(df.w, aes(x=week)) +
  geom_hline(yintercept=0, color="gray65") +
  geom_point(aes(y=tweets.cum), color=col.blue) +
  geom_line(aes(y=fit.spline), color="black") +
  scale_x_continuous(breaks=seq(10, 25, 5)) +
  theme_SCZ()

p.line.deriv2 <-
  ggplot(df.w, aes(x=week, y=fit.spline.deriv2)) +
  geom_hline(yintercept=0, color="gray65") +
  #  annotate("rect", xmin=wk.start, xmax=wk.end, ymin=-Inf, ymax=Inf, fill=col.red, color=NA, alpha=0.25) +
  geom_line() + 
  #  geom_vline(xintercept=df.w$week[c(which.max(df.w$fit.deriv2), which.min(df.w$fit.deriv2))], linetype="dashed") +
  scale_x_continuous(breaks=seq(10, 25, 5)) +
  theme_SCZ()

p.tweets.buffer <-
  ggplot(df.w.buffer.spline, aes(x=week, y=tweets.cum.sub)) +
  geom_hline(yintercept=0, color="gray65") +
  geom_line(color=col.blue) +
  geom_point(color=col.blue) +
  scale_x_continuous(breaks=c(15,20)) +
  theme_SCZ()

p.comparison <-
  ggplot(df.melt, aes(x=week, y=value, color=variable)) +
  geom_hline(yintercept=0, color="gray65") +
  geom_line() +
  geom_point() +
  scale_color_manual(name="Source", labels=c("progress.twitter"="Twitter", "progress.NASS"="NASS"),
                     values=c("progress.twitter"=col.blue, "progress.NASS"=col.red), guide=F) +
  scale_x_continuous(breaks=c(15,20)) +
  theme_SCZ()

tot.height <- 6  # height in inches

pdf(paste0(plot.dir, "Figure_Planting_Methodology_Top3_NoText.pdf"), width=(77/25.4), height=(tot.height*0.6))
grid.arrange(p.bar.tweets.week+theme(text=element_blank(), plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "mm")),
             p.point.tweets.cum.fit+theme(text=element_blank(), plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "mm")),
             p.line.deriv2+theme(text=element_blank(), plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "mm")), 
             ncol=1)
dev.off()

pdf(paste0(plot.dir, "Figure_Planting_Methodology_Bottom2_NoText.pdf"), width=(77/25.4), height=(tot.height*0.4))
grid.arrange(p.tweets.buffer+theme(text=element_blank(), plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "mm")),
             p.comparison+theme(text=element_blank(), plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "mm")),
             ncol=1)
dev.off()