## Figure_Planting_TweetMap.R
#' This is intended to make a figure with a timeseries and map of total tweets
#' in the filtered dataset.
#'
#' Requires output from TweetsOut_01_ClassifyByStates.R

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

## summarize by state and crop
for (crop in crop.list){
  df.crop <- df[str_detect(df$text, crop), ]
  df.crop$crop <- crop
  
  if (exists("df.crop.all")){
    df.crop.all <- rbind(df.crop.all, df.crop)
  } else {
    df.crop.all <- df.crop
  }
}

# summarize by day and week
df.d <- dplyr::summarize(group_by(df, DOY),
                         n.tweets = sum(is.finite(as.numeric(week))))
df.w <- dplyr::summarize(group_by(df, week),
                         n.tweets = sum(is.finite(as.numeric(week))))

## make some plots
# convert state names to lowercase with column name 'region' for merge with map data
df$region <- str_to_lower(df$state)

# summarize by state
df.state <- dplyr::summarize(group_by(df, region),
                             n.tweets = sum(is.finite(as.numeric(id))))
df.state$state.abb <- abb2state(df.state$region, convert=T)

# prepare polygon for ggplot
data.maps <- map_data("state")
df.map <- left_join(data.maps, df.state, by="region")
df.map <- df.map[order(df.map$order),]

# map of tweets
p.map <-
  ggplot(df.map, aes(x=long, y=lat, group=group, fill=log10(n.tweets))) +
  geom_polygon(color="white", size=0.25) +
  scale_fill_viridis(name="log(Tweets)", na.value="gray65") +
#  scale_color_viridis(name="log(Tweets)", alpha=0.75) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  coord_map() +
  theme_SCZ() +
  theme(panel.grid=element_blank(),
        panel.border=element_blank())

# barplots by day and week
p.tweets.d <- 
  ggplot(df.d, aes(x=DOY, y=n.tweets)) +
  geom_hline(yintercept=0, color="gray65") +
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(60, 180, 60), expand=c(0,0)) +
  theme_SCZ()

p.tweets.w <- 
  ggplot(df.w, aes(x=week, y=n.tweets)) +
  geom_hline(yintercept=0, color="gray65") +
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(10, 25, 5), expand=c(0,0)) +
  theme_SCZ()

pdf(paste0(plot.dir, "Figure_Planting_Map+Bars_NoText.pdf"), width=(85/25.4), height=(80/25.4))
grid.arrange(p.map + theme(text=element_blank(), plot.margin=unit(c(0,0,0,0), "mm"), axis.ticks=element_blank(), 
                           legend.key.width=unit(4, "mm"), legend.key.height=unit(4, "mm"), legend.position=c(0,0), 
                           legend.justification=c(0,0), legend.direction="horizontal", legend.background=element_blank()),
            arrangeGrob(p.tweets.d + theme(text=element_blank(), plot.margin=unit(c(0,0.5,0,8), "mm")), 
                        p.tweets.w + theme(text=element_blank(), plot.margin=unit(c(0,0.5,0,8), "mm")), 
                        ncol=2),
            ncol=1, heights=c(1, 0.5))
dev.off()

