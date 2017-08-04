## Figure_Planting_Comparison.R
#' This script is intended to make two comparison plots between NASS and Twitter results:
#'  -Percent planted vs. Time, with different colors
#'  -Scatterplot of Twitter vs NASS
#' 
#' Requires output from TweetsOut_02_CompareToNASS.R

rm(list=ls())

# path to git directory
git.dir <- "C:/Users/Sam/WorkGits/AgroStream/"

# load packages
require(dplyr)
require(ggplot2)
require(hydroGOF)
require(gridExtra)
source(paste0(git.dir, "analysis/plots/plot_colors.R"))
source(paste0(git.dir, "analysis/interp.R"))

# plot directory
plot.dir <- paste0(git.dir, "analysis/Figures+Tables/")

# read in data
df.all <- read.csv(paste0(git.dir, "analysis/TweetsOut_02_CompareToNASS_df.all.csv"), stringsAsFactors=F)

## summarize by state and crop
# comparing progress between NASS and cumsum approaches (units are %)
df.fit <- dplyr::summarize(group_by(df.all, wk.buffer.start, wk.buffer.end, state.abb, crop),
                           RMSE = rmse(progress.twitter, progress.NASS),
                           MAE = mae(progress.twitter, progress.NASS),
                           KGE = KGE(progress.twitter, progress.NASS),
                           bias.prc = pbias(progress.twitter, progress.NASS),
                           tweets.tot = mean(tweets.tot))

# summarize fit by buffer period and crop
df.fit.buffer <- dplyr::summarize(group_by(df.fit, wk.buffer.start, wk.buffer.end, crop),
                                  RMSE.mean = mean(RMSE),
                                  MAE.mean = mean(MAE),
                                  KGE.min = min(KGE),
                                  KGE.mean = mean(KGE),
                                  KGE.max = max(KGE),
                                  bias.min = min(bias.prc),
                                  bias.mean = mean(bias.prc),
                                  bias.max = max(bias.prc))

# sum of the absolute value of biases
df.fit.buffer$bias.sum <- abs(df.fit.buffer$bias.min) + abs(df.fit.buffer$bias.mean) + abs(df.fit.buffer$bias.max)

# select best buffers
i.best.corn <- which(df.fit.buffer$MAE.mean==min(subset(df.fit.buffer, crop=="corn")$MAE.mean))
wk.buffer.start.best.corn <- df.fit.buffer$wk.buffer.start[i.best.corn]
wk.buffer.end.best.corn <- df.fit.buffer$wk.buffer.end[i.best.corn]

i.best.soy <- which(df.fit.buffer$MAE.mean==min(subset(df.fit.buffer, crop=="soy")$MAE.mean))
wk.buffer.start.best.soy <- df.fit.buffer$wk.buffer.start[i.best.soy]
wk.buffer.end.best.soy <- df.fit.buffer$wk.buffer.end[i.best.soy]

# subset to best
df.all <- subset(df.all, (crop=="corn" & wk.buffer.start==wk.buffer.start.best.corn & wk.buffer.end==wk.buffer.end.best.corn) | 
                   (crop=="soy" & wk.buffer.start==wk.buffer.start.best.soy & wk.buffer.end==wk.buffer.end.best.soy))

df.fit.best <- subset(df.fit, (crop=="corn" & wk.buffer.start==wk.buffer.start.best.corn & wk.buffer.end==wk.buffer.end.best.corn) | 
                        (crop=="soy" & wk.buffer.start==wk.buffer.start.best.soy & wk.buffer.end==wk.buffer.end.best.soy))

# make a column that combines state and crop
df.all$crop.state <- paste0(df.all$crop, ".", df.all$state.abb)

# melt
df.melt <- melt(df.all[,c("week", "crop", "state.abb", "crop.state", "progress.twitter", "progress.NASS")],
                id=c("week", "crop", "state.abb", "crop.state"))

df.melt <- df.melt[is.finite(df.melt$value), ]

## make plots
p.facet.prc <-
  ggplot(df.melt, aes(x=week, y=value, color=variable)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ crop.state, ncol=5) +
  scale_color_manual(name="Source", labels=c("progress.twitter"="Twitter", "progress.NASS"="NASS"),
                     values=c("progress.twitter"=col.blue, "progress.NASS"=col.red), guide=F) +
  scale_x_continuous(name="Week", breaks=seq(15,25,5)) +
  scale_y_continuous(name="% Planted") +
  theme_SCZ() +
  theme(legend.position="bottom")

pdf(paste0(plot.dir, "Figure_Planting_Comparison_PrcVsTime_NoText.pdf"), width=(172/25.4), height=(60/25.4))
p.facet.prc + theme(text=element_blank(), plot.margin=unit(c(0.5,0.5,0,0), "mm"))
dev.off()

p.scatter <-
  ggplot(df.all, aes(x=progress.NASS, y=progress.twitter, shape=crop, linetype=crop)) +
  geom_abline(intercept=0, slope=1, color="gray65") +
  geom_point(color="black") +
#  stat_smooth(method="lm", color="black", se=F) +
#  facet_grid(.~crop) +
  scale_x_continuous(name="% Planted, NASS", limits=c(0,100)) +
  scale_y_continuous(name="% Planted, Twitter", limits=c(0,100)) +
  scale_shape_manual(values=c("corn"=24, "soy"=22), guide=F) +
  scale_linetype_manual(values=c("corn"="solid", "soy"="dashed"), guide=F) +
  theme_SCZ() +
  theme(legend.position="bottom")

pdf(paste0(plot.dir, "Figure_Planting_Comparison_TwitterVsNASS_NoText.pdf"), width=(77/25.4), height=(77/25.4))
p.scatter + theme(text=element_blank(), plot.margin=unit(c(0.5,0.5,0,0), "mm"))
dev.off()

# scatterplot overall RMSE
rmse(df.all$progress.twitter, df.all$progress.NASS)
rmse(subset(df.all, crop=="corn")$progress.twitter, subset(df.all, crop=="corn")$progress.NASS)
rmse(subset(df.all, crop=="soy")$progress.twitter, subset(df.all, crop=="soy")$progress.NASS)

mae(df.all$progress.twitter, df.all$progress.NASS)
mae(subset(df.all, crop=="corn")$progress.twitter, subset(df.all, crop=="corn")$progress.NASS)
mae(subset(df.all, crop=="soy")$progress.twitter, subset(df.all, crop=="soy")$progress.NASS)
