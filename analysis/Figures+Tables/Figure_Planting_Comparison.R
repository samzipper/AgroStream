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
require(reshape2)
source(paste0(git.dir, "analysis/plots/plot_colors.R"))
source(paste0(git.dir, "analysis/interp.R"))

# which fit method to use? "spline" or "logistic"
fit.method <- "spline"

# plot directory
plot.dir <- paste0(git.dir, "analysis/Figures+Tables/")

# read in data
df.in <- read.csv(paste0(git.dir, "analysis/TweetsOut_02_CompareToNASS_df.all.csv"), stringsAsFactors=F)

# summarize to method
df.in <- subset(df.in, method==fit.method)

# make a column that combines state and crop
df.in$crop.state <- paste0(df.in$crop, ".", df.in$state.abb)
crop.state.combos <- unique(df.in$crop.state)

## cross-validation to select best buffer
df.cal <- df.in
samples <- t(combn(crop.state.combos, 5))
for (i in 1:dim(samples)[1]){
  # get data frame
  df.cal$sample <- "cal"
  
  # sample crop.state.combos for cal/val
  df.cal$sample[df.cal$crop.state %in% samples[i,]] <- "val"
  
  # calculate fit
  df.iter.fit <- dplyr::summarize(group_by(df.cal, wk.buffer.start, wk.buffer.end, sample),
                                  RMSE = rmse(progress.twitter, progress.NASS),
                                  MAE = mae(progress.twitter, progress.NASS),
                                  KGE = KGE(progress.twitter, progress.NASS),
                                  bias.prc = pbias(progress.twitter, progress.NASS),
                                  iteration=i)
  
  if (exists("df.iter.all")){
    df.iter.all <- rbind(df.iter.all, df.iter.fit)
  } else {
    df.iter.all <- df.iter.fit
  }
}

# summarize by cal/val
df.fit.buffer.iter <- dplyr::summarize(group_by(df.iter.all, wk.buffer.start, wk.buffer.end, sample, iteration),
                                       MAE.iter.mean = mean(MAE),
                                       RMSE.iter.mean = mean(RMSE),
                                       KGE.iter.mean = mean(KGE))

df.fit.buffer <- dplyr::summarize(group_by(df.fit.buffer.iter, wk.buffer.start, wk.buffer.end, sample),
                                  MAE.min = min(MAE.iter.mean),
                                  MAE.mean = mean(MAE.iter.mean),
                                  MAE.max = max(MAE.iter.mean),
                                  MAE.sd = sd(MAE.iter.mean),
                                  RMSE.min = min(RMSE.iter.mean),
                                  RMSE.mean = mean(RMSE.iter.mean),
                                  RMSE.max = max(RMSE.iter.mean),
                                  RMSE.sd = sd(RMSE.iter.mean),
                                  KGE.min = min(KGE.iter.mean),
                                  KGE.mean = mean(KGE.iter.mean),
                                  KGE.max = max(KGE.iter.mean),
                                  KGE.sd = sd(KGE.iter.mean))

# select best buffers
df.fit.buffer.cal <- subset(df.fit.buffer, sample=="cal")
i.best.buffer <- which(df.fit.buffer.cal$MAE.mean==min(df.fit.buffer.cal$MAE.mean))
wk.buffer.start.best <- df.fit.buffer.cal$wk.buffer.start[i.best.buffer]
wk.buffer.end.best <- df.fit.buffer.cal$wk.buffer.end[i.best.buffer]

# subset to best
df.all <- subset(df.in, (wk.buffer.start==wk.buffer.start.best & wk.buffer.end==wk.buffer.end.best))

df.fit.best <- dplyr::summarize(group_by(subset(df.in, (wk.buffer.start==wk.buffer.start.best & wk.buffer.end==wk.buffer.end.best)), crop.state),
                                RMSE = rmse(progress.twitter, progress.NASS),
                                MAE = mae(progress.twitter, progress.NASS),
                                KGE = KGE(progress.twitter, progress.NASS),
                                bias.prc = pbias(progress.twitter, progress.NASS))

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
  theme_SCZ()

pdf(paste0(plot.dir, "Figure_Planting_Comparison_PrcVsTime_NoText.pdf"), width=(172/25.4), height=(60/25.4))
p.facet.prc + theme(text=element_blank(), plot.margin=unit(c(0.5,0.5,0,0), "mm"))
dev.off()

p.scatter <-
  ggplot(df.all, aes(x=progress.NASS, y=progress.twitter)) +
  geom_abline(intercept=0, slope=1, color="gray65") +
  geom_point(color="black", aes(shape=crop)) +
  stat_smooth(method="lm", se=F, color="black") +
  scale_x_continuous(name="% Planted, NASS", limits=c(0,100)) +
  scale_y_continuous(name="% Planted, Twitter", limits=c(0,100)) +
  scale_shape_manual(values=c("corn"=24, "soy"=22), guide=F) +
#  scale_linetype_manual(values=c("corn"="solid", "soy"="dashed"), guide=F) +
  theme_SCZ() +
  theme(legend.position="bottom")

pdf(paste0(plot.dir, "Figure_Planting_Comparison_TwitterVsNASS_NoText.pdf"), width=(77/25.4), height=(77/25.4))
p.scatter + theme(text=element_blank(), plot.margin=unit(c(0.5,0.5,0,0), "mm"))
dev.off()

# validation statistics
df.fit.buffer$MAE.min[df.fit.buffer$sample=="val" & df.fit.buffer$wk.buffer.start==wk.buffer.start.best & df.fit.buffer$wk.buffer.end==wk.buffer.end.best]
df.fit.buffer$MAE.mean[df.fit.buffer$sample=="val" & df.fit.buffer$wk.buffer.start==wk.buffer.start.best & df.fit.buffer$wk.buffer.end==wk.buffer.end.best]
df.fit.buffer$MAE.max[df.fit.buffer$sample=="val" & df.fit.buffer$wk.buffer.start==wk.buffer.start.best & df.fit.buffer$wk.buffer.end==wk.buffer.end.best]
