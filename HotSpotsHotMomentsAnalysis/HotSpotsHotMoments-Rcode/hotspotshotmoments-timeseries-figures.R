# hotspotshotmoments-timeseries-figures.R
# 
# making the figures for the hot spots and hot moments paper
# specifically, making the time series figures to check out the state of the data
#
# O2 - redox - GHG project
# CS O'Connell, UCB, Silver Lab

# output products:
# figures in folder /HotSpotsHotMoments-Figures/


########################################################################
# GET READY TO BRING IN DATA

library(ggplot2)
library(gridExtra)
library(scales)
library(nlme)      ## for lme()
library(multcomp)  ## for multiple comparison stuff
#library(data.table)
#library(chron)
library(lubridate)
#library(lattice)
#library(reshape2)
#options(java.parameters = "-Xmx5000m") # make sure there's sufficient memory to open excel file
library(xlsx)
#library(strucchange) # piecewise regression
library(reshape2)
library(dplyr)
library(plyr)

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# where to save outputs
pathsavefigs = "~/Documents/GITHUB/cso041code_HotSpotsHotMoments/HotSpotsHotMomentsAnalysis/HotSpotsHotMoments-Figures-Analyses/"
pathdata = "~/Documents/GITHUB/cso041code_HotSpotsHotMoments/"
pathrainfalldata = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Rainfall-data-Ryan/"
pathdroughtpaperfiles = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Rprocessed/"


########################################################################
# RAINFALL TIME SERIES FIGURES

# load csv
rainfalldf <- read.csv(paste(pathrainfalldata, "elverde_rainfall_master.csv", sep = ""), stringsAsFactors=FALSE)

# deal with dates and year
rainfalldf$MonthDay <- substr(rainfalldf$Date, 1, nchar(rainfalldf$Date)-3)
rainfalldf$Date <- mdy(rainfalldf$Date)
rainfalldf$Year <- as.factor(rainfalldf$Year)

# subset out just the dates since 11/14/14 (first date of the sensor data)
rainfallstudypd <- subset(rainfalldf, rainfalldf$Date>"2014-11-13")
# note that there is a weird anomoly, but it seems legit: on 2014-12-16, rainfall is recorded as 147.32 mm in a day, but there is a note that says "rainstorm"
# # cumulative rainfall for the jan to jan year
# rainfallstudypd_365 <- subset(rainfalldf, rainfalldf$Date>"2014-12-31" & rainfalldf$Date<="2015-12-31")
# rainfallstudypd_365 <- within(rainfallstudypd_365, acc_sum <- cumsum(Rainfall_mm))

# rainfall by date (study period)
p0 <- ggplot(rainfallstudypd, aes(x=Date, y=Rainfall_mm)) + geom_bar(stat = "identity") + ylab("Daily Rainfall \n(mm)") + theme_bw() + ylim(0.0,50) + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2017-02-28')))

# save figure
png(file = paste(pathsavefigs, "time_series_rainfall_hotspotshotmoments.png", sep=""),width=14,height=7,units="in",res=400)
p0
dev.off()

# subset out the years that you want to use as the daily anomaly comparison
rainfallpast <- subset(rainfalldf, rainfalldf$Date>="2004-01-01" & rainfalldf$Date<="2013-12-31")
rainfallpast$MonthDay2 <- parse_date_time(rainfallpast$MonthDay, "md")

# get average of those ten years for each day
summarytabraincomp <- summarySE(data=rainfallpast, measurevar="Rainfall_mm", c("MonthDay2"), na.rm=TRUE, renameallcols=TRUE) 
# cumulative rainfall for the year
summarytabraincomp <- within(summarytabraincomp, acc_sum <- cumsum(meanRainfall_mm))

# rainfall by date (comparison time period)
p0c <- ggplot(summarytabraincomp, aes(x=MonthDay2, y=meanRainfall_mm)) + geom_bar(stat = "identity") + ylab("Daily Precipitation (mm) \n2004-2013 Mean") + theme_bw() + scale_x_datetime(labels = date_format("%b-%d")) + ylim(0.0,50) 

# calculate the difference between the mean for each day and the date in question
str(rainfallstudypd) # Date, Rainfall_mm # this is the ongoing time period
str(summarytabraincomp) # MonthDay2, meanRainfall_mm

# add a character variable that we'll use to merge
rainfallstudypd$datemerge <- substring(as.character(rainfallstudypd$Date), 6)
summarytabraincomp$datemerge <- substring(as.character(summarytabraincomp$MonthDay2), 6)

# merge datasets
rainfalldeviation <- merge(rainfallstudypd,summarytabraincomp,by="datemerge")
# define difference column
rainfalldeviation$diff <- rainfalldeviation$Rainfall_mm - rainfalldeviation$meanRainfall_mm

# rainfall by date (comparison time period)
p0d <- ggplot(rainfalldeviation, aes(x=Date, y=diff)) + geom_bar(stat = "identity", aes(fill = diff>0), position="dodge") + ylab("Daily Precipitation Anomaly (mm) \nStudy Period vs. 2004-13 Mean") + theme_bw() + theme(legend.position="none") + scale_fill_manual(values=c("red3", "grey25")) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + geom_vline(xintercept=as.numeric(rainfalldeviation$Date[168]), linetype=2) # drought paper ended at 2016-02-25, hence vertical line  #+ scale_x_datetime(labels = date_format("%b-%d")) + geom_vline(xintercept=as.numeric(rainfalldeviation$Date[116]), linetype=2) + geom_vline(xintercept=as.numeric(rainfalldeviation$Date[237]), linetype=2) + geom_vline(xintercept=as.numeric(rainfalldeviation$Date[329]), linetype=2) 

# save figure
png(file = paste(pathsavefigs, "time_series_rainfall_anomaly_hotspotshotmoments.png", sep=""),width=14,height=7,units="in",res=400)
p0d
dev.off()

# what are the really large anomalies?
ht(sort(rainfalldeviation$diff))
ht(sort(rainfallstudypd$Rainfall_mm))
tmp <- which(grepl(147.32, rainfallstudypd$Rainfall_mm))
rainfallstudypd[tmp,]
tmp2 <- which(grepl(114.3, rainfallstudypd$Rainfall_mm))
rainfallstudypd[tmp2,]

# post drought paper, is there a net anomaly?
postdrought <- rainfalldeviation[rainfalldeviation$Date>"2016-02-25",]
sum(postdrought$diff)
sum(postdrought$diff)/dim(postdrought)[1]
# sum(postdrought$diff)
# [1] -744.0844
# > sum(postdrought$diff)/dim(postdrought)[1]
# [1] -2.016489


########################################################################
# MOISTURE TIME SERIES FIGURES

# write.csv(summarytab2, file=paste(pathdroughtpaperfiles, "arraysensorsdf_moisturedailysummarystats_finalpaperdataset_4-11-2017.csv", sep = ""), row.names=FALSE)













########################################################################
# THINGS TO IGNORE: RAINFALL CUMULATIVE CURVE
# 
# # cumulative rainfall graphs onto the same plot
# # get both datasets into same df
# 
# # reference data
# summarytabraincomp$DateJoin <- as.character(summarytabraincomp$MonthDay2)
# summarytabraincomp$DateJoin2 <- substr(summarytabraincomp$DateJoin, 6, nchar(summarytabraincomp$DateJoin))
# 
# # our study data
# rainfallstudypd_365$DateJoin <- as.character(rainfallstudypd_365$Date)
# rainfallstudypd_365$DateJoin2 <- substr(rainfallstudypd_365$DateJoin, 6, nchar(rainfallstudypd_365$DateJoin))
# rainfallstudypd_365_tojoin <- rainfallstudypd_365[,c(4,7,9)]
# names(rainfallstudypd_365_tojoin) <- c("Rainfall_mm_studypd","acc_sum_studypd","DateJoin2")
# 
# # join study onto reference
# summarytabraincomp <- full_join(summarytabraincomp, rainfallstudypd_365_tojoin, by="DateJoin2")
# 
# # cumulative rainfall, reference period and study year
# p0f <- ggplot(summarytabraincomp) + geom_ribbon(aes(x=MonthDay2, ymin=acc_sum-sdRainfall_mm, ymax=acc_sum+sdRainfall_mm), alpha=0.5) + geom_line(aes(x=MonthDay2, y=acc_sum)) + ylab("Cumulative Precipitation (mm) \n2004-2013 vs. 2015") + theme_bw() + scale_x_datetime(labels = date_format("%b-%d"), name="") + geom_line(aes(x=MonthDay2, y=acc_sum_studypd), linetype=3)
# 
# # save figure
# png(file = paste(pathsavefigs, "cumulativerain_comparison.png", sep=""),width=8,height=5,units="in",res=400)
# p0f
# dev.off()
# ### why can't I get a legend working on this graph???


########################################################################
# RAINFALL STATS AND NUMBERS REPORTING

rainfallpast_totmm$tot[rainfallpast_totmm$Year=="2004-13"]
rainfallpast_totmm$tot[rainfallpast_totmm$Year=="2015"]
mean(rainfalldeviation$diff)
sd(rainfalldeviation$diff)
sum(rainfalldeviation$diff>0)
sum(rainfalldeviation$diff<=0)
sum(rainfalldeviation$diff>0) + sum(rainfalldeviation$diff<=0) # check you didn't double count






########################################################################
# BRING IN MOISTURE AND O2 DATA

##### go to the ghg array scripts and save these files as clean csvs at the end of the analyses

vwchourlydf <- read.csv(paste(pathdata, "vwchourly.csv", sep = ""), stringsAsFactors=FALSE)
# only dates after August 1, 2016
vwchourlydf$Date2 <- as.Date(vwchourlydf$Date2)
vwchourlydf_recent <- subset(vwchourlydf, vwchourlydf$Date2>"2016-08-01")
# timing
vwchourlydf_recent$TIMESTAMP3 <- ymd_hms(vwchourlydf_recent$TIMESTAMP2)

# wide to long
vwchourlylong <- gather(vwchourlydf_recent, SensorID, VWC, Sensor01:Sensor35)

library(tidyr)

# vwc
vwchourlylong$VWC2 <- as.numeric(as.character(vwchourlylong$VWC))

# 

topocolors <- rev(colorRampPalette(c('red','orange','purple','blue','dark blue'), space = "Lab")(7))
#topocolors <- heat_hcl(7, h=c(0,-100), l=c(75,40), c=c(40,80), power=1)
#topocolors <- diverge_hcl(7, l=c(50,90), c=100, power=1)
#topocolors <- diverge_hcl(7, h=c(255,330), l=c(40,90))
#topocolors <- rev(colorRampPalette(c('orange','red','purple','blue'), space = "Lab")(7))
#topocolors <- c("#d73027","#fc8d59","#fee090","#ffffbf","#e6f598","#99d594","#4575b4")

topobreaks <- c("1","2","3","4","5","6","7")
topolabs <- c("1 (Ridge)","2","3","4","5","6","7 (Valley)")

# O2 by date (mean and se)
p1 <- ggplot(vwchourlylong, aes(x=TIMESTAMP3, y=VWC, color=SensorID)) + geom_point() + facet_wrap( ~ SensorID, nrow=5) + theme_bw() + theme(axis.text.x=element_text(angle=90)) + theme(axis.text.y=element_text(angle=0))

#+ geom_errorbar(aes(ymin=meanO2-seO2, ymax=meanO2+seO2), alpha=0.5) + ylab("Soil O2 \n(Mean Fraction +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25'))) + scale_colour_manual(values=topocolors, name="Topographic\nLocation", labels=topolabs) + geom_vline(xintercept=as.numeric(summarytab2$Date[bpnum1]), linetype=2) + geom_vline(xintercept=as.numeric(summarytab2$Date[bpnum2]), linetype=2) + geom_vline(xintercept=as.numeric(summarytab2$Date[bpnum3]), linetype=2) 
#+ scale_colour_discrete(name="Topographic\nLocation", labels=topolabs)




# load csv
arraysensorsdf <- read.csv(paste(pathdata, "fulldaily.csv", sep = ""), stringsAsFactors=FALSE)

# make factors where needed
arraysensorsdf$TransectID <- as.factor(arraysensorsdf$TransectID)
arraysensorsdf$TopoLocation <- as.factor(arraysensorsdf$TopoLocation)

# get rid of any O2 values above 0.23
arraysensorsdf$avgO2pct[arraysensorsdf$avgO2pct>0.225] <- NA
arraysensorsdf$sdO2pct[arraysensorsdf$avgO2pct>0.225] <- NA
arraysensorsdf$seO2pct[arraysensorsdf$avgO2pct>0.225] <- NA


# fix weird character O2 thing
#arraysensorsdf$O2 <- as.numeric(arraysensorsdf$O2)



########################################################################
# SUMMARY STATS: O2, MOISTURE ACROSS TRANSECTS AT EACH DATE

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarySE O2
summarytab1tmp <- summarySE(data=arraysensorsdf, measurevar="avgO2pct", c("Date2", "TopoLocation"), na.rm=TRUE, renameallcols=TRUE)
# take out lines where there are no observations
summarytab1 <- subset(summarytab1tmp,summarytab1tmp$N>0.5)



summarytab1tmp <- summarySE(data=arraysensorsdf, measurevar="O2", c("Date", "DayCount", "TopoLocation", "Drought"), na.rm=TRUE, renameallcols=TRUE)
# summarySE moisture
summarytab2tmp <- summarySE(data=arraysensorsdf, measurevar="SoilMoisture", c("Date", "DayCount", "TopoLocation", "Drought"), na.rm=TRUE, renameallcols=TRUE)

# take out weird NA lines
summarytab1 <- subset(summarytab1tmp,summarytab1tmp$N>0.5)
summarytab2 <- subset(summarytab2tmp,summarytab2tmp$N>0.5)

# save these as csv for use internally (to pass to Whendee, etc.)
write.csv(summarytab1, file=paste(pathsavefiles, "arraysensorsdf_O2dailysummarystats.csv", sep = ""), row.names=FALSE)
write.csv(summarytab2, file=paste(pathsavefiles, "arraysensorsdf_moisturedailysummarystats.csv", sep = ""), row.names=FALSE)

# convert summary table dates (needed for doing analysis, but not good before writing to csv)
summarytab1$Date <- ymd(summarytab1$Date)
summarytab2$Date <- ymd(summarytab2$Date)





########################################################################
# ANALYSIS 1: DISTRIBUTION OF FLUXES

# histogram of the fluxes in the dataset
# use to ID what are "hot" fluxes (low and high emissions)



# what is the diurnal cycle?
# does this change across topographic zones?




########################################################################
# ANALYSIS 2: DESCRIPTIVE OF HOT SPOTS AND MOMENTS

# top 10% of fluxes (or whatever) - how many per chamber?
# likelihood that hot moments last >1 reading?
# what about "chains" of high readings? (this is going to be hard to calculate, I think)




########################################################################
# ANALYSIS 3: PREDICTORS OF HOT SPOTS AND MOMENTS

# do a bunch of regressions
# based on that do fancier regressions
# think about using PCA?


# attempt a predictive stats model
# something that also accounts for time lags and legacy effects of the qualities that seem to be good predictors (e.g., O2)?
# check out that model from Whendee's old postdoc










# load csv
depthO2 <- read.csv(paste(pathdata, "initial_depth_o2.csv", sep = ""), stringsAsFactors=FALSE)

# deal with time stamp
depthO2$timestamp2 <- mdy_hm(depthO2$TIMESTAMP)
str(depthO2)

# convert wide to long format
depthO2long <- melt(depthO2, id.vars = "timestamp2", measure.vars = grep("signal", names(depthO2), value = TRUE))

# roughly switch from mV to percent
# from o2sensorscalibration.csv
#         slope	        intercept
#avg	    0.004278315	  0.012834944
#std dev	0.000174294	  0.000522883

slopeuse <- 0.004278315
interceptuse <- 0.012834944
depthO2long$O2pct <- ((depthO2long$value * slopeuse) - interceptuse) * 100

# quick scatterplots for each sensor
ggplot(depthO2long, aes(y = O2pct, x = timestamp2, colour = as.factor(variable))) + geom_point() + geom_line(aes(group = variable))

# get rid of signal_4_Avg.10.
depthO2long_use <- subset(depthO2long, variable != "signal_4_Avg.10.")

# quick scatterplots for each sensor
ggplot(depthO2long_use, aes(y = O2pct, x = timestamp2, colour = as.factor(variable))) + geom_point() + geom_line(aes(group = variable))


########################################################################
# TO DO, NOTES





