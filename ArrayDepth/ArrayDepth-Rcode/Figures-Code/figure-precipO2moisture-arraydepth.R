# figure-precipO2moisture-arraydepth.R
# 
# work with the O2 and moisture sensors for the depth measurements
#
# O2 - redox - GHG project
# CS O'Connell, UCB, Silver Lab

# output products:
# figures in folder /ArrayDepth-Figures/


########################################################################
# GET READY TO BRING IN DATA

library(ggplot2)
library(gridExtra)
library(scales)
library(dplyr)
library(nlme)      ## for lme()
library(multcomp)  ## for multiple comparison stuff
#library(data.table)
#library(chron)
library(lubridate)
#library(lattice)
#library(reshape2)
#options(java.parameters = "-Xmx5000m") # make sure there's sufficient memory to open excel file
library(xlsx)
library(strucchange) # piecewise regression
library(reshape2)

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# where to save outputs
pathsavefigs = "~/Documents/GITHUB/cso041code_ArrayDepth/ArrayDepth-Figures/"
pathdata = "~/Documents/GITHUB/cso041code_ArrayDepth/"


########################################################################
# BRING IN DEPTH INITIAL DATA, MAKE FIGURES

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





