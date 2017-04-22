# hotspotshotmoments-timeseries-GHG-figures.R
# 
# making the figures for the hot spots and hot moments paper
# specifically, making the time series figures for the GHG results
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
pathGHGdata = "~/Documents/GITHUB/NOT REPOS/cso040code_ArrayGHG_LargeFiles/Chamber-data-large-files/eosAnalyzeACProcessed/"
soildatapath <- "~/Documents/GITHUB/NOT REPOS/cso040code_ArrayGHG_LargeFiles/HotSpotsHotMoments-DataMegaFolder/HotSpotsHotMoments-Data-Raw/Sensors/SurfaceOutOfDate/OldestCSOHas/renamedtomatchcode/"
pathsensorchambermatch = "~/Documents/GITHUB/cso041code_HotSpotsHotMoments/HotSpotsHotMomentsAnalysis/"


########################################################################
# BRING IN GHG DATA, MAKE DATAFRAME

# load csv
arrayGHGdf <- read.csv(paste(pathGHGdata, "ArrayGHG_master_chamber_data-deadbandcombined-04202017.csv", sep = ""), stringsAsFactors=FALSE)

# nice names
newnames <- c("DateTime","Chamber","MeasurementDuration_s","MeanCO2_ppm","MeanCH4_ppm","MeanN2O_ppm","MeanNH3_ppm","MeanH2O_percent","ChemDetect_0-1","CavPressure_kPa","CavTemperature_K","WaterContent_fraction","ChamberTemperature_K","ChamberPressure_kPa","FluxCO2_L_umolm2s","FluxCO2_E_umolm2s","FluxCH4_L_nmolm2s","FluxCH4_E_nmolm2s","FluxN2O_L_nmolm2s","FluxN2O_E_nmolm2s","FluxNH3_L_umolm2s","FluxNH3_E_umolm2s","e_FluxCO2_L_umolm2s","e_FluxCO2_E_umolm2s","e_FluxCH4_L_nmolm2s","e_FluxCH4_E_nmolm2s","e_FluxN2O_L_nmolm2s","e_FluxN2O_E_nmolm2s","e_FluxNH3_L_umolm2s","e_FluxNH3_E_umolm2s","AUXVoltage1_V","AUXVoltage2_V","AUXVoltage3_V","AUXCurrent1_mA","AUXCurrent2_mA","Metadata","Deadband","System","DataSource")
names(arrayGHGdf) <- newnames

# make dates nice
arrayGHGdf$DateTime2 <- mdy_hm(arrayGHGdf$DateTime)

# rip out date only
arrayGHGdf$Date <- substr(arrayGHGdf$DateTime2,1,11)
arrayGHGdf$Date <- ymd(arrayGHGdf$Date)

# assign topographic location

# Silver system assign
arrayGHGdf$Topo <- NA
arrayGHGdf$Topo[arrayGHGdf$System=="Silver" & (arrayGHGdf$Chamber=="1" | arrayGHGdf$Chamber=="4" | arrayGHGdf$Chamber=="7")] <- "Ridge"
arrayGHGdf$Topo[arrayGHGdf$System=="Silver" & (arrayGHGdf$Chamber=="2" | arrayGHGdf$Chamber=="5" | arrayGHGdf$Chamber=="8")] <- "Slope"
arrayGHGdf$Topo[arrayGHGdf$System=="Silver" & (arrayGHGdf$Chamber=="3" | arrayGHGdf$Chamber=="6" | arrayGHGdf$Chamber=="9")] <- "Valley"

# Mayes system assign
arrayGHGdf$Topo[arrayGHGdf$System=="Mayes" & (arrayGHGdf$Chamber=="1" | arrayGHGdf$Chamber=="5" | arrayGHGdf$Chamber=="9")] <- "Ridge"
arrayGHGdf$Topo[arrayGHGdf$System=="Mayes" & (arrayGHGdf$Chamber=="2" | arrayGHGdf$Chamber=="3" | arrayGHGdf$Chamber=="6" | arrayGHGdf$Chamber=="7" | arrayGHGdf$Chamber=="10" | arrayGHGdf$Chamber=="11")] <- "Slope"
arrayGHGdf$Topo[arrayGHGdf$System=="Mayes" & (arrayGHGdf$Chamber=="4" | arrayGHGdf$Chamber=="8" | arrayGHGdf$Chamber=="12")] <- "Valley"

# what about the low-slope vs. upper-slope distinction?

# Silver system assign
arrayGHGdf$Topo4Cats <- NA
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Silver" & (arrayGHGdf$Chamber=="1" | arrayGHGdf$Chamber=="4" | arrayGHGdf$Chamber=="7")] <- "Ridge"
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Silver" & (arrayGHGdf$Chamber=="5")] <- "Upper Slope"
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Silver" & (arrayGHGdf$Chamber=="2" | arrayGHGdf$Chamber=="8")] <- "Low Slope"
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Silver" & (arrayGHGdf$Chamber=="3" | arrayGHGdf$Chamber=="6" | arrayGHGdf$Chamber=="9")] <- "Valley"

# Mayes system assign
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Mayes" & (arrayGHGdf$Chamber=="1" | arrayGHGdf$Chamber=="5" | arrayGHGdf$Chamber=="9")] <- "Ridge"
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Mayes" & (arrayGHGdf$Chamber=="2" | arrayGHGdf$Chamber=="6" | arrayGHGdf$Chamber=="10")] <- "Upper Slope"
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Mayes" & (arrayGHGdf$Chamber=="3" | arrayGHGdf$Chamber=="7" | arrayGHGdf$Chamber=="11")] <- "Low Slope"
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Mayes" & (arrayGHGdf$Chamber=="4" | arrayGHGdf$Chamber=="8" | arrayGHGdf$Chamber=="12")] <- "Valley"

# make factors where needed
arrayGHGdf$Chamber <- as.factor(arrayGHGdf$Chamber)
arrayGHGdf$Topo <- as.factor(arrayGHGdf$Topo)
arrayGHGdf$Topo4Cats <- as.factor(arrayGHGdf$Topo4Cats)
# str(arrayGHGdf)

# For the analyses below, keep arrayGHGdf as only the good deadband fluxes
arrayGHGdf_all <- arrayGHGdf
arrayGHGdf <- subset(arrayGHGdf_all, arrayGHGdf_all$Deadband!="Bad_30")


########################################################################
# QUALITY CONTROL FOR GHG DATA

# how many observations did we start with?
gasobvsstart <- dim(arrayGHGdf)[1]

## inappropriate fluxes

# take out any fluxes associated with negative CO2 fluxes (troubleshoot these later)
# throw out all gases if CO2 is bad
arrayGHGdf <- subset(arrayGHGdf, arrayGHGdf$FluxCO2_E_umolm2s>=0)
arrayGHGdf <- subset(arrayGHGdf, arrayGHGdf$FluxCO2_L_umolm2s>=0)
# take out all gases if CO2 fluxes over abs(20) nmol/m2/s
arrayGHGdf <- subset(arrayGHGdf, arrayGHGdf$FluxCO2_E_umolm2s<20)
arrayGHGdf <- subset(arrayGHGdf, arrayGHGdf$FluxCO2_L_umolm2s<20)

# take out any N2O fluxes over abs(100) nmol/m2/s
arrayGHGdf$FluxN2O_E_nmolm2s[abs(arrayGHGdf$FluxN2O_E_nmolm2s) > 100] <- NA
arrayGHGdf$FluxN2O_L_nmolm2s[abs(arrayGHGdf$FluxN2O_L_nmolm2s) > 100] <- NA

# take out any CH4 fluxes over abs(1000) nmol/m2/s
arrayGHGdf$FluxCH4_E_nmolm2s[abs(arrayGHGdf$FluxCH4_E_nmolm2s) > 1000] <- NA
arrayGHGdf$FluxCH4_L_nmolm2s[abs(arrayGHGdf$FluxCH4_L_nmolm2s) > 1000] <- NA

## deployment period too long or short

# take out places where the deployment period of the chamber was too long or too short
toolong <- 661
tooshort <- 539
# print info about removed 
tmp <- arrayGHGdf$MeasurementDuration_s
tmp2 <- tmp[tmp < toolong]
tmp3 <- tmp2[tmp2 > tooshort]
durationok <- length(tmp3)/length(tmp)
cat("You have removed flux measurements where chamber duration length was shorter than ", 
    toolong, " and longer than ", tooshort, " seconds; this left ", durationok*100, 
    "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")

# take out chambers deployed too long/short
arrayGHGdf <- subset(arrayGHGdf, MeasurementDuration_s > tooshort & MeasurementDuration_s < toolong)

## cavity pressure too high or low

# take out places where the cavity pressure wasn't in an ok range
toobig <- mean(arrayGHGdf$CavPressure_kPa) + 4*sd(arrayGHGdf$CavPressure_kPa)
toosmall <- mean(arrayGHGdf$CavPressure_kPa) - 4*sd(arrayGHGdf$CavPressure_kPa)
# print info about removed 
tmp <- arrayGHGdf$CavPressure_kPa
tmp2 <- tmp[tmp < toobig]
tmp3 <- tmp2[tmp2 > toosmall]
durationok <- length(tmp3)/length(tmp)
cat("You have removed flux measurements where chamber cavity pressure was smaller than ", 
    toobig, " and larger than ", toosmall, " kPs; this left ", durationok*100, 
    "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")

# take out chambers with cavity pressure too extreme
arrayGHGdf <- subset(arrayGHGdf, CavPressure_kPa > toosmall & CavPressure_kPa < toobig)

## cavity temperature too high or low

# take out places where the cavity temp wasn't in an ok range
toobig <- mean(arrayGHGdf$CavTemperature_K) + 4*sd(arrayGHGdf$CavTemperature_K)
toosmall <- mean(arrayGHGdf$CavTemperature_K) - 4*sd(arrayGHGdf$CavTemperature_K)
# print info about removed 
tmp <- arrayGHGdf$CavTemperature_K
tmp2 <- tmp[tmp < toobig]
tmp3 <- tmp2[tmp2 > toosmall]
durationok <- length(tmp3)/length(tmp)
cat("You have removed flux measurements where chamber cavity temperature was lower than ", 
    toobig, " and higher than ", toosmall, " K; this left ", durationok*100, 
    "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")

# take out chambers with cavity pressure too extreme
arrayGHGdf <- subset(arrayGHGdf, CavTemperature_K > toosmall & CavTemperature_K < toobig)


###### Nick thinks that the chamber pressure and temperature aren't a good thing to throw out data for (maybe screen and report, but don't dump the data)

## chamber pressure too high or low

# # take out places where the chamber pressure wasn't in an ok range
# toobig <- mean(arrayGHGdf$ChamberPressure_kPa) + 4*sd(arrayGHGdf$ChamberPressure_kPa)
# toosmall <- mean(arrayGHGdf$ChamberPressure_kPa) - 4*sd(arrayGHGdf$ChamberPressure_kPa)
# # print info about removed 
# tmp <- arrayGHGdf$ChamberPressure_kPa
# tmp2 <- tmp[tmp < toobig]
# tmp3 <- tmp2[tmp2 > toosmall]
# durationok <- length(tmp3)/length(tmp)
# cat("You have removed flux measurements where chamber cavity pressure was smaller than ", 
#     toobig, " and larger than ", toosmall, " kPs; this left ", durationok*100, 
#     "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")
# 
# # take out chambers with cavity pressure too extreme
# arrayGHGdf <- subset(arrayGHGdf, ChamberPressure_kPa > toosmall & ChamberPressure_kPa < toobig)
# 
# ## chamber temperature too high or low
# 
# # take out places where the cavity temp wasn't in an ok range
# toobig <- mean(arrayGHGdf$ChamberTemperature_K) + 4*sd(arrayGHGdf$ChamberTemperature_K)
# toosmall <- mean(arrayGHGdf$ChamberTemperature_K) - 4*sd(arrayGHGdf$ChamberTemperature_K)
# # print info about removed 
# tmp <- arrayGHGdf$ChamberTemperature_K
# tmp2 <- tmp[tmp < toobig]
# tmp3 <- tmp2[tmp2 > toosmall]
# durationok <- length(tmp3)/length(tmp)
# cat("You have removed flux measurements where chamber cavity temperature was lower than ", 
#     toobig, " and higher than ", toosmall, " K; this left ", durationok*100, 
#     "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")
# 
# # take out chambers with cavity pressure too extreme
# arrayGHGdf <- subset(arrayGHGdf, ChamberTemperature_K > toosmall & ChamberTemperature_K < toobig)




#### MAYBE SWITCH THIS TO A CUT OFF AND NOT A MEAN +/- STD OPTION

## H2O percent too high or low

# take out places where the cavity temp wasn't in an ok range
toobig <- mean(arrayGHGdf$MeanH2O_percent) + 4*sd(arrayGHGdf$MeanH2O_percent)
toosmall <- mean(arrayGHGdf$MeanH2O_percent) - 4*sd(arrayGHGdf$MeanH2O_percent)
# print info about removed 
tmp <- arrayGHGdf$MeanH2O_percent
tmp2 <- tmp[tmp < toobig]
tmp3 <- tmp2[tmp2 > toosmall]
durationok <- length(tmp3)/length(tmp)
cat("You have removed flux measurements where mean percent H2O was lower than ", 
    toobig, "% and higher than ", toosmall, "%; this left ", durationok*100, 
    "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")

# take out chambers with cavity pressure too extreme
arrayGHGdf <- subset(arrayGHGdf, MeanH2O_percent > toosmall & MeanH2O_percent < toobig)

## water content fraction too high or low

# take out places where the cavity temp wasn't in an ok range
toobig <- mean(arrayGHGdf$WaterContent_fraction) + 4*sd(arrayGHGdf$WaterContent_fraction)
toosmall <- mean(arrayGHGdf$WaterContent_fraction) - 4*sd(arrayGHGdf$WaterContent_fraction)
# print info about removed 
tmp <- arrayGHGdf$WaterContent_fraction
tmp2 <- tmp[tmp < toobig]
tmp3 <- tmp2[tmp2 > toosmall]
durationok <- length(tmp3)/length(tmp)
cat("You have removed flux measurements where mean percent H2O was lower than ", 
    toobig, "% and higher than ", toosmall, "%; this left ", durationok*100, 
    "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")

# take out chambers with cavity pressure too extreme
arrayGHGdf <- subset(arrayGHGdf, WaterContent_fraction > toosmall & WaterContent_fraction < toobig)

#write.csv(arrayGHGdf, file="sampledataforPicarro.csv")

## instances where the mean ppm of a gas is negative or too high

# CO2 mean ppm is too high (indicated potential problem with chamber opening/closing)
toobig <- mean(arrayGHGdf$MeanCO2_ppm) + 4*sd(arrayGHGdf$MeanCO2_ppm)
toosmall <- mean(arrayGHGdf$MeanCO2_ppm) - 4*sd(arrayGHGdf$MeanCO2_ppm)
# print info about removed 
tmp <- arrayGHGdf$MeanCO2_ppm
tmp2 <- tmp[tmp < toobig]
tmp3 <- tmp2[tmp2 > toosmall]
durationok <- length(tmp3)/length(tmp)
cat("You have removed flux measurements where mean ppm CO2 was lower than ", 
    toobig, " and higher than ", toosmall, "; this left ", durationok*100, 
    "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")

# take out chambers with cavity pressure too extreme
arrayGHGdf <- subset(arrayGHGdf, MeanCO2_ppm > toosmall & MeanCO2_ppm < toobig)

# remove instances with negative ppms
# print info about removed 
tmp <- arrayGHGdf$MeanCH4_ppm
negCO2 <- sum(arrayGHGdf$MeanCO2_ppm < 0)
negCH4 <- sum(arrayGHGdf$MeanCH4_ppm < 0)
negN2O <- sum(arrayGHGdf$MeanN2O_ppm < 0)
cat("There were ", negCO2, ", ", negCH4, ", and ", negN2O,
    " observations for CO2, CH4, and N2O respectively where the mean ppm over the ",
    "observation period was negative. ", (negCO2/length(tmp))*100, "%, ",
    (negCH4/length(tmp))*100, "%, and ", (negN2O/length(tmp))*100, "% of the ",
    "observations for CO2, CH4, and N2O respectively will be removed.", sep="")

# cut fluxes for the gas that had the negative reading

# take out any CO2 fluxes with negative ppm values
arrayGHGdf$FluxCO2_E_umolm2s[arrayGHGdf$MeanCO2_ppm < 0] <- NA
arrayGHGdf$FluxCO2_L_umolm2s[arrayGHGdf$MeanCO2_ppm < 0] <- NA

# take out any N2O fluxes with negative ppm values
arrayGHGdf$FluxN2O_E_nmolm2s[arrayGHGdf$MeanN2O_ppm < 0] <- NA
arrayGHGdf$FluxN2O_L_nmolm2s[arrayGHGdf$MeanN2O_ppm < 0] <- NA

# take out any CH4 fluxes with negative ppm values
arrayGHGdf$FluxCH4_E_nmolm2s[arrayGHGdf$MeanCH4_ppm < 0] <- NA
arrayGHGdf$FluxCH4_L_nmolm2s[arrayGHGdf$MeanCH4_ppm < 0] <- NA


## what did we end up with?
gasobvsend <- dim(arrayGHGdf)[1]
remainingobvs <- gasobvsend/gasobvsstart
cat("You began the data quality process with ", gasobvsstart, " observations for each gas. ",
    "You are left with ", gasobvsend, " for ", remainingobvs*100, "% remaining. ",
    ((gasobvsstart-gasobvsend)/gasobvsstart)*100, "% of the observations were removed.", 
    sep="")

# what about for each gas?
naCO2 <- sum(is.na(arrayGHGdf$FluxCO2_L_umolm2s) & is.na(arrayGHGdf$FluxCO2_E_umolm2s))
naCH4 <- sum(is.na(arrayGHGdf$FluxCH4_L_nmolm2s) & is.na(arrayGHGdf$FluxCH4_E_nmolm2s))
naN2O <- sum(is.na(arrayGHGdf$FluxN2O_L_nmolm2s) & is.na(arrayGHGdf$FluxN2O_E_nmolm2s))
cat("Of those ", gasobvsend, " final observations, ",
    naCO2, " fluxes are NA for both the linear and exponential flux estimates of CO2. ",
    "For CH4 and N2O respectively, that situation is true for ", naCH4, " and ", naN2O,
    " of the observations.  That leaves you with ", gasobvsend-naCO2,
    " CO2 flux observations, ", gasobvsend-naCH4, " for CH4, and ", 
    gasobvsend-naN2O, " for N2O.", sep="")


########################################################################
# CHEM DETECT AND OTHER DATA QUALITY GRAPHS

# see evernote "Nick and Chance chemdetect mtg" for info about steps to take

# does meanN2Oppb drift over time or is it event based? should be ~320 +/- 30 ppb
N2Odrift1 <- ggplot(arrayGHGdf, aes(x=DateTime2, y=MeanN2O_ppm, color=Topo)) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"))

# save figures
png(file = paste(pathsavefigs, "drift_N2Oppm.png", sep=""),width=14,height=7,units="in",res=150)
N2Odrift1
dev.off()

# chance could put in the average chemdetect value and not only the binary chemdetect value
chemdetect1 <- ggplot(arrayGHGdf, aes(x=DateTime2, y=`ChemDetect_0-1`, color=Topo)) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"))

png(file = paste(pathsavefigs, "drift_chemdetect.png", sep=""),width=14,height=7,units="in",res=150)
chemdetect1
dev.off()

# chem detect diagnostic figures to send out to Eosense, Picarro
# these were done early in the process of trying to figure out the issues, and I haven't updated the figures since then
# don't always re-do these
makechemdetectfigs <- 2.5
if(makechemdetectfigs < 2) {
  
  ## what about chem_detect_0-1?
  
  sum(arrayGHGdf$`ChemDetect_0-1`)
  length(arrayGHGdf$`ChemDetect_0-1`)
  2471/9462
  # [1] 0.2611499
  # unfortunately, if chemdetect ends up being something we should cut, that's 25% of the samples... so for now I'm not going to cut it
  
  # diagnoses on chemdetect
  
  chemdetect0 <- subset(arrayGHGdf, `ChemDetect_0-1`==0)
  chemdetect1 <- subset(arrayGHGdf, `ChemDetect_0-1`==1)
  
  summary(chemdetect0$FluxCO2_L_umolm2s)
  summary(chemdetect1$FluxCO2_L_umolm2s)
  
  summary(chemdetect0$FluxCO2_E_umolm2s)
  summary(chemdetect1$FluxCO2_E_umolm2s)
  
  summary(chemdetect0$FluxCH4_L_nmolm2s)
  summary(chemdetect1$FluxCH4_L_nmolm2s)
  
  summary(chemdetect0$FluxCH4_E_nmolm2s)
  summary(chemdetect1$FluxCH4_E_nmolm2s)
  
  summary(chemdetect0$FluxN2O_L_nmolm2s)
  summary(chemdetect1$FluxN2O_L_nmolm2s)
  
  summary(chemdetect0$FluxN2O_E_nmolm2s)
  summary(chemdetect1$FluxN2O_E_nmolm2s)
  
  # graphs for chemdetect
  
  CDgraph1 <- ggplot(data=arrayGHGdf, aes(arrayGHGdf$FluxCO2_L_umolm2s)) + geom_histogram() + facet_grid( ~ `ChemDetect_0-1`)
  CDgraph2 <- ggplot(data=arrayGHGdf, aes(arrayGHGdf$FluxCO2_E_umolm2s)) + geom_histogram() + facet_grid( ~ `ChemDetect_0-1`)
  CDgraph3 <- ggplot(data=arrayGHGdf, aes(arrayGHGdf$FluxN2O_L_nmolm2s)) + geom_histogram() + facet_grid( ~ `ChemDetect_0-1`)
  CDgraph4 <- ggplot(data=arrayGHGdf, aes(arrayGHGdf$FluxN2O_E_nmolm2s)) + geom_histogram() + facet_grid( ~ `ChemDetect_0-1`)
  CDgraph5 <- ggplot(data=arrayGHGdf, aes(arrayGHGdf$FluxCH4_L_nmolm2s)) + geom_histogram() + facet_grid( ~ `ChemDetect_0-1`)
  CDgraph6 <- ggplot(data=arrayGHGdf, aes(arrayGHGdf$FluxCH4_E_nmolm2s)) + geom_histogram() + facet_grid( ~ `ChemDetect_0-1`)
  
  png(file = paste(pathsavefigs, "chemdetectdiagnoses.png", sep=""),width=20,height=15,units="in",res=400)
  grid.arrange(CDgraph1, CDgraph2, CDgraph3, CDgraph4, CDgraph5, CDgraph6, nrow = 3, ncol = 2)
  dev.off()
  
  # more graphs for chemdetect
  
  # 1) Group the ChemDetect = 1 by chamber and see if its one specific chamber that is throwing the signals.
  # 2) Group the ChemDetect = 1 by date and see if its a specific date range (or several specific date ranges) that we need to be worrying about.
  
  CDgraph7 <- ggplot(arrayGHGdf, aes(x=Date, y=`ChemDetect_0-1`)) + geom_point() + facet_wrap(~Chamber, nrow=3)
  
  CDgraph8 <- ggplot(arrayGHGdf, aes(x=Date, y=`ChemDetect_0-1`)) + geom_point() + facet_wrap(~Chamber, nrow=9)
  
  png(file = paste(pathsavefigs, "chemdetectdiagnoses2a.png", sep=""),width=12,height=6,units="in",res=400)
  CDgraph7
  dev.off()
  
  png(file = paste(pathsavefigs, "chemdetectdiagnoses2b.png", sep=""),width=3,height=10,units="in",res=400)
  CDgraph8
  dev.off()
  
  # 3) A scatter plot of ChemDetect vs. each gas species average concentration - Water, Ammonia, CH4, N2O,CO2 (which I assume you’ve output as part of your data file).
  
  CDgraph9a <- ggplot(arrayGHGdf, aes(x=MeanCO2_ppm, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph9b <- ggplot(arrayGHGdf, aes(x=MeanCH4_ppm, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph9c <- ggplot(arrayGHGdf, aes(x=MeanN2O_ppm, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph9d <- ggplot(arrayGHGdf, aes(x=MeanNH3_ppm, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph9e <- ggplot(arrayGHGdf, aes(x=MeanH2O_percent, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph9f <- ggplot(arrayGHGdf, aes(x=MeasurementDuration_s, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="right")
  
  png(file = paste(pathsavefigs, "chemdetectdiagnoses3.png", sep=""),width=12,height=6,units="in",res=400)
  grid.arrange(CDgraph9a, CDgraph9b, CDgraph9c, CDgraph9d, CDgraph9e, CDgraph9f, nrow = 2, ncol = 3)
  dev.off()
  
  # 4) A scatter plot of ChemDetect vs. the Error values for the linear fluxes for each gas
  
  CDgraph10a <- ggplot(arrayGHGdf, aes(x=e_FluxCO2_L_umolm2s, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph10b <- ggplot(arrayGHGdf, aes(x=e_FluxCH4_L_nmolm2s, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph10c <- ggplot(arrayGHGdf, aes(x=e_FluxN2O_L_nmolm2s, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph10d <- ggplot(arrayGHGdf, aes(x=e_FluxNH3_L_umolm2s, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="right")
  
  png(file = paste(pathsavefigs, "chemdetectdiagnoses4.png", sep=""),width=8,height=5,units="in",res=400)
  grid.arrange(CDgraph10a, CDgraph10b, CDgraph10c, CDgraph10d, nrow = 2, ncol = 2)
  dev.off()
  
}

## for now, remove the data that had chemdetect running more than some proportion of the time
# actually, let's only remove the N2O data from those time periods

# take out places where chemdetect is above the threshold
chemdetectcutoff <- 0.0001
# print info about removed
tmp <- arrayGHGdf$`ChemDetect_0-1`
tmp2 <- tmp[tmp <= chemdetectcutoff]
durationok <- length(tmp2)/length(tmp)
cat("You have removed flux measurements where ChemDetect_0-1 was flagged more than the threshold proportion (",chemdetectcutoff, "); this left ", durationok*100, "% of the samples remaining (", length(tmp2), " of ", length(tmp), ").", sep="")

# take out N2O fluxes where chemdetect is above the threshold
arrayGHGdf$FluxN2O_E_nmolm2s[arrayGHGdf$`ChemDetect_0-1` > chemdetectcutoff] <- NA
arrayGHGdf$FluxN2O_L_nmolm2s[arrayGHGdf$`ChemDetect_0-1` > chemdetectcutoff] <- NA
cat("Removing the N2O fluxes from the bad ChemDetect periods only.", sep="")
# if taking out all fluxes:
# arrayGHGdf <- subset(arrayGHGdf, `ChemDetect_0-1` <= chemdetectcutoff)


########################################################################
# EXPONENTIAL VS. LINEAR FLUX ESTIMATE DECISION

## which flux gets preferred, linear or exponential, for each observation and gas?

# define ratio of lin flux:lin error and expon flux:expon error for each gas
arrayGHGdf$CO2errratio_L <- (arrayGHGdf$e_FluxCO2_L_umolm2s)/(arrayGHGdf$FluxCO2_L_umolm2s)
arrayGHGdf$CO2errratio_E <- (arrayGHGdf$e_FluxCO2_E_umolm2s)/(arrayGHGdf$FluxCO2_E_umolm2s)
arrayGHGdf$CH4errratio_L <- (arrayGHGdf$e_FluxCH4_L_nmolm2s)/(arrayGHGdf$FluxCH4_L_nmolm2s)
arrayGHGdf$CH4errratio_E <- (arrayGHGdf$e_FluxCH4_E_nmolm2s)/(arrayGHGdf$FluxCH4_E_nmolm2s)
arrayGHGdf$N2Oerrratio_L <- (arrayGHGdf$e_FluxN2O_L_nmolm2s)/(arrayGHGdf$FluxN2O_L_nmolm2s)
arrayGHGdf$e_FluxN2O_E_nmolm2s <- as.numeric(arrayGHGdf$e_FluxN2O_E_nmolm2s)
arrayGHGdf$N2Oerrratio_E <- (arrayGHGdf$e_FluxN2O_E_nmolm2s)/(arrayGHGdf$FluxN2O_E_nmolm2s)

# assign the flux with the better ratio to a "flux to use" column, another column that says whether that was the linear or the exponential flux, and another column with the error ratio

# CO2 fluxuse and fluxusetype
arrayGHGdf <- transform(arrayGHGdf, CO2_umolm2s_fluxuse = ifelse(is.na(FluxCO2_L_umolm2s), as.numeric(FluxCO2_E_umolm2s), ifelse(is.na(FluxCO2_E_umolm2s), as.numeric(FluxCO2_L_umolm2s), ifelse(CO2errratio_L>CO2errratio_E, as.numeric(FluxCO2_E_umolm2s), as.numeric(FluxCO2_L_umolm2s)))))

arrayGHGdf <- transform(arrayGHGdf, CO2_umolm2s_fluxusetype = ifelse(is.na(FluxCO2_L_umolm2s), as.character("exponential"), ifelse(is.na(FluxCO2_E_umolm2s), as.character("linear"), ifelse(CO2errratio_L>CO2errratio_E, as.character("exponential"), as.character("linear")))))

arrayGHGdf <- transform(arrayGHGdf, CO2_umolm2s_fluxuseerror = ifelse(is.na(FluxCO2_L_umolm2s), as.numeric(CO2errratio_E), ifelse(is.na(FluxCO2_E_umolm2s), as.numeric(CO2errratio_L), ifelse(CO2errratio_L>CO2errratio_E, as.numeric(CO2errratio_E), as.numeric(CO2errratio_L)))))

# print info about what fluxes made the final dataset
linsum <- sum(arrayGHGdf$CO2_umolm2s_fluxusetype=="linear")
expsum <- sum(arrayGHGdf$CO2_umolm2s_fluxusetype=="exponential")
totalnum <- length(arrayGHGdf$CO2_umolm2s_fluxusetype)
cat("Of the ", totalnum, " final CO2 fluxes, ",
    linsum, " fluxes were best estimated with a linear fit and ", expsum,
    " were best estimated with an exponential fit. The mean",
    " error ratio of the fluxes used was ", 
    mean(is.finite(abs(arrayGHGdf$CO2_umolm2s_fluxuseerror))), ".", sep="")

# what is a reasonable cut off for error?
tmp <- sort(arrayGHGdf$CO2_umolm2s_fluxuseerror, decreasing = T)
tmp[1:50]

# if error is over a certain ratio, cut that flux
# moreover, if the CO2 error is bad, then cut all the gases
errorcutoff <- 0.10
# print info about removed
tmp <- arrayGHGdf$CO2_umolm2s_fluxuseerror
tmp2 <- tmp[tmp <= errorcutoff]
durationok <- length(tmp2)/length(tmp)
cat("You have removed flux measurements where the CO2 error ratio was above the threshold proportion (",errorcutoff, "); this left ", durationok*100, "% of the samples remaining (", length(tmp2), " of ", length(tmp), ").", sep="")
# take out all fluxes above the threshold
arrayGHGdf <- subset(arrayGHGdf, CO2_umolm2s_fluxuseerror <= errorcutoff)

# if you want to see how the error is distributed:
ggplot(arrayGHGdf, aes(x=DateTime2, y=CO2_umolm2s_fluxuse, color=CO2_umolm2s_fluxuseerror)) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"))

# CH4 fluxuse and fluxusetype
arrayGHGdf <- transform(arrayGHGdf, CH4_nmolm2s_fluxuse = ifelse(is.na(FluxCH4_L_nmolm2s), as.numeric(FluxCH4_E_nmolm2s), ifelse(is.na(FluxCH4_E_nmolm2s), as.numeric(FluxCH4_L_nmolm2s), ifelse(CH4errratio_L>CH4errratio_E, as.numeric(FluxCH4_E_nmolm2s), as.numeric(FluxCH4_L_nmolm2s)))))

arrayGHGdf <- transform(arrayGHGdf, CH4_nmolm2s_fluxusetype = ifelse(is.na(FluxCH4_L_nmolm2s), as.character("exponential"), ifelse(is.na(FluxCH4_E_nmolm2s), as.character("linear"), ifelse(CH4errratio_L>CH4errratio_E, as.character("exponential"), as.character("linear")))))

arrayGHGdf <- transform(arrayGHGdf, CH4_nmolm2s_fluxuseerror = ifelse(is.na(FluxCH4_L_nmolm2s), as.numeric(CH4errratio_E), ifelse(is.na(FluxCH4_E_nmolm2s), as.numeric(CH4errratio_L), ifelse(CH4errratio_L>CH4errratio_E, as.numeric(CH4errratio_E), as.numeric(CH4errratio_L)))))

# print info about what fluxes made the final dataset
linsum <- sum(arrayGHGdf$CH4_nmolm2s_fluxusetype=="linear")
expsum <- sum(arrayGHGdf$CH4_nmolm2s_fluxusetype=="exponential")
totalnum <- length(arrayGHGdf$CH4_nmolm2s_fluxusetype)
cat("Of the ", totalnum, " final CH4 fluxes, ",
    linsum, " fluxes were best estimated with a linear fit and ", expsum,
    " were best estimated with an exponential fit. The mean",
    " error ratio of the fluxes used was ", 
    mean(is.finite(abs(arrayGHGdf$CH4_nmolm2s_fluxuseerror))), ".", sep="")

# what is a reasonable cut off for error?
tmp <- sort(abs(arrayGHGdf$CH4_nmolm2s_fluxuseerror), decreasing = T)
tmp[1000:1050]
ggplot(arrayGHGdf, aes(x=DateTime2, y=CH4_nmolm2s_fluxuseerror, color=CH4_nmolm2s_fluxuse)) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d")) + ylim(-100000, 1000)

ggplot(arrayGHGdf, aes(x=CH4_nmolm2s_fluxuse, y=abs(CH4_nmolm2s_fluxuseerror), color=Topo)) + geom_point() + theme_bw() + ylim(-10,200) + xlim(-30,50)

ggplot(arrayGHGdf, aes(x=DateTime2, y=CH4_nmolm2s_fluxuse, color=abs(CH4_nmolm2s_fluxuseerror))) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d")) + scale_colour_gradient(limits=c(0, 100), low="red", high="black")

# if error is over a certain ratio, cut that flux
# don't cut the other gases just because CH4 is off
errorcutoff <- 50000
# print info about removed
tmp <- abs(arrayGHGdf$CH4_nmolm2s_fluxuseerror)
tmp2 <- tmp[tmp <= errorcutoff]
durationok <- length(tmp2)/length(tmp)
cat("You have removed flux measurements where the CH4 error ratio was above the threshold proportion (",errorcutoff, "); this left ", durationok*100, "% of the samples remaining (", length(tmp2), " of ", length(tmp), ").", sep="")
# take out any CH4 fluxes with error above the threshold
arrayGHGdf$CH4_nmolm2s_fluxuse[abs(arrayGHGdf$CH4_nmolm2s_fluxuseerror) > errorcutoff] <- NA
arrayGHGdf$CH4_nmolm2s_fluxusetype[abs(arrayGHGdf$CH4_nmolm2s_fluxuseerror) > errorcutoff] <- NA

# N2O fluxuse and fluxusetype
arrayGHGdf <- transform(arrayGHGdf, N2O_nmolm2s_fluxuse = ifelse(is.na(FluxN2O_L_nmolm2s), as.numeric(FluxN2O_E_nmolm2s), ifelse(is.na(FluxN2O_E_nmolm2s), as.numeric(FluxN2O_L_nmolm2s), ifelse(N2Oerrratio_L>N2Oerrratio_E, as.numeric(FluxN2O_E_nmolm2s), as.numeric(FluxN2O_L_nmolm2s)))))

arrayGHGdf <- transform(arrayGHGdf, N2O_nmolm2s_fluxusetype = ifelse(is.na(FluxN2O_L_nmolm2s), as.character("exponential"), ifelse(is.na(FluxN2O_E_nmolm2s), as.character("linear"), ifelse(N2Oerrratio_L>N2Oerrratio_E, as.character("exponential"), as.character("linear")))))

arrayGHGdf <- transform(arrayGHGdf, N2O_nmolm2s_fluxuseerror = ifelse(is.na(FluxN2O_L_nmolm2s), as.numeric(N2Oerrratio_E), ifelse(is.na(FluxN2O_E_nmolm2s), as.numeric(N2Oerrratio_L), ifelse(N2Oerrratio_L>N2Oerrratio_E, as.numeric(N2Oerrratio_E), as.numeric(N2Oerrratio_L)))))

# print info about what fluxes made the final dataset
linsum <- sum(arrayGHGdf$N2O_nmolm2s_fluxusetype=="linear")
expsum <- sum(arrayGHGdf$N2O_nmolm2s_fluxusetype=="exponential")
totalnum <- length(arrayGHGdf$N2O_nmolm2s_fluxusetype)
cat("Of the ", totalnum, " final N2O fluxes, ",
    linsum, " fluxes were best estimated with a linear fit and ", expsum,
    " were best estimated with an exponential fit. The mean",
    " error ratio of the fluxes used was ", 
    mean(is.finite(abs(arrayGHGdf$N2O_nmolm2s_fluxuseerror))), ".", sep="")

# what is a reasonable cut off for error?
tmp <- sort(abs(arrayGHGdf$N2O_nmolm2s_fluxuseerror), decreasing = T)
tmp[1:50]
ggplot(arrayGHGdf, aes(x=DateTime2, y=N2O_nmolm2s_fluxuseerror, color=N2O_nmolm2s_fluxuse)) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d")) + ylim(-50000, 1000)

# if error is over a certain ratio, cut that flux
# don't cut the other gases just because N2O is off
errorcutoff <- 1.0
# print info about removed
tmp <- abs(arrayGHGdf$N2O_nmolm2s_fluxuseerror)
tmp2 <- tmp[tmp <= errorcutoff]
durationok <- length(tmp2)/length(tmp)
cat("You have removed flux measurements where the N2O error ratio was above the threshold proportion (",errorcutoff, "); this left ", durationok*100, "% of the samples remaining (", length(tmp2), " of ", length(tmp), ").", sep="")
# take out any N2O fluxes with error above the threshold
arrayGHGdf$N2O_nmolm2s_fluxuse[abs(arrayGHGdf$N2O_nmolm2s_fluxuseerror) > errorcutoff] <- NA
arrayGHGdf$N2O_nmolm2s_fluxusetype[abs(arrayGHGdf$N2O_nmolm2s_fluxuseerror) > errorcutoff] <- NA

# mean(arrayGHGdf$CO2errratio_L, na.rm=T)
# mean(arrayGHGdf$CO2errratio_E, na.rm=T)
# mean(arrayGHGdf$CH4errratio_L, na.rm=T)
# mean(arrayGHGdf$CH4errratio_E, na.rm=T)
# mean(arrayGHGdf$N2Oerrratio_L, na.rm=T)
# mean(is.finite(arrayGHGdf$N2Oerrratio_E), na.rm=T)

# these are the average error values before any high error ratio fluxes got thrown out
mean(abs(arrayGHGdf$CO2_umolm2s_fluxuseerror), na.rm=T)
mean(abs(arrayGHGdf$CH4_nmolm2s_fluxuseerror), na.rm=T)
mean(abs(arrayGHGdf$N2O_nmolm2s_fluxuseerror), na.rm=T)


########################################################################
# SAVE DATASET FOR KANA

write.csv(arrayGHGdf, file=paste(pathdata, "arrayGHGdf_04202017.csv", sep = ""), row.names=FALSE)







########################################################################
# CUT MAYES FLUXES FOR NOW?

# save arrayGHGdf as new variable
#arrayGHGdf_SilverMayes <- arrayGHGdf

# now paper over arrayGHGdf without Mayes fluxes
#arrayGHGdf <- subset(arrayGHGdf, arrayGHGdf$System=="Silver")


########################################################################
# FLUX DISTRIBUTIONS

# let there be nice names
#arrayGHGdf$ChamberNameSilver <- arrayGHGdf$Chamber
#levels(arrayGHGdf$ChamberNameSilver) <- c("1 (Ridge)" , "2 (Slope)" , "3 (Valley)" , "4 (Ridge)" , "5 (Slope)" , "6 (Valley)" , "7 (Ridge)" , "8 (Slope)" , "9 (Valley)", "skip", "skip2", "skip3")

# order levels
arrayGHGdf$Topo4Cats <- factor(arrayGHGdf$Topo4Cats, levels = c("Ridge", "Upper Slope", "Low Slope", "Valley"))

## CO2

# Histogram overlaid with kernel density curve, fluxes by chamber
histCO2a <- ggplot(arrayGHGdf, aes(x=CO2_umolm2s_fluxuse)) + geom_histogram(aes(y=..density..), binwidth=0.5, colour="black", fill="white") + theme_bw() + facet_wrap( ~ Topo4Cats, ncol = 4) + ylab("Density Distribution") + xlab("CO2 Flux (umol/m^2/s)") #+ theme_set(theme_bw(base_size = 36)) # + geom_density(alpha=.2, fill="#FF6666")

png(file = paste(pathsavefigs, "CO2_densityplots.png", sep=""),width=10,height=4,units="in",res=400)
histCO2a
dev.off()

# Histogram overlaid with kernel density curve, all fluxes in one histogram
histCO2b <- ggplot(arrayGHGdf, aes(x=CO2_umolm2s_fluxuse)) + geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") + geom_density(alpha=.2, fill="#FF6666") #+ xlim(c(-0.5,0.75)) 
# histogram with density instead of count on y-axis
# overlay with transparent density plot
# long tail isn't a problem in the combo histogram

png(file = paste(pathsavefigs, "CO2_densityplots_combo.png", sep=""),width=7,height=7,units="in",res=400)
histCO2b
dev.off()

# Histogram overlaid with kernel density curve, faceting by topo location
histCO2c <- ggplot(arrayGHGdf, aes(x=CO2_umolm2s_fluxuse)) + geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") + facet_wrap(System ~ Topo4Cats, ncol = 4) + geom_density(alpha=.2, fill="#FF6666") #+ xlim(c(-0.5,0.75)) 
# histogram with density instead of count on y-axis
# overlay with transparent density plot
# long tail isn't too bad for visualization

png(file = paste(pathsavefigs, "CO2_densityplots_topo.png", sep=""),width=20,height=12,units="in",res=400)
histCO2c
dev.off()

## CH4

# Histogram overlaid with kernel density curve, fluxes by chamber
histCH4a <- ggplot(arrayGHGdf, aes(x=CH4_nmolm2s_fluxuse)) + geom_histogram(aes(y=..density..), binwidth=2, colour="black", fill="white") + theme_bw() + facet_wrap( ~ Topo4Cats, ncol = 4) + ylab("Density Distribution") + xlab("CH4 Flux (nmol/m^2/s)") + xlim(c(-25,50)) + ylim(c(0,0.35)) #+ theme_set(theme_bw(base_size = 36))  #+ geom_density(alpha=.2, fill="#FF6666")
# histogram with density instead of count on y-axis
# overlay with transparent density plot
# without limiting the axes the long tail made things too hard to read

png(file = paste(pathsavefigs, "CH4_densityplots.png", sep=""),width=10,height=4,units="in",res=400)
histCH4a
dev.off()

# Histogram overlaid with kernel density curve, all fluxes in one histogram
histCH4b <- ggplot(arrayGHGdf, aes(x=CH4_nmolm2s_fluxuse)) + geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white") + geom_density(alpha=.2, fill="#FF6666") + xlim(c(-25,50)) 
# histogram with density instead of count on y-axis
# overlay with transparent density plot
# long tail isn't a problem in the combo histogram

png(file = paste(pathsavefigs, "CH4_densityplots_combo.png", sep=""),width=7,height=7,units="in",res=400)
histCH4b
dev.off()

# Histogram overlaid with kernel density curve, faceting by topo location
histCH4c <- ggplot(arrayGHGdf, aes(x=CH4_nmolm2s_fluxuse)) + geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white") + facet_wrap(System ~ Topo4Cats, ncol = 4) + geom_density(alpha=.2, fill="#FF6666") + xlim(c(-25,50))
# histogram with density instead of count on y-axis
# overlay with transparent density plot
# without limiting the axes the long tail made things too hard to read

png(file = paste(pathsavefigs, "CH4_densityplots_topo.png", sep=""),width=20,height=12,units="in",res=400)
histCH4c
dev.off()

## N2O

# Histogram overlaid with kernel density curve, fluxes by chamber
histN2Oa <- ggplot(arrayGHGdf, aes(x=N2O_nmolm2s_fluxuse)) + geom_histogram(aes(y=..density..), binwidth=.05, colour="black", fill="white") + theme_bw() + facet_wrap( ~ Topo4Cats, ncol = 4) + ylab("Density Distribution") + xlab("N2O Flux (nmol/m^2/s)") + xlim(c(-0.25,0.75)) #+ theme(axis.text.x = element_text(size=18,angle=45)) #+ theme_set(theme_bw(base_size = 36))

#+ ylim(c(0,0.35))#+ geom_density(alpha=.2, fill="#FF6666") + xlim(c(-0.5,0.75)) 
# histogram with density instead of count on y-axis
# overlay with transparent density plot
# without limiting the axes the long tail made things too hard to read

png(file = paste(pathsavefigs, "N2O_densityplots.png", sep=""),width=10,height=4,units="in",res=400)
histN2Oa
dev.off()

# Histogram overlaid with kernel density curve, all fluxes in one histogram
histN2Ob <- ggplot(arrayGHGdf, aes(x=N2O_nmolm2s_fluxuse)) + geom_histogram(aes(y=..density..), binwidth=.05, colour="black", fill="white") + geom_density(alpha=.2, fill="#FF6666") #+ xlim(c(-0.5,0.75)) 
# histogram with density instead of count on y-axis
# overlay with transparent density plot
# long tail isn't a problem in the combo histogram

png(file = paste(pathsavefigs, "N2O_densityplots_combo.png", sep=""),width=7,height=7,units="in",res=400)
histN2Ob
dev.off()

# Histogram overlaid with kernel density curve, faceting by topo location
histN2Oc <- ggplot(arrayGHGdf, aes(x=N2O_nmolm2s_fluxuse)) + geom_histogram(aes(y=..density..), binwidth=.05, colour="black", fill="white") + facet_wrap(System ~ Topo4Cats, ncol = 4) + geom_density(alpha=.2, fill="#FF6666") + xlim(c(-0.5,0.75)) 
# histogram with density instead of count on y-axis
# overlay with transparent density plot
# without limiting the axes the long tail made things too hard to read

png(file = paste(pathsavefigs, "N2O_densityplots_topo.png", sep=""),width=20,height=12,units="in",res=400)
histN2Oc
dev.off()


# 
# ### side thing, to send JP the most represented CH4 flux values (see email chain from Jan 2017)
# # get CH4 values for JP
# # Histogram overlaid with kernel density curve, N2O fluxes no chamber faceting
# JPCH4graph <- ggplot(arrayGHGdf, aes(x=CH4_nmolm2s_fluxuse)) + 
#   geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
#                  binwidth=.5,
#                  colour="black", fill="white") +
#   #facet_wrap(System ~ Topo, ncol = 3) +
#   geom_density(alpha=.2, fill="#FF6666") + #+ # Overlay with transparent density plot
#   xlim(c(-5,10)) #+ # without limiting the axes the long tail made things too hard to read
# #ylim(c(0,20)) + # without limiting the axes the long tail made things too hard to read
# # note that this cuts off the high values further right than 10, but ok for her purposes
# # (info for buying an instrument)


########################################################################
# DEFINE OUTLIER FLUXES = HOT SPOTS/MOMENTS

# see http://r-statistics.co/Outlier-Treatment-With-R.html
# below I'm currently using a very simple definition of an outlier (3 * SD), but go back and switch this to the method using the scores() function from the cars package
# just kidding - I tried the scores() function and it doesn't like having NAs in the dataset

# instead, eventually convert this chunk of stuff to a function, a la:
# https://datascienceplus.com/identify-describe-plot-and-removing-the-outliers-from-the-dataset/

outfun <- function(x) {
  abs(x-mean(x,na.rm=TRUE)) > 2*sd(x,na.rm=TRUE)
}

# test if function works
outfun(c(200,rnorm(10)))

# use function for all three GHGs
arrayGHGdf$CO2_umolm2s_fluxuse_outlier <- outfun(arrayGHGdf$CO2_umolm2s_fluxuse)
arrayGHGdf$CH4_nmolm2s_fluxuse_outlier <- outfun(arrayGHGdf$CH4_nmolm2s_fluxuse)
arrayGHGdf$N2O_nmolm2s_fluxuse_outlier <- outfun(arrayGHGdf$N2O_nmolm2s_fluxuse)

# info about outliers

# CO2
outliers_tmp <- sum(arrayGHGdf$CO2_umolm2s_fluxuse_outlier, na.rm = T)
obs_tmp <- length(arrayGHGdf$CO2_umolm2s_fluxuse) - sum(is.na(arrayGHGdf$CO2_umolm2s_fluxuse))
mean_out <- mean(arrayGHGdf$CO2_umolm2s_fluxuse[arrayGHGdf$CO2_umolm2s_fluxuse_outlier==TRUE], na.rm=T)
mean_notout <- mean(arrayGHGdf$CO2_umolm2s_fluxuse[arrayGHGdf$CO2_umolm2s_fluxuse_outlier==FALSE], na.rm=T)
mean_all <- mean(arrayGHGdf$CO2_umolm2s_fluxuse, na.rm=T)

cat("Outliers identified:", outliers_tmp, "\n")
cat("Fluxes considered:", obs_tmp, "\n")
cat("Proportion (%) of outliers:", 100*((outliers_tmp) / obs_tmp), "\n")
cat("Mean flux of the outliers:", mean_out, "\n")
cat("Mean flux without removing outliers:", mean_all, "\n")
cat("Mean flux if we remove outliers:", mean_notout, "\n")

png(file = paste(pathsavefigs, "CO2_outliercheck.png", sep=""),width=10,height=8,units="in",res=400)
par(mfrow=c(2, 3), oma=c(0,0,3,0))
boxplot(arrayGHGdf$CO2_umolm2s_fluxuse, main="With outliers")
boxplot(arrayGHGdf$CO2_umolm2s_fluxuse[arrayGHGdf$CO2_umolm2s_fluxuse_outlier==FALSE], main="Without outliers")
boxplot(arrayGHGdf$CO2_umolm2s_fluxuse[arrayGHGdf$CO2_umolm2s_fluxuse_outlier==TRUE], main="Only outliers")
hist(arrayGHGdf$CO2_umolm2s_fluxuse, main="With outliers", xlab=NA, ylab=NA)
hist(arrayGHGdf$CO2_umolm2s_fluxuse[arrayGHGdf$CO2_umolm2s_fluxuse_outlier==FALSE], main="Without outliers", xlab=NA, ylab=NA)
hist(arrayGHGdf$CO2_umolm2s_fluxuse[arrayGHGdf$CO2_umolm2s_fluxuse_outlier==TRUE], main="Only outliers", xlab=NA, ylab=NA)
title("Outlier Check, CO2", outer=TRUE)
dev.off()

# show boxplot of fluxes by topo
p <- ggplot(arrayGHGdf, aes(x=Topo4Cats, y=CO2_umolm2s_fluxuse)) + geom_boxplot() + theme_bw() + ylab("CO2 Flux (umol/m^2/s)") + xlab("")

# bargraph with mean and sd
tmp <- summarySE(arrayGHGdf, measurevar="CO2_umolm2s_fluxuse", groupvars=c("Topo4Cats"))
barCO2a <- ggplot(tmp, aes(x=Topo4Cats, y=CO2_umolm2s_fluxuse)) +geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=CO2_umolm2s_fluxuse-se, ymax=CO2_umolm2s_fluxuse+se), width=.2, position=position_dodge(.9)) + theme_bw() + ylab("CO2 Flux (umol/m^2/s) \n Mean +/- SE") + xlab("")

png(file = paste(pathsavefigs, "CO2_barplot.png", sep=""),width=5,height=4,units="in",res=400)
barCO2a
dev.off()

png(file = paste(pathsavefigs, "CO2_boxplot.png", sep=""),width=5,height=4,units="in",res=400)
p
dev.off()


# CH4
outliers_tmp <- sum(arrayGHGdf$CH4_nmolm2s_fluxuse_outlier, na.rm = T)
obs_tmp <- length(arrayGHGdf$CH4_nmolm2s_fluxuse) - sum(is.na(arrayGHGdf$CH4_nmolm2s_fluxuse))
mean_out <- mean(arrayGHGdf$CH4_nmolm2s_fluxuse[arrayGHGdf$CH4_nmolm2s_fluxuse_outlier==TRUE], na.rm=T)
mean_notout <- mean(arrayGHGdf$CH4_nmolm2s_fluxuse[arrayGHGdf$CH4_nmolm2s_fluxuse_outlier==FALSE], na.rm=T)
mean_all <- mean(arrayGHGdf$CH4_nmolm2s_fluxuse, na.rm=T)

cat("Outliers identified:", outliers_tmp, "\n")
cat("Fluxes considered:", obs_tmp, "\n")
cat("Proportion (%) of outliers:", 100*((outliers_tmp) / obs_tmp), "\n")
cat("Mean flux of the outliers:", mean_out, "\n")
cat("Mean flux without removing outliers:", mean_all, "\n")
cat("Mean flux if we remove outliers:", mean_notout, "\n")

png(file = paste(pathsavefigs, "CH4_outliercheck.png", sep=""),width=10,height=8,units="in",res=400)
par(mfrow=c(2, 3), oma=c(0,0,3,0))
boxplot(arrayGHGdf$CH4_nmolm2s_fluxuse, main="With outliers")
boxplot(arrayGHGdf$CH4_nmolm2s_fluxuse[arrayGHGdf$CH4_nmolm2s_fluxuse_outlier==FALSE], main="Without outliers")
boxplot(arrayGHGdf$CH4_nmolm2s_fluxuse[arrayGHGdf$CH4_nmolm2s_fluxuse_outlier==TRUE], main="Only outliers")
hist(arrayGHGdf$CH4_nmolm2s_fluxuse, main="With outliers", xlab=NA, ylab=NA)
hist(arrayGHGdf$CH4_nmolm2s_fluxuse[arrayGHGdf$CH4_nmolm2s_fluxuse_outlier==FALSE], main="Without outliers", xlab=NA, ylab=NA)
hist(arrayGHGdf$CH4_nmolm2s_fluxuse[arrayGHGdf$CH4_nmolm2s_fluxuse_outlier==TRUE], main="Only outliers", xlab=NA, ylab=NA)
title("Outlier Check, CH4", outer=TRUE)
dev.off()

# show boxplot of fluxes by topo
p <- ggplot(arrayGHGdf, aes(x=Topo4Cats, y=CH4_nmolm2s_fluxuse)) + geom_boxplot() + theme_bw() + ylab("CH4 Flux (nmol/m^2/s)") + xlab("")

# bargraph with mean and sd
tmp <- summarySE(arrayGHGdf, measurevar="CH4_nmolm2s_fluxuse", groupvars=c("Topo4Cats"), na.rm=T)
barCH4a <- ggplot(tmp, aes(x=Topo4Cats, y=CH4_nmolm2s_fluxuse)) +geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=CH4_nmolm2s_fluxuse-se, ymax=CH4_nmolm2s_fluxuse+se), width=.2, position=position_dodge(.9)) + theme_bw() + ylab("CH4 Flux (nmol/m^2/s) \n Mean +/- SE") + xlab("")

png(file = paste(pathsavefigs, "CH4_barplot.png", sep=""),width=5,height=4,units="in",res=400)
barCH4a
dev.off()

png(file = paste(pathsavefigs, "CH4_boxplot.png", sep=""),width=5,height=4,units="in",res=400)
p
dev.off()


# N2O
outliers_tmp <- sum(arrayGHGdf$N2O_nmolm2s_fluxuse_outlier, na.rm = T)
obs_tmp <- length(arrayGHGdf$N2O_nmolm2s_fluxuse) - sum(is.na(arrayGHGdf$N2O_nmolm2s_fluxuse))
mean_out <- mean(arrayGHGdf$N2O_nmolm2s_fluxuse[arrayGHGdf$N2O_nmolm2s_fluxuse_outlier==TRUE], na.rm=T)
mean_notout <- mean(arrayGHGdf$N2O_nmolm2s_fluxuse[arrayGHGdf$N2O_nmolm2s_fluxuse_outlier==FALSE], na.rm=T)
mean_all <- mean(arrayGHGdf$N2O_nmolm2s_fluxuse, na.rm=T)

cat("Outliers identified:", outliers_tmp, "\n")
cat("Fluxes considered:", obs_tmp, "\n")
cat("Proportion (%) of outliers:", 100*((outliers_tmp) / obs_tmp), "\n")
cat("Mean flux of the outliers:", mean_out, "\n")
cat("Mean flux without removing outliers:", mean_all, "\n")
cat("Mean flux if we remove outliers:", mean_notout, "\n")

png(file = paste(pathsavefigs, "N2O_outliercheck.png", sep=""),width=10,height=8,units="in",res=400)
par(mfrow=c(2, 3), oma=c(0,0,3,0))
boxplot(arrayGHGdf$N2O_nmolm2s_fluxuse, main="With outliers")
boxplot(arrayGHGdf$N2O_nmolm2s_fluxuse[arrayGHGdf$N2O_nmolm2s_fluxuse_outlier==FALSE], main="Without outliers")
boxplot(arrayGHGdf$N2O_nmolm2s_fluxuse[arrayGHGdf$N2O_nmolm2s_fluxuse_outlier==TRUE], main="Only outliers")
hist(arrayGHGdf$N2O_nmolm2s_fluxuse, main="With outliers", xlab=NA, ylab=NA)
hist(arrayGHGdf$N2O_nmolm2s_fluxuse[arrayGHGdf$N2O_nmolm2s_fluxuse_outlier==FALSE], main="Without outliers", xlab=NA, ylab=NA)
hist(arrayGHGdf$N2O_nmolm2s_fluxuse[arrayGHGdf$N2O_nmolm2s_fluxuse_outlier==TRUE], main="Only outliers", xlab=NA, ylab=NA)
title("Outlier Check, N2O", outer=TRUE)
dev.off()


# show boxplot of fluxes by topo
p <- ggplot(arrayGHGdf, aes(x=Topo4Cats, y=N2O_nmolm2s_fluxuse)) + geom_boxplot() + theme_bw() + ylab("N2O Flux (nmol/m^2/s)") + xlab("")

# bargraph with mean and sd
tmp <- summarySE(arrayGHGdf, measurevar="N2O_nmolm2s_fluxuse", groupvars=c("Topo4Cats"), na.rm=T)
barN2Oa <- ggplot(tmp, aes(x=Topo4Cats, y=N2O_nmolm2s_fluxuse)) +geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=N2O_nmolm2s_fluxuse-se, ymax=N2O_nmolm2s_fluxuse+se), width=.2, position=position_dodge(.9)) + theme_bw() + ylab("N2O Flux (nmol/m^2/s) \n Mean +/- SE") + xlab("")

png(file = paste(pathsavefigs, "N2O_barplot.png", sep=""),width=5,height=4,units="in",res=400)
barN2Oa
dev.off()

png(file = paste(pathsavefigs, "N2O_boxplot.png", sep=""),width=5,height=4,units="in",res=400)
p
dev.off()


########################################################################
# BRING IN SOIL VARIABLE DATA

O2hourly <- as.data.frame(read.csv(paste(soildatapath,"O2hourly",".csv",sep=""), stringsAsFactors=FALSE))
vwchourly <- as.data.frame(read.csv(paste(soildatapath,"vwchourly",".csv",sep=""), stringsAsFactors=FALSE))
temphourly <- as.data.frame(read.csv(paste(soildatapath,"temphourly",".csv",sep=""), stringsAsFactors=FALSE))

# what sensor goes with what chamber?
# see Ryan's email (subject line "wiring diagrams")
# I also looked at the chamber and sensor locations:
# Chamber 1 - Sensor 1 (almost equally close to 8)
# Chamber 2 - Sensor 2
# Chamber 3 - Sensor 14
# Chamber 4 - Sensor 22
# Chamber 5 - Sensor 17 (almost equally close to 24)
# Chamber 6 - Sensor 21
# Chamber 7 - Sensor 29
# Chamber 8 - Sensor 34
# Chamber 9 - Sensor 28

# bring in calibration curve info
chambersensormatch <- read.csv(paste(pathsensorchambermatch,"chambersensormatch.csv",sep=""), stringsAsFactors=FALSE)
chambersensormatch$Chamber <- as.factor(chambersensormatch$Chamber)

# join to arrayGHGdf
arrayGHGdf <- full_join(arrayGHGdf, chambersensormatch)

# get O2hourly ready
O2hourly2 <- subset(O2hourly, select=c(TIMESTAMP2,SensorID,O2pct))
O2hourly2$TIMESTAMP2 <- ymd_hms(O2hourly2$TIMESTAMP2)

# have same var names
arrayGHGdf$DateTimeMatch <- arrayGHGdf$DateTime2
O2hourly2$DateTimeMatch <- O2hourly2$TIMESTAMP2

# any arrayGHGdf with NA for time should go
arrayGHGdf <- arrayGHGdf[!is.na(arrayGHGdf$DateTime2),]

z <- lapply(intersect(arrayGHGdf$SensorID,O2hourly2$SensorID),function(id) {
  d1 <- subset(arrayGHGdf,SensorID==id)
  d2 <- subset(O2hourly2,SensorID==id)
  
  d1$indices <- sapply(d1$DateTimeMatch,function(d) which.min(abs(d2$DateTimeMatch - d)))
  d2$indices <- 1:nrow(d2)
  
  merge(d1,d2,by=c('SensorID','indices'))
})

z2 <- do.call(rbind,z)
z2$indices <- NULL

# how far apart are the dates?
z2$DateTimeMatchDiff <- z2$DateTimeMatch.x - z2$DateTimeMatch.y
# this diff is in seconds; convert to hours
z2$DateTimeMatchDiff_hrs <- z2$DateTimeMatchDiff/60/60
# how well matched up are the sensor vs. flux times?
hist(as.numeric(z2$DateTimeMatchDiff_hrs))
hist(as.numeric(z2$DateTimeMatchDiff))
# get rid of rows where the times aren't well matched
z3 <- subset(z2, abs(z2$DateTimeMatchDiff_hrs) < 48)

dim(z3)

### NEED TO DO THE SAME FOR SOIL MOISTURE AND TEMP

# rename and save key dataset
arrayGHGdfsensors <- z3

# save
write.csv(arrayGHGdfsensors, file=paste(pathsavefiles, "arrayGHGdfsensors.csv", sep = ""), row.names=FALSE)


########################################################################
# FLUX CORRELATION WITH SOIL VARIABLES

corr1 <- ggplot(arrayGHGdfsensors, aes(x=O2pct, y=CO2_umolm2s_fluxuse, color=CO2_umolm2s_fluxuse_outlier)) + geom_point() + theme_bw() + theme(legend.position="bottom")

corr2 <- ggplot(arrayGHGdfsensors, aes(x=O2pct, y=CH4_nmolm2s_fluxuse, color=CH4_nmolm2s_fluxuse_outlier)) + geom_point() + theme_bw() + theme(legend.position="bottom")

corr3 <- ggplot(arrayGHGdfsensors, aes(x=O2pct, y=N2O_nmolm2s_fluxuse, color=N2O_nmolm2s_fluxuse_outlier)) + geom_point() + theme_bw() + theme(legend.position="bottom")

png(file = paste(pathsavefigs, "CO2_flux_sensor_corr.png", sep=""),width=5,height=5,units="in",res=400)
corr1
dev.off()

png(file = paste(pathsavefigs, "CH4_flux_sensor_corr.png", sep=""),width=5,height=5,units="in",res=400)
corr2
dev.off()

png(file = paste(pathsavefigs, "N2O_flux_sensor_corr.png", sep=""),width=5,height=5,units="in",res=400)
corr3
dev.off()

corr1b <- ggplot(arrayGHGdfsensors, aes(x=O2pct, y=CO2_umolm2s_fluxuse, color=CO2_umolm2s_fluxuse_outlier)) + geom_point() + theme_bw() + theme(legend.position="bottom") + facet_wrap( ~ Topo, ncol = 3)

corr2b <- ggplot(arrayGHGdfsensors, aes(x=O2pct, y=CH4_nmolm2s_fluxuse, color=CH4_nmolm2s_fluxuse_outlier)) + geom_point() + theme_bw() + theme(legend.position="bottom") + facet_wrap( ~ Topo, ncol = 3)

corr3b <- ggplot(arrayGHGdfsensors, aes(x=O2pct, y=N2O_nmolm2s_fluxuse, color=N2O_nmolm2s_fluxuse_outlier)) + geom_point() + theme_bw() + theme(legend.position="bottom") + facet_wrap( ~ Topo, ncol = 3)


png(file = paste(pathsavefigs, "CO2_flux_sensor_corr_topo.png", sep=""),width=8,height=5,units="in",res=400)
corr1b
dev.off()

png(file = paste(pathsavefigs, "CH4_flux_sensor_corr_topo.png", sep=""),width=8,height=5,units="in",res=400)
corr2b
dev.off()

png(file = paste(pathsavefigs, "N2O_flux_sensor_corr_topo.png", sep=""),width=8,height=5,units="in",res=400)
corr3b
dev.off()

# correlation oxygen and CO2 fluxes

cor(x=arrayGHGdfsensors$O2pct, y=arrayGHGdfsensors$CO2_umolm2s_fluxuse, method="pearson", use="complete.obs")
cor(x=arrayGHGdfsensors$O2pct, y=arrayGHGdfsensors$CH4_nmolm2s_fluxuse, method="pearson", use="complete.obs")
cor(x=arrayGHGdfsensors$O2pct, y=arrayGHGdfsensors$N2O_nmolm2s_fluxuse, method="pearson", use="complete.obs")

# lm and R^2
summary(lm(arrayGHGdfsensors$CO2_umolm2s_fluxuse~arrayGHGdfsensors$O2pct))
summary(lm(arrayGHGdfsensors$CO2_umolm2s_fluxuse~arrayGHGdfsensors$O2pct))$r.squared

summary(lm(arrayGHGdfsensors$CH4_nmolm2s_fluxuse~arrayGHGdfsensors$O2pct))
summary(lm(arrayGHGdfsensors$CH4_nmolm2s_fluxuse~arrayGHGdfsensors$O2pct))$r.squared

summary(lm(arrayGHGdfsensors$N2O_nmolm2s_fluxuse~arrayGHGdfsensors$O2pct))
summary(lm(arrayGHGdfsensors$N2O_nmolm2s_fluxuse~arrayGHGdfsensors$O2pct))$r.squared

# this is only a subset of the results
length(!is.na(arrayGHGdfsensors$O2pct))
# n = 1084



########################################################################
# TO DO, NOTES





