# figure-soilvars-arraydepth.R
# 
# make figures of the depth profile soil variables
#
# O2 - redox - GHG project
# CS O'Connell, UCB, Silver Lab

# see also: 
# PR_ElVerdeArray_SoilData_Master_Combo_csv.csv is organized by hand by Christine
# figure-soilvars-ghgarraydrought.R is where I work with the surface portion of this dataset for the drought paper

# output products:
# figures in folder /ArrayDepthFigures/


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

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")
# gen_data_aov_onlymeansdN
source("~/Documents/GITHUB/RPersonalFunctionsChristine/gen_data_aov_onlymeansdN.r")

# where to save outputs
#pathsavefiles = "~/Documents/GITHUB/cso041code_ArrayDepth/ArrayGHG-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso041code_ArrayDepth/ArrayDepth-Data-Analyses/ArrayDepthFigures/"
pathsoildata = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Soil-data/"


########################################################################
# ORGANIZE SOIL VARIABLES

# load csv
soilvardf <- read.csv(paste(pathsoildata, "PR_ElVerdeArray_SoilData_Master_Combo_csv.csv", sep = ""), stringsAsFactors=FALSE)

# nice names
newnames <- c("Sample","Location","Replicate","Depth","DroughtTimePds","SamplingMonth","TinWgt_g","TinWetSoil_g","TinDrySoil_g","SoilMeasured_FeinHCl_g","SoilMeasured_PinNaHCO3_g","SoilMeasured_pH_g","pH_timeH2Oin","pH_timeread","pH","ReagentMeasured_NaOH","ReagentMeasured_NaHCO3","ReagentMeasured_HCl","ReagentMeasured_KClday0","ReagentMeasured_KClday7","SoilMeasured_NinKClconcentrationsday0_g","SoilMeasured_NinKCltransformationsday0_g","SoilMeasured_NinKCltransformationsday7_g","FeIIconcentration_mgFeg","FeII_IIIconcentration_mgFeg","FeIIIconcentration_mgFeg","P_i_bicarb","P_t_bicarb","P_o_bicarb","P_i_NaOH","P_t_NaOH","P_o_NaOH","P_total")
names(soilvardf) <- newnames

# make factors where needed
soilvardf$Location <- as.factor(soilvardf$Location)
soilvardf$Depth <- as.factor(soilvardf$Depth)
soilvardf$DroughtTimePds <- as.factor(soilvardf$DroughtTimePds)

# pull out the samples for the depth profiles
soilvardepthdf <- subset(soilvardf, soilvardf$DroughtTimePds=='PostDrought' & soilvardf$Location!='Blank')

# re-do location factor levels
soilvardepthdf$Location <- as.character(soilvardepthdf$Location)
soilvardepthdf$Location <- as.factor(soilvardepthdf$Location)

# make numeric where needed
#soilvardepthdf$FeIIconcentration_mgFeg <- as.numeric(soilvardepthdf$FeIIconcentration_mgFeg)
#soilvardepthdf$FeII_IIIconcentration_mgFeg <- as.numeric(soilvardepthdf$FeII_IIIconcentration_mgFeg)
# str(soilvardepthdf)

# get rid of places where Fe(III) is negative
soilvardepthdf$FeIIIconcentration_mgFeg[soilvardepthdf$FeIIIconcentration_mgFeg < 0] <- NA
soilvardepthdf$FeIIconcentration_mgFeg[soilvardepthdf$FeIIIconcentration_mgFeg < 0] <- NA
soilvardepthdf$FeII_IIIconcentration_mgFeg[soilvardepthdf$FeIIIconcentration_mgFeg < 0] <- NA

# get means, std, ste for each depth

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarySE sets - by depth and topographic location for each variable of interest

# pH
summarytabSoilVarspH <- summarySE(data=soilvardepthdf, measurevar="pH", c("Location","Depth"), na.rm=TRUE, renameallcols=TRUE)

# Fe(II)
summarytabSoilVarsFeII <- summarySE(data=soilvardepthdf, measurevar="FeIIconcentration_mgFeg", c("Location","Depth"), na.rm=TRUE, renameallcols=TRUE)

# Fe(III)
summarytabSoilVarsFeIII <- summarySE(data=soilvardepthdf, measurevar="FeIIIconcentration_mgFeg", c("Location","Depth"), na.rm=TRUE, renameallcols=TRUE)

# # Pi
# soilvardepthdf$PiCombo <- soilvardepthdf$P_i_bicarb + soilvardepthdf$P_i_NaOH
# summarytabSoilVarsPiCombo <- summarySE(data=soilvardepthdf, measurevar="PiCombo", c("Location","Depth"), na.rm=TRUE, renameallcols=TRUE)
# # get rid of weird categories
# summarytabSoilVarsPiCombo <- subset(summarytabSoilVarsPiCombo, summarytabSoilVarsPiCombo$N>0)
# 
# # Po
# soilvardepthdf$PoCombo <- soilvardepthdf$P_o_bicarb + soilvardepthdf$P_o_NaOH
# summarytabSoilVarsPoCombo <- summarySE(data=soilvardepthdf, measurevar="PoCombo", c("Location","Depth"), na.rm=TRUE, renameallcols=TRUE)
# # get rid of weird categories
# summarytabSoilVarsPoCombo <- subset(summarytabSoilVarsPoCombo, summarytabSoilVarsPoCombo$N>0)

########################################################################
# DEPTH PROFILE FIGURES

topocolorsGHG <- rev(colorRampPalette(c('red','orange','purple','blue','dark blue'), space = "Lab")(3))

# pH
p1 <- ggplot(summarytabSoilVarspH, aes(x=Depth, y=meanpH)) + geom_point(shape=1) + geom_point(aes(color=Location)) + coord_flip() + ylab("pH (mean +/- s.e.)") + xlab("Depth (cm)") + geom_errorbar(aes(ymin=meanpH-sepH, ymax=meanpH+sepH), width=.1) + scale_colour_discrete(name="Topographic\nLocation") + theme_bw() + scale_x_discrete(limits=c('90-105','75-90','60-75','45-60','30-45','15-30','0-15'))

p1facet <- ggplot(summarytabSoilVarspH, aes(x=Depth, y=meanpH)) + geom_point(shape=1) + geom_point(aes(color=Location)) + coord_flip() + ylab("pH (mean +/- s.e.)") + xlab("Depth (cm)") + geom_errorbar(aes(ymin=meanpH-sepH, ymax=meanpH+sepH), width=.1) + scale_colour_discrete(name="Topographic\nLocation") + theme_bw() + scale_x_discrete(limits=c('90-105','75-90','60-75','45-60','30-45','15-30','0-15')) + facet_grid(Location ~ .)

# pH
p1all <- ggplot(soilvardepthdf, aes(x=Depth, y=pH)) + geom_point(shape=1) + coord_flip() + ylab("pH") + xlab("Depth (cm)") + theme_bw() + scale_x_discrete(limits=c('90-105','75-90','60-75','45-60','30-45','15-30','0-15'))

# Fe(II)
p2 <- ggplot(summarytabSoilVarsFeII, aes(x=Depth, y=meanFeIIconcentration_mgFeg)) + geom_point(shape=1) + geom_point(aes(color=Location)) + coord_flip() + ylab("Fe(II) (mg-Fe/g, mean +/- s.e.)") + xlab("Depth (cm)") + geom_errorbar(aes(ymin=meanFeIIconcentration_mgFeg-seFeIIconcentration_mgFeg, ymax=meanFeIIconcentration_mgFeg+seFeIIconcentration_mgFeg), width=.1) + theme_bw() + scale_x_discrete(limits=c('90-105','75-90','60-75','45-60','30-45','15-30','0-15')) #+ scale_colour_discrete(name="Topographic\nLocation", values=topocolorsGHG) 

p2facet <- ggplot(summarytabSoilVarsFeII, aes(x=Depth, y=meanFeIIconcentration_mgFeg)) + geom_point(shape=1) + geom_point(aes(color=Location)) + coord_flip() + ylab("Fe(II) (mg-Fe/g, mean +/- s.e.)") + xlab("Depth (cm)") + geom_errorbar(aes(ymin=meanFeIIconcentration_mgFeg-seFeIIconcentration_mgFeg, ymax=meanFeIIconcentration_mgFeg+seFeIIconcentration_mgFeg), width=.1) + theme_bw() + scale_x_discrete(limits=c('90-105','75-90','60-75','45-60','30-45','15-30','0-15')) + facet_grid(Location ~ .) #+ scale_colour_discrete(name="Topographic\nLocation", values=topocolorsGHG)

# Fe(III)
p3 <- ggplot(summarytabSoilVarsFeIII, aes(x=Depth, y=meanFeIIIconcentration_mgFeg)) + geom_point(shape=1) + geom_point(aes(color=Location)) + coord_flip() + ylab("Fe(III) (mg-Fe/g, mean +/- s.e.)") + xlab("Depth (cm)") + geom_errorbar(aes(ymin=meanFeIIIconcentration_mgFeg-seFeIIIconcentration_mgFeg, ymax=meanFeIIIconcentration_mgFeg+seFeIIIconcentration_mgFeg), width=.1) + theme_bw() + scale_x_discrete(limits=c('90-105','75-90','60-75','45-60','30-45','15-30','0-15')) #+ scale_colour_discrete(name="Topographic\nLocation", values=topocolorsGHG) 

p3facet <- ggplot(summarytabSoilVarsFeIII, aes(x=Depth, y=meanFeIIIconcentration_mgFeg)) + geom_point(shape=1) + geom_point(aes(color=Location)) + coord_flip() + ylab("Fe(III) (mg-Fe/g, mean +/- s.e.)") + xlab("Depth (cm)") + geom_errorbar(aes(ymin=meanFeIIIconcentration_mgFeg-seFeIIIconcentration_mgFeg, ymax=meanFeIIIconcentration_mgFeg+seFeIIIconcentration_mgFeg), width=.1) + theme_bw() + scale_x_discrete(limits=c('90-105','75-90','60-75','45-60','30-45','15-30','0-15')) + facet_grid(Location ~ .) #+ scale_colour_discrete(name="Topographic\nLocation", values=topocolorsGHG) 


########################################################################
# SAVE FIGURES OVER DEPTH

# save graphs
png(file = paste(pathsavefigs, "arraydepth-pH.png", sep=""),width=6,height=6,units="in",res=400)
p1
dev.off()

png(file = paste(pathsavefigs, "arraydepth-pH-allpts.png", sep=""),width=6,height=6,units="in",res=400)
p1all
dev.off()

png(file = paste(pathsavefigs, "arraydepth-pHfacet.png", sep=""),width=6,height=6,units="in",res=400)
p1facet
dev.off()

png(file = paste(pathsavefigs, "arraydepth-FeII.png", sep=""),width=6,height=6,units="in",res=400)
p2
dev.off()

png(file = paste(pathsavefigs, "arraydepth-FeIIfacet.png", sep=""),width=6,height=6,units="in",res=400)
p2facet
dev.off()

png(file = paste(pathsavefigs, "arraydepth-FeIII.png", sep=""),width=6,height=6,units="in",res=400)
p3
dev.off()

png(file = paste(pathsavefigs, "arraydepth-FeIIIfacet.png", sep=""),width=6,height=6,units="in",res=400)
p3facet
dev.off()


########################################################################
# C AND N DATA IN DIFFERENT CSV

# load csv
CNdf <- read.csv(paste(pathsoildata, "arraydepth-CN.csv", sep = ""), stringsAsFactors=FALSE)

# nice names
newnames <- c("Location","Rep","Depth","File","DateProcessed","Wt_mg","Npct","Cpct","DifN","DifC","AvgNpct","AvgCpct","N_OK","C_OK")
names(CNdf) <- newnames

# make factors where needed
CNdf$Location <- as.factor(CNdf$Location)
CNdf$Depth <- as.factor(CNdf$Depth)

# C/N ratio
#### fill this in

# get means, std, ste for each depth

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarySE sets - by depth and topographic location for each variable of interest

# C
summarytabSoilVarsC <- summarySE(data=CNdf, measurevar="AvgCpct", c("Location","Depth"), na.rm=TRUE, renameallcols=TRUE)

# N
summarytabSoilVarsN <- summarySE(data=CNdf, measurevar="AvgNpct", c("Location","Depth"), na.rm=TRUE, renameallcols=TRUE)

# C/N



########################################################################
# DEPTH PROFILE FIGURES

topocolorsGHG <- rev(colorRampPalette(c('red','orange','purple','blue','dark blue'), space = "Lab")(3))

# C
p5 <- ggplot(summarytabSoilVarsC, aes(x=Depth, y=meanAvgCpct)) + geom_point(shape=1) + geom_point(aes(color=Location)) + coord_flip() + ylab("Percent C (mean +/- s.e.)") + xlab("Depth (cm)") + geom_errorbar(aes(ymin=meanAvgCpct-seAvgCpct, ymax=meanAvgCpct+seAvgCpct), width=.1) + scale_colour_discrete(name="Topographic\nLocation") + theme_bw() + scale_x_discrete(limits=c('90-105','75-90','60-75','45-60','30-45','15-30','0-15'))

p5facet <- ggplot(summarytabSoilVarsC, aes(x=Depth, y=meanAvgCpct)) + geom_point(shape=1) + geom_point(aes(color=Location)) + coord_flip() + ylab("Percent C (mean +/- s.e.)") + xlab("Depth (cm)") + geom_errorbar(aes(ymin=meanAvgCpct-seAvgCpct, ymax=meanAvgCpct+seAvgCpct), width=.1) + scale_colour_discrete(name="Topographic\nLocation") + theme_bw() + scale_x_discrete(limits=c('90-105','75-90','60-75','45-60','30-45','15-30','0-15')) + facet_grid(Location ~ .)

# N
p6 <- ggplot(summarytabSoilVarsN, aes(x=Depth, y=meanAvgNpct)) + geom_point(shape=1) + geom_point(aes(color=Location)) + coord_flip() + ylab("Percent N (mean +/- s.e.)") + xlab("Depth (cm)") + geom_errorbar(aes(ymin=meanAvgNpct-seAvgNpct, ymax=meanAvgNpct+seAvgNpct), width=.1) + scale_colour_discrete(name="Topographic\nLocation") + theme_bw() + scale_x_discrete(limits=c('90-105','75-90','60-75','45-60','30-45','15-30','0-15'))

p6facet <- ggplot(summarytabSoilVarsN, aes(x=Depth, y=meanAvgNpct)) + geom_point(shape=1) + geom_point(aes(color=Location)) + coord_flip() + ylab("Percent N (mean +/- s.e.)") + xlab("Depth (cm)") + geom_errorbar(aes(ymin=meanAvgNpct-seAvgNpct, ymax=meanAvgNpct+seAvgNpct), width=.1) + scale_colour_discrete(name="Topographic\nLocation") + theme_bw() + scale_x_discrete(limits=c('90-105','75-90','60-75','45-60','30-45','15-30','0-15')) + facet_grid(Location ~ .)


# save graphs
png(file = paste(pathsavefigs, "arraydepth-C.png", sep=""),width=6,height=6,units="in",res=400)
p5
dev.off()

png(file = paste(pathsavefigs, "arraydepth-Cfacet.png", sep=""),width=6,height=6,units="in",res=400)
p5facet
dev.off()

png(file = paste(pathsavefigs, "arraydepth-N.png", sep=""),width=6,height=6,units="in",res=400)
p6
dev.off()

png(file = paste(pathsavefigs, "arraydepth-Nfacet.png", sep=""),width=6,height=6,units="in",res=400)
p6facet
dev.off()









# 
# 
# ########################################################################
# # TWO-WAY REPEATED MEASURES ANOVA OF VARS BY DROUGHT PERIOD, TOPO: PH
# 
# ## pH, 0-15cm
# 
# # lme can't handle columns with any NAs
# soilvardepthdf_noNA <- subset(soilvardepthdf, !is.na(pH))
# # take out depths that aren't 0-30 cm
# #soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "0-15" | soilvardepthdf_noNA$Depth == "15-30")
# soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "0-15")
# 
# # aov version of the two-way test
# aov_twoway_droughttopo = aov(pH ~ DroughtTimePds*Topo, data=soilvardepthdf_noNA)
# aovsummary<-xtable(aov_twoway_droughttopo)
# posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
# posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# # summary info
# summary(aov_twoway_droughttopo)
# posthoc1
# posthoc2
# 
# # save diagnostic plots
# png(file = paste(pathsavetab, "ANOVAdiagnostics/pH_15cm_twowayANOVAdiagnostics.png", sep=""),width=6,height=6,units="in",res=150)
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(aov_twoway_droughttopo) # diagnostic plots
# dev.off()
# 
# # save anova info
# 
# # save tables as single excel doc
# # aov() anova output
# write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/pH_twowayANOVA.xlsx",sep=""), sheetName="lme() table 15cm")
# # post-hoc tukey tests
# write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/pH_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 15cm", append=TRUE)
# write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/pH_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 15cm", append=TRUE)
# 
# # set significance stars for graphs
# pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
# cor_stars <- numeric(length=3)
# # cycle through to set number of stars
# for (i in 1:3) {
#   
#   if(pvals[i] < 0.001){
#     cor_stars[i] <- "***"
#   } else if(pvals[i] < 0.01){
#     cor_stars[i] <- "**"
#   } else if(pvals[i] < 0.05){
#     cor_stars[i] <- "*"
#   } else {
#     cor_stars[i] <- "NS"
#   }
#   
# }
# sig_stars_pHtwoway15 <- cor_stars
# 
# ## pH, 15-30cm
# 
# # lme can't handle columns with any NAs
# soilvardepthdf_noNA <- subset(soilvardepthdf, !is.na(pH))
# # take out depths that aren't 0-30 cm
# #soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "0-15" | soilvardepthdf_noNA$Depth == "15-30")
# soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "15-30")
# 
# # aov version of the two-way test
# aov_twoway_droughttopo = aov(pH ~ DroughtTimePds*Topo, data=soilvardepthdf_noNA)
# aovsummary<-xtable(aov_twoway_droughttopo)
# posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
# posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# # summary info
# summary(aov_twoway_droughttopo)
# posthoc1
# posthoc2
# 
# # save diagnostic plots
# png(file = paste(pathsavetab, "ANOVAdiagnostics/pH_30cm_twowayANOVAdiagnostics.png", sep=""),width=6,height=6,units="in",res=150)
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(aov_twoway_droughttopo) # diagnostic plots
# dev.off()
# 
# # save anova info
# 
# # save tables as single excel doc
# # aov() anova output
# write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/pH_twowayANOVA.xlsx",sep=""), sheetName="lme() table 30cm", append=TRUE)
# # post-hoc tukey tests
# write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/pH_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 30cm", append=TRUE)
# write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/pH_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 30cm", append=TRUE)
# 
# # set significance stars for graphs
# pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
# cor_stars <- numeric(length=3)
# # cycle through to set number of stars
# for (i in 1:3) {
#   
#   if(pvals[i] < 0.001){
#     cor_stars[i] <- "***"
#   } else if(pvals[i] < 0.01){
#     cor_stars[i] <- "**"
#   } else if(pvals[i] < 0.05){
#     cor_stars[i] <- "*"
#   } else {
#     cor_stars[i] <- "NS"
#   }
#   
# }
# sig_stars_pHtwoway30 <- cor_stars
# 
# 
# ########################################################################
# # TWO-WAY REPEATED MEASURES ANOVA OF VARS BY DROUGHT PERIOD, TOPO: FE(II)
# 
# ## FeIIconcentration_mgFeg, 0-15cm
# 
# # lme can't handle columns with any NAs
# soilvardepthdf_noNA <- subset(soilvardepthdf, !is.na(FeIIconcentration_mgFeg))
# # take out depths that aren't 0-30 cm
# #soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "0-15" | soilvardepthdf_noNA$Depth == "15-30")
# soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "0-15")
# 
# # aov version of the two-way test
# aov_twoway_droughttopo = aov(FeIIconcentration_mgFeg ~ DroughtTimePds*Topo, data=soilvardepthdf_noNA)
# aovsummary<-xtable(aov_twoway_droughttopo)
# posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
# posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# # summary info
# summary(aov_twoway_droughttopo)
# posthoc1
# posthoc2
# 
# # save diagnostic plots
# png(file = paste(pathsavetab, "ANOVAdiagnostics/FeIIconcentration_mgFeg_twowayANOVAdiagnostics_15cm.png", sep=""),width=6,height=6,units="in",res=150)
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(aov_twoway_droughttopo) # diagnostic plots
# dev.off()
# 
# # save anova info
# 
# # save tables as single excel doc
# # aov() anova output
# write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/FeIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="lme() table 15cm")
# # post-hoc tukey tests
# write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/FeIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 15cm", append=TRUE)
# write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/FeIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 15cm", append=TRUE)
# 
# # set significance stars for graphs
# pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
# cor_stars <- numeric(length=3)
# # cycle through to set number of stars
# for (i in 1:3) {
#   
#   if(pvals[i] < 0.001){
#     cor_stars[i] <- "***"
#   } else if(pvals[i] < 0.01){
#     cor_stars[i] <- "**"
#   } else if(pvals[i] < 0.05){
#     cor_stars[i] <- "*"
#   } else {
#     cor_stars[i] <- "NS"
#   }
#   
# }
# sig_stars_FeIIconcentration_mgFegtwoway15 <- cor_stars
# 
# ## FeIIconcentration_mgFeg, 15-30cm
# 
# # lme can't handle columns with any NAs
# soilvardepthdf_noNA <- subset(soilvardepthdf, !is.na(FeIIconcentration_mgFeg))
# # take out depths that aren't 0-30 cm
# #soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "0-15" | soilvardepthdf_noNA$Depth == "15-30")
# soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "15-30")
# 
# # aov version of the two-way test
# aov_twoway_droughttopo = aov(FeIIconcentration_mgFeg ~ DroughtTimePds*Topo, data=soilvardepthdf_noNA)
# aovsummary<-xtable(aov_twoway_droughttopo)
# posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
# posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# # summary info
# summary(aov_twoway_droughttopo)
# posthoc1
# posthoc2
# 
# # save diagnostic plots
# png(file = paste(pathsavetab, "ANOVAdiagnostics/FeIIconcentration_mgFeg_twowayANOVAdiagnostics_30cm.png", sep=""),width=6,height=6,units="in",res=150)
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(aov_twoway_droughttopo) # diagnostic plots
# dev.off()
# 
# # save anova info
# 
# # save tables as single excel doc
# # aov() anova output
# write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/FeIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="lme() table 30cm", append=TRUE)
# # post-hoc tukey tests
# write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/FeIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 30cm", append=TRUE)
# write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/FeIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 30cm", append=TRUE)
# 
# # set significance stars for graphs
# pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
# cor_stars <- numeric(length=3)
# # cycle through to set number of stars
# for (i in 1:3) {
#   
#   if(pvals[i] < 0.001){
#     cor_stars[i] <- "***"
#   } else if(pvals[i] < 0.01){
#     cor_stars[i] <- "**"
#   } else if(pvals[i] < 0.05){
#     cor_stars[i] <- "*"
#   } else {
#     cor_stars[i] <- "NS"
#   }
#   
# }
# sig_stars_FeIIconcentration_mgFegtwoway30 <- cor_stars
# 
# 
# ########################################################################
# # TWO-WAY REPEATED MEASURES ANOVA OF VARS BY DROUGHT PERIOD, TOPO: FE(III)
# 
# ## FeII_IIIconcentration_mgFeg, 0-15cm
# 
# # lme can't handle columns with any NAs
# soilvardepthdf_noNA <- subset(soilvardepthdf, !is.na(FeII_IIIconcentration_mgFeg))
# # take out depths that aren't 0-30 cm
# #soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "0-15" | soilvardepthdf_noNA$Depth == "15-30")
# soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "0-15")
# 
# # aov version of the two-way test
# aov_twoway_droughttopo = aov(FeII_IIIconcentration_mgFeg ~ DroughtTimePds*Topo, data=soilvardepthdf_noNA)
# aovsummary<-xtable(aov_twoway_droughttopo)
# posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
# posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# # summary info
# summary(aov_twoway_droughttopo)
# posthoc1
# posthoc2
# 
# # save diagnostic plots
# png(file = paste(pathsavetab, "ANOVAdiagnostics/FeII_IIIconcentration_mgFeg_twowayANOVAdiagnostics_15cm.png", sep=""),width=6,height=6,units="in",res=150)
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(aov_twoway_droughttopo) # diagnostic plots
# dev.off()
# 
# # save anova info
# 
# # save tables as single excel doc
# # aov() anova output
# write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/FeII_IIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="lme() table 15cm")
# # post-hoc tukey tests
# write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/FeII_IIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 15cm", append=TRUE)
# write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/FeII_IIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 15cm", append=TRUE)
# 
# # set significance stars for graphs
# pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
# cor_stars <- numeric(length=3)
# # cycle through to set number of stars
# for (i in 1:3) {
#   
#   if(pvals[i] < 0.001){
#     cor_stars[i] <- "***"
#   } else if(pvals[i] < 0.01){
#     cor_stars[i] <- "**"
#   } else if(pvals[i] < 0.05){
#     cor_stars[i] <- "*"
#   } else {
#     cor_stars[i] <- "NS"
#   }
#   
# }
# sig_stars_FeII_IIIconcentration_mgFegtwoway15 <- cor_stars
# 
# ## FeII_IIIconcentration_mgFeg, 15-30cm
# 
# # lme can't handle columns with any NAs
# soilvardepthdf_noNA <- subset(soilvardepthdf, !is.na(FeII_IIIconcentration_mgFeg))
# # take out depths that aren't 0-30 cm
# #soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "0-15" | soilvardepthdf_noNA$Depth == "15-30")
# soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "15-30")
# 
# # aov version of the two-way test
# aov_twoway_droughttopo = aov(FeII_IIIconcentration_mgFeg ~ DroughtTimePds*Topo, data=soilvardepthdf_noNA)
# aovsummary<-xtable(aov_twoway_droughttopo)
# posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
# posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# # summary info
# summary(aov_twoway_droughttopo)
# posthoc1
# posthoc2
# 
# # save diagnostic plots
# png(file = paste(pathsavetab, "ANOVAdiagnostics/FeII_IIIconcentration_mgFeg_twowayANOVAdiagnostics_30cm.png", sep=""),width=6,height=6,units="in",res=150)
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(aov_twoway_droughttopo) # diagnostic plots
# dev.off()
# 
# # save anova info
# 
# # save tables as single excel doc
# # aov() anova output
# write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/FeII_IIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="lme() table 30cm", append=TRUE)
# # post-hoc tukey tests
# write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/FeII_IIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 30cm", append=TRUE)
# write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/FeII_IIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 30cm", append=TRUE)
# 
# # set significance stars for graphs
# pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
# cor_stars <- numeric(length=3)
# # cycle through to set number of stars
# for (i in 1:3) {
#   
#   if(pvals[i] < 0.001){
#     cor_stars[i] <- "***"
#   } else if(pvals[i] < 0.01){
#     cor_stars[i] <- "**"
#   } else if(pvals[i] < 0.05){
#     cor_stars[i] <- "*"
#   } else {
#     cor_stars[i] <- "NS"
#   }
#   
# }
# sig_stars_FeII_IIIconcentration_mgFegtwoway30 <- cor_stars
# 
# 
# ########################################################################
# # TWO-WAY REPEATED MEASURES ANOVA OF VARS BY DROUGHT PERIOD, TOPO: P_O
# 
# ## PoCombo, 0-15cm
# 
# # lme can't handle columns with any NAs
# soilvardepthdf_noNA <- subset(soilvardepthdf, !is.na(PoCombo))
# # take out depths that aren't 0-30 cm
# #soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "0-15" | soilvardepthdf_noNA$Depth == "15-30")
# soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "0-15")
# 
# # aov version of the two-way test
# aov_twoway_droughttopo = aov(PoCombo ~ DroughtTimePds*Topo, data=soilvardepthdf_noNA)
# aovsummary<-xtable(aov_twoway_droughttopo)
# posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
# posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# # summary info
# summary(aov_twoway_droughttopo)
# posthoc1
# posthoc2
# 
# # save diagnostic plots
# png(file = paste(pathsavetab, "ANOVAdiagnostics/PoCombo_15cm_twowayANOVAdiagnostics.png", sep=""),width=6,height=6,units="in",res=150)
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(aov_twoway_droughttopo) # diagnostic plots
# dev.off()
# 
# # save anova info
# 
# # save tables as single excel doc
# # aov() anova output
# write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/PoCombo_twowayANOVA.xlsx",sep=""), sheetName="lme() table, 0-15cm")
# # post-hoc tukey tests
# write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/PoCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1, 0-15cm", append=TRUE)
# write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/PoCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2, 0-15cm", append=TRUE)
# 
# # set significance stars for graphs
# pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
# cor_stars <- numeric(length=3)
# # cycle through to set number of stars
# for (i in 1:3) {
#   
#   if(pvals[i] < 0.001){
#     cor_stars[i] <- "***"
#   } else if(pvals[i] < 0.01){
#     cor_stars[i] <- "**"
#   } else if(pvals[i] < 0.05){
#     cor_stars[i] <- "*"
#   } else {
#     cor_stars[i] <- "NS"
#   }
#   
# }
# sig_stars_PoCombotwoway_15cm <- cor_stars
# 
# ## PoCombo, 15-30cm
# 
# # lme can't handle columns with any NAs
# soilvardepthdf_noNA <- subset(soilvardepthdf, !is.na(PoCombo))
# # take out depths that aren't 0-30 cm
# #soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "0-15" | soilvardepthdf_noNA$Depth == "15-30")
# soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "15-30")
# 
# # aov version of the two-way test
# aov_twoway_droughttopo = aov(PoCombo ~ DroughtTimePds*Topo, data=soilvardepthdf_noNA)
# aovsummary<-xtable(aov_twoway_droughttopo)
# posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
# posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# # summary info
# summary(aov_twoway_droughttopo)
# posthoc1
# posthoc2
# 
# # save diagnostic plots
# png(file = paste(pathsavetab, "ANOVAdiagnostics/PoCombo_30cm_twowayANOVAdiagnostics.png", sep=""),width=6,height=6,units="in",res=150)
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(aov_twoway_droughttopo) # diagnostic plots
# dev.off()
# 
# # save anova info
# 
# # save tables as single excel doc
# # aov() anova output
# write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/PoCombo_twowayANOVA.xlsx",sep=""), sheetName="lme() table, 15-30cm", append=TRUE)
# # post-hoc tukey tests
# write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/PoCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1, 15-30cm", append=TRUE)
# write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/PoCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2, 15-30cm", append=TRUE)
# 
# # set significance stars for graphs
# pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
# cor_stars <- numeric(length=3)
# # cycle through to set number of stars
# for (i in 1:3) {
#   
#   if(pvals[i] < 0.001){
#     cor_stars[i] <- "***"
#   } else if(pvals[i] < 0.01){
#     cor_stars[i] <- "**"
#   } else if(pvals[i] < 0.05){
#     cor_stars[i] <- "*"
#   } else {
#     cor_stars[i] <- "NS"
#   }
#   
# }
# sig_stars_PoCombotwoway_30cm <- cor_stars
# 
# ########################################################################
# # TWO-WAY REPEATED MEASURES ANOVA OF VARS BY DROUGHT PERIOD, TOPO: P_I
# 
# ## PiCombo, 0-15cm
# 
# # take out NAs
# soilvardepthdf_noNA <- subset(soilvardepthdf, !is.na(PiCombo))
# # take out depths that aren't 0-30 cm
# #soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "0-15" | soilvardepthdf_noNA$Depth == "15-30")
# soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "0-15")
# 
# # aov version of the two-way test
# aov_twoway_droughttopo = aov(PiCombo ~ DroughtTimePds*Topo, data=soilvardepthdf_noNA)
# aovsummary<-xtable(aov_twoway_droughttopo)
# posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
# posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# # summary info
# summary(aov_twoway_droughttopo)
# posthoc1
# posthoc2
# 
# # save diagnostic plots
# png(file = paste(pathsavetab, "ANOVAdiagnostics/PiCombo_twowayANOVAdiagnostics_15cm.png", sep=""),width=6,height=6,units="in",res=150)
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(aov_twoway_droughttopo) # diagnostic plots
# dev.off()
# 
# # save anova info
# 
# # save tables as single excel doc
# # aov() anova output
# write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/PiCombo_twowayANOVA.xlsx",sep=""), sheetName="lme() table 15cm")
# # post-hoc tukey tests
# write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/PiCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 15cm", append=TRUE)
# write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/PiCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 15cm", append=TRUE)
# 
# # set significance stars for graphs
# pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
# cor_stars <- numeric(length=3)
# # cycle through to set number of stars
# for (i in 1:3) {
#   
#   if(pvals[i] < 0.001){
#     cor_stars[i] <- "***"
#   } else if(pvals[i] < 0.01){
#     cor_stars[i] <- "**"
#   } else if(pvals[i] < 0.05){
#     cor_stars[i] <- "*"
#   } else {
#     cor_stars[i] <- "NS"
#   }
#   
# }
# sig_stars_PiCombotwoway15 <- cor_stars
# 
# ## PiCombo, 15-30cm
# 
# # take out NAs
# soilvardepthdf_noNA <- subset(soilvardepthdf, !is.na(PiCombo))
# # take out depths that aren't 0-30 cm
# #soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "0-15" | soilvardepthdf_noNA$Depth == "15-30")
# soilvardepthdf_noNA <- subset(soilvardepthdf_noNA, soilvardepthdf_noNA$Depth == "15-30")
# 
# # aov version of the two-way test
# aov_twoway_droughttopo = aov(PiCombo ~ DroughtTimePds*Topo, data=soilvardepthdf_noNA)
# aovsummary<-xtable(aov_twoway_droughttopo)
# posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
# posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# # summary info
# summary(aov_twoway_droughttopo)
# posthoc1
# posthoc2
# 
# # save diagnostic plots
# png(file = paste(pathsavetab, "ANOVAdiagnostics/PiCombo_twowayANOVAdiagnostics_30cm.png", sep=""),width=6,height=6,units="in",res=150)
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(aov_twoway_droughttopo) # diagnostic plots
# dev.off()
# 
# # save anova info
# 
# # save tables as single excel doc
# # aov() anova output
# write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/PiCombo_twowayANOVA.xlsx",sep=""), sheetName="lme() table 30cm", append=TRUE)
# # post-hoc tukey tests
# write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/PiCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 30cm", append=TRUE)
# write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/PiCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 30cm", append=TRUE)
# 
# # set significance stars for graphs
# pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
# cor_stars <- numeric(length=3)
# # cycle through to set number of stars
# for (i in 1:3) {
#   
#   if(pvals[i] < 0.001){
#     cor_stars[i] <- "***"
#   } else if(pvals[i] < 0.01){
#     cor_stars[i] <- "**"
#   } else if(pvals[i] < 0.05){
#     cor_stars[i] <- "*"
#   } else {
#     cor_stars[i] <- "NS"
#   }
#   
# }
# sig_stars_PiCombotwoway30 <- cor_stars




########################################################################
# NOTES, ETC.






