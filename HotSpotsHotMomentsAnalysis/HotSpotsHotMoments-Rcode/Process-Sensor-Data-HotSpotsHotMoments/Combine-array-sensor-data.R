# Combine-array-sensor-data
# based on "Process-arraysensorsdf-Sensor-Data-RCode" 
#     (run that file on new data dowloads first to create inputs!)
# 
# Combines data from most recent download date ("NewDatapath")
#  with old array data ("DataArchivepath")
#  and saves complete dataset in the DataArchive
#  for seven files:
#                   fulldaily.csv
#                   O2dailywideavg.csv
#                   O2hourly.csv
#                   tempdailywideavg.csv
#                   temphourly.csv
#                   vwcdailywideavg.csv
#                   vwchourly.csv

######################################################################

# load packages
library(plyr)
library(dplyr)


# locate datasets
DataArchivepath <- "C:/Users/jstar_000/Desktop/PC400 data/DataArchive/" 
# old data; this should never change unless changing computers

NewDatapath      <- "C:/Users/jstar_000/Desktop/PC400 data/1-23-17/Surface results/" 
# this should change each time data is dowloaded

# create master list of file names
allfiles <- c("fulldaily","O2dailywideavg","O2hourly",
              "tempdailywideavg","temphourly",
              "vwcdailywideavg","vwchourly")


# bring in old data
OldFiles <- list()
for(i in 1:7) {
OldFiles[[i]] <- as.data.frame(read.csv(paste(DataArchivepath,allfiles[i],".csv",sep=""), stringsAsFactors=FALSE))
names(OldFiles)[i] <- paste("Old",allfiles[i],sep="")
}



# bring in new data
NewFiles <- list()
for(i in 1:7) {
  NewFiles[[i]] <- as.data.frame(read.csv(paste(NewDatapath,allfiles[i],".csv",sep=""), stringsAsFactors=FALSE))
  names(NewFiles)[i] <- paste("New",allfiles[i],sep="")
}

# Bind datasets
CompleteFiles <- list()
for(i in 1:7) {
  stopifnot(names(OldFiles[[i]])==names(NewFiles[[i]]))
  CompleteFiles[[i]] <- join(OldFiles[[i]],NewFiles[[i]],type="full")
  names(CompleteFiles)[i] <- paste("Complete",allfiles[i],sep="")
}


# Save all data
for(i in 1:7){
  write.csv(CompleteFiles[[i]],
           file=paste(DataArchivepath,allfiles[i],".csv",sep=""),
           row.names=FALSE)
}