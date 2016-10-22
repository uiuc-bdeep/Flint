#################################################################################
############################## Preliminaries ####################################
#################################################################################

# rm(list=ls(pattern="temp"))
# rm(list=setdiff(ls(), "x")) remove except "x"
rm(list=ls())

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c("readxl", "data.table", "stringr", "plyr", "doBy")
lapply(packages, pkgTest)

#################################################################################
################### read leadData and Trans table ###############################
#################################################################################

leadData <- read.table("~/share/projects/Flint/production/LeadTestsResult/leadData_merged.csv", header = TRUE, sep = ",")
#transData <- read.table("~/share/projects/Flint/production/AllVariables_0913.csv", header = TRUE, sep = ",")
transData <- read.table("~/share/projects/Flint/production/most_recent_1010.csv", header = TRUE, sep = ",")

transData <- transData[which(transData$PropertyCity != ""),]

leadData <- as.data.table(leadData)
transData <- as.data.table(transData)
# TODO: 
# (1) lower case, 
leadData$NewAddress <- trimws(tolower(gsub(pattern="[[:punct:]]", leadData$Address, replacement="")))
leadData$NewAddress <- gsub(pattern="\\s+", replacement=" ", leadData$NewAddress)
setkey(leadData, "LABID")
leadData <- unique(leadData)

transData$NewAddress <- tolower(paste0(transData$PropertyFullStreetAddress, " ", transData$PropertyAddressUnitDesignator, " ", transData$PropertyAddressUnitNumber, " ", transData$PropertyCity, " ", transData$PropertyState))
transData$NewAddress <- trimws(gsub(pattern="[[:punct:]]", transData$NewAddress, replacement=""))
transData$NewAddress <- gsub(pattern="\\s+", replacement=" ", transData$NewAddress)
#setkey(transData, "NewAddress")
#transData <- unique(transData)
# (2) remove ", and ."
# gsub(pattern="[[:punct:]]", leadData$Address, replacement="") remove all punctuation

## merging
mergedData <- merge(x = leadData, y = transData, 
      by.x = c("NewAddress"), 
      by.y = c("NewAddress"),
      all.x = T, all.y = F)

mergedData_date <- mergedData[which(as.Date(mergedData$RecordingDate, format="%Y-%m-%d") > as.Date(mergedData$Results_Se, format="%m/%d/%Y")),] 

mergedData_hadID <- mergedData[which(!is.na(mergedData$ImportParcelID)),]

mergedData_date <- subset(mergedData_date, select = c("TransId", "LABID", "Lead_mgl", "Lead_Viola", "Results_Se", "Notes"))

sum_lead <- summaryBy(Lead_mgl ~ TransId, data=mergedData_date, FUN=c(mean))

transData$Tested_House <- NA

for (i in 1:nrow(sum_lead)){
  
  transData$Tested_House[which(transData$TransId == sum_lead$TransId[i])] <- sum_lead$Lead_mgl.mean[i]
  
}

#allVal_Lead <- merge(x = transData, y = mergedData_date, 
#                         by.x = c("TransId"), 
#                         by.y = c("TransId"),
#                         all.x = T, all.y = F, allow.cartesian=TRUE)

#tempTrans <- read.csv("~/share/projects/Flint/production/LeadTestsResult/forclosureTable.csv")
#colnames(tempTrans)[1] <- "forclosureID"

#allVal_Lead_ForeClosure <- merge(x = allVal_Lead, y = tempTrans, 
#                         by.x = c("TransId"), 
#                         by.y = c("TransId"),
#                         all.x = T, all.y = F)

#write.csv(allVal_Lead, "~/share/projects/Flint/production/outputs/Flint_Hedonics.csv")

# 18130 / 23751
# 11180 / 23751 and 28336 / 40907

