## Preliminaries
rm(list=ls())

pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c("ggplot2","ggmap", "readxl", "data.table", "sp")
lapply(packages, pkgTest)

## read cleaned up data ##
new3_raw <- read.csv("/home/bdeep/share/projects/Flint/stores/LeadTestsData/Test_Results_Flint_07_13_2017.csv", stringsAsFactors = FALSE)
new4_raw <- read.csv("/home/bdeep/share/projects/Flint/stores/LeadTestsData/Test_Results_Flint_12_21_2017.csv", stringsAsFactors = FALSE)
new5_raw <- read.csv("/home/bdeep/share/projects/Flint/stores/LeadTestsData/Test_Results_Flint_07_12_2018.csv", stringsAsFactors = FALSE)
new6_raw <- read.csv("/home/bdeep/share/projects/Flint/stores/LeadTestsData/Test_Results_Flint_12_28_2018.csv", stringsAsFactors = FALSE)
new7_raw <- read.csv("/home/bdeep/share/projects/Flint/stores/LeadTestsData/Test_Results_Flint_07_08_2019.csv", stringsAsFactors = FALSE)
new8_raw <- read.csv("/home/bdeep/share/projects/Flint/stores/LeadTestsData/Test_Results_Flint_09_06_2019.csv", stringsAsFactors = FALSE)

old_raw <- read.csv("/home/bdeep/share/projects/Flint/production/LeadTestsResult/flintLeadTest_Feb_2017_Address.csv", stringsAsFactors = FALSE)


## creating the date ##
new3 <- new3_raw
new3$TestDate <- as.Date(new3$Date.Submitted, format="%m/%d/%Y")
new3 <- new3[which(!is.na(new3$TestDate)),]
new3$PostDate <- as.Date("07/13/2017", format="%m/%d/%Y")
new4 <- new4_raw
new4$TestDate <- as.Date(new4$Date.Submitted, format="%m/%d/%Y")
new4 <- new4[which(!is.na(new4$TestDate)),]
new4$PostDate <- as.Date("12/21/2017",format="%m/%d/%Y")
new5 <- new5_raw
new5$TestDate <- as.Date(new5$Date.Submitted, format="%m/%d/%Y")
new5 <- new5[which(!is.na(new5$TestDate)),]
new5$PostDate <- as.Date("07/12/2018",format="%m/%d/%Y")
new6 <- new6_raw
new6$TestDate <- as.Date(new6$Date.Submitted, format="%m/%d/%Y")
new6 <- new6[which(!is.na(new6$TestDate)),]
new6$PostDate <- as.Date("12/28/2018",format="%m/%d/%Y")
new7 <- new7_raw
new7$TestDate <- as.Date(new7$Date.Submitted, format="%m/%d/%Y")
new7 <- new7[which(!is.na(new7$TestDate)),]
new7$PostDate <- as.Date("07/08/2019",format="%m/%d/%Y")
new8 <- new8_raw
new8$TestDate <- as.Date(new8$Date.Submitted, format="%m/%d/%Y")
new8 <- new8[which(!is.na(new8$TestDate)),]
new8$PostDate <- as.Date("09/06/2019",format="%m/%d/%Y")

old <- old_raw
old$Date <- as.Date(old$Results_Se, format="%m/%d/%Y")
## remove the rows with this comment
old <- old[which(old$Notes != "From Michigan.gov/flintwater."), ]

## for lead, making all NA to be 0 in order to gather the max value ##
new3$X250.ml.Bottle..PPB.[which(is.na(new3$X250.ml.Bottle..PPB.))] <- 0
new3$X750.ml.Bottle..PPB.[which(is.na(new3$X750.ml.Bottle..PPB.))] <- 0
new3$X1.Liter.Calculated..PPB.[which(is.na(new3$X1.Liter.Calculated..PPB.))] <- 0
# 1 bottle is factor so very complicated #
new3$X1.Liter..PPB. <- gsub(",", "", as.character(new3$X1.Liter..PPB.))
new3$X1.Liter..PPB.[which(new3$X1.Liter..PPB. == "")] <- "0"
new3$X1.Liter..PPB.[which(is.na(new3$X1.Liter..PPB.))] <- "0"
new3$X1.Liter..PPB. <- as.numeric(new3$X1.Liter..PPB.)

## for copper, making all NA to be 0 in order to gather the max value ##
new3$X250.ml.Bottle..PPB..1[which(is.na(new3$X250.ml.Bottle..PPB..1))] <- 0
new3$X750.ml.Bottle..PPB..1[which(is.na(new3$X750.ml.Bottle..PPB..1))] <- 0
new3$X1.Liter..PPB..1[which(is.na(new3$X1.Liter..PPB..1))] <- 0
# 1 bottle is factor so very complicated #
new3$X1.Liter..PPB..1 <- gsub(",", "", as.character(new3$X1.Liter..PPB..1))
new3$X1.Liter..PPB..1[which(new3$X1.Liter..PPB..1 == "")] <- "0"
new3$X1.Liter..PPB..1[which(is.na(new3$X1.Liter..PPB..1))] <- "0"
new3$X1.Liter..PPB..1 <- as.numeric(new3$X1.Liter..PPB..1)

# get the lead value and copper value #
new3$Lead_mgl <- 0
new3$Copper_mgl <- 0
for (i in 1:nrow(new3)) {
  new3$Lead_mgl[i] <- max(new3$X250.ml.Bottle..PPB.[i], 
                          new3$X750.ml.Bottle..PPB.[i], 
                          new3$X1.Liter.Calculated..PPB.[i],
                          new3$X1.Liter..PPB.[i])/1000.0
  new3$Copper_mgl[i] <- max(new3$X250.ml.Bottle..PPB..1[i], 
                            new3$X750.ml.Bottle..PPB..1[i], 
                            new3$X1.Liter.Calculated..PPB..1[i],
                            new3$X1.Liter..PPB..1[i])/1000.0
}
new3$Copper_mgl[which(is.na(new3$Copper_mgl))] <- 0

## for lead, making all NA to be 0 in order to gather the max value ##
new4$X250.ml.Bottle..PPB.[which(is.na(new4$X250.ml.Bottle..PPB.))] <- 0
new4$X750.ml.Bottle..PPB.[which(is.na(new4$X750.ml.Bottle..PPB.))] <- 0
new4$X1.Liter.Calculated..PPB.[which(is.na(new4$X1.Liter.Calculated..PPB.))] <- 0
# 1 bottle is factor so very complicated #
new4$X1.Liter..PPB. <- gsub(",", "", as.character(new4$X1.Liter..PPB.))
new4$X1.Liter..PPB.[which(new4$X1.Liter..PPB. == "")] <- "0"
new4$X1.Liter..PPB.[which(is.na(new4$X1.Liter..PPB.))] <- "0"
new4$X1.Liter..PPB. <- as.numeric(new4$X1.Liter..PPB.)

## for copper, making all NA to be 0 in order to gather the max value ##
new4$X250.ml.Bottle..PPB..1[which(is.na(new4$X250.ml.Bottle..PPB..1))] <- 0
new4$X750.ml.Bottle..PPB..1[which(is.na(new4$X750.ml.Bottle..PPB..1))] <- 0
new4$X1.Liter..PPB..1[which(is.na(new4$X1.Liter..PPB..1))] <- 0
# 1 bottle is factor so very complicated #
new4$X1.Liter..PPB..1 <- gsub(",", "", as.character(new4$X1.Liter..PPB..1))
new4$X1.Liter..PPB..1[which(new4$X1.Liter..PPB..1 == "")] <- "0"
new4$X1.Liter..PPB..1[which(is.na(new4$X1.Liter..PPB..1))] <- "0"
new4$X1.Liter..PPB..1 <- as.numeric(new4$X1.Liter..PPB..1)

# get the lead value and copper value #
new4$Lead_mgl <- 0
new4$Copper_mgl <- 0
for (i in 1:nrow(new4)) {
  new4$Lead_mgl[i] <- max(new4$X250.ml.Bottle..PPB.[i], 
                          new4$X750.ml.Bottle..PPB.[i], 
                          new4$X1.Liter.Calculated..PPB.[i],
                          new4$X1.Liter..PPB.[i])/1000.0
  new4$Copper_mgl[i] <- max(new4$X250.ml.Bottle..PPB..1[i], 
                            new4$X750.ml.Bottle..PPB..1[i], 
                            new4$X1.Liter.Calculated..PPB..1[i],
                            new4$X1.Liter..PPB..1[i])/1000.0
}
new4$Copper_mgl[which(is.na(new4$Copper_mgl))] <- 0

## for lead, making all NA to be 0 in order to gather the max value ##
new5$X250.ml.Bottle..PPB.[which(is.na(new5$X250.ml.Bottle..PPB.))] <- 0
new5$X750.ml.Bottle..PPB.[which(is.na(new5$X750.ml.Bottle..PPB.))] <- 0
new5$X1.Liter.Calculated..PPB.[which(is.na(new5$X1.Liter.Calculated..PPB.))] <- 0
# 1 bottle is factor so very complicated #
new5$X1.Liter..PPB. <- gsub(",", "", as.character(new5$X1.Liter..PPB.))
new5$X1.Liter..PPB.[which(new5$X1.Liter..PPB. == "")] <- "0"
new5$X1.Liter..PPB.[which(is.na(new5$X1.Liter..PPB.))] <- "0"
new5$X1.Liter..PPB. <- as.numeric(new5$X1.Liter..PPB.)

## for copper, making all NA to be 0 in order to gather the max value ##
new5$X250.ml.Bottle..PPB..1[which(is.na(new5$X250.ml.Bottle..PPB..1))] <- 0
new5$X750.ml.Bottle..PPB..1[which(is.na(new5$X750.ml.Bottle..PPB..1))] <- 0
new5$X1.Liter..PPB..1[which(is.na(new5$X1.Liter..PPB..1))] <- 0
# 1 bottle is factor so very complicated #
new5$X1.Liter..PPB..1 <- gsub(",", "", as.character(new5$X1.Liter..PPB..1))
new5$X1.Liter..PPB..1[which(new5$X1.Liter..PPB..1 == "")] <- "0"
new5$X1.Liter..PPB..1[which(is.na(new5$X1.Liter..PPB..1))] <- "0"
new5$X1.Liter..PPB..1 <- as.numeric(new5$X1.Liter..PPB..1)

# get the lead value and copper value #
new5$Lead_mgl <- 0
new5$Copper_mgl <- 0
for (i in 1:nrow(new5)) {
  new5$Lead_mgl[i] <- max(new5$X250.ml.Bottle..PPB.[i], 
                          new5$X750.ml.Bottle..PPB.[i], 
                          new5$X1.Liter.Calculated..PPB.[i],
                          new5$X1.Liter..PPB.[i])/1000.0
  new5$Copper_mgl[i] <- max(new5$X250.ml.Bottle..PPB..1[i], 
                            new5$X750.ml.Bottle..PPB..1[i], 
                            new5$X1.Liter.Calculated..PPB..1[i],
                            new5$X1.Liter..PPB..1[i])/1000.0
}
new5$Copper_mgl[which(is.na(new5$Copper_mgl))] <- 0

## for lead, making all NA to be 0 in order to gather the max value ##
new6$X250.ml.Bottle..PPB.[which(is.na(new6$X250.ml.Bottle..PPB.))] <- 0
new6$X750.ml.Bottle..PPB.[which(is.na(new6$X750.ml.Bottle..PPB.))] <- 0
new6$X1.Liter.Calculated..PPB.[which(is.na(new6$X1.Liter.Calculated..PPB.))] <- 0
# 1 bottle is factor so very complicated #
new6$X1.Liter..PPB. <- gsub(",", "", as.character(new6$X1.Liter..PPB.))
new6$X1.Liter..PPB.[which(new6$X1.Liter..PPB. == "")] <- "0"
new6$X1.Liter..PPB.[which(is.na(new6$X1.Liter..PPB.))] <- "0"
new6$X1.Liter..PPB. <- as.numeric(new6$X1.Liter..PPB.)

## for copper, making all NA to be 0 in order to gather the max value ##
new6$X250.ml.Bottle..PPB..1[which(is.na(new6$X250.ml.Bottle..PPB..1))] <- 0
new6$X750.ml.Bottle..PPB..1[which(is.na(new6$X750.ml.Bottle..PPB..1))] <- 0
new6$X1.Liter..PPB..1[which(is.na(new6$X1.Liter..PPB..1))] <- 0
# 1 bottle is factor so very complicated #
new6$X1.Liter..PPB..1 <- gsub(",", "", as.character(new6$X1.Liter..PPB..1))
new6$X1.Liter..PPB..1[which(new6$X1.Liter..PPB..1 == "")] <- "0"
new6$X1.Liter..PPB..1[which(is.na(new6$X1.Liter..PPB..1))] <- "0"
new6$X1.Liter..PPB..1 <- as.numeric(new6$X1.Liter..PPB..1)

# get the lead value and copper value #
new6$Lead_mgl <- 0
new6$Copper_mgl <- 0
for (i in 1:nrow(new6)) {
  new6$Lead_mgl[i] <- max(new6$X250.ml.Bottle..PPB.[i], 
                          new6$X750.ml.Bottle..PPB.[i], 
                          new6$X1.Liter.Calculated..PPB.[i],
                          new6$X1.Liter..PPB.[i])/1000.0
  new6$Copper_mgl[i] <- max(new6$X250.ml.Bottle..PPB..1[i], 
                            new6$X750.ml.Bottle..PPB..1[i], 
                            new6$X1.Liter.Calculated..PPB..1[i],
                            new6$X1.Liter..PPB..1[i])/1000.0
}
new6$Copper_mgl[which(is.na(new6$Copper_mgl))] <- 0

## for lead, making all NA to be 0 in order to gather the max value ##
new7$X250.ml.Bottle..PPB.[which(is.na(new7$X250.ml.Bottle..PPB.))] <- 0
new7$X750.ml.Bottle..PPB.[which(is.na(new7$X750.ml.Bottle..PPB.))] <- 0
new7$X1.Liter.Calculated..PPB.[which(is.na(new7$X1.Liter.Calculated..PPB.))] <- 0
# 1 bottle is factor so very complicated #
new7$X1.Liter..PPB. <- gsub(",", "", as.character(new7$X1.Liter..PPB.))
new7$X1.Liter..PPB.[which(new7$X1.Liter..PPB. == "")] <- "0"
new7$X1.Liter..PPB.[which(is.na(new7$X1.Liter..PPB.))] <- "0"
new7$X1.Liter..PPB. <- as.numeric(new7$X1.Liter..PPB.)

## for copper, making all NA to be 0 in order to gather the max value ##
new7$X250.ml.Bottle..PPB..1[which(is.na(new7$X250.ml.Bottle..PPB..1))] <- 0
new7$X750.ml.Bottle..PPB..1[which(is.na(new7$X750.ml.Bottle..PPB..1))] <- 0
new7$X1.Liter..PPB..1[which(is.na(new7$X1.Liter..PPB..1))] <- 0
# 1 bottle is factor so very complicated #
new7$X1.Liter..PPB..1 <- gsub(",", "", as.character(new7$X1.Liter..PPB..1))
new7$X1.Liter..PPB..1[which(new7$X1.Liter..PPB..1 == "")] <- "0"
new7$X1.Liter..PPB..1[which(is.na(new7$X1.Liter..PPB..1))] <- "0"
new7$X1.Liter..PPB..1 <- as.numeric(new7$X1.Liter..PPB..1)

# get the lead value and copper value #
new7$Lead_mgl <- 0
new7$Copper_mgl <- 0
for (i in 1:nrow(new7)) {
  new7$Lead_mgl[i] <- max(new7$X250.ml.Bottle..PPB.[i], 
                          new7$X750.ml.Bottle..PPB.[i], 
                          new7$X1.Liter.Calculated..PPB.[i],
                          new7$X1.Liter..PPB.[i])/1000.0
  new7$Copper_mgl[i] <- max(new7$X250.ml.Bottle..PPB..1[i], 
                            new7$X750.ml.Bottle..PPB..1[i], 
                            new7$X1.Liter.Calculated..PPB..1[i],
                            new7$X1.Liter..PPB..1[i])/1000.0
}
new7$Copper_mgl[which(is.na(new7$Copper_mgl))] <- 0

## for lead, making all NA to be 0 in order to gather the max value ##
new8$X250.ml.Bottle..PPB.[which(is.na(new8$X250.ml.Bottle..PPB.))] <- 0
new8$X750.ml.Bottle..PPB.[which(is.na(new8$X750.ml.Bottle..PPB.))] <- 0
new8$X1.Liter.Calculated..PPB.[which(is.na(new8$X1.Liter.Calculated..PPB.))] <- 0
# 1 bottle is factor so very complicated #
new8$X1.Liter..PPB. <- gsub(",", "", as.character(new8$X1.Liter..PPB.))
new8$X1.Liter..PPB.[which(new8$X1.Liter..PPB. == "")] <- "0"
new8$X1.Liter..PPB.[which(is.na(new8$X1.Liter..PPB.))] <- "0"
new8$X1.Liter..PPB. <- as.numeric(new8$X1.Liter..PPB.)

## for copper, making all NA to be 0 in order to gather the max value ##
new8$X250.ml.Bottle..PPB..1[which(is.na(new8$X250.ml.Bottle..PPB..1))] <- 0
new8$X750.ml.Bottle..PPB..1[which(is.na(new8$X750.ml.Bottle..PPB..1))] <- 0
new8$X1.Liter..PPB..1[which(is.na(new8$X1.Liter..PPB..1))] <- 0
# 1 bottle is factor so very complicated #
new8$X1.Liter..PPB..1 <- gsub(",", "", as.character(new8$X1.Liter..PPB..1))
new8$X1.Liter..PPB..1[which(new8$X1.Liter..PPB..1 == "")] <- "0"
new8$X1.Liter..PPB..1[which(is.na(new8$X1.Liter..PPB..1))] <- "0"
new8$X1.Liter..PPB..1 <- as.numeric(new8$X1.Liter..PPB..1)

# get the lead value and copper value #
new8$Lead_mgl <- 0
new8$Copper_mgl <- 0
for (i in 1:nrow(new8)) {
  new8$Lead_mgl[i] <- max(new8$X250.ml.Bottle..PPB.[i], 
                          new8$X750.ml.Bottle..PPB.[i], 
                          new8$X1.Liter.Calculated..PPB.[i],
                          new8$X1.Liter..PPB.[i])/1000.0
  new8$Copper_mgl[i] <- max(new8$X250.ml.Bottle..PPB..1[i], 
                            new8$X750.ml.Bottle..PPB..1[i], 
                            new8$X1.Liter.Calculated..PPB..1[i],
                            new8$X1.Liter..PPB..1[i])/1000.0
}
new8$Copper_mgl[which(is.na(new8$Copper_mgl))] <- 0

## subset and combine together ##
TF <- subset(new3, select = c("Sample.Number", "Lead_mgl", "TestDate","PostDate", "Copper_mgl","Street..","Street.Name"))
new3_result <- merge(new3_raw, TF, by=c("Sample.Number"))
TF <- subset(new4, select = c("Sample.Number", "Lead_mgl", "TestDate","PostDate", "Copper_mgl","Street..","Street.Name"))
new4_result <- merge(new4_raw, TF, by=c("Sample.Number"))
TF <- subset(new5, select = c("Sample.Number", "Lead_mgl", "TestDate","PostDate", "Copper_mgl","Street..","Street.Name"))
new5_result <- merge(new5_raw, TF, by=c("Sample.Number"))
TF <- subset(new6, select = c("Sample.Number", "Lead_mgl", "TestDate","PostDate", "Copper_mgl","Street..","Street.Name"))
new6_result <- merge(new6_raw, TF, by=c("Sample.Number"))
TF <- subset(new7, select = c("Sample.Number", "Lead_mgl", "TestDate","PostDate", "Copper_mgl","Street..","Street.Name"))
new7_result <- merge(new7_raw, TF, by=c("Sample.Number"))
TF <- subset(new8, select = c("Sample.Number", "Lead_mgl", "TestDate","PostDate", "Copper_mgl","Street..","Street.Name"))
new8_result <- merge(new8_raw, TF, by=c("Sample.Number"))

new345678_result <- rbind(new3_result, new4_result,new5_result,new6_result,new7_result,new8_result)
new345678_result$Lead..ppb. <- as.character(new345678_result$Lead_mgl*1000.0)
new345678_result$Copper..ppb. <- as.character(new345678_result$Copper_mgl*1000.0)

## writing the newData ##
write.csv(new345678_result, "/home/bdeep/share/projects/Flint/production/LeadTestsResult/newData_345678.csv", row.names = FALSE)

## writing the older data ##
#write.csv(old, "/home/bdeep/share/projects/Flint/production/LeadTestsResult/oldData_345678.csv", row.names = FALSE)

## changing the fields of old leadTests
## lat = Y, lon = X
new345678_result$LABID <- new345678_result$Sample.Number
new345678_result$Results_Se <- new345678_result$Date.Submitted
new345678_result$Lead_Viola <- NA
new345678_result$Lead_Viola[which(new345678_result$Lead_mgl > 0.015)] <- "Yes"
new345678_result$Lead_Viola[which(new345678_result$Lead_mgl <= 0.015)] <- "No"
new345678_result$Copper_Vio <- NA
new345678_result$Copper_Vio[which(new345678_result$Copper_mgl > 1.3)] <- "Yes"
new345678_result$Copper_Vio[which(new345678_result$Copper_mgl <= 1.3)] <- "No"
new345678_result$Sample.Number <- NULL
new345678_result$Date.Submitted <- NULL
#new345678_result$Address <- paste0(new345678_result$Street.., new345678_result$Street.Name, ", ", new345678_result$City, ", ", "MI")
new345678_result$Match_addr <- NA
new345678_result$lat <- NA
new345678_result$lon <- NA
new345678_result$Notes <- "From Michigan.gov/flintwater."

old_result <- subset(old_raw, select = c("LABID", "Lead_mgl", "Lead_Viola", "Copper_mgl", "Copper_Vio", "Date", "Results_Se", "Street..", "Street.Name", "lon", "lat", "City", "Zip.Code", "Notes"))
colnames(old_result) <- c("LABID", "Results_Se", "Lead_mgl", "Lead_Viola", "Copper_mgl", "Copper_Vio", "Date", "Address", "Match_addr", "lon", "lat", "City", "Zip.Code", "Notes")
old_result$Analysis..Lead. <- "Lead"
old_result$Lead..ppb. <- old_result$Lead_mgl*1000.0
old_result$Analysis..Copper. <- "Copper"
old_result$Copper..ppb. <- old_result$Copper_mgl*1000.0
#old_result$Street.. <- ""
#old_result$Street.Name <- ""
old_result$X250.ml.Bottle..PPB. <- NA
old_result$X750.ml.Bottle..PPB. <- NA
old_result$X1.Liter.Calculated..PPB. <- NA
old_result$X1.Liter..PPB. <- NA
old_result$X250.ml.Bottle..PPB..1 <- NA
old_result$X750.ml.Bottle..PPB..1 <- NA
old_result$X1.Liter.Calculated..PPB..1 <- NA
old_result$X1.Liter..PPB..1 <- NA

#[1] "Analysis..Lead."                          "Lead..ppb."                               "Analysis..Copper."                       
#[4] "Copper..ppb."                             "Street.."                                 "Street.Name"                             
#[7] "City"                                     "Zip.Code"                                 "Date"                                    
#[10] "Lead_mgl"                                 "Copper_mgl"                               "X2.Bottle.Kit.250.ml.Bottle..PPB."       
#[13] "X2.Bottle.Kit.750.ml.Bottle..PPB."        "X2.Bottle.Kit.1.Liter.Calculated..PPB."   "X1.Bottle.Kit.1.Liter..PPB."             
#[16] "X2.Bottle.Kit.250.ml.Bottle..PPB..1"      "X2.Bottle.Kit.750.ml.Bottle..PPB..1"      "X2.Bottle.Kit.1.Liter.Calculated..PPB..1"
#[19] "X1.Bottle.Kit.1.Liter..PPB..1"            "LABID"                                    "Results_Se"                              
#[22] "Lead_Viola"                               "Copper_Vio"
new345678_result$Match_addr <- NULL
new345678_result$Street...y<-NULL
new345678_result$Street.Name.y<-NULL
colnames(new345678_result)[11] <-"Street.."
colnames(new345678_result)[12] <-"Street.Name"
new345678_result$Copper..ppb. <-NULL
new345678_result$Lead..ppb. <- NULL

colnames(old_result)[8] <- "Street.."
colnames(old_result)[9] <- "Street.Name"
colnames(old_result)[7] <- "TestDate"
#old_result$Lead..ppb. <- old_result$Lead_Viola*1000
#old_result$Copper..ppb. <- old_result$Copper_Vio*1000
old_result$PostDate <- as.Date("01/02/2017",format="%m/%d/%Y")
combined_result <- rbind(old_result, new345678_result)
combined_result$Date_Range <- NA
combined_result$GlobalID <- NA
colnames(combined_result)[13] <- "Postal"
combined_result <- combined_result[which(!duplicated(combined_result$LABID)),]

write.csv(combined_result, "/home/bdeep/share/projects/Flint_Housing/stores/residential_testing/flintLeadTest_Sep_2019_Address.csv")

combined_result <- read.csv("/home/bdeep/share/projects/Flint_Housing/stores/residential_testing/flintLeadTest_Sep_2019_Address.csv")
colnames(combined_result)[1] <- "X.1"
write.csv(combined_result, "/home/bdeep/share/projects/Flint_Housing/stores/residential_testing/flintLeadTest_Sep_2019_Address.csv", row.names = FALSE)

res_2019 <- read.csv("/home/bdeep/share/projects/Flint_Housing/stores/residential_testing/flintLeadTest_Sep_2019_Address.csv", stringsAsFactors = FALSE)
res_2017 <- read.csv("/home/bdeep/share/projects/Flint/production/LeadTestsResult/flintLeadTest_Feb_2017.csv", stringsAsFactors = FALSE)
res_2019 <- subset(res_2019,select=-c(X250.ml.Bottle..PPB.,X750.ml.Bottle..PPB.,X1.Liter.Calculated..PPB.,X1.Liter..PPB.,X250.ml.Bottle..PPB..1,X750.ml.Bottle..PPB..1,X1.Liter.Calculated..PPB..1,X1.Liter..PPB..1,Date_Range,GlobalID))
res_2017 <- subset(res_2017, select=-c(Lead.2.Bottle.250.ml.Bottle..PPB.,Lead.2.Bottle.750.ml.Bottle..PPB.,Lead.2.Bottle..1.Liter.Calculated..PPB.,Lead.1.Bottle..1.Liter..PPB.,Copper.2.Bottle.250.ml.Bottle..PPB.,Copper.2.Bottle.750.ml.Bottle..PPB.,Copper.2.Bottle.1.Liter.Calculated..PPB.,Copper.1.Bottle.1.Liter..PPB.))
colnames(res_2017)[1] <- "X"
res_2017$Address <-NULL
res_2017$Match_addr <- NULL
res_2017$X <- NULL
colnames(res_2017)[9] <- "TestDate"
colnames(res_2019)[1] <- "X"
res_2019$X <- NULL
res_2017$Copper..ppb. <- NULL
res_2017$Lead..ppb. <- NULL
colnames(res_2017)[6] <- "Postal"
combined_result <- rbind(res_2017, res_2019)
combined_result <- combined_result[which(!duplicated(combined_result$LABID)),]

write.csv(combined_result, "/home/bdeep/share/projects/Flint/production/LeadTestsResult/flintLeadTest_merged_til_2019_Address.csv")


