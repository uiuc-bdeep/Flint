

#######################################
## Preliminaries ######################
#######################################

#######################################
## clean the enviornment
rm(list=ls())
## power parameter for the weights when estimating the lead level
p <- 2
#######################################
## package checking

setwd("~/share/projects/Flint")

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages ##
packages <- c("readxl", "data.table", "sp", "ggplot2", "scales", "plyr", "rgdal", "rgeos", "stringr", "doBy", "readstata13")
lapply(packages, pkgTest)


## Setting for Table Reading ##
options(scipen = 999) # Do not print scientific notation
options(stringsAsFactors = FALSE) ## Do not load strings as factors

## Setting the working directory ##
setwd("~/share/projects/Flint/")        # Change working directory to Flint Project
dir <- "stores/DB26"                    # Change directory to where you've stored ZTRAX

##########################################
#     
#     Step 0:
#
#     1. Adding the Census Blocks
#
############################################

################################################
## reading the census block and hedonics data ##
################################################
shape <- readOGR(dsn = path.expand("~/share/projects/zillow/stores/TIGER/tl_2015_26_tabblock10"), layer = "tl_2015_26_tabblock10")

points_raw <- read.csv("~/share/projects/zillow/production/Hedonics/HedonicsGenesee.csv")
points <- points_raw[which(!is.na(points_raw$PropertyAddressLatitude)),]

## creating the row index
points_with_id <- points
points_with_id$id <- c(1:nrow(points_with_id))

##################################################
## perform the over and transform on projection ##
##################################################
coordinates(points) <- ~ PropertyAddressLongitude + PropertyAddressLatitude
proj4string(points) <- proj4string(shape)

result <- over(points,shape)

result_with_id <- result[c("GEOID10")]
result_with_id$id <- c(1:nrow(result_with_id))

## check the id ##
if (nrow(points_with_id) != nrow(result_with_id)){
  print("lines not match")
}


total_result <- merge(points_with_id, result_with_id, by = "id")
total_result$id <- NULL

total_result$GEOID10 <- as.character(total_result$GEOID10)
colnames(total_result)[length(colnames(total_result))] <- "Census_Block_ID"

## write the file ##
WRITE_CENSUS_BLOCK = FALSE
if(WRITE_CENSUS_BLOCK){
  write.csv(total_result, "~/share/projects/zillow/production/Hedonics/HedonicsGenesee_WithBlock.csv", row.names = FALSE) 
}


##########################################
#     
#     Step 1 & 2:
#
#     1.For every transaction, find the nearest lead test that happened before the transaction date
#           record the corresponding distance, the test's OBJECTID, and the number of days between the test and the transaction
#     2.Estimate the lead level for every house using the inverse distance weighting formula
#           record the estimation seperataly with different values of power parameter p (1,2,3).
#
############################################

#######################################
## Input Section ######################
#######################################

pointsData <- read.csv("~/share/projects/zillow/production/Hedonics/HedonicsGenesee_WithBlock.csv")
leadsData <- read.csv("production/LeadTestsResult/flintLeadTest_21_Feb.csv")
analysis_0 <- pointsData

#########################################
## find the nearest positive lead test ##
#########################################

########################################
## Reading Data and Preparing 

## get rid of the one without recording date
pointsData <- pointsData[which(pointsData$RecordingDate != ""),]

points <- pointsData
leads <- leadsData

# filter out the data that don't have valid longitude/latitude ##
points <- points[!is.na(points$PropertyAddressLatitude),]        
points <- points[!is.na(points$PropertyAddressLongitude),]
leads <- leads[!is.na(leads$X),]
leads <- leads[!is.na(leads$Y),]
colnames(leads)[2] <- "OBJECTID" ## changing the index field into the correct name ##

## make data sets for coordinates ##
pointsCoords <- points
leadsCoords <- leads
coordinates(pointsCoords) <- c("PropertyAddressLongitude", "PropertyAddressLatitude")   # coordinates for all the housing points
coordinates(leadsCoords) <- c("X", "Y")                                                 # coordinates for all the lead test points

## pre-make the container for nearest positive lead tests ##
closestPointVec <- vector(mode="numeric", length=nrow(pointsCoords))          # Storing point indices
closestPointVec_2 <- vector(mode="numeric", length=nrow(pointsCoords))
minDistVec <- vector(mode="numeric", length=nrow(pointsCoords))               # Storing distances
minDistVec_2 <- vector(mode="numeric", length=nrow(pointsCoords))
NearestTestTimeInterval <- vector(mode="numeric", length=nrow(pointsCoords))  # Storing time intervals

## get the valid lead-test points & transfer into coordinates ##
validLeads_global <- leads[leads$Lead_Viola=="Yes",]                        
validLeadsCoords_all <- validLeads_global
coordinates(validLeadsCoords_all) <- c("X", "Y")

########################################
## calculating the distance/ID 

## create the fields and init the vectors ##
minDistVec[1] <- NA
minDistVec_2[1] <- NA
points$NearestLeadOBJECTID <- NA
points$NearestTestTimeInterval <- NA

## starting to loop the points to do the distance measuring ##
for (i in 1 : nrow(pointsCoords)) #nrow(pointsCoords)
{
  ## progress counter ##
  if (i %% 1000 == 0){
    print(i)
  }
  
  ## selecting only the lead tests that happened before the transaction ##
  recording_date <- as.Date(points$RecordingDate[i])
  validLeads_local <- validLeads_global[as.Date(validLeads_global$Results_Se, format="%m/%d/%Y") < recording_date,]

  ## lead tests happened before the transactions ##
  if(nrow(validLeads_local) != 0){
    
    ## make coordinate ##
    validLeadsCoords_local <- validLeads_local
    coordinates(validLeadsCoords_local) <- c("X", "Y")
  
    ## get the distances between the housing point and all the valid lead-test points, in kms ##
    distVec <- spDistsN1(validLeadsCoords_local, pointsCoords[i,], longlat=TRUE)  
    minDistVec[i] <- min(distVec)                             # find the minimum distance
    closestPointVec[i] <- validLeadsCoords_local$OBJECTID[which.min(distVec)]                  # find the index for the lead-test point that has the minimum distance
    
    ## assign the time interval into the table ##
    n_of_days <- as.numeric(recording_date - as.Date(validLeads_local$Results_Se[closestPointVec[i]], format="%m/%d/%Y"))
    points$NearestTestTimeInterval[i] <- n_of_days
  } else{
    minDistVec[i] = NA
    closestPointVec[i] = NA
    points$NearestLeadOBJECTID[i] = NA
    points$NearestTestTimeInterval[i] = NA
  }
  
  ## FOR ALL LEADTESTS, not considering the dates as a factor ##
  distVec_2 <- spDistsN1(validLeadsCoords_all, pointsCoords[i,], longlat=TRUE)  
  minDistVec_2[i] <- min(distVec_2)                                              # find the minimum distance
  closestPointVec_2[i] <- which.min(distVec_2)                                   # find the index for the lead-test point that has the minimum distance
  points$NearestLeadOBJECTID_2[i] <- validLeads_global$OBJECTID[closestPointVec_2[i]]  # append the nearestLeadOBJECTID field
  
}

points$DistToNearestTest_2 <- minDistVec_2
points$NearestLeadOBJECTID <- closestPointVec
points$DistToNearestTest <- minDistVec           # populate the DistToNearestLead field


##########################################
## find the lead level ###################
##########################################

## load data ##
points2 <- data.frame(pointsData$RecordingDate, pointsData$PropertyAddressLatitude, pointsData$PropertyAddressLongitude)   # extract the useful fields
leads2 <- data.frame(leads$Results_Se, leads$X, leads$Y, leads$Lead_mgl)

## init as coordinate ##
points2Coords <- points2
leads2Coords <- leads2
coordinates(points2Coords) <- c("pointsData.PropertyAddressLatitude", "pointsData.PropertyAddressLongitude")          # coordinates for the housing points
coordinates(leads2Coords) <- c("leads.Y", "leads.X")                                                          # coordinates for the lead-test points

## init vector used to store the predicted lead level ##
value_1 <- vector(mode="numeric", length=nrow(points))                                                                      
value_2 <- vector(mode="numeric", length=nrow(points)) 
value_3 <- vector(mode="numeric", length=nrow(points)) 
value2_1 <- vector(mode="numeric", length=nrow(points))                                                                
value2_2 <- vector(mode="numeric", length=nrow(points)) 
value2_3 <- vector(mode="numeric", length=nrow(points)) 

## do the lead level computation
for (i in 1 : nrow(points)) # It is a time consuming step #
{
  ## progress indicator ##
  if (i %% 1000 == 0){
    print(i)
  }                         

##############################################################
#### get the transactions that happened after the lead test ##
  temp <- leads2[as.Date(leads2$leads.Results_Se, format="%m/%d/%Y") < as.Date(points2$pointsData.RecordingDate[i]),]
  
  ## calculating the predicated lead level ##
  if (nrow(temp) > 0)
  {
    temp2 <- temp
    coordinates(temp) <- c("leads.Y","leads.X")                          # coordinates for the valid lead test location
    distVec <- spDistsN1(temp, points2Coords[i, ], longlat=TRUE)         # get the distances between the housing point and all the valid lead-test points
    distVec <- distVec * 1000                                            # km -> m
    weights_1 <- 1/(distVec^1)                                           # get the weight coefficients
    weights_2 <- 1/(distVec^2)
    weights_3 <- 1/(distVec^3)
    value_1[i] <- sum(temp2$leads.Lead_mgl * weights_1)/sum(weights_1)   # predicated lead-level for the current housing point
    value_2[i] <- sum(temp2$leads.Lead_mgl * weights_2)/sum(weights_2)
    value_3[i] <- sum(temp2$leads.Lead_mgl * weights_3)/sum(weights_3)
  } else {
    value_1[i] <- 0
    value_2[i] <- 0
    value_3[i] <- 0
  }
  
########################################################
###### estimating lead levels given all the lead tests
  distVec_2 <- spDistsN1(leads2Coords, points2Coords[i, ], longlat=TRUE)       # get the distances between the housing point and all the valid lead-test points
  distVec_2 <- distVec_2 * 1000                                            # km -> m
  weights2_1 <- 1/(distVec_2^1)                                             # get the weight coefficients
  weights2_2 <- 1/(distVec_2^2)
  weights2_3 <- 1/(distVec_2^3)
  value2_1[i] <- sum(leads2$leads.Lead_mgl * weights2_1)/sum(weights2_1)    # predicated lead-level for the current housing point
  value2_2[i] <- sum(leads2$leads.Lead_mgl * weights2_2)/sum(weights2_2)
  value2_3[i] <- sum(leads2$leads.Lead_mgl * weights2_3)/sum(weights2_3)
  
  
}

points$LeadLevel_1 <- value_1                     # populate the LeadLevel field
points$LeadLevel_2 <- value_2
points$LeadLevel_3 <- value_3
points$LeadLevel2_1 <- value2_1                     # populate the LeadLevel field
points$LeadLevel2_2 <- value2_2
points$LeadLevel2_3 <- value2_3

analysis_2 <- points

###########################
## write file (optional) ##
WRITETOFILE = TRUE     # if true, write to file
if (WRITETOFILE)
{
  write.csv(points, file="~/share/projects/Flint/production/intermediate_output/Analysis_2_March.csv", row.names = FALSE) 
}


##########################################
#     
#     Step 3: Nearest Positive-Null LeadTest in range
#
#     3. Finding the Nearest Positive-Null LeadTest in range. Using the different distances to calculate the number of 
#        leadtest places are null or not.
#
############################################


################################################
## initalize the distance in different places ##
################################################

## unit is km ##
dist1 <- 0.25
dist2 <- 0.5
dist3 <- 1
dist4 <- 1.5
dist5 <- 2
dist6 <- 4

dists <-
  c(dist1, dist2, dist3, dist4, dist5, dist6) # all the distances to loop through
name1_base <- "positive"
name2_base <- "null"


## initalize the input data tables ##
transactions <- analysis_2
lead_tests <- leadsData

## only enable these two lines when we are running on the whole hedonics file

#transactions[,positive_var] <- NULL
#transactions[,null_var] <- NULL



## steps to preparing for the geo opeartions ##
points <- transactions
leads <- lead_tests

points <- points[as.Date(points$RecordingDate) >= as.Date("2015-02-25"),]  #only the transactions after the first lead test

points <- points[!is.na(points$PropertyAddressLatitude),]        # filter out the data that don't have valid longitude/latitude
points <- points[!is.na(points$PropertyAddressLongitude),]
leads <- leads[!is.na(leads$X),]
leads <- leads[!is.na(leads$Y),]

## creating the coordiates ##
pointsCoords <- points
leadsCoords <- leads

coordinates(pointsCoords) <- c("PropertyAddressLongitude", "PropertyAddressLatitude")   # coordinates for all the housing points
coordinates(leadsCoords) <- c("X", "Y")                                                 # coordinates for all the lead test points

## finding the number of positive lead tests ##
for (dist in dists)
{
  dist_s <- toString(dist)
  positive_var <- paste(name1_base, dist_s, sep="_")
  null_var <- paste(name2_base, dist_s, sep="_")
  positive_var <- paste(positive_var, "km", sep="")
  null_var <- paste(null_var, "km", sep="")
  
  ## points == trans data, 
  for (i in 1:nrow(points))
  {
    if (i %% 1000 == 1)
      print(i)
    distVec <- spDistsN1(leadsCoords, pointsCoords[i, ], longlat=TRUE) 
    if (length(distVec > 0))
    {
      leads_local <- leads[distVec <= dist,]
      leads_local <- leads_local[which(as.Date(leads_local$Results_Se, format="%m/%d/%Y") < as.Date(points[i,]$RecordingDate)),] ## count all leadtest before trans happening
      
      yes_temp <- nrow(leads_local[leads_local$Lead_Viola=="Yes",])
      no_temp <- nrow(leads_local[leads_local$Lead_Viola=="No",])
    }
    points[i, positive_var] <-  yes_temp
    points[i, null_var] <- no_temp
  }
}

## remove the extra indexes ##
drop_list <- c("X.1", "X.2", "X.3")
transactions <- transactions[,!(colnames(transactions) %in% drop_list)]
points <- points[,!(names(points) %in% drop_list)]

## insert additional fields on transactions ##
fields <- colnames(points)[which(!(names(points) %in% names(transactions)))]
transactions[,fields] <- NA
for(i in 1:nrow(points)){
  transactions[which(points$TransId[i] == transactions$TransId), c(fields)] <- points[i, c(fields)]
}

analysis_3 <- transactions

## writing the file ##
WRITETOFILE = FALSE     # if true, write to file
if (WRITETOFILE)
{
  write.csv(transactions, "~/share/projects/Flint/production/intermediate_output/Analysis_3_March.csv", row.names = FALSE)
}

## AT THIS POINT ##

##########################################
#     
#     Step 4: Error Estimation
#
#     4. Using the code provided by Renato, finding the estimated error:
#        - count the number of tests within a certain km by the transaction date
#        - count the number of positive tests within a certain km by the transaction date
#        - calculate the cumulative probability of having a positive lead test up to the transaction date.
#
############################################

## setting the numbers for points estimate error in different ranges ##
dist1 <- 0.25
dist2 <- 0.5
dist3 <- 1
dist4 <- 1.5
dist5 <- 2
dist6 <- 4

dists <- c(dist1, dist2, dist3, dist4, dist5, dist6) # all the distances to loop through
name1_base <- "expected_leadLevel"
name2_base <- "standard_error"

## read the file input ##
transactions <- analysis_3

leadsData <- leadsData[which((leadsData$LABID) != ""),]
leadData_merged_all <- leadsData


#drop <- c("cumulative.prob","dev","positive.tests_byDay", "tests_byDay", "deviation_byDay", "X.1", "X.2")
#transactions <- transactions[,!(names(transactions) %in% drop)]

o_transactions <- analysis_3     # o_transactions == original dataset

## remove all the transactions before the first lead test date ##
transactions <- transactions[as.Date(transactions$RecordingDate) > as.Date("2015-02-25"), ]

lead_tests <- leadData_merged_all

## read data and change into coordinates
trans_addr <- transactions
tests_addr <- leadData_merged_all
trans_addr <- trans_addr[!is.na(trans_addr$PropertyAddressLatitude), ]        # filter out the data that don't have valid longitude/latitude
trans_addr <- trans_addr[!is.na(trans_addr$PropertyAddressLongitude), ]
tests_addr <- tests_addr[!is.na(tests_addr$X), ]
tests_addr <- tests_addr[!is.na(tests_addr$Y), ]
trans_coords <- trans_addr
tests_coords <- tests_addr
coordinates(trans_coords) <- c("PropertyAddressLongitude", "PropertyAddressLatitude")   # coordinates for all the housing points
coordinates(tests_coords) <- c("X", "Y")


########################################
## start doing the dist iteration
for (dist in dists)
{
  ## creating the names of the fields based on different distance to lead test ##
  dist_s <- toString(dist)
  expected_LeadLevel <- paste(name1_base, dist_s, sep = "_")
  cumulative.prob <- paste(expected_LeadLevel, "km", sep = "")
  transactions[, cumulative.prob] <- NA
  #o_transactions[, cumulative.prob] <- NA # created at merging
  standard_error <- paste(name2_base, dist_s, sep = "_")
  dev <- paste(standard_error, "km", sep = "")
  transactions[, dev] <- NA
  #o_transactions[, dev] <- NA # created at merging
  
  
  transactions[, cumulative.prob] <- NA
  transactions[, dev] <- NA
  
  
  # add to plot -- red vertical line at date of transaction i
  # output cumulative.prob, cumulative.standard.deviation for transaction i, by date i and date of last test
  
  for (j in 1:nrow(transactions))
  {
    if (j %% 1000 == 1) {
      print(j)
    }
    ## valid tests = lead test before transaction ##
    valid_tests <- lead_tests[as.Date(lead_tests$Results_Se, format = "%m/%d/%Y") < as.Date(transactions[j, ]$RecordingDate), ]
    temp_tests_coords <- valid_tests
    temp_tests_coords <- temp_tests_coords[!is.na(temp_tests_coords$X), ]
    temp_tests_coords <- temp_tests_coords[!is.na(temp_tests_coords$Y), ]
    coordinates(temp_tests_coords) <- c("X", "Y")
    distVec <- spDistsN1(temp_tests_coords, trans_coords[j, ], longlat = TRUE)
    
    ## log is valid_tests inside of range of the parcel ##
    log <- valid_tests[distVec < dist,]
    #nr <- nrow(valid)
    
    
    # there exists lead tests happened before the transactions #
    if (nrow(log) != 0)
    {
      # merge by distance 1km for transaction i
      # subset leadData_merged
      
      ## assign date ##
      log$Date <- as.Date(log$Results_Se, "%m/%d/%Y")
      
      # Create test dummy indicator
      log$test <- as.integer(1)
      
      # agreggate results by day
      log$total_leadtest_bydate <- ave(log$Lead_mgl, log$Date, FUN = sum)
      log$total_num_tests <- ave(log$test, log$Date, FUN = sum)
      
      # subset of results by day
      daily <- log[!duplicated(log$Date), ]
      daily <- daily[as.Date(daily$Date) <= as.Date(transactions[j, ]$RecordingDate),]
      
      # calculate cumulative probability
      n <- as.integer(nrow(daily))
      daily$cumulative.prob <- 0
      
      for (i in 1:n) {                # n = number of daily
        my.date <- daily$Date[i]
        
        cumul_tests <- sum(daily[which(daily$Date <= my.date), "total_num_tests"])
        cumul_positive.tests <- sum(daily[which(daily$Date <= my.date), "total_leadtest_bydate"])
        
        daily[i, "cumulative.prob"] <- cumul_positive.tests / cumul_tests
      }
      
      
      # collect daily mu
      daily.m <- daily[, c("Date", "cumulative.prob")]
      
      # bring back mu to original dataset
      log <- merge(log, daily.m, by = "Date")
      
      # calculate deviation to each obs
      log$dev <- abs(log$Lead_mgl - log$cumulative.prob)
      
      # calculate cumulative standard deviation
      log$deviation_byDay <- ave(log$dev, log$Date, FUN = sum)
      
      # subset of results by day
      daily <- log[!duplicated(log$Date), ]
      
      # calculate cumulative probability
      n <- as.integer(nrow(daily))
      daily$cumulative.standard.error <- 0
      
      ## finding the error ##
      for (i in 1:n)
      {
        my.date <- daily$Date[i]
        
        cumul_tests <- sum(daily[which(daily$Date <= my.date), "total_num_tests"])
        cumul_deviation <- sum(daily[which(daily$Date <= my.date), "deviation_byDay"])
        #!!!!!
        daily[i, "cumulative.standard.error"] <- 
          (cumul_deviation / cumul_tests) / sqrt(cumul_tests) # -> cumulative standard error
      }
      
      # collect daily mu
      daily.m <- daily[, c("Date", "cumulative.standard.error")]
      
      # bring back mu to original dataset
      log <- merge(log, daily.m, by = "Date")
      
      ## add the statistics back to the transaction dataset ##
      ## the last day is the most accurate number           ##
      
      transactions[j, dev] <- daily[which(max(as.numeric(daily$Date)) == daily$Date), ]$cumulative.standard.error           # dev is error here #                                
      transactions[j, cumulative.prob] <- daily[which(max(as.numeric(daily$Date)) == daily$Date), ]$cumulative.prob
    }
    
  }
  
  
}## end of for loop for all fields of whole table ##

## TODO: merge back into the main fields ##
m_trans <- transactions[,c(2,89:100)]
m_trans <- m_trans[which(!duplicated(m_trans$TransId)),]
transactions_96 <- merge(o_transactions, m_trans, by = "TransId", all.x = TRUE)


WRITETOFILE = FALSE     # if true, write to file
if (WRITETOFILE)
{
  write.csv(transactions_96, "~/share/projects/Flint/production/intermediate_output/Analysis_4_March.csv", row.names = FALSE)
}

analysis_4 <- transactions_96


##########################################
#     
#     Step 5: point to edge of the service line
#
#     5. Adding distance from points to the edge of service line.
#
############################################



##################################################################
## Preliminaries
points_raw <- analysis_4
points <- points_raw[which(!is.na(points_raw$PropertyAddressLatitude)),]

shpName <- "~/share/projects/Flint/stores/flintBoundary/"
shpDir <- path.expand(shpName)
layerName <- "flintBoundary"

ogrInfo(dsn = shpDir, layer = layerName)
shp_poly <- readOGR(shpDir, layerName)


coordinates(points) <- ~ PropertyAddressLongitude + PropertyAddressLatitude
proj4string(points) <- CRS("+proj=longlat")

points <- spTransform(points, proj4string(shp_poly))

proj4string(points) <- proj4string(shp_poly)

#res <- over(points, shp_poly)

## performing the point to line ##
m <- gDistance(points, shp_poly, byid=TRUE) # unit is feet

points$distanceToBoundary <- m[1, ]
#writeSpatialShape(points, "~/share/projects/Flint/production/outputs/temp/temp.shp") 

points_raw$distanceToBoundary <- m[1, ]


##################################################################
## Get information from Service Line Connection Type
#points_raw <- analysis_4
#points <- points_raw[which(!is.na(points_raw$PropertyAddressLatitude)),]

points <- points_raw[which(!is.na(points_raw$PropertyAddressLatitude)),]

shpName <- "~/share/projects/Flint/stores/2015ParcelswConnType/"
shpDir <- path.expand(shpName)
layerName <- "2015ParcelswConnType"

ogrInfo(dsn = shpDir, layer = layerName)
shp_poly <- readOGR(shpDir, layerName)


coordinates(points) <- ~ PropertyAddressLongitude + PropertyAddressLatitude
proj4string(points) <- CRS("+proj=longlat")

points <- spTransform(points, proj4string(shp_poly))

proj4string(points) <- proj4string(shp_poly)

res <- over(points, shp_poly)

#plot(shp_poly)
#plot(points_res$PropertyAddressLatitude ~ points_res$PropertyAddressLongitude, col = "red", cex = 1)

## after finding result re-read the file with same order and same format. preparing to attach the result:

points_res <- points_raw
points_res <- points_res[which(!is.na(points_res$PropertyAddressLatitude)),]

points_res$ParcelswConnTypeID <- res$OBJECTID
points_res$SL_Type <- res$SL_Type
points_res$SL_Type2 <- res$SL_Type2
points_res$SL_Lead <- res$SL_Lead
points_res$ServiceLine <- 0
points_res$ServiceLine[which(!is.na(points_res$ParcelswConnTypeID))] <- 1



write.csv(points_res, "~/share/projects/Flint/production/intermediate_output/Analysis_5_March.csv", row.names = FALSE)

analysis_5 <- points_res




##########################################
#     
#     Step 6: LeadTest on Actual Parcel
#
#     6. Adding the fields of lead test mean on actual Parcels
#
############################################



leadData <- leadsData
#transData <- read.table("~/share/projects/Flint/production/AllVariables_0913.csv", header = TRUE, sep = ",")
transData <- analysis_5
transData_raw <- analysis_5

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

## adding another variable that is coming from previous value

transData_raw$Tested_House <- NA

for (i in 1:nrow(sum_lead)){
  
  transData_raw$Tested_House[which(transData_raw$TransId == sum_lead$TransId[i])] <- sum_lead$Lead_mgl.mean[i]
  
}

#transData <- as.data.frame(transData)

#transData$NewAddress <- NULL

analysis_6 <- transData_raw

WRITETOFILE = FALSE     # if true, write to file
if (WRITETOFILE)
{
  write.csv(transData_raw, "~/share/projects/Flint/production/intermediate_output/Analysis_6_March.csv", row.names = FALSE)
}

##########################################
#     Step 7:
#     By Yifang Zhang
#
#
#     Goal:     Finding the Mean of Lead Level of Census Blocks in Flint
#               Formula: Mean of Lead Level = Total Lead Level in Census Block / Number of Lead Tests
#
#     Input:    Most Recent Lead Test Table:      
#                         ~/share/projects/Flint/production/LeadTestsResult/leadData_merged_Dec.csv
#               Required Census Block Table:      
#                         ~/share/projects/zillow/stores/TIGER/tl_2015_26_tabblock10
#               Hedonics Dataset in Flint:        
#                         ~/share/projects/Flint/production/outputs/GeneseeHedonics_Dec15.csv
#
#     Output:   Hedonics Dataset in Flint with the Mean of Lead Level of Census Blocks:
#                         
#
############################################

## Reading the Data ##
leadsData_raw <- leadsData
hedonics_raw <- analysis_6
shape <- readOGR(dsn = path.expand("~/share/projects/zillow/stores/TIGER/tl_2015_26_tabblock10"), layer = "tl_2015_26_tabblock10")



## perform the over function to map LeadTests and CensusBlocks in same projection ##
leadsData <- leadsData_raw[which(!is.na(leadsData_raw$X)),]

coordinates(leadsData) <- ~ X + Y
proj4string(leadsData) <- proj4string(shape)

# result index == lead index
# 
result <- over(leadsData,shape) 


#plot(shape)
#plot(leadsData, col = "red", cex = 1)


## analyzing the ID of census blocks ##

# 1. finding the mean for each census block
census <- shape 
leadsData$CensusID <- result$GEOID10

census$acc_lead <- 0
census$num_leadtest <- 0
for (i in 1:nrow(leadsData)){
  if(!is.na(leadsData$CensusID[i]) & !is.na(leadsData$Lead_mgl[i])){
    matchedID <- which(leadsData$CensusID[i] == census$GEOID10)
    census$acc_lead[matchedID] <- census$acc_lead[matchedID] + leadsData$Lead_mgl[i]
    census$num_leadtest[matchedID] <- census$num_leadtest[matchedID] + 1
  }
  #if (i %% 1000 == 0){
  print(i)
  #}
}

census$Lead_Mean <- census$acc_lead / census$num_leadtest
census$Lead_Mean[which(is.infinite(census$Lead_Mean) | is.nan(census$Lead_Mean))] <- NA


# 2. appending this info on Hedonics data

hedonics_raw$Census_Block_Lead_Mean <- NULL

temp_census <- data.frame(census)
temp_census <- subset(temp_census, select = c("GEOID10", "Lead_Mean", "STATEFP10", "COUNTYFP10", "TRACTCE10", "BLOCKCE10"))
colnames(temp_census) <- c("Census_Block_ID", "Census_Block_Lead_Mean", "statece10", "countyce10", "tractce10", "blockce10")

hedonics_result <- merge(hedonics_raw, temp_census, by=c("Census_Block_ID"), all.x=TRUE)
hedonics_result2 <- hedonics_result[, c(2:107,1,108:112)]
hedonics_result2$Census_Block_ID <- as.character(hedonics_result2$Census_Block_ID)


analysis_7 <- hedonics_result2

write.csv(hedonics_result2, "~/share/projects/Flint/production/intermediate_output/Analysis_7_March.csv")


##################################################################
## Get information from Service Line Connection Type (new service line)

points_raw <- analysis_7
points <- points_raw[which(!is.na(points_raw$PropertyAddressLatitude)),]

shpName <- "~/share/projects/Flint/stores/SL_Conn_11_7/"
shpDir <- path.expand(shpName)
layerName <- "SL_Conn_11_7"

ogrInfo(dsn = shpDir, layer = layerName)
shp_poly <- readOGR(shpDir, layerName)


coordinates(points) <- ~ PropertyAddressLongitude + PropertyAddressLatitude
proj4string(points) <- CRS("+proj=longlat")

points <- spTransform(points, proj4string(shp_poly))

proj4string(points) <- proj4string(shp_poly)

shp_poly$ParcelswConnTypeID_New <- seq.int(nrow(shp_poly))

res <- over(points, shp_poly)

#plot(shp_poly)
#plot(points_res$PropertyAddressLatitude ~ points_res$PropertyAddressLongitude, col = "red", cex = 1)

## after finding result re-read the file with same order and same format. preparing to attach the result:

points_res <- points_raw
points_res <- points_res[which(!is.na(points_res$PropertyAddressLatitude)),]

points_res$ParcelswConnTypeID_New <- res$ParcelswConnTypeID_New
points_res$Curr_Conn <- res$Curr_Conn
points_res$ConnSource <- res$ConnSource
points_res$ServiceLine_New <- 0
points_res$ServiceLine_New[which(!is.na(points_res$ParcelswConnTypeID))] <- 1


analysis_8 <- points_res

write.csv(analysis_8, "~/share/projects/Flint/production/intermediate_output/Analysis_8_March.csv")

#################################################################
## done for new service line, starting to add buyer and seller ##
#################################################################

## Setting the working directory
setwd("~/share/projects/migration/")


#####################################################################################
## reading the dataset
movingTable <- read.csv("~/share/projects/migration/production/movingTable_Jan4.csv", stringsAsFactors = FALSE)

mainTable <- analysis_8


## flint and serviceline ##
movingTable$BuyerFlint <- 0
movingTable$BuyerFlint[which(movingTable$BuyerMailCity == "FLINT" & movingTable$BuyerMailState == "MI")] <- 1

movingTable$BuyerGenesee <- 0
movingTable$BuyerGenesee[which(movingTable$FIPS.x == 26049)] <- 1

movingTable$BuyerFlint_total <- 1

movingTable$SellerFlint <- 0
movingTable$SellerFlint[which(movingTable$SellerMailCity == "FLINT" & movingTable$SellerMailState == "MI")] <- 1

movingTable$SellerGenesee <- 0
movingTable$SellerGenesee[which(movingTable$FIPS.y == 26049)] <- 1

movingTable$SellerFlint_total <- 1

## do the aggregation ##
movingTable_sub <- subset(movingTable, select = c("TransId", "BuyerFlint", "SellerFlint", "BuyerGenesee", "SellerGenesee", "BuyerFlint_total", "SellerFlint_total"))

movingTable_sub <- aggregate(cbind(movingTable_sub$BuyerFlint, movingTable_sub$SellerFlint, movingTable_sub$BuyerGenesee, movingTable_sub$SellerGenesee, movingTable_sub$BuyerFlint_total, movingTable_sub$SellerFlint_total), by=list(movingTable_sub$TransId), FUN=sum)
colnames(movingTable_sub) <- c("TransId", "BuyerFlint", "SellerFlint", "BuyerGenesee", "SellerGenesee", "BuyerFlint_total", "SellerFlint_total")

## adding fields ##
res <- merge(mainTable, movingTable_sub, by = c("TransId"), all.x = T)

res$BuyerFlint[which(is.na(res$BuyerFlint))] <- 0
res$BuyerGenesee[which(is.na(res$BuyerGenesee))] <- 0
res$BuyerFlint_total[which(is.na(res$BuyerFlint_total))] <- 0
res$SellerFlint[which(is.na(res$SellerFlint))] <- 0
res$SellerGenesee[which(is.na(res$SellerGenesee))] <- 0
res$SellerFlint_total[which(is.na(res$SellerFlint_total))] <- 0

res$X <- NULL

analysis_9 <- res

write.csv(analysis_9, "~/share/projects/Flint/production/intermediate_output/Analysis_9_March.csv", row.names = FALSE)

#################################################################



write.csv(analysis_9, "~/share/projects/Flint/production/outputs/GeneseeHedonics_March_5.csv", row.names = FALSE)


rm(list= ls()[!(ls() %in% c('analysis_0','analysis_1','analysis_2','analysis_3','analysis_4','analysis_5','analysis_6', 'analysis_7', 'analysis_8', 'analysis_9', 'leadsData'))])

