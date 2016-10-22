##########################################
#     1.For every transaction, find the nearest lead test that happened before the transaction date
#           record the corresponding distance, the test's OBJECTID, and the number of days between the test and the transaction
#     2.Estimate the lead level for every house using the inverse distance weighting formula
#           record the estimation seperataly with different values of power parameter p (1,2,3).
#
############################################

#######################################
## input section ######################
#######################################



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

## These lines load the required packages
packages <- c("readxl", "data.table", "sp")
lapply(packages, pkgTest)

#######################################
## These lines set several options

options(scipen = 999) # Do not print scientific notation
options(stringsAsFactors = FALSE) ## Do not load strings as factors


#######################################
## Setting the working directory

# setwd("/Volumes/share/projects/Flint/") # working directory
#setwd("share/projects/Flint/")
dir <- "stores/DB26"                    # Change directory to where you've stored ZTRAX


#######################################
# create file connection and read the files
# ##path
#points_data <- read.csv("stores/DB26/GeneseeZillow/geoFinalResult.csv")
#pointsData <- read.csv("production/crawled.csv")
# newly crawled data
# pointsData <- read.csv("production/new_data__.csv")
# whole hedonics data
pointsData <- read.csv("production/inputs/Flint_original_data.csv")


#leadsData <- read.csv("stores/LeadTestsData/LeadTestsLatLonFixed.csv")
leadsData <- read.csv("production/LeadTestsResult/leadData_merged.csv")  

#########################################
## find the nearest positive lead test ##
#########################################

#########################################
## load data 

points <- pointsData
leads <- leadsData
points <- points[!is.na(points$PropertyAddressLatitude),]        # filter out the data that don't have valid longitude/latitude
points <- points[!is.na(points$PropertyAddressLongitude),]
leads <- leads[!is.na(leads$X),]
leads <- leads[!is.na(leads$Y),]
pointsCoords <- points
leadsCoords <- leads
coordinates(pointsCoords) <- c("PropertyAddressLongitude", "PropertyAddressLatitude")   # coordinates for all the housing points
coordinates(leadsCoords) <- c("X", "Y")                                                 # coordinates for all the lead test points

require(sp)

closestPointVec <- vector(mode="numeric", length=nrow(pointsCoords))        # vector used to store the point indices
minDistVec <- vector(mode="numeric", length=nrow(pointsCoords))             # vector used to store the distances
NearestTestTimeInterval <- vector(mode="numeric", length=nrow(pointsCoords))

closestPointVec_2 <- vector(mode="numeric", length=nrow(pointsCoords))        # vector used to store the point indices
minDistVec_2 <- vector(mode="numeric", length=nrow(pointsCoords))             # vector used to store the distances



validLeads_global <- leads[leads$Lead_Viola=="Yes",]                               # get the valid lead-test points
validLeadsCoords_all <- validLeads_global
coordinates(validLeadsCoords_all) <- c("X", "Y")
########################################
## calculate the distance/ID 

{
  minDistVec[1] = NA
  points$NearestLeadOBJECTID[1] = NA
  points$NearestTestTimeInterval[1] = NA
}
for (i in 1 : nrow(pointsCoords))
{
  if (i %% 1000 == 0)         # progress counter
  {
    print(i)
  }
  recording_date <- as.Date(points$RecordingDate[i])
  # selecting only the lead tests that happened before the transaction
  validLeads_local <- validLeads_global[as.Date(validLeads_global$Results_Se, format="%m/%d/%Y") < recording_date,]
  validLeadsCoords_local <- validLeads_local
  # lead tests happened before the transactions
  if (nrow(validLeadsCoords_local) != 0) 
  {
    coordinates(validLeadsCoords_local) <- c("X", "Y")
    # get the distances between the housing point and all the valid lead-test points, in kms
    distVec <- spDistsN1(validLeadsCoords_local, pointsCoords[i,], longlat=TRUE)  
    minDistVec[i] <- min(distVec)                                              # find the minimum distance
    closestPointVec[i] <- which.min(distVec)                                   # find the index for the lead-test point that has the minimum distance
    #points$NearestLeadOBJECTID[i] <- validLeads_local$OBJECTID[closestPointVec[i]]  # append the nearestLeadOBJECTID field
    n_of_days <- as.numeric(recording_date - as.Date(validLeads_local$Results_Se[closestPointVec[i]], format="%m/%d/%Y"))  
    points$NearestTestTimeInterval[i] <- n_of_days
   
  } else {
    minDistVec[i] = NA
    #points$NearestLeadOBJECTID[i] = NA
    points$NearestTestTimeInterval[i] = NA
  }
  #############################################
  ######all lead tests
  ###########################################
  distVec_2 <- spDistsN1(validLeadsCoords_all, pointsCoords[i,], longlat=TRUE)  
  minDistVec_2[i] <- min(distVec_2)                                              # find the minimum distance
  closestPointVec_2[i] <- which.min(distVec_2)                                   # find the index for the lead-test point that has the minimum distance
  #points$NearestLeadOBJECTID_2[i] <- validLeads_global$OBJECTID[closestPointVec_2[i]]  # append the nearestLeadOBJECTID field
  
  
}
points$DistToNearestTest <- minDistVec           # populate the DistToNearestLead field
points$DistToNearestTest_2 <- minDistVec_2

##########################################
## find the lead level ###################
##########################################

##########################################
## load data

points2 <- data.frame(pointsData$RecordingDate, pointsData$PropertyAddressLatitude, pointsData$PropertyAddressLongitude)   # extract the useful fields
leads2 <- data.frame(leads$Results_Se, leads$X, leads$Y, leads$Lead_mgl)
#leads2 <- leads2[!is.na(leads2$X),]
#leads2 <- leads2[!is.na(leads2$Y),]
points2Coords <- points2
leads2Coords <- leads2
coordinates(points2Coords) <- c("pointsData.PropertyAddressLatitude", "pointsData.PropertyAddressLongitude")          # coordinates for the housing points
coordinates(leads2Coords) <- c("leads.Y", "leads.X")                                                          # coordinates for the lead-test points


#############################################
## use inverse distance to find lead level 

value_1 <- vector(mode="numeric", length=nrow(points))          # vector used to store the predicted lead level                                                           
value_2 <- vector(mode="numeric", length=nrow(points)) 
value_3 <- vector(mode="numeric", length=nrow(points)) 

value2_1 <- vector(mode="numeric", length=nrow(points))          # vector used to store the predicted lead level                                                           
value2_2 <- vector(mode="numeric", length=nrow(points)) 
value2_3 <- vector(mode="numeric", length=nrow(points)) 
#for (i in 82600:nrow(points))
for (i in 1 : nrow(points))
{
  if (i %% 1000 == 0)                         # progress indicator
    print(i)
  temp <- leads2[as.Date(leads2$leads.Results_Se, format="%m/%d/%Y") < as.Date(points2$pointsData.RecordingDate[i]),]   # get the transactions that happened after the lead test
  if (nrow(temp) > 0)
  {
    temp2 <- temp
    coordinates(temp) <- c("leads.Y","leads.X")                # coordinates for the valid lead test location
    distVec <- spDistsN1(temp, points2Coords[i, ], longlat=TRUE)       # get the distances between the housing point and all the valid lead-test points
    distVec <- distVec * 1000                                            # km -> m
    weights_1 <- 1/(distVec^1)                                             # get the weight coefficients
    weights_2 <- 1/(distVec^2)
    weights_3 <- 1/(distVec^3)
    value_1[i] <- sum(temp2$leadsData.Lead_mgl * weights_1)/sum(weights_1)    # predicated lead-level for the current housing point
    value_2[i] <- sum(temp2$leadsData.Lead_mgl * weights_2)/sum(weights_2)
    value_3[i] <- sum(temp2$leadsData.Lead_mgl * weights_3)/sum(weights_3)
  }
  else
  {
    value_1[i] <- 0
    value_2[i] <- 0
    value_3[i] <- 0
  }
  
  #########################
  #### estimating lead levels given all the lead tests
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
##################################################
# write the table to the file in the production folder under Flint/
WRITETOFILE = TRUE     # if true, write to file
if (WRITETOFILE)
{
  #write.csv(points, file="/Volumes/share/projects/Flint/production/crawledWithLeadLevel.csv")
 # write.csv(points, file="production/new_data_leads.csv") 
  write.csv(points, file="production/intermediate_output/Flint_Analysis_1_2.csv") 
}
