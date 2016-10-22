#Yes and noes for lead tests happened before the transactions

rm(list=ls())
whole_set = TRUE        # if we are running the script on the whole data set
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

options(scipen = 999) # Do not print scientific notation
options(stringsAsFactors = FALSE) ## Do not load strings as factors

{
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
}

##path
# "Flint_analysis_1_2.csv"
transactions <- read.csv("production/intermediate_output/Flint_Analysis_1_2.csv")
lead_tests <- read.csv("production/LeadTestsResult/leadData_merged.csv")


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## only enable these two lines when we are running on the whole hedonics file
if (whole_set == TRUE)
{
  transactions[,positive_var] <- 0
  transactions[,null_var] <- 0
}
###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
points <- transactions
leads <- lead_tests

points <- points[as.Date(points$RecordingDate) >= as.Date("2015-02-25"),]  #only the transactions after the first lead test

points <- points[!is.na(points$PropertyAddressLatitude),]        # filter out the data that don't have valid longitude/latitude
points <- points[!is.na(points$PropertyAddressLongitude),]
leads <- leads[!is.na(leads$X),]
leads <- leads[!is.na(leads$Y),]
pointsCoords <- points
leadsCoords <- leads
coordinates(pointsCoords) <- c("PropertyAddressLongitude", "PropertyAddressLatitude")   # coordinates for all the housing points
coordinates(leadsCoords) <- c("X", "Y")                                                 # coordinates for all the lead test points

require(sp)
for (dist in dists)
{
  dist_s <- toString(dist)
  positive_var <- paste(name1_base, dist_s, sep="_")
  null_var <- paste(name2_base, dist_s, sep="_")
  positive_var <- paste(positive_var, "km", sep="")
  null_var <- paste(null_var, "km", sep="")
  
  
  for (i in 1:nrow(points))
  {
    if (i %% 1000 == 1)
      print(i)
    distVec <- spDistsN1(leadsCoords, pointsCoords[i, ], longlat=TRUE) 
    if (length(distVec > 0))
    {
      leads_local <- leads[distVec <= dist,]
      leads_local <- leads_local[as.Date(leads_local$Results_Se, format="%m/%d/%Y") < points[i,]$RecordingDate,]
      
      yes_temp <- nrow(leads_local[leads_local$Lead_Viola=="Yes",])
      no_temp <- nrow(leads_local[leads_local$Lead_Viola=="No",])
    }
    points[i, positive_var] <-  yes_temp
    points[i, null_var] <- no_temp
  }
}
transactions <- transactions[,-c(1)]
points <- points[,-c(1)]
transactions[points$X,] = points



write.csv(transactions, "production/intermediate_output/Flint_Analysis_3.csv")
