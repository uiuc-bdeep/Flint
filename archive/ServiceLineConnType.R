#     ------------------------------------------------------------------------
#   |                                                                         |
#   |  find Service Line Connection Type                                      |
#   |                                                                         |
#   |  By:                                                                    |
#   |  Yifang Zhang                                                           |                            
#   |  University of Illinois at Urbana Chamapaign                            |
#   |                                                                         |
#     ------------------------------------------------------------------------


##################################################################
## Preliminaries
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
packages <- c("readxl", "data.table", "rgdal", "sp", "rgeos")
lapply(packages, pkgTest)


##################################################################
## Preliminaries
points_raw <- read.csv("~/share/projects/Flint/production/outputs/flint_hedonics_1021_withoutConn.csv")
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

write.csv(points_res, "~/share/projects/Flint/production/outputs/Flint_Hedonics_1021.csv", row.names = FALSE)

points_had <- points_res[which(!is.na(points_res$ParcelswConnTypeID)),]
#write.csv(points_had, "~/share/projects/Flint/production/Parcels_Conn/Flint_Hedonic_hadConnectType.csv", row.names = FALSE)

########################################################################
## field for those are not in the CONN_TYPE

FIND_NEAREST_DISTANCE = FALSE
if(FIND_NEAREST_DISTANCE == TRUE){
  
  setSessionTimeLimit(cpu = Inf, elapsed = Inf)
  
  points_notin <- points[which(is.na(res$OBJECTID)),]
  points_res$nearestConnID <- points_res$ParcelswConnTypeID
  points_res$nearestConnDist <- 0
  
  for(i in 1:nrow(points_notin)){
    
    dist <- (gDistance(points_notin[i,], shp_poly, byid=TRUE))
    min_dist <- min(dist)
    min_id <- which(dist == min_dist)
    min_poly <- shp_poly[min_id,]
    points_res$nearestConnID[which(points_res$TransId == points_notin$TransId[i])] <- min_poly$OBJECTID
    points_res$nearestConnDist[which(points_res$TransId == points_notin$TransId[i])] <- min_dist
    if(i%%1000 == 0){
      print(i)
    }
  }
}

