# Prelimilary
rm(list=ls())
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c("readxl", "data.table", "rgdal", "sp")
lapply(packages, pkgTest)

abbr <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", 
          "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", 
          "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", 
          "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", 
          "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", 
          "WY", "AS", "GU", "MP", "PR", "VI", "UM")

code <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", 
          "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", 
          "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", 
          "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", 
          "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", 
          "56", "60", "66", "69", "72", "78", "74")

# need to delete the state path
hedonics_output <- "/home/bdeep/share/projects/Zillow_Housing/stores/Hedonics/hedonics_output_2019/"
census_output <- "/home/bdeep/share/projects/Zillow_Housing/stores/Hedonics/State_Hedonics_Census/"
state <- "OH"
state_code <- code[which(abbr == state)]

get_layerName <- function(shp_file_path)
{
  relative_path <- path.expand(shp_file_path)
  print(relative_path)
  return(ogrListLayers(relative_path)[1])
}

{
  ##################################################
  ## please filling in the dataframe you need to use as points here
  #points <- finalResult
  points <- readRDS(paste0(hedonics_output, state, "Hedonics_2019.rds"))
  
  ## trim the points data_frame, leave only transid, longtitude and latitude columns.
  points_abb <- points[, c("TransId", "PropertyAddressLongitude", "PropertyAddressLatitude")]
  
  # test to see if TransId is unique. Turns out it is
  #a = anyDuplicated(points_abb$TransId)
  
  # remove NAs for lat and lon for shapefile
  points_abb <- points[which(!is.na(points$PropertyAddressLongitude) & !is.na(points$PropertyAddressLatitude)),]
  points <- points[which(!is.na(points$PropertyAddressLongitude) & !is.na(points$PropertyAddressLatitude)),]
  # step 2-2: transfer both points_abb and points into shapefile #
  coordinates(points_abb) <- cbind(points$PropertyAddressLongitude, points$PropertyAddressLatitude)
  proj4string(points_abb) <- CRS("+proj=longlat")
  #coordinates(points) <- cbind(points$PropertyAddressLongitude, points$PropertyAddressLatitude)
  #proj4string(points) <- CRS("+proj=longlat")

  ##################################################
  ## please filling in the polygon path you wanted in here
  # patH fixed
  polygons_path <- paste0("/home/bdeep/share/projects/Zillow_Housing/stores/CensusBlocks/", state_code, "/tl_2016_", state_code, "_tabblock10.shp") # "~/share/projects/zillow/stores/CensusBlocks/09/tl_2016_09_tabblock10.shp"
  # step 4: preprocess for the polygons #
  shpLayerName <- get_layerName(polygons_path)
  shp_poly <- readOGR(path.expand(polygons_path), shpLayerName)
  # step 4-1: checking for default projection #
  # if(is.na(proj4string(shp_poly))){
  #   default_projection <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  #   proj4string(shp_poly) <- CRS(default_projection)
  # }

  # step 4-2: assigning indices on polygon dataframe #
  shp_poly$ID <- seq.int(nrow(shp_poly)) # not useful ID, remove later

  # step 5: transform the projection from points to the projection of the shapefile #
  points_abb <- spTransform(points_abb, proj4string(shp_poly))
  proj4string(points_abb) <- proj4string(shp_poly)
  gc()
  print("here")
  # step 6: perform the over operation #
  res <- over(points_abb, shp_poly)
  print("over done")

  ## step 6.5: merge points with overed points_abb
  # points_df = as.data.frame(points)
  # points_abb_df = as.data.frame(points_abb)
  # points_res <- merge(points_abb, points, by=c("TransId"))
  points_res = as.data.frame(points)
  
  


  # step 7: Appending the polygons' information to points dataframe #
  points_res <- cbind(points_res, res)

  saveRDS(points_res, paste0(census_output, state, "Hedonics_withTract_2019.rds"))
  print("file generated")
  
  ###########################################################################

  # ## remove NAs for lat and lon for shapefile
  # points <- points[which(!is.na(points$PropertyAddressLongitude) & !is.na(points$PropertyAddressLatitude)),]
  # # step 2-2: transfer it into shapefile #
  # coordinates(points) <- cbind(points$PropertyAddressLongitude, points$PropertyAddressLatitude)
  # proj4string(points) <- CRS("+proj=longlat")
  # 
  # 
  # ##################################################
  # ## please filling in the polygon path you wanted in here
  # # patH fixed
  # polygons_path <- paste0("/home/bdeep/share/projects/Zillow_Housing/stores/CensusBlocks/", state_code, "/tl_2016_", state_code, "_tabblock10.shp") # "~/share/projects/zillow/stores/CensusBlocks/09/tl_2016_09_tabblock10.shp"
  # # step 4: preprocess for the polygons #
  # shpLayerName <- get_layerName(polygons_path)
  # shp_poly <- readOGR(path.expand(polygons_path), shpLayerName)
  # # step 4-1: checking for default projection #
  # # if(is.na(proj4string(shp_poly))){
  # #   default_projection <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  # #   proj4string(shp_poly) <- CRS(default_projection)
  # # }
  # 
  # # step 4-2: assigning indices on polygon dataframe #
  # shp_poly$ID <- seq.int(nrow(shp_poly)) # not useful ID, remove later
  # 
  # # step 5: transform the projection from points to the projection of the shapefile #
  # points <- spTransform(points, proj4string(shp_poly))
  # proj4string(points) <- proj4string(shp_poly)
  # gc()
  # print("here")
  # # step 6: perform the over operation #
  # res <- over(points, shp_poly)
  # print("over done")
  # # step 7: Appending the polygons' information to points dataframe #
  # points_res <- as.data.frame(points)
  # points_res <- cbind(points_res, res)
  # 
  # saveRDS(points_res, paste0(hedonics_output, state, "Hedonics_withTract.rds"))
}


















