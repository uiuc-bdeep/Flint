#     ------------------------------------------------------------------------
#   |                                                                         |
#   |  Points Over Polygons using Rgdal library                                     |
#   |                                                                         |
#   |  By:                                                                    |
#   |  Yifang Zhang & Jiajun Xu                                                          |                            
#   |  University of Illinois at Urbana Chamapaign                            |
#   |                                                                         |
#     ------------------------------------------------------------------------

## state GA is not avaliable

points_over_polygon <-function(points_path, polygons_path, outputFilePath){
  
  ##################################################################
  ## Preliminaries
  
  ## This function will check if a package is installed, and if not, install it
  pkgTest <- function(x) {
    if (!require(x, character.only = TRUE))
    {
      install.packages(x, dep = TRUE)
      if(!require(x, character.only = TRUE)) stop("Package not found")
    }
  }
  
  ## These lines load the required packages
  packages <- c("readxl", "data.table", "rgdal", "sp", "rgeos", "tools")
  lapply(packages, pkgTest)
  
  ## assigning the variables ##
  pointFilePath <- points_path
  polygonFilePath <- polygons_path
  layerName <- get_layername_from_path(polygons_path)
  default_projection <- "+proj=longlat +datum=WGS84 +no_defs"
    #polygons_layer_name
  
  
  ## reading the points file
  pointFilePathExt <- file_ext(pointFilePath)
  if(pointFilePathExt == "rds"){
    points_raw <- readRDS(pointFilePath)
  } else{
    points_raw <- read.csv(pointFilePath)
  }
  
  
  points <- points_raw[which(!is.na(points_raw$PropertyAddressLongitude)),]
  points <- points[which(!is.na(points$PropertyAddressLatitude)),]
  
  ## reading state shapefile 
  shpName <- polygonFilePath
  shpDir <- path.expand(shpName)
  ################################################
  ## store the old working dir, and set the new dir to the shapefile folder
  wd <- getwd()
  vec <- strsplit(shpDir, "/")     #split the strings by 
  shapefile_name <- vec[[1]][lengths(vec)]     # get the shapefile file name
  temp_wd <- gsub(shapefile_name, "", shpDir)
  setwd(temp_wd)
  
  
  
  ##################################################
  ogrInfo(dsn = shpDir, layer = layerName)
  shp_poly <- readOGR(shapefile_name, layerName)
  setwd(wd)
  ## adding the indexes in front of precincts 
  shp_poly$addedID <- seq.int(nrow(shp_poly))
  
  ## performing the projection for points
  coordinates(points) <- ~ PropertyAddressLongitude + PropertyAddressLatitude
  proj4string(points) <- CRS("+proj=longlat")
  
  ## if the shapefile does not have projection, we will defaulting it to WGS 84
  if(is.na(proj4string(shp_poly))){
    proj4string(shp_poly) <- CRS(default_projection) ## "+proj=longlat +datum=WGS84 +no_defs"
  }
  
  
  ## transform the projection from points to the projection of the shapefile
  points <- spTransform(points, proj4string(shp_poly))
  proj4string(points) <- proj4string(shp_poly)
  
  ## perform the over function
  res <- over(points, shp_poly)
  
  ## optional: plotting the data in R
  #plot(shp_poly)
  #plot(points_res$PropertyAddressLatitude ~ points_res$PropertyAddressLongitude, col = "red", cex = 1)
  
  ## Appending the result information after the Hedonics Data
  points_res <- points_raw
  points_res <- points_res[which(!is.na(points_res$PropertyAddressLatitude)),]
  points_res <- points_res[which(!is.na(points_res$PropertyAddressLongitude)),]
  
  final_res <- cbind(points_res, res)
  
  print(paste("writing to", outputFilePath))
  saveRDS(final_res, outputFilePath)
  
  #return(final_res)
  
}

## testing only ##
#merged_result <- points_over_polygon(points_path = "~/share/projects/VotingBehavior/production/NCHedonics.rds", 
#                                     polygons_path = "~/share/projects/VotingBehavior/store/NC/NC_Shapefiles/", 
#                                     polygons_layer_name = "nc_final", 
#                                     default_projection = "+proj=longlat +datum=WGS84 +no_defs",
#                                     outputFilePath = "~/share/projects/VotingBehavior/MI_Save/NC_over.rds")

points_over_polygon_both_shapefile <-function(points_path, points_layer_name, polygons_path, polygons_layer_name, default_projection, outputFilePath)
{
  ## This function will check if a package is installed, and if not, install it
  pkgTest <- function(x) {
    if (!require(x, character.only = TRUE))
    {
      install.packages(x, dep = TRUE)
      if(!require(x, character.only = TRUE)) stop("Package not found")
    }
  }
  
  ## These lines load the required packages
  packages <- c("readxl", "data.table", "rgdal", "sp", "rgeos", "tools")
  lapply(packages, pkgTest)
  
  ## reading point shapefile 
  pointName <- points_path
  pointDir <- path.expand(pointName)
  ogrInfo(dsn = pointDir, layer = points_layer_name)
  points_raw <- readOGR(pointDir, points_layer_name)
  points <- points_raw
  
  if(is.na(proj4string(points))){
    proj4string(points) <- CRS(default_projection) ## "+proj=longlat +datum=WGS84 +no_defs"
  }
  
  ## reading polygon shapefile 
  shpName <- polygons_path
  shpDir <- path.expand(shpName)
  ogrInfo(dsn = shpDir, layer = polygons_layer_name)
  shp_poly <- readOGR(shpDir, polygons_layer_name)
  
  if(is.na(proj4string(shp_poly))){
    proj4string(shp_poly) <- CRS(default_projection) ## "+proj=longlat +datum=WGS84 +no_defs"
  }
  
  ## transform the projection from points to the projection of the shapefile
  points <- spTransform(points, proj4string(shp_poly))
  proj4string(points) <- proj4string(shp_poly)
  
  ## perform the over function
  res <- over(points, shp_poly)
  
  ## Appending the result information after the Hedonics Data
  points_res <- as(points_raw, "data.frame")
  #points_res <- points_res[which(!is.na(points_res$PropertyAddressLatitude)),]
  #points_res <- points_res[which(!is.na(points_res$PropertyAddressLongitude)),]
  
  final_res <- cbind(points_res, res)
  
  saveRDS(final_res, outputFilePath)
  
  return(final_res)
}


## temp function ##
points_over_polygon_fieldNameForPoints <-function(points_path, polygons_path, polygons_layer_name, default_projection, outputFilePath){
  
  ##################################################################
  ## Preliminaries
  
  ## This function will check if a package is installed, and if not, install it
  pkgTest <- function(x) {
    if (!require(x, character.only = TRUE))
    {
      install.packages(x, dep = TRUE)
      if(!require(x, character.only = TRUE)) stop("Package not found")
    }
  }
  
  ## These lines load the required packages
  packages <- c("readxl", "data.table", "rgdal", "sp", "rgeos", "tools")
  lapply(packages, pkgTest)
  
  ## assigning the variables ##
  pointFilePath <- points_path
  polygonFilePath <- polygons_path
  layerName <- polygons_layer_name
  
  
  ## reading the points file
  pointFilePathExt <- file_ext(pointFilePath)
  if(pointFilePathExt == "rds"){
    points_raw <- readRDS(pointFilePath)
  } else{
    points_raw <- read.csv(pointFilePath)
  }
  
  
  points <- points_raw[which(!is.na(points_raw$Longitude)),]
  points <- points[which(!is.na(points$Latitude)),]
  
  ## reading state shapefile 
  shpName <- polygonFilePath
  shpDir <- path.expand(shpName)
  ogrInfo(dsn = shpDir, layer = layerName)
  shp_poly <- readOGR(shpDir, layerName)
  
  ## adding the indexes in front of precincts 
  shp_poly$addedID <- seq.int(nrow(shp_poly))
  
  ## performing the projection for points
  coordinates(points) <- ~ Longitude + Latitude
  proj4string(points) <- CRS("+proj=longlat")
  
  ## if the shapefile does not have projection, we will defaulting it to WGS 84
  if(is.na(proj4string(shp_poly))){
    proj4string(shp_poly) <- CRS(default_projection) ## "+proj=longlat +datum=WGS84 +no_defs"
  }
  
  
  ## transform the projection from points to the projection of the shapefile
  points <- spTransform(points, proj4string(shp_poly))
  proj4string(points) <- proj4string(shp_poly)
  
  ## perform the over function
  res <- over(points, shp_poly)
  
  ## optional: plotting the data in R
  #plot(shp_poly)
  #plot(points_res$PropertyAddressLatitude ~ points_res$PropertyAddressLongitude, col = "red", cex = 1)
  
  ## Appending the result information after the Hedonics Data
  points_res <- points_raw
  points_res <- points_res[which(!is.na(points_res$Latitude)),]
  points_res <- points_res[which(!is.na(points_res$Longitude)),]
  
  final_res <- cbind(points_res, res)
  
  print("writing matching outputs to a rds file")
  saveRDS(final_res, outputFilePath)
  
  return(final_res)
  
}

get_layerName <- function(state)
{
  relative_path <- paste("store",state, paste(state, "_Shapefile/", state, "_final.shp", sep=""), sep = "/")
  return(ogrListLayers(relative_path)[1])
}

get_relative_path <- function(state)  # when already under the store folder, input the state name to get the path to the shapefile folder
{
    return (paste(state, "/", state, "_Shapefile/", sep=""))
}
test_state_for_layername <- function(state) # test the state for the listlayer function
{
    # sapply(abbr, test_state_for_layername)  test all 50 states for this function
    wd <- paste(base, get_relative_path(state), sep="")
    folder <- tryCatch(setwd(wd), error=function(e) {print(paste("no folder for state", state))
                                              return(-1)})# setting the wd to state_Shapefile/
    if (folder == (-1))
    {
      no <- no + 1 
    }
    
    spath <- paste(state, "_final.shp", sep="")
    success <- tryCatch(ogrInfo(spath)$layer, error=function(e) {return(-1)} )#{state <- tolower(state)})
    if (success == (-1))
    {
      print(paste("lowering the letters for state", state))
      state2 <- tolower(state)
      spath2 <- paste(state2, "_final.shp", sep="")
      success2 <- tryCatch(ogrInfo(spath2)$layer, error=function(e) {no <- no + 1
                                                          print(paste("failed:", state))})
      if (success2 == (-1))
      {
        print("success: ", state)
      }
    }
    else
    {
      yes <- yes+1
      print(paste("success:", state))
    }
}
#################################################
# get the input shape file given the state name
#     input:  
#           state: string, the name of the stae
#     output:
#           input_path: string, the path to the polygon shapefile for the given state
#     description:
#           check if the state folder exists
#           check if the shapefile is in lower or uppercase
get_shape_file <- function(project_path, state)
{
    state_folder <- paste(project_path, "store", state, sep="/")
    if ( !file.exists(state_folder) )
    {
      print(paste(state, "folder doesn't exist"))
      return(-1)
    }
    shapefile_folder <- paste(state_folder, paste(state,"_Shapefile/", sep=""), sep="/")
    if ( !file.exists(shapefile_folder) )
    {
      shapefile_folder <- paste(state_folder, paste(tolower(state),"_Shapefile/", sep=""), sep="/")
      if ( !file.exists(shapefile_folder) )
      {
        shapefile_folder <- paste(state_folder, paste(tolower(state),"_shapefile/", sep=""), sep="/") 
      }
    }
    shapefile <- paste(state, "_final.shp", sep="")
    input_path <- paste(shapefile_folder, shapefile, sep="")
    if ( !file.exists(input_path) )
    {
      shapefile <- paste(tolower(state), "_final.shp", sep="")
      input_path <- paste(shapefile_folder, shapefile, sep="")
    }
      
    return(input_path)
    
}
###############################################
# function for getting the output file for the PointsOverPolygon function
#     input: 
#         state: string, the state for the polygon file
#     output:
#         output_path: string, the path to the output matching file
#                     in the format of statename+"_matching.rds" e.g. OH_matching.rds

get_matching_result_path <- function(project_path, state)
{
    #production_folder <- "~/share/projects/VotingBehavior/production/"
    production_folder <- paste(project_path, "production/", sep="/")
    return(paste(production_folder, state, "_matching.rds", sep=""))
}


##################################################
# function for extracting layer names form the path
#   input:
#       pathname: string, the path to the shapefile 
#   output:
#       layer_name: string, the layer name for the given shapfile
#   description:
#       This function is for the case where we need to find the layername when 
#         the input shapefile name is given.
#
get_layername_from_path <- function(pathname)
{
    vec <- strsplit(pathname, "/")     #split the strings by 
    file_name <- vec[[1]][lengths(vec)]     # get the shapefile file name
    layer_name <- gsub(".shp", "", file_name)     # assuming the layer name is the same as the file name
    return(layer_name)
}