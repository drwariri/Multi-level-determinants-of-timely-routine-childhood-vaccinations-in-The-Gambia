#load packages
library(raster)
library(tidyverse)
library(sp)

#Set working directory
setwd("C:/filepath")

##############################################################################################################################
#1. create shapefile with dhs cluster location and urban_rural status
### read dhs cluster data into R###
data <- read_csv("data_dhs_coordinates.csv")

#Create a SpatialPointsDataFrame object from your "data" dataset
coordinates(data) <- c("longitude", "latitude")

# Define the coordinate reference system (CRS) for The Gambia
crs_gambia <- CRS("+proj=longlat +datum=WGS84")

# Assign the CRS to the SpatialPointsDataFrame
proj4string(data) <- crs_gambia

# Define the output file path for the shapefile
output_file <- "dhs_clust_locations.shp"

# Write the SpatialPointsDataFrame to a shapefile
writeOGR(data, dsn = ".", layer = "dhs_clust_locations", driver = "ESRI Shapefile")


################################################################################################################
#2. Project data and create buffer based on dhs shapefile and modelled raster surface
#load point and travel time data
cl_dhs <- shapefile("dhs_clust_locations.shp")
ttime_all <- raster(paste0("accessibility_all_mixed.tif"))
ttime_fixed <- raster(paste0("accessibility_fixed_mixed.tif"))
ttime_all_walking <- raster(paste0("accessibility_all_walking.tif"))
ttime_fixed_walking <- raster(paste0("accessibility_fixed_walking.tif"))

#create corrdinate reference system for projecting data.
crs_gam <- "+proj=utm +zone=28 +datum=WGS84 +units=m"

#project dhs cluster location shapefile data
cl_prj <- spTransform(cl_dhs, crs(crs_gam))


#create a new variable to hold the buffer distance values (I call mine "bdist")
#use ifelse to define a buffer distance based on an existing variable
#All points in the Urban region will have 5km buffers,while Rural will be 2km. We use 5000 and 2000 because the units are in meters.
cl_prj$bdist <- ifelse(cl_prj$rrl_rbn == "Urban", 2000,5000)

#create the buffer shapefile
cl_buffer <- raster::buffer(cl_prj, width=cl_prj$bdist,dissolve=FALSE)

##########################
#use buffer shapefile to extract the median travel time within buffers. You could also use mean, max, min and other statistics
#1. ttime_all
tt <- raster::extract(ttime_all, cl_buffer,fun=median, na.rm=TRUE, df=TRUE)

#add the new travel times to the dhs cluster shapefile
cl_prj$median_tt <- tt$accessibility_all

#create a dataframe from the shapefile to examine the data
cl_df_all <- as.data.frame(cl_prj)

#write ttime_all data data file
write.csv(cl_df_all, "mixed_all.csv")


##########################
#2. ttime_fixed
tt_fixed <- raster::extract(ttime_fixed, cl_buffer,fun=median, na.rm=TRUE, df=TRUE)

#add the new travel times to the dhs cluster shapefile
cl_prj$median_tt <- tt_fixed$accessibility_fixed

#create a dataframe from the shapefile to examine the data
cl_df_fixed <- as.data.frame(cl_prj)

#write ttime_all data data file
write.csv(cl_df_fixed, "mixed_fixed.csv")


############################
#3. ttime_all
tt_walking <- raster::extract(ttime_all_walking, cl_buffer,fun=median, na.rm=TRUE, df=TRUE)

#add the new travel times to the dhs cluster shapefile
cl_prj$median_tt <- tt_walking$accessibility_all_walking

#create a dataframe from the shapefile to examine the data
cl_df_all_walking <- as.data.frame(cl_prj)

#write ttime_all data data file
write.csv(cl_df_all_walking, "walking_all.csv")


#############################
#4. ttime_fixed
tt_walking_fixed <- raster::extract(ttime_fixed_walking, cl_buffer,fun=median, na.rm=TRUE, df=TRUE)

#add the new travel times to the dhs cluster shapefile
cl_prj$median_tt <- tt_walking_fixed$accessibility_fixed_walking

#create a dataframe from the shapefile to examine the data
cl_df_fixed_walking <- as.data.frame(cl_prj)

#write ttime_all data data file
write.csv(cl_df_fixed_walking, "walking_fixed.csv")

