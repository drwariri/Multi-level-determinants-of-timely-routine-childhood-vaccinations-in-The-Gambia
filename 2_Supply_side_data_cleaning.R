##########Load packages################################
library(ggplot2)
library(tidyverse)
library(raster); library(maptools)
library(gtools); library(sp); library(spdep)
library(rgdal)
library(rasterVis)
library(viridisLite)


#Set working directory
setwd("C:/filepath")

# Read the TIF travel time files into R
ttime_all <- raster(paste0("accessibility_all_mixed.tif")) #from Accesmod download, IMPORT the 'Disc Image File' into QGIS to convert to TIF
ttime_fixed <- raster(paste0("accessibility_fixed_mixed.tif"))##from QGIS, EXPORT as .TIF/.TIFF file by right clicking file>export>save as
ttime_all_walking <- raster(paste0("accessibility_all_walking.tif"))
ttime_fixed_walking <- raster(paste0("accessibility_fixed_walking.tif"))



# Define the target CRS (WGS84) AND Reproject the raster
target_crs <- "+proj=longlat +datum=WGS84"
ttime_fixed <- projectRaster(ttime_fixed, crs = target_crs)
ttime_all <- projectRaster(ttime_all, crs = target_crs)

# actual plot
#1. any facility_mixed
revMagma <- rasterTheme(region = rev(magma(10))) #this is needed to reverse the order of the levelplot default colour
levelplot(ttime_all, par.settings = revMagma, margin=FALSE, xlab = "", ylab = "", main = "1x1 km map of modelled travel time to the nearest immunisation clinic (any)") #margin=FALSE removes margin plot

# Save the plot as a .png file
png("tt_any_pixel_plot.png", height = 4, width = 8, units = "in", res = 300)
# Print the plot to the device
print(p)
dev.off()


#2. fixed facility_mixed
revMagma <- rasterTheme(region = rev(magma(10))) #this is needed to reverse the order of the levelplot default colour
 p <- levelplot(ttime_fixed, par.settings = revMagma, margin=FALSE, xlab = "", ylab = "", main = "1x1 km map of modelled travel time to the nearest fixed immunisation clinic") #margin=FALSE removes margin plot

# Save the plot as a .png file
png("tt_fixd_pixel_plot.png", height = 4, width = 8, units = "in", res = 300)
# Print the plot to the device
print(p)
dev.off()

#3. any facility_walking
revMagma <- rasterTheme(region = rev(magma(10))) #this is needed to reverse the order of the levelplot default colour
levelplot(ttime_all_walking, par.settings = revMagma, margin=FALSE, xlab = "", ylab = "") #margin=FALSE removes margin plot

#4. fixed facility_walking
revMagma <- rasterTheme(region = rev(magma(10))) #this is needed to reverse the order of the levelplot default colour
levelplot(ttime_fixed_walking, par.settings = revMagma, margin=FALSE, xlab = "", ylab = "") #margin=FALSE removes margin plot
###########################################################################################################################################


###################Cleaning and merging facility-level data###############################################################################
### read data into R###
facility_all <- read_csv("facilities.csv")
facility <- read_csv("new.csv")

#Join both files
facility_all$Site_ID <- as.character(facility_all$Site_ID)
facilities_new <- left_join(facility_all, facility, by = "Site_ID")

#drop some variables
facilities_new <- facilities_new %>%
  dplyr::select(-site_name, -...4, -cold_van_access_to_Site, -SUV_access)

# Create a new variable 'vaccine_staff' based on conditions
facilities_new$vaccine_staff <- ifelse(facilities_new$site_category == "Health Facility", 
                                       facilities_new$vacc_staff, 1)

# Create the new variable 'open_weekly' based on conditions
facilities_new <- facilities_new %>%
  mutate(open_weekly = ifelse(days_open_month >= 4, "yes", "no")) %>% 
  mutate(vaccine_staff_cat = ifelse(vaccine_staff >= 2, "2 or more", "only 1"))

# Calculate tertiles (catchment_pop, vacc_staff) and Add new category (tertiles) to the dataset as new variables
tertiles_catchment <- quantile(facilities_new$catchment_pop, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
facilities_new$catchment_pop_cat <- cut(facilities_new$catchment_pop, breaks = tertiles_catchment, labels = c("Low", "Medium", "High"), include.lowest = TRUE)

tertiles_catchment <- quantile(facilities_new$u5_pop, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
facilities_new$u5_pop_cat <- cut(facilities_new$u5_pop, breaks = tertiles_catchment, labels = c("Low", "Medium", "High"), include.lowest = TRUE)

# Create service_readiness variable
facilities_new$service_readiness <- 0  # Initialize with 0

# Assign scores based on conditions
facilities_new$service_readiness <- facilities_new$service_readiness +
  ifelse(facilities_new$cold_store == "Yes", 1, 0) +
  ifelse(facilities_new$site_category == "Health Facility", 1, 0) +
  ifelse(facilities_new$open_weekly == "yes", 1, 0) +
  ifelse(facilities_new$vaccine_staff_cat == "2 or more", 1, 0) +
  ifelse(facilities_new$catchment_pop_cat %in% c("Low", "Medium"), 1, 0)


#write facilities data to file
write.csv(facilities_new, "facilities_new_obj3.csv")

############################################################################################################################################
############join clusters to the nearest facilities#########################################################################################

###read in the two datasets
facilities_new_obj3 <- read_csv("facilities_new_obj3.csv")
cluster_hf_joined_final <- read_csv("cluster_hf_joined_final.csv")


###convert the Site_ID variable to as.character
facilities_new_obj3$Site_ID <- as.character(facilities_new_obj3$Site_ID)
cluster_hf_joined_final$Site_ID <- as.character(cluster_hf_joined_final$Site_ID)

#Join both files
fac_new <- left_join(cluster_hf_joined_final, facilities_new_obj3, by = "Site_ID")

#drop some variables
fac_new <- fac_new %>%
  dplyr::select(-parent_id, -longitude, -latitude, -parent_name, -region, -public_private, -days_open_month, -vaccine_staff)


##reclassify "service_readiness
fac_new <- fac_new %>%
    mutate(service_avail_readiness = case_when(
    service_readiness %in% 0:1 ~ "low (0-1)",
    service_readiness %in% 2:3 ~ "intermediate (2-3)",
    service_readiness %in% 4:5 ~ "high (4-5)",
    TRUE ~ as.character(service_readiness)))

#write facilities data to file
write.csv(fac_new, "cluster_hf_joined_obj3.csv")
