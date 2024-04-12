##########Load packages################################
library(ggplot2)
library(tidyverse)
library(dplyr)

#Set working directory
setwd("C:/filepath")

filePathData <- "C:/filepath/"


### read data into R###
sub <- read_csv("sub.csv")
outcome_data <- read_csv("outcome_data.csv")


# Categorize month_birth variation into season from the "sub" dateset
sub <- mutate(sub, 
              season = ifelse(month_birth %in% c(11, 12, 1, 2, 3, 4, 5), "dry", "wet"))

#combine variables "motocycle" and "car" into one variation "motorized"
sub <- mutate(sub, 
              motorized = ifelse(motocycle == "No" & car == "No", "No", "Yes"))

#select the new variables we need
sub_new <- sub[, c("women_id", "season", "motorized")]


##read raw DHS data into R and selection additional/new variables##
child_recode <- read_csv("GMKR81SV/GMKR81FL.csv")

##select additional variables AND rename AND create_joing_ID variables##
child_select <- child_recode %>% 
  filter(B19 >= 0 & B19 <=35) %>% 
  dplyr::select(HIDX, V001, V002, V003, V026, V104, V151, V136, V169A, V170, V459, V466, V467D, V481, V501, M14, M70) %>% 
  rename(child_index = HIDX, cluster = V001, household = V002, respondentid = V003, residence_cat = V026, years_residence = V104, sex_hh_head = V151, 
         hh_size = V136, hh_mobilephone = V169A, mat_bank_acc = V170, hh_bednets = V459, mat_decide_care = V466, dist_hc_far = V467D, mat_insurance = V481,
         marital_status = V501, ANC_no = M14, PNC_no = M70) %>% 
  mutate(women_id = paste(cluster, household, respondentid, sep = "_")) %>% 
  mutate(household_id = paste(cluster, household, sep = "_"))

# Drop the specified variables from the dataset
child_select <- child_select %>%
  dplyr::select(-residence_cat, -cluster, -household, -respondentid, -mat_decide_care, -household_id)

#categorize variables from the child_select dataset
#1. ANC_no
child_select <- child_select %>%
  mutate(ANC_no__cat = recode(ANC_no, .default = ANC_no, 
                            "No antenatal visits" = "no ANC",
                            "1" = "one to three",
                            "2" = "one to three",
                            "3" = "one to three",
                            "4" = "four or more",
                            "5" = "four or more",
                            "6" = "four or more",
                            "7" = "four or more",
                            "8" = "four or more",
                            "9" = "four or more",
                            "10" = "four or more",
                            "12" = "four or more",
                            "13" = "four or more",
                            "14" = "four or more",
                            "18" = "four or more",
                            "20" = "four or more")) %>% 
  mutate(years_residence_cat = case_when(
    years_residence == 0 ~ "<1 year",
    years_residence %in% 1:3 ~ "1-3 years",
    years_residence %in% 4:5 ~ "4-5 years",
    years_residence %in% 6:49 | years_residence == "Always" ~ ">5 years",
    TRUE ~ as.character(years_residence)
    ))%>% 
  mutate(hh_size_cat = case_when(
    hh_size %in% 1:4 ~ " small (1-4)",
    hh_size %in% 5:8 ~ "medium (5-8)",
    hh_size %in% 9:90 ~ " large (9 or more)",
    TRUE ~ as.character(hh_size)
    ))%>%
  mutate(marital_status_cat = case_when(
    marital_status == "Never in union" ~ "Never in union",
    marital_status %in% c("Married", "Living with partner") ~ "married/with partner",
    marital_status %in% c("Widowed", "Divorced", "No longer living together/separated") ~ "divorced/widowed/separated",
    TRUE ~ as.character(marital_status)
  ))


##JOIN ALL THREE DATASETS

outcome_data$join <- 1:5148
sub_new$join <- 1:5148
child_select$join <- 1:5148

# Merge the datasets using the common variable "join"
merged_data <- merge(outcome_data, sub_new, by = "join", all.x = TRUE)
merged_data <- merge(merged_data, child_select, by = "join", all.x = TRUE)

#drop some variables
merged_data <- merged_data %>%
  dplyr::select(-women_id.x, -women_id.y, -join, -...1, -H1A)


### read modelled trave time data into R###
tt_any_mixed <- read_csv("mixed_all.csv") #manually change name of tt_median to "tt_any_mixed" before reading in
tt_fixed_mixed <- read_csv("mixed_fixed.csv") #manually change name of tt_median to "tt_fixed_mixed" before reading in
tt_any_walking <- read_csv("walking_all.csv") #manually change name of tt_median to "tt_any_walking" before reading in
tt_fixed_walking <- read_csv("walking_fixed.csv") #manually change name of tt_median to "tt_fixed_walking" before reading in

# Perform a left join on the 'cluster' variable
merged_data <- left_join(merged_data, tt_any_mixed, by = "cluster")
merged_data <- left_join(merged_data, tt_fixed_mixed, by = "cluster")
merged_data <- left_join(merged_data, tt_any_walking, by = "cluster")
merged_data <- left_join(merged_data, tt_fixed_walking, by = "cluster")

##select and check
test2 <- merged_data[, c("cluster", "tt_any_mixed", "tt_fixed_mixed", "tt_any_walking", "tt_fixed_walking")]


#write facilities data to file
write.csv(merged_data, "outcome_data_obj3.csv")

###############################################################################################################################################
#####################categorize travel time####################################################################################################

### read data into R###
outcome_data_obj3 <- read_csv("outcome_data_obj3.csv")

# Categorize tt_any_mixed variable
outcome_data_obj3$tt_any_mixed_cat <- cut(
  outcome_data_obj3$tt_any_mixed,
  breaks = c(-1, 14, 29, 44, 59, Inf),
  labels = c("<15 mins", "15 - <30 mins", "30 - <45 mins", "45 - <60 mins", "60 mins and above"),
  include.lowest = TRUE)

# Categorize tt_fixed_mixed variable
outcome_data_obj3$tt_fixed_mixed_cat <- cut(
  outcome_data_obj3$tt_fixed_mixed,
  breaks = c(-1, 14, 29, 44, 59, Inf),
  labels = c("<15 mins", "15 - <30 mins", "30 - <45 mins", "45 - <60 mins", "60 mins and above"),
  include.lowest = TRUE)

# Categorize tt_any_walking variable
outcome_data_obj3$tt_any_walking_cat <- cut(
  outcome_data_obj3$tt_any_walking,
  breaks = c(-1, 14, 29, 44, 59, Inf),
  labels = c("<15 mins", "15 - <30 mins", "30 - <45 mins", "45 - <60 mins", "60 mins and above"),
  include.lowest = TRUE)

# Categorize tt_fixed_walking variable
outcome_data_obj3$tt_fixed_walking_cat <- cut(
  outcome_data_obj3$tt_fixed_walking,
  breaks = c(-1, 14, 29, 44, 59, Inf),
  labels = c("<15 mins", "15 - <30 mins", "30 - <45 mins", "45 - <60 mins", "60 mins and above"),
  include.lowest = TRUE)

#write facilities data to file
write.csv(outcome_data_obj3, "outcome_data_obj3_1.csv")

############################################################################################################################################
###merge clusters with nearest facilities################################################################################################

### read data into R###
outcome_data_obj3_1 <- read_csv("outcome_data_obj3_1.csv")
cluster_hf_joined_obj3 <- read_csv("cluster_hf_joined_obj3.csv")

# Perform a left join on the 'cluster' variable
outcome_data_obj3_final <- left_join(outcome_data_obj3_1, cluster_hf_joined_obj3, by = "cluster")

#write facilities data to file
write.csv(outcome_data_obj3_final, "outcome_data_obj3_final.csv")

################################################################################################################################################
###make chloroplet of travel time showing cluster locations#########################################################################################


### read modelled trave time data into R###
data <- read_csv("outcome_data_obj3_1.csv") #manually change name of tt_median to "tt_any_mixed" before reading in

###select the variables needed to make the plot
data1 <- data %>% 
  select(cluster, latitude, longitude, tt_fixed_mixed, tt_fixed_mixed_cat, tt_any_mixed, tt_any_mixed_cat)

# Group by the 'cluster' variable and select the first observation from each cluster
data2 <- data1 %>%
  group_by(cluster) %>%
  slice(1) %>%
  ungroup()


####Read in all shape files
gmb_shp_1 <- st_read(paste0(filePathData,"gmb_admbnda_adm1_2022.shp"))
gmb_shp <- st_read(paste0(filePathData,"gmb_admbnda_adm2_2022.shp"))


# Convert both shapefiles to sf objects with the same geometry column name
gmb_shp_1_sf <- st_as_sf(gmb_shp_1, "newname")
gmb_shp_sf <- st_as_sf(gmb_shp, "newname")


# Combine shapefiles
gmb_combined <- bind_rows(gmb_shp_1_sf, gmb_shp_sf)


# Create a variable to distinguish between admin 1 and admin 2 boundaries
gmb_combined$type <- ifelse(is.na(gmb_combined$ADM2_EN), "ADM1_EN", "ADM2_EN")


# Set a custom color palette for the travel time categories
tt_palette <- c("#256515", "#b2db76", "#e8b1d5", "#d34093", "#910054")

# Plot spatial location of cluster-level travel time to fixed clinic in The Gambia
p <- ggplot() +
  geom_sf(data = gmb_combined[gmb_combined$type == "ADM2_EN", ], color = "white", size = 0.8) +
  geom_sf(data = gmb_combined[gmb_combined$type == "ADM1_EN", ], color = "black", size = 0.9, alpha = 0.1) +
  geom_point(data = data2, aes(x = longitude, y = latitude), color = "grey", size = 2.5, shape = 1) +  # Add grey outline
  geom_point(data = data2, aes(x = longitude, y = latitude, fill = tt_fixed_mixed_cat), size = 2, shape = 21) +
  scale_fill_manual(values = tt_palette, name = "Travel Time") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_blank(), legend.position = "bottom", legend.box = "horizontal") +
  labs(title = "Cluster-level travel time to nearest fixed facility")

ggsave(filename= "tt_fixed_plot.png", plot=p, height=4, width=8, units="in", device = "png", dpi=300)
#####################################################################################################################################################
###make chloroplet of all immunisation clinics in The Gambia#########################################################################################

### read modelled trave time data into R###
facilities <- read_csv("facilities.csv") #manually change name of tt_median to "tt_any_mixed" before reading in

# Set a custom color palette for the travel time categories
facilities_palette <- c("#35b079", "#910054")

p <- ggplot() +
  geom_sf(data = gmb_combined[gmb_combined$type == "ADM2_EN", ], color = "white", size = 0.8) +
  geom_sf(data = gmb_combined[gmb_combined$type == "ADM1_EN", ], color = "black", size = 0.9, alpha = 0.1) +
  geom_point(data = facilities, aes(x = longitude, y = latitude), color = "grey", size = 2.5, shape = 1) +  # Add grey outline
  geom_point(data = facilities, aes(x = longitude, y = latitude, fill = site_category), size = 2, shape = 21) +
  scale_fill_manual(values = facilities_palette, name = "Category of immunisation facility") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_blank(), legend.position = "bottom", legend.box = "horizontal") +
  labs(title = "The 394 Health facility (fixed clinics) and outreach immunisation clinics in The Gambia")

ggsave(filename= "facilities_plot.png", plot=p, height=4, width=8, units="in", device = "png", dpi=300)
