#=============================================================
##############################################################
# Producing Interactive Map of BC Herptofauna Road Mortality in R
# # 
# Date: February 16, 2023
# Author: Kurt D.Frei, MSc. RPBio. 
# Email: kurt.frei@gov.bc.ca
##############################################################
#=============================================================

#=============================================================
# Install Required Packages
# [Note: use the lib option of install.packages is necessary.]
#=============================================================
install.packages(c("bcmaps",
                   "sf",
                   "dplyr",
                   "leaflet",
                   "tmap",
                   "rio"))

# remotes::install_github("r-lib/rlang")

install.packages("rlang", dependencies = TRUE)

#=============================================================
# Load Required Packages
#=============================================================

library(rlang)  # rlang_1.0.2
library(sf)
library(bcmaps)
library(tmap)
library(dplyr)
library(rinat)
library(rio)

sessionInfo()

#=============================================================
# Retrieve the boundaries of the BC province from the bcmaps package
#=============================================================

bc_bound_layer <- bc_bound()

class(bc_bound_layer)  # sf object, etc.

st_crs(bc_bound) # EPSG: 3005
# The B.C. government standard is EPSG: 3005, which corresponds to 
# the BC Albers projection, 
# which is suitable for analyses on large areas. 
# It is an ‘equal area’ projection,
# meaning that the size of areas is not distorted.

# Get most recent raw data from Canadian Amphibians & Reptiles on Roads
# Be patient. This may take awhile.
data  <- get_inat_obs_project(94083, 
                              type = c("observations",
                                       "info"),
                              raw = TRUE)
#Clean
data_final <- data %>%  
  select(id,
         observed_on,
         lon = longitude,
         lat = latitude,
         species_guess,
         place_guess,
         taxon.id,                        
         taxon.name,                      
         taxon.rank,                       
         taxon.ancestry,                   
         taxon.common_name.id,             
         taxon.common_name.name,           
         taxon.common_name.is_valid,    
         taxon.common_name.lexicon,        
         iconic_taxon.id,                  
         iconic_taxon.name,                
         iconic_taxon.rank,                
         iconic_taxon.rank_level,
  )%>% 
  na.omit()

write.csv(data_final,
          "observations.csv", 
          row.names = FALSE)
# As we are commencing a spatial operation, both files need to be 
#projected using the same coordinate reference system. 
#In this case, we ensure that both spatial files are set 
#to The B.C. government standard  EPSG: 3005, which corresponds
# to the BC Albers projection

iNaturalist <- st_read("observations.csv")

iNaturalist <- st_as_sf(iNaturalist, 
                        coords = c("lon", "lat"), 
                        crs = 4269, agr = "constant")
iNaturalist <- transform_bc_albers(iNaturalist)
st_crs(iNaturalist)
class(iNaturalist)

BMP <- import("HerpRoadMort_Fixed.xlsx")
BMP<- st_as_sf(BMP, 
               coords = c("x", "y"), 
               crs = 4269, agr = "constant")
BMP <- transform_bc_albers(BMP)

Golden_Ears_PP <- st_read("Mike_Lake_Road_Amphibs.kml")

Golden_Ears_PP <- transform_bc_albers(Golden_Ears_PP)

# make sure
st_crs(iNaturalist)
st_crs(BMP)
st_crs(Golden_Ears_PP)

# turns view map on
tmap_mode("view") 
#=============================================================
# Plot the boundaries of  BC 
#============================================================
tm <- tm_shape(bc_bound_layer) +
  tm_borders() 
tm

# Subset data to BC only
iNaturalist <- st_filter(iNaturalist, bc_bound_layer)

# Replace String with Another Stirng
BMP$Taxa[BMP$Taxa == 'Herpetofuana'] <- 'Both'

#rename the columns for the optional legend to make sense
iNaturalist  <- iNaturalist %>% 
  rename(iNaturalist = iconic_taxon.name)
iNaturalist_Class  <- as.factor(iNaturalist$iNaturalist)
levels(iNaturalist_Class)

#Do the same for the BMP doc
BMP  <- BMP %>% 
  rename( Class = Taxa )

BMP_Class  <- as.factor(BMP$Class)
levels(BMP_Class)



tm <-    tm_shape(bc_bound_layer) +
  tm_borders() +
  tm_shape(BMP) +
  tm_dots(size=0.1,
          shape=21,
          col = "Class", 
          palette=c(Reptile ='blue',
                    Amphibian ='orange',
                    Both='burlywood4'))+
  tm_shape(iNaturalist) +
  tm_dots(size=0.1,
          shape=21,
          col = "iNaturalist",
          palette=c(Reptilia ='blue',
                    Amphibia ='orange'),
          legend.show = FALSE) +
  tm_shape(Golden_Ears_PP) +
  tm_dots(size=0.1,
          shape=21,
          col = "orange")
tm

# save 
tmap_save(tm, "map.html")
