
#-----------------------------------------------------#
#         Gecoding Boston Homicide Data               #
#-----------------------------------------------------#

library(sp)
library(maptools)
library(ggmap)
library(rgeos)
library(dplyr)
library(maptools)
library(rgdal)
library(stringdist)

# putting in file paths for Homicide Data
hom1 <- read.csv("E:/BARI/BARI Work/Files2Geocode/Boston Homicides_2010to2016.csv", stringsAsFactors = F)
landParcels_path = "E:/BARI/BARI Work/Files2Geocode/LandParcels.2017.csv"
landParcelsShpPath = "E:/BARI/BARI Work/Files2Geocode/LandParcels.2017/"
# naming the shapefile
landParcelsShpName = "LandParcels.2017"
  # geofile
samPath = "E:/BARI/BARI Work/Files2Geocode/sam_wGeos"
  # files from MassGIS
roadsCSVPath = "E:/BARI/BARI Work/Files2Geocode/roads_updated.csv"
roadsShpPath = "E:/BARI/BARI Work/Files2Geocode/Roads 2015/"
roadsShpName = "roads_updated"


# cleaning files
  # first seperating out the street number, name, and suffix
# temp = clean_address(hom1$Location)

homloc = clean_address(hom1$Location)

hom1$STNO1 <- homloc[,2]
hom1$STNO2 <- homloc[,3]
hom1$STNAME <- homloc[,4]
hom1$STSUFF <- homloc[,5]

  # making sure no license number duplicates
toGeocode = hom1[ !duplicated(hom1$Incident.),]

  # deleting addresses with NAs
toGeocode <- toGeocode[complete.cases(toGeocode$STNO1),]


#-------------- Where I'm stuck --------------#
# Geocoding
  # geocoding against the land parcels file
geocode_lp <- geocode(toGeocode = toGeocode, tgID = "Incident.", refName = "LandParcels", smallestGeo = "Land_Parcel_ID",
                      geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"), refCSVPath = landParcels_path)



