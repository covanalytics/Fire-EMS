

setwd("U:/CityWide Performance/CovStat/CovStat Projects/Fire/Inspections")

library("xlsx")
library("plyr")
library("dplyr")
library("tidyr")
library("reshape")
library("reshape2")
library("stringr")
library("zoo")
library("lubridate")
library("splitstackshape")
library("arcgisbinding")
library("sp")
library("spdep")
library("rgdal")
library("maptools")
library("foreign")

#### Load Update ####
inspections_update <-  read.dbf("Update.dbf")

##########################################################
#### Spatial Data Creation                            ####
#### Assign closest neighborhood and sector in ArcGIS ####
#### Join neighborhoods first, then fire sectors     ####
##########################################################

#### Create Spatial Points Data.Frame from Lat/Long Coordinates ----
inspectSP <- inspections_update
coordinates(inspectSP) <- ~LONGITUDE+LATITUDE

### Define Coordinate system for spatial points data.frame ----
reference <- CRS("+init=epsg:4326")
proj4string(inspectSP) <- reference

#### Write spatial points data.frame to a shapefile ----
writeOGR(obj = inspectSP, dsn ="C:/Users/tsink/Mapping/Geocoding/Fire", 
         layer = "FireInspectionsUpdate", driver = 'ESRI Shapefile', overwrite_layer = TRUE)

#########################
####Connect to ArcGIS####
#########################
#### Initialize arcgisbinding----
arcgis_process <- function() {
  arc.check_product()
  
  #### Read GIS Features -----
  readFIRE_IN<- arc.open("C:/Users/tsink/Mapping/Geocoding/Fire/Inspections2.shp")
  
  #### Create Data.Frame from GIS data -----
  fireIN_GIS <- arc.select(readFIRE_IN)
  
  ####Bind hidden lat/long coordinates back to data frame ------------
  shape <- arc.shape(fireIN_GIS)
  fireIN_GIS<- data.frame(fireIN_GIS, long=shape$x, lat=shape$y)
  
  # Assign First Mutual Aid ------
  fireIN_GIS$NbhdLabel[fireIN_GIS$Distance > 0] <- "Outside City"
  
  # Assign Closest Neighborhood for calls in downtown sector but outside of neighborhod boundary -----
  fireIN_GIS$NbhdLabel <- ifelse(fireIN_GIS$Distance_1 == 0 & fireIN_GIS$NbhdLabel == "Outside City", as.character(fireIN_GIS$NEIGHBORHO),
                               as.character(fireIN_GIS$NbhdLabel))
  
  # Assign Mutual Aid to fire sector -----
  fireIN_GIS$Runcard[fireIN_GIS$Distance_1 > 0] <- "Outside City"
  
  # If call is inside neighborhood but outside sector -----
  fireIN_GIS$NbhdLabel <- ifelse(fireIN_GIS$Distance == 0 & fireIN_GIS$Distance_1 > 0, "Outside City",
                               as.character(fireIN_GIS$NbhdLabel))
  
  ##Invalid coordinates and neighborhood indication
  fireIN_GIS$NbhdLabel[fireIN_GIS$lat == 0] <- "Invalid Coordinates"
  fireIN_GIS$Runcard[fireIN_GIS$lat == 0] <- 99
  inspections <- fireIN_GIS
  
  return(inspections)
}
inspections <- arcgis_process()

#### Delete and Rename Names ####
inspections <- inspections[, c(-1:-3, -18:-21, -23:-34, -37)]
names(inspections) <- tolower(names(inspections))
inspections <- plyr::rename(inspections, c("nbhdlabel"="NbhdLabel", "id_1"="Id_1", "runcard"="Runcard",
                                           "long"="longitude", "lat"="latitude"))
inspections$reinspect <- as.character(inspections$reinspect)
inspections$reinspect <- ifelse(inspections$reinspect == "0", "FALSE", "TRUE")

###   SQLite Database ####
### Append new data -----
library("RSQLite")
cons.fire <- dbConnect(drv=RSQLite::SQLite(), dbname="O:/AllUsers/CovStat/Data Portal/repository/Data/Database Files/Fire.db")
dbWriteTable(cons.fire, "FireInspections", inspections, append = TRUE)

#### Load all data and write over tableau file -----
dash_inspect <- dbGetQuery(cons.fire, 'select * from FireInspections')
write.csv(dash_inspect, "U:/CityWide Performance/CovStat/CovStat Projects/Fire/TableauFiles/FireInspections.csv", row.names = FALSE)
dbDisconnect(cons.fire)

#write to CovStat Repository
write.csv(dash_inspect, file="O:/AllUsers/CovStat/Data Portal/Repository/Data/Fire_EMS/Inspections.csv", row.names = FALSE)







