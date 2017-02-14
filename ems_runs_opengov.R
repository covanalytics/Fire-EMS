

setwd("U:/CityWide Performance/CovStat/CovStat Projects/Fire/EMSruns")

library("xlsx")
library("plyr")
library("dplyr")
library("tidyr")
library("splitstackshape")
library("magrittr")
library("gmodels")
library("descr")
library("lubridate")
library("ggmap")
library("arcgisbinding")
library("sp")
library("spdep")
library("rgdal")
library("maptools")
library("ggmap")
library("RSQLite")
 
#### Load first update file ####  
update1 <- read.csv("Update1.csv", header=TRUE,  stringsAsFactors = FALSE)
update1 <- update1[order(update1$IMX),]
update1 <- update1[, c(-3:-7, -11, -13:-17)]
  
#### Load second update file ####                   
update2  <- read.csv("Update2.csv", header=TRUE,  stringsAsFactors = FALSE)
update2 <- update2[order(update2$IMX),]
update2 <- update2[, c(-2, -5:-8, -15:-21,-23:-27)]

#### Merge Update files by IMX ####
update <- merge(update1, update2, by="IMX")

## Add Count column and 'Overdose/Suspected Overdose
update$Count <- 1
update$CallType <- ifelse(grepl("Overdose", update$DispatchedAs), "Overdose/Suspected Overdose", as.character(update$DispatchedAs)) 

update <- update[,-(8)]
names(update)[2] <- "IncidentNum"

runs <- update

####Remove practice/training runs####
runs$IncidentNum <- ifelse(grepl("del", runs$IncidentNum), "PRACTICE", 
                           ifelse(grepl("DEL", runs$IncidentNum), "PRACTICE", as.character(runs$IncidentNum)))
runs$Pickup <- ifelse(grepl("delete", runs$Pickup), "PRACTICE", as.character(runs$Pickup))
runs <- subset(runs, IncidentNum!= "PRACTICE" & Pickup!= "PRACTICE")

####Assign Company Name based on responding unit#####

runs$Company[runs$Unit == "PAR-1" | runs$Unit == "R-1" | runs$Unit == "A-1"] <- "Company 1: Eastside (100 E Robbins St)"
runs$Company[runs$Unit == "P-2"] <- "Company 2: West Covington (1252 Parkway Ave)"
runs$Company[runs$Unit == "P-5"] <- "Company 5: South Covington (1255 Hands Pike)"
runs$Company[runs$Unit == "P-6" | runs$Unit == "A-4" | runs$Unit == "A-6"] <- "Company 6: Peaselburg (1502 Holman Ave)"
runs$Company[runs$Unit == "P-8" | runs$Unit == "T-7" | runs$Unit == "A-2" | runs$Unit == "A-3" | runs$Unit == "A-8"] <- "Company 8: Latonia (3315 Church St)"
runs$Company[runs$Unit == "A-5"] <- "Special Event"

####Assign Unit A-3 to proper location in FY 2014 and 2015
#runs$Company[runs$Unit =="A-3"& runs$Fiscal.Year == "2014"] <- "Company 5: South Covington (1255 Hands Pike)"
#runs$Company[runs$Unit =="A-3"& runs$Fiscal.Year == "2015"] <- "Company 5: South Covington (1255 Hands Pike)"


###Assign ciy for pick up location
runs$Area <- ifelse(grepl("COV", runs$Pickup),"Covington", 
                    ifelse(grepl("LATONIA", runs$Pickup),"Covington",
                    ifelse(grepl("DECOURSEY", runs$Pickup),"Covington",  
                    ifelse(grepl("Decoursey", runs$Pickup),"Covington",     
                    ifelse(grepl("FT MITCHELL", runs$Pickup),"FT Mitchell",      
                    ifelse(grepl("FT WRIGHT", runs$Pickup),"FT Wright",
                    ifelse(grepl("FORT WRIGHT", runs$Pickup),"FT Wright",       
                    ifelse(grepl("NEWPORT", runs$Pickup),"Newport", 
                    ifelse(grepl("FORT THOMAS", runs$Pickup),"Fort Thomas",  
                    ifelse(grepl("BROMLEY", runs$Pickup),"Bromley",        
                    ifelse(grepl("KENTONVALE", runs$Pickup),"Kentonvale",       
                    ifelse(grepl("EDGEWOOD", runs$Pickup),"Edgewood", 
                    ifelse(grepl("BURKHART", runs$Pickup),"Burkhart",        
                    ifelse(grepl("CINCINNATI", runs$Pickup),"Cincinnati",       
                    ifelse(grepl("LOOKOUT HEIGHTS", runs$Pickup),"Lookout Heights",        
                    ifelse(grepl("TAYLOR MILL", runs$Pickup),"Taylor Mill", 
                    ifelse(grepl("LAS VEGAS", runs$Pickup),"Las Vegas",        
                    ifelse(grepl("PARK HILLS", runs$Pickup),"Park Hills",
                    ifelse(grepl("ROUSE", runs$Pickup),"ROUSE",       
                    ifelse(grepl("INDEPENDENCE", runs$Pickup),"Independence",      
                    ifelse(grepl("FORT MITCHELL", runs$Pickup),"FT Mitchell",
                    ifelse(grepl("FLORENCE", runs$Pickup),"Florence",
                    ifelse(grepl("LUDLOW", runs$Pickup),"Ludlow",
                    ifelse(grepl("VILLA HILLS", runs$Pickup),"Villa Hills",       
                    ifelse(grepl("ERLANGER", runs$Pickup),"Erlanger",
                    ifelse(grepl("NICHOLSON", runs$Pickup),"Nicholson",       
                    ifelse(grepl("MACHIAS", runs$Pickup),"Machias",  
                    ifelse(grepl("BELLEVUE", runs$Pickup),"Bellevue", 
                    ifelse(grepl("HEBRON", runs$Pickup),"Hebron",        
                    ifelse(grepl("ELSMERE", runs$Pickup),"Elsmere",as.character(runs$Pickup)))))))))))))))))))))))))))))))

runs <- unique(runs)


####Spell out acronyms
runs$CallType[runs$CallType == "MA"] <- "Medical Alarm"
runs$CallType[runs$CallType == "MCI"] <- "Mass Cacuality Incident"
runs$CallType[runs$CallType == "OPNT"] <- "Other Pain, Non-Traumatic"
runs$CallType[runs$CallType == "PA"] <- "Patient Assist"
runs$CallType[runs$CallType == "CO Poison"] <- "Carbon Monoxide Poison"
runs$CallType[runs$CallType == "CP"] <- "Chest Pain"
runs$CallType[runs$CallType == "MVA"] <- "Motor Vehicle Accident"
runs$CallType[runs$CallType == "Abd Pain"] <- "Abdominal Pain"
runs$CallType[runs$CallType == "AMS"] <- "Altered Mental Status"
runs$CallType[runs$CallType == "GSW/Stab"] <- "Gun Shot Wound/Stab"
runs$CallType[runs$CallType == "C-Arrest"] <- "Cardiac Arrest"
runs$Disposition[runs$Disposition == "False Al/Unfounded/No Pt"] <- "False Alarm/Unfounded/No Patient"

##Recode empty cells that did not have a call type recorded
runs$CallType <- sub("^$", "Type Not Recorded", runs$CallType)
##Fix Pickup for Geocoding
runs$Pickup <- sub(" COV ", " COVINGTON", runs$Pickup)
runs$Pickup <- sub("COVINGTONKY ", " COVINGTON KY", runs$Pickup)
runs$Pickup <- sub("LATONIA", "COVINGTON", runs$Pickup)


####  Preparing Addresses for Mapping   #####
runs$Pickup <- gsub("#.\\d*|Apt.\\d*|#\\s.\\d*|[[:punct:]]|^\\|", " ", runs$Pickup)
n.coordinates <- geocode(runs$Pickup)
runs <- cbind(runs, n.coordinates)
  
####Add zero to NAs in lat and lon after geocoding----
for (i in 1:length(runs$lat)){
  if(is.na(runs$lat[i]))
    runs$lat[i] <- 0
}
for (i in 1:length(runs$lon)){
  if(is.na(runs$lon[i]))
    runs$lon[i] <- 0
}

#### Send to ArcGIS ####
send_arcgis <- function(dataframe, path, layerName){
  coordinates(dataframe) <- ~lon+lat
  ## Define Coordinate system for spatial points data.frame 
  reference <- CRS("+init=epsg:4326")
  proj4string(dataframe) <- reference
  ## Assign closest neighborhood and sector in ArcGIS
  writeOGR(obj = dataframe, dsn = path, layer = layerName, driver = 'ESRI Shapefile', overwrite_layer = TRUE)
}
send_arcgis(runs, "C:/Users/tsink/Mapping/Geocoding/EMS", "AmbulanceUpdate")

#### Receive from ArcGIS ####
receive_arcgis <- function(fromPath, dataframeName) {
  arc.check_product()
  ## Read GIS Features 
  read<- arc.open(fromPath)
  ## Create Data.Frame from GIS data 
  dataframeName <- arc.select(read)
  ## Bind hidden lat/long coordinates back to data frame 
  shape <- arc.shape(dataframeName)
  dataframeName<- data.frame(dataframeName, long=shape$x, lat=shape$y)
}
ambUpdate <- receive_arcgis("C:/Users/tsink/Mapping/Geocoding/EMS/Join_Output_2.shp", ambUpdate)  
  

#### Mutual Aid ####
# Assign First Mutual Aid 
ambUpdate$NbhdLabel[ambUpdate$Distance > 0] <- "Mutual Aid"

# Assign Closest Neighborhood in downtown sector but outside of neighborhod boundary 
ambUpdate$NbhdLabel <- ifelse(ambUpdate$Distance_1 == 0 & ambUpdate$NbhdLabel == "Mutual Aid", as.character(ambUpdate$NEIGHBORHO),
                              as.character(ambUpdate$NbhdLabel))

# Assign Mutual Aid to sector 
ambUpdate$Runcard[ambUpdate$Distance_1 > 0] <- "Mutual Aid"

# If call is inside neighborhood but outside sector 
ambUpdate$NbhdLabel <- ifelse(ambUpdate$Distance == 0 & ambUpdate$Distance_1 > 0, "Mutual Aid",
                              as.character(ambUpdate$NbhdLabel))

##Invalid coordinates and neighborhood indication
ambUpdate$NbhdLabel[ambUpdate$lat == 0] <- "Invalid Coordinates"
ambUpdate$Runcard[ambUpdate$lat == 0] <- "Invalid Coordinates"

#### Columns to Keep and Name ####
ambUpdate <- ambUpdate[, c(-1:-5, -19, -25:-28, -30:-41, -44)]
names(ambUpdate) <- c("Disposition", "PickUp", "Destination", "Unit", "Date", "DispatchedTime", "Enroute",
                      "OnLocation", "ToHospital", "AtHospitalTime", "InService", "Mileage", "ChiefComplaint",
                      "FoundToBe", "Count", "CallType", "Company", "Area", "NbhdLabel", "Id_1", "Runcard",
                      "long", "lat")

#### Write Files ####
sql_write <- function(connection, dbName, dbGname, Rfile, dbPull, RepoPath, TablPath,...) {
  #Database
  connection <- dbConnect(drv=RSQLite::SQLite(), dbname = dbName)
  dbWriteTable(connection, dbGname, Rfile, ...)
  #Pull entire database
  Rfile <- dbGetQuery(connection, dbPull)
  #CovStat Repository
  write.csv(Rfile, file=RepoPath, row.names = FALSE)
  #Tableau File
  write.csv(Rfile, file=TablPath, row.names = FALSE)
  dbDisconnect(connection)
}
dbName <- "O:/AllUsers/CovStat/Data Portal/repository/Data/Database Files/Fire.db", row.names = FALSE
dbPull <- 'select * from AmbulanceRuns'
RepoPath <- "O:/AllUsers/CovStat/Data Portal/Repository/Data/Fire_EMS/EMSRuns.csv"
TablPath <- "U:/CityWide Performance/CovStat/CovStat Projects/Fire/TableauFiles/EMSRuns.csv"
sql_write(cons.fire, dbName , "AmbulanceRuns", ambUpdate, dbPull, RepoPath, TablPath, append = TRUE)




###Google sheet load trial#
#library(gsheet)
#url <- 'docs.google.com/spreadsheets/d/1IE_R3er-NY8AxbFPyeJJQJC3QWPXUdmbhR4dNbmxQes/edit?usp=sharing'
#a <- gsheet2tbl(url)

#test <- port.ins[port.ins$NewLeaseDate > as.Date("1899-12-31"),]
#as.Date(x, origin = "1970-01-01")
