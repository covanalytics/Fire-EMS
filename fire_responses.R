

setwd("U:/CityWide Performance/CovStat/CovStat Projects/Fire/Unit Responses")

library("xlsx")
library("plyr")
library("dplyr")
library("tidyr")
library("splitstackshape")
library("magrittr")
library("gmodels")
library("descr")
library("arcgisbinding")
library("sp")
library("spdep")
library("rgdal")
library("maptools")
library("ggmap")

#### Load Update ####
responses <-  read.csv("Update.csv", header=TRUE, stringsAsFactors = FALSE)
#change to calendar year
responses$Year <- "2017"
runs <- responses

#### NIFRS Codes ####
runs$G_codeN[runs$inci_type >= 100 & runs$inci_type <= 199] <- 1
runs$G_code[runs$inci_type >= 100 & runs$inci_type <= 199] <- "Fire"
runs$G_codeN[runs$inci_type >= 200 & runs$inci_type <= 299] <- 2
runs$G_code[runs$inci_type >= 200 & runs$inci_type <= 299] <- "Overpressure Rupture, Explosions, Overheat{no fire}"
runs$G_codeN[runs$inci_type >= 300 & runs$inci_type <= 399]  <- 3
runs$G_codeN[which(is.na(runs$inci_type))]  <- 3
runs$G_code[which(is.na(runs$inci_type))]  <- "Rescue & Emergency Medical Service Incident"
runs$G_code[runs$inci_type >= 300 & runs$inci_type <= 399] <- "Rescue & Emergency Medical Service Incident"
runs$G_codeN[runs$inci_type >= 400 & runs$inci_type <= 499] <- 4
runs$G_code[runs$inci_type >= 400 & runs$inci_type <= 499] <- "Hazardous Condition {No Fire}"
runs$G_codeN[runs$inci_type >= 500 & runs$inci_type <= 599 | runs$inci_type == 5531] <- 5
runs$G_code[runs$inci_type >= 500 & runs$inci_type <= 599 | runs$inci_type == 5531] <- "Service Call"
runs$G_codeN[runs$inci_type >= 600 & runs$inci_type <= 699] <- 6
runs$G_code[runs$inci_type >= 600 & runs$inci_type <= 699] <- "Good Intent Call"
runs$G_codeN[runs$inci_type >= 700 & runs$inci_type <= 799 | runs$inci_type == 7331 | runs$inci_type == 7431] <- 7
runs$G_code[runs$inci_type >= 700 & runs$inci_type <= 799 | runs$inci_type == 7331 | runs$inci_type == 7431] <- "False Alarm & False call"
runs$G_codeN[runs$inci_type >= 800 & runs$inci_type <= 899] <- 8
runs$G_code[runs$inci_type >= 800 & runs$inci_type <= 899] <- "Severe Weather & Natural disaster"
runs$G_codeN[runs$inci_type >= 900 & runs$inci_type <= 999] <- 9
runs$G_code[runs$inci_type >= 900 & runs$inci_type <= 999] <- "Special Incident Type"

#### Run Descriptions ####
###Fire codes----
runs$S_code[runs$inci_type >= 111 & runs$inci_type <= 118] <- "Structure Fire"
runs$S_code[runs$inci_type >= 120 & runs$inci_type <= 123] <- "Fire in mobile property used as fixed structure"
runs$S_code[runs$inci_type >= 130 & runs$inci_type <= 138] <- "Mobile property (vehicle) fire"
runs$S_code[runs$inci_type >= 140 & runs$inci_type <= 143] <- "Natural vegetation fire"
runs$S_code[runs$inci_type >= 150 & runs$inci_type <= 155] <- "Outside rubbish fire"
runs$S_code[runs$inci_type >= 160 & runs$inci_type <= 164] <- "Special outside fire"
runs$S_code[runs$inci_type >= 170 & runs$inci_type <= 173] <- "Cultivated vegetation, crop fire"

###Overpressure codes----
runs$S_code[runs$inci_type == 200] <- "Overpressure rupture, explosion, overheat, Other"
runs$S_code[runs$inci_type >= 210 & runs$inci_type <= 213] <- "Overpressure rupture from steam (no ensuing fire)"
runs$S_code[runs$inci_type >= 220 & runs$inci_type <= 223] <- "Overpressure rupture from air or gas no fire)"
runs$S_code[runs$inci_type == 231] <- "Overpressure rupture, chemical reaction (no fire)"
runs$S_code[runs$inci_type >= 240 & runs$inci_type <= 244] <- "Explosion (no fire)"
runs$S_code[runs$inci_type == 251] <- "Excessive heat, scorch burns with no ignition"

####Rescue and Emergency codes----
runs$S_code[runs$inci_type == 300] <- "Rescue, emergency medical call (EMS), other"
runs$S_code[runs$inci_type == 311] <- "Medical assist"
runs$S_code[runs$inci_type >= 320 & runs$inci_type <= 324] <- "Emergency medical services (EMS) Incident"
runs$S_code[runs$inci_type == 331] <- "Lock-In"
runs$S_code[runs$inci_type >= 340 & runs$inci_type <= 343 ] <- "Search for lost person"
runs$S_code[runs$inci_type >= 350 & runs$inci_type <= 357] <- "Extrication, rescue"
runs$S_code[runs$inci_type >= 360 & runs$inci_type <= 365] <- "Water or ice-related rescue"
runs$S_code[runs$inci_type >= 370 & runs$inci_type <= 372] <- "Electrical rescue"
runs$S_code[runs$inci_type == 381] <- "Rescue or EMS standby"
runs$S_code[which(is.na(runs$inci_type))]  <- "Ambulance Runs"
runs$descript[which(is.na(runs$inci_type))]  <- "Ambulance Runs (EMS only)"

####Hazardous Condtion codes----
runs$S_code[runs$inci_type == 400] <- "Hazardous condition, Other"
runs$S_code[runs$inci_type >= 410 & runs$inci_type <= 413] <- "Combustible/flammable spills & leaks"
runs$S_code[runs$inci_type >= 420 & runs$inci_type <= 424] <- "Chemical release, reaction, or toxic condition"
runs$S_code[runs$inci_type == 430 & runs$inci_type <= 431] <- "Radioactive condition"
runs$S_code[runs$inci_type >= 440 & runs$inci_type <= 445] <- "Electrical wiring/equipment problem"
runs$S_code[runs$inci_type == 451] <- "Biological Hazard"
runs$S_code[runs$inci_type >= 460 & runs$inci_type <= 463 ] <- "Accident, potential accident"
runs$S_code[runs$inci_type == 471] <- "Explosive, bomb removal"
runs$S_code[runs$inci_type >= 480 & runs$inci_type <= 482] <- "Attempted burning, illegal action"

####Service Call codes----
runs$S_code[runs$inci_type == 500] <- "Service call, other"
runs$S_code[runs$inci_type >= 510 & runs$inci_type <= 512] <- "Person in distress"
runs$S_code[runs$inci_type >= 520 & runs$inci_type <= 522] <- "Water problem"
runs$S_code[runs$inci_type == 531] <- "Smoke, odor problem"
runs$S_code[runs$inci_type >= 540 & runs$inci_type <= 542] <- "Animal problem or rescue"
runs$S_code[runs$inci_type >= 550 & runs$inci_type <= 555 | runs$inci_type == 5531] <- "Public service assistance"
runs$S_code[runs$inci_type == 561] <- "Unathorized burning"
runs$S_code[runs$inci_type == 571] <- "Cover assignment, standby at fire station, move-up"

####Good Intent Call codes----
runs$S_code[runs$inci_type == 600] <- "Good intent call, Other"
runs$S_code[runs$inci_type == 611] <- "Dispatched and cancelled en route"
runs$S_code[runs$inci_type >= 621 & runs$inci_type <= 622] <- "Wrong location, no emergency found"
runs$S_code[runs$inci_type >= 631 & runs$inci_type <= 632] <- "Controlled burning"
runs$S_code[runs$inci_type == 641] <- "Vicinity alarm"
runs$S_code[runs$inci_type >= 650 & runs$inci_type <= 653] <- "Steam, Other gas mistaken for smoke"
runs$S_code[runs$inci_type == 661] <- "EMS call where party has been transported"
runs$S_code[runs$inci_type >= 671 & runs$inci_type <= 672] <- "HazMat release investigation w/no HazMat"

####False alarm and False call codes----
runs$S_code[runs$inci_type == 700] <- "False alarm and false call, Other"
runs$S_code[runs$inci_type >= 710 & runs$inci_type <= 715] <- "Malicious, mishievous false alarm"
runs$S_code[runs$inci_type == 721] <- "Bomb scare"
runs$S_code[runs$inci_type >= 730 & runs$inci_type <= 736 | runs$inci_type == 7331] <- "System or detector malfunction"
runs$S_code[runs$inci_type >= 740 & runs$inci_type <= 746 | runs$inci_type == 7431] <- "Unintentional system/detector operation (no fire)"
runs$S_code[runs$inci_type == 751] <- "Biohazard scare"

####Severe weather & natural disaster codes----
runs$S_code[runs$inci_type == 800] <- "Severe Weather & Natural Disaster - Other"
runs$S_code[runs$inci_type >= 811 & runs$inci_type <= 815] <- "Severe Weather & Natural Disaster - Specified"

####Special incident type codes----
runs$S_code[runs$inci_type == 900] <- "Special type of incident, other"
runs$S_code[runs$inci_type == 911] <- "Citizen complaint"

####Month Assignment----
runs$date <- ifelse(grepl("Jul", runs$alm_date),"July",
                    ifelse(grepl("Sep", runs$alm_date),"September",
                    ifelse(grepl("Oct", runs$alm_date),"October",       
                    ifelse(grepl("Nov", runs$alm_date),"November",
                    ifelse(grepl("Dec", runs$alm_date),"December",
                    ifelse(grepl("Mar", runs$alm_date),"March",
                    ifelse(grepl("Apr", runs$alm_date),"April",     
                    ifelse(grepl("May", runs$alm_date),"May",       
                    ifelse(grepl("Jun", runs$alm_date),"June",      
                    ifelse(grepl("Aug", runs$alm_date),"August", 
                    ifelse(grepl("Jan", runs$alm_date),"January", 
                    ifelse(grepl("Feb", runs$alm_date),"February", ""))))))))))))

#### Company Name ####
runs$Company[runs$unit == "PAR1" | runs$unit == "R1" | runs$unit == "A1" | runs$unit == "T1" | runs$unit == "C2"
             | runs$unit == "C9" | runs$unit == "C1" | runs$unit == "C3" | runs$unit == "C4" | runs$unit == "PM1" 
             | runs$unit == "FB1" | runs$unit == "P1" ] <- "Company 1: Eastside (100 E Robbins St)"
runs$Company[runs$unit == "P2"] <- "Company 2: West Covington (1252 Parkway Ave)"
runs$Company[runs$unit == "P5"] <- "Company 5: South Covington (1255 Hands Pike)"
runs$Company[runs$unit == "P6" | runs$unit == "A4" | runs$unit == "A6"] <- "Company 6: Peaselburg (1502 Holman Ave)"
runs$Company[runs$unit == "P8" | runs$unit == "T7" | runs$unit == "A2" | runs$unit == "A3" | runs$unit == "A8"| runs$unit == "P7"] <- "Company 8: Latonia (3315 Church St)"
runs$Company[runs$unit == "A5"] <- "Special Event"


####Assign unit A-3 to proper location in FY 2014 and 2015----
runs$Company[runs$unit =="A3"& runs$Fiscal.Year == "2014"] <- "Company 5: South Covington (1255 Hands Pike)"
runs$Company[runs$unit =="A3"& runs$Fiscal.Year == "2015"] <- "Company 5: South Covington (1255 Hands Pike)"

##########################################################
#### Spatial Data Creation                            ####
#### Assign closest neighborhood and sector in ArcGIS ####
#### Join neighborhoodds first, then fire sectors     ####
##########################################################

#### Create Spatial Points Data.Frame from Lat/Long Coordinates ----
runsSP <- runs
coordinates(runsSP) <- ~longitude+latitude

### Define Coordinate system for spatial points data.frame ----
reference <- CRS("+init=epsg:4326")
proj4string(runsSP) <- reference

#### Write spatial points data.frame to a shapefile ----
writeOGR(obj = runsSP, dsn ="C:/Users/tsink/Mapping/Geocoding/Fire", 
         layer = "FireUnitResponseUpdate", driver = 'ESRI Shapefile', overwrite_layer = TRUE)

#########################
####Connect to ArcGIS####
#########################
#### Initialize arcgisbinding----
arc.check_product()

#### Read GIS Features -----
readFIREU<- arc.open("C:/Users/tsink/Mapping/Geocoding/Fire/RESPONSES2.shp")

#### Create Data.Frame from GIS data -----
fireUGIS <- arc.select(readFIREU)

####Bind hidden lat/long coordinates back to data frame ------------
shape <- arc.shape(fireUGIS)
fireUGIS<- data.frame(fireUGIS, long=shape$x, lat=shape$y)

#### Mutual Aid ####
# Assign First Mutual Aid ------
fireUGIS$NbhdLabel[fireUGIS$Distance > 0] <- "Mutual Aid"

# Assign Closest Neighborhood for calls in downtown sector but outside of neighborhod boundary -----
fireUGIS$NbhdLabel <- ifelse(fireUGIS$Distance_1 == 0 & fireUGIS$NbhdLabel == "Mutual Aid", as.character(fireUGIS$NEIGHBORHO),
                            as.character(fireUGIS$NbhdLabel))

# Assign Mutual Aid to fire sector -----
fireUGIS$Runcard[fireUGIS$Distance_1 > 0] <- "Mutual Aid"

# If call is inside neighborhood but outside sector -----
fireUGIS$NbhdLabel <- ifelse(fireUGIS$Distance == 0 & fireUGIS$Distance_1 > 0, "Mutual Aid",
                            as.character(fireUGIS$NbhdLabel))

#### Delete and Rename Columns ####
fireUGIS <- fireUGIS[, c(-1:-3, -47:-50, -52:-63, -66)]
names(fireUGIS) <- c("inci_no", "expo_no", "inci_type", "descript", "alm_date", "number", "st_prefix",
                     "street", "st_type", "addr_2", "city", "zip", "mutl_aid", "descript_b", "disp_time", "alm_time",
                     "arv_time", "clr_time", "shift", "prop_loss", "no_prop_lo", "cont_loss", "no_cont_lo",
                     "prop_val", "no_prop_va", "cont_val", "no_cont_va", "fire_sprd", "descript_c",
                     "unit_id", "unit", "notif_time", "roll_time", "cancel_tim", "arv_time_b", "clr_time_b", "in_time",
                     "Year", "G_codeN", "G_code", "S_code", "date", "Company", "NbhdLabel", "Id_1",
                     "Runcard", "longitude", "latitude")

###   SQLite Database ####
### Append new data -----
library("RSQLite")
cons.fire <- dbConnect(drv=RSQLite::SQLite(), dbname="O:/AllUsers/CovStat/Data Portal/repository/Data/Database Files/Fire.db")
dbWriteTable(cons.fire, "FireUnitResponse", fireUGIS, overwrite = TRUE)

#### Load all data and write over tableau file -----

dash_runsFU <- dbGetQuery(cons.fire, 'select * from FireUnitResponse')
write.csv(dash_runsFU, "U:/CityWide Performance/CovStat/CovStat Projects/Fire/TableauFiles/FireUnitResp.csv")
dbDisconnect(cons.fire)

#### CovStat Repository ----
write.csv(dash_runsFU, file="O:/AllUsers/CovStat/Data Portal/Repository/Data/Fire_EMS/FireResponses.csv")







