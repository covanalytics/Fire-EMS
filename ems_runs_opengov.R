

setwd("U:/OpenGov/Unique Reports/EMS Runs")

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

#Read the contents of each file into a data.frame
##Calculate response time prior to loading "Onlocation - Dispatch Time" *24*60
runs14  <- read.csv("EMS_Runs14.csv", header=TRUE,  stringsAsFactors = FALSE)
runs14$Fiscal.Year <- "2014"
runs14$Count <- 1
runs14$CallType <- ifelse(grepl("Overdose", runs14$DispatchedAs), "Overdose/Suspected Overdose", as.character(runs14$DispatchedAs)) 

runs15  <- read.csv("EMS_Runs15.csv", header=TRUE,  stringsAsFactors = FALSE)
runs15$Fiscal.Year <- "2015"
runs15$Count <- 1
runs15$CallType <- ifelse(grepl("Overdose", runs15$DispatchedAs), "Overdose/Suspected Overdose", as.character(runs15$DispatchedAs)) 

runs16  <- read.csv("EMS_Runs16.csv", header=TRUE,  stringsAsFactors = FALSE)
runs16$Fiscal.Year <- "2016"
runs16$Count <- 1
runs16$CallType <- ifelse(grepl("Overdose", runs16$DispatchedAs), "Overdose/Suspected Overdose", as.character(runs16$DispatchedAs)) 

runs17  <- read.csv("EMS_Runs17.csv", header=TRUE,  stringsAsFactors = FALSE)
runs17$Fiscal.Year <- "2017"
runs17$Count <- 1
runs17$CallType <- ifelse(grepl("Overdose", runs17$DispatchedAs), "Overdose/Suspected Overdose", as.character(runs17$DispatchedAs)) 


#####Put files together
runs <- do.call("rbind", list( runs14,runs15, runs16, runs17))

####Remove practice/training runs####
runs$IncidentNum <- ifelse(grepl("del", runs$IncidentNum), "PRACTICE", 
                    ifelse(grepl("DEL", runs$IncidentNum), "PRACTICE", as.character(runs$IncidentNum)))
runs$PickUpAddress <- ifelse(grepl("delete", runs$PickUpAddress), "PRACTICE", as.character(runs$PickUpAddress))
runs <- subset(runs, IncidentNum!= "PRACTICE" & PickUpAddress!= "PRACTICE")


####Assign Company Name based on responding unit#####
runs$Company[runs$Unit == "PAR-1" | runs$Unit == "R-1" | runs$Unit == "A-1"] <- "Company 1: Eastside (100 E Robbins St)"
runs$Company[runs$Unit == "P-2"] <- "Company 2: West Covington (1252 Parkway Ave)"
runs$Company[runs$Unit == "P-5"] <- "Company 5: South Covington (1255 Hands Pike)"
runs$Company[runs$Unit == "P-6" | runs$Unit == "A-4" | runs$Unit == "A-6"] <- "Company 6: Peaselburg (1502 Holman Ave)"
runs$Company[runs$Unit == "P-8" | runs$Unit == "T-7" | runs$Unit == "A-2" | runs$Unit == "A-3" | runs$Unit == "A-8"] <- "Company 8: Latonia (3315 Church St)"
runs$Company[runs$Unit == "A-5"] <- "Special Event"

####Assign Unit A-3 to proper location in FY 2014 and 2015
runs$Company[runs$Unit =="A-3"& runs$Fiscal.Year == "2014"] <- "Company 5: South Covington (1255 Hands Pike)"
runs$Company[runs$Unit =="A-3"& runs$Fiscal.Year == "2015"] <- "Company 5: South Covington (1255 Hands Pike)"


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

#dup <- runs[duplicated(runs),]  show the repeat entries

runs <- unique(runs)

###Drop fields not needed
drops <- names(runs) %in% c("DOS", "IMX", "IMX.1", "IncidentNum", "IncidentNum.1", "Dispatched", "Enroute", "RunType", "PickUpAddress", "MapPage", "DistrictStatus",
                            "Medic1", "Medic2", "Medic3", "Medic4", "DayName", "FireIncidentNumber", "RunType.1", "TotalCallTime", "MapPage.1",
                            "Patient", "PtGender", "Unit.1", "DestAddress", "Medic1.1", "Medic2.1", "Medic3.1", "Medic4.1", 
                            "DistrictStatus.1", "DispatchedAs", "Station", "ResponseType", "Avg1")
runs <- runs[!drops]

names(runs)[1] <- c("ResponseTime")
names(runs)[5] <- c("Unit")
names(runs)[6] <- c("Date")
names(runs)[7] <- c("DispatchedTime")
names(runs)[9] <- c("OnLocation")

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

###Mirrors file for OpenGov EMS Run Times#####
###Time from dispatch to arrival on scene
runtimes <- runs


runs$Pickup <- gsub("#.\\d*|Apt.\\d*|#\\s.\\d*|[[:punct:]]|^\\|", " ", runs$Pickup)
#####################################################
####  Preparing Addresses for Mapping   #####
#####################################################
##1st 2500
n1.runs <- runs[1:2500,]
n1.coordinates <- geocode(n1.runs$Pickup)

n1.runs <- cbind(n1.runs, n1.coordinates)
save(n1.runs, file="U:/OpenGov/Unique Reports/EMS Runs/n1.RData")
load("n1.RData")##View n1.runs to see object

### 2nd 2500
n2.runs <- runs[2501:5000,]
n2.coordinates <- geocode(n2.runs$Pickup)

n2.runs <- cbind(n2.runs, n2.coordinates)
save(n2.runs, file="U:/OpenGov/Unique Reports/EMS Runs/n2.RData")
load("n2.RData")###View n2.runs to see object

### 3rd 2500
n3.runs <- runs[5001:7500,]
n3.coordinates <- geocode(n3.runs$Pickup)

n3.runs <- cbind(n3.runs, n3.coordinates)
save(n3.runs, file="U:/OpenGov/Unique Reports/EMS Runs/n3.RData")
load("n3.RData")###View n2.runs to see object

### 4th 2500
n4.runs <- runs[7501:10000,]
n4.coordinates <- geocode(n4.runs$Pickup)

n4.runs <- cbind(n4.runs, n4.coordinates)
save(n4.runs, file="U:/OpenGov/Unique Reports/EMS Runs/n4.RData")
load("n4.RData")###View n2.runs to see object

### 5th 2500
n5.runs <- runs[10001:12500,]
n5.coordinates <- geocode(n5.runs$Pickup)

n5.runs <- cbind(n5.runs, n5.coordinates)
save(n5.runs, file="U:/OpenGov/Unique Reports/EMS Runs/n5.RData")
load("n5.RData")###View n2.runs to see object

### 6th 2500
n6.runs <- runs[12501:15000,]
n6.coordinates <- geocode(n6.runs$Pickup)

n6.runs <- cbind(n6.runs, n6.coordinates)
save(n6.runs, file="U:/OpenGov/Unique Reports/EMS Runs/n6.RData")
load("n6.RData")###View n2.runs to see object

n4 <- n4.runs

runsT <- runs

runsT$Pickup <- gsub("#.\\d*|Apt.\\d*|#\\s.\\d*|[[:punct:]]|^\\|", " ", runsT$Pickup)

runsT$Pickup <- gsub("  ", " ", runsT$Pickup)
######Write file for Tableau#######
###Java limits prevent from writing to EXCEL.  Copy and paste CSV extract to the EXCEL file sheet 'FinalData' and refresh in Tableau
write.csv(runs, file="U:/OpenGov/Unique Reports/EMS Runs/EMS Runs.csv")
write.csv(runsT, file="C:/Users/tsink/Mapping/Geocoding/EMS/EMS RunsC.csv")
###write.xlsx(runsCombined, file="U:/OpenGov/Unique Reports/EMS Runs/EMS Runs.xlsx", sheetName="FinalData", append = TRUE)

##Write to CovStat Repository###
write.csv(runs, file="O:/AllUsers/CovStat/Data Portal/Repository/Data/Fire_EMS/EMS Runs.csv")




###Preparing runs file for OpenGov#######################################################################
runs <- subset(runs, Disposition!="Call cancelled" & Area != "Las Vegas")
runs <- subset(runs, Company != "Special Event")

runs$DispatchedTime <- NULL
runs$Enroute <- NULL
runs$OnLocation <- NULL
runs$ToHospitalTime <- NULL
runs$AtHospitalTime <- NULL
runs$InService <- NULL
runs$ResponseTime <- NULL
runs$RunType <- NULL
runs$Destination <- NULL
runs$FoundToBe <- NULL
runs$ChiefComplaint <- NULL
runs$Mileage <- NULL
runs$Date <- NULL


###Create group for top 10 and 'Other'
runs$`Call Type Group` <- ifelse(grepl("Medical", runs$CallType),"Medical", 
                        ifelse(grepl("Falls", runs$CallType),"Falls",                            
                        ifelse(grepl("Respiratory", runs$CallType),"Respiratroy",
                        ifelse(grepl("Psych", runs$CallType),"Psych",
                        ifelse(grepl("Chest Pain", runs$CallType),"Chest Pain", 
                        ifelse(grepl("Unconscious", runs$CallType),"Unconscious",
                        ifelse(grepl("Abdominal Pain", runs$CallType),"Abdominal Pain",
                        ifelse(grepl("Motor Vehicle Accident", runs$CallType),"Motor Vehicle Accident",
                        ifelse(grepl("Seizures", runs$CallType),"Seizures", 
                        ifelse(grepl("Overdose/Suspected Overdose", runs$CallType),"Overdose/Suspected Overdose", "Other"))))))))))

runs <- runs[c("Fiscal.Year", "Unit", "Company","Disposition", "CallType", "Call Type Group", "Area", "Count")]
names(runs)[5] <- "Call Type"
names(runs)[7] <- "Pick Up City"

write.csv(runs, file="U:/OpenGov/Unique Reports/EMS Runs/EMSruns_OPENGOV.csv", row.names=FALSE)


####Preparing run times file for OpenGov Dispatch to Arrival#################################################################
#runtimes$ResponseTime[which(is.na(runtimes$ResponseTime))]  <- "0"

runtimes$ResponseTime <- as.numeric(runtimes$ResponseTime)

###Removing outliers and false times
runtimes <- subset(runtimes, runtimes$ResponseTime>0 & runtimes$ResponseTime<=50)

###Aggregating minutes by company
runtimes$ResponseTime <- as.numeric(runtimes$ResponseTime)
runtimes <- aggregate(cbind(ResponseTime, Count) ~ Fiscal.Year + Company, runtimes, sum)
runtimes <- runtimes[order(runtimes$Company),]

####Getting average minutes by dividing total call minutes by number of calls
runtimes$Avg_Minutes <- runtimes$ResponseTime/ runtimes$Count
runtimes$Avg_Minutes <- round(runtimes$Avg_Minutes, 2)

runtimes$ResponseTime <- NULL
runtimes$Count <- NULL

names(runtimes) <- c("Fiscal Year", "Company", "Average Minutes")

runtimes <- subset(runtimes, Company != "Special Event")


###Writing file for EMS run times by Company
write.csv(runtimes, file="U:/OpenGov/Unique Reports/EMS Runs/EMSruntimes_OPENGOV.csv")






####Google sheet load trial######################################################################################
library(gsheet)
url <- 'docs.google.com/spreadsheets/d/1IE_R3er-NY8AxbFPyeJJQJC3QWPXUdmbhR4dNbmxQes/edit?usp=sharing'
a <- gsheet2tbl(url)


