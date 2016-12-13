
###Responses
setwd("U:/OpenGov/Unique Reports/Fire/Data")

library("xlsx")
library("plyr")
library("dplyr")
library("tidyr")
library("splitstackshape")
library("magrittr")
library("gmodels")
library("descr")

# Read the contents of the file into a data.frame
###Just read in for current update
responses17 <-  read.csv("Responses17.csv", header=TRUE, stringsAsFactors = FALSE)
responses17$FiscalYear <- "2017"
runs <- responses17

responses16 <-  read.csv("Responses16.csv", header=TRUE, stringsAsFactors = FALSE)
responses16$FiscalYear <- "2016"

responses15 <-  read.csv("Responses15.csv", header=TRUE, stringsAsFactors = FALSE)
responses15$FiscalYear <- "2015"

responses14 <-  read.csv("Responses14.csv", header=TRUE, stringsAsFactors = FALSE)
responses14$FiscalYear <- "2014"

runs <- do.call("rbind", list(responses14, responses15, responses16, responses17))

##runs$alm_date <- as.Date(runs$alm_date, "%d-%B-%Y")

#####################################################################################################################################

#### Assign General Fire Codes based on first digit in code number in a column called G_code"
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


#####################################################################################################################################
####Drop rows with EMS only runs; these have "NA" in inci_type column
##runs <- runs[!is.na(runs$inci_type),]

####Assign Next Level Groups based on first tow digits in code number in a column called S_code
###Fire codes
runs$S_code[runs$inci_type >= 111 & runs$inci_type <= 118] <- "Structure Fire"
runs$S_code[runs$inci_type >= 120 & runs$inci_type <= 123] <- "Fire in mobile property used as fixed structure"
runs$S_code[runs$inci_type >= 130 & runs$inci_type <= 138] <- "Mobile property (vehicle) fire"
runs$S_code[runs$inci_type >= 140 & runs$inci_type <= 143] <- "Natural vegetation fire"
runs$S_code[runs$inci_type >= 150 & runs$inci_type <= 155] <- "Outside rubbish fire"
runs$S_code[runs$inci_type >= 160 & runs$inci_type <= 164] <- "Special outside fire"
runs$S_code[runs$inci_type >= 170 & runs$inci_type <= 173] <- "Cultivated vegetation, crop fire"

###Overpressure codes
runs$S_code[runs$inci_type == 200] <- "Overpressure rupture, explosion, overheat, Other"
runs$S_code[runs$inci_type >= 210 & runs$inci_type <= 213] <- "Overpressure rupture from steam (no ensuing fire)"
runs$S_code[runs$inci_type >= 220 & runs$inci_type <= 223] <- "Overpressure rupture from air or gas no fire)"
runs$S_code[runs$inci_type == 231] <- "Overpressure rupture, chemical reaction (no fire)"
runs$S_code[runs$inci_type >= 240 & runs$inci_type <= 244] <- "Explosion (no fire)"
runs$S_code[runs$inci_type == 251] <- "Excessive heat, scorch burns with no ignition"

####Rescue and Emergency codes
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

####Hazardous Condtion codes
runs$S_code[runs$inci_type == 400] <- "Hazardous condition, Other"
runs$S_code[runs$inci_type >= 410 & runs$inci_type <= 413] <- "Combustible/flammable spills & leaks"
runs$S_code[runs$inci_type >= 420 & runs$inci_type <= 424] <- "Chemical release, reaction, or toxic condition"
runs$S_code[runs$inci_type == 430 & runs$inci_type <= 431] <- "Radioactive condition"
runs$S_code[runs$inci_type >= 440 & runs$inci_type <= 445] <- "Electrical wiring/equipment problem"
runs$S_code[runs$inci_type == 451] <- "Biological Hazard"
runs$S_code[runs$inci_type >= 460 & runs$inci_type <= 463 ] <- "Accident, potential accident"

runs$S_code[runs$inci_type == 471] <- "Explosive, bomb removal"
runs$S_code[runs$inci_type >= 480 & runs$inci_type <= 482] <- "Attempted burning, illegal action"

####Service Call codes
runs$S_code[runs$inci_type == 500] <- "Service call, other"
runs$S_code[runs$inci_type >= 510 & runs$inci_type <= 512] <- "Person in distress"
runs$S_code[runs$inci_type >= 520 & runs$inci_type <= 522] <- "Water problem"
runs$S_code[runs$inci_type == 531] <- "Smoke, odor problem"
runs$S_code[runs$inci_type >= 540 & runs$inci_type <= 542] <- "Animal problem or rescue"
runs$S_code[runs$inci_type >= 550 & runs$inci_type <= 555 | runs$inci_type == 5531] <- "Public service assistance"
runs$S_code[runs$inci_type == 561] <- "Unathorized burning"
runs$S_code[runs$inci_type == 571] <- "Cover assignment, standby at fire station, move-up"

####Good Intent Call codes
runs$S_code[runs$inci_type == 600] <- "Good intent call, Other"
runs$S_code[runs$inci_type == 611] <- "Dispatched and cancelled en route"
runs$S_code[runs$inci_type >= 621 & runs$inci_type <= 622] <- "Wrong location, no emergency found"
runs$S_code[runs$inci_type >= 631 & runs$inci_type <= 632] <- "Controlled burning"
runs$S_code[runs$inci_type == 641] <- "Vicinity alarm"
runs$S_code[runs$inci_type >= 650 & runs$inci_type <= 653] <- "Steam, Other gas mistaken for smoke"
runs$S_code[runs$inci_type == 661] <- "EMS call where party has been transported"
runs$S_code[runs$inci_type >= 671 & runs$inci_type <= 672] <- "HazMat release investigation w/no HazMat"

####False alarm and False call codes
runs$S_code[runs$inci_type == 700] <- "False alarm and false call, Other"
runs$S_code[runs$inci_type >= 710 & runs$inci_type <= 715] <- "Malicious, mishievous false alarm"
runs$S_code[runs$inci_type == 721] <- "Bomb scare"
runs$S_code[runs$inci_type >= 730 & runs$inci_type <= 736 | runs$inci_type == 7331] <- "System or detector malfunction"
runs$S_code[runs$inci_type >= 740 & runs$inci_type <= 746 | runs$inci_type == 7431] <- "Unintentional system/detector operation (no fire)"
runs$S_code[runs$inci_type == 751] <- "Biohazard scare"

####Severe weather & natural disaster codes
runs$S_code[runs$inci_type == 800] <- "Severe Weather & Natural Disaster - Other"
runs$S_code[runs$inci_type >= 811 & runs$inci_type <= 815] <- "Severe Weather & Natural Disaster - Specified"

####Special incident type codes
runs$S_code[runs$inci_type == 900] <- "Special type of incident, other"
runs$S_code[runs$inci_type == 911] <- "Citizen complaint"

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




####Assign Company Name based on responding unit#####
runs$Company[runs$unit == "PAR1" | runs$unit == "R1" | runs$unit == "A1" | runs$unit == "T1" | runs$unit == "C2"
             | runs$unit == "C9" | runs$unit == "C1" | runs$unit == "C3" | runs$unit == "C4" | runs$unit == "PM1" 
             | runs$unit == "FB1" | runs$unit == "P1" ] <- "Company 1: Eastside (100 E Robbins St)"

runs$Company[runs$unit == "P2"] <- "Company 2: West Covington (1252 Parkway Ave)"

runs$Company[runs$unit == "P5"] <- "Company 5: South Covington (1255 Hands Pike)"

runs$Company[runs$unit == "P6" | runs$unit == "A4" | runs$unit == "A6"] <- "Company 6: Peaselburg (1502 Holman Ave)"

runs$Company[runs$unit == "P8" | runs$unit == "T7" | runs$unit == "A2" | runs$unit == "A3" | runs$unit == "A8"| runs$unit == "P7"] <- "Company 8: Latonia (3315 Church St)"

runs$Company[runs$unit == "A5"] <- "Special Event"


####Assign unit A-3 to proper location in FY 2014 and 2015
runs$Company[runs$unit =="A3"& runs$Fiscal.Year == "2014"] <- "Company 5: South Covington (1255 Hands Pike)"
runs$Company[runs$unit =="A3"& runs$Fiscal.Year == "2015"] <- "Company 5: South Covington (1255 Hands Pike)"


####For Geocoding Current Update###
runs1 <- runs
runs1 <- subset(runs1, UpdateMonth == "16-Nov")
write.csv(runs1, file="C:/Users/tsink/Mapping/Geocoding/Fire/FIRE_RESPONSES.csv")


##Write file for Tableau#######
#write.csv(runs, file="U:/OpenGov/Unique Reports/Fire/Reports from R/FIRE_RESPONSES.csv")
###covert to Excel with sheet name = 'Sheet1'

#runs <- subset(runs,FiscalYear != "2017" )
#write.csv(runs, file="U:/OpenGov/Unique Reports/Fire/Reports from R/FIRE_RESPONSES.csv")
#save(runs, file="U:/OpenGov/Unique Reports/Fire/Reports from R/Robjects/runs.RData")

##Write file for Response Times in OpenGov#######
###Make sure to add a new column calculating Response Time (Arv Time - Dispatch Time)*24*60
write.csv(runs, file="U:/OpenGov/Unique Reports/Fire/Data/FireRESPONSESTimeOpenGov.csv")






####Runs for OpenGov#####
runs$count <- 1

keeps <- names(runs) %in% c("descript", "FiscalYear", "G_code", "S_code", "unit", "Company", "count")
runs <- runs[keeps]
runs <- runs[c("FiscalYear", "G_code", "S_code", "descript", "Company", "unit", "count")]
names(runs) <- c("Fiscal Year", "Run Code", "Incident Type","Description", "Company", "Unit", "Count")

runs <- subset(runs, Company != "")
runs <- subset(runs, Company != "Special Event")

#runs$`Mutual Aid Type` <- sub("^$", "none", runs$`Mutual Aid Type`)
#runs$`Mutual Aid Type` <- sub("None", "none", runs$`Mutual Aid Type`)
#runs$`Mutual Aid Dept.` <- sub("^$", "none", runs$`Mutual Aid Dept`)

write.csv(runs, file="U:/OpenGov/Unique Reports/Fire/Data/FireRESPONSEOpenGov.csv")

######Make sure freqeuncey column is of numeric class
#freq$Freq <- as.numeric(freq$Freq)


###Response time###############
##Make sure to have added a new column calculating Response Time (Arv Time - Alarm Time)*24*60
fire.time <-  read.csv("FireRESPONSESTimeOpenGov.csv", header=TRUE, stringsAsFactors = FALSE)
fire.time$Count <- 1 #Add count for each record to use in getting average response time for each year
fire.time<- subset(fire.time, fire.time$ResponseTime>0 & fire.time$ResponseTime<=50)

###Aggregating minutes by year and company
fire.time <- aggregate(cbind(ResponseTime, Count) ~  FiscalYear + Company, fire.time, sum)
fire.time <- fire.time[order(fire.time$FiscalYear, fire.time$Company, -fire.time$Count),]

####Getting total average minutes by dividing total call minutes by number of calls
fire.time$Avg.Minutes <- fire.time$ResponseTime/ fire.time$Count
fire.time$Avg.Minutes <- round(fire.time$Avg.Minutes, 2)


fire.time$ResponseTime <- NULL
fire.time$Count <- NULL

names(fire.time) <- c("Fiscal Year", "Company", "Average Company Response (Minutes)")

fire.time <- subset(fire.time, Company != "Special Event")

write.csv(fire.time, file="U:/OpenGov/Unique Reports/Fire/Data/FireRESPONSESTimeOpenGov.csv")


