

setwd("U:/CityWide Performance/Fire/Inspections")

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


inspections14 <-  read.csv("FY2014.csv", header=TRUE, stringsAsFactors = FALSE)
inspections14$FiscalYear <- "2014"

inspections15 <-  read.csv("FY2015.csv", header=TRUE, stringsAsFactors = FALSE)
inspections15$FiscalYear <- "2015"

inspections16 <-  read.csv("FY2016.csv", header=TRUE, stringsAsFactors = FALSE)
inspections16$FiscalYear <- "2016"

inspections17 <-  read.csv("FY2017.csv", header=TRUE, stringsAsFactors = FALSE)
inspections17$FiscalYear <- "2017"

inspections <- do.call("rbind", list (inspections14, inspections15, inspections16, inspections17))

#inspections <- cSplit(inspections, "descript", sep = "-", type.convert = character)

#write to CovStat Repository
write.csv(inspections, file="O:/AllUsers/CovStat/Data Portal/Repository/Data/Fire_EMS/Inspections.csv", row.names = FALSE)

#Write for geocoding
write.csv(inspections,"C:Users/tsink/Mapping/geocoding/Fire/Inspections.csv")