
setwd("U:/CityWide Performance/Fire/Training")

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


training <-  read.csv("TRAINING.csv", header=TRUE, stringsAsFactors = FALSE)


#Drop first and last name of staff
training <- training[c(-13,-14)]

#write to CovStat Repository
write.csv(training, file="O:/AllUsers/CovStat/Data Portal/Repository/Data/Fire_EMS/Training.csv", row.names = FALSE)


