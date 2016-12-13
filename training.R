
setwd("U:/CityWide Performance/Fire/Training")

library("xlsx", lib.loc="~/R/win-library/3.2")
library("plyr", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library("tidyr", lib.loc="~/R/win-library/3.2.5")
library("reshape", lib.loc="~/R/win-library/3.2")
library("reshape2", lib.loc="~/R/win-library/3.2.5")
library("stringr", lib.loc="~/R/win-library/3.2")
library("zoo", lib.loc="~/R/win-library/3.2")
library("lubridate", lib.loc="~/R/win-library/3.2")
library("splitstackshape", lib.loc="~/R/win-library/3.2")


training <-  read.csv("TRAINING.csv", header=TRUE, stringsAsFactors = FALSE)



training <- training[c(-13,-14)]

#write to CovStat Repository
write.csv(training, file="O:/AllUsers/CovStat/Data Portal/Repository/Data/Fire_EMS/Training.csv", row.names = FALSE)


