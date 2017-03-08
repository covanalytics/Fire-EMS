
setwd("U:/CityWide Performance/CovStat/CovStat Projects/Fire/Training")
library("plyr")

#### Load Update ####
update <- read.csv("Update.csv", header=TRUE, stringsAsFactors = FALSE)

#### SQLite storage ####
library("RSQLite")
cons.fire <- dbConnect(drv=RSQLite::SQLite(), dbname="O:/AllUsers/CovStat/Data Portal/repository/Data/Database Files/Fire.db")
dbWriteTable(cons.fire, "FireTraining", update, overwrite = TRUE)

## Load Database ---------------------------
dash_train <- dbGetQuery(cons.fire, 'select * from FireTraining')
write.csv(dash_train, "U:/CityWide Performance/CovStat/CovStat Projects/Fire/TableauFiles/FireTraining.csv", row.names = FALSE)
dbDisconnect(cons.fire)

#### Write to CovStat Repository ####
## Drop first and last name of staff ----
dash_train <- dash_train[c(-13,-14)]
write.csv(dash_train, file="O:/AllUsers/CovStat/Data Portal/Repository/Data/Fire_EMS/Training.csv", row.names = FALSE)


