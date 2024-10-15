buildings <- data.frame(location=c(1, 2, 3), name=c("building1", "building2", "building3"))
data <- data.frame(survey=c(1,1,1,2,2,2), LocationID=c(1,2,3,2,3,1),
                   efficiency=c(51,64,70,71,80,58))

(buildings <- data.frame(location=c(1, 2, 3), name=c("building1", "building2", "building3")))
buildings2 <- data.frame(location=c(5, 4, 6), name=c("building5", "building4", "building6"))
(allBuildings <- rbind(buildings, buildings2))
(AllBuildings <- merge(buildings, buildings2, by = "location", all = TRUE))
(AllBuildings <- merge(buildings, buildings2, by = NULL))

census <- read.csv(url("https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/state/totals/nst-est2020-alldata.csv"))  

infections_Pop <- merge(US_COVID_Data, census, by.x = "state", by.y = "NAME")
View(infections_Pop)
infections_Pop <- subset(infections_Pop, select = -c(SUMLEV))
