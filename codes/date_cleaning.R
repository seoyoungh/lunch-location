library(dplyr)
library(lubridate)

#pdata
pdata = read.csv("final.csv", header = TRUE, stringsAsFactors = FALSE, na.strings ="")
pdata <- subset(pdata, AirScore != "NA"  & AvailableTime != 0)
write.csv(pdata, file="Final_2.csv", row.names = F)
