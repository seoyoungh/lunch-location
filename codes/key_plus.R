library(dplyr)
library(lubridate)


# 5명인 조
dset <- c("dataset1","dataset2","dataset3","dataset4","dataset5")

# 6명인 조
#dset <- c("dataset1","dataset2","dataset3","dataset4","dataset5", "dataset6")

k = "G" ######

for (i in c(1:length(dset))){
  print(paste("***********" ,dset[i] , "*********"))
  title = paste("E:/경희대학교/2019_1/데이터마이닝/CODE수정/dataplace/team g/",dset[i],"_placecleanfinal.csv",sep="")
  pdata = read.csv(title)
  
  key = paste(k,i, sep="")
  pdata[,"Key"] = key
  
  title_f = paste("dataset_", key, ".csv",sep="")
  write.csv(pdata,file = title_f,row.names = F)
  
}