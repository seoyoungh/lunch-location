library(dplyr)
library(ggplot2)

m1 <- read.csv("Final_2.csv")
m1$Dorm <- c(0)

for (i in c(1:nrow(m1))){
  if (m1$Key == "A5"|m1$Key == "D3"|m1$Key == "G5"|m1$Key == "H4"|m1$Key == "H5"){
    m1$Dorm<-replace(m1$Dorm, i, 1)
  }
}

# �� �߰� 
m1[,"Dorm"] = Dorm

# ���� �ۼ�
write.csv(m1, file = "Final_2.csv", row.names = FALSE)