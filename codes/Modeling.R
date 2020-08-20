data= read.csv("Final_model.csv")

# 3번 예외처리
data$IOS <- ifelse(data$Distance=='NA', 0, data$IOS)
data$IOS <- ifelse(data$IOS=='NA', 0, data$IOS)
data$IOS[is.na(data$IOS)]<-0


# 평균값 집어넣기

data[,c("Distance")][is.na(data[,c("Distance")])] <- 348.261763
data[,c("AvailableTime")][is.na(data[,c("AvailableTime")])] <- 6398.26519

# 기숙사 yes,no 변경
data$Dorm = ifelse(data$Dorm == 1, "YES","NO")



library(caret)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

library(rpart)
library(rpart.plot)

str(data)
#train/ test#
################################################

  index = createDataPartition(y= data$IOS, p = 0.7, list = FALSE)
  
  
  train = data[index,]
  test = data[-index,]
  train = train[,-c(1,2,3)]
  test = test[,-c(1,2,3)]
  
  # 잘기능했는지 확인 (0: 0.25, 1: 0.75)
  table(train$IOS)/sum(table(train$IOS))
  table(test$IOS)/sum(table(test$IOS))
  
  ##rpart 패키지
  rpart_tree = rpart(as.factor(IOS)~.,train)
  #rpart_tree
  
  par(mfrow = c(1,1))
  #plot(rpart_tree);text(rpart_tree, pretty = 0)
  
  #fancyRpartPlot(rpart_tree)
  
  #rpart.plot(rpart_tree, cex=0.7)
  
  rpart_pred = predict(rpart_tree,test,type= "class")
  confusionMatrix(rpart_pred,as.factor(test$IOS))
  cc = confusionMatrix(rpart_pred,as.factor(test$IOS))
  print(cc)



##############################################

#가지치기 전

####사진저장####
printcp(rpart_tree)
jpeg(filename = "pru.jpeg",width= 800, height = 800)
plotcp(rpart_tree)
dev.off()

jpeg(filename = "bef_p_model.jpeg",width= 1500, height = 1300)
fancyRpartPlot(rpart_tree)
dev.off()

###########################################3

cp = rpart_tree$cptable[which.min(rpart_tree$cptable[,"xerror"]),"CP"]



rpart.prune <- prune(rpart_tree,cp = rpart_tree$cptable[which.min(rpart_tree$cptable[,"xerror"]),"CP"])

rpart.plot(rpart.prune,cex = 0.7)

rpart_22 = predict(rpart.prune,test,type="class")



jpeg(filename = "aft_pru_model.jpeg",width= 1500, height = 1300)
fancyRpartPlot(rpart_22)
dev.off()

confusionMatrix(table(rpart_22,test$IOS))



