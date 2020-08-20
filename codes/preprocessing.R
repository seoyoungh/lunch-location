library(dplyr)
library(ggplot2)

#csv 파일 제목 리스트 -해당 날짜 파일 없는 경우 리스트에서 삭제 바랍니다!
date <- c("04_15","04_16","04_17","04_18","04_19","04_22","04_23","04_24","04_25","04_26","04_29","04_30","05_01","05_02","05_03","05_07","05_08","05_09","05_10","05_13")


#왼쪽 대각선 함수
left_diagonal <- function(x,y){
  x1 = 127.083109
  y1 = 37.236679
  
  y2_y1 = 0.005788
  x2_x1 = -0.007226
  
  m = y2_y1/x2_x1
  x_x1= x-x1
  
  t = m * x_x1 + y1
  
  
  # 교내 밖이면 True, 아니면 False
  ifelse (y < t, return (T), return(F))
  
}

#오른쪽 대각선 함수
right_diagonal <- function(x,y){
  
  
  x1 = 127.081423
  y1 = 37.247570
  
  y2_y1 = -0.010891
  x2_x1 = 0.00883
  
  m = y2_y1/x2_x1
  x_x1= x-x1
  
  t = m * x_x1 + y1
  
  # 교내 밖이면 True, 아니면 False
  ifelse (y > t, return (T), return(F))
  
}

# 정문 옆 공원 제외 함수 
k_park <- function(x,y){
  if (x < 127.077140 & y > 37.24680){
    return (T)
  }
  else{
    return (F)
  }
}

# 마을 제외 함수 
k_town <- function(x,y){
  if (y <= 37.24680 & y > 37.246398){
    if (x < 127.07747){
      return (T)
    }
  }
  return (F)
}

cnt = 1
for (i in date){
  title <- paste(i,".csv",sep="")
  map <- read.csv(title)

  
  m1 <- data.frame(map$lat,map$lon,map$ns1.ele, map$ns1.time)

  m1 <- rename(m1, Latitude = map.lat, Longitude = map.lon, Elevation = map.ns1.ele, Timestamp = map.ns1.time)
  
  m2 <- m1
  
  
  
  #1 정문밖 자르기
  m1 <- m1[!(m1$Latitude > 37.247570),]
  
  #2 아래 자르기
  m1 <- m1[!(m1$Latitude < 37.236679),]
  
  #3 왼쪽 자르기
  m1 <- m1[!(m1$Longitude < 127.075883),]
  
  #4 오른쪽 자르기
  m1 <- m1[!(m1$Longitude > 127.090253),]
  
  ex_lst <- c()
  
  if (nrow(m1) != 0){
    for (k in 1:nrow(m1)){
      
      tmpx = m1[k,2]
      tmpy = m1[k,1]
      
      
      if(left_diagonal(tmpx, tmpy)){
        ex_lst <- c(ex_lst,k)
        next
      }else if(right_diagonal(tmpx,tmpy)){
        
        ex_lst <- c(ex_lst,k)
        next
        
      }else if(k_park(tmpx,tmpy)){
        
        ex_lst <- c(ex_lst,k)
        next
        
      }else if(k_town(tmpx,tmpy)){
        
        ex_lst <- c(ex_lst,k)
        next
      }
    }
  }else{ #서울캠 데이터의 경우
    save_title <- paste("dataset",cnt,".csv",sep="")
    cnt <- cnt + 1
    
    write.csv(m2, file = save_title, row.names = FALSE)
    
    next
  }
  
  
  if (length(ex_lst != 0)){
    count <- 0
    for (j in 1:length(ex_lst)){
      m1 <- m1[-(ex_lst[j]-count),]
      count <- count+1
    }
  }
  
  
  if (nrow(m1) != 0){
    
    #파일저장
    
    save_title <- paste("dataset",cnt,".csv",sep="")
    cnt <- cnt + 1
    
    write.csv(m1, file = save_title, row.names = FALSE)
    
    
    ###################################
    # 전처리가 잘 되었는지 이미지로   #
    # 확인을 원하지 않으시면 아래     #
    # 코드를 지워주세요!!!            # 
    ###################################
    
    
    #####지우시는 경우 여기부터#####
    
    #원본 이미지 저장
    save_jpg1 <- paste(i,".jpg",sep = "")
    ggplot(data = m2, aes(x = Longitude, y = Latitude))+geom_point(pch= 20)+ggtitle(paste(i,"original",sep="")) + 
      annotate("text", x = 127.08334, y =37.23957, label = "전자정보대학", size = 5, colour = "red")+ 
      annotate("text", x = 127.08080, y =37.24634, label = "공과대학", size = 5, colour = "red")+  
      annotate("text", x = 127.07845, y =37.24746, label = "국제캠 정문", size = 5, colour = "red")
    
    ggsave(file=save_jpg1, width=15, height=15, units=c("cm"))
    
    
    #전처리 이미지 저장
    save_jpg2 <- paste(i,"_trans.jpg",sep = "")
    ggplot(data = m1, aes(x = Longitude, y = Latitude))+geom_point(pch= 20, colour = "red")+ggtitle(paste(i,"_preprocessing",sep="")) + 
      annotate("text", x = 127.08334, y =37.23957, label = "전자정보대학", size = 5, colour = "black")+ 
      annotate("text", x = 127.08080, y =37.24634, label = "공과대학", size = 5, colour = "black")+  
      annotate("text", x = 127.07845, y =37.24746, label = "국제캠 정문", size = 5, colour = "black")
    
    ggsave(file=save_jpg2, width=15, height=15, units=c("cm"))
    
    ##### 여기까지만 지워주세요 #####
    
  }
}


