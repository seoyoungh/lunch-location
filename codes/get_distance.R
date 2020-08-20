library(dplyr)
library(lubridate)
library(Imap)

num <- 0
date <- c("dataset1","dataset2","dataset3","dataset4","dataset5")

# 6명인 조
#date <- c("dataset1","dataset2","dataset3","dataset4","dataset5","dataset6")

#place_list 번호 받아서 좌표 평균으로 리턴
xy_convert <- function(place_list){
  x_vector <- c()
  y_vector <- c()
 
  #+++++++++기준좌표+++++++++#
  
  ## 정문(37.247441, 127.078443)
  #4 멀관(37.24459, 127.07642)
  #5 공대(37.24637, 127.08067)
  #6 자대(37.23930, 127.08337)
  #7 예디(37.24172, 127.08451)
  #8 국제(37.24209, 127,08126)
  #9 생과(37.24286, 127.08128)
  #10 체대(37.24436, 127.08035)
  #11 외대(37.24511, 127.07764) 
  
  #+++++++++++++++++++++++++++#
  
  for(i in place_list){
    if(i == 4 | i == 400) {
      x_vector <- c(x_vector, 37.24459)
      y_vector <- c(y_vector, 127.07642)
      }
         
    else if(i==5 | i== 500) {
      x_vector <- c(x_vector, 37.24637)
      y_vector <- c(y_vector, 127.08067)
      }
         
    else if(i==6 | i== 600) {
      x_vector <- c(x_vector, 37.23930)
      y_vector <- c(y_vector, 127.08337)
      }
         
    else if(i==7 | i== 700) {
      x_vector <- c(x_vector, 37.24172)
      y_vector <- c(y_vector, 127.08451)
      }
         
    else if(i==8 | i== 800) {
      x_vector <- c(x_vector, 37.24209)
      y_vector <- c(y_vector, 127,08126)
      }
         
    else if(i==9 | i== 900) {
      x_vector <- c(x_vector, 37.24286)
      y_vector <- c(y_vector, 127.08128)
      }
         
    else if(i==10 | i== 1000) {
      x_vector <- c(x_vector, 37.24436)
      y_vector <- c(y_vector, 127.08035)
      }
         
    else {x_vector <- c(x_vector, 37.24511)
          y_vector <- c(y_vector, 127.07764)}
        
  }
  
  return(round(c(mean(x_vector),mean(y_vector)),5))
}

for (i in date){

  title = paste(i,".csv",sep="")
  pdata = read.csv(title)
  
  date_list = unique(pdata$Date)
  
  #distance초기화
  distance <- c()
  
  #날짜별 분류 후 시간제한
  for(j in date_list){
    filtered <- pdata%>%filter(Date==j)
    filtered <- filtered%>%filter(hour(Startpoint)>=11 & hour(Startpoint)<14 | hour(Endpoint)>=11 & hour(Endpoint)<14)
  
    
    place_list <- c() #place_list초기화
    
    place_iter <- c(4:11, c(4:11)*100) #place종류를 담은 vector
    
    # place가 있으면 vector에 추가
    for(q in place_iter){
      if(q %in% filtered$Place){place_list<-c(place_list, as.numeric(q))}
    }
    mean_xy <- xy_convert(place_list) #평균좌표
    
    #평균좌표와 정문좌표와의 거리를 m단위로 환산 후 distance vector에 append
    temp_dist <- gdist(lon.1 = 37.247441, lat.1 = 127.078443, lon.2 = mean_xy[1], lat.2 = mean_xy[2], units = "m")
    distance <- c(distance, temp_dist)
  }

  # key 할당
  num = num + 1
  key = paste("G",as.character(num),sep="") # 팀에 맞게 알파벳 변경
  
  # 파일 생성 부분
  dsave = data.frame(key, date_list, distance)
  colnames(dsave) = c('Key','Date', 'Distance(m)')
  
  # 임시로 Time.csv로 작성해 두었습니다
  dfile <- paste("Distance.csv",sep="")
  
  # 처음 dataset에서만 파일 만들고 그 후로는 CSV파일 존재하면 행추가
  if (!file.exists(dfile)){
    write.csv(dsave,file = dfile,row.names = F)
  }
  else{
    write.table(dsave, dfile, row.names = F, col.names = F, append = T, sep= ",")
  }
}