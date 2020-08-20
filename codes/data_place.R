library(lubridate)

out <- function(x,y){ # 정문 부근
  if ( x <= 127.07936 & x >= 127.078274){
    if (y <= 37.247447 & y >= 37.244304){
      return (T)
    }
  }
  
  if (y >= 37.246458){
    return (T)
  }
  
  return(F)
}


box <- function(x,y){
  
  # 우정원 
  if ( x <= 127.07747 & x >= 127.07588){
    if (y <= 37.24640 & y >= 37.24548){
      return (1)
    }
  }
  
  # 제2긱여자동
  if ( x <= 127.07781 & x >= 127.07590){
    if (y <= 37.24400 & y >= 37.24329){
      return (2)
    }
  }
  
  #제2긱 남자동
  if (x <= 127.077625 & x >= 127.07680){
    if (y <= 37.24292 & y >= 37.242199){
      return (3)
    }
  }
  
  # 학생회관
  if ( x <= 127.08044 & x >= 127.07935){
    if (y <= 37.24227 & y >= 37.24168){
      return (4)
    }
  }
  
  # 멀티미디어관 
  if ( x <= 127.07697 & x >= 127.07588){
    if (y <= 37.24488 & y >= 37.24430){
      return (5)
    }
  }
  
  
  # 공과대학
  if (y >= 37.24532 & y <= 37.24743) {
    if (x >= 127.07993 & x <= 127.08142) {
      return (6)
    }
  }
  
  # 전자정보대학
  if (y >= 37.23878 & y <= 37.23983) {
    if (x >= 127.08279 & x <= 127.08396) {
      return (7)
    }
  }
  
  # 예술디자인대학
  if (y >= 37.24131 & y <= 37.24214) {
    if (x >= 127.08389 & x <= 127.08514) {
      return (8)
    }
  }
  
  # 국제대학
  if (y >= 37.24184 & y <= 37.24234) {
    if (x >= 127.08086 & x <= 127.08167) {
      return (9)
    }
  }
  
  #생대
  if (y >= 37.24266 & y <= 37.24306) {
    if (x >= 127.08080 & x <= 127.08177) {
      return (10)
    }
  }
  
  
  #체대
  if (y >= 37.24384 & y <= 37.24488) {
    if (x >= 127.07987 & x <= 127.08083) {
      return (11)
    }
  }
  
  #외대
  if (y >= 37.24476 & y <= 37.24546) {
    if (x >= 127.07693 & x <= 127.07836) {
      return (12)
    }
  }
  
  
  # else
  return (0)
}

dset <- c("dataset1","dataset2","dataset3","dataset4","dataset5")
#dset <- c("dataset1","dataset2","dataset3","dataset4")

#dset <- c("dataset1","dataset2","dataset3","dataset4","dataset5","dataset6")


for (m in dset){
  
  title = paste(m,".csv",sep="")
  map = read.csv(title)
  
  
  # 시간변환
  time <- map$Timestamp # 해당 조의 시간 변수를 작성해야함
  date <- strptime(time,format="%Y-%m-%dT%H:%M:%S")
  #date <- strptime(time,format="%Y-%m-%d%H:%M:%S")
  date<-ymd_hms(date, tz = "UTC")
  attributes(date)$tzone = "Asia/Seoul"
  map[,"Timestamp"] = date
  
  ########## 날짜 찾기 ##############
  
  # 변수 초기화
  len <- c(1:length(date))
  tmp = date[1]
  day_lst = c(make_date(2019,month(tmp),day(tmp)))
  day_idx = c(1)
  cnt = 1
  
  
  # 날짜가 변경되는 인덱스를 리스트에 추가
  for (i in len){
    day_d = make_date(2019,month(date[i]),day(date[i]))
    
    if (day_d != day_lst[cnt]){
      day_idx = c(day_idx, i)
      day_lst = c(day_lst, day_d)
      cnt = cnt + 1
      
    }
  }
  
  
  #### timestamp 사이가 20분 이상인 경우, place "out"으로 처리 ####
  
  
  ############ place "OUT" 처리 ##############
  
  # 연산을 위해 마지막 인덱스 추가
  day_idx = c(day_idx, length(date))
  day_idx
  
  # 날짜 별로 for문
  for (i in c(1:(length(day_idx)-1))){
    
    
    begin = day_idx[i]
    end = day_idx[i+1]-2
    
    # 첫 sp는 첫 timestamp로 설정
    startpoint = c(map$Timestamp[begin])
    endpoint = c()
    place = c(box(map$Longitude[begin],map$Latitude[begin]))
    place_idx = 1 
    
    for (k in c((begin+1):end)){ # 날짜 별 시간 가리키는 인덱스 k
      
      nd = map$Timestamp[k]    #현재시간
      td = map$Timestamp[k+1]   #다음시간
      
      nnd = as.numeric(nd)
      ntd = as.numeric(td)
      
      # place가 바뀌면 그 전 timestamp ep로, 해당 timestamp sp로 설정
      # 20분 이상 빈 경우
      if((ntd-nnd)>=1200){
        # 정문 쪽에서 나간 경우
        # exception - 체대에서 버스타고 나간경우
        if (out(map$Longitude[k],map$Latitude[k])){
          if (length(endpoint) == 0){
            endpoint = c(map$Timestamp[k])
          }else{
            endpoint = c(endpoint, nd)
          }
          
          place = c(place, -1)
          place_idx = place_idx + 1
          startpoint = c(startpoint,nd)
          next
        }
        else{ # 정문쪽 안나갔는데 끊긴 경우 
          if (length(endpoint) == 0){
            endpoint = c(map$Timestamp[k])
          }else{
            endpoint = c(endpoint, nd)
          }
          
          aa = box(map$Longitude[k-1],map$Latitude[k-1])
          bb = box(map$Longitude[k],map$Latitude[k])
          dd= box(map$Longitude[k+1],map$Latitude[k+1])
          if ((bb) != 0){
            place = c(place, bb*100)
          }else if (dd != 0){
            place = c(place,dd*100)
          }else if(aa != 0){
            place = c(place,aa*100)
          }else{
            place = c(place, 10000)
          }
          place_idx = place_idx + 1
          startpoint = c(startpoint,nd)
          next
        }
        
      }
      
      #20분 이상 안 빈 경우
      # 장소가 변경되었는가 기준으로 sp, ep 조절
      
      n_place = box(map$Longitude[k],map$Latitude[k])
      
      if (n_place != place[place_idx]){
        
        if (length(endpoint) == 0){
          endpoint = c(map$Timestamp[k])
        }else{
          endpoint = c(endpoint, map$Timestamp[k])
        }
        
        place = c(place,n_place)
        place_idx = place_idx +1 
        startpoint = c(startpoint,nd)
        
      }
      
    }
    
    
    # Add last endpoint of the day 
    if (length(startpoint) > length(endpoint)){
      if (startpoint[length(startpoint)] != map$Timestamp[end+1]){
        if (length(endpoint) == 0){
          endpoint = c(map$Timestamp[end+1])
        }else{
          endpoint = c(endpoint, map$Timestamp[end+1])
        }
      }
      else{
        startpoint = startpoint[-length(startpoint)]
        place = place[-length(place)]
      }
    }
    
    ###### 건물간 이동이 있을 때 중간에 반드시 0이 존재해야 함
    
    #요일별 for문 종료 -> csv 파일로 따로 저장
    f = data.frame(startpoint,endpoint,(as.numeric(endpoint)-as.numeric(startpoint)),place)
    f[,"date"] = day_lst[i]
    f = f[,c(5,1,2,3,4)] 
    colnames(f) = c('Date','Startpoint','Endpoint','Difference','Place')
    
    title_f = paste(m,"_place.csv",sep="")
    
    if (!file.exists(title_f)){
      write.csv(f,file = title_f,row.names = F)
    }
    else{
      write.table(f, title_f, row.names = F, col.names = F, append = T, sep= ",")
    }
    
    
  }
}

