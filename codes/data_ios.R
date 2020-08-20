library(dplyr)
library(lubridate)

final = read.csv("Temp_final.csv")



#### 기숙사 여부 ####

keyval = unique(final$Key)
size = length(final$Dorm)

dormval = c()


for(k in keyval){
  for (i in c(1:sizen)){
    if (k == final$Key[i]){
      dormval = c(dormval,final$Dorm[i])
      break
    }
  }
}

# 키 값에 대응하는 기숙사 값 생성한 데이터프레임
toto = data.frame(keyval, dormval)


#####################

conv_sec <- function(point){
  return(hour(point)*60*60 + minute(point)*60 + second(point))
}

#모든 조
dset <- c("dataset_A1","dataset_A2","dataset_A3","dataset_A5","dataset_B1","dataset_B2","dataset_B3","dataset_B4","dataset_B5","dataset_C1","dataset_C2","dataset_C3","dataset_C4","dataset_C5","dataset_D1","dataset_D2","dataset_D3","dataset_D4","dataset_D5","dataset_E1","dataset_E2","dataset_E3","dataset_E4","dataset_E5","dataset_F1","dataset_F2","dataset_F3","dataset_F4","dataset_F5","dataset_F6","dataset_G1","dataset_G2","dataset_G3","dataset_G4","dataset_G5")



all_ios <- c()
for (i in dset){
  print(paste("***********" ,i , "*********"))
  title = paste(i,".csv",sep="")
  pdata = read.csv(title)
  
  date_list = unique(pdata$Date)
  
  keykey = substr(i,9,10)
  
  ios <- c()
  
  #날짜별 분류 후 시간제한
  for(j in date_list){
    date_filter <- pdata%>%filter(Date==j)
    time_filter <- date_filter%>%filter(hour(Startpoint)>=11 & hour(Startpoint)<14 | hour(Endpoint)>=11 & hour(Endpoint)<14)
    
    
    st = ymd_hms(time_filter$Startpoint)
    et = ymd_hms(time_filter$Endpoint)
    
    p = time_filter$Place
    
    
    if (!(-1 %in% p)){
      
      ###############
      print("교내")
      ###############
      ios <- c(ios, 1)
    }else{
      #제2긱 학생인 경우
      didx = which(toto$keyval == keykey)
      dval = toto$dormval[didx]
      if (dval){ ## 후에 수정
        ###############
        print("-1존재, 제2긱 학생")
        ###############
        ios <- c(ios,0)
      }else{
        
        # 학관/제2긱에 20분 이상 머무른 경우
        
        flag = 1
        hj = c(2,3)
        TFhj = p %in% hj
        
        
        for (hjidx in c(1:length(TFhj))){
          if (TFhj[hjidx]){ #학관/제2긱에 머무른 경우
            # 그 차이가 20분 이상인 경우
            if (as.numeric(et[hjidx]) - as.numeric(st[hjidx]) >= 1200){
              ###############
              print("-1존재, 학관/제2긱 20분이상 머무름")
              ###############
              ios <- c(ios,1)
              flag = 0
              break
            }
          }
        }
        
        flag2 = 1
        # 학관/제2긱 20분이상 머무르지 않은 경우
        if (flag){
          # 중간에 빌 때
          
          TFout = p %in% -1
          for (outidx in c(1:length(TFout))){
            if (TFout[outidx]){ #p가 -1인 경우
              if (hour(et[outidx]) <= 14 & hour(st[outidx])>=11){
                ###############
                print("-1존재, 학/제 20분 안머무름, 중간 빌때")
                ###############
                ios <- c(ios, 0)
                flag2 = 0
                break
              }
            }
          }
          
          
          if (flag2){
            # 앞뒤 빌 때
            
            if(max(conv_sec(time_filter$Endpoint) > (12*60*60 + 30*60))){
              
              #{if(2시 이후에 찍힘): OS,  
              if(max(conv_sec(date_filter$Startpoint) > 14*60*60)){
                ###############
                print("-1존재, 학/제 20분 안머무름, 뒤 빌때, 2시 이후")
                ###############
                ios <- c(ios, 0)
              }
              else {#아니면 is 
                ###############
                print("-1존재, 학/제 20분 안머무름, 뒤 빌때, 2시 이후 아닌경우 ")
                ###############
                ios <- c(ios, 1)
              }
            }
            
            #else if(앞쪽이비는경우:12시반 기준으로 이전에 들어옴):IS
            else if(min(conv_sec(time_filter$Startpoint) < (12*60*60 + 30*60))){
              ###############
              print("-1존재, 학/제 20분 안머무름, 앞 빌때, 이전에 들어옴")
              ###############
              ios <- c(ios, 1)
            }
            else {
              ###############
              print("-1존재, 학/제 20분 안머무름, 앞 빌때, 이전에 들어옴 아닌경우 ")
              ###############
              ios <- c(ios, 0)
            }
            
          }
        }
        
      }
    }
  }
  

  #dataset별로 파일 저장 
  qqq= data.frame(date_list, ios)
  title_f = paste(i, "ios.csv",sep="")
  write.csv(qqq,file = title_f,row.names = F)
  

  
  all_ios <- c(all_ios, ios)
  
}



final[,"IOS"] = all_ios
write.csv(final,file = "Final.csv",row.names = F)