library(lubridate)


dset <- c("dataset1","dataset2","dataset3","dataset4","dataset5")
#dset <- c("dataset1","dataset2","dataset3","dataset4","dataset5","dataset6")

for (q in dset){
  print(q)
  title = paste(q,"_place.csv",sep="")
  pdata = read.csv(title)
  
  place = as.numeric(as.character(pdata$Place))
  difference = pdata$Difference
  
  pdata[,"Date"] = as.Date(pdata$Date)
  pdata[,"Startpoint"] = ymd_hms(pdata$Startpoint)
  pdata[,"Endpoint"] = ymd_hms(pdata$Endpoint)
  
  lenp = length(pdata$Place)
  
  
  
  # 날짜 먼저 뽑기
  day_lst = unique(pdata$Date)
  day_idx = c()
  
  for (k in day_lst){
    t = which(pdata$Date == k)
    day_idx = c(day_idx,t[1])
  }
  day_idx
  day_idx = c(day_idx,length(pdata$Date))
  
  #날짜 별 데이터 다루기
  for (m in c(1:(length(day_idx)-1))){
    
    begin = day_idx[m]
    end = day_idx[m+1]-2
    
    
    # place 값 변경 1
    if ((begin+1) == day_idx[m+1] || begin >= end){
      next
    }
    for (i in c((begin+1):end)){
      
      bp = place[i-1]
      pp = place[i]
      fp = place[i+1]
      
      #100곱해진 값
      if (pp >= 100){
        pp100 = pp/100
        if (bp == pp100 & fp == pp100){
          pp = bp
        }else if(bp == pp100){
          pp = bp
        }else if(fp == pp100)
          pp = fp
      }else{
        # 0 600 0 / 10000
      }
      
      place[i] = pp
      
      
      
      # place가 0인데 위 아래가 동일하다
      # => 해당 숫자로 변경
      if (pp == 0 & bp == fp){
        place[i] = bp
      }
      
    }
  }
  
  
  
  pdata[,"Place"] = place
  
  
  #title_a = paste(q, "_replace1.csv",sep="")
  #write.csv(pdata,file = title_a,row.names = F)
  
  
  ############################################################
  
  # 변경한 place값 토대로 재구성
  place= pdata$Place
  
  #날짜 별 데이터 다루기
  for (m in c(1:(length(day_idx)-1))){
    
    begin = day_idx[m]
    end = day_idx[m+1]-1
    
    ts = c(pdata$Startpoint[begin])
    
    
    te = c()
    tdf = c()
    tp = c(place[begin])
    
    tidx = 1
    
    if ((begin+1) == day_idx[m+1]|| begin > end){
      te = c(pdata$Endpoint[begin])
      diff = as.numeric(te[tidx]) - as.numeric(ts[tidx])
      tdf = c(tdf, diff)
    }
    else{
      for (i in c((begin+1):end)){
        if(place[i] != tp[tidx]){
          #장소 달라지면 시간 쪼개기
          if (length(te)==0){
            te = c(pdata$Endpoint[i-1])
          }else{
            te = c(te,pdata$Endpoint[i-1])
          }
          
          
          ts = c(ts,pdata$Endpoint[i-1])
          tp = c(tp,place[i])
          
          
          diff = as.numeric(te[tidx]) - as.numeric(ts[tidx])
          tdf = c(tdf, diff)
          
          tidx = tidx + 1
          
          
        }
      }
      
    }
    
    # endpoint
    if (length(ts) > length(te)){
      if (ts[length(ts)] != pdata$Endpoint[end]){
        if (length(te) == 0){
          te = c(pdata$Endpoint[end])
          diff = as.numeric(te[tidx]) - as.numeric(ts[tidx])
          tdf = c(tdf, diff)
          tidx = tidx + 1
        }else{
          te = c(te, pdata$Endpoint[end])
          diff = as.numeric(te[tidx]) - as.numeric(ts[tidx])
          tdf = c(tdf, diff)
          
          tidx = tidx + 1
        }
      }
      else{
        ts = ts[-length(ts)]
        tp = tp[-length(tp)]
        tidx = tidx - 1
      }
    }
    
    td = c(pdata$Date[begin])
    if (length(tp) > 1){
      for (i in c(2:length(tp))){
        td = c(td,pdata$Date[begin])
      }
    }
    
    if (m == 1){
      nd = td
      ns = ts
      ne = te
      ndf = tdf
      np = tp
    }else{
      nd = c(nd,td)
      ns = c(ns,ts)
      ne = c(ne,te)
      ndf = c(ndf,tdf)
      np = c(np,tp)
      
    }
    
    dsave =data.frame (nd,ns,ne,ndf,np)
    colnames(dsave) = c('Date','Startpoint','Endpoint','Difference','Place')
    
    #title_a = paste(q, "_placeclean1.csv",sep="")
    #write.csv(dsave,file = title_a,row.names = F)
    
  }
  
  
  ############################################################
  
  # place 변경 2
  place = pdata$Place
  
  # 그 부근을 지나간건데 건물 내로 잡힌 경우 다 0으로 만들기
  for (m in c(1:(length(day_idx)-1))){
    
    begin = day_idx[m]
    end = day_idx[m+1]-2
    
    # place 값 변경
    
    if ((begin+1) == day_idx[m+1]|| begin >= end){
      next
    }
    for (i in c((begin+1):end)){
      
      
      bp = place[i-1]
      pp = place[i]
      ppdiff = difference[i]
      fp = place[i+1]
      
      # 0 10 0인데 10이 180초 이하인경우 0으로
      if (bp == 0 & bp == fp){
        if (pp != 0 & ppdiff <= 180){
          place[i] = 0
        }
      }
      
      # 0 10 2  인데 10이 180초 이하면 0으로
      else if (bp ==0 & fp != pp){
        if (pp != 0 & ppdiff <= 180){
          place[i]= 0
        }
      } 
      else if(fp == 0 & bp != pp){
        if (pp!= 0 & ppdiff <= 180){
          place[i] = 0
        }
      }
      
      # 연속으로 숫자 합쳤는데 180초 이하이고 그 앞뒤에 0들이 분포했던 경우 -> 0으로 치환
      
    }
  }
  
  pdata[,"Place"] = place
  
  
  #title_a = paste(q, "_replace2.csv",sep="")
  #write.csv(pdata,file = title_a,row.names = F)
  
  
  
  ############################################################
  
  
  
  
  
  # 변경한 place값 토대로 재구성
  place = pdata$Place
  
  #날짜 별 데이터 다루기
  for (m in c(1:(length(day_idx)-1))){
    
    begin = day_idx[m]
    end = day_idx[m+1]-1
    
    ts = c(pdata$Startpoint[begin])
    
    te = c()
    tdf = c()
    tp = c(place[begin])
    
    tidx = 1
    
    if ((begin+1) == day_idx[m+1]|| begin > end){
      te = c(pdata$Endpoint[begin])
      diff = as.numeric(te[tidx]) - as.numeric(ts[tidx])
      tdf = c(tdf, diff)
    }
    else{
      for (i in c((begin+1):end)){
        if(place[i] != tp[tidx]){
          #장소 달라지면 시간 쪼개기
          if (length(te)==0){
            te = c(pdata$Endpoint[i-1])
          }else{
            te = c(te,pdata$Endpoint[i-1])
          }
          
          
          ts = c(ts,pdata$Endpoint[i-1])
          tp = c(tp,place[i])
          
          
          diff = as.numeric(te[tidx]) - as.numeric(ts[tidx])
          tdf = c(tdf, diff)
          
          tidx = tidx + 1
          
          
        }
      }
      
    }
    
    # endpoint
    if (length(ts) > length(te)){
      if (ts[length(ts)] != pdata$Endpoint[end]){
        if (length(te) == 0){
          te = c(pdata$Endpoint[end])
          diff = as.numeric(te[tidx]) - as.numeric(ts[tidx])
          tdf = c(tdf, diff)
          tidx = tidx + 1
        }else{
          te = c(te, pdata$Endpoint[end])
          diff = as.numeric(te[tidx]) - as.numeric(ts[tidx])
          tdf = c(tdf, diff)
          
          tidx = tidx + 1
        }
      }
      else{
        ts = ts[-length(ts)]
        tp = tp[-length(tp)]
        tidx = tidx - 1
      }
    }
    
    td = c(pdata$Date[begin])
    if (length(tp) > 1){
      for (i in c(2:length(tp))){
        td = c(td,pdata$Date[begin])
      }
    }
    
    if (m == 1){
      nd = td
      ns = ts
      ne = te
      ndf = tdf
      np = tp
    }else{
      nd = c(nd,td)
      ns = c(ns,ts)
      ne = c(ne,te)
      ndf = c(ndf,tdf)
      np = c(np,tp)
      
    }
    
    dsave =data.frame (nd,ns,ne,ndf,np)
    colnames(dsave) = c('Date','Startpoint','Endpoint','Difference','Place')
    
    title_a = paste(q, "_placecleanfinal.csv",sep="")
    write.csv(dsave,file = title_a,row.names = F)
    
  }
  
}

