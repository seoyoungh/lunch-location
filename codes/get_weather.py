
# coding: utf-8

# In[42]:


#네이버 날씨 크롤링

#url따올 패키지 불러오기
from urllib.request import urlopen, Request

import urllib
#크롤링 패키지
from bs4 import BeautifulSoup

url_march = "https://weather.naver.com/period/pastWetrMain.nhn?ym=201903&naverRgnCd=02117"
page_march = urlopen(url_march)
soup3 = BeautifulSoup(page_march, "html.parser")
#print(soup3) #크롤링 성공 여부 확인용 

#print(soup3.find_all(class_="icon")) #날씨 태그 크롤링 성공 여부 확인용
infolist=[]
infolist=(soup3.find_all(class_="icon"))
#print(infolist)
weatherls=[] #날씨 정보만 담을 리스트 생성

for i in range(0,31): #전처리를 위하여각 아이템들을 str형태로 변환하기
    infolist[i] = str(infolist[i])
    j=0
    while (j<len(infolist[i])):
        j += 1;
        if (infolist[i][j:j+3] == "src" ): #날씨정보 바로 앞 태그
            weatherls.append((infolist[i][j+60:j+63]))
            
print(weatherls[1][1])

print(len(weatherls)) #31일 동안의 태그가 모두 리스트에 들어왔는지 확인
print(weatherls)
for i in range (0,31):
    if("s1" in weatherls[i]): #맑음태그 = s1
        weatherls[i]=5;
    elif("s21" == weatherls[i][0:3]): #구름많음태그 =s21
        weatherls[i]=3;
    elif("2" == weatherls[i][1]): #구름조금태그 = s2
        weatherls[i]=4;
    elif("s9" in weatherls[i]): #흐림태그 =s9
        weatherls[i]=2;
    elif("s4" in weatherls[i]): #비태그 =s4
        weatherls[i]=1;
    elif("s5" in weatherls[i]): #눈태그 = s5
        weatherls[i]=0;
        
print("3월 날씨 리스트 점수")
print(weatherls)
len(weatherls)


# In[39]:


#네이버 날씨 크롤링

#url따올 패키지 불러오기
from urllib.request import urlopen, Request

import urllib
#크롤링 패키지
from bs4 import BeautifulSoup

url_april= "https://weather.naver.com/period/pastWetrMain.nhn?ym=201904&naverRgnCd=02117"
page_april = urlopen(url_april)
soup4 = BeautifulSoup(page_april, "html.parser")
#print(soup3) #크롤링 성공 여부 확인용 

#print(soup3.find_all(class_="icon")) #날씨 태그 크롤링 성공 여부 확인용
infolist=[]
infolist=(soup4.find_all(class_="icon"))
#print(infolist)
weatherls=[] #날씨 정보만 담을 리스트 생성

for i in range(0,30): #전처리를 위하여각 아이템들을 str형태로 변환하기
    infolist[i] = str(infolist[i])
    j=0
    while (j<len(infolist[i])):
        j += 1;
        if (infolist[i][j:j+3] == "src" ): #날씨정보 바로 앞 태그
            weatherls.append((infolist[i][j+60:j+63]))
            
print(weatherls[1][1])

print(len(weatherls)) #31일 동안의 태그가 모두 리스트에 들어왔는지 확인
print(weatherls)
for i in range (0,30):
    if("s1" in weatherls[i]): #맑음태그 = s1
        weatherls[i]=5;
    elif("s21" == weatherls[i][0:3]): #구름많음태그 =s21
        weatherls[i]=3;
    elif("2" == weatherls[i][1]): #구름조금태그 = s2
        weatherls[i]=4;
    elif("s9" in weatherls[i]): #흐림태그 =s9
        weatherls[i]=2;
    elif("s4" in weatherls[i]): #비태그 =s4
        weatherls[i]=1;
    elif("s5" in weatherls[i]): #눈태그 = s5
        weatherls[i]=0;

print("4월 날씨 리스트 점수")
print(weatherls)
len(weatherls)


# In[43]:


#네이버 날씨 크롤링

#url따올 패키지 불러오기
from urllib.request import urlopen, Request

import urllib
#크롤링 패키지
from bs4 import BeautifulSoup

url_may= "https://weather.naver.com/period/pastWetrMain.nhn?ym=201905&naverRgnCd=02117"
page_may = urlopen(url_may)
soup5 = BeautifulSoup(page_may, "html.parser")
#print(soup3) #크롤링 성공 여부 확인용 

#print(soup3.find_all(class_="icon")) #날씨 태그 크롤링 성공 여부 확인용
infolist=[]
infolist=(soup5.find_all(class_="icon"))


weatherls=[] #날씨 정보만 담을 리스트 생성

for i in range(0,31): #전처리를 위하여각 아이템들을 str형태로 변환하기
    infolist[i] = str(infolist[i])
    j=0
    while (j<len(infolist[i])):
        j += 1;
        if (infolist[i][j:j+3] == "src" ): #날씨정보 바로 앞 태그
            weatherls.append((infolist[i][j+60:j+63]))
            
print(weatherls[1][1])

print(len(weatherls)) #31일 동안의 태그가 모두 리스트에 들어왔는지 확인
print(weatherls)
for i in range (0,31):
    if("s1" in weatherls[i]): #맑음태그 = s1
        weatherls[i]=5;
    elif("s21" == weatherls[i][0:3]): #구름많음태그 =s21
        weatherls[i]=3;
    elif("2" == weatherls[i][1]): #구름조금태그 = s2
        weatherls[i]=4;
    elif("s9" in weatherls[i]): #흐림태그 =s9
        weatherls[i]=2;
    elif("s4" in weatherls[i]): #비태그 =s4
        weatherls[i]=1;
    elif("s5" in weatherls[i]): #눈태그 = s5
        weatherls[i]=0;

print("5월 날씨 리스트 점수")
print(weatherls)


# In[ ]:


#아웃풋 요약
# 3월 날씨 리스트 점수 1~31
# [2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 1, 2, 5, 1, 1, 2, 2, 2, 2, 1, 1, 5, 0, 5, 2, 2, 2, 2, 4, 0, 1]
#4월 날씨 리스트 점수 1~30
#[5, 5, 2, 2, 2, 1, 4, 4, 1, 1, 1, 2, 2, 1, 5, 2, 4, 1, 2, 1, 1, 2, 1, 1, 1, 1, 4, 1, 1, 2]
#5월 날씨 리스트 점수 1~31 
#[2, 2, 2, 2, 2, 1, 5, 3, 2, 2, 2, 2, 1, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 3, 1, 100,100,100,100] #100은 결측치 28일 이후 데이터 없음

