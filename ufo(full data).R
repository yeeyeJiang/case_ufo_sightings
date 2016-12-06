setwd("C:\\Users\\jiang\\Desktop\\研究生\\研二上\\DataAnalysis-master\\课堂练习")
library(dplyr)
library(ggplot2)
library(zoo)

#pre-treatment

##loading data----
raw <- read.table("ufo_awesome.tsv",header = F ,
                       sep = "\t" , 
                       stringsAsFactors = F ,
                       fileEncoding = "UTF-8" ,
                       # colClasses = c("numeric" , "numeric"), as.is = T  --> 
                          #上面命令可以分辨是否日期数据中混入了文本数据
                          #Error in scan() expected 'a real', got 'IowaCity,IA'
                          #说明确实混入了，先按照文本读取，再在数据清洗这一步处理
                      
                       fill = T ,
                       na.strings = "" , 
                       col.names = c("date" , "enddate" ,
                                      "location" , "appearence" ,
                                      "lasting-time" , "details")
                       #,nrows = 10000 
                  )

##cleansing data----
#最难的是：formulate criteria ，distinguish boundary between normal& UNnormal

#出现问题：类型不符（就是上面colClass所指,以及一些结构形状不符,如:日期出现0000）
#                 -> 逻辑上的异常值 -> NA

#knowledge!
#******提取字符串函数******
#substr() 在字符串中提取子串 
#nchar()  提取字符串长度
#grep()   求某段字符在字符串向量（不是单个字符串）中的位置
#**************************

dataCleaned <- raw[1:3] 

###处理date变量中的类型不符_______________v

dateNA <- data.frame(
                rownum = 
                  which( nchar(dataCleaned$date) != 8 )
                )
dateNA[2:4] <- dataCleaned[dateNA$rownum , ]

for (i in 1:nrow(dateNA)) 
  dateNA$date[i] <- ifelse( nchar(dateNA$enddate[i]) == 8 ,
                            dateNA$enddate[i] ,NA )

dataCleaned[dateNA$rownum , 1] <- dateNA$date
rm(dateNA , i)

#以上，关于date的类型不符处理完毕，
#但是还有一个不足！！！
#就是有的date里面储存着其他变量的信息，根据以上处理是直接做成NA_______^

# detect logical outliers in date
range(dataCleaned$date , na.rm = T) #--> normal(only correct for first 10000)

#一般，date有错的，后面的location也会有错，反正最后是用state抽出要用数据，
#所以这里逻辑异常不处理
#it's covenient for analysis to put date in the format of "19900809" 


###处理location的类型不符__________________v

summary(
  
  nchar( #通过字数检测异常情况
    dataCleaned$location
    )
)
#**************result(for the first 10000)**************
#   Min.  1st Qu.  Median    Mean   3rd Qu.    Max.    NA's 
#   2.0    12.0     15.0     18.8     19.0    2819.0    43 
#*******************************************************
boxplot(
  nchar( 
  dataCleaned$location
))
 
hist(  nchar( #通过字数检测异常情况
          dataCleaned$location
            ) ,
       xlim = c(0 , 80) ,
       ylim = c(0 , 10) , #ylim 从2000，1000，200,40,20 往下逐步检查
       breaks = seq(0 , 3000 , by = 5)
)

#according to hists, results ,we need 检查65以上的字数，和5以下的字数
#but, take empirical knowledge into consideration ,
#a location can hardly be streched out within 5 words ,
#so ,go through locations with nchars below 10 words .

hist(  nchar(
  dataCleaned$location
) ,
xlim = c(65 , 70) ,
ylim = c(0 , 20) , 
breaks = seq(0 , 16000 , by = 0.5) ,
main = "location字数在65个以上的观测值频数" ,
xlab = "location字数"
)

pi <- 0.05 
#pi可以理解为经验出错率，用贝叶斯的思想，
#我们从以往的经验里得到这种数据一般的出错率在多少
#然后用在这个数据上，频率小于这个 pi的变量视为有错变量

hist(  nchar(
  dataCleaned$location
) ,
xlim = c(0 , 10) ,
ylim = c(0 , nrow(raw) * pi) ,  #通过不断调整x和y的范围，初步找出分界值
                                #进一步找出分界值需要通过查看原始信息
                                #尴尬的是，在这一标准下很难找出清晰的边界
                                #只能权衡舍弃少量混入异常值中的正确值

        #在10000观测值时，可以通过不断调整y的范围找出不连续点
        #但是full data时，异常值非常多（可能上千），这时候若还用10000时候的
        #y上限10甚至100去看，都是连续的
breaks = seq(0 , 16000 , by = 0.5)  ,
main = "location字数在10个以下的观测值频数" ,
xlab = "location字数"
)

#发现66以上和8以下出现频率不连续的观测值，考虑为异常值

dataCleaned %>%
  filter(nchar(location) %in% 66:70 ) #--> normal&unnormal both exist

dataCleaned %>% #
  filter(nchar(location) < 5)  #--> unnormal

dataCleaned %>%
  filter(nchar(location) >100)  #--> unnormal


locatNA <- data.frame()
locatNA <- data.frame(
  rownum = 
    which( #nchar(dataCleaned$location) < 8 |
           nchar(dataCleaned$location) > 65  )
 )
locatNA[2:4] <- dataCleaned[locatNA$rownum , ]
#handle one by one
locatNA <- locatNA[-3 , ]
dataCleaned <- dataCleaned[-locatNA$rownum , ] 

locatNA <- data.frame(
  rownum = 
    which( nchar(dataCleaned$location) < 5 )
 )
locatNA[2:4] <- dataCleaned[locatNA$rownum , ]
dataCleaned <- dataCleaned[-locatNA$rownum , ] 

boxplot(
  nchar( 
    dataCleaned$location 
  ))
   #--> normal
#only at the aspect of nchar ,but there still be many probelms behind the screen

rm(locatNA)

#location的类型不符处理完毕__________________^

##reading data----

#若以下代码中用df[1]提取列而不是df[,1]
#则所有代码都运行不了
#因为df[i]提取出来的是data.frame,后续的赋值会把df当做一个整体去赋值
#df[,i]提取出的是value的集合，跟c()的结果是一样的
#这种格式问题要注意，比如list取出的元素还是list

#ufoSight[1:2] <- as.Date.character(dataCleaned[,1:2] , format = "%Y%m%d")
#dataCleaned$yearmon <- format(as.yearmon(dataCleaned[,1]) , "%Y-%m")

x <- strsplit(dataCleaned[,3] , ",") #strsplit出来的结果是list
                            #若不想可以用函数unlist()去掉list结构
for(i in 1:length(x)){
  n <- length(x[[i]])
  if(n < 3){
  dataCleaned$city[i] <- x[[i]][1] #如果记录没有逗号，全部的信息
                               #(including state or country)就会分到city里
  dataCleaned$state[i] <- x[[i]][2]
  }
  else{
    dataCleaned$city[i] <- paste0(x[[i]][1:(n-1)] , collapse = "")
    dataCleaned$state[i] <- x[[i]][n] 
  }
}
rm(i,x,n)

#check whether 'state' includes outliers or not _____________________v
#sort entries of state by nchar(normal nchar is 2 ) 
#                                       --> still exists

stateNA <- data.frame()
stateNA <- data.frame(
  rownum = 
    which( nchar(dataCleaned$state) != 3
         )
 )
stateNA[2:6] <- dataCleaned[stateNA$rownum , ] #全部都是其他国家的obs.

#many observations is of other countries, triggering variety of texting formats
#for example,
#country names are put inside ()
#country names are put outside () ,or even there is no ()
#"," is not used for separating city&country ,but for site&city
#some contain only site ,but states can be interpreted in terms of the info.
#有的出现的真实地点本来就很模糊，记录的地点信息只能用句子描述
# ,然后city出现的是前半句，state里是句子的后半句
#有的有好多个逗号，有的又没有逗号分隔
#还有更尴尬的，有的从村儿开始写，然后就变成：A村,B县(然后后面不一定出现国家)
#...
#but, thanks to magority of the U.S's obs. being in tidy format ..haha

#所以，目前采取的方法无视，反正后面也是从所有观测值里用state的名字提取obs.
#但是，这么做也有缺陷：
#1. 有的观测值没有location信息，但其实他有可能属于美国的
#2. 有的观测值有city的信息，但是没有state的信息，可以认为判断出他属于美国 
#       ——> 但通过浏览stateNA里state == NA的观测值，发现不存在这种情况
#故，考虑到复杂性，目前先这样吧！！！嘎嘎~

rm(stateNA) #愉快地删掉stateNA WITHOUT DONING ANYTHING

#****************总结一下****************
#location的异常值处理：主要是解决detail混入location的问题，通过nchar发现
#state的异常值：主要是由于存在其他国家的观测值，通过看stateNA里具体情况
#date的异常值：主要是混入文本型变量，通过colClass执行出错发现

#关于state的类型和逻辑异常值处理结束__________________________________^

#group by state & yearmon----

dataCleaned %>%
  filter(nchar(state) == 3) -> ufoSight 

#虽然state只有两个字符，但是里面包含其他国家城市的缩写
#state50 <- sort(unique(ufoSight$state))
#fix(state50)

state50 <- c(" AK", " AL", " AR", " AZ", " CA", " CO", " CT"," DE",
  " FL", " GA", " HI", " IA", " ID", " IL", " IN", " KS",
  " KY", " LA", " MA", " MD", " ME", " MI", " MN", " MO",
  " MS", " MT", " NC", " ND", " NE", " NH", " NJ", " NM",
  " NV", " NY", " OH", " OK", " ON", " OR", " RI", " SC", 
  " SD", " TN", " TX", " UT", " VA", " VT", " WA", " WI", 
  " WV", " WY")

dataCleaned %>%
  filter( state %in% state50 ) ->sightofUS 

date = seq(
        as.Date("1990-01-01"),
        as.Date("2010-01-01"),
        by = "1 month" )

df1 <- data.frame( date , yearmon = format(date , "%Y-%m") )
format( 
  as.yearmon(
    as.Date.character(sightofUS[,1] , format = "%Y%m%d")) ,
  "%Y-%m"
  ) -> sightofUS$yearmon

sightofUS %>%
  group_by(yearmon , state) %>%
  summarise(numOfSight = n()) -> df2

sightByYearmon <- list()

for (i in unique(df2$state)) {
  
  df2 %>%
    filter(state == i) %>%
    right_join(df1, , by = "yearmon") -> x
  
  x[,2] <- i
  sightByYearmon[[i]] <- x
  
}
rm(x,i)

#handle NAs----

dtForPic <- sightByYearmon[[1]]
for(i in 2:length(unique(df2$state)))
  dtForPic <- rbind(dtForPic , sightByYearmon[[i]])

rm(i)

dtForPic$numOfSight[
  which(
    is.na(dtForPic$numOfSight)
    )
  ] <- 0

rm(df1 , df2 , date)

#plot----

ggplot(dtForPic)+
  geom_line(aes(date,numOfSight) , color = "darkblue" )+
  facet_wrap(~ state , nrow = 10)+
  #scale_x_discrete(breaks = c("1994-08","1994-12","1995-08"))+
  labs(list(
    title = "Number of UFO sightings by Month-Year and U.S. State (1990-2010)",
    x = "Years", y = "Number of Sightings"))+
  theme_bw(
    #strip.background = element_rect(color = "grey" , size = 0.5), 
    #strip.text = element_text(face = "bold" , size = 9),
    #panel.background = element_rect(color = "grey10" , fill = "white"),
    #panel.grid.major = element_line(color = "gray3" , size = 0.1),
    #axis.text = element_text(color = "black", face = "bold" , size = 8)
    )
 








