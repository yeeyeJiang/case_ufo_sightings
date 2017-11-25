#setwd("C:\\Users\\jiang\\Desktop\\研究生\\研二上\\DataAnalysis-master\\课堂练习")
library(dplyr)
library(ggplot2)
library(zoo)

#pre-treatment

##loading data----
raw <- read.table("ufo_awesome.tsv",header = F ,
                       sep = "\t" , 
                       stringsAsFactors = F ,
                       fileEncoding = "UTF-8" ,                      
                       fill = T ,
                       na.strings = "" , 
                       col.names = c("date" , "enddate" ,
                                      "location" , "appearence" ,
                                      "lasting-time" , "details")
                       #,nrows = 10000 
                  )

##cleansing data----

dataCleaned <- raw[1:3] 

###handle the problem brought by non-uniformed class in the variable "data" _______________v

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

#__________________________________________________________________________________________^

# detect logical outliers in date
range(dataCleaned$date , na.rm = T) #--> normal(only correct for first 10000)

#it's easier for analysis to put date in the format of "19900809" 


##handle the problem brought by non-uniformed class in the variable "location"_____________v

summary(
  
  nchar( #detect the outliers via field length
    dataCleaned$location
    )
)

boxplot(
  nchar( 
  dataCleaned$location
))
 
hist(  nchar(
          dataCleaned$location
            ) ,
       xlim = c(0 , 80) ,
       ylim = c(0 , 10) , #ylim goes downwards in the way like 2000，1000，200,40,20 
       breaks = seq(0 , 3000 , by = 5)
)

#according to hists, results ,we need to check those observations whose field lengths are greater than 65, also the ones below 5

#but, take empirical knowledge into consideration ,
#a location can hardly be streched out within 5 words ,
#so ,go through locations with nchars below 10 words .

hist(  nchar(
  dataCleaned$location
) ,
xlim = c(65 , 70) ,
ylim = c(0 , 20) , 
breaks = seq(0 , 16000 , by = 0.5) ,
main = "location字数在65个以上的观测值频数" , #frequency where the fields length is over 65
xlab = "location字数"
)

pi <- 0.05 
#parameter pi can be taken as error rate
#according to the hint got from Bayes framework, we can give a empirical proper value to pi

hist(  nchar(
  dataCleaned$location
) ,
xlim = c(0 , 10) ,
ylim = c(0 , nrow(raw) * pi) ,  #this step is mutual, one need to tune the "xlim" and "ylim" several times till rough values being found

breaks = seq(0 , 16000 , by = 0.5)  ,
main = "location字数在10个以下的观测值频数" , #frequency where the fields length is below 10
xlab = "location字数"
)

#the smoothness in frenquency stop at 66 and 8, so the ones below 8 and over 66 are reasonable taken as outliers

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

#__________________________________________________________________________________________________^

##reading data----

#ufoSight[1:2] <- as.Date.character(dataCleaned[,1:2] , format = "%Y%m%d")
#dataCleaned$yearmon <- format(as.yearmon(dataCleaned[,1]) , "%Y-%m")

x <- strsplit(dataCleaned[,3] , ",") 
for(i in 1:length(x)){
  n <- length(x[[i]])
  if(n < 3){
  dataCleaned$city[i] <- x[[i]][1] 
                               
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
stateNA[2:6] <- dataCleaned[stateNA$rownum , ] 

#many observations are of other countries, triggering chaos of texting format
#for example,
#country names are put inside ()
#country names are put outside () ,or even there is no ()
#"," is not used for separating city&country ,but for site&city
#some contain only site ,but states can be interpreted in terms of the info.
#but, thanks to magority are of the U.S's obvs. being in tidy format 

rm(stateNA) #delete stateNA WITHOUT DONING ANYTHING
#_____________________________________________________________________^

#group by state & yearmon----

dataCleaned %>%
  filter(nchar(state) == 3) -> ufoSight 

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
