library(dplyr)

head(airquality)

air <- filter(airquality, Month==6)
airquality[airquality$Month == 6,]
subset(airquality, subset=(Month==6))

# & |

air <- filter(airquality, Month == 6, Temp > 90)
air <- filter(airquality, Month == 6 & Temp > 90)
air <- filter(airquality, Ozone > 80 | Temp > 90)

# 행번호로 추출
slice(airquality,6:10)
slice(airquality,n())
slice(airquality,(n()-4):n())

# 정렬
air <- arrange(airquality, Temp, Month, Day)
air <- arrange(airquality, desc(Temp), Month, Day)
head(air)

# 열선택
air <- select(airquality, Month, Day, Temp)
head(air)

air <- select(airquality, Temp:Day)
head(air)

air <- select(airquality, -(Temp:Day))
head(air)

air <- select(airquality, Solar=Solar.R)

air <- rename(airquality, Solar=Solar.R)

# Month의 Unique한 값들은?
air <- distinct(select(airquality,Month))

air <- mutate(airquality, 
              Temp.C = (Temp-32)/1.8,
              Diff = Temp.C-mean(Temp.C))
head(air)


air <- mutate(airquality, 
              Temp.C = (Temp-32)/1.8,
              Diff = Temp.C-mean(Temp.C))

# transform은 막 생성한 변수 사용 불가
air <- transform(airquality,
          Temp.C = (Temp-32) / 1.8)

summarise(airquality, 
          Mean = mean(Temp, na.rm = TRUE),
          Median = median(Temp, na.rm= TRUE),
          SD  = sd(Temp, na.rm=TRUE),
          Max = max(Temp, na.rm=TRUE),
          n = n(),
          distinct_Month = n_distinct(Month),
          distinct_First = first(Month),
          distinct_Last= last(Month))

# 표본 추출 (행 추출)
sample_n(airquality,5)
sample_frac(airquality,0.05)

air_group <- group_by(airquality,Month)
air_group

summarise(air_group, 
          Mean.Temp=mean(Temp,na.rm=TRUE))
summarise(air_group, 
          Mean.Temp=mean(Temp,na.rm=TRUE),
          SD.Temp=sd(Temp,na.rm=TRUE),
          Days=n())

air <- airquality %>%
  select(Ozone,Temp,Month) %>%
  summarise(Mean.Ozone=mean(Ozone,na.rm=TRUE),
            Mean.Temp=mean(Temp,na.rm=TRUE)) %>%
  filter(Mean.Ozone > 40 | Mean.Temp > 80)
air












