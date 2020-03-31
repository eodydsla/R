# 참고자료 : 이부일 교수님 Youtube

library(tidyverse)
# 예제 데이터 : ggplot2::diamonds
# 질적 자료   : cut, color, clarity
# 양적 자료 : caret, depth, table, price, x, y, z

# dyplr 패키지를 활용한 기술 통계량 구하기
# dplyr::summarise(data, 
#                  variable.name = function(variable))
# n(), mean(), mean(trim = ), median, mad(), IQR(),
# min(), max(), diff(range())

dplyr::summarise(diamonds,
                 n           = n(),
                 Mean        = mean(price),
                 TrimmedMean = mean(price,trim=0.05),
                 Median      = median(price),
                 Range       = diff(range(price)),
                 IQR         = IQR(price),
                 SD          = sd(price),
                 MAD         = mad(price),
                 Q1          = quantile(price,0.25),
                 Q3          = quantile(price,0.75))

# %>% Forward-Pipe Opeartor
# magrittr 패키지에서 제공하는 기능

# data %>% function() %>% function()

diamonds %>%
  dplyr::group_by(cut,color) %>%
  dplyr::summarise(n           = n(),
                   Mean        = mean(price),
                   SD          = sd(price),
                   Min         = min(price),
                   Max         = max(price))

writeex1::write_xlsx(result,path=".")


## 1. Select
## 데이터에서 열 추출(Slicing)
## dplyr::select(data,variable)

dplyr::select(diamonds, carat)
dplyr::select(diamonds, carat, dpeth, x)
dplyr::select(diamonds, color:x)
dplyr::select(diamonds, -carat)
dplyr::select(diamonds, -c(dpeth,price))
dplyr::select(diamonds, -c(color:x))

# 변수명에 패턴이 있는 경우
# i. 변수명에 특정 문자를 포함하고 있는 경우
colnames(diamonds)
dplyr::select(diamonds, contains("c"))

# ii. 변수명 중에서  특정한 문자로 시작하는 경우
dplyr::select(diamonds, starts_with("c"))

# iii. 변수명 중에서 특정한 문자로 끝나는 경우
dplyr::select(diamonds, ends_with("e"))

# 2. filter()
# 행을 추출하기
# dplyr :: filter(data, 조건)
# 비교연산자 : >, >=, <, <=, ==, !=, !
# 논리연산자 : ,, &, |
# is.na(), !is.na()
# between(variable, left,right)

# i. cut이 'Fair'인 데이터
dplyr::filter(diamonds, cut == "Fair")

# ii. cut이 'Fair'인 데이터
dplyr::filter(diamonds, cut != "Fair")

# iii. price가 18000보다 이상인데이터 
dplyr::filter(diamonds, price >= 18000)

# iv.  price가 18000보다 이하인데이터 
dplyr::filter(diamonds, price <= 18000)

# v. cut은 "Fair"이고 price는 18000이상인 데이터
dplyr::filter(diamonds, cut == "Fair", price >= 18000)
dplyr::filter(diamonds, cut == "Fair" & price >= 18000)

# vi. cut은 "Fair"이거나 또는 price는 18000이상인 데이터
dplyr::filter(diamonds, cut == "Fair" | price >= 18000)

# vii. price중에서 결측치(missing value)인 데이터
dplyr::filter(diamonds, is.na(price))

# ix. price중에서 결측치가 없는 데이터
dplyr::filter(diamonds, !is.na(price))

# x. price가 15000이상 ~ 18000이하인 데이터
dplyr::filter(diamonds, between(price,left=15000,right=18000))

# xi. cut이 "Good"이거나 또는 "Very Good"인 데이터
# 원소 %in% 집
dplyr::filter(diamonds, cut == "Fair" | cut == "Very Good")
dplyr::filter(diamonds, cut %in% c("Fair","Very Good"))






