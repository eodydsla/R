# wide 포맷은 변수 갯수증가에 따라 열이 증가
# long 포맷 데이터는 변수 갯수가 증가해도2개의 열로 모든 데이터 표현 가능
# Country import export
#    A      100    200
#    B      250    720
#    C      530    920

# Country  type   amount
#    A    import    100
#    B    import    250
#    C    import    530
#    A    export    200
#    B    export    720
#    C    export    920

# melt () : wide -> long
# dcast () : long -> wide

library(reshape2)

aq.long <- melt(airquality, id.vars=c("Month","Day"))
head(aq.long)

aq.side <- dcast(aq.long,
                 Month + Day ~ variable,
                 value.var = "value")
head(aq.wide)

# 기본은 구분자 1개에 1개 데이터가 되어야 함
# 월별 30,31개씩 데이터 존재 (기본 : 데이터 수)

dcast(aq.long, Month~ variable)

# aggragate 사용
dcast(aq.long, Month~ variable,
      fun.aggregate = mean, na.rm=TRUE)


     

