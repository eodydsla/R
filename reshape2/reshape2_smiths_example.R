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

smiths
melt(data=smiths)
melt(data=smiths, id.vars=c("subject"))
melt(data=smiths, measure.vars=c(2:5))
melt(data=smiths, measure.vars=c("time","age","weight","height"))
melt(data=smiths, id.vars=c("subject","time"))
smiths.long <- melt(data=smiths, 
                   measure.vars=c("time","age","weight","height"),
                   variable.name="var",
                   value.name="val")

# dcast(data, formula)
dcast(data=smiths.long, formula=subject~var)

# 마지막열을 값이 있는 열로 간주
dcast(data=smiths.long, formula=subject~var,value.var='val')

head(airquality)

     

