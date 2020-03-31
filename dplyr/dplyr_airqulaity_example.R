library(dplyr)

head(airquality)

air <- filter(airquality, Month==6)
airquality[airquality$Month == 6,]
subset(airquality, subset=(Month==6))





