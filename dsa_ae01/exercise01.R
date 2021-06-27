#dsa_aeds01

install.packages("tidyverse")
library(tidyverse)

#directory definition
setwd("~/dataset")

#consult the current directory
getwd()

df <- read_csv("hour.csv")

#mean and median
summary(df)

#or

lapply(df, mean)
lapply(df, median)


#mode function

md <- lapply(df, table)
lapply(md, max)

#average temperature per weekday of the df

df$weekday[df$weekday=="0"] <- "Monday"
df$weekday[df$weekday=="1"] <- "Tuesday"
df$weekday[df$weekday=="2"] <- "Wednesday"
df$weekday[df$weekday=="3"] <- "Thursday"
df$weekday[df$weekday=="4"] <- "Friday"
df$weekday[df$weekday=="5"] <- "Saturday"
df$weekday[df$weekday=="6"] <- "Sunday"

options(digits = 2)

tempmean_wd <-group_by(df, weekday)%>% 
summarise(Total=mean(temp))

colnames(tempmean_wd) <-c("Days of the week","Temperature average")

view(tempmean_wd) 


        