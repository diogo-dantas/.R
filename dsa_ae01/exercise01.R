#dsa_aeds01

install.packages("tidyverse")

#directory definition
setwd("/home/tiodiovo/Área de Trabalho/practice/practiceR/dataset")

df <- read.csv("hour.csv")

#mean and median
summary(df)

#mode function
getmode <- function(v){
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v,uniqv)))]
}
        
seas_mode <- getmode(df$season)
print(seas_mode)

