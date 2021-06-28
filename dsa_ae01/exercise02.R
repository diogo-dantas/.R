#dsa_aeds01

#defining the workbook
setwd("~/Área de Trabalho/practice/practiceR/dataset")

#install and load packages
library(tidyverse)

#load the dataset
data <- read.csv("dataset_exercise02.csv")
str(data)

#three colors of the best selling vehicles
colorData<-table(data$cor)
dfcor <- data.frame(colorData)
colnames(dfcor) <- c("Colors","Absolute Frequency")
arrange(dfcor,desc(dfcor$`Absolute Frequency`))

#year of the best selling car
dataYear <- table(data$ano)
dfYear <- data.frame(dataYear)
colnames(dfYear) <- c("Year","Absolute Frequency")
arrange(dfYear,desc(dfYear$`Absolute Frequency`))

#alternative solution - top 3
data %>% 
 count(ano) %>% 
 top_n(3,n) %>% 
 arrange(desc(n))

#barplot for colorData
barplot(colorData, main = "Counts")

#alternative solution
data %>% 
 count(cor) %>% 
 top_n(3,n) %>% 
 ggplot(aes(x = cor, y = n)) +
        geom_bar(stat = "identity") +
        coord_cartesian(y = c(0,50)) +
        labs(title = "Table Cars") +
        xlab("Colors") +
        ylab("Total vehicles sold") +
        theme_minimal()

#percentage of vehicles with automatic transmission
autoTrans <- table(data$transmissao)
percent_autoTrans <- round(prop.table(autoTrans)*100,1)
percent_autoTrans

#alternative solution
data %>% 
 group_by(transmissao) %>% 
 summarise(perc_sales_trans = n()) %>% 
 mutate("%" = round((perc_sales_trans / sum(perc_sales_trans)*100),3))

#pie chart for transmission sales analysis
df_autoTrans <- data.frame(percent_autoTrans)
df_autoTrans$Freq  <- paste(df_autoTrans$Var1, df_autoTrans$Freq, sep = " ") 
df_autoTrans$Freq <- paste(df_autoTrans$Freq, "%", sep = " " )
pie(percent_autoTrans,labels = df_autoTrans$Freq, col = rainbow(length(df_autoTrans$Freq)), main = "Pie chart for transmission sales analysis")






