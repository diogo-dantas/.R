#defining the workbook
setwd("~/Área de Trabalho/practice/practiceR/dataset")

#load the dataset
data <- read.csv("dataset_exercise03.csv")
str(data)

#average service time
servTime <- data$tempo_telefone
mean(servTime)

#median service time
median(servTime)

#standard deviation and service time variance
sd(servTime)
var(servTime)

#32nd, 57th and 98th percentiles of service time and quartiles
quantile(servTime)
quantile(servTime, c(.32, .57, .98))

#interquantile interval of service time
IQR(servTime)

#boxplot service time
boxplot(servTime, horizontal = TRUE)

#covariance between service time and total number of customers
totalCust =data$clientes
cov(servTime,totalCust)

#absolute frequency of service time
range(servTime)
breaks <- seq(1.5, 5.5, by = 0.5)
servTime.cut <- cut(servTime,breaks,right = FALSE)
servTime.freq <- table(servTime.cut)
cbind(servTime.freq)
 
#histogram service time
colors <- c("red","yellow","blue","pink","green","cyan","brown","gray")
hist(servTime, right = FALSE, col = colors, main = "Service Time", xlab = "duration in minutes")

#relative frequency of service time
options(digits = 1)
servTime.relFreq <- (servTime.freq/nrow(data))
cbind(servTime.freq,servTime.relFreq)

#accumulated frequency line chart
freqCum <- c(0,cumsum(servTime.freq))
plot(breaks, freqCum, main = "Service Time x Total Costumers", xlab = "duration in minutes", ylab = "customers")

#scatterplot
plot(servTime, totalCust, xlab = "Service Time", ylab = "Total Customers")

