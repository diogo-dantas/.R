#dsa_aeds01 - Data Analysis in Credit Card Operators 

#defining the workbook
setwd("~/Área de Trabalho/practice/practiceR/dataset")

#install and load packages
install.packages("mlbench")
install.packages("caret")
install.packages("e1071")
library(mlbench) 
library(caret)
library(e1071)

#load the dataset
mydata <- read.csv("database_caseStudy01.csv") 
View(mydata)

#check the balance of variables
skewness(mydata$LIMIT_BAL)
histogram(mydata$LIMIT_BAL)

#summarizes the data
summary(mydata)
str(mydata)

#calculates preprocessing parameters
preprocessParams <- preProcess(mydata, method=c("BoxCox"))
print(preprocessParams)

#transform the dataset using parameters
transformed <- predict(preprocessParams, mydata)
mydata <- transformed

#summarizes the data
str(mydata)
skewness(mydata$LIMIT_BAL)
histogram(mydata$LIMIT_BAL)

#transforms categorical variables
mydata$default.payment.next.month <- factor(mydata$default.payment.next.month)
mydata$SEX <- as.factor(mydata$SEX)
mydata$EDUCATION <-  as.factor(mydata$EDUCATION)
mydata$MARRIAGE <- as.factor(mydata$MARRIAGE)
mydata = na.omit(mydata)
summary(mydata)
str(mydata)

#splits data into training and testing
row <- nrow(mydata)
row 
set.seed(12345)
trainindex <- sample(row, 0.7*row, replace=FALSE)
training <- mydata[trainindex,]
validation <- mydata[-trainindex,]

trainingx<- training  
trainingy <- training[,24]            
validationx <- validation[,-24]
validationy <- validation[,24]

#apply Random Forest to find the most relevant variables
install.packages("randomForest")
library(randomForest)
rfModel = randomForest( training$default.payment.next.month ~ ., data=training, ntree=500 ) 
varImpPlot(rfModel) 


# Model Building with KNN

#prepares datasets with the best predictor variables
trainingx<- training[,-c(2,3,4,5,24)] 
trainingy <- training[,24]            
validationx <- validation[,-c(2,3,4,5,24)]  
validationy <- validation[,24]

# KNN Model
knnModel = train(x=trainingx, y=trainingy, method="knn",
                 preProc=c("center","scale"),
                 tuneLength=10)

knnModel

#accuracy plot
plot(knnModel$results$k, 
     knnModel$results$Accuracy, 
     type="o",
     xlab="Número de Vizinhos Mais Próximos (K)",
     ylab="Acurácia", 
     main="Modelo KNN Para Previsão de Concessão de Cartão de Crédito")


#making predictions 
knnPred = predict(knnModel, newdata=validationx)

knnPred
