library(foreign)
library(C50)
library(ggplot2)
library(caret)
setwd("/Birzeit/Artificial intelligence/Project2/Data/")
#read File 
Odata <- read.spss("SefSec_2014_HH_weight new.sav",to.data.frame = TRUE)
View(dataset)
#create my dataset
dataset <- Odata[,c(108,94,93,92,81,76,75,438,428,422,418,410,162,386,685,680,684,608,607,606,603,600,656)]
dataset <- na.omit(dataset)
#shufle data

#set.seed(15)
#shuffledData <- dataset[order(runif(nrow(dataset))), ]
#trainingdb <- shuffledData[1:floor(nrow(shuffledData)*0.3), ]
#View(dataset)

#testingdb <- shuffledData[(floor(nrow(shuffledData)*0.7)+1) :nrow(shuffledData), ]
#using c50
controler <- C5.0Control(seed = 10, sample = 0.7)
#model
View(trainingdb)

model <- C5.0(dataset[ , -23], dataset[ , 23],control = controler , trials = 10)
#model <- C5.0(dataset[ , -24], dataset[ , 24],trials = 10)
#trials 10 drops 25% of error which is optimal 
summary(model)
plot(model)

confusionMatrix(dataset2[,23],dataset2[,24] )

result <- predict(model, dataset[,-24])
dataset2 <- (cbind(dataset, result))


temp <- result == dataset$I04_1
print(temp)
accuracy <- (length(which(temp)) / length(temp))*100.0
sprintf("The accuracy = %.2f",accuracy)
