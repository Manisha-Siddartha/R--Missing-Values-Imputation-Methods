#Importing Libraries
library('tibble')     # data wrangling
library('data.table') # data manipulation
library(mice) 
library(Amelia)
library('class')


iris_data <- as.tibble(fread("C:/Users/Manisha/Documents/Machine learning -R/Iris.csv")) #read Data
toConvert <- c("SepalLengthCm","SepalWidthCm","PetalLengthCm","PetalWidthCm")#Columns to create missing values
as.data.frame(sapply(iris_data[toConvert],as.numeric))#converting data to numeric

iris_data_mean <- iris_data   #create copy of original data
iris_data_amelia <- iris_data #create copy of original data
iris_data_mice <-iris_data    #create copy of original data
iris_NAR <- iris_data         #create copy of original data

#percentage of data for random imputation
x <- c(2, 5, 10 , 15, 20, 25)

#Data frames to store Comparision values for RMSE and KNN
compare_RMSE <- data.frame(meanMethod = numeric(6), ameliaMethod = numeric(6), miceMethod = numeric(6))
compare_KNN <- data.frame(sentosa = numeric(18), versicolor = numeric(18), virginica = numeric(18))

#retrieves class labels
Species_cst <- factor(iris_data$Species, levels = c("Iris-setosa", "Iris-versicolor","Iris-virginica"), labels = c("setosa", "versicolor","virginica"))
#percentage of all classes in the original dataset
original_CLpercentages <- round(prop.table(table(iris_data$Species_cst)) * 100, digits = 1)

#Normalize function
normalize <- function(x) 
  return ((x - min(x)) / (max(x) - min(x))) 
normalized_originaldataset <- as.data.frame(lapply(iris_data[2:5], normalize))#normalizing original dataset

#root mean square error
rmse <- function(error)
  sqrt(mean(error^2))


for(k in 1:length(x)){
  
  NumMissValues <- (nrow(iris_data)*ncol(iris_data)*x[k])/100    #Number of values to set NA
  inds <- as.matrix(expand.grid(1:nrow(iris_data), 2:(ncol(iris_data)-1))) #convert into a 2 column matrix.First column contains values from data frame. Second column contains column numbers.
  selected <- inds[sample(nrow(inds), NumMissValues),]    # Sample randomly
  
  #Original values before imputation
  original <- c()
  for(i in 1:NumMissValues)
    original[i] <- unlist(iris_data[selected[i,1],selected[i,2]])
  original
 
  
  #creating random missing values
  iris_data_mean[selected] <- NA     # Selected` is a matrix of (row, col) indices
  
  #replace the missing values with calculated mean
  means <- sapply(iris_data_mean[toConvert],function(y) mean(y,na.rm=TRUE, round(1)))
  for(i in 2:(ncol(iris_data_mean)-1))
    iris_data_mean[is.na(iris_data_mean[,i]), i] <- means[i-1]
  
  #imputed values
  Imputed <- c()
  for(i in 1:NumMissValues)
    Imputed[i] <- unlist(iris_data_mean[selected[i,1],selected[i,2]])
  Imputed
  
  #calculating RMSE error for mean method
  compare_RMSE[k,1] <-  rmse(abs(original - Imputed))
  
  #KNN classification for mean method imputation
  normailzed_meanMethod <- as.data.frame(lapply(iris_data_mean[2:5], normalize))
  iris_test_pred_mean <- knn(train = normalized_originaldataset , test = normailzed_meanMethod ,cl = Species_cst, k=30)
  mean_CLpercentages <- round(prop.table(table(iris_test_pred_mean)) * 100, digits = 1)
  compare_KNN[3*k-2,] <- mean_CLpercentages
  
  #creating random missing values
  iris_data_amelia[selected] <- NA
  #Using amelia to impute values
  irisdata2 <- amelia(iris_data_amelia[,toConvert])
  iris_data_amelia<-iris_data2[,1:6]
  
  #Imputed values
  Imputed <- c()
  for(i in 1:NumMissValues)
    Imputed[i] <- unlist(iris_data_amelia[selected[i,1],selected[i,2]])
  Imputed
  
  #RMSE error for amelia
  compare_RMSE[k,2] <-  rmse(abs(original - Imputed))
  
  #KNN classification
  normailzed_ameliaMethod <- as.data.frame(lapply(iris_data_amelia[2:5], normalize))
  iris_test_pred_amelia <- knn(train = normalized_originaldataset , test = normailzed_ameliaMethod ,cl = Species_cst, k=30)
  amelia_CLpercentages <- round(prop.table(table(iris_test_pred_amelia)) * 100, digits = 1)
  compare_KNN[3*k-1,] <- amelia_CLpercentages
  
  
  #create random missing values to apply mice imputation method
  
  iris_data_mice[selected] <- NA
  tempData <-  mice(iris_data_mice, m = 5,maxit=20,meth='pmm',seed=500)
  iris_data_mice <- complete(tempData,1)
  
  #Imputed values using mice
  Imputed <- c()
  for(i in 1:NumMissValues)
    Imputed[i] <- unlist(iris_data_mice[selected[i,1],selected[i,2]])
  Imputed
  
  #RMSE error calculation for mice method
  compare_RMSE[k,3] <-  rmse(abs(original - Imputed))
  
  #KNN classification for mice method
  normailzed_miceMethod <- as.data.frame(lapply(iris_data_mice[2:5], normalize))
  iris_test_pred_mice <- knn(train = normalized_originaldataset , test = normailzed_miceMethod ,cl = Species_cst, k=30)
  mice_CLpercentages <- round(prop.table(table(iris_test_pred_mice)) * 100, digits = 1)
  compare_KNN[3*k,] <- mice_CLpercentages
  
  
}

#RMSE Comparision between three imputation methods for 6 different percentages of x
counts <- data.matrix(compare_RMSE, rownames.force = NA)
barplot(counts, main=" RMSE Comparision between three imputation methods for 6 different percentages of x",
         ylab="RMSE", col=c("darkblue","red", "green","pink","yellow","brown"),
        legend = x, beside=TRUE)


#KNN comparision of  predicted class labels percentages for 2% imputation of missing values using 3 different meth0ds
counts <- data.matrix(compare_KNN[1:3,], rownames.force = NA)
barplot(counts, main=" KNN comparision of  predicted class labels percentages for 2% imputation",
        ylab="KNN", col=c("darkblue","red", "green"),
        legend = c('mean','amelia','mice'), beside=TRUE)

#KNN comparision of  predicted class labels percentages for 5% imputation of missing values using 3 different meth0ds
counts <- data.matrix(compare_KNN[4:6,], rownames.force = NA)
barplot(counts, main=" KNN comparision of  predicted class labels percentages for 5% imputation",
        ylab="KNN", col=c("darkblue","red", "green"),
        legend = c('mean','amelia','mice'), beside=TRUE)

#KNN comparision of  predicted class labels percentages for 10% imputation of missing values using 3 different meth0ds
counts <- data.matrix(compare_KNN[7:9,], rownames.force = NA)
barplot(counts, main=" KNN comparision of  predicted class labels percentages for 10% imputation",
        ylab="KNN", col=c("darkblue","red", "green"),
        legend = c('mean','amelia','mice'), beside=TRUE)

#KNN comparision of  predicted class labels percentages for 15% imputation of missing values using 3 different meth0ds
counts <- data.matrix(compare_KNN[10:12,], rownames.force = NA)
barplot(counts, main=" KNN comparision of  predicted class labels percentages for 15% imputation",
        ylab="KNN", col=c("darkblue","red", "green"),
        legend = c('mean','amelia','mice'), beside=TRUE)

#KNN comparision of  predicted class labels percentages for 20% imputation of missing values using 3 different meth0ds
counts <- data.matrix(compare_KNN[13:15,], rownames.force = NA)
barplot(counts, main=" KNN comparision of  predicted class labels percentages for 20% imputation",
        ylab="KNN", col=c("darkblue","red", "green"),
        legend = c('mean','amelia','mice'), beside=TRUE)

#KNN comparision of  predicted class labels percentages for 25% imputation of missing values using 3 different meth0ds
counts <- data.matrix(compare_KNN[16:18,], rownames.force = NA)
barplot(counts, main=" KNN comparision of  predicted class labels percentages for 25% imputation",
        ylab="KNN", col=c("darkblue","red", "green"),
        legend = c('mean','amelia','mice'), beside=TRUE)

#missing values not at random

irisdata_notatRandom_mean <- iris_NAR
irisdata_notatRandom_amelia <- iris_NAR
irisdata_notatRandom_mice <- iris_NAR

#comparing RMSE and KNN
compare_NARRMSE <- data.frame(meanMethod = numeric(1), ameliaMethod = numeric(1), miceMethod = numeric(1))
compare_NARKNN <- data.frame(sentosa = numeric(3), versicolor = numeric(3), virginica = numeric(3))

original <- irisdata_notatRandom_mean$SepalLengthCm[irisdata_notatRandom_mean$SepalLengthCm == 5.1]
irisdata_notatRandom_mean$SepalLengthCm[irisdata_notatRandom_mean$SepalLengthCm == 5.1] <- NA
missingvalues <- irisdata_notatRandom_mean[is.na(irisdata_notatRandom_mean$SepalLengthCm),1]

means <- sapply(irisdata_notatRandom_mean[,2],function(y) mean(y,na.rm=TRUE, round(1)))
irisdata_notatRandom_mean[is.na(irisdata_notatRandom_mean[,2]), 2] <- means
Imputed <- c()
Imputed <- unlist(irisdata_notatRandom_mean[unlist(missingvalues),2])

compare_NARRMSE$meanMethod <- rmse(abs(Imputed-original))
normailzed_NARmeanMethod <- as.data.frame(lapply(irisdata_notatRandom_mean[2:5], normalize))

iris_test_pred_NARmean <- knn(train = normalized_originaldataset , test = normailzed_NARmeanMethod ,cl = Species_cst, k=30)
mean_CLpercentages <- round(prop.table(table(iris_test_pred_NARmean)) * 100, digits = 1)
compare_NARKNN[1,] <- mean_CLpercentages

irisdata_notatRandom_amelia$SepalLengthCm[irisdata_notatRandom_amelia$SepalLengthCm == 5.1] <- NA
irisdata2 <- amelia(irisdata_notatRandom_amelia[,2])
irisdata_notatRandom_amelia<-iris_data2[,1:6]

Imputed <- c()
Imputed <- unlist(irisdata_notatRandom_amelia[unlist(missingvalues),2])
Imputed

compare_NARRMSE$ameliaMethod <-  rmse(abs(original - Imputed))
normailzed_NARameliaMethod <- as.data.frame(lapply(irisdata_notatRandom_amelia[2:5], normalize))
iris_test_pred_NARamelia <- knn(train = normalized_originaldataset , test = normailzed_NARameliaMethod ,cl = Species_cst, k=30)
amelia_CLpercentages <- round(prop.table(table(iris_test_pred_NARamelia)) * 100, digits = 1)
compare_NARKNN[2,] <- amelia_CLpercentages

irisdata_notatRandom_mice$SepalLengthCm[irisdata_notatRandom_mice$SepalLengthCm == 5.1] <- NA
tempData <-  mice(irisdata_notatRandom_mice, m = 5,maxit=50,meth='pmm',seed=500)
irisdata_notatRandom_mice <- complete(tempData,1)

Imputed <- c()
Imputed <- unlist(irisdata_notatRandom_mice[unlist(missingvalues),2])
Imputed

compare_NARRMSE$miceMethod<-  rmse(abs(original - Imputed))
normailzed_NARmiceMethod <- as.data.frame(lapply(irisdata_notatRandom_mice[2:5], normalize))
iris_test_pred_mice <- knn(train = normalized_originaldataset , test = normailzed_NARmiceMethod ,cl = Species_cst, k=30)
mice_CLpercentages <- round(prop.table(table(iris_test_pred_mice)) * 100, digits = 1)
compare_NARKNN[3,] <- mice_CLpercentages


counts <- data.matrix(compare_NARRMSE,rownames.force = NA)
barplot(counts, main="Not at random missing values RMSE Comparision", 
        xlab="three different imputation methods")

counts <- data.matrix(compare_NARKNN, rownames.force = NA)
barplot(counts, main=" Comparision of different class percentages after KNN prediction",
        ylab="Percentage", col=c("darkblue","red", "green"),
        legend = c("mean","amelia","mice"), beside=TRUE)
