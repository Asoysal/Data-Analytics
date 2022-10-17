library(class)
library(tree)

#1 Classification using trees and validation set approach
cars = read.csv("Carseats.csv",header=TRUE,sep=";")

# Data is splitted randomly on two groups, one for training the model and the other for testing it
set.seed(7)
cars.train = sample(c(TRUE,FALSE),nrow(cars),rep=TRUE)
cars.test = (!cars.train)

tree.cars = tree(High~., data = cars[cars.train,]) # The tree model is built using the training data
tree.pred = predict(tree.cars,cars[cars.test,],type="class") # The model is fed the test data to obtain the predictions

cars.t = table(cars[cars.test,]$High,tree.pred) # Confussion matrix comparing the predictions and test data
cars.error = (cars.t[1,2] + cars.t[2,1])/sum(cars.t) # Test error obtained from the confussion matrix
cars.error

#2 Classification using K-Nearest Neightbors and k-fold cross validation
qualitycontrol = read.csv("qualitycontrol.csv",header=TRUE,sep=";")
qualitycontrol$activate = 
  ifelse(qualitycontrol$activate=="Yes",1,0)
qualitycontrol$sensor = 
  ifelse(qualitycontrol$sensor=="success",1,0)

# Data is splitted randomly on 5 different groups
k=5
folds = sample(1:k,nrow(qualitycontrol),replace=TRUE)
errors = rep(0,k)

K=10
for(i in 1:k){
  train.X = qualitycontrol[folds!=i,-1] # k-1 folds are used as training data
  train.Y = qualitycontrol[folds!=i,1]
  test.X = qualitycontrol[folds==i,-1] # 1 fold is used as test data
  # knn allows to build the model with the training data and at the same time obtain the predictions for the test data
  knn.pred = knn(train.X,test.X,train.Y,K) 
  fold.t = table(qualitycontrol$activate[folds==i],knn.pred) # Confussion matrix
  errors[i] = (fold.t[1,2] + fold.t[2,1])/sum(fold.t) # Test error obtained from the confussion matrix
}
mean.errors = mean(errors) # Mean of the k test errors obtained for the k folds
mean.errors
