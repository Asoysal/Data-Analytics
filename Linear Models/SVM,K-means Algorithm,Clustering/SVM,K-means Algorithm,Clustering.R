library(ISLR)
library(e1071)
library(datasets)

data("OJ", package = "ISLR")

#create data
set.seed(10)
train = sample(1:1070, 800)
OJ.train = OJ[train,]
OJ.test = OJ[-train,]

#Fit a support vector classifier to the training data using cost=0.01, with Purchase as
#the response and the other variables as predictors
svmfit = svm (Purchase~., data=OJ.train, kernel ="linear",cost =0.01)
summary(svmfit)
# For a cost of 0.01 we obtain 451 suport vectors, 226 for the CH class
# and 225 for MM

#the training and test error rates
ypred.train = predict(svmfit, OJ.train)
(table.train = table(OJ.train$Purchase,ypred.train))
(train.error = (table.train[1,2] + table.train[2,1])/sum(table.train))

ypred.test = predict(svmfit, OJ.test)
(table.test = table(OJ.test$Purchase,ypred.test))
(test.error = (table.test[1,2] + table.test[2,1])/sum(table.test))
# We obtain a train error of 0.18 and a test error of 0.156

# The optimal cost is 0.86
tune.out = tune(svm,Purchase~., data=OJ.train, kernel ="linear",
                ranges = list(cost = seq(0.01,1,0.05)))

bestmod = tune.out$best.model
summary(bestmod)


# We obtain a train error of 0.169 and a test error of 0.141
svmfit = svm (Purchase~., data=OJ.train, kernel ="linear",cost =0.86)

ypred.train = predict(svmfit, OJ.train)
(table.train = table(OJ.train$Purchase,ypred.train))
(train.error = (table.train[1,2] + table.train[2,1])/sum(table.train))

ypred.test = predict(svmfit, OJ.test)
(table.test = table(OJ.test$Purchase,ypred.test))
(test.error = (table.test[1,2] + table.test[2,1])/sum(table.test))


#a support vector machine with a radial kernel.  the default value for gamma.

svmfit.rad = svm (Purchase~., data=OJ.train, kernel ="radial",cost = 0.01)
summary(svmfit.rad)
# For a cost of 0.01 and radial kernel we obtain 622 suport vectors
# 313 for the class CH and 309 for the class MM

#
ypred.train = predict(svmfit.rad, OJ.train)
(table.train = table(OJ.train$Purchase,ypred.train))
(train.error = (table.train[1,2] + table.train[2,1])/sum(table.train))

ypred.test = predict(svmfit.rad, OJ.test)
(table.test = table(OJ.test$Purchase,ypred.test))
(test.error = (table.test[1,2] + table.test[2,1])/sum(table.test))
# We obtain a train error of 0.386 and a test error of 0.4

#
tune.out = tune(svm,Purchase~., data=OJ.train, kernel ="radial",
                ranges = list(cost = seq(0.01,1,0.05)))

bestmod = tune.out$best.model
summary(bestmod)
# The optimal cost is 0.41
#
svmfit.rad = svm (Purchase~., data=OJ.train, kernel ="radial",cost =0.41)

ypred.train = predict(svmfit.rad, OJ.train)
(table.train = table(OJ.train$Purchase,ypred.train))
(train.error = (table.train[1,2] + table.train[2,1])/sum(table.train))

ypred.test = predict(svmfit.rad, OJ.test)
(table.test = table(OJ.test$Purchase,ypred.test))
(test.error = (table.test[1,2] + table.test[2,1])/sum(table.test))
# We obtain a train error of 0.165 and a test error of 0.148

#a support vector machine with a polynomial kernel. Set degree = 2.
svmfit.pol = svm (Purchase~., data=OJ.train, kernel ="polynomial",cost = 0.01, degree =2)
summary(svmfit.pol)
# For a cost of 0.01 and polynomial kernel we obtain 623 suport vectors
# 314 for the class CH and 309 for the class MM

#
ypred.train = predict(svmfit.pol, OJ.train)
(table.train = table(OJ.train$Purchase,ypred.train))
(train.error = (table.train[1,2] + table.train[2,1])/sum(table.train))

ypred.test = predict(svmfit.pol, OJ.test)
(table.test = table(OJ.test$Purchase,ypred.test))
(test.error = (table.test[1,2] + table.test[2,1])/sum(table.test))
# We obtain a train error of 0.386 and a test error of 0.4

#
tune.out = tune(svm,Purchase~., data=OJ.train, kernel ="polynomial",
                ranges = list(cost = seq(0.01,1,0.05)))

bestmod = tune.out$best.model
summary(bestmod)
# The optimal cost is 0.96 and a degree of 3

#
svmfit.pol = svm (Purchase~., data=OJ.train, kernel ="polynomial",cost =0.96, degree = 3)

ypred.train = predict(svmfit.pol, OJ.train)
(table.train = table(OJ.train$Purchase,ypred.train))
(train.error = (table.train[1,2] + table.train[2,1])/sum(table.train))

ypred.test = predict(svmfit.pol, OJ.test)
(table.test = table(OJ.test$Purchase,ypred.test))
(test.error = (table.test[1,2] + table.test[2,1])/sum(table.test))
# We obtain a train error of 0.176 and a test error of 0.178



# Overall the best result is obtained for the linear kernel and a cost of 0.86

#================================

data("USArrests", package = "datasets")

#the dendrogram.
hc=hclust(dist(USArrests,method = "euclidean"),method="complete")
plot(hc)

#results in three distinct clusters.
abline(a=150,b=0,col="red")

#the list of states which belong to each cluster
hier.output=cutree(hc, h=150)
hier.output

#the K-means algorithm
set.seed(5)
kmeans.output=kmeans(USArrests,3)
kmeans.output

#the K-means algorithm
table(hier.output,kmeans.output$cluster)


#n the scores of the first Principal Component of USArrest
PCA = princomp(USArrests,cor=TRUE)
summary(PCA)
PCA$scores
PCA$loadings

#
PCA_USArrests = USArrests
PCA_USArrests$Z1 = NA
PCA_USArrests$Z1 = 0.536*PCA_USArrests$Murder + 0.583*PCA_USArrests$Assault + 0.278*PCA_USArrests$UrbanPop + 0.543*PCA_USArrests$Rape
PCA_USArrests = PCA_USArrests$Z1

#
PCA_hc=hclust(dist(PCA_USArrests,method = "euclidean"),method="complete")
plot(PCA_hc)

#
abline(a=100,b=0,col="red")

#
PCA.hier.output=cutree(PCA_hc, h=100)
PCA.hier.output

#
set.seed(5)
PCA.kmeans.output=kmeans(PCA_USArrests,3)
PCA.kmeans.output

table(PCA.hier.output,hier.output)
table(PCA.kmeans.output$cluster,kmeans.output$cluster)
# We obtain the same clusters when using the K-means algorithm, but there are some differences
# with the hierarchical clustering
