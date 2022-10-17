library(ggplot2)
library(ggcorrplot) # Correlation matrix
library(GGally) # Scatterplot matrix
library(car) # Collinearity
library(leaps) # Subset selection
library(class)
library(MASS) # LDA
library(tree)
library(randomForest)

og.efficiency = read.csv("buildings.csv", header = TRUE, sep = ",") # Original data
og.efficiency.Y1 = og.efficiency[,-10] # Data without Y2
og.efficiency.Y2 = og.efficiency[,-9] # Data without Y1

#==================================================================
#==================== 1. Preliminary Analysis =====================
#==================================================================

# Histrograms for the target variables
ggplot(og.efficiency.Y1, aes(Y1)) +
  geom_histogram(bins=20, fill="steelblue", col ="black") +
  labs(title="Histogram of the Heating Load")

ggplot(og.efficiency.Y2, aes(Y2)) +
  geom_histogram(bins=20, fill="steelblue", col ="black") +
  labs(title="Histogram of the Cooling Load")

# Density plots (not all them written)
ggplot(og.efficiency, aes(x=X1))+
  geom_density()

# Scatter plots (not all of them written)
ggplot(og.efficiency, aes(x = X1, y =Y1)) +
  geom_point(col="dodgerblue3",size=2) +
  theme_bw()

ggplot(og.efficiency, aes(x = X1, y =Y2)) +
  geom_point(col="dodgerblue3",size=2) +
  theme_bw()

# Correlogram
correlations <- round(cor(og.efficiency),1)
ggcorrplot(correlations, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram", 
           ggtheme=theme_bw)

#==================================================================
#================ 2. Linear Regression with VSA ===================
#==================================================================

set.seed(1)
regression.train = sample(c(TRUE,FALSE),nrow(og.efficiency.Y1),rep=TRUE)
regression.test = (!regression.train)

# Multiple linear regression model for Y1 and all predictors
mlr = lm(Y1~.,data = og.efficiency.Y1, subset = regression.train)
summary(mlr)
alias(mlr) # Detection of the multicollinearity for predictor X4

# Data modification to delete multicollinear predictor X4
efficiency = og.efficiency[,-4] # Data without variable X4
efficiency.Y1 = efficiency[,-9] # Data without X4 and Y2
efficiency.Y2 = efficiency[,-8] # Data without X4 and Y1

#==================================================================
#======== 2.1 Multiple linear Regression for Heating Load =========
#==================================================================

# Multiple linear regression model for Y1 without predictor X4
mlr = lm(Y1~.,data = efficiency.Y1, subset = regression.train)
summary(mlr)
mlr

# Hypothesis test
# Residual plot
res.plot.data = data.frame(predict(mlr),residuals(mlr))
ggplot(res.plot.data, aes(predict.mlr.,residuals.mlr.)) +
  geom_point(col="salmon",size=2) +
  geom_hline(yintercept = 0, linetype="dashed", color = "palegreen4", size=1.5) +
  labs (x = "Fitted values", y = "Residuals") +
  theme_bw()

# Correlated errors plot
ggplot(res.plot.data, aes(seq(1,nrow(res.plot.data),1),residuals.mlr.)) +
  geom_point(col="salmon",size=1) +
  geom_line(col="black",linetype = "dotted",size=0.5) +
  geom_hline(yintercept = 0, linetype="dashed", color = "palegreen4", size=1.5) +
  labs (x = "Fitted values", y = "Residuals") +
  theme_bw()

# VIF coefficient
vif(mlr)
mean(vif(mlr))

# Mean square error
predictions = predict.lm(mlr,efficiency.Y1[regression.test,])
mse = mean((efficiency.Y1$Y1[regression.test]-predictions)^2)
mse

# Best Subset Selection
bss = regsubsets(Y1~.,data=efficiency.Y1,nvmax=7)
bss.summary=summary(bss)
bss.summary

bss.summary$cp #CP
bss.summary$bic #BIC
bss.summary$adjr2 #Adjuster R^2

coef(bss,6)

fit.measures = data.frame(seq(1,7),bss.summary$cp,bss.summary$bic,bss.summary$adjr2)
colnames(fit.measures) = c("Numb.Pred","Cp","BIC","Adj.R2")

#Cp  
ggplot(fit.measures, aes(x = Numb.Pred, y = Cp)) +
  geom_line(col = "firebrick3",size = 1) +
  scale_x_continuous(breaks = seq(0,20)) +
  labs(y="Cp", x="Number of predictors") +
  theme_bw()

#BIC
ggplot(fit.measures, aes(x = Numb.Pred, y = BIC)) +
  geom_line(col = "firebrick3",size = 1) +
  scale_x_continuous(breaks = seq(0,20)) +
  labs(y="BIC", x="Number of predictors") +
  theme_bw()

#Adjusted R2
ggplot(fit.measures, aes(x = Numb.Pred, y = Adj.R2)) +
  geom_line(col = "firebrick3",size = 1) +
  scale_x_continuous(breaks = seq(0,20)) +
  labs(y="Adjusted R2", x="Number of predictors") +
  theme_bw()


#==================================================================
#======== 2.2 Multiple linear Regression for Cooling Load =========
#==================================================================

mlr = lm(Y2~.,data = efficiency.Y2, subset = regression.train)
summary(mlr)
mlr

# Hypothesis test
# Residual plot
res.plot.data = data.frame(predict(mlr),residuals(mlr))
ggplot(res.plot.data, aes(predict.mlr.,residuals.mlr.)) +
  geom_point(col="salmon",size=2) +
  geom_hline(yintercept = 0, linetype="dashed", color = "palegreen4", size=1.5) +
  labs (x = "Fitted values", y = "Residuals") +
  theme_bw()

# Correlated errors plot
ggplot(res.plot.data, aes(seq(1,nrow(res.plot.data),1),residuals.mlr.)) +
  geom_point(col="salmon",size=1) +
  geom_line(col="black",linetype = "dotted",size=0.5) +
  geom_hline(yintercept = 0, linetype="dashed", color = "palegreen4", size=1.5) +
  labs (x = "Fitted values", y = "Residuals") +
  theme_bw()

# VIF coefficient
vif(mlr)
mean(vif(mlr))

# Mean square error
predictions = predict.lm(mlr,efficiency.Y2[regression.test,])
mse = mean((efficiency.Y2$Y2[regression.test]-predictions)^2)
mse

# Best Subset Selection
bss = regsubsets(Y2~.,data=efficiency.Y2,nvmax=7)
bss.summary=summary(bss)
bss.summary

bss.summary$cp #CP
bss.summary$bic #BIC
bss.summary$adjr2 #Adjuster R^2

coef(bss,5)

fit.measures = data.frame(seq(1,7),bss.summary$cp,bss.summary$bic,bss.summary$adjr2)
colnames(fit.measures) = c("Numb.Pred","Cp","BIC","Adj.R2")

#Cp  
ggplot(fit.measures, aes(x = Numb.Pred, y = Cp)) +
  geom_line(col = "firebrick3",size = 1) +
  scale_x_continuous(breaks = seq(0,20)) +
  labs(y="Cp", x="Number of predictors") +
  theme_bw()

#BIC
ggplot(fit.measures, aes(x = Numb.Pred, y = BIC)) +
  geom_line(col = "firebrick3",size = 1) +
  scale_x_continuous(breaks = seq(0,20)) +
  labs(y="BIC", x="Number of predictors") +
  theme_bw()

#Adjusted R2
ggplot(fit.measures, aes(x = Numb.Pred, y = Adj.R2)) +
  geom_line(col = "firebrick3",size = 1) +
  scale_x_continuous(breaks = seq(0,20)) +
  labs(y="Adjusted R2", x="Number of predictors") +
  theme_bw()

#==================================================================
#======================= 3. Classification ========================
#==================================================================

#==================================================================
#=========================== 3.1 LDA ==============================
#==================================================================

engy_lda = efficiency.Y2
engy_lda$level = NA

for(i in 1:nrow(engy_lda)){
  if(engy_lda$Y2[i] > 35.5) {engy_lda$level[i] <- "high"}
  else if(engy_lda$Y2[i] < 23.2) {engy_lda$level[i] <- "low"}
  else {engy_lda$level[i] <- "medium"}
}

engy_lda["Y2"] = NULL

# Pie chart
ggplot(engy_lda, aes(x = "", fill = factor(level))) + 
  geom_bar(width = 1, col = "black") +
  theme(axis.text.x=element_blank()) +
  scale_fill_brewer(palette="Dark2") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Cooling Load", 
       x=NULL, 
       y=NULL, 
       title="Cooling Load Level", 
       caption="Source: https://www.kaggle.com/elikplim/eergy-efficiency-dataset") +
  coord_polar(theta = "y", start=0)

lda.fit = lda(level~., data=engy_lda)
lda.fit

lda.pred = predict(lda.fit, data=engy_lda)
lda.class = lda.pred$class
fold.t = table(engy_lda$level,lda.class)
fold.t

test.error = (fold.t[1,2] + fold.t[1,3] + fold.t[2,1] + fold.t[2,3] + fold.t[3,1] + fold.t[3,2])/sum(fold.t)
test.error

#==================================================================
#========== 3.2 KNN (without k-fold cross validation) =============
#==================================================================

engy_knn = efficiency.Y2
engy_knn$load_bin = NA

engy_knn$load_bin = 
  ifelse(engy_knn$Y2 > 29.5 ,1,0)

engy_knn["Y2"] = NULL


ggplot(engy_knn, aes(x = "", fill = factor(load_bin))) + 
  geom_bar(width = 1, col = "black") +
  theme(axis.text.x=element_blank()) +
  scale_fill_brewer(palette="Dark2", labels = c("low", "high")) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Cooling Load", 
       x=NULL, 
       y=NULL, 
       title="Cooling Load Level binary", 
       caption="Source: https://www.kaggle.com/elikplim/eergy-efficiency-dataset") +
  coord_polar(theta = "y", start=0)

data.knn.X = engy_knn[,-8]

#matrix containing the class labels for the training data
data.knn.Y = engy_knn[,8]

#number of nearest neighbors to be used
K = 10

knn.pred.basic = knn(data.knn.X,data.knn.X,data.knn.Y,K)

#Confusion matrix
fold.t.knn =table(engy_knn$load_bin,knn.pred.basic)  
knn.error = (fold.t.knn[1,2] + fold.t.knn[2,1])/sum(fold.t.knn)
knn.error

#==================================================================
#============ 3.3 KNN (with k-fold cross validation) ==============
#==================================================================

#Training/Testing error using the k-fold cross-validation
#Random training and testing sample

j=8 #number of folds
set.seed(1)

folds = sample(1:j,nrow(engy_knn),replace=TRUE) #spliting of the sample
cv.error = rep(0,j)

i=1

for(i in 1:j){
  
  knn.pred = knn(data.knn.X[folds!=i,],data.knn.X[folds==i,],data.knn.Y[folds!=i],K)
  
  fold.t =table(engy_knn$load_bin[folds==i],knn.pred)  
  cv.error[i] = (fold.t[1,2] + fold.t[2,1])/sum(fold.t)
}

mean.cv.error = mean(cv.error)
cv.error
mean.cv.error

#==================================================================
#============== 3.4 Multicalss classifcation Tree =================
#==================================================================

engy_tree = efficiency.Y2
engy_tree$level = NA

for(i in 1:nrow(engy_tree)){
  if(engy_tree$Y2[i] > 35.5) {engy_tree$level[i] <- "high"}
  else if(engy_tree$Y2[i] < 23.2) {engy_tree$level[i] <- "low"}
  else {engy_tree$level[i] <- "medium"}
}

engy_tree["Y2"] = NULL

engy_tree$level = as.factor(engy_tree$level)
tree.energy = tree(level~., data = engy_tree, method="class") 
summary(tree.energy)
tree.energy

#plot the tree
plot(tree.energy)
text(tree.energy,pretty=0,pos=4)

#make predictions in the training set
tree.pred = predict(tree.energy,type="class")


#training confusion matrix
tree.table = table(engy_tree$level,tree.pred) 
tree.table

tree.error = (tree.table[1,2] + tree.table[1,3] + tree.table[2,1] + tree.table[2,3] + tree.table[3,1] + tree.table[3,2])/sum(tree.table)
tree.error

#==================================================================
#================ 3.5 Binary classifcation Tree ===================
#==================================================================

engy_bintree = efficiency.Y2
engy_bintree$load_bin = NA

engy_bintree$load_bin = 
  ifelse(engy_bintree$Y2 > 29.5 ,"High","Low")

engy_bintree["Y2"] = NULL

engy_bintree$load_bin = as.factor(engy_bintree$load_bin)
tree.energy.bin = tree(load_bin~., data = engy_bintree) 
summary(tree.energy.bin)
tree.energy.bin

#plot the tree
plot(tree.energy.bin)
text(tree.energy.bin,pretty=0,pos=4)


#make predictions in the training set
tree.pred = predict(tree.energy.bin,type="class")

#training confusion matrix
bintree.table = table(engy_bintree$load_bin,tree.pred) 
bintree.table
bintree.error = (bintree.table[1,2] + bintree.table[2,1])/sum(bintree.table)
bintree.error

#==================================================================
#====================== 3.5 Random Forest =========================
#==================================================================

engy_forest = efficiency.Y2
engy_forest$level = NA

for(i in 1:nrow(engy_forest)){
  if(engy_forest$Y2[i] > 35.5) {engy_forest$level[i] <- "high"}
  else if(engy_forest$Y2[i] < 23.2) {engy_forest$level[i] <- "low"}
  else {engy_forest$level[i] <- "medium"}
}

engy_forest["Y2"] = NULL

engy_forest$level = as.factor(engy_forest$level)

forest = randomForest(level~., data = engy_forest, 
                      mtry=3, importance =TRUE)
forest

#make predictions using forest in the training set
forest.pred = predict(forest,type="class")

#training confusion matrix
table(engy_forest$level,forest.pred)

#importance of variables
varImpPlot(forest)

#==================================================================
#================ 4. Principal Components Analysis ================
#==================================================================

efficiency.PCA = og.efficiency[,-10]
efficiency.PCA = efficiency.PCA[,-9]
PCA = princomp(efficiency.PCA,cor=TRUE)
summary(PCA)
PCA$loadings
PCA$scores
