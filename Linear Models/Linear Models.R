library(ggplot2)
library(ggcorrplot)
library(leaps)

# Load Data
ozone <- read.csv("LAozone.csv",header=TRUE,sep=";")

#delete has missing values or not.
ozone <- na.omit(ozone)

# Acording to histogram, The number of cold days is higher than the number of hot days.
ggplot(ozone,aes(ozone)) +
  geom_histogram(bins=10, fill="steelblue", col ="black") +
  labs(title="Histogram", 
       subtitle="Cw",
       caption="Source: ozone",
       x="Cw")

ggplot(ozone, aes(ozone)) +
  geom_histogram(aes(fill=factor(cw)), bins=10, col ="blue") +
  labs(title="Histogram", 
       subtitle="cw",
       caption="Source: ozone",
       x="cw",
       fill="# cw")

# the pairwise correlations between the quantitative variables in this dataset temp and ibt are higly corelated , ibh and ibt are inverse correlated in the correlogram of zone.

#To modify c/w in activate by 1/0
ozone$cw = 
  
  ifelse(ozone$cw=="W",1,0)

correlations <- round(cor(ozone),1)
ggcorrplot(correlations, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of ozone",
           ggtheme=theme_bw)


#5)fitting  Fit the simple linear regression model in which the response variable is ozone and the
#predictor is temperature. R^2= 0.6083 if it is close to 1, the model mostly is fitted.

#simple linear regression
slr = lm(ozone~temp,data=ozone)
slr

confint(slr,level = 0.95)

#Hypothesis tests & R^2
summary(slr)



# Making a scatter plot containing the observations for ozone and temperature and include
#the fitted regression line.
#scatter plot
ggplot(ozone, aes(x = temp, y =ozone)) +
  geom_point(col="dodgerblue3",size=2) +
  theme_bw()
# regression line 
ggplot(ozone, aes(x = temp, y =ozone)) +
  geom_point(col="dodgerblue3",size=2) +
  geom_smooth(method="lm", se =FALSE, col = "firebrick3", size=1.5) +
  theme_bw()



#Fitting Fit the multiple linear regression model in which all the variables are used to predict
#ozone R^2 = 0.706 if it is close to 1, the model mostly is fitted.

#Multiple Linear Regression model

mlr <- lm(ozone~.,data=ozone)
mlr

confint(mlr,level = 0.95)

#Hypothesis tests & R^2
summary(mlr)

#the best linear models    
# out.ozone = 22.30-0.005 vh +0.076 wind + 0.073 humidity+0.66 temp - 0.0005 ibg + 0.02 ibt- 0.006 vis - 3.319 CWW

bss = regsubsets(ozone~.,data=ozone,nvmax=9)
#Save the summary
bss.summary=summary(bss) # the summary of the model.
names(bss.summary) 
bss.summary$rsq #R^2 
bss.summary$rss #RSS
bss.summary$cp #CP 
bss.summary$bic #BIC
bss.summary$adjr2 #Adjuster R^2 

coef(bss,8)

#Fitting the lasso model for 50 values of the penalty parameter λ within the range (105, 10−5)
#32.56-0.0068 vh + 0.06 wind + 0.070 humidity + 0.49 temp - o.0005 ibh + 0.0014 dpg +0.025 ibt -0.0065 vis

library(glmnet)

#To modify c/w in activate by 1/0
ozone$cw = 

 ifelse(ozone$cw=="W",1,0)


x=model.matrix(ozone~.,ozone)[,-1]
y=ozone$ozone
grid = 10^seq(5,-5,length=50)

lasso.mod = glmnet(x,y,alpha=1,lambda=grid)
dim(coef(lasso.mod))
lasso.mod$lambda[50]
coef(lasso.mod)[,50]
summary(lasso.mod)




