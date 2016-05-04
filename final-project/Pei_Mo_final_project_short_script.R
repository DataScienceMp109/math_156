#Final Project
#Class name: Spring 2016, MATH 156, Harvard University 
#Student Name: Mo Pei

install.packages("dplyr")
install.packages("caret")
install.packages("ordinal")
install.packages("e1071")
install.packages("ROCR")
install.packages("TeachingDemos")
install.packages("visreg")
install.packages("influence.ME")
install.packages("mosaic")
install.packages("mosaicData")
install.packages("Lock5Data")
install.packages("GGally")

library(GGally)
library(dplyr)
library(caret)
library(ordinal)
library(e1071)
library(ROCR)
library(TeachingDemos)
library(splines)
require(visreg)
library(lme4)
library(mosaic)
library(mosaicData)
library(Lock5Data)

# Salaries for Professors
# Description
# 
# The 2008-09 nine-month academic salary for Assistant Professors, 
# Associate Professors and Professors in a college in the U.S. 
# The data were collected as part of the on-going 
# effort of the college's administration to monitor 
# salary differences between male and female faculty members.




Salaries <- read.csv("C:/Users/peimo/Desktop/MATH 156/Final_Project/Data/Salaries.csv")

#(1) Two sample T-test & T confidence interval

#(a) Two sample T-test 
# salaryy mean difference between  of theoretical professors
# and "applied" departments professors

PS_t<-subset(Salaries,select=salary,subset=(discipline=='A'&rank=='Prof'))
PS_a<-subset(Salaries,select=salary,subset=(discipline=='B'&rank=='Prof'))

nrow(PS_t)
nrow(PS_a)


# randomly choose 100 samples each of two groups
choose_range_t<-c(1:nrow(PS_t))
sample_index<-sample(choose_range_t,40)
sample_PS_t<-PS_t[sample_index,]

choose_range_a<-c(1:nrow(PS_a))
sample_index<-sample(choose_range_a,40)
sample_PS_a<-PS_a[sample_index,]

sample_salaries<-data.frame(sample_PS_t,sample_PS_a)
colnames(sample_salaries) <- c("t","a")  



#Two sample T-Test to test if mean of theoretical professors
#is different from B "applied" departments professors.

#Assumtions verification
# The populations from which the samples have been drawn should be normal
qqnorm(sample_salaries$t)
qqline(sample_salaries$t)

qqnorm(sample_salaries$a)
qqline(sample_salaries$a)
# The variance of the two groups should be similar
hist(sample_salaries$t, probability = TRUE)
curve(dnorm(x, mean=mean(sample_salaries$t),sd=sd(sample_salaries$t))
      , col = "red", add= TRUE)
hist(sample_salaries$a, probability = TRUE)
curve(dnorm(x, mean=mean(sample_salaries$a),sd=sd(sample_salaries$a))
      , col = "red", add= TRUE)
# Each professors salary is independent to each other 

# Samples have to be randomly selected. 

t.test(sample_salaries$t,sample_salaries$a,alt="greater")



#Conclusion: with df = 77.994, p-value = 0.9946
# so there is no significant evidence support there is a 
# salaryy mean difference between  of theoretical professors
# and "applied" departments professors


#(b) T confidence interval
# 95% confidence interval of mean of salary of male full professor 

# I will build t confidence interval to estimate the mean of male professors

PS_m<-subset(Salaries,select=salary,subset=(sex=='Male'&rank=='Prof'))

#Assumtions verification
# Samples have to be randomly drawn independent of each other.
# I assume the salary of different professors is independent of each other

# Randomization Condition 
# The data must be sampled randomly. 

# The populations from which the samples have been drawn should be normal
qqnorm(PS_m$salary)
qqline(PS_m$salary)

# The standard deviation of the populations should be equal 
hist(PS_m$salary, probability = TRUE)
curve(dnorm(x, mean=mean(PS_m$salary),sd=sd(PS_m$salary))
      , col = "red", add= TRUE)


mean(PS_m$salary)
nrow(PS_m)

plot(x =c(110000,145000), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot

mean(PS_m$salary); var(PS_m$salary)
#Use Student's t(theoritical) to get a confidence interval
mean(PS_m$salary) + qt(0.025, 247) * sd(PS_m$salary)/sqrt(248); mean(PS_m$salary) + qt(0.975, 247) * sd(PS_m$salary)/sqrt(248)
#Run the automated t test
t.test(PS_m$salary, conf.level = .95)    

#We are 95% percent sure that
#the true mean of mean of salary of male full professor 
#is between 123592.and 1 130649.5 

#simulation t confidence interval


means <- numeric(1000);lower = numeric(1000); upper = numeric(1000)
for (i in 1:1000) {
  choose_range_t<-c(1:nrow(PS_m))
  sample_index<-sample(choose_range_t,50)
  x<-PS_m[sample_index,]#random sample from population with mean 25, variance 16
  means[i] = mean(x) #accumulate statistics about the sample mean
  #Estimate a confidence interval from Student t
  lower[i] <- mean(x) + qt(0.025, 49) * sd(x)/sqrt(50) #usually less than the true mean
  upper[i] <- mean(x) + qt(0.975, 49) * sd(x)/sqrt(50) #usually greater than the true mean
  if(i <= 100) segments(lower[i], i, upper[i], i)
}
abline (v = mean(PS_m$salary), col = "red") #vertical line at true mean
#Quantiles for the sampling distribution of the population
qnt<-quantile(means, c(.025, .975)); qnt ; sum(qnt)/2
#These are close to the mean confidence interval
mean(lower); mean(upper); (mean(lower)+ mean(upper))/2

sum(means > qnt[2])/1000 #what fraction of the time did upper quantile fall below true mean?
sum(mean(PS_m$salary)> upper)/1000 #what fraction of the time did Student's confidence interval fall below the true mean?
sum(means < qnt[1])/1000 #what fraction of the time did lower quantile lie above true mean?
sum(mean(PS_m$salary) < lower)/1000 #what fraction of the time did Student's confidence interval lie above the true mean?

# The reason why the percentage of points lower then lower .25 bound is  
# the two original samples are not strictly normaly distributed. 
# So we are 95 percent sure that the true mean of salary of male full professor 
# is between 120010.0 and 133831.5 




# Simple linear regression

# I am interested to check if there is a relationship between 
# professors' salary and experince 


#I especially choose professors and Assocociate professors
yrs_since_phd<-subset(Salaries,select=yrs.since.phd,subset=((rank=='Prof'))|
                        ((rank=='AssocProf')))
salary<-subset(Salaries,select=salary,subset=((rank=='Prof'))|
                 ((rank=='AssocProf')))

#scaled by divided by 1000 
scale_salary<-salary/1000

yrs_phd_salary<-data.frame(yrs_since_phd,scale_salary)
colnames(yrs_phd_salary) <- c("years.since.Phd","scaled.salary")  

choose_range_t<-c(1:nrow(PS_t))
sample_index<-sample(choose_range_t,100)
yrs_phd_salary<-yrs_phd_salary[sample_index,]

plot(yrs_phd_salary$years.since.Phd,yrs_phd_salary$scaled.salary,main="Professors' salary",xlab = 'years since Phd'
     ,ylab = 'salary')

abline(h = mean(yrs_phd_salary$scaled.salary), col = "red")
abline(v = mean(yrs_phd_salary$years.since.Phd), col = "red")

# of the points are in first quadrant and third
# quadrant, that covriance of years since Phd and salary should be positive 

cor.test(yrs_phd_salary$years.since.Phd,yrs_phd_salary$scaled.salary)
# from Pearson's product-moment correlation
# I can see there is a positive correlation
# betwween professors' salary and experince. 


hb_m.lm <- lm(scaled.salary~ years.since.Phd,data=yrs_phd_salary);hb_m.lm


#  Appropriate use of novel statistics (eg, trimmed mean, skewness, 
#  median absolutedeviation, least-absolute-error regression, ratios, 
#  order statistics, R squared) 
summary(hb_m.lm)
# the inference of beta, I find years.since.Phd has p value of 0.0837
# and has coefficient of 0.6555, that means one addtional years 
# there is associate 0.6555 * 1000 salary increase


# simple linear regression assumptions verification
# A graphical display unlike one presented in the textbook or 
# course scripts
par(mfrow = c(2, 2))
plot(hb_m.lm)
par(mfrow = c(1, 1))

# delete outliers 
# For a given continuous variable, outliers are those observations that lie outside 1.5*IQR, where IQR, 
# the 'Inter Quartile Range' is the difference between 75th and 25th quartiles. 
# Look at the points outside the whiskers in below box plot.

lowerq = quantile(yrs_phd_salary$scaled.salary)[2]
upperq = quantile(yrs_phd_salary$scaled.salary)[4]
iqr = upperq - lowerq #Or use IQR(data)
# Compute the bounds for a mild outlier:

mild.threshold.upper = (iqr * 1.5) + upperq;mild.threshold.upper
mild.threshold.lower = lowerq - (iqr * 1.5);mild.threshold.lower


hist(yrs_phd_salary$scaled.salary,probability = TRUE)
curve(dnorm(x, mean=mean(yrs_phd_salary$scaled.salary),sd=sd(yrs_phd_salary$scaled.salary))
      , col = "red", add= TRUE)

summary(yrs_phd_salary$scaled.salary)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 57.8    91.0   107.3   113.7   134.2   231.5 

#trim to 75% 
trimed_yrs_phd_salary<-subset(yrs_phd_salary,subset=(yrs_phd_salary$scaled.salary>=mild.threshold.lower & 
                                                       yrs_phd_salary$scaled.salary<=mild.threshold.upper))


hb_m.lm <- lm(scaled.salary~ years.since.Phd,data=trimed_yrs_phd_salary);hb_m.lm

confint(hb_m.lm, 'years.since.Phd', level=0.95)
#    2.5 %    97.5 %
#   years.since.Phd -0.6526206 0.8834089


par(mfrow = c(2, 2))
plot(hb_m.lm)
par(mfrow = c(1, 1))

summary(hb_m.lm)
# the inference of beta, I find years.since.Phd has p value of 0.0615
# and has coefficient of 0.5443, that means one addtional year
# there is associate 0.5443 * 1000 salary increase
# A scatter plot with regression line

plot(yrs_phd_salary$years.since.Phd,yrs_phd_salary$scaled.salary
     ,main="years since Phd"
     ,xlab = 'years since Phd'
     ,ylab = 'salary')
abline(a= 111.818,b=0.329,col='red')


#leverage points
#there might have some leverage points to drag
#the regression line
cooks_distence<-cooks.distance(hb_m.lm)
plot(cooks_distence)


#Prediction of a professor who has 28 years working experince
mass <- data.frame(years.since.Phd=28)
confint_mass <- predict(hb_m.lm, mass, interval=c("confidence"))
print(confint_mass)
# the confidence interval of mean salary is below
# fit      lwr      upr
# 1 120.9482 114.2686 127.6278
predint_mass <- predict(hb_m.lm, mass, interval=c("prediction"))
print(predint_mass)
# # the prediction interval of  salary is below 
# fit      lwr      upr
# 1 120.9482 76.02208 165.8743

# Beta confidence interval & prediction interval
visreg(hb_m.lm,main='confidence interval & prediction interval')



#(3) bayesian statistics

# bayesian statistics
# Bayesian prior updated by data

# At british primier soccer league, leicester city
# just won the first ever championship in the club 
# history since 1884. Who can imagine that last season
# leicester city was one of the worst team in premier league
# they lost more of the games and almost was degraded to 
# championship league(2rd league in England)
# I got some match data of leicester city since last year august. 
# let see how their odds changed!


# (a)
theta <- seq(0,1,by=.1) #ranges from 0.0 to 1.0
#The "Bayesian prior" specifies the probability of each value.
prior <- c(0.9,0.05,0.03,0.01,0.005,0.005,0,0,0,0,0); sum(prior) #must sum to 1
#A broken-line plot of the prior
plot(theta, prior, type = "b", main='leicester city winning distribution',ylim = c(0, 1), ylab = "Probability")

likelihood <- theta^3*(1-theta)^2; likelihood 
P1W2L<-sum(prior* likelihood); P1W2L
posterior <-prior * likelihood/ P1W2L; posterior
sum(posterior)
sum(theta*prior) #prior mean
sum(theta*posterior) #posterior mean

#Add the new "posterior" distribution to the plot
lines(theta, posterior, type="b", col = "red")


likelihood2 <- theta^5*(1-theta)^0
P2W3L<-sum(posterior* likelihood2)
posterior2 <-posterior * likelihood2/ P2W3L
sum(theta*posterior2) #expectation is same as before
lines(theta, posterior2, type="b", col = "blue")

likelihood3 <- theta^7*(1-theta)^0
P2W3L<-sum(posterior2* likelihood3)
posterior3 <-posterior2 * likelihood3/ P2W3L
sum(theta*posterior3) #expectation is same as before
lines(theta, posterior3, type="b", col = "brown")

likelihood4 <- theta^8*(1-theta)^2
P2W3L<-sum(posterior3* likelihood4)
posterior4 <-posterior3 * likelihood4/ P2W3L
sum(theta*posterior4) #expectation is same as before
lines(theta, posterior4, type="b", col = "green")

#I can see that their probalilty of wining the premier league 
#championship has been greatly increased! 
legend("topleft",legend = c("prior", "posterior1", "posterior2","posterior3","posterior4"),
       lty = 1, col = c("black","red","blue", "brown", "green"))






# (4) Multiple linear regression 

# DATE: Time period (1-30)
# CONSUME: Ice cream consumption in pints per capita
# PRICE: Per pint price of ice cream in dollars
# INC: Weekly family income in dollars
# TEMP: Mean temperature in degrees F

# I investigate the factors affecting Ice cream consumption
# Also, is there any interactions among those factors

iceCreamConsumption <- read.table("C:/Users/peimo/Desktop/MATH 156/Final_Project/Data/iceCreamConsumption.csv", header=TRUE, quote="\"")

#remove null missing value
iceCreamConsumption<-subset(iceCreamConsumption,subset=Lag.temp!='?')


m_ic<-lm(IC~price + temp+income, data=iceCreamConsumption)
summary(m_ic)

# R-squared:  0.6948,  Adjusted R-squared:  0.6582 
# with relative high r-squared and Adjusted R-squared,
# the model looks good!

# verfiy assumptions
# I assume each data is randomly selected
# each data is indepent to each other
# I can see from following plots, residual is rondomly located
# so it meets equal variance assumtion
# from qq plot, the data is approximately normaly distributed

par(mfrow = c(2, 2))
plot(hb_m.lm)
par(mfrow = c(1, 1))


# detect if there is interaction tearm 
# I suspect price and income interactoin term
m_ic<-lm(IC~price + temp+income+price*income, data=iceCreamConsumption)
summary(m_ic)

# I can see ineractin term price:income is signficant 
# with p value of 0.0491.  
# I can see that after adding interaction term the r squre increases 
# also adjusted r squre increases
# before adding interaction term
# R-squared:  0.6948,  Adjusted R-squared:  0.6582 
# after adding interaction term
# R-squared:  0.7411,  Adjusted R-squared:  0.698

#overall model: Multiple R-squared:  0.7411,  Adjusted R-squared:  0.698 
# F-statistic: 17.18 on 4 and 24 DF,  p-value: 8.968e-07
# the model is significant

# t= 2.034 and p value is 0.0531 
# and so we reject null hypothesis. There is relationship between 
# CreamConsum and price. After accounting for other fators.

# t= 6.769 and p value is 5.31e-07 
# and so we reject null hypothesis. There is relationship between 
# CreamConsum and temperature. After accounting for other fators.

# t= 2.143 and p value is 0.0424
# and so we reject null hypothesis. There is relationship between 
# CreamConsum and income After accounting for other fators.

# t= -2.072 and p value is 0.0491
# and so we reject null hypothesis. There is relationship between 
# CreamConsum and price:income After accounting for other fators.

par(mfrow = c(2, 2))
plot(hb_m.lm)
par(mfrow = c(1, 1))


# check Multicollinarity
# When some of your predictor variables are correlated

vif(m_ic)
ggpairs(iceCreamConsumption[,1:4])
cor(iceCreamConsumption[,2:4])
pairs(iris[,1:4])


#Prediction price=0.23,temp=50, income=50, price_income=50
mass <- data.frame(price=0.23,temp=50, income=50, price_income=50)
confint_mass <- predict(m_ic, mass, interval=c("confidence"))
print(confint_mass)


predint_mass <- predict(m_ic, mass, interval=c("prediction"))
print(predint_mass)


par(mfrow = c(2, 2))
visreg(m_ic,main='confidence interval & prediction interval')
par(mfrow = c(1, 1))





# (c) Logistic regression, SVM, KNN

# the data is from a website.
# about customer's information and if they buy from the site or not
# i want to use logistic regressino and other machine learning models
# to make a good classifier to help marketing team to target certain
# customers

# our strategy is to send right customers's emails and coupons 
# the worst situation is they would respond us to buy our products
# but we thought they would not!



KidCreative <- read.csv("C:/Users/peimo/Desktop/MATH 156/Final_Project/Data/KidCreative.csv")
KidCreative[,"Obs.No."]<- NULL


# a receiver operating characteristic (ROC), or ROC curve, 
# is a graphical plot that illustrates the performance of 
# a binary classifier system as its discrimination threshold is varied. 
# The curve is created by plotting the true positive rate (TPR) 
# against the false positive rate (FPR) at various threshold settings. 
# The true-positive rate is also known as sensitivity, or recall 
# in machine learning.

ROC_curve <- function(model,t_set,resp,model_name)
{
  prob <- predict(model, newdata=t_set, type="response")
  pred <- prediction(prob, resp)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  
  roc.data <- data.frame(fpr=unlist(perf@x.values),
                         tpr=unlist(perf@y.values),
                         model="GLM")
  
  ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0(model_name," ROC Curve w/ AUC=", auc))
}


rmse <- function(error)
{
  sqrt(mean(error^2))
}

# logistic regression 
cus_m<-glm(Buy~.,data=KidCreative,family=binomial)
summary(cus_m)

error <- cus_m$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 5.703778
predictionRMSE
# 2.116948

ROC_curve(cus_m,KidCreative,KidCreative$Buy,"logistic reg")




# Use the caret R package to split the data into a training set with 80% of data 
# and a test set with the remaing 20%. Then use glm() to build a model. What is the accuracy?
library(dplyr)
library(caret)

inTrain <- createDataPartition(y = KidCreative$Buy, p = 0.8)
train_set <- slice(KidCreative, inTrain$Resample1)
test_set <- slice(KidCreative, -inTrain$Resample1)

fit <- glm(Buy~., data=train_set, family="binomial")
pred <- predict(fit, newdata = test_set, type = "response")
tab <- table(pred = round(pred), truth = test_set$Buy)
confusionMatrix(tab)
# pred   0   1
# 0 105   5
# 1   3  21

ROC_curve(fit,test_set,test_set$Buy,"logistic reg")

error <- fit$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 5.703778
predictionRMSE
# 1.842402


# We see that obtain a very high accuracy, but note that this is a random variable due to the random split of our data. 
# Try 10 new random splits and report on how much our accuracy changes.

acc <- sapply(1:10,function(i){
  inTrain <- createDataPartition(KidCreative$Buy,
                                 p=0.8)
  train_set <- slice(KidCreative, inTrain$Resample1)
  test_set <- slice(KidCreative, -inTrain$Resample1)
  fit <- glm(Buy~., data=train_set, family="binomial")
  pred <- predict(fit, newdata = test_set, type = "response")
  tab <- table(pred = round(pred), 
               truth = test_set$Buy)
  confusionMatrix(tab)$overall["Accuracy"]
})
mean(acc)
# 0.9149254
sd(acc)
# 0.0228531

#SVM cross validation
inTrain <- createDataPartition(y = KidCreative$Buy, p = 0.8)
train_set <- slice(KidCreative, inTrain$Resample1)
test_set <- slice(KidCreative, -inTrain$Resample1)

fit <- svm(Buy~., data=train_set, family="binomial")
pred <- predict(fit, newdata = test_set, type = "response")
tab <- table(pred = round(pred), truth = test_set$Buy)
confusionMatrix(tab)
#      truth
# pred   0   1
# 0 107  11
# 1   3  13

ROC_curve(fit,test_set,test_set$Buy,"SVM ")


error <- fit$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 5.703778
predictionRMSE
# 0.2162786


# so compared with logistic model
# even SVM has lower rmse 
# but it generates more false negative
# it is the worst situation that
# if you send customer email, they would buy it but you did not!
# so logistic regressin is better!
# 
# 
# One more Optoin
# KNN
# 
# Compare your glm() model to a knn(). Use the train() function to 
# run 10 cross validations leaving out 20% of the data. Plot your results.

control <- trainControl(method='cv', number=10, p=.8)
dat <- mutate(KidCreative, y=factor(Buy))
res <- train(y ~ .,
             data = dat,
             method = "knn",
             trControl = control,
             tuneLength = 1, # How fine a mesh to go on grid
             tuneGrid=data.frame(k=seq(1,25,2)),
             metric="Accuracy")
res$results %>% 
  ggplot(aes(k, Accuracy, ymin= Accuracy - AccuracySD, 
             ymax = Accuracy + AccuracySD)) +
  geom_point() + geom_errorbar()








