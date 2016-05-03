#Final Project
#Class name: Spring 2016, MATH 156, Harvard University 
#Student Name: Mo Pei

install.packages("mosaic")
install.packages("mosaicData")
install.packages("Lock5Data")
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
# 
# 
# Data Format
# 
# A data frame with 397 observations on the following 6 variables.
# 
# rank
# a factor with levels AssocProf AsstProf Prof
# 
# discipline (categorical)
# a factor with levels A ("theoretical" departments) 
# or B ("applied" departments).
# 
# yrs.since.phd (numeric)
# years since PhD.
# 
# yrs.service (numeric)
# years of service.
# 
# sex (categorical)
# a factor with levels Female Male
# 
# salary (numeric)
# nine-month salary, in dollars.
# 
# References
# Fox J. and Weisberg, S. (2011) An R Companion to Applied Regression, 
# Second Edition Sage.


Salaries <- read.csv("C:/Users/peimo/Desktop/MATH 156/Final_Project/Data/Salaries.csv")

#(1) Two sample T-test & T confidence interval

#(a) Two sample T-test 
# salaryy mean difference between  of theoretical professors
# and "applied" departments professors

PS_t<-subset(Salaries,select=salary,subset=(discipline=='A'&rank=='Prof'))
PS_a<-subset(Salaries,select=salary,subset=(discipline=='B'&rank=='Prof'))

nrow(PS_t)
# 131
nrow(PS_a)
# 135

# randomly choose 100 samples each of two groups
choose_range_t<-c(1:nrow(PS_t))
sample_index<-sample(choose_range_t,40)
sample_PS_t<-PS_t[sample_index,]

choose_range_a<-c(1:nrow(PS_a))
sample_index<-sample(choose_range_a,40)
sample_PS_a<-PS_a[sample_index,]

sample_salaries<-data.frame(sample_PS_t,sample_PS_a)
colnames(sample_salaries) <- c("t","a")  

plot(sample_salaries$t,sample_salaries$a,xlab="salary of theoretical type professors"
     ,ylab="salary of theoretical type professors")
abline(a=0,b=1,col='red')

#Two sample T-Test to test if mean of theoretical professors
#is different from B "applied" departments professors.

#Assumtions verification
# The populations from which the samples have been drawn should be normal
qqnorm(sample_salaries$t)
qqline(sample_salaries$t)

qqnorm(sample_salaries$a)
qqline(sample_salaries$a)
# The standard deviation of the populations should be equal 
hist(sample_salaries$t, probability = TRUE)
curve(dnorm(x, mean=mean(sample_salaries$t),sd=sd(sample_salaries$t))
      , col = "red", add= TRUE)
hist(sample_salaries$a, probability = TRUE)
curve(dnorm(x, mean=mean(sample_salaries$a),sd=sd(sample_salaries$a))
      , col = "red", add= TRUE)
# Samples have to be randomly drawn independent of each other. 

t.test(sample_salaries$t,sample_salaries$a,alt="greater")

# Welch Two Sample t-test
# 
# data:  sample_salaries$t and sample_salaries$a
# t = -2.786, df = 179.59, p-value = 0.997
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   -17249.93       Inf
# sample estimates:
#   mean of x mean of y 
# 121678.3  132503.7 


#Conclusion: I do not find significant evidence support there is a 
# salaryy mean difference between  of theoretical professors
# and "applied" departments professors




# salaryy mean difference between  of male professors
# and female professors

#I am interested to test if female professors make similar amount of salary
#as male professors do

PS_t<-subset(Salaries,select=salary,
             subset=(sex=='Male'&rank=='Prof')|(sex=='Male'&rank=='AssocProf'))

PS_a<-subset(Salaries,select=salary,
             subset=(sex=='Female'&rank=='Prof')|(sex=='Female'&rank=='AssocProf'))

nrow(PS_t)
# 248
nrow(PS_a)
# 18

# randomly choose 100 samples each of two groups
choose_range_t<-c(1:nrow(PS_t))
sample_index<-sample(choose_range_t,28)
sample_PS_t<-PS_t[sample_index,]


sample_PS_a<-PS_a


sample_salaries<-data.frame(sample_PS_t,sample_PS_a)
colnames(sample_salaries) <- c("t","a")  

plot(sample_salaries$t,sample_salaries$a,xlab="salary of theoretical type professors"
     ,ylab="salary of theoretical type professors")
abline(a=0,b=1,col='red')

#Two sample T-Test to test if mean of theoretical professors
#is different from B "applied" departments professors.

#Assumtions verification
# Samples have to be randomly drawn independent of each other.
# I assume the salary of different professors is independent of each other

# Randomization Condition 
# The data must be sampled randomly. 

# The populations from which the samples have been drawn should be normal
qqnorm(sample_salaries$t)
qqline(sample_salaries$t)

qqnorm(sample_salaries$a)
qqline(sample_salaries$a)
# The standard deviation of the populations should be equal 
hist(sample_salaries$t, probability = TRUE)
curve(dnorm(x, mean=mean(sample_salaries$t),sd=sd(sample_salaries$t))
      , col = "red", add= TRUE)
hist(sample_salaries$a, probability = TRUE)
curve(dnorm(x, mean=mean(sample_salaries$a),sd=sd(sample_salaries$a))
      , col = "red", add= TRUE)


t.test(sample_salaries$t,sample_salaries$a,alt="greater")

# Welch Two Sample t-test
# 
# data:  sample_salaries$t and sample_salaries$a
# t = 2.2889, df = 53.296, p-value = 0.01304
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   4337.106      Inf
# sample estimates:
#   mean of x mean of y 
# 126163.1  110019.5 


#Conclusion: I do find significant (p-value = 0.01304)evidence support there is a 
# salaryy mean difference between  of male professors
# and female professors




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
#Use Student's t to get a confidence interval
mean(PS_m$salary) + qt(0.025, 247) * sd(PS_m$salary)/sqrt(248); mean(PS_m$salary) + qt(0.975, 247) * sd(PS_m$salary)/sqrt(248)
#Run the automated t test
t.test(PS_m$salary, conf.level = .95)    #it gives the same numbers.

# One Sample t-test
# 
# data:  PS_m$salary
# t = 70.955, df = 247, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   123592.1 130649.5
# sample estimates:
#   mean of x 
# 127120.8 

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

# So we are 95 percent sure that the true mean of salary of male full professor 
# is between 123592.1 and 130649.5.


#(2) Regression analysis 

# (a) Simple linear regression

yrs_since_phd<-subset(Salaries,select=yrs.since.phd,subset=((rank=='Prof'))|
                        ((rank=='AssocProf')))
salary<-subset(Salaries,select=salary,subset=((rank=='Prof'))|
                 ((rank=='AssocProf')))

scale_salary<-salary/1000

yrs_phd_salary<-data.frame(yrs_since_phd,scale_salary)
colnames(yrs_phd_salary) <- c("years.since.Phd","scaled.salary")  

choose_range_t<-c(1:nrow(PS_t))
sample_index<-sample(choose_range_t,50)
yrs_phd_salary<-yrs_phd_salary[sample_index,]

plot(yrs_phd_salary$years.since.Phd,yrs_phd_salary$scaled.salary,main="years since Phd",xlab = 'years since Phd'
     ,ylab = 'salary')

abline(h = mean(yrs_phd_salary$scaled.salary), col = "red")
abline(v = mean(yrs_phd_salary$years.since.Phd), col = "red")

# of the points are in first quadrant and third
# quadrant, that covriance of years since Phd and salary should be positive 

cor.test(yrs_phd_salary$years.since.Phd,yrs_phd_salary$scaled.salary)
# Pearson's product-moment correlation
# 
# data:  yrs_since_phd and stdize_salary
# t = 9.1775, df = 395, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.3346160 0.4971402
# sample estimates:
# cor 
# 0.4192311 

#Also, besides the observation from plot,
#we can calculate the covariance and gives correlation co efficient of 0.4192311
#it means years since Phd and salary are positively correlated


hb_m.lm <- lm(scaled.salary~ years.since.Phd,data=yrs_phd_salary);hb_m.lm



summary(hb_m.lm)
# Call:
#   lm(formula = scale_salary ~ yrs_since_phd)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -84.171 -19.432  -2.858  16.086 102.383 
# 
# Coefficients:
#               Estimate Std. Error   t value Pr(>|t|)    
# (Intercept)    91.7187     2.7658  33.162   <2e-16 ***
#   yrs_since_phd   0.9853     0.1074   9.177   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 27.53 on 395 degrees of freedom
# Multiple R-squared:  0.1758,  Adjusted R-squared:  0.1737 
# F-statistic: 84.23 on 1 and 395 DF,  p-value: < 2.2e-16

# simple linear regression assumptions verification

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

summary(hb_m.lm)


# Residuals:
#   Min      1Q  Median      3Q     Max 
# -20.184  -9.385  -1.588   9.856  25.262 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     104.20025    1.88648   55.23  < 2e-16 ***
#   years.since.Phd   0.23612    0.07401    3.19  0.00165 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 11.65 on 198 degrees of freedom
# Multiple R-squared:  0.04889,	Adjusted R-squared:  0.04409 
# F-statistic: 10.18 on 1 and 198 DF,  p-value: 0.001653

par(mfrow = c(2, 2))
plot(hb_m.lm)
par(mfrow = c(1, 1))


# A scatter plot with regression line
plot(yrs_phd_salary$years.since.Phd,yrs_phd_salary$scaled.salary
     ,main="years since Phd"
     ,xlab = 'years since Phd'
     ,ylab = 'salary')
abline(a= 111.818,b=0.329,col='red')


#leverage points
install.packages("influence.ME")
library(lme4)
cooks_distence<-cooks.distance(hb_m.lm)
plot(cooks_distence)


#Prediction
mass <- data.frame(years.since.Phd=28)
confint_mass <- predict(hb_m.lm, mass, interval=c("confidence"))
print(confint_mass)
predint_mass <- predict(hb_m.lm, mass, interval=c("prediction"))
print(predint_mass)

install.packages("visreg")
require(visreg)
visreg(hb_m.lm,main='confidence interval & prediction interval')



# (b) Multiple linear regression 

iceCreamConsumption <- read.table("C:/Users/peimo/Desktop/MATH 156/Final_Project/Data/iceCreamConsumption.csv", header=TRUE, quote="\"")

#remove null missing value
iceCreamConsumption<-subset(iceCreamConsumption,subset=Lag.temp!='?')

m_ic<-lm(IC~price + income, data=iceCreamConsumption)
summary(m_ic)

install.packages("TeachingDemos")
library(TeachingDemos)
library(splines)
Predict.Plot(iceCreamConsumption$IC ~ iceCreamConsumption$price+iceCreamConsumption$income)

m_ic<-lm(IC~price + temp+income, data=iceCreamConsumption)
summary(m_ic)

# Call:
#   lm(formula = IC ~ price + income, data = iceCreamConsumption)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.098772 -0.034685 -0.009381  0.034351  0.117713 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.5777990  0.4161919   1.388    0.177
# price       -0.6669640  1.3804937  -0.483    0.633
# income      -0.0004845  0.0017534  -0.276    0.784
# 
# Residual standard error: 0.05809 on 26 degrees of freedom
# Multiple R-squared:  0.01126,  Adjusted R-squared:  -0.0648 
# F-statistic: 0.148 on 2 and 26 DF,  p-value: 0.8632

par(mfrow = c(2, 2))
plot(hb_m.lm)
par(mfrow = c(1, 1))

m_ic<-lm(IC~price + temp+income+price*income, data=iceCreamConsumption)
summary(m_ic)

Call:
  lm(formula = IC ~ price + temp + income + price * income, data = iceCreamConsumption)

# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.057528 -0.016359 -0.000848  0.016866  0.071892 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -6.3298130  3.1053503  -2.038   0.0527 .  
# price        23.3540200 11.4796351   2.034   0.0531 .  
# temp          0.0028231  0.0004171   6.769 5.31e-07 ***
#   income        0.0780758  0.0364267   2.143   0.0424 *  
#   price:income -0.2786003  0.1344397  -2.072   0.0491 *  
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.03094 on 24 degrees of freedom
# Multiple R-squared:  0.7411,  Adjusted R-squared:  0.698 
# F-statistic: 17.18 on 4 and 24 DF,  p-value: 8.968e-07

par(mfrow = c(2, 2))
plot(hb_m.lm)
par(mfrow = c(1, 1))

vif(m_ic)
# price         temp       income price:income 
# 244.51895      1.33091   1526.23069   1714.78293 

install.packages("GGally")
library(GGally)
ggpairs(iceCreamConsumption[,1:4])
cor(iceCreamConsumption[,2:4])

pairs(iris[,1:4])


#Prediction
mass <- data.frame(price=0.23,temp=50, income=50, price_income=50)
confint_mass <- predict(m_ic, mass, interval=c("confidence"))
print(confint_mass)
# fit        lwr      upr
# 1 -0.1173488 -0.5319696 0.297272

predint_mass <- predict(m_ic, mass, interval=c("prediction"))
print(predint_mass)
# fit       lwr       upr
# 1 -0.1173488 -0.536858 0.3021603

par(mfrow = c(2, 2))
visreg(m_ic,main='confidence interval & prediction interval')
par(mfrow = c(1, 1))





# (c) Logistic regression

KidCreative <- read.csv("C:/Users/peimo/Desktop/MATH 156/Final_Project/Data/KidCreative.csv")

cus_m<-glm(Buy~Income+Is.Female+
      +Is.Married
      +Has.College	
    +Is.Professional	
    +Is.Retired	
    +Unemployed	
    +Residence.Length	
    +Dual.Income	
    +Minors	
    +Own	
    +House	
    +White	
    +English	
    +Prev.Child.Mag	
    +Prev.Parent.Mag,data=KidCreative,family=binomial)
summary(cus_m)

# Call:
#   glm(formula = Buy ~ Income + Is.Female + +Is.Married + Has.College + 
#         Is.Professional + Is.Retired + Unemployed + Residence.Length + 
#         Dual.Income + Minors + Own + House + White + English + Prev.Child.Mag + 
#         Prev.Parent.Mag, family = binomial, data = KidCreative)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.36655  -0.08416  -0.00955  -0.00149   2.49038  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      -1.791e+01  2.223e+00  -8.058 7.74e-16 ***
#   Income            2.016e-04  2.359e-05   8.545  < 2e-16 ***
#   Is.Female         1.646e+00  4.651e-01   3.539 0.000401 ***
#   Is.Married        5.662e-01  5.864e-01   0.966 0.334272    
# Has.College      -2.794e-01  4.437e-01  -0.630 0.528962    
# Is.Professional   2.253e-01  4.650e-01   0.485 0.627981    
# Is.Retired       -1.159e+00  9.323e-01  -1.243 0.214015    
# Unemployed        9.886e-01  4.690e+00   0.211 0.833030    
# Residence.Length  2.468e-02  1.380e-02   1.788 0.073798 .  
# Dual.Income       4.518e-01  5.215e-01   0.866 0.386279    
# Minors            1.133e+00  4.635e-01   2.444 0.014521 *  
#   Own               1.056e+00  5.594e-01   1.888 0.058976 .  
# House            -9.265e-01  6.218e-01  -1.490 0.136238    
# White             1.864e+00  5.454e-01   3.417 0.000632 ***
#   English           1.530e+00  8.407e-01   1.821 0.068678 .  
# Prev.Child.Mag    1.557e+00  7.119e-01   2.188 0.028704 *  
#   Prev.Parent.Mag   4.777e-01  6.240e-01   0.766 0.443900    
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 646.05  on 672  degrees of freedom
# Residual deviance: 182.33  on 656  degrees of freedom
# AIC: 216.33
# 
# Number of Fisher Scoring iterations: 9


