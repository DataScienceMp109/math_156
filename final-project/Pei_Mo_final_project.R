#Final Project
#Class name: Spring 2016, MATH 156, Harvard University 
#Student Name: Mo Pei


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
sample_index<-sample(choose_range_t,130)
sample_PS_t<-PS_t[sample_index,]

choose_range_a<-c(1:nrow(PS_a))
sample_index<-sample(choose_range_a,130)
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

yrs_since_phd<-subset(Salaries,select=yrs.since.phd,subset=((rank=='Prof')&(sex=='Male'))|
                        ((rank=='AssocProf')&(sex=='Male')))
salary<-subset(Salaries,select=salary,subset=((rank=='Prof')&(sex=='Male'))|
                        ((rank=='AssocProf')&(sex=='Male')))

scale_salary<-salary/1000

yrs_phd_salary<-data.frame(yrs_since_phd,scale_salary)
colnames(yrs_phd_salary) <- c("years.since.Phd","scaled.salary")  


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
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 27.53 on 395 degrees of freedom
# Multiple R-squared:  0.1758,	Adjusted R-squared:  0.1737 
# F-statistic: 84.23 on 1 and 395 DF,  p-value: < 2.2e-16

# simple linear regression assumptions verification

par(mfrow = c(2, 2))
plot(hb_m.lm)
par(mfrow = c(1, 1))

# delete outliers 
# For a given continuous variable, outliers are those observations that lie outside 1.5*IQR, where IQR, 
# the ‘Inter Quartile Range’ is the difference between 75th and 25th quartiles. 
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


summary(hb_m.lm)


# Residuals:
#   Min      1Q  Median      3Q     Max 
# -20.184  -9.385  -1.588   9.856  25.262 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     104.20025    1.88648   55.23  < 2e-16 ***
#   years.since.Phd   0.23612    0.07401    3.19  0.00165 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 11.65 on 198 degrees of freedom
# Multiple R-squared:  0.04889,	Adjusted R-squared:  0.04409 
# F-statistic: 10.18 on 1 and 198 DF,  p-value: 0.001653

par(mfrow = c(2, 2))
plot(hb_m.lm)
par(mfrow = c(1, 1))


# A scatter plot with regression line
plot(yrs_phd_salary$years.since.Phd,yrs_phd_salary$scaled.salary,main="years since Phd",xlab = 'years since Phd'
     ,ylab = 'salary')
abline(a= 111.818,b=0.329,col='red')




# (b) Multiple linear regression



