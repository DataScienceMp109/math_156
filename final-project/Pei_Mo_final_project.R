
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

PS_t<-subset(Salaries,select=salary,subset=(discipline=='A'&rank=='Prof'))
PS_a<-subset(Salaries,select=salary,subset=(discipline=='B'&rank=='Prof'))

nrow(PS_t)
# 131
nrow(PS_a)
# 135

# randomly choose 100 samples each of two groups
choose_range_t<-c(1:nrow(PS_t))
sample_index<-sample(choose_range,130)
sample_PS_t<-PS_t[sample_index,]

choose_range_t<-c(1:nrow(PS_a))
sample_index<-sample(choose_range,130)
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
sample_index<-sample(choose_range,28)
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


# I will build t confidence interval to estimate the mean of male professors
PS_m<-subset(Salaries,select=salary,subset=(sex=='Male'&rank=='Prof'))
mean(PS_m$salary)
nrow(PS_m)

plot(x =c(110000,140000), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot

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
  sample_index<-sample(choose_range,50)
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




