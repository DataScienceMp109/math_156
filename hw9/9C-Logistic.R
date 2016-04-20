#Math 156 Script 9C-Logistic regression.R
#Last modified: April 9, 2016 by Paul Bamberg

#Based on section 9.6 of the textbook

#Topic 1 - doing linear regression when the response is always zero or one
#Load the Fatal Analysis Reporting System data
Fatalities <- read.csv("Fatalities.csv");head(Fatalities)
#This predictor is Age, the response is Alcohol

#First, the usual scatter plot
plot(Fatalities$Age, Fatalities$Alcohol) #looks like Figure 9.23a

#Make a contingency table for Age and Alcohol
tbl<-table(Fatalities$Age, Fatalities$Alcohol);head(tbl)

#Turn the vector of ages from the table into a numeric vector
x<-as.numeric(rownames(tbl));x #a vector of the ages

#Now make a table of the probability of alcohol involvement as a function of age
#The following is heavy-handed, but I cannot figure out an easier way to construct the table.
prob<-numeric(length(x))
for (i in 1:length(x)) {
  prob[i] = sum((Fatalities$Age == x[i])*Fatalities$Alcohol)/sum((Fatalities$Age == x[i]))
}
plot(x,prob) #if we had lots more data this might look more like other regression examples

#It would be possible but unconventional to do standard linear regression
fatal.lm <- lm(prob~x);fatal.lm
abline(fatal.lm)   #slope is -0.004
#Notice that for a 90-year-old driver,
#the predicted probability of alcohol involvement is negative!

#An equivalent approach is to treat drivers of the same age as separate cases (realistic). 
#Again we have a predicted probability as a function of age,
#but the observed probability is always 0 or 1
plot(Fatalities$Age, Fatalities$Alcohol) #looks like Figure 9.23a
fatal2.lm <- lm(Fatalities$Alcohol ~ Fatalities$Age);fatal2.lm
#The regression parameters are exactly the same!
abline(fatal2.lm, col="red")


#Topic 2 - a maximum likelihood approach
#We could still assume that probability is a linear function of age
attach(Fatalities)
library(stats4)
MLL<- function(alpha, beta)
  -sum(log(alpha+beta*Age)*Alcohol+ log(1-alpha-beta*Age)*(1-Alcohol))
mle(MLL,start = list(alpha = 0.4, beta = -0.004)) #fails - logs of negative numbers

#Topic 3 - logistic regression
#The conventional approach is to use a different functional form.
#We no longer assume that alcohol use probability p is a linear function of age x.
#Instead, we assume that p = exp(alpha x+beta)/(1 + exp(alpha x+beta)).
#This function can never be less than zero nor greater than 1.
#Start with minus the log of the likelihood function.
MLL<- function(alpha, beta)
  -sum( log( exp(alpha+beta*Age)/(1+exp(alpha+beta*Age)) )*Alcohol+ log(1/(1+exp(alpha+beta*Age)))*(1-Alcohol) )
results<-mle(MLL,start = list(alpha = -0.1, beta = -0.02))
results@coef
curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)
#The shape is similar, but the probability stays positive.




