#Math 156 Script 9A-LinearRegression.R
#Last modified: April 9, 2016 by Paul Bamberg

#Topic 1 - Covariance and correlation for two random variables.
#Example from sections 9.1 and 9.2 of the textbook.

#Load the Black Spruce data
Spruce <-read.csv("Spruce.csv"); head(Spruce)
Ht <- Spruce$Ht.change; Di <- Spruce$Di.change

#Does a large height change go with a large diameter change?
plot(Ht, Di)   #replicates figure 9.3 on page 249
#A scatter plot suggests so.
abline(h = mean(Di), col = "red")
abline(v = mean(Ht), col = "red")
#The NE quadrant where both variables are above the mean is heavily populated.
#So is the SW quadrant where both variables are below the mean.
#Covariance is E[XY] - E[X]E[Y] -- zero for independent variables.
cov(Ht, Di) #just the covariance
#As with variance, R computes an unbiased estimate from a sample.
#This would be the correct formula if we had the entire population.
mean( (Ht-mean(Ht)) * (Di-mean(Di)))
#As with variance, the built-in R function divides by n-1
n<- length(Ht)
sum( (Ht-mean(Ht)) * (Di-mean(Di)))/(n-1)

#Here is a quicker way to get the answer (similar to Proposition 9.1)
Covar<-(sum(Ht*Di)-n*mean(Ht)*mean(Di))/(n-1); Covar

#If we bind the columns into a matrix, we get the entire covariance matrix
cov(cbind(Ht,Di));  
#The diagonal entries are the sample variances
var(Ht); var(Di)

#Divide the covariance by the product of the standard deviations
Covar/(sd(Ht)* sd(Di))
#This is the correlation
cor(Ht,Di) #in the range [-1,1]
#The correlation does not depend on whether we divide by n or by n-1

#Topic 2 - Least-squares regression for two random variables.

#Find the regression line y = a + bx for the same spruce data

#Here is the formula for the slope of the line
b <- sum( (Ht-mean(Ht)) * (Di-mean(Di)) / sum((Ht-mean(Ht))^2));b #equation 9.4

#Here is the formula for the intercept
a <- mean(Di) - b*mean(Ht);a   #equation 9.5

#We can add this regression line to the plot of the data
abline(a, b, col = "red")

#It is quicker to use the built-in R function
spruce.lm <- lm(Di.change~Ht.change, data =Spruce);spruce.lm

#R also has a function to fit the data with a cubic polynomial
lines(smooth.spline(Ht, Di, df=3), col = "blue")

#The coefficients a and b were chosen to minimize the sum of squares of the residuals
#we can split the observations into predicted values and residuals
PredictDi = a + b * Ht
points(Ht,PredictDi, col = "green",add = TRUE, pch = 20)  #these lie on the regression line
#The residual is the observation minus the prediction
ResidDi = Di - PredictDi

#We can plot the residuals separately
par(mfrow=c(2,1))
plot(Ht,Di)      #for comparison
abline(a, b, col = "red")
points(Ht,PredictDi, col = "green",add = TRUE, pch = 20)

plot(Ht,ResidDi)    #by construction, the residuals have a mean of zero
abline(h=0, col = "red")

#By using a cubic polynomial we could make the sum of squares a bit smaller.
lines(smooth.spline(Spruce$Ht.change, resid(spruce.lm), df=3), col = "blue")
#If the cubic curve is close to flat, linear regression fits the data nicely.
par(mfrow=c(1,1)) #always restore graphics parameters!

#Splitting up the variance
vDi <- var(Di); vDi #variance of observations
vPDi <- var(PredictDi); vPDi #variance of predicted values
vRDi <-var(ResidDi); vRDi #variance of residuals

#The variance of the observations is the sum of the two pieces
vDi; vPDi +  vRDi

#The square of the correlation equals fraction of the variance explained by the predictions.
#This ratio is called R-squared and appears in many problems in the textbook.
vPDi/vDi; cor(Ht, Di)^2

#Topic 3 - a maximum-likelihood approach
#This approach leads to the same regression line but needs stronger assumptions.
#View the heights as a fixed vector, not a random variable.
#Assume that the diameter is a linear function of the height, plus a random residual
#Assume further that the residuals are independent
#and normally distributed with variance sigsq
library(stats4)   #needed for MLL

#Construct minus the Log-likelihood (done on p. 269 of textbook)
MLL<-function(alpha, beta, sigsq) n*sqrt(2*pi*sigsq)+ sum((Di-alpha-beta*Ht)^2)/(2*sigsq)
mle(MLL, start = list(alpha = 0 , beta = 0.2, sigsq = 1))
#The MLE estimates of alpha and beta are the a and b from least-squares
a; b
#The MLE estimate of the variance of the residuals is way off
vRDi  #computed from the data

#The problem is that for spruce trees, the residuals do not have a normal distribution
hist(ResidDi, breaks= 20, probability = TRUE)
curve(dnorm(x, mean(resid(spruce.lm)),sd(resid(spruce.lm))), col = "red", add = TRUE)
#Perhaps the big positive residuals are from well-fertilized trees

#Topic 4 - checking the MLE approach when the residuals really have a normal distribution
#For comparison, We can fabricate some data that complies with the model.
a<- 0.6 #intercept
b<- 0.3 #slope
s<- 0.2 #variance of residuals
x<- runif(100, min = 3, max = 7) #like heights, not uniformly spaced
y<- a + b*x + rnorm(100,0, sqrt(s))  #like diameters, with random residuals

#First do the standard regression-line stuff using the built-in function.
linMod <- lm(y ~ x); linMod  #the coefficients will not necessarily equal a and b!
plot(x,y)
abline(linMod, col = "red")

#Now try the MLE approach
MLL<-function(alpha, beta, sigsq) n*sqrt(2*pi*sigsq)+ sum((y-alpha-beta*x)^2)/(2*sigsq)
results<- mle(MLL, start = list(alpha = 0 , beta = 0.2, sigsq = 1));results
results@coef #this is how to extract something from an S4 class
#The MLE estimates of alpha and beta are the same as before
 
mean((y-a-b*x)^2) #variance of the random residuals that we generated
mean((y-results@coef[1]-results@coef[2]*x)^2) #variance of the calculated residuals

#The MLE estimate of the variance is pretty good, because the residuals now fit the model.
hist(resid(linMod), breaks= 20, probability = TRUE)
curve(dnorm(x, mean(resid(linMod)),sd(resid(linMod))), col = "red", add = TRUE)

