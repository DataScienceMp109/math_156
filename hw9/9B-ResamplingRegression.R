#Math 156 Script 9B-ResamplingRegression.R
#Last modified: April 9, 2016 by Paul Bamberg

#Topic 1 - getting a confidence interval for regression coefficients
#Section 9.5 - pages 279-286 of the textbook
#You can skip the details of Theorems 7.3 through 7.5 (two of which are not proved!)

#Load Olympic men's skating data from 2010
Skating2010<- read.csv("Skating2010.csv"); head(Skating2010)

#Plot the free score against the short score
plot(Skating2010$Short, Skating2010$Free)

#Look at the correlation
cor(Skating2010$Short, Skating2010$Free)

#Get the regression line by using the lm() function of R
skate.lm <- lm(Free ~ Short, data = Skating2010)
summary(skate.lm)  #detailed explanation on page 273 of the textbook.
abline(7.9691, 1.7347)
#Notice that lm() also tells us the standard error of the slope beta, 0.2424.

#Replicate the calculation of the confidence interval on page 273.
#We are assuming independent, normally distributed residuals
#There are 24 skaters, but we estimated two parameters.
#So we should use the t distribution with 22 degrees of freedom

curve(dt(x, 22), from = -5, to = 5)
#Find the quantiles for a 95% confidence interval
q95 = qt(c(.025, .975), 22);  q95
abline(v = q95[1]); abline(v = q95[2])

#To get a t confidence interval, rescale by the standard error of beta,
#then add in the estimate of beta.
CI <- 0.2424*q95 + 1.747; CI

#we can also predict the expected free score for a skater whose short score is 60
Y60 <- 7.9691 + 1.7347*60; Y60
#If we want to calculate a confidence interval for this score, things get complicated. 
#Furthermore, all the t confidence calculations assume that the residuals are independent
#and normally distributed.

#Note the correlation for future reference
cor(Skating2010$Short, Skating2010$Free)

#Topic 2 - applying the bootstrap to linear regression
#We can do bootstrapping to assess the significance of these numbers.
#A bootstrap sample consists of 24 pairs of scores, chosen with replacement.
#It will  have duplicate rows for some of the skaters!

n <- nrow(Skating2010); n
index <- sample(1:n,n, replace = TRUE); index #choose 24 rows
Skate.boot <- Skating2010[index, ]; Skate.boot #will have duplicate names!
skate.boot.lm <- lm(Free ~ Short, data = Skate.boot)
skate.boot.lm   #compare with slope of 1.747

#The correlation will be different for each bootstrap sample
cor(Skate.boot$Short, Skate.boot$Free)  #compare with 0.836
#Resample by hand several times to see how beta and the correlation vary


#Now resample 5000 times to investigate the distribution of estimates
N <- 5000; cor.boot <- numeric(N); alpha.boot <- numeric(N)
beta.boot <- numeric(N); Y60.boot <-numeric(N)
for (i in 1:N){
  index <- sample(1:n,n, replace = TRUE); index 
  Skate.boot <- Skating2010[index, ]; Skate.boot
  skate.boot.lm <- lm(Free ~ Short, data = Skate.boot)
  alpha.boot[i] <- coef(skate.boot.lm)[1]  #intercept
  beta.boot[i] <- coef(skate.boot.lm)[2]  #slope
  Y60.boot[i] <- alpha.boot[i] + 60 * beta.boot[i]  #predicted free score
  cor.boot[i] <- cor(Skate.boot$Short, Skate.boot$Free)
}
#This computation takes a few seconds even on a fast computer!

#Now we can look at the distribution of the resampled results
hist(cor.boot)   #resembles Figure 9.17a
quantile( cor.boot, c(.025, .975)) #95% confidence interval for correlations
hist(beta.boot)   #resembles Figure 9.17b
quantile( beta.boot, c(.025, .975)) #95% confidence interval for slope
hist(Y60.boot)   # resembles Figure 9.18
quantile( Y60.boot, c(.025, .975)) #95% confidence interval for predicted free score

#We can also draw 50 of the regression lines to compare with our estimated one
plot(Skating2010$Short, Skating2010$Free)
abline(7.9691, 1.7347, lwd = 4)   #from the actual data
for (i in 1:50) {
  abline(alpha.boot[i], beta.boot[i], col = "red")   #from the resampled data
}
mean(beta.boot)   #is 1.74 - bootstrapping cannot improve our estimate of the mean

#Topic 3 - Permutation test for lack of independence

#To test for lack of independence, we can permute one of the variables.
#The book permutes the x values; it seems nicer to permute y
#After permutation, the correlation and beta should have expectation zero
cor.obs <- cor(Skating2010$Short, Skating2010$Free); cor.obs
beta.obs <- 1.7347
index = sample(n, replace = FALSE) #a permutation of 1:24

Free.perm <- Skating2010$Free[index] #permutation of free skate scores
plot(Skating2010$Short, Free.perm)   #no correlation should be evident
coef(lm(Free.perm~ Skating2010$Short))[2]  #beta should be small in magnitude
cor(Skating2010$Short, Free.perm) #as likely to be negative as positive

#Now do 5000 permutations to see whether your actual beta could arise by chance.
N <- 4999; n <- nrow(Skating2010)
cor.perm <- numeric(N); beta.perm <- numeric(N)
for (i in 1:N) {
  index = sample(n, replace = FALSE) #a permutation of 1:24
  Free.perm <- Skating2010$Free[index]
  beta.perm[i] <-coef(lm(Free.perm~ Skating2010$Short))[2]  #beta
  cor.perm[i] <-cor(Skating2010$Short, Free.perm) #correlation
}
hist(beta.perm, xlim = c(-2,2))
abline(v = beta.obs, col = "red")    #1.737 is off the charts
hist(cor.perm, xlim = c(-1,1))
abline(v = cor.obs, col = "red")    #0.83 is off the charts






  


