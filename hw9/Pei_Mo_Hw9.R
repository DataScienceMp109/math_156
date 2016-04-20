
# Student Name: Mo Pei
# Assignment Name: Homework 9



# Problem One
Beerwings <- read.csv("C:/Users/peimo/Desktop/MATH 156/Data/Beerwings.csv")
# (a)
Hotwings<-Beerwings$Hotwings
Beer<-Beerwings$Beer

summary(Beer)
summary(Hotwings)

plot(Beer, Hotwings, main="Hotwings and Beer", 
     xlab="Beer ", ylab="Hotwings ", pch=19)

abline(h = mean(Hotwings), col = "red")
abline(v = mean(Beer), col = "red")

# From plot, we can see most of the points are in first quadrant and third
# quadrant, that means covriance of Hotwings and Beer should be positive 

cor(Beer,Hotwings)
# 0.7841224
# Hotwings and Beer are positively correlated with correlation coefficient of 
# 0.7841224

# (b)

hb_m.lm <- lm(Beer~ Hotwings, data =Beerwings);hb_m.lm
# Coefficients:
#   (Intercept)     Hotwings  
# 3.040        1.941  

# Equation: Beer hat = 3.040 + 1.941 * hotwings  
# Interpretation: one additional hotwings increase, there will be assciociated
# of 1.941 in beer increase

# (c)
summary(hb_m.lm)$r.squared 
# 0.614848
# So 0.614848 variability in the beers' change is explained by the model


# Problem Two
Illiteracy <- read.csv("C:/Users/peimo/Desktop/MATH 156/Data/Illiteracy.csv")

# (a)
  
Illit<-Illiteracy$Illit
Births<-Illiteracy$Births
plot(Illit,  Births, main="Births and Illit", 
     xlab="Births ", ylab="Illit ", pch=20)

abline(h = mean(Births), col = "red")
abline(v = mean(Illit), col = "red")

# we can see from scatter plot birth rate and illiteracy are roughly positively
# correlated

# (b)
ib_m.lm <- lm(Births~Illit, data =Illiteracy);ib_m.lm
# Coefficients:
#   (Intercept)        Illit  
# 1.94874      0.05452    

# Equation: Births hat = 1.94874 + 0.05452 * Illit  
# Interpretation: one additional percent Illit increase, there will be assciociated
# of 0.05452 in Births increase

# (c)

plot(Illiteracy$Births,resid(ib_m.lm),ylab='Residuals')
abline(h=0)
# it violates the condition of equal variance of residual
# we can see a systematic pattern of residuals, it is a fan shape
# So least square method might not work for this problem

# (d)
# No, even birth rate and litteracy are associated with each other but 
# it does not mean causation. 
# Thereby, we cannnot improvement of literacy causes increase of birth rate


# Problem Three

# (a)
n <- nrow(Illiteracy)
#Now resample 5000 times to investigate the distribution of estimates
N <- 5000; cor.boot <- numeric(N); 
for (i in 1:N){
  index <- sample(n, replace = TRUE)
  Illiteracy.boot <- Illiteracy[index, ]
  cor.boot[i] <- cor(Illiteracy.boot$Births, Illiteracy.boot$Illit)
}
#This computation takes a few seconds even on a fast computer!

mean(cor.boot)
# 0.766798
sd(cor.boot)
# 0.0416538
#Now we can look at the distribution of the resampled results
hist(cor.boot)   #resembles Figure 9.17a
quantile( cor.boot, c(.025, .975)) #95% confidence interval for correlations
# 2.5%     97.5% 
#   0.6790740 0.8402563 

# (b)
#Now do 5000 permutations to see whether your actual beta could arise by chance.
N <- 4999
n <- nrow(Illiteracy)
cor.perm <- numeric(N)
for (i in 1:N) {
  index = sample(n, replace = FALSE) #a permutation of 1:24
  Illiteracy.perm <- Illiteracy$Births[index]
  cor.perm[i] <-cor(Illiteracy$Illit, Illiteracy.perm) #correlation
}

hist(cor.perm, xlim = c(-1,1))
abline(v = observed, col = "red")    #0.83 is off the charts


# Problem Four
Titanic <- read.csv("C:/Users/peimo/Desktop/MATH 156/Data/Titanic.csv")

# (a)

glm(Survived~Age,data=Titanic,family=binomial)
# Coefficients:
#   (Intercept)          Age  
# -0.66076     -0.02376 

# Interpretation: one additional age increase, there will be assciociated
# of 0.02376 decrease in log odds of survival 

# (b)
e <- exp(1) 

alpha<-(-0.66076)
beta<-(-0.02376)
# by formula
# odds of 30's male survival 

e^(alpha+beta*30)
# 0.2532039

# odds of 40's male survival 

e^(alpha+beta*40)
# 0.1996559

#odds ratio of survival of 30s to 40s
e^(alpha+beta*30)/e^(alpha+beta*40)
# 1.268202

# the odds of survival to 30s male is 1.268202 times greater then odds of 40s male


# (c)
n <- nrow(Titanic)
#Now resample 5000 times to investigate the distribution of estimates
N <- 5000; beta.boot <- numeric(N); 
for (i in 1:N){
  index <- sample(n, replace = TRUE)
  Titanic.boot <- Titanic[index, ]
  fit.boot<-glm(Survived~Age,data=Titanic.boot,family=binomial)
  beta.boot[i] <- coef(fit.boot)[2]
}
#This computation takes a few seconds even on a fast computer!

mean(beta.boot)
# -0.02388528
sd(beta.boot)
# 0.008184851
#Now we can look at the distribution of the resampled results
hist(beta.boot)   #resembles Figure 9.17a
quantile( beta.boot, c(.025, .975)) #95% confidence interval for correlations
# 2.5%        97.5% 
#   -0.040253308 -0.008284875 

# (d)

# survival probablity of 60s male

e<-exp(1)

e^(alpha+beta*60)/(1+e^(alpha+beta*60))
# 0.1104296

n <- nrow(Titanic)
#Now resample 5000 times to investigate the distribution of estimates
N <- 5000; beta.boot <- numeric(N); alpha.boot <- numeric(N); 
pPred.boot<-numeric(N)
for (i in 1:N){
  index <- sample(n, replace = TRUE)
  Titanic.boot <- Titanic[index, ]
  fit.boot<-glm(Survived~Age,data=Titanic.boot,family=binomial)
  alpha.boot[i] <- coef(fit.boot)[1]
  beta.boot[i] <- coef(fit.boot)[2]
  pPred.boot[i] <-plogis(alpha.boot[i]+beta.boot[i]*60)
  
}

hist(pPred.boot)   # resembles Figure 9.18
quantile( pPred.boot, c(.025, .975)) #95% confidence interval for predicted free score
# 2.5%      97.5% 
#   0.06301572 0.17180543 

