#Furio Jacomini

rm(list = ls()); cat("\014")

#Problem 1
set.seed(456)
sample1 <- rnorm (10 , mean = 2 , sd = 1)
sample2 <- rnorm (100, mean = 2 , sd = 1)
sample3 <- rnorm (1000 , mean = 2 , sd = 1)
sample4 <- rnorm (10000 , mean = 2 , sd = 1)
mean(sample1)
mean(sample2)
mean(sample3)
mean(sample4)
#as we can see, the sample mean continues to get close to 2 the higher the size of the sample

#part b


r <- 10000 #number of times we will replicate the "experiment"
ybar <- numeric(r)
#storing results

set.seed(123)
for (j in 1:r){
  sampleA <- rnorm(10, mean = 2, sd = 1)
  ybar[j] <- mean(sampleA) 
}

set.seed(123)
for (j in 1:r){
  sampleB <- rnorm(100, mean = 2, sd = 1)
  ybar[j] <- mean(sampleB) 
}

set.seed(123)
for (j in 1:r){
  sampleC <- rnorm(1000, mean = 2, sd = 1)
  ybar[j] <- mean(sampleC) 
}

set.seed(123)
for (j in 1:r){
  sampleD <- rnorm(10000, mean = 2, sd = 1)
  ybar[j] <- mean(sampleD) 
}

sd(sampleA)
sd(sampleB)
sd(sampleC)
sd(sampleD)
#standard deviation is becoming closer to zero as population increases
#Reasoning is because the mean becomes more precise and the variability begins to decrease

#Partc
plot(density(sampleA))

plot(density(sampleB))

plot(density(sampleC))

plot(density(sampleD))

#we see that the plots become closer to the mean and more normal

#PartD

set.seed(12345)
tsample1 <- rnorm(10,1)
mean(tsample1)

tsample2 <- rnorm(100,1)
mean(tsample2)

tsample3 <- rnorm(1000,1)
mean(tsample3)

tsample4 <- rnorm(10000,1)
mean(tsample4)

t5 <- 10000
ybar1t <- numeric(t5)
ybar2t <- numeric(t5)
ybar3t <- numeric(t5)
ybar4t <- numeric(t5)

for (i in 1:t5){
  tsample1 <- rt(t1,1)
  ybar1t[i] <- mean(tsample1)
}
sd(ybar1t)
for (i in 1:t5){
  tsample2 <- rt(t1,1)
  ybar2t[i] <- mean(tsample2)
}
sd(ybar2t)

for (i in 1:t5){
  tsample3 <- rt(t1,1)
  ybar3t[i] <- mean(tsample3)
}
sd(ybar3t)

for (i in 1:t5){
  tsample1 <- rt(t1,1)
  ybar4t[i] <- mean(tsample4)
}
sd(ybar4t)

plot(density(ybar1t),xlim=c(0,100),ylim=c(0,3))
lines(density(ybar2t))
lines(density(ybar3t))
lines(density(ybar4t))


#Problem2parta

rm(list = ls()); cat("\014")

r=1000
ybar <- numeric(r) #here we will store the results
mu <- 4
sd <- sqrt(8)
n <- 100

for (j in 1:r){
  sample <- rnorm(n,mu,sd)
  ybar[j] <- mean(sample) 
}
plot(density(sqrt(n)*(ybar-mu)/sd)) #Plots the estimated density of ybar
curve(dnorm(x,0,1), add =TRUE, lty =2) #We overlay a STANDARD NORMAL density
#The approximation is very close. 

#partb

plot(density(n^(1/4)*(ybar-mu)/sd)) #Plots the estimated density of ybar
curve(dnorm(x,0,1), add =TRUE, lty =2) #We overlay a STANDARD NORMAL density
#we can see here that the standard normal overlay graph is very vide and does not 
#follow the graph well at all

#partc

plot(density(sqrt(n)*(ybar-3)/sd)) #Plots the estimated density of ybar
curve(dnorm(x,0,1), add =TRUE, lty =2) #We overlay a STANDARD NORMAL density
#here we can see that the graph is not in the correct spot as we moved it over by 1

#problem3

# Set the number of random points to generate
n <- 1000000

# Generate random x values between 0 and 1
random_x <- runif(n)

# Calculate the corresponding y values (y = x^2)
random_A <- random_x^2

# Estimate the area A using the proportion of points below the curve
estimated_A <- points_below_curve / n_points

# The estimated area A is approximately 1/3. 
# The more points you generate (larger n_points), the more accurate the estimate.

cat("Estimated A:", estimated_A, "\n")


#problem4
##Part a
# Yi = 1 + 2Xi + Ui,
###Because we are doing the reverse regression algebraicly we find that 
# Xi = gam0 + gam1Y1 +Vi
# gam0 would be = to -1/2 and gam1 = to 1/2


###Montecarlo example

rm(list = ls()); cat("\014") # clear

set.seed(123456) # set manually the initial seed

# Now we will set the size of each sample AND also set the number of iterations. 
n <- 500 ; r <- 100
# n=500 means that ONE random sample has size n. r=100 means that we will generate 100
# different random samples of size n. 

# Set model parameters
gam0 <- -0.5
gam1 <- 0.5
mu_y <- 0
sigma_y <- 1
mu_v <- 0
sigma_v <- 1

# different gam1_hat's before running a loop.
gam0_hat <- numeric(r)
gam1_hat <- numeric(r)

# Now we will assume that outcome's randomness only comes from the error term (v).
# This is, we will fix "x" and use the same "x" for all 100 iterations.
y <- rnorm(n, mu_y, sigma_y) 

# Now we use a for loop to carry out the simulation.
for (j in 1:r) { 
  
  v <- rnorm(n, mu_v, sigma_v) # generate a vector of normal numbers
  x <- gam0 + gam1 * y + v 
  
  gam_hats <- coefficients(lm(x ~ y)) # fit regression model, store coefficients
  gam0_hat[j] <- gam_hats["(Intercept)"] # store intercept in position j .
  gam1_hat[j] <- gam_hats["y"] # store slope in position j 
}

hist(gam0_hat) # centered around the true value (unbiasedness)
hist(gam1_hat) # same
mean(gam0_hat) # close to -0.5!
mean(gam1_hat) # close to 0.5!

#partc
# is it consistent 

for (j in 1:r) { 
  
  v <- rnorm(100, mu_v, sigma_v) # generate a vector of normal numbers
  x <- gam0 + gam1 * y + v # use assumed DGP to generate the outcome
  
  gam_hats <- coefficients(lm(x ~ y)) # fit regression model, store coefficients
  gam0_hat[j] <- gam_hats["(Intercept)"] # store intercept in position j of beta0_hat.
  gam1_hat[j] <- gam_hats["y"] # store slope in position j of beta1_hat
}
plot(density(v)) 

for (j in 1:r) { 
  
  v <- rnorm(500, mu_v, sigma_v) # generate a vector of normal numbers
  x <- gam0 + gam1 * y + v # use assumed DGP to generate the outcome
  
  gam_hats <- coefficients(lm(x ~ y)) # fit regression model, store coefficients
  gam0_hat[j] <- gam_hats["(Intercept)"] # store intercept in position j of beta0_hat.
  gam1_hat[j] <- gam_hats["y"] # store slope in position j of beta1_hat
}
plot(density(v)) 

for (j in 1:r) { 
  
  v <- rnorm(1000, mu_v, sigma_v) # generate a vector of normal numbers
  x <- gam0 + gam1 * y + v # use assumed DGP to generate the outcome
  
  gam_hats <- coefficients(lm(x ~ y)) # fit regression model, store coefficients
  gam0_hat[j] <- gam_hats["(Intercept)"] # store intercept in position j of beta0_hat.
  gam1_hat[j] <- gam_hats["y"] # store slope in position j of beta1_hat
}

plot(density(v)) 


##We can see that the answers are consistent

###partd

rm(list = ls()); cat("\014") # clear

gam0 <- -0.5
gam1 <- 0.5
mu <- 0
sd <- 1
n <- 500
r <- 100
# different gam1_hat's before running a loop.
gam0_hat <- numeric(r)
gam1_hat <- numeric(r)
avar_hat_gam1 <-numeric(r)

for (j in 1:r){
  y <- rnorm(n,mu,sd)
  v <- rnorm(n,mu,sd)
  x <- gam0 + gam1*y + v  #Data Generating Process
  
  gam1_hat[j] <- cov(x,y)/var(x)
  gam0_hat[j] <- mean(y) - gam1_hat[j]*mean(x)
  hat_sigma_2 <- mean((y - gam0_hat[j] - gam1_hat[j]*x)^2)  #mean of squared residuals
  avar_hat_gam1[j] <- hat_sigma_2/var(x)/n #NOT ROBUST
}

CIlower <- gam1_hat - 1.96*sqrt(avar_hat_gam1)
CIupper <- gam1_hat + 1.96*sqrt(avar_hat_gam1)
reject1 <- as.logical((CIlower>2) + (CIupper<2))

color <- rep(gray(.5),100)
color[reject1[1:100]] <- "black"

plot(0, xlim=c(0.3,0.5), ylim=c(1,100), ylab="Sample Number", xlab="", main="Corect H0")
abline(v=2,lty=2)
for (j in 1:100){
  lines(c(CIlower[j],CIupper[j]), c(j,j), col=color[j], lwd=2)
}

###Number 5

##parta
n<-50
r<-100
p<-0.1
sd<-sqrt(0.1)
phat<-numeric(r)

for (j in 1:r){
  sample <- rnorm(n,p,sd)
  phat[j] <- mean(sample) 
}  
  
plot(density(sqrt(n)*(phat-p)/sd)) #Plots the estimated density of p-hat
curve(dnorm(x,0,1), add =TRUE, lty =2)

###Now p = 0.5
p <- 0.5
sd <- sqrt(0.5)
for (j in 1:r){
  sample <- rnorm(n,p,sd)
  phat[j] <- mean(sample) 
}  

plot(density(sqrt(n)*(phat-p)/sd)) #Plots the estimated density of p-hat
curve(dnorm(x,0,1), add =TRUE, lty =2)


###Partb
p<-0.1
sd<-sqrt(0.1)

contain_true_value <- logical(n)

for (j in 1:r){
  sample <- rbinom(r, 1, p)
  sample_mean <- mean(sample)
  CIlower <- phat - 1.96*(sd)
  CIupper <- phat + 1.96*(sd)
  reject1 <- as.logical((CIlower>2) + (CIupper<2))

  contain_true_value[j] <- CIlower <= p && p <= CIupper
}

proportion_containing_true_value <- mean(contain_true_value)

cat("Proportion of Confidence Intervals Containing p =", p, ":", proportion_containing_true_value, "\n")


p<-0.5
sd<-sqrt(0.5)

contain_true_value <- logical(n)

for (j in 1:r){
  sample <- rbinom(r, 1, p)
  sample_mean <- mean(sample)
  CIlower <- phat - 1.96*(sd)
  CIupper <- phat + 1.96*(sd)
  reject1 <- as.logical((CIlower>2) + (CIupper<2))
  
  contain_true_value[j] <- CIlower <= p && p <= CIupper
}

proportion_containing_true_value <- mean(contain_true_value)

cat("Proportion of Confidence Intervals Containing p =", p, ":", proportion_containing_true_value, "\n")
