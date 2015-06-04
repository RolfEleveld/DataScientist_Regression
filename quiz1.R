#Consider the data set given below
x <- c(0.18, -1.54, 0.42, 0.95)
#And weights given by
w <- c(2, 1, 3, 1)
#Give the value of mu that minimizes the least squares equation sum(wi(xi-mu)^2)
mySum <- function(mu){
    sumvalue = 0
    for(i in 1:4){
        sumvalue = sumvalue + w[i]*(x[i] - mu)^2
    }
    sumvalue
}
mu <- seq(0.0025, 1.077, by = 0.005)
plot(mu, mySum(mu))

#Consider the following data set
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
#Fit the regression through the origin and get the slope treating y as the outcome and x as the regressor. (Hint, do not center the data since we want regression through the origin, not through the means of the data.)
summary(lm(y~x-1))
mySum <- function(beta1){
    sumvalue=0
    for(i in 1:length(x)){
        sumvalue=sumvalue + (y[i] - beta1 * x[i])^2
    }
    sumvalue
}
beta1<-seq(-1.713, 0.8263, by=0.005)
plot(beta1, mySum(beta1))

#Consider data with an outcome (Y) and a predictor (X).
#The standard deviation of the predictor is one half that of the outcome.
#The correlation between the two variables is .5.
#What value would the slope coefficient for the regression model with Y as the outcome and X as the predictor?
beta0<-cor(X,Y)*Sy/Sx 
#cor(X,Y) = .5
#2Sx=Sy
#beta0=.5*2Sx/Sx = 1
beta1 <- mean(Y) -beta0*mean(X)
#cov(X,Y) = cor(x,Y)*Sx*Sy=.5*2Sx*Sx = Sx^2

#Do data(mtcars) from the datasets package and fit the regression model with mpg as the outcome and weight as the predictor. Give the slope coefficient
data(mtcars)
summary(lm(mtcars$mpg~mtcars$wt))

#Students were given two hard tests and scores were normalized to have empirical mean 0 and variance 1.
#The correlation between the scores on the two tests was 0.4.
#What would be the expected score on Quiz 2 for a student who had a normalized score of 1.5 on Quiz 1?
#cor(X,Y) = 0.4
0.4*1.5

#Consider the data given by the following
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
#What is the value of the first measurement if x were normalized (to have mean 0 and variance 1)?
(x-mean(x))/sd(x)


#Consider the following data set (used above as well). What is the intercept for fitting the model with x as the predictor and y as the outcome?
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
summary(lm(y~x))

#You know that both the predictor and response have mean 0. 
#What can be said about the intercept when you fit a linear regression?

#Consider the data given by
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
#What value minimizes the sum of the squared distances between these points and itself?
mySum <- function(point){
    sumvalue=0
    for(i in 1:length(x)){
        sumvalue=sumvalue + (point - x[i])^2
    }
    sumvalue
}
points<-seq(0.2, 1.0, by=0.05)
plot(points, mySum(points))
