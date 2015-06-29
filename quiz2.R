#Consider the following data with x as the predictor and y as as the outcome. 
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
#Give a P-value for the two sided hypothesis test of whether beta1 from a linear regression model is 0 or not.
plot(x,y)
fit<-lm(y~x)
coef(fit)
summary(fit)
abline(fit, lwd=2)

#Consider the previous problem, give the estimate of the residual standard deviation
e<- resid(fit)
sd(e)
plot(x,abs(e))
abline(h=sd(e))
summary(fit)

#In the mtcars data set, fit a linear regression model of weight (predictor) on mpg (outcome)
#Get a 95% confidence interval for the expected mpg at the average weight. What is the lower endpoint?
data(mtcars)
summary(mtcars)
x<-mtcars$wt
y<-mtcars$mpg
fit<-lm(y~x)
summary(fit)
newx <- c(mean(x))
predict(fit, newdata=data.frame(x=newx))
data.frame(predict(fit, newdata = data.frame(x=newx),interval = ("confidence")))

#Refer to the previous question. Read the help file for mtcars. What is the weight coefficient interpreted as?
?mtcars

#Consider again the mtcars data set and a linear regression model with mpg as predicted by weight (1,000 lbs).
#A new car is coming weighing 3000 pounds. Construct a 95% prediction interval for its mpg. What is the upper endpoint?
newx <- c(3)
data.frame(predict(fit, newdata = data.frame(x=newx),interval = ("prediction")))

#Consider again the mtcars data set and a linear regression model with mpg as predicted by weight (in 1,000 lbs).
#A "short" ton is defined as 2,000 lbs. Construct a 95% confidence interval for the expected change in mpg per 1 short ton increase in weight.
#Give the lower endpoint.
sumCoef <- summary(fit)$coefficients
(sumCoef[2,1] + -1 * qt(.975, df = fit$df) * sumCoef[2, 2]) * 2
