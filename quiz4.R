#Consider the space shuttle data ?shuttle in the MASS library. 
#Consider modeling the use of the autolander as the outcome (variable name use).
#Fit a logistic regression model with autolander (variable auto) use (labeled as "auto" 1) versus not (0) as predicted by wind sign (variable wind). 
#Give the estimated odds ratio for autolander use comparing head winds, labeled as "head" in the variable headwind (numerator) to tail winds (denominator). 
?shuttle
library(MASS)
data(shuttle)
head(shuttle)
str(shuttle)
library(plyr)
#we need auto to be 1 and noauto to be 0
sdata<-mutate(shuttle, auto=as.numeric(use == "auto"), headwind=as.numeric(wind=="head"))
str(sdata)
fitb<-glm(auto~headwind, data=sdata, family="binomial")
summary(fitb)
plot(sdata$headwind,fitb$fitted,pch=19,col="blue",xlab="Score",ylab="Prob headwind causes auto to be on")
exp(fitb$coef) #gives ratio, since head =1 this is the numerator
exp(confint(fitb))
anova(fitb,test="Chisq")

#Consider the previous problem. Give the estimated odds ratio for autolander use 
#comparing head winds (numerator) to tail winds (denominator) adjusting for wind strength from the variable magn.
fitb<-glm(auto~headwind+as.numeric(magn), data=sdata, family="binomial")
summary(fitb)
plot(sdata$headwind,fitb$fitted,pch=19,col="blue",xlab="Score",ylab="Prob headwind causes auto to be on adjusted for wind magnitude")
exp(fitb$coef) #gives ratio, since head =1 this is the numerator

#If you fit a logistic regression model to a binary variable, for example use of the autolander, 
#then fit a logistic regression model for one minus the outcome (not using the autolander) what happens to the coefficients?
fitb<-glm(I(1-auto)~headwind+as.numeric(magn), data=sdata, family="binomial")
summary(fitb)

#Consider the insect spray data InsectSprays. Fit a Poisson model using spray as a factor level. 
#Report the estimated relative rate comapring spray A (numerator) to spray B (denominator).
require(datasets)
data(InsectSprays)
require(stats)
head(InsectSprays)
fit<-glm(count ~ spray, data = InsectSprays,family="poisson")
summary(fit)
confint(fit)
fit<-glm(count ~ spray -1, data = InsectSprays,family="poisson")
summary(fit)
spray2 <- relevel(InsectSprays$spray, "B")
fit<-glm(count ~ spray2, data = InsectSprays,family="poisson")
summary(fit)
confint(fit)
exp(fit$coef)

#Consider a Poisson glm with an offset, t. So, for example, 
#a model of the form glm(count ~ x + offset(t), family = poisson) 
#where x is a factor variable comparing a treatment (1) to a control (0) and t is the natural log of a monitoring time. 
#What is impact of the coefficient for x if we fit the model glm(count ~ x + offset(t2), family = poisson) 
#where t2 <- log(10) + t? In other words, what happens to the coefficients if we change the units of the offset variable. 
#(Note, adding log(10) on the log scale is multiplying by 10 on the original scale.)
fit<-glm(count ~ spray,offset=rep(log(10)+1, 72), data = InsectSprays,family="poisson")
summary(fit)

#Consider the data
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
#Using a knot point at 0, fit a linear model that looks like a hockey stick with two lines meeting at x=0.
#Include an intercept term, x and the knot point term. What is the estimated slope of the line after 0?
plot(y~x)
knots <- c(0); 
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1, x, splineTerms)
fit<-lm(y ~ xMat - 1)
yhat <- predict(fit)
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)
summary(fit)
