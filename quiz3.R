#Consider the mtcars data set. Fit a model with mpg as the outcome 
#that includes number of cylinders as a factor variable and weight as confounder. 
#Give the adjusted estimate for the expected change in mpg comparing 8 cylinders to 4.
data(mtcars)
summary(mtcars)
fit_cyl <- lm(mpg~cyl, data=mtcars)
fit_cyl_wt <- lm(mpg~cyl+wt, data=mtcars)
anova(fit_cyl, fit_cyl_wt)
plot(mpg~cyl, data=mtcars, type="n", col=cyl)
points(mpg~cyl, data=mtcars[mtcars$cyl==8], col="salmon")
points(mpg~cyl, data=mtcars[mtcars$cyl==4], col="blue")
abline(fit_cyl,col="salmon")
abline(fit_cyl_wt,col="blue")
summary(fit_cyl)$coef
summary(fit_cyl_wt)$coef
#from 8 to 4
4*summary(fit_cyl_wt)$coef[2,1]

#Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders 
#as a factor variable and weight as a possible confounding variable. Compare the effect of 8 versus 4 cylinders
#on mpg for the adjusted and unadjusted by weight models. Here, adjusted means including the weight variable as
#a term in the regression model and unadjusted means the model without weight included. What can be said about 
#the effect comparing 8 and 4 cylinders after looking at models with and without weight included?.
fit_cyl <- lm(mpg~factor(cyl), data=mtcars)
fit_cyl_wt <- lm(mpg~factor(cyl)+wt, data=mtcars)
summary(fit_cyl)$coef
summary(fit_cyl_wt)$coef
#less impact when it is adjusted.


#Consider the  mtcars  data set. Fit a model with mpg as the outcome that considers number of cylinders
#as a factor variable and weight as confounder. Now fit a second model with mpg as the outcome model that 
#considers the interaction between number of cylinders (as a factor variable) and weight. Give the P-value 
#for the likelihood ratio test comparing the two models and suggest a model using 0.05 as a type I error rate 
#significance benchmark
fit_cyl_wt_int <- lm(mpg~cyl+wt+cyl:wt, data=mtcars)
summary(fit_cyl_wt_int)


#Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor
#variable and weight inlcuded in the model as
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
#How is the wt coefficient interpretted?


#Consider the following data set
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
#Give the hat diagonal for the most influential point
fit<-lm(y~x)
summary(fit)
plot(y~x)
abline(fit)
z<-hatvalues(fit)
plot(z~x)

#Consider the following data set
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
#Give the slope dfbeta for the point with the highest hat value.
plot(y~x)
fit<-lm(y~x)
z<-dfbetas(fit)
z

#Consider a regression relationship between Y and X with and without adjustment for a third variable Z.
#Which of the following is true about comparing the regression coefficient between Y and X with and without adjustment for Z.
