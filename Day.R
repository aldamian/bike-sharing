########## Cleaning all ##########

rm(list = ls())


########## Libraries ##########
library(ggplot2)
library(leaps)
#install.packages("Hmisc")
library("Hmisc")
#install.packages("corrplot")
library(corrplot)
library(Metrics)



########## Uploading data set ##########
day = read.csv("day.csv")
View(day)
attach(day)

########### Histogram cnt, registered and casual ##########
par(mfrow = c(2,3))
hist(cnt, xlab = "", ylab = "", main = "Total dailiy users", breaks = 30 )
hist(registered, xlab = "", ylab = "", main = "Registered dailiy users", breaks = 30 )
hist(casual, xlab = "", ylab = "", main = "Casual dailiy users", breaks = 30 )

########## Scatter plot ##########
day <- day[,-c(1,2)]
pairs(day)

########## Correlation matrix ##########
cor(day)
View(cor(day))
par(mfrow = c(1,1))
corrplot(cor(day), method = "number")

#notes: cnt (season, year, month, weathersit, temp, atemp)
#notes: season,month .. weaherist,humidity .. 
#notes: interesting that workingdy impact negatively on casual but positively on registered


########## DIVIDING TRAINING AND TESTING DATA ##########
# More or less 80/20
set.seed(1)
day [c("season", "yr", "mnth", "holiday", "weekday", "workingday", "weathersit" )] <- lapply(day[c("season", "yr", "mnth", "holiday", "weekday", "workingday", "weathersit")], factor)
rand = sample(1:731, 600, replace = FALSE)
train1 <- day[rand,]
test1 <- day[-rand,] 



########## LINEAR REGRESSION ##########


########## SIMPLE LINEAR REGRESSION ##########


# Temperature linear
plot(temp, cnt) #seems there could be a linear relationship
lines(lowess(temp, cnt))
lm.temp = lm(cnt ~ temp, data = train1)
summary(lm.temp)
abline(lm.temp, col = "red")
confint(lm.temp) #confidence interval coefficients estimates
predict(lm.temp,train1, interval = "confidence")
predict(lm.temp, train1, interval = "prediction")#weird that gives negative counts
sqrt(mean((train1$cnt - predict(lm.temp, train1))^2))

plot(train1$cnt, predict(lm.temp, train1))
plot(lm.temp$fitted.values, lm.temp$residuals)# not linear #not Homoscedasticity 
qqnorm(rstandard(lm.temp), pch = 1, ylab = "Standarized residuals")
qqline(rstandard(lm.temp), col = "steelblue", lwd = 2)

#For test, prediction
sqrt(mean((test1$cnt - predict(lm.temp, test1))^2))
plot(test1$cnt, predict(lm.temp, test1))
resultsTemp = data.frame(test1[[14]],predict(lm.temp, test1)) #not for the amount, but for pattern prediction can be used


# Temperature quadratic ## not good
plot((temp)^2, cnt) #not linear
plot(temp, cnt)
lm.temp2 = lm(cnt ~ poly(temp,2, raw = TRUE), data = train1)
lines(train1$temp, (predict(lm.temp2)), col="red")
summary(lm.temp2)
confint(lm.temp2)
predict(lm.temp2, train1)
sqrt(mean((train1$cnt - predict(lm.temp2, train1))^2)) #lower than with simple, also with other seed

plot(train1$cnt, predict(lm.temp2, train1))
plot(lm.temp2$fitted.values, lm.temp2$residuals)# not linear #not Homoscedasticity
qqnorm(rstandard(lm.temp2), pch = 1, ylab = "Standarized residuals")
qqline(rstandard(lm.temp2), col = "steelblue", lwd = 2)

#For test, prediction
sqrt(mean((test1$cnt - predict(lm.temp2, test1))^2))
plot(test1$cnt, predict(lm.temp2, test1))
resultsTemp2 = data.frame(test1[[14]], predict(lm.temp2, test1)) #number 34 and 24 wrong, because the temp is 0.09 very different from normal values


# Temperature cubic ## not good
plot((temp)^3, cnt) #not linear at all
plot(temp, cnt)
lm.temp3 = lm(cnt ~ poly(temp,3, raw = TRUE), data = train1)
lines(train1$temp, (predict(lm.temp3)), col="red")
summary(lm.temp3)
confint(lm.temp3)
predict(lm.temp3, train1)
sqrt(mean((train1$cnt - predict(lm.temp3, train1))^2)) #lower than with simple, also with other seed

plot(train1$cnt, predict(lm.temp2, train1))
plot(lm.temp2$fitted.values, lm.temp2$residuals)
qqnorm(rstandard(lm.temp3), pch = 1, ylab = "Standarized residuals")
qqline(rstandard(lm.temp3), col = "steelblue", lwd = 2)


#For test, prediction
sqrt(mean((test1$cnt - predict(lm.temp3, test1))^2))
plot(test1$cnt, predict(lm.temp3, test1))
resultsTemp3 = data.frame(test1[[14]], predict(lm.temp3, test1))

# Temperature logaritmic ## improves a bit compared to the lm.temp
plot(log(temp), cnt)
lm.templog = lm(cnt ~ log(temp), data = train1)
summary(lm.templog)
abline(lm.templog, col = "red")
predict(lm.templog, test1) ## weird that predicts some negative counts!
mean((test1$cnt - predict(lm.templog, test1))^2) 
plot(test1$cnt,predict(lm.templog, test1), xlim = c(500,9000),ylim = c(500,7000))
plot(lm.templog$fitted.values, lm.templog$residuals)

# Temperature squared root ## a bit worse than log
plot(sqrt(temp), cnt)
lines(lowess(sqrt(temp), cnt))
lm.tempsqrt = lm(cnt ~ sqrt(temp), data = train1)
summary(lm.tempsqrt)
abline(lm.tempsqrt, col = "red")
predict(lm.tempsqrt, test1) 
mean((test1$cnt - predict(lm.tempsqrt, test1))^2) 
plot(test1$cnt,predict(lm.tempsqrt, test1))
plot(lm.tempsqrt$fitted.values, lm.tempsqrt$residuals)





# Temperature feeling atemp linear
plot(atemp, cnt, ylab = "Total users", xlab = "Feeling temperature")
lines(lowess(atemp, cnt)) #shows best approximation line
lm.atemp = lm(cnt ~ atemp, data = train1)
summary(lm.atemp)
abline(lm.atemp, col = "red")
confint(lm.atemp) #confidence interval coefficients estimates
predict(lm.atemp,train1, interval = "confidence")
predict(lm.atemp, train1, interval = "prediction")
sqrt(mean((train1$cnt - predict(lm.atemp, train1))^2))

plot(train1$cnt, predict(lm.atemp, train1), xlab = "Real", ylab = "Predicted")
lines(train1$cnt, train1$cnt, col = "red")
plot(lm.atemp$fitted.values, lm.atemp$residuals, xlab = "Fitted values", ylab = "Residuals") 
abline(h=0, col = "red", lwd = 3)
qqnorm(rstandard(lm.atemp), pch = 1, ylab = "Standarized residuals")
qqline(rstandard(lm.atemp), col = "steelblue", lwd = 2)

#For test, prediction
sqrt(mean((test1$cnt - predict(lm.atemp, test1))^2))
plot(test1$cnt, predict(lm.atemp, test1), xlab = "Real", ylab = "Predicted")
resultsaTemp = data.frame(test1[[14]],predict(lm.atemp, test1)) 
plot(resultsaTemp$predict.lm.atemp..test1., xlab = "Test dataset point", ylab = "Predicted values for cnt", col = test1$yr)
plot(resultsaTemp$test1..14.., xlab = "Test dataset point", ylab = "Real values for cnt in test dataset", col = test1$yr)

# Temperature feeling quadratic 
plot(atemp, cnt)
lm.atemp2 = lm(cnt ~ poly(atemp,2, raw = TRUE), data = train1)
lines(smooth.spline(train1$atemp, predict(lm.atemp2)), col="red")
summary(lm.atemp2)
confint(lm.atemp2)
predict(lm.atemp2, train1)
sqrt(mean((train1$cnt - predict(lm.atemp2, train1))^2)) #lower than with simple, also with other seed

plot(train1$cnt, predict(lm.atemp2, train1), xlab = "Real", ylab = "Predicted")
plot(lm.atemp2$fitted.values, lm.atemp2$residuals, xlab = "Fitted", ylab = "Residuals")# not linear #not Homoscedasticity
qqnorm(rstandard(lm.atemp2), pch = 1, ylab = "Standarized residuals")
qqline(rstandard(lm.atemp2), col = "steelblue", lwd = 2)

#For test, prediction
sqrt(mean((test1$cnt - predict(lm.atemp2, test1))^2))
plot(test1$cnt, predict(lm.atemp2, test1))
resultsaTemp2 = data.frame(test1[[14]], predict(lm.atemp2, test1))


# Temperature feeling cubic 
plot(atemp, cnt, xlab = "Feeling temperature, atemp", ylab = "Total users, cnt")
lm.atemp3 = lm(cnt ~ poly(atemp,3, raw = TRUE), data = train1)
lines(smooth.spline(train1$atemp, predict(lm.atemp3)), col="red", lwd = 3)
summary(lm.atemp3)
confint(lm.atemp3)
predict(lm.atemp3, train1)
sqrt(mean((train1$cnt - predict(lm.atemp3, train1))^2)) #lower than with simple, also with other seed

plot(train1$cnt, predict(lm.atemp3, train1), xlab = "Real", ylab = "Predicted")
plot(lm.atemp2$fitted.values, lm.atemp3$residuals, xlab = "Fitted", ylab = "Residuals")
qqnorm(rstandard(lm.atemp3), pch = 1, ylab = "Standarized residuals")
qqline(rstandard(lm.atemp3), col = "steelblue", lwd = 2)

#For test, prediction
sqrt(mean((test1$cnt - predict(lm.atemp3, test1))^2))
plot(test1$cnt, predict(lm.atemp3, test1))
resultsaTemp3 = data.frame(test1[[14]], predict(lm.atemp3, test1))
plot(resultsaTemp3$predict.lm.atemp3..test1., col = test1$yr)


# Temperature feeling logarithmic 
plot(log(atemp), cnt)
lm.atemplog = lm(cnt ~ log(atemp), data = train1)
lines(train1$atemp, (predict(lm.atemplog)), col="red")
summary(lm.atemplog)
confint(lm.atemplog)
predict(lm.atemplog, train1)
sqrt(mean((train1$cnt - predict(lm.atemplog, train1))^2)) #lower than with simple, also with other seed

plot(train1$cnt, predict(lm.atemplog, train1), xlab = "Real", ylab = "Predicted")
plot(lm.atemplog$fitted.values, lm.atemplog$residuals, xlab = "Fitted", ylab = "Residuals")# not linear #not Homoscedasticity
qqnorm(rstandard(lm.atemplog), pch = 1, ylab = "Standarized residuals")
qqline(rstandard(lm.atemplog), col = "steelblue", lwd = 2)

#For test, prediction
sqrt(mean((test1$cnt - predict(lm.atemplog, test1))^2))
plot(test1$cnt, predict(lm.atemplog, test1))
resultsatemplog = data.frame(test1[[14]], predict(lm.atemplog, test1))

# Temperature feeling squared root  
plot(sqrt(atemp), cnt)
lm.atempsqrt = lm(cnt ~ sqrt(atemp), data = train1)
lines(train1$atemp, (predict(lm.atempsqrt)), col="red")
summary(lm.atempsqrt)
confint(lm.atempsqrt)
predict(lm.atempsqrt, train1)
sqrt(mean((train1$cnt - predict(lm.atempsqrt, train1))^2)) #lower than with simple, also with other seed

plot(train1$cnt, predict(lm.atempsqrt, train1), xlab = "Real", ylab = "Predicted")
plot(lm.atempsqrt$fitted.values, lm.atempsqrt$residuals, xlab = "Fitted", ylab = "Residuals")# not linear #not Homoscedasticity
qqnorm(rstandard(lm.atempsqrt), pch = 1, ylab = "Standarized residuals")
qqline(rstandard(lm.atempsqrt), col = "steelblue", lwd = 2)

#For test, prediction
sqrt(mean((test1$cnt - predict(lm.atempsqrt, test1))^2))
plot(test1$cnt, predict(lm.atempsqrt, test1))
resultsatempsqrt = data.frame(test1[[14]], predict(lm.atempsqrt, test1))

# Windspeed linear
plot(windspeed, cnt, ylab = "Total rents", xlab = "Wind speed") #seems there could be a linear relationship
lines(lowess(windspeed, cnt))
lm.windspeed = lm(cnt ~ windspeed, data = train1)
summary(lm.windspeed)
abline(lm.windspeed, col = "red")
confint(lm.windspeed) #confidence interval coefficients estimates
predict(lm.windspeed,train1, interval = "confidence")
predict(lm.windspeed, train1, interval = "prediction")
sqrt(mean((train1$cnt - predict(lm.windspeed, train1))^2))

plot(train1$cnt, predict(lm.windspeed, train1), xlab = "Real", ylab = "Predicted")
plot(lm.windspeed$fitted.values, lm.windspeed$residuals, xlab = "Fitted values", ylab = "Residuals")# not linear #not Homoscedasticity 
qqnorm(rstandard(lm.windspeed), pch = 1, ylab = "Standarized residuals")
qqline(rstandard(lm.windspeed), col = "steelblue", lwd = 2)

#For test, prediction
sqrt(mean((test1$cnt - predict(lm.windspeed, test1))^2))
plot(test1$cnt, predict(lm.windspeed, test1), xlab = "Real", ylab = "Predicted")
resultswindspeed = data.frame(test1[[14]],predict(lm.windspeed, test1)) #not for the amount, but for pattern prediction can be used

# windspeede quadratic 
plot((windspeed), cnt)
lm.windspeed2 = lm(cnt ~ poly(windspeed,2, raw = TRUE), data = train1)
lines(train1$windspeed, (predict(lm.windspeed2)), col="red")
summary(lm.windspeed2)
confint(lm.windspeed2)
predict(lm.windspeed2, train1)
sqrt(mean((train1$cnt - predict(lm.windspeed2, train1))^2)) #lower than with simple, also with other seed

plot(train1$cnt, predict(lm.windspeed2, train1), xlab = "Real", ylab = "Predicted")
plot(lm.windspeed2$fitted.values, lm.windspeed2$residuals, xlab = "Fitted", ylab = "Residuals")# not linear #not Homoscedasticity
qqnorm(rstandard(lm.windspeed2), pch = 1, ylab = "Standarized residuals")
qqline(rstandard(lm.windspeed2), col = "steelblue", lwd = 2)

#For test, prediction
sqrt(mean((test1$cnt - predict(lm.windspeed2, test1))^2))
plot(test1$cnt, predict(lm.windspeed2, test1))
resultswindspeed2 = data.frame(test1[[14]], predict(lm.windspeed2, test1))


# windspeed cubic 
plot((windspeed), cnt)
lm.windspeed3 = lm(cnt ~ poly(windspeed,3, raw = TRUE), data = train1)
lines(train1$windspeed, (predict(lm.windspeed3)), col="red")
summary(lm.windspeed3)
confint(lm.windspeed3)
predict(lm.windspeed3, train1)
sqrt(mean((train1$cnt - predict(lm.windspeed3, train1))^2)) #lower than with simple, also with other seed

plot(train1$cnt, predict(lm.windspeed3, train1), xlab = "Real", ylab = "Predicted")
plot(lm.windspeed3$fitted.values, lm.windspeed3$residuals, xlab = "Fitted", ylab = "Residuals")# not linear #not Homoscedasticity
qqnorm(rstandard(lm.windspeed3), pch = 1, ylab = "Standarized residuals")
qqline(rstandard(lm.windspeed3), col = "steelblue", lwd = 2)

#For test, prediction
sqrt(mean((test1$cnt - predict(lm.windspeed3, test1))^2))
plot(test1$cnt, predict(lm.windspeed3, test1))
resultswindspeed3 = data.frame(test1[[14]], predict(lm.windspeed3, test1))


# windspeed logaritmic 
plot(log(windspeed), cnt)
lm.windspeedlog = lm(cnt ~ log(windspeed), data = train1)
lines(train1$windspeed, (predict(lm.windspeedlog)), col="red")
summary(lm.windspeedlog)
confint(lm.windspeedlog)
predict(lm.windspeedlog, train1)
sqrt(mean((train1$cnt - predict(lm.windspeedlog, train1))^2)) #lower than with simple, also with other seed


#For test, prediction
sqrt(mean((test1$cnt - predict(lm.windspeedlog, test1))^2))
plot(test1$cnt, predict(lm.windspeedlog, test1))
resultswindspeedlog = data.frame(test1[[14]], predict(lm.windspeedlog, test1))

# windspeed squared error  ## a bit worse than logaritmic
plot(sqrt(windspeed), cnt)
lm.windspeedsqrt = lm(cnt ~ sqrt(windspeed), data = train1)
lines(train1$windspeed, (predict(lm.windspeedsqrt)), col="red")
summary(lm.windspeedsqrt)
confint(lm.windspeedsqrt)
predict(lm.windspeedsqrt, train1)
sqrt(mean((train1$cnt - predict(lm.windspeedsqrt, train1))^2)) #lower than with simple, also with other seed


#For test, prediction
sqrt(mean((test1$cnt - predict(lm.windspeedsqrt, test1))^2))
plot(test1$cnt, predict(lm.windspeedsqrt, test1))
resultswindspeedsqrt = data.frame(test1[[14]], predict(lm.windspeedsqrt, test1))

# Season ## not very good, does not make sense only to take the season. Only fro understanding better patterns
contrasts(season)
plot(season, cnt)
lm.season = lm(cnt ~ season, data = train1)
summary(lm.season)
predict(lm.season, test1) 

# Month
contrasts(mnth)
boxplot(cnt ~ mnth)
lm.mnth1 = lm(cnt ~ mnth, data = train1)
sum <- summary(lm.mnth1)
predict(lm.mnth1, test1)
mean((test1$cnt - predict(lm.mnth1, test1))^2)

# Humidity ## improves with ^2 but still bad
plot(hum*hum, cnt)
lm.hum = lm(cnt ~ poly(hum,2), data = train1)
summary(lm.hum)
abline(lm.hum, col = "red") ## weird wanring message
predict(lm.hum, test1)
mean((test1$cnt - predict(lm.hum, test1))^2)

# Weathersit
weathersit <- as.factor(weathersit)
contrasts(weathersit)
boxplot(cnt ~ weathersit)
weathersit = lm(cnt ~ weathersit)
summary(weathersit)






########## MULTIPLE LINEAR REGRESSION ##########

## BEST SUBSET SELECTION 
#day <- day[, -c(12,13)]
str(day)
View(day)
day [c("season", "yr", "mnth", "holiday", "weekday", "workingday", "weathersit" )] <- lapply(day[c("season", "yr", "mnth", "holiday", "weekday", "workingday", "weathersit")], factor)


fit.full <- regsubsets(cnt ~., data = day, nvmax = 28)
summary <- summary(fit.full)
summary
summary$rsq
names(summary)
plot(fit.full,scale = "r2")

par(mfrow = c(1,1))
plot(summary$rsq, xlab = "Number of variables", ylab = "RSQ", type = "l")
which.max(summary$rsq)
points(28,summary$rsq[28], col = "red", cex = 2, pch = 20 )

plot(summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R2", type = "l")
which.max(summary$adjr2)
points(23,summary$rsq[23], col = "red", cex = 2, pch = 20 )

plot(summary$cp, xlab = "Number of variables", ylab = "CP", type = "l")
which.min(summary$cp)
points(19,summary$cp[19], col = "red", cex = 2, pch = 20 )

plot(summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
which.min(summary$bic)
points(15,summary$bic[15], col = "red", cex = 2, pch = 20 )



## BACKWARDS STEPWISE
fit.back <- regsubsets(cnt ~., data = day, nvmax = 28, method = "backward")
summaryBC <- summary(fit.back)
summaryBC
names(summaryBC)
plot(fit.back,scale = "r2")

plot(summaryBC$rsq, xlab = "Number of variables", ylab = "RSQ", type = "l")
which.max(summaryBC$rsq)
points(28,summaryBC$rsq[28], col = "red", cex = 2, pch = 20 )

plot(summaryBC$adjr2, xlab = "Number of variables", ylab = "Adjusted R2", type = "l")
which.max(summaryBC$adjr2)
points(25,summaryBC$rsq[25], col = "red", cex = 2, pch = 20 )

plot(summaryBC$cp, xlab = "Number of variables", ylab = "CP", type = "l")
which.min(summaryBC$cp)
points(23,summaryBC$cp[23], col = "red", cex = 2, pch = 20 )

plot(summaryBC$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
which.min(summaryBC$bic)
points(13,summaryBC$bic[13], col = "red", cex = 2, pch = 20 )


## FORWARD STEPWISE

fit.for <- regsubsets(cnt ~., data = day, nvmax = 28, method = "forward")
summaryFW <- summary(fit.for)
summaryFW
names(summaryFW)
plot(fit.for,scale = "r2")

plot(summaryFW$rsq, xlab = "Number of variables", ylab = "RSQ", type = "l")
which.max(summaryFW$rsq)
points(28,summaryFW$rsq[28], col = "red", cex = 2, pch = 20 )

plot(summaryFW$adjr2, xlab = "Number of variables", ylab = "Adjusted R2", type = "l")
which.max(summaryFW$adjr2)
points(22,summaryBC$rsq[22], col = "red", cex = 2, pch = 20 )

plot(summaryFW$cp, xlab = "Number of variables", ylab = "CP", type = "l")
which.min(summaryFW$cp)
points(19,summaryFW$cp[19], col = "red", cex = 2, pch = 20 )

plot(summaryFW$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
which.min(summaryFW$bic)
points(18,summaryBC$bic[18], col = "red", cex = 2, pch = 20 )


##### CROSS VALIDATION #####
# Predict function for reagsubstets
predict.regsubsets = function(object,newdata,id,...){# define function for prediction
  form=as.formula(object$call[[2]])# object: variable passed to the fct regfit; call is a list of at least two elements
  mat=model.matrix(form,newdata)# define matrix
  coefi=coef(object,id=id)# define coefficients
  xvars=names(coefi)# display names of the predictors
  mat[,xvars]%*%coefi# display matrix with the coefficients
}

## VALIDATON SET APPROACH
fit.best = regsubsets(cnt~., data=train1,nvmax=28)# perform best subset selection on training data 
test.mat=model.matrix(cnt~.,data=test1)# make a model matrix from the test data

val.errors=rep(NA,28)# replicate for all val.errors from NA to 28
for(i in 1:28){# for all i from 1 to 28
  coefi=coef(fit.best,id=i)# extract the coefficients from regfit.best with increasing nr. of pred.
  pred=test.mat[,names(coefi)]%*%coefi# get the prediction of the coefficients using test.mat
  val.errors[i]=mean((test1$cnt-pred)^2)# compute the test MSE
}

val.errors# display all test MSEs
which.min(val.errors)# give the one with the minimal test MSE
coef(fit.best, 16)
plot(val.errors, xlab = "Number of predictors", ylab = "Test MSE")
lines(val.errors)


## K-FOLD
k <- 10 
set.seed(1)
folds <- sample(1:k, nrow(day), replace = TRUE)
cv.errors <- matrix(NA, k, 28, dimnames = list(NULL, paste(1:28))) #null matrix to store results

for(j in 1:k)  {# define loop for best subset selection on training data
  best.fit = regsubsets(cnt ~.,data = day[folds!=j,],nvmax=28)# best subset selection on training data set
  for(i in 1:28){# define loop for best subset selection on test data 
    pred = predict(best.fit,day[folds==j,],id=i)# prediction/model testing of best subset selection
     cv.errors[j,i] = mean((day$cnt[folds==j]-pred)^2)# give cross-val. 10x29-matrix as test MSE
  }
}

mean.cv.errors = apply(cv.errors,2,mean)
which.min(mean.cv.errors)
plot(mean.cv.errors, xlab = "Number of predictors", ylab = "Test MSE")
lines(mean.cv.errors)

coef(fit.full,16)
coef(fit.for,16)
coef(fit.back,16) 



####### SELECTING SOME MODELS FOR MODEL ASSESSMENT #######

#5
coef(fit.full,5)
coef(fit.for,5)
coef(fit.back,5)

#16
coef(fit.full,16)
coef(fit.for,16)
coef(fit.back,16)

#28
coef(fit.full,28)
coef(fit.for,28)
coef(fit.back,28)
