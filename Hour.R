########## Cleaning all ##########

rm(list = ls())


########## Libraries ##########


library(ggplot2)
library(leaps)
#install.packages("Hmisc")
library("Hmisc")
#install.packages("corrplot")
library(corrplot)
library(dplyr)




########## Uploading and investigating data set ##########

hour = read.csv("hour.csv")
View(hour)
attach(hour)
names(hour)
dim(hour)
sum(is.na(hour$cnt)) #no missing observations


########### Histogram cnt, registered and casual ##########

par(mfrow = c(1,3))
hist(cnt, xlab = "", ylab = "", main = "Total hourly users", breaks = 30 )
hist(registered, xlab = "", ylab = "", main = "Registered hourly users", breaks = 30 )
hist(casual, xlab = "", ylab = "", main = "Casual hourly users", breaks = 30 )


########## Scatter plot and correlation matrix ##########
hour = hour[-c(1,2)]
pairs(hour)
par(mfrow = c(1,1))
cor(hour)
View(cor(hour))
corrplot(cor(hour), method = "number")


########## DEVIDING TRAINING AND TESTING DATA ##########
# More or less 80/20
set.seed(1)
hour [c("season", "yr", "mnth", "hr", "holiday", "weekday", "workingday", "weathersit" )] <- lapply(hour[c("season", "yr", "mnth", "hr", "holiday", "weekday", "workingday", "weathersit")], factor)
rand = sample(1:17379, 13000, replace = FALSE)
train2 <- hour[rand,]
test2 <- hour[-rand,] 

########## LINEAR REGRESSION ##########


########## SIMPLE LINEAR REGRESSION ##########
# Temperature feeling
#boxplot(cnt ~ atemp)
plot(atemp, cnt, xlab = "Feeling temperature, atemp", ylab = "Totral users, cnt") 
lm.atemp = lm(cnt ~ atemp, data = train2)
summary(lm.atemp)
predict(lm.atemp, test2) 
mean((test2$cnt - predict(lm.atemp, test2))^2)
plot(test2$cnt,predict(lm.atemp, test2))

# Temperature feeling squared
plot(atemp^2, cnt) # not linear at all...
lm.atemp2 = lm(cnt ~ poly(atemp,2), data = train2)
summary(lm.atemp2)
predict(lm.atemp, test2) 
mean((test2$cnt - predict(lm.atemp, test2))^2)
plot(test2$cnt,predict(lm.atemp, test2))

# Temperature feeling squared root
plot(sqrt(atemp), cnt) # not linear at all...
lm.atempsqrt = lm(cnt ~ poly(atemp,2), data = train2)
summary(lm.atempsqrt)
predict(lm.atempsqrt, test2) 
mean((test2$cnt - predict(lm.atempsqrt, test2))^2)
plot(test2$cnt,predict(lm.atempsqrt, test2))

# Temperature feeling exp
plot(exp(atemp), cnt) # not linear at all...
lm.atempsqrt = lm(cnt ~ poly(atemp,2), data = train2)
summary(lm.atempsqrt)
predict(lm.atempsqrt, test2) 
mean((test2$cnt - predict(lm.atempsqrt, test2))^2)
plot(test2$cnt,predict(lm.atempsqrt, test2))

# Temperature feeling squared
plot(log(atemp), cnt) # not linear at all...
lm.atemp2 = lm(cnt ~ poly(atemp,2), data = train2)
summary(lm.atemp2)
predict(lm.atemp, test2) 
mean((test2$cnt - predict(lm.atemp, test2))^2)
plot(test2$cnt,predict(lm.atemp, test2))

#### what if we plot only for each hour, separately?
plot(hr,cnt)

holiday = hour %>% filter(holiday == 1)
View(holiday)
plot(holiday$hr, holiday$cnt)

weekend = hour %>% filter(weekday == 0 | weekday == 6)
View(weekend)
plot(weekend$hr, weekend$cnt)

hour0 <- hour %>% filter(hr == 0)
View(hour0)
plot(hour0$atemp, hour0$cnt, col = hour0$weekday )
legend("topright", legend = c("0", "1", "2", "3", "4", "5", "6"), col = hour0$weekday, pch=rep(c(16),each=4),bty="n",cex=0.7,pt.cex=0.7)


hour1 <- hour %>% filter(hr == 1)
View(hour1)
plot(hour1$atemp, hour1$cnt, col = hour1$weekday, lwd =2, xlab = "Feeling temperature for hour = 1", ylab = "Total users for hour = 1")
legend("topright", legend = c("6", "0", "1", "2", "3", "4", "5"), col = hour1$weekday, pch=rep(c(16),each=4),bty="n",cex=0.7,pt.cex=0.7)

hour2<- hour %>% filter(hr == 2)
View(hour2)
plot(hour2$atemp, hour2$cnt, col = hour2$weekday, lwd =2, xlab = "Feeling temperature for hour = 4", ylab = "Total users for hour = 4")
legend("topright", legend = c("6","0", "1", "2", "3", "4", "5"), col = hour3$weekday, pch=rep(c(16),each=4),bty="n",cex=0.7,pt.cex=0.7)


hour8 <- hour %>% filter(hr == 8)
View(hour8)
plot(hour8$atemp, hour8$cnt, col = hour8$weekday, lwd = 2, xlab = "Feeling temperature for hour = 8", ylab = "Total users for hour = 8")
legend("topright", legend = c("6","0", "1", "2", "3", "4", "5"), col = hour8$weekday, pch=rep(c(16),each=4),bty="n",cex=0.7,pt.cex=0.7)

hour17 <- hour %>% filter(hr == 17)
View(hour17)
plot(hour17$atemp, hour17$cnt, col = hour17$weekday, lwd = 2,xlab = "Feeling temperature for hour = 17", ylab = "Total users for hour = 17" )
legend("topright", legend = c("6", "0","1", "2", "3", "4", "5"), col = hour17$weekday, pch=rep(c(16),each=4),bty="n",cex=0.7,pt.cex=0.7)


# Humidity
plot(hum, cnt)
lm.hum = lm(cnt ~ hum)
summary(lm.hum)
abline(lm.hum, col = "red")

# Windspeed
plot(windspeed, cnt)
lm.wspeed = lm(cnt ~ windspeed)
summary(lm.wspeed)
abline(lm.wspeed, col = "red")

# Month
mnth = as.factor(mnth)
contrasts(mnth)
boxplot(cnt ~ mnth)
lm.mnth = lm(cnt ~ mnth)
summary(lm.mnth)  #does not make sense for the hour data set

#Season
season = as.factor(season)
contrasts(season)
boxplot(cnt ~ season)
lm.season = lm(cnt ~season)
summary(lm.season) #does not make sense for the hour data set


#Weathersituation
weathersit = as.factor(weathersit)
contrasts(weathersit)
boxplot(cnt ~ weathersit)
lm.weathersit = lm(cnt ~weathersit)
summary(lm.weathersit)


########## MULTIPLE LINEAR REGRESSION ##########

#filtering the hours that we dont think is so important due to computatinal time
hoursub <- hour %>% filter(hr != 0 & hr != 1 & hr != 2 & hr != 3 & hr != 4 & hr != 5 & hr != 20 & hr != 21 & hr != 21 & hr != 22 & hr != 23)
hoursub <- hoursub[,-3] #we dont consider the month, only the season, since R takes a long time and season and month are correlated

## BEST SUBSET SELECTION 
hoursub <- hoursub[, -c(12,13)]
str(hoursub)
View(hoursub)
hoursub [c("season", "yr",  "hr", "holiday", "weekday", "workingday", "weathersit" )] <- lapply(hoursub[c("season", "yr",  "hr", "holiday", "weekday", "workingday", "weathersit")], factor)
attach(hoursub)

fit.full <- regsubsets(cnt ~., data = hoursub, nvmax = 40)
summary <- summary(fit.full)
summary
summary$rsq
names(summary)


par(mfrow = c(1,1))
plot(summary$rsq, xlab = "Number of variables", ylab = "RSQ", type = "l")
which.max(summary$rsq)
points(31,summary$rsq[31], col = "red", cex = 2, pch = 20 )

plot(summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R2", type = "l")
which.max(summary$adjr2)
points(28,summary$rsq[28], col = "red", cex = 2, pch = 20 )

plot(summary$cp, xlab = "Number of variables", ylab = "CP", type = "l")
which.min(summary$cp)
points(27,summary$cp[27], col = "red", cex = 2, pch = 20 )

plot(summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
which.min(summary$bic)
points(24,summary$bic[24], col = "red", cex = 2, pch = 20 )

## BACKWARDS STEPWISE
fit.back <- regsubsets(cnt ~., data = hoursub, nvmax = 40, method = "backward")
summaryBC <- summary(fit.back)
summaryBC
names(summaryBC)

plot(summaryBC$rsq, xlab = "Number of variables", ylab = "RSQ", type = "l")
which.max(summaryBC$rsq)
points(31,summaryBC$rsq[31], col = "red", cex = 2, pch = 20 )

plot(summaryBC$adjr2, xlab = "Number of variables", ylab = "Adjusted R2", type = "l")
which.max(summaryBC$adjr2)
points(31,summaryBC$rsq[31], col = "red", cex = 2, pch = 20 )

plot(summaryBC$cp, xlab = "Number of variables", ylab = "CP", type = "l")
which.min(summaryBC$cp)
points(30,summaryBC$cp[30], col = "red", cex = 2, pch = 20 )

plot(summaryBC$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
which.min(summaryBC$bic)
points(23,summaryBC$bic[23], col = "red", cex = 2, pch = 20 )

## FORWARD STEPWISE

fit.for <- regsubsets(cnt ~., data = hoursub, nvmax = 40, method = "forward")
summaryFW <- summary(fit.for)
summaryFW
names(summaryFW)

plot(summaryFW$rsq, xlab = "Number of variables", ylab = "RSQ", type = "l")
which.max(summaryFW$rsq)
points(31,summaryFW$rsq[31], col = "red", cex = 2, pch = 20 )

plot(summaryFW$adjr2, xlab = "Number of variables", ylab = "Adjusted R2", type = "l")
which.max(summaryFW$adjr2)
points(28,summaryBC$rsq[28], col = "red", cex = 2, pch = 20 )

plot(summaryFW$cp, xlab = "Number of variables", ylab = "CP", type = "l")
which.min(summaryFW$cp)
points(27,summaryFW$cp[27], col = "red", cex = 2, pch = 20 )

plot(summaryFW$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
which.min(summaryFW$bic)
points(24,summaryBC$bic[24], col = "red", cex = 2, pch = 20 )

set.seed(1)# set seed
rand = sample(1:10191, 8153, replace = FALSE)
train3 <- hoursub[rand,]
test3 <- hoursub[-rand,] 


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
fit.best = regsubsets(cnt~., data=train3,nvmax=31)# perform best subset selection on training data 
test.mat = model.matrix(cnt~.,data=test3)# make a model matrix from the test data

val.errors=rep(NA,31)# replicate for all val.errors from NA to 31
for(i in 1:31){# for all i from 1 to 31
  coefi=coef(fit.best,id=i)# extract the coefficients from regfit.best with increasing nr. of pred.
  pred=test.mat[,names(coefi)]%*%coefi# get the prediction of the coefficients using test.mat
  val.errors[i]=mean((test3$cnt-pred)^2)# compute the test MSE
}

val.errors# display all test MSEs
which.min(val.errors)# give the one with the minimal test MSE
coef(fit.best, 28)
plot(val.errors, xlab = "Number of predictors", ylab = "Test MSE")#weird
lines(val.errors)


## K-FOLD
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(hoursub), replace = TRUE)
cv.errors <- matrix(NA, k, 31, dimnames = list(NULL, paste(1:31))) #null matrix to store results

for(j in 1:k)  {# define loop for best subset selection on training data
  best.fit = regsubsets(cnt ~.,data = hoursub[folds!=j,],nvmax=31)# best subset selection on training data set
  for(i in 1:31){# define loop for best subset selection on test data 
    pred = predict(best.fit,hoursub[folds==j,],id=i)# prediction/model testing of best subset selection
    cv.errors[j,i] = mean((hoursub$cnt[folds==j]-pred)^2)# give cross-val. 10x29-matrix as test MSE
  }
}

mean.cv.errors = apply(cv.errors,2,mean)
which.min(mean.cv.errors)
plot(mean.cv.errors, xlab = "Number of predictors", ylab = "Test MSE")
lines(mean.cv.errors)

####### SELECTING SOME MODELS FOR MODEL ASSESSMENT #######

#9
coef(fit.full,9)
coef(fit.for,9)
coef(fit.back,9)

#18
coef(fit.full,18)
coef(fit.for,18)
coef(fit.back,18)

#26
coef(fit.full,26)
coef(fit.for,26)
coef(fit.back,26)
