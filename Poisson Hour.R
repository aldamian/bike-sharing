library(readxl)
hour <- read_excel("Master/Data Science and Machine Learning/Project/Bike-Sharing-Dataset/hour.xlsx")
View(hour)

hour [c("season", "yr", "mnth","hr", "holiday", "weekday", "workingday", "weathersit" )] <- lapply(hour[c("season", "yr", "mnth", "hr", "holiday", "weekday", 
                                                                                                          "workingday", "weathersit")], factor)
qcc.overdispersion.test(hour$cnt, type = "poisson")

fit <- glm(cnt ~ hr + season + holiday + workingday + weathersit + temp + atemp + hum + windspeed, data = hour, family = poisson)
summary(fit)

poi.mod <- glm(cnt ~ dteday + season + hr + holiday + workingday + weathersit + temp + atemp + hum + windspeed, family = poisson, data = hour)
exp(poi.mod$coef)


predict(poi.mod, type = "response")

model <- glm(cnt ~ hr + season + holiday + workingday + weathersit + temp + atemp + hum + windspeed, data = hour, family = quasipoisson)
exp(model$coef)

fit <- glm(cnt ~ hr + season + holiday + workingday + weathersit + temp + atemp + hum + windspeed, data = hour, family = quasipoisson(link = "log"))
summary(fit)

predict(model, type = "response")

