day [c("season", "yr", "mnth", "holiday", "weekday", "workingday", "weathersit" )] <- lapply(day[c("season", "yr", "mnth", "holiday", "weekday", 
                                                                                                   "workingday", "weathersit")], factor)
qcc.overdispersion.test(day$cnt, type = "poisson")

fit <- glm(cnt ~ dteday + season + holiday + workingday + weathersit + temp + atemp + hum + windspeed, data = day, family = poisson)
summary(fit)

poi.mod <- glm(cnt ~ dteday + season + holiday + workingday + weathersit + temp + atemp + hum + windspeed, family = poisson, data = day)
exp(poi.mod$coef)

predict(poi.mod, type = "response")

model <- glm(cnt ~ dteday + season + holiday + workingday + weathersit + temp + atemp + hum + windspeed, family = quasipoisson, data = day)
exp(model$coef)

fit <- glm(cnt ~ dteday + season + holiday + workingday + weathersit + temp + atemp + hum + windspeed, data = day, family = quasipoisson(link = "log"))
summary(fit)

predict(model, type = "response")
