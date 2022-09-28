library(tidyverse)
library(lubridate)
bsb <- read.csv("BSB_tagging_data.csv")
#Q 1.1:
bsb <- bsb %>%
  #mutate(recapture_date = as.Date(Date_at_recapture, "%m/%d/%y")) %>% 
  mutate(sex_change = Sex_at_capture != Sex_at_recapture) %>%  
          # year_recapture = lubridate::year(recapture_date),
          # month_recapture = lubridate::month(recapture_date),
          # day_recapture = lubridate::day(recapture_date)) %>%
          # filter(month_recapture > 7)
  drop_na() 

sum_change <- sum(bsb$sex_change)+1
sum_female <- sum(bsb$Sex_at_recapture == "F") - (sum_change+1)

x <- seq(0, 1, by = 0.01)
plot(dbeta(x, shape1 = sum_change, shape2 = sum_female))  

#Q 1.2:
qbeta(c(0.025,0.975), shape1 = sum_change, shape2 = sum_female) 

#Q 1.3:

mod <- bsb %>% 
glm(sex_change ~ Length_at_capture,data =., family = binomial())
summary(mod)

#Q 1.4:
plot(sex_change ~ Length_at_capture, data = bsb, xlab = "Length at capture (mm)", ylab = "Probability of Sex Change")
length.predict <- data.frame(Length_at_capture = seq(min(bsb$Length_at_capture), max(bsb$Length_at_capture)), len = 500)
sex_change_predict <- predict(mod, length.predict, type = "response", se.fit = TRUE)
lines(length.predict$Length_at_capture, sex_change_predict)

#Q 1.5: 
fit <- sex_change_predict$fit
fit2 <- mod$family$linkinv(fit)

sex_change_predict_300 <- predict(mod, data.frame(Length_at_capture=300), type = "response", se.fit = TRUE)
pred.int <- data.frame(lwr=sex_change_predict_300$fit - 1.96*sex_change_predict_300$se.fit,
                       fit=sex_change_predict_300$fit,
                       upr=sex_change_predict_300$fit + 1.96*sex_change_predict_300$se.fit)

#Q 1.6


  
