rm(list=ls())
graphics.off()

##Projet de Modélisation Prédictive
library(mgcv)
library(yarrr)
library(magrittr)
library(forecast)
library(tidyverse)
library(randomForest)
library(qgam)



source('R/score.R')
Data0 <- read_delim("Data/train.csv", delim=",")
Data1<- read_delim("Data/test.csv", delim=",")

Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)

Data0$WeekDays <- as.factor(Data0$WeekDays)
Data1$WeekDays <- as.factor(Data1$WeekDays)

sel_a <- which(Data0$Year<=2021)
sel_b <- which(Data0$Year>2021)

a_sel <- which(Data0$Year <= 2020)
b_sel <- which(Data0$Year > 2020 & Data0$Year <= 2021)

###GAM Kalman

equation <- Net_demand ~ s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') +
  s(Net_demand.1, bs='cr', by = as.factor(WeekDays))+ s(Net_demand.7, bs='cr') +
  WeekDays + BH + s(Temp_s99_min, k = 10, bs = 'cr') + s(Temp_s99_max, k = 10, bs = 'cr') +
  s(Wind) + Christmas_break  + te(as.numeric(Date), Nebulosity, k=c(4,10)) + s(Time, k = 10, bs = 'cr') +
  s(Load.7, k = 5)

gamn <- gam(equation, data = Data0)

Data1c <- Data1
Data1c <- Data1[,-c(37, 36)]
Data1c[,"Load"] <- 0
Data1c[,"Net_demand"] <- 0
Data1c[, "Solar_power"] <- 0
Data1c[, "Wind_power"] <- 0

for (i in (1:dim(Data1c)[1] - 1)){
  Data1c$Load[i] <- Data1c$Load.1[i+1]
  Data1c$Net_demand[i] <- Data1c$Net_demand.1[i+1]
  Data1c$Solar_power[i] <- Data1c$Solar_power.1[i+1]
  Data1c$Wind_power[i] <- Data1c$Wind_power.1[i+1]
}

all_data <- rbind(Data0, Data1c)
all_data <- all_data[-3866,]


X <- predict(gamn, newdata=all_data, type='terms')
###scaling columns
for (j in 1:ncol(X)){
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])}
X <- cbind(X,1)
d <- ncol(X)

y <- all_data$Net_demand

ssm <- viking::statespace(X, y)


#ssm_em <- viking::select_Kalman_variances(ssm, X[1:3471,], y[1:3471], method = 'em', n_iter = 100,
                                          #Q_init = diag(d), verbose = 10, mode_diag = T)

#(ssm_em, "Models/KalmanEM_def.RDS")

ssm_em <- readRDS("Models/KalmanEM_def.RDS")
ssm_em <- predict(ssm_em, X, y, type='model', compute_smooth = TRUE)

gamn.forecast <- ssm_em$pred_mean%>%tail(394)

gam_alone.forecast <- predict(gamn, newdata = Data1)


#gamn.forecast <- predict(gamn, Data1)

##### GAM Ridge

equation <- Net_demand ~ s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr')  +
  s(Temp_s99,k=10, bs='cr') + WeekDays +BH +
  s(Wind) + te(as.numeric(Date), Nebulosity, k=c(4,10)) +
  s(Net_demand.1, bs='cr') +  s(Net_demand.7, bs='cr')
#gam_ridge <- gam(as.formula(equation), data=Data0, method="REML", select=TRUE)

gam_ridge <- readRDS("Models/gam_ridge.RDS")

gam_ridge.forecast <- predict(gam_ridge, newdata = Data1)


### GAM et RF sur les erreurs

equation <- Net_demand ~ s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Net_demand.1, bs='cr', by = as.factor(WeekDays))+ s(Net_demand.7, bs='cr') +
  WeekDays + BH + s(Temp_s99_min, k = 10, bs = 'cr') + s(Temp_s99_max, k = 10, bs = 'cr') +
  s(Wind) + Christmas_break + te(Solar_power.1, Wind_power.1, by = as.factor(WeekDays), k = c(3,3), bs = 'cr') +
  te(as.numeric(Date), Nebulosity, k=c(4,10))
gam <- gam(equation, data=Data0)


res_gam <- (Data0$Net_demand - predict(gam, newdata = Data0))


res_tab <- Data0[, c("Temp", "toy", "Time", "WeekDays")]
res_tab[,"Res"] <- res_gam

rf_err <- randomForest(Res ~ Temp + toy + WeekDays + Time, data=res_tab)

gam.predict <- predict(gam, newdata = Data1)
rf.predict <- predict(rf_err, newdata = Data1)

final_pred <- gam.predict + rf.predict




#### Random Forest

#rf=randomForest(Net_demand ~Load.1 + Load.7 + Temp + Temp_s95 + Temp_s99 +
#                  Temp_s95_min + Temp_s95_max + Temp_s99_min + Temp_s99_max +
 #                 Wind + Wind_weighted + Nebulosity + Nebulosity_weighted + toy +
  #                WeekDays + BH_before + BH + BH_after + Year + Month + DLS +
   #               Summer_break + Christmas_break + Holiday + Holiday_zone_a +
    #              Holiday_zone_b + Holiday_zone_c + BH_Holiday +
     #             Solar_power.1 + Solar_power.7 + Wind_power.1 + Wind_power.7 +
      #            Net_demand.1 + Net_demand.7, data=Data0, mtry=7)

#saveRDS(rf, "Models/randomForest.RDS")

rf <- readRDS("Models/randomForest.RDS")

rf.forecast <- predict(rf, newdata = Data1)

### QGAM

equation <- Net_demand ~s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + 
  s(Load.1, bs='cr', by= as.factor(WeekDays))+ s(Load.7, bs='cr')  + as.factor(WeekDays) +BH + s(Time, k = 10, bs = 'cr')

equation_var <- ~  s(Temp,k=10, bs='cr') + s(Load.1) + as.factor(WeekDays) + s(toy, k = 30, bs = 'cc') + s(Time, k = 10, bs='cr')

#gqgam08 <- qgam(list(equation, equation_var), data=Data0, qu=0.8)

#saveRDS(gqgam08, "Models/qgam08.RDS")

gqgam08 <- readRDS("Models/qgam08.RDS")

gqgam08.forecast <- predict(gqgam08, newdata = Data1, qu = 0.8)

#gqgam02 <- qgam(list(equation, equation_var), data=Data0, qu=0.2)

gqgam02 <- readRDS("Models/qgam02.RDS")

gqgam02.forecast <- predict(gqgam02, newdata = Data1, qu = 0.2)

#saveRDS(gqgam02, "Models/qgam02.RDS")


###Expert aggregation 

library(opera)

experts1 <- cbind(gqgam08.forecast[-395],gqgam02.forecast[-395], gam_ridge.forecast[-395], gamn.forecast, final_pred[-395], rf.forecast[-395])
experts <- experts1
colnames(experts) <- c("gam08","gam02","gam_ridge", "GAM_Kalman", "gam_rf_erreurs", "rf")
MLPol <- mixture(Y=Data1$Net_demand.1[-1], experts=experts, model = "MLpol")
MLPol_pinball <- mixture(Y=Data1$Net_demand.1[-1], experts=experts, loss.type = list(name='pinball', tau=0.8), model = "MLpol")

plot(MLPol_pinball)

plot(MLPol)

MLPol_pinball$weights[394,]




experts_agg <- read.csv("Submissions/gam.csv")
experts_agg$Net_demand[1:394] <- MLPol_pinball$prediction
experts_agg$Net_demand[395] <- gam_ridge.forecast[395]
write.csv(experts_agg, "Submissions/Kalman_agg6_.csv", row.names = FALSE)

experts_agg <- read.csv("Submissions/gam.csv")
experts_agg$Net_demand[1:394] <- MLPol$prediction
experts_agg$Net_demand[395] <- gam_ridge.forecast[395]
write.csv(experts_agg, "Submissions/Kalman_agg6_RMSE.csv", row.names = FALSE)



