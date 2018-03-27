### This file calibrates 2 EV energy consumption models: Yao's model and Yang's model
### Use OBD-II data collected from a Nissan Leaf



##### Yao's model #####
rm(list=ls())

path = "C:\\..."
df <- read.csv(file = paste0(path,'-Regression.csv'), stringsAsFactors = F)

#delete voltage, current outliers
df <- subset(df, !df$HVBatt_Voltage.V. %in% boxplot.stats(df$HVBatt_Voltage.V.)$out &
               !df$HVBatt_Current.A. %in% boxplot.stats(df$HVBatt_Current.A.)$out)

#delete acceleration outliers
df <- subset(df, df$Acceleration>=-6 & df$Acceleration<=5)

#ECR
df$ECR <- -df$HVBatt_Current.A. * df$HVBatt_Voltage.V. #energy consumption rate, W, also J/s

#Yao's model
yao <- df[,c('ECR', 'Veh_Speed.m.s.', 'Acceleration')]
colnames(yao) <- c('ECR', 'S', 'A')
yao$S2 <- yao$S^2
yao$S3 <- yao$S^3
yao$A2 <- yao$A^2
yao$A3 <- yao$A^3
yao$SA <- yao$S*yao$A
yao$S2A <- yao$S^2*yao$A
yao$S3A <- yao$S^3*yao$A
yao$SA2 <- yao$S*yao$A^2
yao$S2A2 <- yao$S^2*yao$A^2
yao$S3A2 <- yao$S^3*yao$A^2
yao$SA3 <- yao$S*yao$A^3
yao$S2A3 <- yao$S^2*yao$A^3
yao$S3A3 <- yao$S^3*yao$A^3

yao$S4 <- yao$S^4
yao$A4 <- yao$A^4
yao$S4A <- yao$S^4*yao$A
yao$S4A2 <- yao$S^4*yao$A^2
yao$S4A3 <- yao$S^4*yao$A^3
yao$S4A4 <- yao$S^4*yao$A^4
yao$SA4 <- yao$S*yao$A^4
yao$S2A4 <- yao$S^2*yao$A^4
yao$S3A4 <- yao$S^3*yao$A^4

#4 modes: acceleration, deceleration, idle, cruise
th <- 0.0
yao_a <- subset(yao, yao$A >= th)
yao_d <- subset(yao, yao$A < -th)
yao_c <- subset(yao, yao$A >= -th & yao$A <= th & yao$S!=0)
yao_i <- subset(yao, yao$A >= -th & yao$A <= th & yao$S==0)

#fit the model
fit_a <- lm(data=yao_a, ECR ~ S + S2 + S3 + 
              A + SA + S2A + S3A +
              A2 + SA2 + S2A2 + S3A2
            +A3 + SA3 + S2A3 + S3A3
)
summary(fit_a)
fit_a$coefficients
coef_a <- as.data.frame(t(as.matrix(fit_a$coefficients)))
colnames(coef_a)[1] <- 'Intercept'

fit_d <- lm(data=yao_d, ECR ~ S + S2 + S3 + 
              A + SA + S2A + S3A +
              A2 + SA2 +  S2A2 + S3A2
            +A3 + SA3 + S2A3 + S3A3
)
summary(fit_d)
fit_d$coefficients
coef_d <- as.data.frame(t(as.matrix(fit_d$coefficients)))
colnames(coef_d)[1] <- 'Intercept'

fit_c <- lm(data=yao_c, ECR ~ S + S2 + S3
)
summary(fit_c)
fit_c$coefficients
coef_c <- as.data.frame(t(as.matrix(fit_c$coefficients)))
colnames(coef_c)[1] <- 'Intercept'

coef_i <- mean(yao_i$ECR)

#validate the model
path = "C:\\..." #use 1 second data to validate
trips_summary <- read.csv(paste0(substring(path,1,7), '_trips-summary.csv'))
trips_summary$Date <- as.character(trips_summary$Date)
trips_summary$Date <- as.POSIXct(trips_summary$Date, format = "%B %d, %Y %I:%M:%S %p", 
                                 tz = "America/New_York")
attributes(trips_summary$Date)$tzone <- "UTC"

for (i in 1:nrow(trips_summary)) {
  setwd(path)
  csv_path <- paste0(substring(as.character(trips_summary$Date[i]),1,10),
                     'T',
                     substring(as.character(trips_summary$Date[i]),12,13),
                     '-',
                     substring(as.character(trips_summary$Date[i]),15,16),
                     '-',
                     substring(as.character(trips_summary$Date[i]),18,19),
                     '.csv')
  if(csv_path %in% dir(path)) {
    df <- read.csv(csv_path, stringsAsFactors = F)
  } else{
    trips_summary$Electrical.Energy.Consumed..kWh.IU[i] <- NA
    trips_summary$Electrical.Energy.Consumed..kWh.Fit[i] <- NA
    next
  }
  
  #speed
  df$Veh_Speed.km.h. <- round(df$Veh_Speed.km.h./3.6, 4) #change km/h to m/s
  names(df)[names(df)=='Veh_Speed.km.h.'] <- 'S'
  names(df)[names(df)=='Acceleration'] <- 'A'
  
  #ECR, IU
  df$ECR <- -df$HVBatt_Current.A. * df$HVBatt_Voltage.V. #energy consumption rate, W, also J/s
  trips_summary$Electrical.Energy.Consumed..kWh.IU[i] <- round(sum(df$ECR)/1000/3600, 2) #unit: kWh
  
  #fit
  df$S2 <- df$S^2
  df$S3 <- df$S^3
  df$A2 <- df$A^2
  df$A3 <- df$A^3
  df$SA <- df$S*df$A
  df$S2A <- df$S^2*df$A
  df$S3A <- df$S^3*df$A
  df$SA2 <- df$S*df$A^2
  df$S2A2 <- df$S^2*df$A^2
  df$S3A2 <- df$S^3*df$A^2
  df$SA3 <- df$S*df$A^3
  df$S2A3 <- df$S^2*df$A^3
  df$S3A3 <- df$S^3*df$A^3
  
  df$ECR.Fit <- ifelse(df$A>=0, coef_a$Intercept[1] + df$S*coef_a$S[1] + df$S2*coef_a$S2[1] + df$S3*coef_a$S3[1] + 
                         df$A*coef_a$A[1] + df$A2*coef_a$A2[1] + df$A3*coef_a$A3[1] + 
                         df$SA*coef_a$SA[1] + df$S2A*coef_a$S2A[1] + df$S3A*coef_a$S3A[1] + 
                         df$SA2*coef_a$SA2[1] + df$S2A2*coef_a$S2A2[1] + df$S3A2*coef_a$S3A2[1] + 
                         df$SA3*coef_a$SA3[1] + df$S2A3*coef_a$S2A3[1] + df$S3A3*coef_a$S3A3[1],
                       ifelse(df$A<0, coef_d$Intercept[1] + df$S*coef_d$S[1] + df$S2*coef_d$S2[1] + df$S3*coef_d$S3[1] + 
                                df$A*coef_d$A[1] + df$A2*coef_d$A2[1] + df$A3*coef_d$A3[1] + 
                                df$SA*coef_d$SA[1] + df$S2A*coef_d$S2A[1] + df$S3A*coef_d$S3A[1] + 
                                df$SA2*coef_d$SA2[1] + df$S2A2*coef_d$S2A2[1] + df$S3A2*coef_d$S3A2[1] + 
                                df$SA3*coef_d$SA3[1] + df$S2A3*coef_d$S2A3[1] + df$S3A3*coef_d$S3A3[1],
                              ifelse(df$S!=0, coef_c$Intercept[1] + df$S*coef_c$S[1] + df$S2*coef_c$S2[1] + df$S3*coef_c$S3[1],
                                     coef_i
                              )))
  
  trips_summary$Electrical.Energy.Consumed..kWh.Fit[i] <- round(sum(df$ECR.Fit)/1000/3600, 2) #unit: kWh
}

#trips from the proposed energy consumption model
trip_id <- read.csv("C:\\...")
trips_summary <- subset(trips_summary, trips_summary$Trip.Id %in% trip_id$Trip.Id)

#validate
summary(lm(data=trips_summary, Electrical.Energy.Consumed..kWh.IU ~ Electrical.Energy.Consumed..kWh.Fit))
library(ggplot2)
p <- ggplot(trips_summary, aes(x=Electrical.Energy.Consumed..kWh.IU, y=Electrical.Energy.Consumed..kWh.Fit)) + 
  geom_point() +
  labs(x='Actual trip energy consumption (kWh)', y='Estimated trip energy consumption (kWh)') +
  theme(axis.title=element_text(size=14), axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  #stat_smooth(method=lm, se=F) +
  scale_x_continuous(limits = c(0, 15)) + 
  scale_y_continuous(limits = c(0, 15)) 
p



#MAPE
mean(abs((trips_summary$Electrical.Energy.Consumed..kWh.Fit-trips_summary$Electrical.Energy.Consumed..kWh.IU)/trips_summary$Electrical.Energy.Consumed..kWh.IU) * 100)
mean((trips_summary$Electrical.Energy.Consumed..kWh.Fit-trips_summary$Electrical.Energy.Consumed..kWh.IU)/trips_summary$Electrical.Energy.Consumed..kWh.IU * 100)

#RMSE
library(Metrics)
rmse(trips_summary$Electrical.Energy.Consumed..kWh.IU, trips_summary$Electrical.Energy.Consumed..kWh.Fit)





##### Yang's model #####

rm(list=ls())

#set parameters
m <- 1621 #kg vehicle curb weight 1521 + 100 passenger weight
g <- 9.8066 #m/s2
rho_air <- 1.2256 #kg/m3
A <- 2.3316 #m2
Cd <- 0.28
f <- 0.01
eta_e <- 0.85
eta_te <- 0.85
eta_m <- 0.91 #cite Fiori

#model development
path = "C:\\..."
df <- read.csv(file = paste0(path,'-Regression.csv'), stringsAsFactors = F)

#delete voltage, current outliers
df <- subset(df, !df$HVBatt_Voltage.V. %in% boxplot.stats(df$HVBatt_Voltage.V.)$out &
               !df$HVBatt_Current.A. %in% boxplot.stats(df$HVBatt_Current.A.)$out)

#delete Acceleration outliers
df <- subset(df, df$Acceleration>=-6 & df$Acceleration<=5)

#ECR
df$ECR <- -df$HVBatt_Current.A. * df$HVBatt_Voltage.V. #energy consumption rate, W, also J/s

#VSP
df$VSP <- df$Veh_Speed.m.s.*(1.1*df$Acceleration + g*f) + 0.5*rho_air*Cd*A/m*df$Veh_Speed.m.s.^3

#model validation
path = "C:\\..." #use 1 second data to validate

#read trips summary
trips_summary <- read.csv(paste0(substring(path,1,7), '_trips-summary.csv'))
trips_summary$Date <- as.character(trips_summary$Date)
trips_summary$Date <- as.POSIXct(trips_summary$Date, format = "%B %d, %Y %I:%M:%S %p", 
                                 tz = "America/New_York")
attributes(trips_summary$Date)$tzone <- "UTC"

#add auxiliary load in new trip summary to this trip summary
trips_summary_new <- read.csv(paste0(substring(path,1,7), '_trips-summary_new.csv'))
trips_summary$Auxiliary.Load..kW. <- trips_summary_new$Auxiliary.Load..kW.
trips_summary$Auxiliary.Load..kW.[trips_summary$Auxiliary.Load..kW.<0] <- 0

for (i in 1:nrow(trips_summary)) {
  setwd(path)
  csv_path <- paste0(substring(as.character(trips_summary$Date[i]),1,10),
                     'T',
                     substring(as.character(trips_summary$Date[i]),12,13),
                     '-',
                     substring(as.character(trips_summary$Date[i]),15,16),
                     '-',
                     substring(as.character(trips_summary$Date[i]),18,19),
                     '.csv')
  if(csv_path %in% dir(path)) {
    df <- read.csv(csv_path, stringsAsFactors = F)
  } else{
    trips_summary$Electrical.Energy.Consumed..kWh.IU[i] <- NA
    trips_summary$Electrical.Energy.Consumed..kWh.Fit[i] <- NA
    next
  }
  
  #speed
  df$Veh_Speed.km.h. <- round(df$Veh_Speed.km.h./3.6, 4) #change km/h to m/s
  names(df)[names(df)=='Veh_Speed.km.h.'] <- 'S'
  names(df)[names(df)=='Acceleration'] <- 'A'
  
  #ECR, IU
  df$ECR <- -df$HVBatt_Current.A. * df$HVBatt_Voltage.V. #energy consumption rate, W, also J/s
  trips_summary$Electrical.Energy.Consumed..kWh.IU[i] <- round(sum(df$ECR)/1000/3600, 2) #unit: kWh
  
  df$VSP <- df$S*(1.1*df$A + g*f) + 0.5*rho_air*Cd*A/m*df$S^3
  
  df$eta_rb <- 1/exp(0.0411/abs(df$A))
  
  #P_auxiliary
  df$P_aux <- 1000*trips_summary$Auxiliary.Load..kW.[i] #W
  
  df_hwy <- subset(df, df$A >= 0) #here df_hwy=df_acc
  df_city <- subset(df, df$A < 0) #here df_city=df_dec
  
  df_hwy$ECR.Fit <- m*df_hwy$VSP/eta_te/eta_e + df_hwy$P_aux
    
  df_city$ECR.Fit <- ifelse(df_city$S<5, df_city$S*0.1*m*eta_te*eta_m*df_city$VSP + df_city$P_aux,
                            (0.5+0.3/20*(df_city$S-5))*m*eta_te*eta_m*df_city$VSP + df_city$P_aux)
  
  df <- rbind(df_hwy, df_city)
  
  trips_summary$Electrical.Energy.Consumed..kWh.Fit[i] <- round(sum(df$ECR.Fit)/1000/3600, 2) #unit: kWh
  
}

#trips from the proposed energy consumption model
trip_id <- read.csv("C:\\...")
trips_summary <- subset(trips_summary, trips_summary$Trip.Id %in% trip_id$Trip.Id)

#validate
summary(lm(data=trips_summary, Electrical.Energy.Consumed..kWh.IU ~ Electrical.Energy.Consumed..kWh.Fit))
library(ggplot2)
p <- ggplot(trips_summary, aes(x=Electrical.Energy.Consumed..kWh.IU, y=Electrical.Energy.Consumed..kWh.Fit)) + 
  geom_point() +
  labs(x='Actual trip energy consumption (kWh)', y='Estimated trip energy consumption (kWh)') +
  theme(axis.title=element_text(size=14), axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  scale_x_continuous(limits = c(0, 15)) + 
  scale_y_continuous(limits = c(0, 15)) 
p


#MAPE
mean(abs((trips_summary$Electrical.Energy.Consumed..kWh.Fit-trips_summary$Electrical.Energy.Consumed..kWh.IU)/trips_summary$Electrical.Energy.Consumed..kWh.IU) * 100)
mean((trips_summary$Electrical.Energy.Consumed..kWh.Fit-trips_summary$Electrical.Energy.Consumed..kWh.IU)/trips_summary$Electrical.Energy.Consumed..kWh.IU * 100)
#RMSE
library(Metrics)
rmse(trips_summary$Electrical.Energy.Consumed..kWh.IU, trips_summary$Electrical.Energy.Consumed..kWh.Fit)





##### plot 3 models together #####
proposed <- trips_summary
yao <- trips_summary
yang <- trips_summary

proposed_1 <- proposed[, c('Electrical.Energy.Consumed..kWh.IU', 'Electrical.Energy.Consumed..kWh.Fit')]
proposed_1$Model <- 'Proposed'
yao_1 <- yao[, c('Electrical.Energy.Consumed..kWh.IU', 'Electrical.Energy.Consumed..kWh.Fit')]
yao_1$Model <- 'Yao'
yang_1 <- yang[, c('Electrical.Energy.Consumed..kWh.IU', 'Electrical.Energy.Consumed..kWh.Fit')]
yang_1$Model <- 'Yang'

trips_summary <- rbind(yang_1, yao_1, proposed_1)

p <- ggplot(trips_summary, aes(x=Electrical.Energy.Consumed..kWh.IU, y=Electrical.Energy.Consumed..kWh.Fit,
                               color=Model)) + 
  geom_point(shape=1, size=1) +
  labs(x='Actual trip energy consumption (kWh)', y='Estimated trip energy consumption (kWh)') +
  theme(legend.position=c(0.95,0.4),
        legend.justification=c(1,1),
        axis.title=element_text(size=8), 
        axis.text = element_text(size=7),
        legend.text = element_text(size=7),
        legend.title=element_text(size=7),
        legend.key.size=unit(0.15, 'in'),
        legend.margin=unit(0, 'in')) +
  scale_x_continuous(limits = c(0, 15)) + 
  scale_y_continuous(limits = c(0, 15)) +
  stat_function(fun=function(x) x, color='grey')
p



