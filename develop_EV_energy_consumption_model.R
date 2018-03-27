### This file proposes an energy consumption model for battery electric vehicles (BEV)
### With vehicle speed and acceleration as input to this model,
### the instantaneous electricity consumption can be estimated.
### If OBD-II data are available from other BEV,
### a set of model parameters specific to that vehicle can be also calibrated.
### The features of this model are that (1) vehicle specific power (VSP) is considered,
### in order to model BEV regenerative braking; (2) impacts of ambient temperatures
### on vehicle auxiliary (e.g. air-conditioning) are considered.



rm(list=ls())

#set parameters
m <- 1621 #kg vehicle curb weight 1521 + 100 passenger weight
g <- 9.8066 #m/s2
rho_air <- 1.2256 #kg/m3
A <- 2.3316 #m2
Cd <- 0.28
f <- 0.01



##### model development #####
path = "C:\\..."
df <- read.csv(file = paste0(path,'-Regression.csv'), stringsAsFactors = F)

#delete voltage, current outliers
df <- subset(df, !df$HVBatt_Voltage.V. %in% boxplot.stats(df$HVBatt_Voltage.V.)$out &
               !df$HVBatt_Current.A. %in% boxplot.stats(df$HVBatt_Current.A.)$out)

#delete acceleration outliers
df <- subset(df, df$Acceleration>=-6 & df$Acceleration<=5)

#ECR (electricity consumption rate)
df$ECR <- -df$HVBatt_Current.A. * df$HVBatt_Voltage.V. #energy consumption rate, W, also J/s

#VSP (vehicle specific power)
df$VSP <- df$Veh_Speed.m.s.*(1.1*df$Acceleration + g*f) + 0.5*rho_air*Cd*A/m*df$Veh_Speed.m.s.^3
summary(df$VSP)

#date
df$Date <- as.Date(df$Timestamp)
df <- subset(df, df$Date < "2017-04-01")

#P_auxiliary (auxiliary power)
df$P_aux <- exp(-0.089400*df$Outside_Air_Temp.DegC. + 6.711928)

#separate city and highway
boundary <- 72/3.6 #m/s, 45mph
df_hwy <- subset(df, df$Veh_Speed.m.s. >= boundary)
df_city <- subset(df, df$Veh_Speed.m.s. < boundary)

#group VSP by 10%
aa <- df_city
summary(aa$VSP)
aa$VSP_Group <- c(-10:17)[
  findInterval(aa$VSP, c(-Inf, seq(-10, 16, by=1), Inf))]
with(aa, tapply(aa$ECR, aa$VSP_Group, mean))
plot(with(aa, tapply(aa$ECR, aa$VSP_Group, mean)), type='l', x=c(-10:17))

#separate training data by speed and VSP
df_hwy_p <- subset(df_hwy, VSP > 0 )
df_hwy_i <- subset(df_hwy, VSP == 0)
df_hwy_n <- subset(df_hwy, VSP < 0 )
df_city_p <- subset(df_city, VSP > 0 )
df_city_i <- subset(df_city, VSP == 0)
df_city_n <- subset(df_city, VSP < 0 )

#regression

#on highway
fit_hwy_p <- lm(data=df_hwy_p, ECR ~ VSP
                + P_aux
                )
summary(fit_hwy_p)
df_hwy_n$eta_rb <- 1/exp(0.0411/abs(df_hwy_n$A))
summary(df_hwy_n$eta_rb)
fit_hwy_n <- lm(data=df_hwy_n, ECR ~ VSP
                + P_aux
            )
summary(fit_hwy_n)
idle_hwy <- mean(df_hwy_i$ECR)

#on city
fit_city_p <- lm(data=df_city_p, ECR ~ VSP
                 + P_aux
                 )
summary(fit_city_p)
df_city_n$eta_rb <- 1/exp(0.0411/abs(df_city_n$A))
summary(df_city_n$eta_rb)
fit_city_n <- lm(data=df_city_n, ECR ~ VSP
                 + P_aux
)
summary(fit_city_n)
fit_city_i <- lm(data=df_city_i, ECR ~ P_aux
)
summary(fit_city_i)


