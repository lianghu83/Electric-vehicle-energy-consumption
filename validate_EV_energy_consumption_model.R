### This file validates the EV energy consumption model



##### model validation by trip #####
path = "C:\\..." #use 1 second data to validate

#read trips-summary.csv
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
  
  df$VSP <- df$S*(1.1*df$A + g*f) + 0.5*rho_air*Cd*A/m*df$S^3
  
  df$eta_rb <- 1/exp(0.0411/abs(df$A))
  
  #P_auxiliary
  df <- subset(df, df$Outside_Air_Temp.DegC. != Inf)
  df$P_aux <- exp(-0.089400*df$Outside_Air_Temp.DegC. + 6.711928)
  
  df_hwy <- subset(df, df$S >= boundary)
  df_city <- subset(df, df$S < boundary)
  
  df_hwy$ECR.Fit <- ifelse(df_hwy$VSP > 0, fit_hwy_p$coefficients[1] + fit_hwy_p$coefficients[2]*df_hwy$VSP + fit_hwy_p$coefficients[3]*df_hwy$P_aux #+ fit_hwy_p$coefficients[4]*df_hwy$VSP^3              
                           ,ifelse(df_hwy$VSP == 0, idle_hwy,
                                   fit_hwy_n$coefficients[1] + fit_hwy_n$coefficients[2]*df_hwy$VSP + fit_hwy_n$coefficients[3]*df_hwy$P_aux #+ fit_hwy_n$coefficients[4]*df_hwy$VSP^3
                           ))
  df_city$ECR.Fit <- ifelse(df_city$VSP > 0, fit_city_p$coefficients[1] + fit_city_p$coefficients[2]*df_city$VSP + fit_city_p$coefficients[3]*df_city$P_aux #+ fit_city_p$coefficients[4]*df_city$VSP^3
                            ,ifelse(df_city$VSP == 0, fit_city_i$coefficients[1] + fit_city_i$coefficients[2]*df_city$P_aux,
                                    fit_city_n$coefficients[1] + fit_city_n$coefficients[2]*df_city$VSP + fit_city_n$coefficients[3]*df_city$P_aux #+ fit_city_n$coefficients[4]*df_city$VSP^3
                            ))
  df <- rbind(df_hwy, df_city)
  
  trips_summary$Electrical.Energy.Consumed..kWh.Fit[i] <- round(sum(df$ECR.Fit)/1000/3600, 2) #unit: kWh
  
}



#check fleetcarma EC, IU EC, Fit EC
library(GGally)
trips_summary <- trips_summary[-570,]
attributes(trips_summary$Date)$tzone <- "America/Chicago"
trips_summary$month <- as.Date(trips_summary$Date, tz="America/Chicago")
trips_summary <- subset(trips_summary, Electrical.Energy.Consumed..kWh.>0 &
                          is.na(Electrical.Energy.Consumed..kWh.Fit)==FALSE)
trips_summary <- subset(trips_summary, Electrical.Energy.Consumed..kWh.>0)
trips_summary$diff <- abs(trips_summary$Electrical.Energy.Consumed..kWh.Fit-trips_summary$Electrical.Energy.Consumed..kWh.IU)/trips_summary$Electrical.Energy.Consumed..kWh.IU
trips_summary <- subset(trips_summary, diff<0.5)
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



##### extract one trip; compare actual and estimated energy consumption #####
setwd(path)
df <- read.csv("XXX.csv", stringsAsFactors = F)

#speed
df$Veh_Speed.km.h. <- round(df$Veh_Speed.km.h./3.6, 4) #change km/h to m/s
names(df)[names(df)=='Veh_Speed.km.h.'] <- 'S'
names(df)[names(df)=='Acceleration'] <- 'A'

#ECR, IU
df$ECR <- -df$HVBatt_Current.A. * df$HVBatt_Voltage.V. #energy consumption rate, W, also J/s

#VSP
df$VSP <- df$S*(1.1*df$A + g*f) + 0.5*rho_air*Cd*A/m*df$S^3

#P_auxiliary
df <- subset(df, df$Outside_Air_Temp.DegC. != Inf)
df$P_aux <- exp(-0.089400*df$Outside_Air_Temp.DegC. + 6.711928)

#fit ECR 
df$Second <- as.numeric(rownames(df))
df_hwy <- subset(df, df$S >= boundary)
df_city <- subset(df, df$S < boundary)

df_hwy$ECR.Fit <- ifelse(df_hwy$VSP > 0, fit_hwy_p$coefficients[1] + fit_hwy_p$coefficients[2]*df_hwy$VSP + fit_hwy_p$coefficients[3]*df_hwy$P_aux #+ fit_hwy_p$coefficients[4]*df_hwy$VSP^3              
                         ,ifelse(df_hwy$VSP == 0, idle_hwy,
                                 fit_hwy_n$coefficients[1] + fit_hwy_n$coefficients[2]*df_hwy$VSP + fit_hwy_n$coefficients[3]*df_hwy$P_aux #+ fit_hwy_n$coefficients[4]*df_hwy$VSP^3
                         ))
df_city$ECR.Fit <- ifelse(df_city$VSP > 0, fit_city_p$coefficients[1] + fit_city_p$coefficients[2]*df_city$VSP + fit_city_p$coefficients[3]*df_city$P_aux #+ fit_city_p$coefficients[4]*df_city$VSP^3
                          ,ifelse(df_city$VSP == 0, fit_city_i$coefficients[1] + fit_city_i$coefficients[2]*df_city$P_aux,
                                  fit_city_n$coefficients[1] + fit_city_n$coefficients[2]*df_city$VSP + fit_city_n$coefficients[3]*df_city$P_aux #+ fit_city_n$coefficients[4]*df_city$VSP^3
                          ))

df <- rbind(df_hwy, df_city)
df <- df[order(df$Second), ]
df$Second <- NULL

library(xlsx)
write.xlsx(df, file = "C:\\XXX.xlsx",
           sheetName = "Sheet", row.names = FALSE)


