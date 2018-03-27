### This file validates the calibrated VT-Micro gasoline consumption model



##### Validate by EPA standard driving cycles #####

df <- read.csv("C:\\...")
coef <- read.csv("C:\\...")

df$FC <- ifelse(df$A>=0, exp(coef$Intercept[1] + df$S*coef$S[1] + df$S2*coef$S2[1] + df$S3*coef$S3[1] + 
                               df$A*coef$A[1] + df$A2*coef$A2[1] + df$A3*coef$A3[1] + 
                               df$SA*coef$SA[1] + df$S2A*coef$S2A[1] + df$S3A*coef$S3A[1] + 
                               df$SA2*coef$SA2[1] + df$S2A2*coef$S2A2[1] + df$S3A2*coef$S3A2[1] + 
                               df$SA3*coef$SA3[1] + df$S2A3*coef$S2A3[1] + df$S3A3*coef$S3A3[1]),
                exp(coef$Intercept[2] + df$S*coef$S[2] + df$S2*coef$S2[2] + df$S3*coef$S3[2] + 
                      df$A*coef$A[2] + df$A2*coef$A2[2] + df$A3*coef$A3[2] + 
                      df$SA*coef$SA[2] + df$S2A*coef$S2A[2] + df$S3A*coef$S3A[2] + 
                      df$SA2*coef$SA2[2] + df$S2A2*coef$S2A2[2] + df$S3A2*coef$S3A2[2] + 
                      df$SA3*coef$SA3[2] + df$S2A3*coef$S2A3[2] + df$S3A3*coef$S3A3[2]) )

11.04/(sum(df$FC)/1000*0.264172)
7.45/(sum(df$FC)/1000*0.264172)
10.26/(sum(df$FC)/1000*0.264172) 
1.18/(sum(df$FC)/1000*0.264172) 
8.01/(sum(df$FC)/1000*0.264172) 



##### Validate by trip gasoline consumption #####

path = "C:\\..."

col_highway <- 12

#fuel consumption model
coef <- read.csv("C:\\...")

#read trips-summary.csv
trips_summary <- read.csv(paste0(substring(path,1,7), '_trips-summary.csv'))
#trips_summary$MPG <- trips_summary$Trip.Distance..mi./trips_summary$Gasoline.Consumed
trips_summary$Date <- as.character(trips_summary$Date)
trips_summary$Date <- as.POSIXct(trips_summary$Date, format = "%B %d, %Y %I:%M:%S %p", 
                                 tz = "America/New_York")
attributes(trips_summary$Date)$tzone <- "UTC"

#validate by each trip
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
    trips_summary$Gasoline.Consumed.MAF[i] <- NA
    trips_summary$Gasoline.Consumed.Fit[i] <- NA
    next
  }
  
  if(ncol(df)!=col_highway){
    trips_summary$Gasoline.Consumed.MAF[i] <- NA
    trips_summary$Gasoline.Consumed.Fit[i] <- NA
    next
  }
  
  if(colnames(df)[length(colnames(df))]=='X'){df <- df[,-ncol(df)]}
  df <- df[-1,]
  df$Engine_RPM.rpm. <- as.numeric(df$Engine_RPM.rpm.)
  df <- subset(df, df$Engine_RPM.rpm.!=0)
  
  #acceleration
  acc <- round(diff(df$Veh_Speed.km.h.)/3.6, 4)
  df$Acceleration <- append(acc, 0, after=length(acc)) #unit: m/s/s
  names(df)[names(df)=='Acceleration'] <- 'A'
  
  #speed
  df$Veh_Speed.km.h. <- round(df$Veh_Speed.km.h./3.6, 4) #change km/h to m/s
  names(df)[names(df)=='Veh_Speed.km.h.'] <- 'S'
  
  #fuel consumption of the trip
  Fuel_Density <- 719.7 #g/L
  df$FC_L1 <- round(1000*df$MAF.g.s./14.7/(1+df$LongTermFuelTrim_B1/100)/Fuel_Density, 4) #ml/s
  df$FC_S1 <- round(1000*df$MAF.g.s./14.7/(1+df$ShortTermFuelTrim_B1/100)/Fuel_Density, 4) #ml/s
  
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
  
  df$FC.Fit <- ifelse(df$A>=0, exp(coef$Intercept[1] + df$S*coef$S[1] + df$S2*coef$S2[1] + df$S3*coef$S3[1] + 
                                 df$A*coef$A[1] + df$A2*coef$A2[1] + df$A3*coef$A3[1] + 
                                 df$SA*coef$SA[1] + df$S2A*coef$S2A[1] + df$S3A*coef$S3A[1] + 
                                 df$SA2*coef$SA2[1] + df$S2A2*coef$S2A2[1] + df$S3A2*coef$S3A2[1] + 
                                 df$SA3*coef$SA3[1] + df$S2A3*coef$S2A3[1] + df$S3A3*coef$S3A3[1]),
                  exp(coef$Intercept[2] + df$S*coef$S[2] + df$S2*coef$S2[2] + df$S3*coef$S3[2] + 
                        df$A*coef$A[2] + df$A2*coef$A2[2] + df$A3*coef$A3[2] + 
                        df$SA*coef$SA[2] + df$S2A*coef$S2A[2] + df$S3A*coef$S3A[2] + 
                        df$SA2*coef$SA2[2] + df$S2A2*coef$S2A2[2] + df$S3A2*coef$S3A2[2] + 
                        df$SA3*coef$SA3[2] + df$S2A3*coef$S2A3[2] + df$S3A3*coef$S3A3[2]) )
  
  trips_summary$Gasoline.Consumed.MAF[i] <- round(sum(df$FC_L1)/1000*0.264172, 2) #unit is gal
  trips_summary$Gasoline.Consumed.Fit[i] <- round(sum(df$FC.Fit)/1000*0.264172, 2)
  
  #extract one trip
  if(i==260){aa <- df}
  
}

#remove NA in Gasoline.Consumed.Fit
trips_summary <- subset(trips_summary, is.na(trips_summary$Gasoline.Consumed.Fit)==F)

#MAPE
mean(abs((trips_summary$Gasoline.Consumed.Fit-trips_summary$Gasoline.Consumed)/trips_summary$Gasoline.Consumed) * 100)
#RMSE
library(Metrics)
rmse(trips_summary$Gasoline.Consumed, trips_summary$Gasoline.Consumed.Fit)

#check fleetcarma FC, MAF FC, fitted FC
library(GGally)
ggpairs(trips_summary[,c('Gasoline.Consumed', 'Gasoline.Consumed.MAF', 'Gasoline.Consumed.Fit')])
summary(lm(data=trips_summary, Gasoline.Consumed ~ Gasoline.Consumed.Fit))
summary(lm(data=trips_summary, Gasoline.Consumed.MAF ~ Gasoline.Consumed.Fit))

#plot scatterplot and fitted line
library(ggplot2)
trips_summary$Gasoline.Consumed <- trips_summary$Gasoline.Consumed/3.78541
trips_summary$Gasoline.Consumed.Fit <- trips_summary$Gasoline.Consumed.Fit/3.78541
p <- ggplot(trips_summary, aes(x=trips_summary$Gasoline.Consumed, y=trips_summary$Gasoline.Consumed.Fit)) + 
  geom_point() +
  labs(x='Actual trip fuel consumption (L)', y='Estimated trip fuel consumption (L)') +
  theme(axis.title=element_text(size=14), axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  scale_x_continuous(limits = c(0, 0.5)) + 
  scale_y_continuous(limits = c(0, 0.5)) 
ggsave(filename="C:\\...jpeg", plot=p, width=4, height=4, units="in")
summary(lm(data=trips_summary, Gasoline.Consumed.Fit ~ 0 + Gasoline.Consumed))


