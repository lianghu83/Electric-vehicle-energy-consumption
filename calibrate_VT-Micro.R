### This file uses raw OBD-II data collected from gasoline vehicles,
### to calibrate a popular fuel consumption estimation model called VT-Micro.
### With vehicle speed and acceleration as input to the VT-Micro model,
### the instantaneous MPG can be estimated.
### If OBD-II data are available from other vehicle,
### a VT-Micro model specific to that vehicle can be also calibrated.



##### Process raw OBD-II data #####

library(plyr)

#OBD-II data directory
path = "C:\\..."

col_highway <- 12

file.name <- dir(path)

for (i in 1:length(file.name)) {
  setwd(path)
  df <- read.csv(file = file.name[i], stringsAsFactors = F)
  
  if(ncol(df)!=col_highway){next}
  
  if(colnames(df)[length(colnames(df))]=='X'){df <- df[,-ncol(df)]}
  df <- df[-1,]
  
  #data type
  df$Engine_RPM.rpm. <- as.numeric(df$Engine_RPM.rpm.)
  
  #speed, keep last row of each second
  df$Timestamp <- as.factor(df$Timestamp)
  tmp <- ddply(df, .(Timestamp), function(x) x[c(nrow(x)), ])
  
  #derive acceleration
  tmp$Timestamp <- as.POSIXct(tmp$Timestamp, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
  attributes(tmp$Timestamp)$tzone <- "America/Chicago"
  time_diff <- as.numeric(diff(tmp$Timestamp))
  speed_diff <- diff(tmp$Veh_Speed.km.h.)
  acc <- round(speed_diff/time_diff/3.6, 4) #unit: m/s/s
  tmp$Acceleration <- append(acc, 0, after=length(acc))
  
  #MAF (mass air flow), keep first row of each MAF group
  tmp$Flag[1] <- 1
  for (j in 2:nrow(tmp)) {
    if(tmp$MAF.g.s.[j]==tmp$MAF.g.s.[j-1]){
    #if(tmp$MAF.g.s.[j]==tmp$MAF.g.s.[j-1] & difftime(tmp$Timestamp[j], tmp$Timestamp[j-1], units="sec")!=5){
      tmp$Flag[j] <- tmp$Flag[j-1]
    } else {tmp$Flag[j] <- tmp$Flag[j-1]+1}
  }
  tmp$Flag <- as.factor(tmp$Flag)
  tmp <- ddply(tmp, .(Flag), function(x) x[c(1), ])
  tmp$Flag <- NULL
  
  #remove RPM==0
  tmp <- subset(tmp, tmp$Engine_RPM.rpm.!=0)
  
  #calculate gasoline consumption
  Fuel_Density <- 719.7 #g/L
  tmp$FC_L1 <- round(1000*tmp$MAF.g.s./14.7/(1+tmp$LongTermFuelTrim_B1/100)/Fuel_Density, 4) #ml/s
  tmp$FC_S1 <- round(1000*tmp$MAF.g.s./14.7/(1+tmp$ShortTermFuelTrim_B1/100)/Fuel_Density, 4) #ml/s
  
  #write tmp into folder Raw-V1
  if(file.exists(paste0(path, '-V1'))){
    setwd(paste0(path, '-V1'))
  } else {
    dir.create(paste0(path, '-V1'))
    setwd(paste0(path, '-V1'))
  }
  write.csv(tmp, file=file.name[i], row.names = F)

}



##### Merge multiple files into one large file #####
file.name <- dir(paste0(path, '-V1'))
setwd(paste0(path, '-V1'))
total <- do.call("rbind", lapply(file.name, read.csv, header = TRUE))
total$Veh_Speed.km.h. <- round(total$Veh_Speed.km.h./3.6, 4) #change km/h to m/s
names(total)[names(total)=='Veh_Speed.km.h.'] <- 'Veh_Speed.m.s.'
write.csv(total, file = paste0(path,'-Regression.csv'), row.names = F)



##### Calibrate VT-Micro model #####

rm(list=setdiff(ls(), 'path'))
df <- read.csv(file = paste0(path,'-Regression.csv'), stringsAsFactors = F)
#remove cold start at the beginning of the trip, as VT-Micro is hot stable condition
df <- subset(df, df$GPS_Time!=0)

vt <- df[,c('FC_L1', 'Veh_Speed.m.s.', 'Acceleration')]
colnames(vt) <- c('FC_L1', 'S', 'A')
vt$S2 <- vt$S^2
vt$S3 <- vt$S^3
vt$A2 <- vt$A^2
vt$A3 <- vt$A^3
vt$SA <- vt$S*vt$A
vt$S2A <- vt$S^2*vt$A
vt$S3A <- vt$S^3*vt$A
vt$SA2 <- vt$S*vt$A^2
vt$S2A2 <- vt$S^2*vt$A^2
vt$S3A2 <- vt$S^3*vt$A^2
vt$SA3 <- vt$S*vt$A^3
vt$S2A3 <- vt$S^2*vt$A^3
vt$S3A3 <- vt$S^3*vt$A^3

vt_acc <- subset(vt, vt$A>=0 & vt$FC_L1>0)
vt_dec <- subset(vt, vt$A<0 & vt$FC_L1>0)

#acceleration mode
fit_acc <- lm(data=vt_acc, log(FC_L1) ~ S + S2 + S3 + 
                A + SA + S2A + S3A +
                A2 + SA2 + S2A2 + S3A2 + 
                A3 + 
                SA3 + 
                S2A3 + 
                S3A3)
summary(fit_acc)
vt_acc$FC_fit <- exp(fit_acc$fitted.values)
plot(vt_acc$FC_L1, vt_acc$FC_fit)
cor(vt_acc$FC_L1, vt_acc$FC_fit)
summary(lm(data=vt_acc, vt_acc$FC_L1 ~ vt_acc$FC_fit))
par(mfrow=c(2,2))
plot(fit_acc)
library(car)
qqPlot(fit_acc,  id.method="identify",
       simulate=TRUE, main="Q-Q Plot")
library(gvlma)
summary(gvlma(fit_acc))
#MAPE
mean(abs((vt_acc$FC_L1-vt_acc$FC_fit)/vt_acc$FC_L1) * 100)
#AIC
AIC(fit_acc)
#RMSE
library(Metrics)
rmse(vt_acc$FC_L1, vt_acc$FC_fit)

#chi-square test
hist_L1 <- hist(vt_acc$FC_L1)
expected <- hist_L1$counts
expected[17] <- expected[17] + expected[18]
expected <- expected[1:17]
hist_fit <- hist(vt_acc$FC_fit)
observed <- hist_fit$counts
chisq.test(expected, observed)
chisq.test(observed, expected)

#deceleration mode
fit_dec <- lm(data=vt_dec, log(FC_L1) ~ S + S2 + S3 + 
                A + 
                SA + 
                S2A + S3A +
                A2 + 
                SA2 + 
                S2A2 + 
                S3A2
              +A3 + SA3 + S2A3 + S3A3
)
summary(fit_dec)
vt_dec$FC_fit <- exp(fit_dec$fitted.values)
plot(vt_dec$FC_L1[1:5000], vt_dec$FC_fit[1:5000])
cor(vt_dec$FC_L1, vt_dec$FC_fit)
summary(lm(data=vt_dec, vt_dec$FC_L1 ~ vt_dec$FC_fit))
#MAPE
mean(abs((vt_dec$FC_L1-vt_dec$FC_fit)/vt_dec$FC_L1) * 100)
#AIC
AIC(fit_dec)
#RMSE
library(Metrics)
rmse(vt_dec$FC_L1, vt_dec$FC_fit)

#chi-square test
hist_L1 <- hist(vt_dec$FC_L1, breaks = seq(0.0, 7.0, 0.5))
expected <- hist_L1$counts
hist_fit <- hist(vt_dec$FC_fit, breaks = seq(0.0, 7.0, 0.5))
observed <- hist_fit$counts
chisq.test(expected, observed)
chisq.test(observed, expected)


