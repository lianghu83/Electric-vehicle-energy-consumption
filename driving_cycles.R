### driving cycles from EPA


#change driving cycle and distance here
path <- "https://www.epa.gov/sites/production/files/2015-10/"
#UDDS, city driving conditions, light-duty vehicles
dc <- "uddscol" 
distance <- 7.45 #miles
#FTP, EPA75
dc <- "ftpcol" 
distance <- 11.04 #miles
#HWFET, highway driving conditions under 60 mph
dc <- "hwycol" 
distance <- 10.26 #miles
#NYCC, low speed stop-and-go traffic conditions
dc <- "nycccol"
distance <- 1.18 #miles
#US06, high accleration aggressive driving schedule that is often identified as Supplement FTP driving schedule
dc <- "us06col"
distance <- 8.01 #miles


#the driving cycle file
df <- read.table(paste0(path, dc, '.txt'), sep="\t", header=F, skip=2)
colnames(df) <- c('Time', 'Speed.mph')

df$S <- round(df$Speed.mph*0.44704, 4) #m/s
acc <- round(diff(df$S), 4)
df$A <- append(acc, 0, after=length(acc)) #unit: m/s/s

df$S2 <- df$S^2
df$S3 <- df$S^3
df$SA <- df$S*df$A
df$S2A <- df$S^2*df$A
df$S3A <- df$S^3*df$A
df$A2 <- df$A^2
df$SA2 <- df$S*df$A^2
df$S2A2 <- df$S^2*df$A^2
df$S3A2 <- df$S^3*df$A^2
df$A3 <- df$A^3
df$SA3 <- df$S*df$A^3
df$S2A3 <- df$S^2*df$A^3
df$S3A3 <- df$S^3*df$A^3


#write
write.csv(df, file=paste0('H:\\Driving cycles\\', dc, '.csv'), row.names=F)


