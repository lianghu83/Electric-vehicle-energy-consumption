### This file links vehicle GPS to Wavetronix traffic sensors located at Iowa interstates



setwd("C:\\...")
#Wavetronix sensor inventory
df <- read.csv("Wavetronix Inventory-8 16 2016.csv", stringsAsFactors = F)

#remove columns
df <- df[,-c(1:2, 4:6, 8)]

#remove error GPS
df <- subset(df, df$LatCorrect > 40 & df$LongCorrect < -89)

#remove ramp
library(stringr)
df <- subset(df, str_sub(df$ns1.lanes.type, -4, -1) != "ramp")
df <- subset(df, grepl("Ramp", df$ns1.detector.id) == F)
df <- subset(df, grepl("-R", df$ns1.detector.id) == F)
df <- subset(df, grepl("ramp", df$ns1.detector.id) == F)

#add number
rownames(df) <- 1:nrow(df)
df$ID <- as.numeric(rownames(df))
df <- df[, c(17, 1:16)]

#add new direction
df$direction.new <- df$ns1.approach.direction

#CB (Council Bluffs, IA)
df$direction.new[df$ID == 596] <- 's'
df$direction.new[df$ID == 597] <- 'n'
df$direction.new[df$ID == 573] <- 's'
df$direction.new[df$ID == 563] <- 'n'
df$direction.new[df$ID == 575] <- 's'
df$direction.new[df$ID == 565] <- 'n'
df$direction.new[df$ID == 650] <- 'w'
df$direction.new[df$ID == 627] <- 'w'
df$direction.new[df$ID == 626] <- 'e'
df$direction.new[df$ID == 629] <- 'e'
df$direction.new[df$ID == 630] <- 'w'
df$direction.new[df$ID == 645] <- 'e'
df$direction.new[df$ID == 567] <- 'e'
df$direction.new[df$ID == 577] <- 'w'
df$direction.new[df$ID == 568] <- 'e'
df$direction.new[df$ID == 578] <- 'w'
df$direction.new[df$ID == 569] <- 'e'
df$direction.new[df$ID == 579] <- 'w'
df$direction.new[df$ID == 570] <- 'e'
df$direction.new[df$ID == 580] <- 'w'
df$direction.new[df$ID == 638] <- 's'

#QC (Quad City, IA)
df$direction.new[df$ID == 55] <- 's'
df$direction.new[df$ID == 56] <- 'n'
df$direction.new[df$ID == 15] <- 's'
df$direction.new[df$ID == 14] <- 'n'
df$direction.new[df$ID == 13] <- 's'
df$direction.new[df$ID == 11] <- 'n'
df$direction.new[df$ID == 16] <- 's'
df$direction.new[df$ID == 12] <- 'n'
df$direction.new[df$ID == 17] <- 's'
df$direction.new[df$ID == 10] <- 'n'
df$direction.new[df$ID == 28] <- 's'
df$direction.new[df$ID == 27] <- 'n'
df$direction.new[df$ID == 18] <- 's'
df$direction.new[df$ID == 9] <- 'n'
df$direction.new[df$ID == 19] <- 's'
df$direction.new[df$ID == 8] <- 'n'
df$direction.new[df$ID == 20] <- 's'
df$direction.new[df$ID == 7] <- 'n'
df$direction.new[df$ID == 21] <- 's'
df$direction.new[df$ID == 6] <- 'n'
df$direction.new[df$ID == 23] <- 's'
df$direction.new[df$ID == 4] <- 'n'
df$direction.new[df$ID == 24] <- 's'
df$direction.new[df$ID == 3] <- 'n'
df$direction.new[df$ID == 25] <- 's'
df$direction.new[df$ID == 2] <- 'n'
df$direction.new[df$ID == 26] <- 's'
df$direction.new[df$ID == 1] <- 'n'

#IC and CR (Iowa City, Cedar Rapids, IA)
df$direction.new[df$ID == 680] <- 'n'
df$direction.new[df$ID == 681] <- 's'
df$direction.new[df$ID == 683] <- 's'
df$direction.new[df$ID == 685] <- 'e'
df$direction.new[df$ID == 684] <- 'w'
df$direction.new[df$ID == 686] <- 'e'
df$direction.new[df$ID == 687] <- 'w'
df$direction.new[df$ID == 703] <- 's'
df$direction.new[df$ID == 702] <- 'n'
df$direction.new[df$ID == 705] <- 's'
df$direction.new[df$ID == 704] <- 'n'
df$direction.new[df$ID == 716] <- 'e'
df$direction.new[df$ID == 717] <- 'w'

#WL (Waterloo, IA)
df$direction.new[df$ID == 501] <- 'e'
df$direction.new[df$ID == 524] <- 'w'
df$direction.new[df$ID == 503] <- 'e'
df$direction.new[df$ID == 523] <- 'w'
df$direction.new[df$ID == 504] <- 'e'
df$direction.new[df$ID == 522] <- 'w'
df$direction.new[df$ID == 505] <- 'e'
df$direction.new[df$ID == 521] <- 'w'
df$direction.new[df$ID == 506] <- 'e'
df$direction.new[df$ID == 520] <- 'w'
df$direction.new[df$ID == 507] <- 'e'
df$direction.new[df$ID == 519] <- 'w'
df$direction.new[df$ID == 544] <- 'w'

#SC (Sioux City, IA)
df$direction.new[df$ID == 342] <- 'e'
df$direction.new[df$ID == 343] <- 'w'
df$direction.new[df$ID == 304] <- 'n'
df$direction.new[df$ID == 305] <- 's'
df$direction.new[df$ID == 298] <- 'n'
df$direction.new[df$ID == 299] <- 's'
df$direction.new[df$ID == 301] <- 'n'
df$direction.new[df$ID == 300] <- 's'
df$direction.new[df$ID == 328] <- 'w'
df$direction.new[df$ID == 329] <- 'e'
df$direction.new[df$ID == 330] <- 'w'
df$direction.new[df$ID == 331] <- 'e'
df$direction.new[df$ID == 332] <- 'w'
df$direction.new[df$ID == 333] <- 'e'
df$direction.new[df$ID == 355] <- 'w'
df$direction.new[df$ID == 354] <- 'e'
df$direction.new[df$ID == 351] <- 'w'
df$direction.new[df$ID == 350] <- 'e'
df$direction.new[df$ID == 334] <- 'w'
df$direction.new[df$ID == 335] <- 'e'
df$direction.new[df$ID == 344] <- 'w'
df$direction.new[df$ID == 345] <- 'e'
df$direction.new[df$ID == 346] <- 'w'
df$direction.new[df$ID == 347] <- 'e'
df$direction.new[df$ID == 336] <- 'w'
df$direction.new[df$ID == 337] <- 'e'
df$direction.new[df$ID == 338] <- 'w'
df$direction.new[df$ID == 339] <- 'e'

#Ames and Des Moines, IA
df$direction.new[df$ID == 132] <- 'w'
df$direction.new[df$ID == 133] <- 'e'
df$direction.new[df$ID == 134] <- 'w'
df$direction.new[df$ID == 135] <- 'e'
df$direction.new[df$ID == 207] <- 's'
df$direction.new[df$ID == 206] <- 'n'
df$direction.new[df$ID == 205] <- 's'
df$direction.new[df$ID == 204] <- 'n'
df$direction.new[df$ID == 66] <- 's'
df$direction.new[df$ID == 65] <- 'n'
df$direction.new[df$ID == 64] <- 's'
df$direction.new[df$ID == 63] <- 'n'
df$direction.new[df$ID == 203] <- 's'
df$direction.new[df$ID == 202] <- 'n'
df$direction.new[df$ID == 201] <- 's'
df$direction.new[df$ID == 200] <- 'n'
df$direction.new[df$ID == 68] <- 's'
df$direction.new[df$ID == 67] <- 'n'
df$direction.new[df$ID == 70] <- 's'
df$direction.new[df$ID == 69] <- 'n'
df$direction.new[df$ID == 101] <- 'n'
df$direction.new[df$ID == 102] <- 's'
df$direction.new[df$ID == 229] <- 'n'
df$direction.new[df$ID == 230] <- 's'
df$direction.new[df$ID == 97] <- 'n'
df$direction.new[df$ID == 98] <- 's'

#output
write.csv(df, file = "Wavetronix Inventory-8 16 2016-Modified.csv", row.names = F)

#separate cities
quad.city <- subset(df, df$ns1.link.ownership == "IADOT-QC")
council.bluffs <- subset(df, df$ns1.link.ownership == "IADOT-CB")
iowa.city <- subset(df, df$ns1.link.ownership == "IADOT-IC")
ames <- subset(df, df$ns1.link.ownership == "IADOT-AM")
des.moines <- subset(df, df$ns1.link.ownership == "IADOT-DSM")
cedar.rapids <- subset(df, df$ns1.link.ownership == "IADOT-CR")
sioux.city <- subset(df, df$ns1.link.ownership == "IADOT-SC")


