library(tidyverse)
library(ggplot2)
library(jmuOutlier)
library(lubridate)

data <- read.csv('~/Documents/snail_project/GUD/GUD_Data_7Feb22.csv')
data <- data[-c(1781:1986),] # Empty Rows at the Bottom
tank.info <- read.csv('~/Documents/snail_project/GUD/TankInfo_Data.csv')

tank.info <- tank.info[tank.info$replicate == 1,]
tank.info <- tank.info[,-c(1,5,6,7)]
tank.info <- distinct(tank.info)
new.dat <- merge(data, tank.info, by=c("tank","tank"))

#write.csv(new.dat, '~/Documents/snail_project/GUD/GUD_Data_7Feb22.csv', row.names=FALSE)
new.dat$TotalArea <- as.numeric(new.dat$TotalArea)

summary(new.dat)
new.dat[which(new.dat$TotalArea == 0),] #Error in the dataframe -- changing to NA
new.dat[which(new.dat$TotalArea == 0),'TotalArea'] <- NA

#Error with too large of GreenArea
new.dat[which(new.dat$GreenArea..in.2. > new.dat$TotalArea),] #need to take another look at row 1010 Tank 39 Snail RR

unique(new.dat[which(is.na(new.dat$TotalArea)),'Notes'])

#Error with Notes that should not correspond to NA in TotalArea
new.dat[which(is.na(new.dat$TotalArea & new.dat$Notes == "")),] #Check these Tank 7 Snail GG and Tank 36 Snail BB
new.dat[which(is.na(new.dat$TotalArea & new.dat$Notes == "same as M-45:00")),] #check tank 56 snail N

#Remove the NA's in TotalArea -- these should be snails not on an Algae Tile
new.dat <- new.dat[-which(is.na(new.dat$TotalArea)),]

#Remove never moved because there is not GUD
new.dat <- new.dat[-which(new.dat$NeverMovedYN == 1),]

colnames(new.dat) <- c('tank', 'replicate', 'snail', 'movedYN', 'movetime', 'totalarea', 'greenarea', 'notes',
                       'density', 'aggregation')

ggplot(new.dat, aes(x=greenarea)) + geom_histogram() + theme_classic()
ggplot(new.dat, aes(x=totalarea)) + geom_histogram() + theme_classic()

ggplot(new.dat, aes(x=greenarea, color=as.factor(density))) +
  geom_histogram(fill="white") + theme_classic()

ggplot(new.dat, aes(x=greenarea, color=as.factor(aggregation))) +
  geom_histogram(fill="white") + theme_classic()

ggplot(new.dat, aes(y=greenarea, x = as.numeric(density))) +
  geom_point() + theme_classic()

ggplot(new.dat, aes(y=greenarea, x = aggregation)) +
  geom_point() + theme_classic()

#What is giving up Density?
new.dat$gud <- new.dat$greenarea/new.dat$totalarea
summary(new.dat$gud) #median = 87.2%
#new.dat[which(gud == 1),]

#Feeding rate
#new.dat$fr <- new.dat$totalarea - new.dat$greenarea
new.dat$minutes <- period_to_seconds(hms(new.dat$movetime))/60 #minutes till move
feeding <- (1- new.dat$gud)/new.dat$minutes #proportion of alage grazed per minute
summary(feeding)#median = 0.035
