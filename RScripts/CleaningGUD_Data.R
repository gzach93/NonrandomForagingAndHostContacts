#Libraries
library(tidyverse)
library(ggplot2)
library(jmuOutlier)
library(lubridate)

#Load in Data
data <- read.csv('Data/GUD_Data.csv')
tank.info <- read.csv('Data/TankInfo_Data.csv')

tank.info <- tank.info[tank.info$replicate == 1,]
tank.info <- tank.info[,-c(1,5,6,7)]
tank.info <- distinct(tank.info)
new.dat <- merge(data, tank.info, by=c("tank","tank"))

new.dat$TotalArea <- as.numeric(new.dat$TotalArea)

summary(new.dat)

#Remove never moved because there is not GUD
new.dat <- new.dat[-which(new.dat$NeverMovedYN == 1),]

colnames(new.dat) <- c('tank', 'snail', 'movedYN', 'movetime', 'totalarea', 'greenarea',
                       'density', 'aggregation')

#Plotting
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

#Feeding rate
new.dat$minutes <- period_to_seconds(hms(new.dat$movetime))/60 #minutes till move
feeding <- (1- new.dat$gud)/new.dat$minutes #proportion of alage grazed per minute
summary(feeding)

