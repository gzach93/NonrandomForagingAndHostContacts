#Packages
library(ggplot2)
library(tidyverse)
library(ggpubr)

#Load Data
abm.dat <- read.csv('OutputABMSimFinal.csv')
exp.dat <- read.csv('ExpPatchData.csv')

#Using Patch Time and # Patches From last tick in ABM
abm.dat <- abm.dat[abm.dat$ticks == 50,]


#Tank 12 Snail R - Floating
exp.dat[exp.dat$tank == 12 & exp.dat$snail == 'R','floating'] <- 1
#Tank 56 Snail l - on side at near 14:45mins
exp.dat[exp.dat$tank == 56 & exp.dat$snail == 'l','floating'][8:17] <- 1
#Tank 19 Snail RY - Floating
exp.dat[exp.dat$tank == 19 & exp.dat$snail == 'RY','floating'] <- 1
#Tank 14 Snail RP - Floating and on side
exp.dat[exp.dat$tank == 14 & exp.dat$snail == 'RP','floating'] <- 1

#Remove Snails that were on wall or floating
exp.dat <- exp.dat[exp.dat$floating == 0,]

#ABM Patch Duration Plots
ggplot(abm.dat, aes(x = stochasticity, y = report.patch.time,
                    color = as.factor(algae.distribution))) +
  geom_point() + theme_classic() +
  facet_wrap(~number.of.snails)


#Experimental Patch Duration Plots
ggplot(exp.dat, aes(x = aggregation, y = duration/60)) +
  geom_point() + theme_classic() +
  facet_wrap(~snail.density)

##################################################
#### Comparision of Number of Patches Visites ####
##################################################
#ABM Number of Patch Plots
ggplot(abm.dat, aes(x = stochasticity, y = report.patches.visited/4,
                    color = as.factor(algae.distribution))) +
  geom_point() + theme_classic() +
  facet_wrap(~number.of.snails)

#Get #Patches for Experimental Data
numpatch <- exp.dat %>%
            group_by(snail.density, aggregation, snail, tank) %>%
            tally()

#Experiemtal Number of Patch Plots
ggplot(numpatch, aes(x = aggregation, y = n)) +
  geom_point() + theme_classic() +
  facet_wrap(~snail.density)

#
colnames(abm.dat)
abm.dur <- abm.dat[,c(5,6,8,13)]
abm.dur <- abm.dur %>%
  group_by(stochasticity, algae.distribution) %>%
  summarise(mean = mean(report.patch.time), 
            lower.CI = quantile(report.patch.time, 0.025),
            high.CI = quantile(report.patch.time, 0.975))

adj.stochs <- c(0, 8, 18, 30, 50, 100)
abm.dur <- abm.dur[abm.dur$stochasticity %in% adj.stochs,]

exp.dur <- exp.dat %>%
           group_by(aggregation) %>%
           summarise(mean = mean(duration/60), 
                     lower.CI = quantile(duration/60, 0.025),
                     high.CI = quantile(duration/60, 0.975))

colnames(exp.dur) <- c('aggregation', 'mean', 'lower.CI', 'high.CI')
colnames(abm.dur) <- c('stochasticity', 'aggregation', 'mean', 'lower.CI', 'high.CI')
exp.dur$stochasticity <- 'Experiment'
abm.dur$stochasticity <- as.character(abm.dur$stochasticity)
new.dur <- rbind(exp.dur,abm.dur)

new.dur$stochasticity <- factor(new.dur$stochasticity, levels = c('Experiment', '0', '8',
                                                                  '18', '30', '50', 
                                                                  '100'))

dur.plot <- ggplot(new.dur, aes(x = stochasticity, y = mean,
                    color = as.factor(aggregation))) +
  geom_linerange(aes(ymin = lower.CI, ymax = high.CI),
                 position = position_dodge(width = .5)) +
  geom_point(position = position_dodge(width = .5)) + theme_classic() +
  labs(color = 'Aggregation Level') +
  ylab('Mean Patch Duration (mins)') +
  xlab('Stochasticity') +   theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) 
dur.plot

#Number patches
abm.num <- abm.dat[,c(5,6,8,19)]
abm.num <- abm.num %>%
  group_by(stochasticity, algae.distribution) %>%
  summarise(mean = mean(report.patches.visited/4), 
            lower.CI = quantile(report.patches.visited/4, 0.025),
            high.CI = quantile(report.patches.visited/4, 0.975))

adj.stochs <- c(0, 8, 18, 30, 50, 100)
abm.num <- abm.num[abm.num$stochasticity %in% adj.stochs,]

exp.num <- numpatch %>%
  group_by(aggregation) %>%
  summarise(mean = mean(n), 
            lower.CI = quantile(n, 0.025),
            high.CI = quantile(n, 0.975))

colnames(exp.num) <- c('aggregation', 'mean', 'lower.CI', 'high.CI')
colnames(abm.num) <- c('stochasticity', 'aggregation', 'mean', 'lower.CI', 'high.CI')
exp.num$stochasticity <- 'Experiment'
abm.num$stochasticity <- as.character(abm.num$stochasticity)
new.num <- rbind(exp.num,abm.num)

new.num$stochasticity <- factor(new.num$stochasticity, levels = c('Experiment', '0', '8',
                                                                  '18', '30', '50', 
                                                                  '100'))

num.plot <- ggplot(new.num, aes(x = stochasticity, y = mean,
                    color = as.factor(aggregation))) +
  geom_linerange(aes(ymin = lower.CI, ymax = high.CI), 
                 position = position_dodge(width = .5)) +
  geom_point(position = position_dodge(width = .5)) + theme_classic() +
  labs(color = 'Aggregation Level') +
  ylab('Mean Number of Patches Visited') +
  xlab('Stochasticity') +   theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) 

##FIGURE CODE
#pdf('~/Desktop/Figure4_Movement_9Dec23.pdf',
 # width = 11, height = 8)
ggarrange(dur.plot, num.plot, common.legend = T,
          legend = 'bottom') #, labels = c('A', 'B'))
#dev.off()







