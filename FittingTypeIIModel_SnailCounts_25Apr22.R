#Packages
library(ggplot2)
library(R2jags)
library(dplyr)
library(reshape2)
library(ggpubr)

#Experimental Data
#What is Handling Time and Does it Change with Algae Pattern and Snail Density?
#Load in Data
dur.dat <- read.csv('~/Documents/snail_project/FinalFiles/ResourceDistributionExperiment_ContactData.csv')

#Only Working with e data
#dur.dat <- dur.dat[dur.dat$ObservationPeriod == 'e',]

ggplot(dur.dat, aes(x = Density, y = Duration)) +
  geom_point() + theme_classic() +
  facet_wrap(~AggregationIndex)

ggplot(dur.dat, aes(x = Density, y = log(Duration))) +
  geom_point() + theme_classic() +
  facet_wrap(~AggregationIndex)

#mod1 <- lm(log(Duration)~Density + AggregationIndex, dur.dat)
#summary(mod1)

#Contact Duration (Our Proxy for Handling Time) is same Across Snail Density and Algae Pattern
dur.mean <- dur.dat %>%
  group_by(Trial, Tank, AggregationIndex, Density) %>%
  summarise(duration = median(Duration))

ggplot(dur.mean, aes(x = Density, y = duration)) +
  geom_point() + theme_classic() +
  facet_wrap(~AggregationIndex)

##Experimental Data 
#Fitting Hollings Type II Function to Contact Data Across Snail Densities

#Load in Data
exp.dat <- read.csv('~/Documents/snail_project/SnailExpData_23Feb22.csv')

#Plot Data
ggplot(exp.dat, aes(y = Count, x = SnailDensityFix)) +
  geom_point() + theme_classic() +
  facet_wrap(~AggregationIndex)

#Unique Tank for rep 2  
exp.dat[exp.dat$Trial == 2, 'Tank'] <- exp.dat[exp.dat$Trial == 2, 'Tank'] + 56


##################  Mean Values Model ##############
#Based on Contact Duration Time in Exp -- we assume handling time is same
#Need to Vary encounter rate

#Prior for Handling Rate
hist(dur.dat$Duration,freq = F)
#lines(0:8,dnorm(0:8, mean = 4, sd = 2)) #presicion = 1/2^2 = .25 
#median(dur.dat$Duration) #55 seconds -- overall

H.values <- dur.dat %>%
            group_by(AggregationIndex) %>%
            summarise(median_dur = median(Duration)/60)

model <- "
model {
# Priors 
#H <- 55/60
#H ~ dnorm(4,.25) -- Using this super informative prior makes aggregation index encouter rate the same
sigma ~ dexp(0.1) # standard deviation
tau <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS

#algae Distrubution Effect
for(j in 1:NAlgae){
a[j] ~ dnorm(0,1)
}

# Likelihood 
for (i in 1:N.obs) {
y[i] ~ dnorm(mu[i], tau)
mu[i]<-((exp(a[algae.dist[i]]))*TOTALTIME*(N[i]))/(1+exp(a[algae.dist[i]])*(H[i]/1)*(N[i]-1))
}

#Model Fits
 for(j in 1:NAlgae){
     for(i in 1:N.obs.test){
pred[j,i] ~ dnorm(pred.mu[j,i], tau)
pred.mu[j,i] <- ((exp(a[j]))*TOTALTIME*(Ntest[i]))/(1+exp(a[j])*(H[i]/1)*(Ntest[i]-1))
}
} 

}"

mean.dat <- exp.dat %>%
  group_by(Trial, AggregationIndex, SnailDensityFix) %>%
  summarise(Count = mean(Count))

mean.dat <- merge(x = H.values, y = mean.dat, by.x = 'AggregationIndex', 
      by.y = 'AggregationIndex', all.y = TRUE)


mean.dat$AggregationIndex <- mean.dat$AggregationIndex +1

y <- mean.dat$Count #Counts
N <- mean.dat$SnailDensityFix #Density
#Number of Rows
N.obs <- nrow(mean.dat)
#How Many Algae Patterns
NAlgae <- length(unique(mean.dat$AggregationIndex))
#Time of Experiment
TOTALTIME <- 45
#Algae Pattern
algae.dist <- mean.dat$AggregationIndex
H <- mean.dat$median_dur/60

#Test Data
Ntest <- seq(0,16, by = 2) #Snail Densities from 0 to 16 by 1
N.obs.test <- length(Ntest) 

data <- list(y=y, N=N, algae.dist=algae.dist, TOTALTIME=TOTALTIME, H = H,
             N.obs = N.obs, NAlgae = NAlgae, Ntest = Ntest, N.obs.test = N.obs.test)

params <- c("a", "sigma", "pred") #H

inits <- function() {list(a=rep(0, NAlgae))} 
                          #H=rnorm(1, mean = 4, sd = 2)) }

outTypeIIcontacts <- jags(data = data, inits = inits, 
                          parameters.to.save = params,
                          n.chains = 3, n.iter = 20000, n.burnin = 10000, 
                          model.file = textConnection(model), DIC=TRUE)

outTypeIIcontacts.mcmc <- as.mcmc(outTypeIIcontacts)

plot(outTypeIIcontacts.mcmc[,c(1:8,72)])


preds <- outTypeIIcontacts$BUGSoutput$sims.matrix[,-c(1:8,72)]

par(mfrow = c(1,1))
plot(Count~SnailDensityFix, mean.dat, 
     col = mean.dat$AggregationIndex,
     xlab = 'Snail Density',
     ylab = 'Average # Contacts',
     xlim = c(0, 16))

cnts1<-seq(1, dim(preds)[2], by=7)

for(i in 1:7){
  cnts<-cnts1+(i-1)
  #lines(Ntest, apply(preds[,cnts], 2, quantile, probs=0.025), lty = 2, col=i )
  #lines(Ntest,apply(preds[,cnts], 2, quantile, probs=0.975), lty = 2, col=i )
  lines(Ntest, colMeans(preds[,cnts]), col=i)
}
legend('topleft', fill = c(1:8), legend = unique(mean.dat$AggregationIndex))


hist(outTypeIIcontacts$BUGSoutput$sims.matrix[,'a[1]'], col = 'blue',
     main = '', #xlim = c(-7.5, -5),
     ylim = c(0, 3), freq = F)
hist(outTypeIIcontacts$BUGSoutput$sims.matrix[,'a[2]'], col = 'red', add = T, freq = F)
hist(outTypeIIcontacts$BUGSoutput$sims.matrix[,'a[3]'], col = 'green', add = T, freq = F)
hist(outTypeIIcontacts$BUGSoutput$sims.matrix[,'a[4]'], col = 'black', add = T, freq = F)
hist(outTypeIIcontacts$BUGSoutput$sims.matrix[,'a[5]'], col = 'grey', add = T, freq = F)
hist(outTypeIIcontacts$BUGSoutput$sims.matrix[,'a[6]'], col = 'yellow', add = T, freq = F)
hist(outTypeIIcontacts$BUGSoutput$sims.matrix[,'a[7]'], col = 'purple', add = T, freq = F)
abline(v = mean(outTypeIIcontacts$BUGSoutput$sims.matrix[,'a[1]']), lwd = 3, col = 'blue')
abline(v = mean(outTypeIIcontacts$BUGSoutput$sims.matrix[,'a[2]']), lwd = 3, col = 'red')
abline(v = mean(outTypeIIcontacts$BUGSoutput$sims.matrix[,'a[3]']), lwd = 3, col = 'green')
abline(v = mean(outTypeIIcontacts$BUGSoutput$sims.matrix[,'a[4]']), lwd = 3, col = 'black')
abline(v = mean(outTypeIIcontacts$BUGSoutput$sims.matrix[,'a[5]']), lwd = 3, col = 'grey')
abline(v = mean(outTypeIIcontacts$BUGSoutput$sims.matrix[,'a[6]']), lwd = 3, col = 'yellow')
abline(v = mean(outTypeIIcontacts$BUGSoutput$sims.matrix[,'a[7]']), lwd = 3, col = 'purple')
legend('topleft', fill = c('blue', 'red', 'green', 
                           'black', 'grey', 'yellow', 
                           'purple'),
       legend = c(0:6))

x <- melt(outTypeIIcontacts$BUGSoutput$sims.matrix[,c(1:7)])
x$aggregationindex <- NA
x[x$Var2 == 'a[1]', 'aggregationindex'] <- 0
x[x$Var2 == 'a[2]', 'aggregationindex'] <- 1
x[x$Var2 == 'a[3]', 'aggregationindex'] <- 2
x[x$Var2 == 'a[4]', 'aggregationindex'] <- 3
x[x$Var2 == 'a[5]', 'aggregationindex'] <- 4
x[x$Var2 == 'a[6]', 'aggregationindex'] <- 5
x[x$Var2 == 'a[7]', 'aggregationindex'] <- 6


ggplot(x, aes(x = aggregationindex, y = value)) +
  geom_point() + theme_classic() +
  ylab('Encounter Rate') + xlab('Aggregation Index')
#4 is lower than expected -- this is the 4 squares algae


preds.long <- melt(preds)
preds.long$AggregationIndex <- as.numeric(substr(preds.long$Var2, 6,6))
preds.long$AggregationIndex <- preds.long$AggregationIndex - 1
preds.long$SnailDensity <- as.numeric(substr(preds.long$Var2, 8,8))
preds.long[preds.long$SnailDensity == 1, 'SnailDensity'] <- 0

preds.long[preds.long$SnailDensity == 6, 'SnailDensity'] <- 10
preds.long[preds.long$SnailDensity == 7, 'SnailDensity'] <- 12
preds.long[preds.long$SnailDensity == 8, 'SnailDensity'] <- 14
preds.long[preds.long$SnailDensity == 9, 'SnailDensity'] <- 16
preds.long[preds.long$SnailDensity == 2, 'SnailDensity'] <- 2
preds.long[preds.long$SnailDensity == 4, 'SnailDensity'] <- 6
preds.long[preds.long$SnailDensity == 3, 'SnailDensity'] <- 4
preds.long[preds.long$SnailDensity == 5, 'SnailDensity'] <- 8


ggplot(preds.long, aes(x = SnailDensity, y = value)) +
  geom_point() + theme_classic() + 
  facet_wrap(~AggregationIndex)

sum.preds <- preds.long %>%
  group_by(SnailDensity, AggregationIndex) %>%
  summarise(mean = mean(value),
            lower.CI = quantile(value, 0.025),
            upper.CI = quantile(value, 0.975))

mean.dat$AggregationIndex <- mean.dat$AggregationIndex - 1

ggplot(sum.preds, aes(x = SnailDensity, y = mean)) +
  geom_line() + 
  geom_line(aes(x = SnailDensity, y = lower.CI), linetype = 'dashed') +
  geom_line(aes(x = SnailDensity, y = upper.CI), linetype = 'dashed') +
  theme_classic() + 
  facet_wrap(~AggregationIndex) + 
  xlab('Snail Density') + ylab('Mean # Snail Contacts') +
  geom_point(data = mean.dat, aes(x = SnailDensityFix, y = Count))

##### ABM Model Fitting #######
#source('~/Documents/snail_project/nlogo_cleaner.r')
#dat <- read.csv('~/Desktop/SnailSimsFinal_14Mar23/FinalSims_15May23/FinalSim_15May23.csv',
 #                 header = F)
#new.dat <- nlogo.cleaner(num.set = 10, num.runs = 10, 
 #                        num.sum = 6, num.out = 11, 
  #                       ticks = 50, num.combs = 336, 
   #                      dat = dat)
#write.csv(new.dat,'~/Desktop/SnailSimsFinal_14Mar23/FinalSims_15May23/OutputABMSimFinal_15May23.csv')

abm.dat <- read.csv('~/Desktop/SnailSimsFinal_14Mar23/FinalSims_15May23/OutputABMSimFinal_15May23.csv')
#abm.dat <- read.csv('~/Desktop/OutputABMSimnolimit_2Sep22.csv')
#abm.dat <- read.csv('~/Desktop/SnailSimsFinal_14Mar23/OutputABMSimFinal_14Mar23.csv')

abm.dat <- abm.dat[,-1]
colnames(abm.dat) <- c('run', 'start_infections', 'feeding', 'algae_conc',
                        'snails', 'stoch',
                        'infect', 'algae', 'recover', 'GUD', 'resources',
                        'edge', 'patch_time',
                        'Ss', 'Is', 'Rs', 'contacts', 'total_contacts',
                        'patches','unique_contacts', 'contact_dur', 'ticks')

stochs <- c(0, 8, 18, 30, 50, 100) #This is the stochasticites that I am running fitting the models on
abm.dat$algae <- abm.dat$algae + 1

#Contacts are double counted in the simulation (A-B and B-A)
abm.dat$contacts <- abm.dat$contacts / 2

#optimal foraging have longer contact duration -- proabably because snails are grouping on food
#No differences between algae distrubutions
ggplot(abm.dat[abm.dat$ticks == 50,], 
       aes(x = stoch, y = contact_dur, 
                    color = as.factor(algae))) +
  geom_point() + theme_classic() +
  facet_wrap(~snails)


ggplot(abm.dat[abm.dat$ticks == 50,], 
       aes(x = snails, y = contacts, 
           color = as.factor(stoch))) +
  geom_point() + theme_classic() +
  facet_wrap(~algae)

amb.median.dur <- abm.dat[abm.dat$ticks == 50,] %>%
  group_by(stoch, algae) %>%
  summarize(median_dur = median(contact_dur))

model.abm <- "
model {
# Priors 
#H ~dunif(0,50)
sigma ~ dexp(0.1) # standard deviation
tau <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS

#algae Distrubution Effect
for(j in 1:NAlgae){
a[j] ~ dnorm(0,1)
}

# Likelihood 
for (i in 1:N.obs) {
y[i] ~ dnorm(mu[i], tau)
mu[i]<-((exp(a[algae.dist[i]]))*TOTALTIME*(N[i]))/(1+exp(a[algae.dist[i]])*(H[i]/1)*(N[i]-1))
}

#Model Fits
for(j in 1:NAlgae){
for(i in 1:N.obs.test){
pred[j,i] ~ dnorm(pred.mu[j,i], tau)
pred.mu[j,i] <- ((exp(a[j]))*TOTALTIME*(Ntest[i]))/(1+exp(a[j])*(H[i]/1)*(Ntest[i]-1))
}
} 

}"


abm.dat.50 <- abm.dat[abm.dat$ticks == 50,]

abm.model.data <- merge(x = abm.dat.50, y = amb.median.dur, by.x = c('stoch', 'algae'),
                        by.y = c('stoch', 'algae'), all.x = TRUE)



#Run Each Stoch Seperatly
results <- plyr::dlply(.data = abm.model.data, .variables = "stoch", .fun = function(x){
  y <- x$contacts #Counts - divded by 2 above when data is loaded in
  N <- x$snails/4 #Density
  #Number of Rows
  N.obs <- nrow(x)
  #How Many Algae Patterns
  NAlgae <- length(unique(x$algae))
  #Time of Experiment
  TOTALTIME <- 50
  #Algae Pattern
  algae.dist <- x$algae
  #Test Data
  Ntest <- seq(0,16, by = 2) #Snail Densities from 0 to 16 by 1
  N.obs.test <- length(Ntest) 
  H <- x$median_dur
  
  data <- list(y=y, N=N, algae.dist=algae.dist, TOTALTIME=TOTALTIME,
               N.obs = N.obs, NAlgae = NAlgae, Ntest = Ntest, N.obs.test = N.obs.test,
               H = H)
  
  params <- c("a", "sigma", "pred")
  
  inits <- function() {list(a=rep(-10, NAlgae))} 
                            #H=runif(1, 0, 50)) }
  
  outTypeIIcontacts <- jags(data = data, inits = inits, 
                            parameters.to.save = params,
                            n.chains = 3, n.iter = 7000, n.burnin = 5000, 
                            model.file = textConnection(model.abm), DIC=TRUE)
  
  #outTypeIIcontacts.mcmc <- as.mcmc(outTypeIIcontacts)
  #outTypeIIcontacts.summary <- summary(outTypeIIcontacts.mcmc, q=c(0.025, 0.975)); outTypeIIcontacts.summary
  
  return(outTypeIIcontacts)
})
#saveRDS(results, '~/Desktop/SnailSimsFinal_14Mar23/FinalSims_15May23/ABMSnailSims_R0_15May23.RDS')
results <- readRDS('~/Desktop/SnailSimsFinal_14Mar23/FinalSims_15May23/ABMSnailSims_R0_15May23.RDS')
#results <- readRDS('~/Desktop/SnailSimsFinal_14Mar23/ABMSnailSims_14Mar23.RDS')
#saveRDS(results, '~/Documents/snail_project/FinalABMSnailSims/ABMResults_31Jan23.RDS')
#results <- readRDS('~/Desktop/ABMResultsnolimit_2Sep22.RDS')
#results <- readRDS('~/Documents/snail_project/FinalABMSnailSims/ABMResults_31Jan23.RDS')
plot(as.mcmc(results[[1]])[,c(1:7, 8, 72)])

#acf(as.mcmc(results[[7]])[[3]][,3])

encounter.rates <- data.frame(rbind(results[[1]]$BUGSoutput$sims.matrix[,1:7],
                     results[[2]]$BUGSoutput$sims.matrix[,1:7],
                     results[[3]]$BUGSoutput$sims.matrix[,1:7],
                     results[[4]]$BUGSoutput$sims.matrix[,1:7],
                     results[[5]]$BUGSoutput$sims.matrix[,1:7],
                     results[[6]]$BUGSoutput$sims.matrix[,1:7]))
                     #results[[7]]$BUGSoutput$sims.matrix[,1:7]))
                     #results[[8]]$BUGSoutput$sims.matrix[,1:7],
                     #results[[9]]$BUGSoutput$sims.matrix[,1:7],
                     #results[[10]]$BUGSoutput$sims.matrix[,1:7],
                     #results[[11]]$BUGSoutput$sims.matrix[,1:7],
                     #results[[12]]$BUGSoutput$sims.matrix[,1:7])
encounter.rates$stochs <- rep(stochs, each = 3000)

encounter.rates <- melt(encounter.rates, id.vars  = 'stochs')

encounter.means <- encounter.rates %>%
  group_by(stochs, variable) %>%
  summarise(mean = mean(value),
            lower.ci = quantile(value, 0.025),
            upper.ci = quantile(value, 0.975))

encounter.means$variable <- as.character(encounter.means$variable)
encounter.means[encounter.means$variable == 'a.1.','variable'] <- '0'
encounter.means[encounter.means$variable == 'a.2.','variable'] <- '1'
encounter.means[encounter.means$variable == 'a.3.','variable'] <- '2'
encounter.means[encounter.means$variable == 'a.4.','variable'] <- '3'
encounter.means[encounter.means$variable == 'a.5.','variable'] <- '4'
encounter.means[encounter.means$variable == 'a.6.','variable'] <- '5'
encounter.means[encounter.means$variable == 'a.7.','variable'] <- '6'
encounter.means$variable <- as.numeric(encounter.means$variable)

adj.stochs <- c(0, 8, 18, 30, 50, 100)
x$stochs <- 'Experiment'

x.means <- x %>%
  group_by(stochs, aggregationindex) %>%
  summarise(mean = mean(value),
            lower.ci = quantile(value, 0.025),
            upper.ci = quantile(value, 0.975))

ggplot(encounter.means[encounter.means$stochs %in% adj.stochs,], aes(y = exp(mean), x = variable)) +  geom_point() + theme_classic() +
  geom_errorbar(data = encounter.means, aes(ymin = exp(lower.ci), ymax = exp(upper.ci), width = 0.2)) +
  facet_wrap(~stochs) + 
  ylab('Encounter Rate') + xlab('Aggregation Index')

x.means$variable <- x.means$aggregationindex

encounter.means.adj <- encounter.means[encounter.means$stochs %in% adj.stochs,]
encounter.means.adj$stochs <- as.character(encounter.means.adj$stochs)

all.encounter <- rbind(encounter.means.adj, x.means[,-2])


all.encounter$stochs <- factor(all.encounter$stochs, 
                        levels = c('0', '8', '18', '30', '50', '100',
                                   'Experiment'))

encounter.plot <- ggplot(all.encounter, aes(y = exp(mean), x = variable,
                            color = as.factor(stochs))) +
  geom_point() + theme_classic() + 
  geom_line() +
  geom_linerange(data = all.encounter, aes(ymin = exp(lower.ci), ymax = exp(upper.ci),
                                                                                     x = variable,
                                            width = 0.2, color = as.factor(stochs))) +
  ylab('Encounter Rate') + xlab('Aggregation Index') +
  #geom_point(data = x.means, aes(x = stochs, y = exp(mean))) +
  #geom_errorbar(data = x.means, 
   #             aes(x = stochs, ymin = exp(lower.ci), ymax = exp(upper.ci), 
    #                width = 0.2, color = as.factor(variable))) +
  scale_color_manual(values = c("0" = "#FDE725FF",
                                #"4" = "#CD9600",
                                "8" = "#73D055FF",
                                #"10" = "#7CAE00",
                                #"14" = "#0CB702",
                                "18" = "#29AF2FFF",
                                #"22" = "#00C19A",
                                #"26" = "#00B8E7",
                                "30" = "#2D708EFF",
                                #"40" = "#8494FF",
                                "50" = "#404788FF",
                                "100" = "#440154FF",
                                "Experiment" = "black")) +
  labs(color = 'Stochasticity') + theme(legend.position="bottom") + 
  guides(color = guide_legend(nrow = 1))
encounter.plot

#handling.time <- data.frame(c(results[[1]]$BUGSoutput$sims.matrix[,1],
 #                                   results[[2]]$BUGSoutput$sims.matrix[,1],
  #                                  results[[3]]$BUGSoutput$sims.matrix[,1],
   #                                 results[[4]]$BUGSoutput$sims.matrix[,1],
    #                                results[[5]]$BUGSoutput$sims.matrix[,1],
     #                               results[[6]]$BUGSoutput$sims.matrix[,1],
      #                              results[[7]]$BUGSoutput$sims.matrix[,1],
       #                             results[[8]]$BUGSoutput$sims.matrix[,1],
        #                            results[[9]]$BUGSoutput$sims.matrix[,1],
         #                           results[[10]]$BUGSoutput$sims.matrix[,1],
          #                          results[[11]]$BUGSoutput$sims.matrix[,1],
           #                         results[[12]]$BUGSoutput$sims.matrix[,1]))
#handling.time$stochs <- rep(stochs, each = 3000)
#colnames(handling.time) <- c('H', 'stochs')

#handling.means <- handling.time %>%
 # group_by(stochs) %>%
  #summarise(mean = mean(H),
   #         lower.ci = quantile(H, 0.025),
    #        upper.ci = quantile(H, 0.975))

#ggplot(handling.means, aes(y = mean, x = stochs)) +
 # geom_point() + theme_classic() +
  #geom_errorbar(data = handling.means, aes(ymin = lower.ci, ymax = upper.ci, width = 0.2)) +
  #ylab('Handling Time') + xlab('Stochasticity')



preds.abm <- data.frame(rbind(results[[1]]$BUGSoutput$sims.matrix[,9:71],
                           results[[2]]$BUGSoutput$sims.matrix[,9:71],
                           results[[3]]$BUGSoutput$sims.matrix[,9:71],
                           results[[4]]$BUGSoutput$sims.matrix[,9:71],
                           results[[5]]$BUGSoutput$sims.matrix[,9:71],
                           results[[6]]$BUGSoutput$sims.matrix[,9:71]))
                           #results[[7]]$BUGSoutput$sims.matrix[,9:71],
                           #results[[8]]$BUGSoutput$sims.matrix[,9:71],
                           #results[[9]]$BUGSoutput$sims.matrix[,9:71],
                           #results[[10]]$BUGSoutput$sims.matrix[,9:71],
                           #results[[11]]$BUGSoutput$sims.matrix[,9:71],
                           #results[[12]]$BUGSoutput$sims.matrix[,9:71]))
preds.abm$stoch <- rep(stochs, each = 3000)
preds.abm <- melt(preds.abm, id.vars  = 'stoch')

preds.abm$AggregationIndex <- as.numeric(substr(preds.abm$variable, 6,6))
preds.abm$AggregationIndex <- preds.abm$AggregationIndex - 1
preds.abm$SnailDensity <- as.numeric(substr(preds.abm$variable, 8,8))
preds.abm[preds.abm$SnailDensity == 1, 'SnailDensity'] <- 0
preds.abm[preds.abm$SnailDensity == 6, 'SnailDensity'] <- 10
preds.abm[preds.abm$SnailDensity == 7, 'SnailDensity'] <- 12
preds.abm[preds.abm$SnailDensity == 8, 'SnailDensity'] <- 14
preds.abm[preds.abm$SnailDensity == 9, 'SnailDensity'] <- 16
preds.abm[preds.abm$SnailDensity == 2, 'SnailDensity'] <- 2
preds.abm[preds.abm$SnailDensity == 4, 'SnailDensity'] <- 6
preds.abm[preds.abm$SnailDensity == 3, 'SnailDensity'] <- 4
preds.abm[preds.abm$SnailDensity == 5, 'SnailDensity'] <- 8


sum.preds.abm <- preds.abm %>%
  group_by(SnailDensity, AggregationIndex, stoch) %>%
  summarise(mean = mean(value),
            lower.CI = quantile(value, 0.025),
            upper.CI = quantile(value, 0.975))

abm.dat.50$AggregationIndex <- abm.dat.50$algae

abm.dat.mean <- abm.dat.50 %>%
  group_by(snails, AggregationIndex, stoch) %>%
  summarise(mean = mean(contacts))

ggplot(sum.preds.abm, aes(x = SnailDensity, y = mean,
                          color = as.factor(AggregationIndex))) +
  geom_line() + 
  geom_line(aes(x = SnailDensity, y = lower.CI, 
                color = as.factor(AggregationIndex)), linetype = 'dashed') +
  geom_line(aes(x = SnailDensity, y = upper.CI, 
                color = as.factor(AggregationIndex)), linetype = 'dashed') +
  theme_classic() + 
  facet_wrap(~stoch) + 
  xlab('Snail Density') + ylab('Mean # Snail Contacts') +
  geom_point(data = abm.dat.mean, aes(x = snails/4, y = mean,
                                 color = as.factor(AggregationIndex)))

abm.dat.mean$AggregationIndex <- abm.dat.mean$AggregationIndex -1

sum.preds$stoch <- 'Experiment'
mean.dat$stoch <- 'Experiment'

adj.stochs <- c(0, 8, 18, 30, 50, 100)

ggplot(sum.preds.abm[sum.preds.abm$stoch %in% adj.stochs,], aes(x = SnailDensity, y = mean,
                                                                color = as.factor(stoch))) +
  geom_line() + 
  geom_line(aes(x = SnailDensity, y = lower.CI, 
                color = as.factor(stoch)), linetype = 'dashed') +
  geom_line(aes(x = SnailDensity, y = upper.CI, 
                color = as.factor(stoch)), linetype = 'dashed') +
  theme_classic() + 
  facet_wrap(~AggregationIndex, nrow = 1) + 
  xlab('Snail Density') + ylab('Mean # Snail Contacts Per Tank') +
  labs(color='Stochasticity') +
  geom_point(data = abm.dat.mean[abm.dat.mean$stoch %in% adj.stochs,], aes(x = snails/4, y = mean,
                                                                           color = as.factor(stoch)))

#mean.dat$AggregationIndex <- mean.dat$AggregationIndex -1
pred.plots <- ggplot() +
  geom_line(data = sum.preds, aes(x = SnailDensity, y = mean), lwd = 1) +
  #geom_line(data = sum.preds, aes(x = SnailDensity, y = lower.CI), linetype = 'dashed', lwd = 1) +
  #geom_line(data = sum.preds, aes(x = SnailDensity, y = upper.CI), linetype = 'dashed', lwd = 1) +
  geom_ribbon(data = sum.preds, aes(ymax = upper.CI, ymin = lower.CI, x = SnailDensity, fill = 'black'),color = NA, alpha = .5) +
  geom_point(data = mean.dat, aes(x = SnailDensityFix, y = Count), shape = 15) +
  geom_line(data = sum.preds.abm, aes(x = SnailDensity, y = mean,
                                                                     color = as.factor(stoch))) + 
  geom_ribbon(data = sum.preds.abm, aes(ymin=lower.CI, ymax=upper.CI, 
                  x = SnailDensity, fill=as.factor(stoch)), alpha = 0.3, color = NA,
              show.legend = FALSE) +
  theme_classic() + 
  facet_wrap(~AggregationIndex, nrow = 1) + 
  xlab('Snail Density') + 
  ylab('Mean # Snail Contacts Per Tank') +
  xlab('Number of Snails') +
  labs(color='Stochasticity') + 
  geom_point(data = abm.dat.mean[abm.dat.mean$stoch %in% adj.stochs,], aes(x = snails/4, y = mean,
                                      color = as.factor(stoch))) +
  scale_color_manual(values = c("0" = "#FDE725FF",
                                "8" = "#73D055FF",
                                "18" = "#29AF2FFF",
                                "30" = "#2D708EFF",
                                "50" = "#404788FF",
                                "100" = "#440154FF",
                                "Experiment" = "black")) +
  scale_fill_manual(values = c("0" = "#FDE725FF",
                                "8" = "#73D055FF",
                                "18" = "#29AF2FFF",
                                "30" = "#2D708EFF",
                                "50" = "#404788FF",
                                "100" = "#440154FF",
                                "Experiment" = "black"),
                    guide = "none") +
  theme(legend.position="bottom") + 
  guides(colour = guide_legend(nrow = 1)) +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
        #axis.ticks.x = element_blank(),
        #axis.text.x = element_blank()) + 
  theme(plot.margin = margin(t = 0,  # Top margin
                             b = 0)) + # Bottom margin
  coord_cartesian(ylim = c(0, 7.5))

pred.plots


#Algae HeatMaps 
alg.labs <- c('Algae Pattern = 0', 'Algae Pattern = 1', 'Algae Pattern = 2',
              'Algae Pattern = 3', 'Algae Pattern = 4', 'Algae Pattern = 5',
              'Algae Pattern = 6')
names(alg.labs) <- c(0,1,2,3,4,5,6)

maps <- data.frame('x' = rep(rep(1:6, times = 6),times = 7),
                   'y' = rep(rep(1:6, each = 6),times = 7),
                   algae = c(0,1,0,1,0,0,1,0,1,0,1,
                             0,0,1,0,1,0,1,1,0,1,0,1,
                             0,0,1,0,1,0,1,0,0,1,0,1,0, 0,0,1,0,1,0,0,1,0,1,0,0,1,
                             0,1,0,1,1,1,1,0,1,0,1,0,0,1,
                             0,1,0,0,1,0,1,0,0, 0,0,1,0,1,0,0,1,0,0,
                             1,0,1,0,1,0,1,1,1,1,0,
                             1,0,1,0,1,0,0,1,0,0,1,0,1,0,0,
                             0,1,0,0,1,0,1,1,0,0,1,1,0,0,1,1
                             ,0,0,0,0,1,1,0,0,1,1,0,0,1,1,0,1,0,0,1,0,
                             0,0,1,1,0,0,0,0,1,1,0,0,1,1,0,0,1,1,1,1,0,
                             0,1,1,0,0,1,1,0,0,0,0,1,1,0,0,
                             0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,1,
                             1,0,0,1,1,1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,
                             0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,
                             1,0,0,1,1,1,1,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
                   pattern = rep(0:6, each = 36))

map.plot <- ggplot(maps, aes(x,y,fill = as.factor(algae))) +
  geom_tile(color = 'black') + theme_void() + 
  scale_fill_manual(values=c('white', 'limegreen')) +
  facet_wrap(~pattern, nrow = 1,
             labeller = labeller(pattern = alg.labs)) +
  #theme(axis.title.x=element_blank(),
   #     axis.text.x=element_blank(),
    #    axis.ticks.x=element_blank(),
     #   axis.title.y=element_blank(),
      #  axis.text.y=element_blank(),
       # axis.ticks.y=element_blank()) +
  theme(legend.position="none") + 
  theme(plot.margin = margin(t = 0,  # Top margin
                            r = 0,  # Right margin
                            b = 0,  # Bottom margin
                            l = 35)) + # Left margin
theme(strip.background = element_blank(), strip.text.x = element_blank())
map.plot  

pdf('~/Desktop/SnailSimsFinal_14Mar23/Figure_Map_19June23.pdf',
  height = 1, width = 5)
map.plot
dev.off()

#Handling Time Plot
mean.exp.dur <- dur.dat %>%
  group_by(AggregationIndex) %>%
  summarise(mean = median(Duration/60),
            lower.ci = quantile(Duration/60, 0.025),
            upper.ci = quantile(Duration/60, 0.975))
mean.exp.dur$stoch <- 'Experiment'
mean.exp.dur$part <- 'A'

abm.dat.50.dur <- abm.dat.50 %>%
  group_by(stoch, algae) %>%
  summarise(mean = median(contact_dur),
            lower.ci = quantile(contact_dur, 0.025),
            upper.ci = quantile(contact_dur, 0.975))
abm.dat.50.dur$AggregationIndex <- abm.dat.50.dur$algae
abm.dat.50.dur$part <- 'B'
abm.dat.50.dur <- abm.dat.50.dur[abm.dat.50.dur$stoch %in% adj.stochs,]

abm.dat.50.dur$AggregationIndex <- abm.dat.50.dur$AggregationIndex - 1

ggplot(data = abm.dat.50.dur[abm.dat.50.dur$stoch %in% adj.stochs,], 
       aes(x = stoch, y = mean,
           color = as.factor(AggregationIndex))) +
  geom_point() + theme_classic() +
  geom_errorbar(data = abm.dat.50.dur[abm.dat.50.dur$stoch %in% adj.stochs,], 
                aes(x = stoch, ymin = lower.ci, ymax = upper.ci, 
                    width = 0.2, color = as.factor(AggregationIndex)))
  
ggplot(data = mean.exp.dur, aes(x = stoch, y = mean,
                                color = as.factor(AggregationIndex))) +
  geom_point() + theme_classic() +
  geom_errorbar(data = mean.exp.dur, 
               aes(x = stoch, ymin = lower.ci, ymax = upper.ci, 
                   width = 0.2, color = as.factor(AggregationIndex)))

abm.dat.50.dur$stoch <- as.character(abm.dat.50.dur$stoch)
mean.exp.dur$stoch <- as.character(mean.exp.dur$stoch)

all.dur <- rbind(abm.dat.50.dur[,c(1,3:7)], mean.exp.dur)

all.dur$stoch <- factor(all.dur$stoch, 
                        levels = c('0', '8', '18', '30', '50', '100',
                                   'Experiment'))


dur.plot <- ggplot(data = all.dur, aes(x = stoch, y = mean, 
                                color = as.factor(AggregationIndex))) +
  geom_point(position = position_dodge(width = 0.5)) + theme_classic() +
  geom_linerange(data = all.dur, 
                aes(x = stoch, ymin = lower.ci, ymax = upper.ci,
                   color = as.factor(AggregationIndex)),
                position = position_dodge(width = .5)) +
  ylab('Average Contact Duration (Handling Time)') + xlab('Stochasticity') +
  labs(color = 'Aggregation Index') +
  theme(legend.position = 'bottom') +
  guides(colour = guide_legend(nrow = 1)) 

dur.plot

#pdf('~/Desktop/SnailParamsPlots_18May22.pdf',
 #   height = 8, width = 11)
ggarrange(encounter.plot, dur.plot, common.legend = T,
          legend = 'bottom')
#dev.off()

pred.abm.plots <-ggplot(sum.preds.abm[sum.preds.abm$stoch %in% adj.stochs,], aes(x = SnailDensity, y = mean,
                                                                                 color = as.factor(stoch))) +
  geom_line() + 
  geom_ribbon(data = sum.preds.abm, aes(ymin=lower.CI, ymax=upper.CI, 
                  x = SnailDensity, fill=as.factor(stoch)), alpha = 0.2, color = NA,
              show.legend = FALSE) +
  theme_classic() + 
  facet_wrap(~AggregationIndex, nrow = 1) + 
  ylab('Mean # Snail Contacts Per Simulation') +
  xlab('') +
  labs(color='Stochasticity') + 
  geom_point(data = abm.dat.mean[abm.dat.mean$stoch %in% adj.stochs,], aes(x = snails/4, y = mean,
                                                                           color = as.factor(stoch))) +
    scale_color_manual(values = c("0" = "#FDE725FF",
                                "8" = "#73D055FF",
                                "18" = "#29AF2FFF",
                                "30" = "#2D708EFF",
                                "50" = "#404788FF",
                                "100" = "#440154FF")) + 
  theme(legend.position="bottom") + 
  guides(colour = guide_legend(nrow = 1)) +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  theme(plot.margin = margin(t = 0,  # Top margin
                             b = 0)) + # Bottom margin
  coord_cartesian(ylim = c(0, 7.5))

pred.abm.plots

pred.exp.plots <- ggplot() +
  theme_classic() + 
  facet_wrap(~AggregationIndex, nrow = 1) + 
  xlab('Snail Density') + 
  ylab('Mean # Snail Contacts Per Tank') +
  xlab('') +
  labs(color='Stochasticity') + 
  geom_line(data = sum.preds, aes(x = SnailDensity, y = mean)) +
  geom_ribbon(data = sum.preds, aes(x = SnailDensity, ymax = upper.CI, ymin = lower.CI), fill = 'grey',color = NA, alpha = 2) +
  geom_point(data = mean.dat, aes(x = SnailDensityFix, y = Count)) +
  scale_color_manual(values = c("0" = "#FDE725FF",
                                #"4" = "#CD9600",
                                "8" = "#73D055FF",
                                #"10" = "#7CAE00",
                                #"14" = "#0CB702",
                                "18" = "#29AF2FFF",
                                #"22" = "#00C19A",
                                #"26" = "#00B8E7",
                                "30" = "#2D708EFF",
                                #"40" = "#8494FF",
                                "50" = "#404788FF",
                                "100" = "#440154FF",
                                "Experiment" = "black")) +
  theme(legend.position="bottom") + 
  guides(colour = guide_legend(nrow = 1)) +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  #axis.ticks.x = element_blank(),
  #axis.text.x = element_blank()) + 
  theme(plot.margin = margin(t = 0,  # Top margin
                             b = 0)) + # Bottom margin
  coord_cartesian(ylim = c(0, 7.5))

pred.exp.plots

ggarrange(pred.abm.plots, pred.exp.plots, common.legend = T,
          legend = 'bottom', ncol = 1)

##########################################################
##########################################################
#################### ABM Disease #########################
##########################################################
##########################################################

findRo <- function(dat, tick){
  Ro.dat <- dat[dat$ticks == tick,]
  Ro.dat$contact.rate <- Ro.dat$contacts/Ro.dat$ticks 
  Ro.dat$transmission <- Ro.dat$infect/100
  Ro.dat$Ro <- (Ro.dat$contact.rate * Ro.dat$transmission)*Ro.dat$recover
  
  width<-function (z=1.96, lambda, N) {
    W<-z*sqrt(lambda/N)
    return(W)
  }
  
  outRo <- Ro.dat %>% 
    group_by(snails, stoch, algae) %>% 
    summarise(Ro = mean(Ro), numsnails = n()) %>%
    mutate(highCI = (Ro + width(lambda=Ro, N=numsnails)), 
           lowCI = (Ro - width(lambda=Ro, N=numsnails)))
  outRo$lowCI[outRo$lowCI<0]<-0
  
  outRo$adjust <- outRo$Ro
  outRo[outRo$adjust < 1, 'adjust'] <- NA
  
  return(outRo)
}

R0 <- findRo(abm.dat, tick = 50)  
R0$algae <- R0$algae - 1 

ggplot(R0[R0$stoch == 0 | R0$stoch == 100,], aes(x = snails/4, y = Ro)) + geom_point() +
  theme_classic() + facet_wrap(~algae) +
  geom_errorbar(aes(ymin=lowCI,ymax=highCI, x = snails/4),
                width = 1, position = position_dodge(width = 2))

#R0 Plot with Snail on X axis and colored by stochasticity
Ro.plot <- ggplot(R0, aes(x = snails/4, y = Ro, 
               color=as.factor(stoch))) +
  theme_classic() +
  geom_ribbon(data = R0, 
                aes(ymin=lowCI, ymax=highCI, x = snails/4,
                    fill=as.factor(stoch)),
                alpha = .2, show.legend = FALSE, color = NA) +
  geom_point() + 
  facet_wrap(~algae, nrow = 1) +
  scale_shape_manual(values=c(0:5)) +
  xlab('Snail Density') + 
  ylab(expression(paste("Mean ", R[0], sep = ""))) +
  labs(color = 'Stochasticity') +
  guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1)) +
  geom_hline(yintercept=1,linetype=2) +
  scale_color_manual(values = c("0" = "#FDE725FF",
                                "8" = "#73D055FF",
                                "18" = "#29AF2FFF",
                                "30" = "#2D708EFF",
                                "50" = "#404788FF",
                                "100" = "#440154FF",
                                "Experiment" = "black")) +
  scale_fill_manual(values = c("0" = "#FDE725FF",
                                "8" = "#73D055FF",
                                "18" = "#29AF2FFF",
                                "30" = "#2D708EFF",
                                "50" = "#404788FF",
                                "100" = "#440154FF",
                                "Experiment" = "black")) +
  theme(legend.position="bottom") + guides(colour = guide_legend(nrow = 1)) +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  xlim(0, 18) +
  theme(plot.margin = margin(t = 0,  # Top margin
                             b = 0)) # Bottom margin
Ro.plot

#R0 plot with stochasticty on x-axis and colored by algae distrubution
Ro.plot2 <- ggplot(R0, aes(x = stoch, y = Ro, 
                          color= as.factor(algae))) +
  theme_classic() +
  geom_ribbon(data = R0, 
              aes(ymin=lowCI, ymax=highCI, x = stoch,
                  fill=as.factor(algae)),
              alpha = .2, show.legend = FALSE, color = NA) +
  geom_point() + 
  facet_wrap(~snails, nrow = 1) +
  scale_shape_manual(values=c(0:5)) +
  xlab('Stochasticity') + 
  ylab(expression(paste("Mean ", R[0], sep = ""))) +
  labs(color = 'Resource Distribution') +
  guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1)) +
  geom_hline(yintercept=1,linetype=2) +
  #scale_color_manual(values = c("0" = "#FDE725FF",
   #                             "8" = "#73D055FF",
    #                            "18" = "#29AF2FFF",
     #                           "30" = "#2D708EFF",
      #                          "50" = "#404788FF",
       #                         "100" = "#440154FF",
        #                        "Experiment" = "black")) +
  #scale_fill_manual(values = c("0" = "#FDE725FF",
   #                            "8" = "#73D055FF",
    #                           "18" = "#29AF2FFF",
     #                          "30" = "#2D708EFF",
      #                         "50" = "#404788FF",
       #                        "100" = "#440154FF",
        #                       "Experiment" = "black")) +
  theme(legend.position="bottom") + guides(colour = guide_legend(nrow = 1)) +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  xlim(0, 100) +
  theme(plot.margin = margin(t = 0,  # Top margin
                             b = 0)) # Bottom margin
Ro.plot2


#R0 plot with denstiy on x axis and colored by resource distrubution
stoch.labels <- c('Stochasticity = 0',
                  'Stochasticity = 8',
                  'Stochasticity = 18',
                  'Stochasticity = 30',
                  'Stochasticity = 50',
                  'Stochasticity = 100',
                  'Experiment')
names(stoch.labels) <- c('0', '8', '18', '30', '50', '100','Experiment')

exp.placeholder <- data.frame(snails = seq(from = 2, to = 16, by = 2),
                              stoch = as.factor(rep('Experiment', times = 8)),
                              algae = rep(-1, 8),
                              Ro = rep(-1, 8),
                              numsnails = rep(0,8),
                              highCI = rep(-1, 8),
                              lowCI = rep(-1, 8),
                              adjust = NA)

R0$stoch <- as.factor(R0$stoch)

R0.data <- rbind(R0, exp.placeholder)
R0.data$stoch <- factor(R0.data$stoch, levels = c('Experiment',0, 8, 18, 30, 50, 100))


Ro.plot3 <- ggplot(R0.data, aes(x = snails/4, y = Ro, 
                           color= as.factor(algae))) +
  theme_classic() +
  geom_point() +
  geom_line() + 
  facet_wrap(~stoch, nrow = 1, labeller = labeller(stoch = stoch.labels, multi_line = TRUE)) +
  xlab('Snail Density') + 
  ylab(expression(paste("Mean ", R[0], sep = ""))) +
  labs(color = 'Resource Distribution') +
  guides(color = guide_legend(nrow = 1)) +
  geom_hline(yintercept=1,linetype=2) +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
theme(legend.position="bottom") + guides(colour = guide_legend(nrow = 1)) +
  xlim(0, 16) + ylim(0,4) +
  theme(plot.margin = margin(t = 0,  # Top margin
                             b = 0)) # Bottom margin
Ro.plot3


max.inf <- abm.dat %>%
  group_by(snails, stoch, algae, run) %>%
  summarise(max.inf = max(Is)) %>%
  group_by(snails, stoch, algae) %>%
  summarise(mean.Is = mean(max.inf),
            lower.ci = quantile(max.inf, 0.025),
            upper.ci = quantile(max.inf, 0.975))


exp.placeholder <- data.frame(snails = seq(from = 2, to = 16, by = 2),
                              stoch = as.factor(rep('Experiment', times = 8)),
                              algae = rep(-1, 8),
                              mean.Is = rep(-1, 8),
                              lower.ci = rep(-1, 8),
                              upper.ci = rep(-1, 8))

max.inf$stoch <- as.factor(max.inf$stoch)

inf.data <- rbind(max.inf, exp.placeholder)
inf.data$stoch <- factor(inf.data$stoch, levels = c('Experiment',0, 8, 18, 30, 50, 100))


inf.plot <- ggplot(inf.data, aes(x = snails/4, y = mean.Is/4, 
                           color= as.factor(algae))) +
  theme_classic() +
  geom_point() +
  geom_line() + 
  facet_wrap(~stoch, nrow = 1, labeller = labeller(stoch = stoch.labels, multi_line = TRUE)) +
  xlab('Snail Density') + 
  ylab('Mean Maximum Prevalance') +
  labs(color = 'Resource Distribution') +
  guides(color = guide_legend(nrow = 1)) +
  #geom_linerange(data = max.inf, aes(x = snails/4, ymin = lower.ci, ymax = upper.ci, color = as.factor(algae))) +
  theme(legend.position="bottom") + guides(colour = guide_legend(nrow = 1)) +
  xlim(0, 16) + ylim(0,16) +
  theme(plot.margin = margin(t = 0,  # Top margin
                             b = 0),
          strip.background = element_blank(),
          strip.text.x = element_blank()
        ) # Bottom margin
inf.plot


sum.preds$stoch <- as.factor(sum.preds$stoch)
sum.preds.abm$stoch <- as.factor(sum.preds.abm$stoch)

pred.fulldat <- rbind(sum.preds, sum.preds.abm)

pred.plots.2 <- ggplot() +
  geom_line(data = pred.fulldat, aes(x = SnailDensity, y = mean, color = as.factor(AggregationIndex),
                                     group = as.factor(AggregationIndex))) +
  #geom_point(data = mean.dat, aes(x = SnailDensityFix, y = Count, color =as.factor(AggregationIndex))) +
  #geom_point(data = abm.dat.mean, aes(x = snails/4, y = mean, color = as.factor(AggregationIndex))) +
  #geom_line(data = sum.preds, aes(x = SnailDensity, y = mean), lwd = 1) +
  #geom_ribbon(data = sum.preds, aes(ymax = upper.CI, ymin = lower.CI, x = SnailDensity, fill = 'black'),color = NA, alpha = .5) +
  #geom_line(data = sum.preds.abm, aes(x = SnailDensity, y = mean,
   #                                   color = as.factor(AggregationIndex))) + 
  #geom_ribbon(data = sum.preds.abm, aes(ymin=lower.CI, ymax=upper.CI, 
   #                                     x = SnailDensity, fill=as.factor(AggregationIndex)), alpha = 0.3, color = NA,
    #          show.legend = FALSE) +
  theme_classic() + 
  facet_wrap(~stoch, nrow = 1, labeller = labeller(stoch = stoch.labels, multi_line = TRUE)) + 
  xlab('Snail Density') + 
  ylab('Mean # Snail Contacts Per Tank') +
  xlab('') +
  labs(color='Aggregation Index') + 
   theme(legend.position="bottom") + 
  guides(colour = guide_legend(nrow = 1)) +
  #axis.ticks.x = element_blank(),
  #axis.text.x = element_blank()) + 
  theme(plot.margin = margin(t = 0,  # Top margin
                             b = 0)) + # Bottom margin
  coord_cartesian(ylim = c(0, 7.5))

pred.plots.2

#pdf('~/Desktop/SnailSimsFinal_14Mar23/New_Figure2_Contacts_Disease_19Jun23.pdf',
 # height = 8, width = 11)
ggarrange(pred.plots.2 + rremove('xlab'), 
          Ro.plot3 + rremove('xlab'), inf.plot, 
          common.legend = T,
          legend = 'bottom', ncol = 1,
          labels = c('A','B', 'C')
)
#dev.off()


R0[which(R0$stoch == 0 & R0$snails == 64 & R0$algae == 0),'Ro']

#data.plots <- ggarrange(pred.plots + rremove("xlab") + rremove("legend"), Ro.plot, nrow = 2,
 #         labels = c('B', 'C'),
  #        align = 'hv',
   #       common.legend = TRUE, legend =  'bottom',
    #      heights = c(2,2), widths = c(2,2))
#data.plots

#pdf('~/Desktop/SnailSimsFinal_14Mar23/Figure2_Contacts_23Mar23.pdf',
 #  height = 8, width = 11)
ggarrange(map.plot, pred.plots,
          labels = c('A','B'),
          nrow = 2,
          heights = c(.5, 2), widths = c(.5,2))
#dev.off()


#ggarrange(map.plot, pred.plots, Ro.plot, nrow = 3,
 #         labels = c('A', 'B', 'C'),
  #        heights = c(1,2,2), widths = c(1,2,2))

library(viridis)

#HeatMap Plot -- Supplemental Material?
ggplot(R0, 
       aes(x = algae, y = as.factor(stoch))) + 
  geom_tile(aes(fill = Ro)) + 
  theme_classic() + 
  facet_wrap(~snails) +
  #scale_fill_steps(low = "grey", high = "purple", breaks = breaks,    trans = "log") +
  scale_fill_viridis(discrete=FALSE) +
  #scale_fill_gradient2(low = "black", mid = 'white', high = 'red',
  #                    midpoint = 0, limits = range(outRo$Ro)) +
  ggtitle('')+ 
  ylab('Stochasticity') + xlab('Algae Pattern') +
  labs(fill = 'R0')


R0


##########################################################
##########################################################
###### Exploring Enounter Rate and Handling Time #########
##########################################################
##########################################################

#Handling Time -- Exp
dur.dat$group <- NA
dur.dat[dur.dat$AggregationIndex %in% c(0:3),'group'] <- 0
dur.dat[dur.dat$AggregationIndex %in% c(4:6),'group'] <- 1
dur.dat$group <- as.factor(dur.dat$group)

mod.h.exp <- lm(Duration/60 ~ AggregationIndex*group , dur.dat)
summary(mod.h.exp)

predict.handle.exp <- data.frame(predict(mod.h.exp, newdata = data.frame(
                              "AggregationIndex" = 0:6,
                              "group" = as.factor(c(0,0,0,0,1,1,1))),
                              interval = 'confidence',
                              level = .95))
predict.handle.exp$aggregationindex <- 0:6
predict.handle.exp$stoch <- 'Experiment'
colnames(predict.handle.exp) <- c('fit', 'lwr','upr', 'AggregationIndex', 'stoch')

ggplot() +
  geom_point(data = dur.dat, aes(y = Duration/60, x = AggregationIndex,
                                 color = group)) +
  theme_classic() +
  geom_line(data = predict.handle.exp, aes(y = fit, x = 0:6)) +
  ylab('Contact Duration/Handling Time') + xlab('Aggregation Index')

#Handling Time -- ABM
abm.dat.50$group <- 0
abm.dat.50$AggregationIndex <- abm.dat.50$AggregationIndex - 1
abm.dat.50[abm.dat.50$AggregationIndex > 3,'group'] <- 1
abm.dat.50$group <- as.factor(abm.dat.50$group) 

mod.h.abm <- lm(contact_dur ~ AggregationIndex*as.factor(stoch)*group, abm.dat.50)
summary(mod.h.abm)

newdata.handle <- data.frame("AggregationIndex" = rep(0:6, times = 6),
                             "stoch" = rep(stochs, each = 7),
                             "group" = as.factor(c(0,0,0,0,1,1,1)))

newdata.handle <- cbind(newdata.handle,
                        predict(mod.h.abm,newdata = newdata.handle, 
                                    interval = 'confidence', 
                                    level = .95))

newdata.handle$stoch <- as.factor(newdata.handle$stoch)
newdata.handle$stoch <- factor(newdata.handle$stoch, 
                               levels = c('0', '8', '18', '30', '50', '100'))


ggplot() +
  geom_point(data = abm.dat.50, aes(y = contact_dur, x = AggregationIndex)) +
  theme_classic() +
  facet_wrap(~factor(stoch, levels = c(stochs))) +
  geom_line(data = newdata.handle, aes(y = fit, x = AggregationIndex)) +
  ylab('Contact Duration/Handling Time') + xlab('Aggregation Index')


#Encounter Rates -- ABM
encounter.rates$variable <- as.character(encounter.rates$variable)
encounter.rates[encounter.rates$variable == 'a.1.','variable'] <- '0'
encounter.rates[encounter.rates$variable == 'a.2.','variable'] <- '1'
encounter.rates[encounter.rates$variable == 'a.3.','variable'] <- '2'
encounter.rates[encounter.rates$variable == 'a.4.','variable'] <- '3'
encounter.rates[encounter.rates$variable == 'a.5.','variable'] <- '4'
encounter.rates[encounter.rates$variable == 'a.6.','variable'] <- '5'
encounter.rates[encounter.rates$variable == 'a.7.','variable'] <- '6'
encounter.rates$variable <- as.numeric(encounter.rates$variable)

ggplot(encounter.rates, 
       aes(y = exp(value), x = variable)) +  
  geom_point() + theme_classic() +
  facet_wrap(~stochs) + 
  ylab('Encounter Rate') + xlab('Aggregation Index')

encounter.rates$group <- 0
encounter.rates[encounter.rates$variable > 3,'group'] <- 1

encounter.rates$group <- as.factor(encounter.rates$group)
encounter.rates$stochs <- as.factor(encounter.rates$stochs)

#Sampling posterior
nrow(encounter.rates)/50

table(encounter.rates$stochs, encounter.rates$variable)

encounter.rates.samps <- encounter.rates %>% 
  group_by(stochs, variable) %>%
  slice(seq(1, n(), by = 60))

table(encounter.rates.samps$stochs, encounter.rates.samps$variable)

mod.a.abm <- lm(value ~ stochs*group*variable, encounter.rates.samps) #- stochs:group:variable
summary(mod.a.abm)

newdata <- data.frame("stochs" = as.factor(rep(rep(stochs, each = 7), times = 2)),
                      "variable" = rep(rep(0:6, times = 6), times = 2))
newdata$group <- 0
newdata[newdata$variable > 3,'group'] <- 1
newdata$group <- as.factor(newdata$group)

newdata <- cbind(newdata,
                predict(mod.a.abm,newdata = newdata, 
                        interval = 'confidence', 
                        level = .95))
ggplot(newdata, 
       aes(y = exp(fit), x = variable,
          color = as.factor(group))) +  
  geom_point() + theme_classic() +
  facet_wrap(~stochs) + 
  ylab('Encounter Rate') + xlab('Aggregation Index')

ggplot() +  
  geom_point(data = encounter.rates.samps, 
             aes(y = value, x = variable)) +
  geom_line(data = newdata, 
            aes(y = fit, x = variable)) +
  geom_line(data = newdata, 
            aes(y = lwr, x = variable), lty = 2) +
  geom_line(data = newdata, 
            aes(y = upr, x = variable), lty = 2) +
  theme_classic() +
  facet_wrap(~stochs) + 
  ylab('Encounter Rate') + xlab('Aggregation Index')

#Encounter Rates -- Experiment
x$group <- 0
x[x$aggregationindex > 3,'group'] <- 1
x$group <- as.factor(x$group)

#Sampling posterior
nrow(x)/50

table(x$stochs, x$aggregationindex)

x.samps <- x %>% 
  group_by(stochs, aggregationindex) %>%
  slice(seq(1, n(), by = 60))



mod.a.exp <- lm(value ~ aggregationindex*group, x.samps)
summary(mod.a.exp)

newdata.exp <- data.frame('aggregationindex' = 0:6,
                          'group' = c(0,0,0,0,1,1,1))
newdata.exp$group <- as.factor(newdata.exp$group)

newdata.exp <- cbind(newdata.exp,
  predict(mod.a.exp,newdata = newdata.exp, 
                         interval = 'confidence', 
                         level = .95))
ggplot() +  
  geom_point(data = x.samps, 
             aes(y = value, x = aggregationindex)) +
  geom_line(data = newdata.exp, 
            aes(y = fit, x = aggregationindex)) +
  geom_line(data = newdata.exp, 
            aes(y = lwr, x = aggregationindex), lty = 2) +
  geom_line(data = newdata.exp, 
            aes(y = upr, x = aggregationindex), lty = 2) +
  theme_classic() +
  ylab('Encounter Rate') + xlab('Aggregation Index')

### Parameter Paper Plots ###
#Encounter Rate
x.samps$stochs <- 'Experiment'
x.samps <- x.samps[,-c(1:2)]
colnames(x.samps) <- c('value', 'variable', 'stochs', 'group')
encounter.rates.comb <- rbind(x.samps, encounter.rates.samps)
newdata.exp$stochs <- 'Experiment'
colnames(newdata.exp) <- c('variable', 'group', 'fit', 'lwr', 'upr', 'stochs')
newdata.a.combine <- rbind(newdata, newdata.exp)

encounter.rates.comb$stochs <- factor(encounter.rates.comb$stochs, levels = c('Experiment',0, 8, 18, 30, 50, 100))
newdata.a.combine$stochs <- factor(newdata.a.combine$stochs, levels = c('Experiment',0, 8, 18, 30, 50, 100))

encounter.plot <- ggplot() +  
  geom_point(data = encounter.rates.comb, 
             aes(y = exp(value), x = variable,
                 color = as.factor(stochs))) +
  geom_line(data = newdata.a.combine, 
            aes(y = exp(fit), x = variable,
                color = as.factor(stochs))) +
  geom_line(data = newdata.a.combine, 
            aes(y = exp(lwr), x = variable,
                color = as.factor(stochs)), lty = 2) +
  geom_line(data = newdata.a.combine, 
            aes(y = exp(upr), x = variable,
                color = as.factor(stochs)), lty = 2) +
  theme_classic() + facet_wrap(~stochs, nrow = 1, labeller = labeller(stochs = stoch.labels, multi_line = TRUE)) +
  ylab('Encounter Rate') + xlab('') +
  scale_color_manual(values = c("0" = "#FDE725FF",
                                #"4" = "#CD9600",
                                "8" = "#73D055FF",
                                #"10" = "#7CAE00",
                                #"14" = "#0CB702",
                                "18" = "#29AF2FFF",
                                #"22" = "#00C19A",
                                #"26" = "#00B8E7",
                                "30" = "#2D708EFF",
                                #"40" = "#8494FF",
                                "50" = "#404788FF",
                                "100" = "#440154FF",
                                "Experiment" = "black")) +
  labs(color = 'Stochasticity') + theme(legend.position="bottom") + 
  guides(color = guide_legend(nrow = 1))

#Handling Time
dur.dat$stoch <- 'Experiment'

#ggplot() +
 # geom_point(data = abm.dat.50, aes(y = contact_dur, x = stoch,
  #                                  color = as.factor(AggregationIndex-1)),
   #          position = position_dodge(width = .5)) +
  #theme_classic() +
  #geom_line(data = newdata.handle, aes(y = fit, x = stoch,
   #                                    color = as.factor(AggregationIndex))) +
  #geom_line(data = newdata.handle, aes(y = lwr, x = stoch,
   #                                   color = as.factor(AggregationIndex)), lty = 2) +
  #geom_line(data = newdata.handle, aes(y = upr, x = stoch,
   #                                    color = as.factor(AggregationIndex)), lty = 2) +
  #ylab('Contact Duration/Handling Time') + xlab('Aggregation Index') +
  #geom_point(data = dur.dat, aes(y = Duration/60, x = stoch,
   #                              color = as.factor(AggregationIndex))) +
  #geom_line(data = predict.handle.exp, aes(y = fit, x = stoch,
   #                                        color = as.factor(aggregationindex))) +
  #geom_line(data = predict.handle.exp, aes(y = lwr, x = stoch,
   #                                        color = as.factor(aggregationindex)), lty = 2) +
  #geom_line(data = predict.handle.exp, aes(y = upr, x = stoch,
   #                                        color = as.factor(aggregationindex)), lty = 2) +
  #scale_color_manual(values = c("0" = "#FDE725FF",
   #                             #"4" = "#CD9600",
    #                            "8" = "#73D055FF",
     #                           #"10" = "#7CAE00",
      #                          #"14" = "#0CB702",
       #                         "18" = "#29AF2FFF",
        #                        #"22" = "#00C19A",
         #                       #"26" = "#00B8E7",
          #                      "30" = "#2D708EFF",
           #                     #"40" = "#8494FF",
            #                    "50" = "#404788FF",
             #                   "100" = "#440154FF",
              #                  "Experiment" = "black")) +
  #labs(color = 'Aggregation Index') + theme(legend.position="bottom") +
  #guides(color = guide_legend(nrow = 1))

colnames(predict.handle.exp) <- c('fit', 'lwr', 'upr', 'AggregationIndex','stoch')
predict.handle.exp$group <- c(0,0,0,0,1,1,1)
predicted_handling_data <- rbind(newdata.handle,predict.handle.exp)
  

dur.plot_2 <- ggplot() +
  geom_point(data = all.dur, aes(x = AggregationIndex, y = mean, 
                                 color = as.factor(AggregationIndex))) +
  geom_linerange(data = all.dur, 
                 aes(x = AggregationIndex, ymin = lower.ci, ymax = upper.ci,
                     color = as.factor(AggregationIndex)),
                 position = position_dodge(width = .5)) +
  #geom_line(data = predicted_handling_data, aes(y = lwr, x = AggregationIndex),
   #                                    color = 'black', lty = 2) +
  geom_ribbon(data = predicted_handling_data, aes(ymin = lwr, ymax = upr,
                                                  x = AggregationIndex), fill = 'black', alpha = .2) + 
  geom_line(data = predicted_handling_data, aes(y = fit, x = AggregationIndex),
                                                color = 'black', lty = 1) +
  #geom_line(data = predicted_handling_data, aes(y = upr, x = AggregationIndex),
   #                                             color = 'black', lty = 2) +
  geom_point(position = position_dodge(width = 0.5)) + theme_classic() +
  facet_wrap(~stoch, nrow = 1, strip.position = "bottom") +
  ylab('Average Contact Duration (Handling Time)') + xlab('Stochasticity') +
  labs(color = 'Aggregation Index') +
  theme(legend.position = 'bottom',
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank()) +
  guides(colour = guide_legend(nrow = 1)) #+
  
dur.plot_2


#Duration Violin Plot
abm.durdata.combo <- abm.dat.50[,c('stoch', 'AggregationIndex', 'contact_dur')]
#abm.durdata.combo$AggregationIndex <- abm.durdata.combo$AggregationIndex +1
dur.dat$stoch <- 'Experiment'
exp.durdata.combo <- dur.dat[c('AggregationIndex', 'stoch', 'Duration')]
colnames(exp.durdata.combo) <- c('AggregationIndex', 'stoch', 'contact_dur')
exp.durdata.combo$contact_dur <- exp.durdata.combo$contact_dur/60

dur.data.comb <- rbind(abm.durdata.combo, exp.durdata.combo)

dur.data.comb$stoch <- as.factor(dur.data.comb$stoch)
dur.data.comb$stoch <- factor(dur.data.comb$stoch, levels = c("Experiment", '0','8','18','30','50','100'))

dur.plot_3 <- ggplot() +
  geom_violin(data = dur.data.comb, aes(x = AggregationIndex, y = contact_dur, 
                                 color = as.factor(AggregationIndex))) +
  
  geom_ribbon(data = predicted_handling_data, aes(ymin = lwr, ymax = upr,
                                                  x = AggregationIndex), fill = 'black', alpha = .2) + 
  geom_line(data = predicted_handling_data, aes(y = fit, x = AggregationIndex),
            color = 'black', lty = 1) +
  geom_point(position = position_dodge(width = 0.5)) + theme_classic() +
  facet_wrap(~stoch, nrow = 1) +
  ylab('Average Contact Duration (Handling Time)') + xlab('Aggregation Index') +
  labs(color = 'Aggregation Index') +
  theme(legend.position = 'bottom', strip.background = element_blank(), 
        strip.text.x = element_blank()) + 
  guides(colour = guide_legend(nrow = 1)) 

dur.plot_3

####### Parameter Plot #######
param.plot <- ggarrange(encounter.plot, dur.plot_3,
                        ncol = 1, labels = c("A", "B"),
                        common.legend = TRUE,
                        legend = 'bottom',
                        align = 'hv')
#pdf('~/Desktop/SnailSimsFinal_14Mar23/Figure_Params_19June23.pdf',
 # height = 8, width = 11)
param.plot
#dev.off()

#Output Model Summaries for Paper
#library(broom)
#write.csv(broom::tidy(mod.a.abm), '~/Desktop/SnailSimsFinal_14Mar23/FinalSims_15May23/Encounter_ABM_15May23.csv')
#write.csv(broom::tidy(mod.a.exp), '~/Desktop/SnailSimsFinal_14Mar23/FinalSims_15May23/Encounter_EXP_15May23.csv')
#write.csv(broom::tidy(mod.h.abm), '~/Desktop/SnailSimsFinal_14Mar23/FinalSims_15May23/Handling_ABM_15May23.csv')
#write.csv(broom::tidy(mod.h.exp), '~/Desktop/SnailSimsFinal_14Mar23/FinalSims_15May23/Handling_EXP_15May23.csv')










