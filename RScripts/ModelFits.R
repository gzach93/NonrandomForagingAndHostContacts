#Libraries
library(R2jags)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(ggpubr)
library(performance)

#Load in Experimental Data
Data <- read.csv("SnailExpData.csv") #Contact Data
dur.dat <- read.csv('ResourceDistributionExperiment_ContactData.csv') #Contact Duration Data

#####################################################################
###################  Experimental Data Model ########################
#####################################################################
model <- "
model {

# Priors 
mu~dunif(0, 1000)
alpha~dunif(0, 5) ##this is k, the dispersion param for NB
H ~ dunif(0, 45)

for (j in 1:NTanks) {
a[j]~dgamma(1,mu) ##exponential distrib with mean = 1/mu
}

# Likelihood 
for (i in 1:NObs) {
y[i] ~ dpois(lambda[i])
lambda[i]<-rho[i]*pred[i] ##rho gives us our NB
pred[i]<-((a[UniqueTank[i]])*TOTALTIME*(N[i]))/(1+(a[UniqueTank[i]])*H*(N[i]-1))
rho[i] ~ dgamma(alpha, alpha)
}

}"

#Below Runs the Model Above Separately for each resource distribution
results <- plyr::dlply(.data = Data, .variables = "AggregationIndex", .fun = function(x){

#Create Data Objects Below -- x is what dply uses for the subset of data
  
  #Need unique tank number 1-16
  #length(Data$Trial)
  x$UniqueTankNum <- as.numeric(paste(x$Trial, x$Tank, sep="."))
  length(unique(x$UniqueTankNum))
  table(x$UniqueTankNum)
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[1]]<-1
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[2]]<-2
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[3]]<-3
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[4]]<-4
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[5]]<-5
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[6]]<-6
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[7]]<-7
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[8]]<-8
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[9]]<-9
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[10]]<-10
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[11]]<-11
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[12]]<-12
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[13]]<-13
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[14]]<-14
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[15]]<-15
  x$UniqueTank[x$UniqueTankNum == unique(x$UniqueTankNum)[16]]<-16
  NTanks<-length(unique(x$UniqueTank))
  NObs<-length(x$Trial)
  
  y<-x$Count
  N<-x$SnailDensity
  UniqueTank<-x$UniqueTank
  TOTALTIME <- 45

#Put Data For Model into List to use in Model
  data <- list(y=y, N=N, UniqueTank=UniqueTank, TOTALTIME=TOTALTIME, NTanks = NTanks, NObs = NObs)
#Parameters to Monitor
  params <- c("mu", "alpha", "H")
#Set Initial Parameter Values
  inits <- function() { list(a=rep(0.1, 16), mu=runif(1, 0, 100), H =runif(1,0,45), alpha=runif(1, 0, 0.2)) }
#Run Model
  outTypeIIcontacts <- jags(data = data, inits = inits, parameters.to.save = params, n.chains = 3, 
                          n.iter = 60000, n.burnin = 15000, model.file = textConnection(model), DIC=TRUE)

})
#Save Model Results to Not have to rerun the experimental model code
saveRDS(results,'ExperimentalModelResults.RDS')

#If Experimental Model Results Were Saved Load in to Skip Rerunning Model
results.exp <- readRDS('ExperimentalModelResults.RDS')

#### THE CODE BELOW WORKS WHEN MODEL RESULTS ARE SAVED AND LOADED BACK IN

#Use Model Parameter Estimates to Fit Contact Model for Across Host Densities
output <- NA
#Run Loop to Get Estimates for Each Aggregation Index
for(i in 1:7){
outTypeIIcontacts.mcmc <- as.mcmc(results.exp[[i]])
#acfplot(outTypeIIcontacts.mcmc)
outTypeIIcontacts.summary <- summary(outTypeIIcontacts.mcmc, q=c(0.025, 0.975)); outTypeIIcontacts.summary
#HPDinterval(outTypeIIcontacts.mcmc)
#plot(outTypeIIcontacts.mcmc)

outTypeIIcontacts.summary$statistics
#((1/mu)*TOTALTIME*(Ntest[i]-0))/(1+(1/mu)*(H)*(Ntest[i]-1))
TestN<-seq(1, max(Data$SnailDensity), 1) #Host Test Densities

a <- 1/outTypeIIcontacts.summary$statistics[4]
H <- outTypeIIcontacts.summary$statistics[3] 
TOTALTIME <- 45

TestContacts<-(a*TOTALTIME*TestN)/(1+a*H*(TestN-1))

Pred<-data.frame(cbind(TestN, TestContacts))
Pred$AggregationIndex <- i

output <- rbind(output, Pred)
}

output <- output[-1,]#Remove Empty First Row
output$AggregationIndex <- output$AggregationIndex -1 #Adjust Aggregation Index

#First Look at Experimental Host Contact Model Fits
ggplot() +
  geom_jitter(width = 0.05) + 
  geom_line(data=output, aes(x=TestN, y=TestContacts)) +
  geom_point(data = Data, aes(y = Count, x = SnailDensity)) +
  facet_grid(~AggregationIndex) +
  theme_classic() 

#95% Crediable Interval - similar code as above
output.ci <- c(NA)
for(i in 1:7){
  outTypeIIcontacts.mcmc <- as.mcmc(results.exp[[i]])
  outTypeIIcontacts.summary <- summary(outTypeIIcontacts.mcmc, q=c(0.025, 0.975))
  TestN<-seq(1, max(Data$SnailDensity), 1)
  a.low <- 1/outTypeIIcontacts.summary$quantiles[8]
  H.low <- outTypeIIcontacts.summary$quantiles[7] 
  a.high <- 1/outTypeIIcontacts.summary$quantiles[4]
  H.high <- outTypeIIcontacts.summary$quantiles[3] 
  TOTALTIME <- 45
  TestContacts.low<-(a.low*TOTALTIME*TestN)/(1+a.low*H.low*(TestN-1))
  TestContacts.high<-(a.high*TOTALTIME*TestN)/(1+a.high*H.high*(TestN-1))
  Pred<-data.frame(cbind(TestN, TestContacts.low, TestContacts.high))
  Pred$AggregationIndex <- i
  output.ci <- rbind(output.ci, Pred)
}

output.ci <- output.ci[-1,]
output.ci$AggregationIndex <- output.ci$AggregationIndex -1

#Experimental Model Fits with Credible Intervals
ggplot() +
  geom_jitter(width = 0.05) + 
  geom_line(data=output, aes(x=TestN, y=TestContacts)) +
  geom_ribbon(data=output.ci, aes(x=TestN, ymax=TestContacts.high, ymin = TestContacts.low), alpha = .4) +
  geom_point(data = Data, aes(y = Count, x = SnailDensity)) +
  facet_grid(~AggregationIndex) +
  theme_classic() 

#Create Dataframe with Experimental Handling Time Estimates
exp.H <- data.frame('H' = c(results.exp[[1]]$BUGSoutput$sims.matrix[,1],
                            results.exp[[2]]$BUGSoutput$sims.matrix[,1],
                            results.exp[[3]]$BUGSoutput$sims.matrix[,1],
                            results.exp[[4]]$BUGSoutput$sims.matrix[,1],
                            results.exp[[5]]$BUGSoutput$sims.matrix[,1],
                            results.exp[[6]]$BUGSoutput$sims.matrix[,1],
                            results.exp[[7]]$BUGSoutput$sims.matrix[,1]))
exp.H$AggreationIndex <- rep(0:6, each = 3000)

#Violin Plots for Handling Time Estimates
ggplot() +
  geom_violin(data = exp.H, aes(x = as.factor(AggreationIndex), y = H)) +
  theme_classic()

#Create Data frame with Experimental Encounter Rate Estimates -- Mu Values
exp.a <- data.frame('a' = c(results.exp[[1]]$BUGSoutput$sims.matrix[,4],
                            results.exp[[2]]$BUGSoutput$sims.matrix[,4],
                            results.exp[[3]]$BUGSoutput$sims.matrix[,4],
                            results.exp[[4]]$BUGSoutput$sims.matrix[,4],
                            results.exp[[5]]$BUGSoutput$sims.matrix[,4],
                            results.exp[[6]]$BUGSoutput$sims.matrix[,4],
                            results.exp[[7]]$BUGSoutput$sims.matrix[,4]))
exp.a$AggreationIndex <- rep(0:6, each = 3000)
exp.a$a <- 1/exp.a$a #Transform Mu values to Encounter Rates

#Violin Plot For Experimental Encounter Rates
ggplot() +
  geom_violin(data = exp.a, aes(x = as.factor(AggreationIndex), y = a)) +
  theme_classic()

######################################
########### ABM Section###############
######################################

#Load in Cleaned ABM Data -- Long Format
abm.dat <- read.csv('OutputABMSimFinal.csv')

#Rename ABM Data Columns
colnames(abm.dat) <- c('run', 'start_infections', 'feeding', 'algae_conc',
                       'snails', 'stoch',
                       'infect', 'algae', 'recover', 'GUD', 
                       'resources','edge', 'patch_time',
                       'Ss', 'Is', 'Rs', 'contacts', 'total_contacts',
                       'patches','unique_contacts', 'contact_dur', 
                       'ticks')

stochs <- c(0, 8, 18, 30, 50, 100) #This is the ABM stochasticites that are being used
abm.dat$algae <- abm.dat$algae + 1 #Add One to avoid issues in Model Loops below

#Plot of Host Contacts at Last Time Points For each Aggregation Index and Stochasticity
ggplot() +
  geom_point(data = abm.dat[which(abm.dat$ticks == 50),], 
             aes(x = snails, y = contacts, 
                 color = as.factor(stoch)), position = 'jitter') + 
  theme_classic() +
  facet_wrap(~algae, nrow = 1)

#ABM Contact Model
model.abm <- "
model {
# Priors 
#H ~dunif(0,50)
sigma ~ dexp(0.1) # standard deviation
tau <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS

#algae Distrubution Effect
for(j in 1:NAlgae){
a[j] ~ dgamma(1,1)
H[j] ~ dunif(0,50)
}

# Likelihood 
for (i in 1:N.obs) {
y[i] ~ dnorm(mu[i], tau)
mu[i]<-(a[algae.dist[i]]*TOTALTIME*(N[i]))/(1+a[algae.dist[i]]*(H[algae.dist[i]]/1)*(N[i]-1))
}

#Model Fits
for(j in 1:NAlgae){
for(i in 1:N.obs.test){
pred[j,i] ~ dnorm(pred.mu[j,i], tau)
pred.mu[j,i] <- (a[j]*TOTALTIME*(Ntest[i]))/(1+a[j]*(H[j]/1)*(Ntest[i]-1))
}
} 

}"

#Only Fit the Data with the Last Time Point Contact Data
abm.dat.50 <- abm.dat[abm.dat$ticks == 50,]
abm.model.data <- abm.dat.50

table(abm.model.data$stoch, abm.model.data$algae, abm.model.data$snails)

#Run Each Stoch Separately
results <- plyr::dlply(.data = abm.model.data, .variables = "stoch", .fun = function(x){
 y <- x$contacts #Counts
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

  data <- list(y=y, N=N, algae.dist=algae.dist, TOTALTIME=TOTALTIME,
               N.obs = N.obs, NAlgae = NAlgae, Ntest = Ntest, N.obs.test = N.obs.test)

  #Parameters to Monitor
  params <- c("a", "sigma", "pred.mu", "H")
  
  inits <- function() {list(a=rgamma(NAlgae, 1, 1), 
                            H=runif(NAlgae, 0, 50)) }
  
  outTypeIIcontacts <- jags(data = data, inits = inits, 
                            parameters.to.save = params,
                            n.chains = 3, n.iter = 80000, n.burnin = 15000, 
                            n.thin = 20,
                            model.file = textConnection(model.abm), DIC=FALSE)
  
  #outTypeIIcontacts.mcmc <- as.mcmc(outTypeIIcontacts)
  #outTypeIIcontacts.summary <- summary(outTypeIIcontacts.mcmc, q=c(0.025, 0.975)); outTypeIIcontacts.summary
  
  return(outTypeIIcontacts)
})
saveRDS(results, 'ABMModelResults.RDS') #Save Model Run to Not have to Rerun Model Loop
results <- readRDS('ABMModelResults.RDS') #Load in Saved Model Results to Avoid Rerunning Model Loop

#####  CODE BELOW WORKS BELOW WHEN MODEL RESULTS ARE SAVED AND LOADED BACK IN 



#Look at ABM Model Output
plot(as.mcmc(results[[6]])[,c(1:7)]) #a
plot(as.mcmc(results[[1]])[,c(8:14)]) #H


##Encounter Rates ABM Data
encounter.rates <- data.frame(rbind(results[[1]]$BUGSoutput$sims.matrix[,8:14],
                                  results[[2]]$BUGSoutput$sims.matrix[,8:14],
                                  results[[3]]$BUGSoutput$sims.matrix[,8:14],
                                  results[[4]]$BUGSoutput$sims.matrix[,8:14],
                                  results[[5]]$BUGSoutput$sims.matrix[,8:14],
                                  results[[6]]$BUGSoutput$sims.matrix[,8:14]))
encounter.rates$stochs <- rep(stochs, each = nrow(results[[1]]$BUGSoutput$sims.matrix[,8:14]))

encounter.rates <- melt(encounter.rates, id.vars  = 'stochs') #Switch to Long
encounter.rates$variable <- as.character(encounter.rates$variable)
exp.a$stochs <- 'Experiment'
colnames(exp.a) <- c('value', 'variable', 'stochs')
encounter.rates <- rbind(encounter.rates, exp.a) #Combine Experimental and ABM Encounter Rates Estimates

#Find Mean and Interval Around Encounter Rates for each Stoch
encounter.means <- encounter.rates %>%
  group_by(stochs, variable) %>%
  summarise(mean = mean(value),
            lower.ci = quantile(value, 0.025),
            upper.ci = quantile(value, 0.975))
#Rename Ecounter Rates 
encounter.means$variable <- as.character(encounter.means$variable)
encounter.means[encounter.means$variable == 'a.1.','variable'] <- '0'
encounter.means[encounter.means$variable == 'a.2.','variable'] <- '1'
encounter.means[encounter.means$variable == 'a.3.','variable'] <- '2'
encounter.means[encounter.means$variable == 'a.4.','variable'] <- '3'
encounter.means[encounter.means$variable == 'a.5.','variable'] <- '4'
encounter.means[encounter.means$variable == 'a.6.','variable'] <- '5'
encounter.means[encounter.means$variable == 'a.7.','variable'] <- '6'
encounter.means$variable <- as.numeric(encounter.means$variable)

#Reorder Levels
encounter.means$stochs <- factor(encounter.means$stochs, 
                                 levels = c('Experiment', '0', '8', '18', '30', '50', '100'))

#Create Stoachasticty Labels 
stoch.labels <- c('Stochasticity = 0',
                  'Stochasticity = 8',
                  'Stochasticity = 18',
                  'Stochasticity = 30',
                  'Stochasticity = 50',
                  'Stochasticity = 100',
                  'Experiment')
names(stoch.labels) <- c('0', '8', '18', '30', '50', '100','Experiment')

#Encounter Rate Plot
encounter.plot <- ggplot(encounter.means, aes(y = mean, x = variable,
                                            color = as.factor(stochs))) +
  geom_point() + theme_classic() + 
  geom_line() +
  geom_linerange(data = encounter.means, aes(ymin = lower.ci, ymax = upper.ci,
                                           x = variable,
                                           width = 0.2, color = as.factor(stochs))) +
  ylab('Encounter Rate') + xlab('Aggregation Index') +
  scale_color_manual(values = c("0" = "#FDE725FF",
                                "8" = "#73D055FF",
                                "18" = "#29AF2FFF",
                                "30" = "#2D708EFF",
                                "50" = "#404788FF",
                                "100" = "#440154FF",
                                "Experiment" = "black")) +
  labs(color = 'Stochasticity') + theme(legend.position="bottom") + 
  facet_grid(~stochs, labeller = labeller(stochs = stoch.labels)) +
  guides(color = guide_legend(nrow = 1)); encounter.plot

#Handling Time ABM Data
handling.time <- data.frame(rbind(results[[1]]$BUGSoutput$sims.matrix[,1:7],
                                  results[[2]]$BUGSoutput$sims.matrix[,1:7],
                                  results[[3]]$BUGSoutput$sims.matrix[,1:7],
                                  results[[4]]$BUGSoutput$sims.matrix[,1:7],
                                  results[[5]]$BUGSoutput$sims.matrix[,1:7],
                                  results[[6]]$BUGSoutput$sims.matrix[,1:7]))
handling.time$stochs <- rep(stochs, each = nrow(results[[1]]$BUGSoutput$sims.matrix[,1:7]))

handling.time <- melt(handling.time, id.vars  = 'stochs')
handling.time$variable <- as.character(handling.time$variable)

exp.H$stochs <- 'Experiment'
colnames(exp.H) <- c('value', 'variable', 'stochs')
handling.time <- rbind(handling.time, exp.H) #Combine Experimental and ABM Handling Times
#Mean and Interval of Handling Times
handling.means <- handling.time %>%
  group_by(stochs, variable) %>%
  summarise(mean = mean(value),
            lower.ci = quantile(value, 0.025),
            upper.ci = quantile(value, 0.975))

#Renames Hanlding Time Variables
handling.means$variable <- as.character(handling.means$variable)
handling.means[handling.means$variable == 'H.1.','variable'] <- '0'
handling.means[handling.means$variable == 'H.2.','variable'] <- '1'
handling.means[handling.means$variable == 'H.3.','variable'] <- '2'
handling.means[handling.means$variable == 'H.4.','variable'] <- '3'
handling.means[handling.means$variable == 'H.5.','variable'] <- '4'
handling.means[handling.means$variable == 'H.6.','variable'] <- '5'
handling.means[handling.means$variable == 'H.7.','variable'] <- '6'
handling.means$variable <- as.numeric(handling.means$variable)
#Reorder Factors
handling.means$stochs <- factor(handling.means$stochs, 
                                 levels = c('Experiment', '0', '8', '18', '30', '50', '100'))

#Handling Time Plot 
handling.plot <- ggplot(handling.means, aes(y = mean, x = variable,
                                              color = as.factor(stochs))) +
  geom_point() + theme_classic() + 
  geom_line() +
  geom_linerange(data = handling.means, aes(ymin = lower.ci, ymax = upper.ci,
                                             x = variable,
                                             width = 0.2, color = as.factor(stochs))) +
  ylab('Handling Time (Min)') + xlab('Aggregation Index') +
  scale_color_manual(values = c("0" = "#FDE725FF",
                                "8" = "#73D055FF",
                                "18" = "#29AF2FFF",
                                "30" = "#2D708EFF",
                                "50" = "#404788FF",
                                "100" = "#440154FF",
                                "Experiment" = "black")) +
  labs(color = 'Stochasticity') + theme(legend.position="bottom") +
  facet_grid(~stochs, labeller = labeller(stochs = stoch.labels)) +
  guides(color = guide_legend(nrow = 1)); handling.plot

#Parameter Plots
ggarrange(encounter.plot, handling.plot, 
          common.legend = T,
          ncol = 1, labels = c('A', 'B'),
          legend = 'bottom')

#Model Fits for ABM
preds.abm <- data.frame(rbind(results[[1]]$BUGSoutput$sims.matrix[,15:77],
                              results[[2]]$BUGSoutput$sims.matrix[,15:77],
                              results[[3]]$BUGSoutput$sims.matrix[,15:77],
                              results[[4]]$BUGSoutput$sims.matrix[,15:77],
                              results[[5]]$BUGSoutput$sims.matrix[,15:77],
                              results[[6]]$BUGSoutput$sims.matrix[,15:77]))

preds.abm$stoch <- rep(stochs, each = nrow(results[[1]]$BUGSoutput$sims.matrix[,8:14]))
preds.abm <- melt(preds.abm, id.vars  = 'stoch')

#Create Host Density Column
preds.abm$AggregationIndex <- as.numeric(substr(preds.abm$variable, 9,9))
preds.abm$AggregationIndex <- preds.abm$AggregationIndex - 1
preds.abm$SnailDensity <- as.numeric(substr(preds.abm$variable, 11,11))
preds.abm[preds.abm$SnailDensity == 1, 'SnailDensity'] <- 0
preds.abm[preds.abm$SnailDensity == 6, 'SnailDensity'] <- 10
preds.abm[preds.abm$SnailDensity == 7, 'SnailDensity'] <- 12
preds.abm[preds.abm$SnailDensity == 8, 'SnailDensity'] <- 14
preds.abm[preds.abm$SnailDensity == 9, 'SnailDensity'] <- 16
preds.abm[preds.abm$SnailDensity == 2, 'SnailDensity'] <- 2
preds.abm[preds.abm$SnailDensity == 4, 'SnailDensity'] <- 6
preds.abm[preds.abm$SnailDensity == 3, 'SnailDensity'] <- 4
preds.abm[preds.abm$SnailDensity == 5, 'SnailDensity'] <- 8

#Mean and Interval Around Mean Contacts
sum.preds.abm <- preds.abm %>%
  group_by(SnailDensity, AggregationIndex, stoch) %>%
  summarise(mean = mean(value),
            lower.CI = quantile(value, 0.025),
            upper.CI = quantile(value, 0.975))

abm.dat.50$AggregationIndex <- abm.dat.50$algae

#Mean Data
abm.dat.mean <- abm.dat.50 %>%
  group_by(snails, AggregationIndex, stoch) %>%
  summarise(mean = mean(contacts))


#Model Fits To Mean Data
ggplot(sum.preds.abm, aes(x = SnailDensity, y = mean,
                          color = as.factor(AggregationIndex))) +
  geom_line() + 
  geom_line(aes(x = SnailDensity, y = lower.CI, 
                color = as.factor(AggregationIndex)), linetype = 'dashed') +
  geom_line(aes(x = SnailDensity, y = upper.CI, 
                color = as.factor(AggregationIndex)), linetype = 'dashed') +
  theme_classic() + 
  facet_wrap(~stoch, nrow = 1) + 
  xlab('Snail Density') + ylab('Mean # Snail Contacts') +
  geom_point(data = abm.dat.mean, aes(x = snails/4, y = mean,
                                      color = as.factor(AggregationIndex)))

abm.dat.mean$AggregationIndex <- abm.dat.mean$AggregationIndex -1

output$stoch <- 'Experiment'
Data$stoch <- 'Experiment'

#ABM Contact Model Fits Plot 
ggplot(sum.preds.abm, aes(x = SnailDensity, y = mean, color = as.factor(stoch))) +
  geom_line() + 
  geom_line(aes(x = SnailDensity, y = lower.CI, 
                color = as.factor(stoch)), linetype = 'dashed') +
  geom_line(aes(x = SnailDensity, y = upper.CI, 
                color = as.factor(stoch)), linetype = 'dashed') +
  theme_classic() + 
  facet_wrap(~AggregationIndex, nrow = 1) + 
  xlab('Snail Density') + ylab('Mean # Snail Contacts Per Tank') +
  labs(color='Stochasticity') +
  geom_point(data = abm.dat.mean, aes(x = snails/4, y = mean,color = as.factor(stoch)))

#ABM Contact Model Fits Plot 
pred.plots <- ggplot() +
  geom_line(data = sum.preds.abm, aes(x = SnailDensity, y = mean,
                                      color = as.factor(stoch))) + 
  geom_ribbon(data = sum.preds.abm, aes(ymin=lower.CI, ymax=upper.CI, 
                                       x = SnailDensity, fill=as.factor(stoch)), alpha = 0.5, color = NA,
              show.legend = FALSE) +
  theme_classic() + 
  facet_wrap(~AggregationIndex, nrow = 1) + 
  xlab('Snail Density') + 
  ylab('Mean # Snail Contacts Per Tank') +
  labs(color='Stochasticity') + 
  geom_point(data = abm.dat.mean, aes(x = snails/4, y = mean,color = as.factor(stoch))) +
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
  theme(plot.margin = margin(t = 0,  # Top margin
                             b = 0)) + # Bottom margin
  coord_cartesian(ylim = c(0, 20));pred.plots

#Experimental Contact Model Fits
pred.plots.exp <- ggplot() +
  geom_ribbon(data = output.ci, aes(ymax = TestContacts.high, ymin = TestContacts.low, x = TestN, fill = 'black'),color = NA, alpha = .5) +
  geom_point(data = Data, aes(x = SnailDensity, y = Count), col = 'antiquewhite4', shape = 15, position = 'jitter') +
  theme_classic() + 
  geom_line(data = output, aes(x = TestN, y = TestContacts), lwd = 1) +
  facet_wrap(~AggregationIndex, nrow = 1) + 
  xlab('Host Density') + 
  ylab('Individual Snail Contacts') +
  labs(color='Stochasticity') + 
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
  theme(plot.margin = margin(t = 0,  # Top margin
                             b = 0)) + # Bottom margin
  coord_cartesian(ylim = c(0, 20));pred.plots.exp

#Supplemental Contact Model Fits
#pdf('Supplemental_ContactModelFits_15Nov23.pdf',
 #  width = 11, height = 8)
ggarrange(pred.plots + rremove('xlab'), 
          pred.plots.exp, 
          common.legend = T,
          ncol = 1, #labels = c('A', 'B'),
          legend = 'bottom')
#dev.off()

#Algae Distrubution Maps - Used in Figure 1  
alg.labs <- c('Algae Pattern = 0', 'Algae Pattern = 1', 'Algae Pattern = 2',
              'Algae Pattern = 3', 'Algae Pattern = 4', 'Algae Pattern = 5',
              'Algae Pattern = 6')
names(alg.labs) <- c(0,1,2,3,4,5,6)

#Data of Empty and Filled Pataches
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
#Algae Distrubution Plot
map.plot <- ggplot(maps, aes(x,y,fill = as.factor(algae))) +
  geom_tile(color = 'black') + theme_void() + 
  scale_fill_manual(values=c('white', 'limegreen')) +
  facet_wrap(~pattern, nrow = 1,
             labeller = labeller(pattern = alg.labs)) +
  theme(legend.position="none") + 
  theme(plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 35)) + # Left margin
  theme(strip.background = element_blank(), strip.text.x = element_blank())  

#Save Map Plot to Use in Figure 1
#pdf('Figure_Map_19June23.pdf',
 #   height = 1, width = 5)
map.plot
#dev.off()

#ABM Model Fits
pred.abm.plots <-ggplot(sum.preds.abm, aes(x = SnailDensity, y = mean, color = as.factor(stoch))) +
  geom_line() + 
  geom_ribbon(data = sum.preds.abm, aes(ymin=lower.CI, ymax=upper.CI, 
                                        x = SnailDensity, fill=as.factor(stoch)), alpha = 0.2, color = NA,
              show.legend = FALSE) +
  theme_classic() + 
  facet_wrap(~AggregationIndex, nrow = 1) + 
  ylab('Mean # Snail Contacts Per Simulation') +
  xlab('') +
  labs(color='Stochasticity') + 
  geom_point(data = abm.dat.mean, aes(x = snails/4, y = mean,color = as.factor(stoch))) +
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
  coord_cartesian(ylim = c(0, 20));pred.abm.plots


##########################################################
##########################################################
#################### ABM Disease #########################
##########################################################
##########################################################
#Function to Calculate R0 From ABM Data
findRo <- function(dat, tick){ #Tick and Data are Needed
  Ro.dat <- dat[dat$ticks == tick,]
  Ro.dat$contact.rate <- Ro.dat$contacts/Ro.dat$ticks 
  Ro.dat$transmission <- Ro.dat$infect/100
  Ro.dat$Ro <- (Ro.dat$contact.rate * Ro.dat$transmission)*Ro.dat$recover
  
  width<-function (z=1.96, lambda, N) {
    W<-z*sqrt(lambda/N)
    return(W)
  }
  #Summarize and Find Mean and Interval of R0
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

#Calculate R0 for Each Simulation Combination at Tick = 50
R0 <- findRo(abm.dat, tick = 50)  
R0$algae <- R0$algae - 1 #Adjust Aggregation Index

#R0 Plot of optimal Foraging and Random Foraging
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

#R0 plot with stochasticty on x-axis and colored by algae distribution
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
theme(legend.position="bottom") + guides(colour = guide_legend(nrow = 1)) +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  xlim(0, 100) +
  theme(plot.margin = margin(t = 0,  # Top margin
                             b = 0)) # Bottom margin
Ro.plot2


#R0 plot with density on x axis and colored by resource distribution
stoch.labels <- c('Stochasticity = 0',
                  'Stochasticity = 8',
                  'Stochasticity = 18',
                  'Stochasticity = 30',
                  'Stochasticity = 50',
                  'Stochasticity = 100',
                  'Experiment')
names(stoch.labels) <- c('0', '8', '18', '30', '50', '100','Experiment')
#Place Holder Spots for Experimental 
exp.placeholder <- data.frame(snails = seq(from = 2, to = 16, by = 2),
                              stoch = as.factor(rep('Experiment', times = 8)),
                              algae = rep(-1, 8),
                              Ro = rep(-1, 8),
                              numsnails = rep(0,8),
                              highCI = rep(-1, 8),
                              lowCI = rep(-1, 8),
                              adjust = NA)

R0$stoch <- as.factor(R0$stoch)
#Combine R0 Data and Placeholder Data and reorder the data
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
  xlim(0, 16) + ylim(0,8) +
  theme(plot.margin = margin(t = 0,  # Top margin
                             b = 0)) # Bottom margin
Ro.plot3

#Max Prevalence Data Summary
max.inf <- abm.dat %>%
  group_by(snails, stoch, algae, run) %>%
  summarise(max.inf = max(Is)) %>%
  group_by(snails, stoch, algae) %>%
  summarise(mean.Is = mean(max.inf),
            lower.ci = quantile(max.inf, 0.025),
            upper.ci = quantile(max.inf, 0.975))

#Placeholder
exp.placeholder <- data.frame(snails = seq(from = 2, to = 16, by = 2),
                              stoch = as.factor(rep('Experiment', times = 8)),
                              algae = rep(-1, 8),
                              mean.Is = rep(-1, 8),
                              lower.ci = rep(-1, 8),
                              upper.ci = rep(-1, 8))

max.inf$stoch <- as.factor(max.inf$stoch)

inf.data <- rbind(max.inf, exp.placeholder)
inf.data$stoch <- factor(inf.data$stoch, levels = c('Experiment',0, 8, 18, 30, 50, 100))

#Prevalence Plot
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

sum.preds.abm$stoch <- as.factor(sum.preds.abm$stoch)

output$lower.CI <- NA
output$upper.CI <- NA
colnames(output) <- c('SnailDensity', 'mean', 'AggregationIndex', 'stoch', 'lower.CI', 'upper.CI')

pred.fulldat <- rbind(output, sum.preds.abm)

pred.fulldat$stoch <- factor(pred.fulldat$stoch, levels = c('Experiment', '0','8','18','30','50','100'))

#Model Fits for Both Experiment and ABM -- Plotted Across Stochasiticity
pred.plots.2 <- ggplot() +
  geom_line(data = pred.fulldat, aes(x = SnailDensity, y = mean, color = as.factor(AggregationIndex),
                                     group = as.factor(AggregationIndex))) +
  theme_classic() + 
  facet_wrap(~stoch, nrow = 1, labeller = labeller(stoch = stoch.labels, multi_line = TRUE)) + 
  xlab('Snail Density') + 
  ylab('Mean # Snail Contacts Per Tank') +
  xlab('') +
  labs(color='Aggregation Level') + 
  theme(legend.position="bottom") + 
  guides(colour = guide_legend(nrow = 1)) +
  theme(plot.margin = margin(t = 0,  # Top margin
                             b = 0)) + # Bottom margin
  coord_cartesian(ylim = c(0, 15));pred.plots.2

#Figure 2 Plot
#pdf('New_Figure2_Contacts_Disease_8Dec23.pdf',
 #   height = 8, width = 11)
ggarrange(pred.plots.2 + rremove('xlab') + ylab('Host Contact Rates'), 
          Ro.plot3 + rremove('xlab'), inf.plot, 
          common.legend = T,
          legend = 'bottom', ncol = 1)
          #labels = c('a','b', 'c'))
#dev.off()


#Extra Plot of Model Fits below algae distrubutions
ggarrange(map.plot, pred.plots,
          labels = c('A','B'),
          nrow = 2,
          heights = c(.5, 2), widths = c(.5,2))

##########################################################
##########################################################
###### Exploring Encounter Rate and Handling Time ########
##########################################################
##########################################################

#Handling Times -- ABM Renaming variables
handling.time$variable <- as.character(handling.time$variable)
handling.time[handling.time$variable == 'H.1.','variable'] <- '0'
handling.time[handling.time$variable == 'H.2.','variable'] <- '1'
handling.time[handling.time$variable == 'H.3.','variable'] <- '2'
handling.time[handling.time$variable == 'H.4.','variable'] <- '3'
handling.time[handling.time$variable == 'H.5.','variable'] <- '4'
handling.time[handling.time$variable == 'H.6.','variable'] <- '5'
handling.time[handling.time$variable == 'H.7.','variable'] <- '6'
handling.time$variable <- as.numeric(handling.time$variable)

#Handling Time Plot
ggplot(handling.time, aes(y = value, x = variable)) +  
  geom_point() + theme_classic() +
  facet_wrap(~stochs) + 
  ylab('Handling Time') + xlab('Aggregation Index')

#Creating Distance to Resource Column
handling.time$group <- 0.56
handling.time[handling.time$variable == 4,'group'] <- 0.67
handling.time[handling.time$variable == 5,'group'] <- 0.72
handling.time[handling.time$variable == 6,'group'] <- 0.86

handling.time$stochs <- as.factor(handling.time$stochs)

#Sampling posterior
table(handling.time$stochs, handling.time$variable)

handling.time.samps.abm <- handling.time[!(handling.time$stochs %in% c('Experiment')),] %>% 
  group_by(stochs, group, variable) %>%
  slice(seq(1, n(), by = 650))

handling.time.samps.exp <- handling.time[handling.time$stochs %in% c('Experiment'),] %>% 
  group_by(stochs, group, variable) %>%
  slice(seq(1, n(), by = 200))

#Combining Samples
handling.time.samps <- rbind(handling.time.samps.abm, handling.time.samps.exp)

table(handling.time.samps$stochs, handling.time.samps$variable)
handling.time.samps.exp <- handling.time.samps[!(handling.time.samps$stochs %in% c('Experiment')),]

#Run Models for 0-18 Stochs
mod.h.abm <- list()
for(i in 1:(length(stochs)-3)){
mod.h.abm[[i]] <- glm(value ~ group*variable, data = handling.time.samps[handling.time.samps$stochs %in% c(stochs[[i]]),],
                      family = 'Gamma')
}
summary(mod.h.abm[[2]])

#handling Time Plot for Optimal Foraging Across Algae Distribution
ggplot(handling.time.samps[which(handling.time.samps$stochs == "0"),], 
       aes(y = value, x = variable)) +  
  geom_point() + theme_classic() +
  facet_wrap(~stochs) + 
  ylab('Handling Time') + xlab('Aggregation Index')

#Creating New Data For Predict Function
newdata.h <- data.frame(#"stochs" = as.factor(rep(rep(stochs, each = 7), times = 2)),
                      "variable" = 0:6)

newdata.h$group <- 0.56
newdata.h[newdata.h$variable == 4,'group'] <- 0.67
newdata.h[newdata.h$variable == 5,'group'] <- 0.72
newdata.h[newdata.h$variable == 6,'group'] <- 0.86
newdata.h$group <- as.numeric(newdata.h$group)

#Use Predict Functions to Get Model Fits
newdata.h.fits <-list()
for(i in 1:length(mod.h.abm)){
newdata.h.fits[[i]] <- cbind(newdata.h,
                 predict.glm(mod.h.abm[[i]], newdata = newdata.h,
                             type = 'response', se.fit = TRUE))
}

#Model Fit Data
newdata.h <- rbind(newdata.h.fits[[1]],
      newdata.h.fits[[2]],
      newdata.h.fits[[3]])
newdata.h$stochs <- rep(stochs[1:3], each = 7)

#Model Fits Plot
ggplot() +  
  geom_point(data = handling.time.samps[!(handling.time.samps$stochs %in% c('Experiment')),], 
             aes(y = value, x = variable)) +
  geom_line(data = newdata.h, 
            aes(y = fit, x = variable)) +
  theme_classic() +
  facet_wrap(~stochs) + 
  ylab('Handling Time') + xlab('Aggregation Index')

#Encounter Rates -- Experiment
mod.h.exp <- glm(value ~ variable*group, handling.time.samps[(handling.time.samps$stochs %in% c('Experiment')),],
                 family = 'Gamma')
summary(mod.h.exp)
#car::vif(mod.h.exp)#,type = 'predictor)

#predict function data
newdata.exp.h <- data.frame('variable' = 0:6,
                          'group' = c(0.56,0.56,0.56,0.56,0.67,0.72,0.86))
newdata.exp.h$group <- as.numeric(newdata.exp.h$group)
newdata.exp.h$variable <- as.numeric(newdata.exp.h$variable)

#Data frame for predict function
newdata.exp.h <- cbind(newdata.exp.h,
                     predict.glm(mod.h.exp,newdata = newdata.exp.h, 
                             type = 'response', se.fit = TRUE))
#Experimental Model fits
ggplot() +  
  geom_point(data = handling.time.samps[(handling.time.samps$stochs %in% c('Experiment')),], 
             aes(y = value, x = variable)) +
  geom_line(data = newdata.exp.h, 
            aes(y = fit, x = variable)) +
  theme_classic() +
  ylab('Handling Time') + xlab('Aggregation Index')

##########################
### Encounter Rates -- ABM rename
encounter.rates$variable <- as.character(encounter.rates$variable)
encounter.rates[encounter.rates$variable == 'a.1.','variable'] <- '0'
encounter.rates[encounter.rates$variable == 'a.2.','variable'] <- '1'
encounter.rates[encounter.rates$variable == 'a.3.','variable'] <- '2'
encounter.rates[encounter.rates$variable == 'a.4.','variable'] <- '3'
encounter.rates[encounter.rates$variable == 'a.5.','variable'] <- '4'
encounter.rates[encounter.rates$variable == 'a.6.','variable'] <- '5'
encounter.rates[encounter.rates$variable == 'a.7.','variable'] <- '6'
encounter.rates$variable <- as.numeric(encounter.rates$variable)

#Encounter Rate Plot
ggplot(encounter.rates, aes(y = value, x = variable)) +  
  geom_point() + theme_classic() +
  facet_wrap(~stochs) + 
  ylab('Encounter Rate') + xlab('Aggregation Index')

#Distance to Resource Column Creation
encounter.rates$group <- 0.56
encounter.rates[encounter.rates$variable == 4,'group'] <- 0.67
encounter.rates[encounter.rates$variable == 5,'group'] <- 0.72
encounter.rates[encounter.rates$variable == 6,'group'] <- 0.86
encounter.rates$group <- as.numeric(encounter.rates$group)

encounter.rates$stochs <- as.factor(encounter.rates$stochs)

#Sampling posterior
table(encounter.rates$stochs, encounter.rates$variable)

encounter.rates.samps.abm <- encounter.rates[!(encounter.rates$stochs %in% c('Experiment')),] %>% 
  group_by(stochs, group, variable) %>%
  slice(seq(1, n(), by = 650))

encounter.rates.samps.exp <- encounter.rates[encounter.rates$stochs %in% c('Experiment'),] %>% 
  group_by(stochs, group, variable) %>%
  slice(seq(1, n(), by = 200))
#Combine Samples
encounter.rates.samps <- rbind(encounter.rates.samps.abm, encounter.rates.samps.exp)

table(encounter.rates.samps$stochs, encounter.rates.samps$variable)
encounter.rates.samps.exp <- encounter.rates.samps[!(encounter.rates.samps$stochs %in% c('Experiment')),]

#Linear Model fits encounter rates
mod.a.abm <- list()
for(i in 1:(length(stochs)-3)){
mod.a.abm[[i]] <- glm(value ~ group*variable, encounter.rates.samps.exp[encounter.rates.samps.exp$stochs %in% c(stochs[i]),],
                      family = Gamma)
}
summary(mod.a.abm[[1]])
#car::vif(mod.a.abm)#, type = 'predictor')

#newdata for predictions
newdata <- data.frame(#"stochs" = as.factor(rep(rep(stochs, each = 7), times = 2)),
                      "variable" = 0:6, 'group' = c(0.56,0.56,0.56,0.56,0.67,0.72,0.86))

newdata.fits <- list()
for(i in 1:length(mod.a.abm)){
newdata.fits[[i]] <- cbind(newdata,
                 predict.glm(mod.a.abm[[i]], newdata = newdata, 
                         type = 'response', se.fit = TRUE))
}
#Model fits predictions
newdata <- rbind(newdata.fits[[1]],
                   newdata.fits[[2]],
                   newdata.fits[[3]])

newdata$stochs <- rep(stochs[1:3], each = 7)

#Model Fits and Encounter Rate Plots
ggplot() +  
  geom_point(data = encounter.rates.samps[!(encounter.rates.samps$stochs %in% c('Experiment')),], 
             aes(y = value, x = variable)) +
  geom_line(data = newdata, 
            aes(y = fit, x = variable)) +
  theme_classic() +
  facet_wrap(~stochs) + 
  ylab('Encounter Rate') + xlab('Aggregation Index')

#Encounter Rates -- Experiment
mod.a.exp <- glm(value ~ variable*group, encounter.rates.samps[(encounter.rates.samps$stochs %in% c('Experiment')),],
                 family = Gamma)
summary(mod.a.exp)
#car::vif(mod.a.exp)#, type = 'predictor')

#New data for prediction function
newdata.exp <- data.frame('variable' = 0:6,
                          'group' = c(0.56,0.56,0.56,0.56,0.67,0.72,0.86))
newdata.exp$group <- as.numeric(newdata.exp$group)

newdata.exp <- cbind(newdata.exp,
                     predict(mod.a.exp,newdata = newdata.exp,
                             type = 'response', se.fit = TRUE))
#Experimental Encounter Rates Model Fits
ggplot() +  
  geom_point(data = encounter.rates.samps[(encounter.rates.samps$stochs %in% c('Experiment')),], 
             aes(y = value, x = variable)) +
  geom_line(data = newdata.exp, 
            aes(y = fit, x = variable)) +
  theme_classic() +
  ylab('Encounter Rate') + xlab('Aggregation Index')

##############################
### Parameter Paper Plots ###
##############################

#Encounter Rate Plot
newdata.exp$stochs <- 'Experiment'
newdata.a.combine <- rbind(newdata, newdata.exp)

newdata.a.combine$lwr <- newdata.a.combine$fit - (2*newdata.a.combine$se.fit)
newdata.a.combine$upr <- newdata.a.combine$fit + (2*newdata.a.combine$se.fit)

encounter.rates.samps$stochs <- factor(encounter.rates.samps$stochs, levels = c('Experiment',0, 8, 18, 30, 50, 100))
newdata.a.combine$stochs <- factor(newdata.a.combine$stochs, levels = c('Experiment',0, 8, 18))


encounter.plot <- ggplot() +  
  geom_point(data = encounter.rates.samps, 
             aes(y = value, x = variable,
                 color = as.factor(stochs))) +
  geom_line(data = newdata.a.combine, 
            aes(y = fit, x = variable,
                color = as.factor(stochs))) +
  geom_line(data = newdata.a.combine, 
            aes(y = lwr, x = variable,
                color = as.factor(stochs)), lty = 2) +
  geom_line(data = newdata.a.combine, 
            aes(y = upr, x = variable,
                color = as.factor(stochs)), lty = 2) +
  theme_classic() + facet_wrap(~stochs, nrow = 1, labeller = labeller(stochs = stoch.labels, multi_line = TRUE)) +
  ylab('Encounter Rate') + xlab('') +
  scale_color_manual(values = c("0" = "#FDE725FF",
                                "8" = "#73D055FF",
                                "18" = "#29AF2FFF",
                                "30" = "#2D708EFF",
                                "50" = "#404788FF",
                                "100" = "#440154FF",
                                "Experiment" = "black")) +
  labs(color = 'Stochasticity') + theme(legend.position="bottom") + 
  guides(color = guide_legend(nrow = 1));encounter.plot

#Handling Time Plot
newdata.exp.h$stochs <- 'Experiment'
newdata.h.combine <- rbind(newdata.h, newdata.exp.h)

newdata.h.combine$lwr <- newdata.h.combine$fit - (2*newdata.h.combine$se.fit)
newdata.h.combine$upr <- newdata.h.combine$fit + (2*newdata.h.combine$se.fit)


handling.time.samps$stochs <- factor(handling.time.samps$stochs, levels = c('Experiment',0, 8, 18, 30, 50, 100))
newdata.h.combine$stochs <- factor(newdata.h.combine$stochs, levels = c('Experiment',0, 8, 18, 30, 50, 100))


handling.plot <- ggplot() +  
  geom_point(data = handling.time.samps, 
             aes(y = value, x = variable,
                 color = as.factor(stochs))) +
  geom_line(data = newdata.h.combine, 
            aes(y = fit, x = variable,
                color = as.factor(stochs))) +
  geom_line(data = newdata.h.combine, 
            aes(y = lwr, x = variable,
                color = as.factor(stochs)), lty = 2) +
  geom_line(data = newdata.h.combine, 
            aes(y = upr, x = variable,
                color = as.factor(stochs)), lty = 2) +
  theme_classic() + facet_wrap(~stochs, nrow = 1, labeller = labeller(stochs = stoch.labels, multi_line = TRUE)) +
  ylab('Handling Time (mins)') + xlab('') +
  scale_color_manual(values = c("0" = "#FDE725FF",
                                "8" = "#73D055FF",
                                "18" = "#29AF2FFF",
                                "30" = "#2D708EFF",
                                "50" = "#404788FF",
                                "100" = "#440154FF",
                                "Experiment" = "black")) +
  labs(color = 'Stochasticity') + 
  theme(legend.position="bottom",strip.background = element_blank(),
    strip.text.x = element_blank()) + 
  guides(color = guide_legend(nrow = 1));handling.plot

##########################################
####### Parameter Plot -- Figure 2 #######
param.plot <- ggarrange(encounter.plot, 
                        handling.plot + xlab('Aggregation Level'),
                        ncol = 1, #labels = c("A", "B"),
                        common.legend = TRUE,
                        legend = 'bottom',
                        align = 'hv')
#pdf('Figure_Params_9Dec23.pdf',
 #height = 8, width = 11)
param.plot
#dev.off()
##########################################


##################################################################
########## Duration Violin Plot - Supplemental Figure 7 ##########
##################################################################

abm.durdata.combo <- abm.dat.50[,c('stoch', 'AggregationIndex', 'contact_dur')]
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
  geom_point(position = position_dodge(width = 0.5)) + theme_classic() +
  facet_wrap(~stoch, nrow = 1, labeller = labeller(stoch = stoch.labels)) +
  ylab('Average Contact Duration (mins)') + xlab('Aggregation Level') +
  labs(color = 'Aggregation Level') +
  theme(legend.position = 'bottom') + 
  guides(colour = guide_legend(nrow = 1));dur.plot_3



#pdf('Figure_ContactDuration_9Dec23.pdf',
 #height = 8, width = 11)
dur.plot_3
#dev.off()
#############################################

##################################################################################
########## Linear Model Output Summaries - Supplemental Tables  2 and 3 ##########
##################################################################################


#Output Model Summaries for Paper -- create and save csv files with model output -- supplemental material
#library(broom)
#write.csv(rbind(tidy(mod.a.abm[[1]]),tidy(mod.a.abm[[2]]),tidy(mod.a.abm[[3]])), 'Encounter_ABM_9Dec23.csv')
#write.csv(broom::tidy(mod.a.exp), 'Encounter_EXP_9Dec23.csv')
#write.csv(rbind(tidy(mod.h.abm[[1]]),tidy(mod.h.abm[[2]]),tidy(mod.h.abm[[3]])), 'Handling_ABM_9Dec23.csv')
#write.csv(broom::tidy(mod.h.exp), 'Handling_EXP_9Dec23.csv')


