####################################################
######Code to Test ABM Algae Regrowth Effect #######
####################################################

#Libraries
library(R2jags)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(ggpubr)
library(performance)

#Data - Load in Data From Patch Regrowth Model in Long Format
#abm.dat

#How does # of algal patches change over time
sum.algae.patches <- abm.dat %>%
 group_by(snails, stoch, algae, ticks) %>%
summarise(mean = mean(algae_patches),
         se = sd(algae_patches)/n(),
        n = n())

ggplot() +
 geom_line(data = sum.algae.patches[sum.algae.patches$stoch %in% c(0,100),], 
          aes(x = ticks, y = mean, color = as.factor(snails), linetype = as.factor(stoch))) +
theme_classic() +
facet_grid(algae~regrow)

#Rename ABM Data Columns
colnames(abm.dat) <- c('run', 'start_infections', 'feeding', 'algae_conc',
                       'snails', 'stoch',
                       'infect', 'algae', 'recover', 'GUD', 'regrow', 
                       'resources','edge', 'patch_time',
                       'Ss', 'Is', 'Rs', 'contacts', 'total_contacts',
                       'patches','unique_contacts', 'contact_dur', 'alage_patches', 
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
abm.dat.50 <- abm.dat.50[abm.dat.50$regrow == 100,] #The Resources will Regrow
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


saveRDS(results, 'ABMModelResults_Regrowth.RDS') #Save Model Run to Not have to Rerun Model Loop
results_regrowth <- readRDS('ABMModelResults_Regrowth.RDS') #Load in Saved Model Results to Avoid Rerunning Model Loop

#Model Fits for ABM -- Regrowth 
preds.abm.regrowth <- data.frame(rbind(results_regrowth[[1]]$BUGSoutput$sims.matrix[,15:77],
                                       results_regrowth[[2]]$BUGSoutput$sims.matrix[,15:77],
                                       results_regrowth[[3]]$BUGSoutput$sims.matrix[,15:77],
                                       results_regrowth[[4]]$BUGSoutput$sims.matrix[,15:77],
                                       results_regrowth[[5]]$BUGSoutput$sims.matrix[,15:77],
                                       results_regrowth[[6]]$BUGSoutput$sims.matrix[,15:77]))

preds.abm.regrowth$stoch <- rep(stochs, each = nrow(results_regrowth[[1]]$BUGSoutput$sims.matrix[,8:14]))
preds.abm.regrowth <- melt(preds.abm.regrowth, id.vars  = 'stoch')

#Create Host Density Column
preds.abm.regrowth$AggregationIndex <- as.numeric(substr(preds.abm.regrowth$variable, 9,9))
preds.abm.regrowth$AggregationIndex <- preds.abm.regrowth$AggregationIndex - 1
preds.abm.regrowth$SnailDensity <- as.numeric(substr(preds.abm.regrowth$variable, 11,11))
preds.abm.regrowth[preds.abm.regrowth$SnailDensity == 1, 'SnailDensity'] <- 0
preds.abm.regrowth[preds.abm.regrowth$SnailDensity == 6, 'SnailDensity'] <- 10
preds.abm.regrowth[preds.abm.regrowth$SnailDensity == 7, 'SnailDensity'] <- 12
preds.abm.regrowth[preds.abm.regrowth$SnailDensity == 8, 'SnailDensity'] <- 14
preds.abm.regrowth[preds.abm.regrowth$SnailDensity == 9, 'SnailDensity'] <- 16
preds.abm.regrowth[preds.abm.regrowth$SnailDensity == 2, 'SnailDensity'] <- 2
preds.abm.regrowth[preds.abm.regrowth$SnailDensity == 4, 'SnailDensity'] <- 6
preds.abm.regrowth[preds.abm.regrowth$SnailDensity == 3, 'SnailDensity'] <- 4
preds.abm.regrowth[preds.abm.regrowth$SnailDensity == 5, 'SnailDensity'] <- 8

#Model Fits for ABM - No Regrowth 
results <- readRDS('ABMModelResults.RDS') #This is output from the modelfit script file - this is the model above run with no regrowth

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


#Summarize Model Fits
#Mean and Interval Around Mean Contacts
sum.preds.abm.regrowth <- preds.abm.regrowth %>%
  group_by(SnailDensity, AggregationIndex, stoch) %>%
  summarise(mean = mean(value),
            lower.CI = quantile(value, 0.025),
            upper.CI = quantile(value, 0.975))

#Mean and Interval Around Mean Contacts
sum.preds.abm <- preds.abm %>%
  group_by(SnailDensity, AggregationIndex, stoch) %>%
  summarise(mean = mean(value),
            lower.CI = quantile(value, 0.025),
            upper.CI = quantile(value, 0.975))


#Difference Plot 
#Merging Data and Find differences between the two fits
difference <- merge(sum.preds.abm.regrowth[,1:4], sum.preds.abm[,1:4], by = c('SnailDensity', 'AggregationIndex', 'stoch'))
difference$difference <-  difference$mean.y - difference$mean.x

#Plotting Difference
ggplot(difference, aes(x = SnailDensity, y = difference, color = as.factor(stoch))) +
 geom_line() + 
  theme_classic() + 
 facet_wrap(~AggregationIndex, nrow = 1) + 
  xlab('Snail Density') + ylab('Mean # Contacts (Regrowth) - Mean # Contacts (No Regrowth)') +
 labs(color='Stochasticity') + 
  ylim(c(-1,1))

##############################################