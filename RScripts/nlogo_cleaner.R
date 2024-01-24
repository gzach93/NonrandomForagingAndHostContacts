
#variables
#num.set  #number of setting rows
#num.runs #number of runs/replicates used
#num.sum #number of summary rows
#num.out #number of output variables
#ticks #number of timesteps used
#num.combs #number of treatment combinations

nlogo.cleaner <- function(num.set, num.runs, num.sum, num.out,
                          ticks, num.combs, dat){

#Split the Data
settings.dat <- dat[1:num.set,]
summary.dat <- dat[(num.set + 1):(num.set + num.sum),]
dat <- dat[-c(1:(num.set + num.sum)),-1]

###Clean up the Data###
#add column names
colnames(dat) <- dat[1,]
dat <- dat[-1,] #removes column name row

output <- NA #creates an object to store reorganized data
n.col <- ncol(dat)#number of columns in data

total <- num.combs*num.runs #total = #runs * #combinations

for(i in 1:total){
set <- seq(1,n.col, by = num.out)#creates a sequence to split up data 
m <-set[i] #first column of a run
n <- set[i] + (num.out-1) #secound column of a run
new.dat <- dat[1:(ticks+1), m:n] #data from a run
output <- rbind(output,new.dat) #putting data from a run under other runs -- makes data long
}

output <- output[-1,] #removes the NA in first row from object creation


setting.col <- seq(2,(ncol(settings.dat)-(num.out-1)), by = num.out) #Creates a sequence of where the filled in columns are
settings.dat <- data.frame(t(settings.dat[,c(1,setting.col)]))#grabs only the filled in columns and transposes the dataframe
colnames(settings.dat) <- settings.dat[1,] #renames columns using first row
settings.dat <- settings.dat[-1,] #removes first row, where names were
settings.dat <- settings.dat[rep(seq_len(nrow(settings.dat)), each = (ticks+1)),] #repeats the row for the number of ticks/timesteps used

final.dat <- cbind(settings.dat, output) #binds setting data together and output data
final.dat$ticks <- rep(seq(0,ticks, by = 1), times = total) # adds in ticks/time steps

return(final.dat)
}
