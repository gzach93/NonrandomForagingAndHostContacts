#The Code Below Takes the nlogo_cleaner and netlogo output csv file

#NOTE: The First Seven Rows in Netlogo output csv file need to be removed prior to running this code
#This can be done in the terminal (mac) or command prompt (windows)

#Load Cleaning Function
source('nlogo_cleaner.r')
#Load in Horizontal Data with Heading Rows removed
#dat <- read.csv('Data.csv',header = F) #This Data is output from Netlogo with first seven rows with Netlogo information removed

#Run Cleaning function
new.dat <- nlogo.cleaner(num.set = 10, #number of setting rows
                         num.runs = 10, #number of runs/replicates used
                         num.sum = 6, #number of summary rows
                         num.out = 11, #number of output variables
                         ticks = 150,#number of timesteps used
                         num.combs = 336, #number of treatment combinations
                         dat = dat) #Horizontal Data

#Save Output to Be used in Later Code
write.csv(new.dat,'OutputABMSimFinal_Rerun.csv')
