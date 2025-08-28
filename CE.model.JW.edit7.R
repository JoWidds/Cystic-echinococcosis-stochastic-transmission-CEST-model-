###############################################
## Transmission dynamics model for CE in Rio Negro Argentina created by J.Widdicombe, Aug 2025
## Beta values fitted to prev 4.5% and 56.3% in dogs and sheep respectively (2009) following 29 years of PZQ application, prior to EG95 vac.  
# As per Mujica et. al 2021 ## Fitted to adult sheep prev (>2 years old)  
# Fitted with 20 year burn in, 29 years PZQ only. Introduction of the EG95 vaccine in the Rio Negro control programme during 2009-2025)
### All new intervention packages run from 2025 onwards.

setwd ("C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Final Model Simulations")
rm(list=ls())
source('C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Model.fitting.R') 
FinalPars <- read.csv('C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Fitting/ParticleSim6.csv')#Posteriors 

yr <- 20+29+16+10  #20 yr burn in (start 1960), then worming 29 years(1980), EG95 16 years(2009).
timesteps <- seq(1,yr*12,1) 
dt = 1/12 #monthly

scenario = 4

#### Set interventions
treat.lambs = T
treat.dogs = T ## T or F

worm.eff <- 0.65 #coverage, assumes 100% efficacy 
#worming frequency every x month (worming q3 monthly for 29 years, then set to new worming freq thereafter) 
# vf vaccine freq #coverage
# Vaccine - start in 2009 
# vac.eff= 0.78 based on efficacy of vaccine being 98% following three doses. 
# Therefore probability at each round is (1-x)^3 = 1-0.98. x = 0.78
# Assumption is if farm is vaccinated, then all animals on that unit are vaccinated. 
# Each simulation becomes a farming unit. Some farms are not vaccintaed as have chosen to opt out

if (scenario ==1){ ##CF
    wf = 3
    vf = 0.60 
} else if (scenario ==2){ ## Low freq
    wf = 6 
    vf = 0.60  
} else if (scenario ==3){ ## High freq
    wf = 1
    vf = 0.60  
} else if (scenario ==4){
    wf = 3
    vf = 0.60}    ## vf remains the same until 2025, then coverage increased (as per for loop). 


Calendar.Year <- matrix(1:length(timesteps), ncol = 12, byrow=T)
Worm.protocol <- Calendar.Year[21:65,] #start PZQ interventions after burn in (20 yrs) for 45 years (1980 - 2025)
Worm.protocol <- sort(c(Worm.protocol))  
Worm.protocol <- Worm.protocol[seq(1, length(Worm.protocol), 3)] ##every 3 months between 1980 and 2025 ()
Worm.protocol <- c(Worm.protocol, seq(Worm.protocol[180]+1,length(timesteps), by = wf))##sets worming to every x month after 2025


if (yr>49){
  Vac.protocol <- Calendar.Year[49:nrow(Calendar.Year),1]
  Vac.protocol2 <- Calendar.Year[49:nrow(Calendar.Year),2]
}


##sine wave for egg decay rate by season ### see figures.model for curve plot 
x = seq(1, 12, 1)
  for (i in x) {
      season <- 0.11*sin(x/4)+ 0.01}
season <- rep(season, 20) ##20 years worth of seasons egg risk!

reps <- 10000  #25000

#### Define model parameters based on fitting ####

dog_beta_vec <- sample(FinalPars$cbetaD, size = reps, replace = T, prob = NULL) ##Drawing randomly from final fitted parameters 
sheep_beta_vec <- sample(FinalPars$cbetaL, size = reps, replace = T, prob = NULL) 

Sim.list <- c()

iterationlist <- replicate(n = length(sheep_beta_vec),
                  expr ={as.data.frame(matrix(NA, nrow = length(timesteps), ncol= 58))},
                    simplify = F)

set.seed(1)

for (s in 1:length(sheep_beta_vec)) {
  
    Sim.list[[1]] <- stepFStoc(Sd.IP,Ed.IP,Id.IP,Rd.IP, ##Dogs Initial parameters
                                Ss.IP,Es.IP,Es2.IP,Es3.IP,Is.IP,Vs.IP, ##Sheep Initial parameters
                                Eggs.IP, ##Eggs
                                dog_beta_vec[s],
                                sheep_beta_vec[s], 
                                Cs.IP)##added in culled sheep 


###simulates first year based on initial parameters given
      for (i in 2:length(timesteps)) {
  
      Sim.list[[i]] <- stepFStoc(Sim.list[[i-1]][1:5],Sim.list[[i-1]][6:10], ##Dogs
                            Sim.list[[i-1]][11:15],Sim.list[[i-1]][16:20], ##Dogs
                            Sim.list[[i-1]][21:26],Sim.list[[i-1]][27:32], ##Sheep
                            Sim.list[[i-1]][33:38],Sim.list[[i-1]][39:44], ##Sheep
                            Sim.list[[i-1]][45:50],Sim.list[[i-1]][51:56], ##Sheep
                            Sim.list[[i-1]][57], ##eggs
                            dog_beta_vec[s],
                            sheep_beta_vec[s],
                            Sim.list[[i-1]][58])##culled sheep 

          #### AGEING animals yearly
      if ( (i/12) %% 1 == 0) { ### %% is integer divide (i.e when whole number, not fraction produced), here saying every 12th timestep (==1 year) do something.....
          # Sheep
      Sim.list[[i]][c(21:56,58)] <- demographics.sheep(Sim.list[[i]][21:26],Sim.list[[i]][27:32],
                                            Sim.list[[i]][33:38],Sim.list[[i]][39:44],
                                            Sim.list[[i]][45:50],Sim.list[[i]][51:56],Sim.list[[i]][58])##culled sheep 
          
          ## Dogs
      Sim.list[[i]][1:20] <-  demographics.dogs(Sim.list[[i]][1:5],Sim.list[[i]][6:10],
                                            Sim.list[[i]][11:15],Sim.list[[i]][16:20])
          
                  } 
          
          
          ####INTERVENTIONS
          ## Dog worming
          if(i %in% Worm.protocol & treat.dogs==T) { ##Worming 
          Sim.list[[i]][1:20] <- doDogDeworm(Sim.list[[i]][1:5],Sim.list[[i]][6:10],
                                            Sim.list[[i]][11:15], Sim.list[[i]][16:20])
          
                    } ##worming 'if' statment
      
      if (scenario ==4 & i >= 780) { ##updates last 10 years of vac coverage 
            vf <- 0.8
                  } else {
            vf <- 0.6
        }

          if(i %in% Vac.protocol & treat.lambs==T) {
          
          vac.eff= sample(c(0, 0.78), prob = c(1-vf,vf), 1, replace = T) ##coverage is 60% (Deterministic) - c(0.40,0.60)
          
          tmp <- doLambVac(Sim.list[[i]][21:22],Sim.list[[i]][27:28],Sim.list[[i]][33:34], Sim.list[[i]][39:40],vac.eff)
          
          
          ####double check this and below lines! should there be add/subtract of animals to maintain numbers? 
                            Sim.list[[i]][c(21,27,33,39)] <-  tmp[c(1,3,5,7)] #lambs (not vaccinated) 
                            Sim.list[[i]][c(22,28,34,40)] <-  tmp[c(2,4,6,8)] #1y old (not vaccinated)
                            
                            Sim.list[[i]][51] <- Sim.list[[i]][51] + sum(tmp[c(9,11,13,15)]) #Vac lambs
                            Sim.list[[i]][52] <- Sim.list[[i]][52] + sum(tmp[c(10,12,14,16)]) #Vaccinated 1y old
          
                } ##lamb vac 'if' statement
      
          if(i %in% Vac.protocol2 & treat.lambs==T) {
            
          tmp <- doLambVac2(Sim.list[[i]][21],Sim.list[[i]][27],Sim.list[[i]][33], Sim.list[[i]][39], vac.eff)
          
                          Sim.list[[i]][c(21,27,33,39)] <- tmp[1:4] #lambs(not vaccinated)
                          Sim.list[[i]][51] <- Sim.list[[i]][51] + sum(tmp[5:8]) #lambs (vaccinated)
                                                  
                } ##lamb vac 2 'if' statement
            
            } #for function

            iterationlist[[s]] <-  as.data.frame(matrix(unlist(Sim.list),nrow=length(Sim.list),byrow=TRUE))
            
        } ##for betas

#iterationlist

colnames <- c("Sd.1","Sd.2","Sd.3","Sd.4","Sd.5",
              "Ed.1","Ed.2","Ed.3","Ed.4","Ed.5",
              "Id.1","Id.2","Id.3","Id.4","Id.5",
              "Rd.1","Rd.2","Rd.3","Rd.4","Rd.5",
              "Ss.lambs","Ss.1-2","Ss.2-3","Ss.3-4","Ss.4-5","Ss.5-6",
              "Es.lambs","Es.1-2","Es.2-3","Es.3-4","Es.4-5","Es.5-6",
              "Es2.lambs","Es2.1-2","Es2.2-3","Es2.3-4","Es2.4-5","Es2.5-6",
              "Es3.lambs","Es3.1-2","Es3.2-3","Es3.3-4","Es3.4-5","Es3.5-6",
              "Is.lambs","Is.1-2","Is.2-3","Is.3-4","Is.4-5","Is.5-6",
              "Vs.lambs","Vs.1-2","Vs.2-3","Vs.3-4","Vs.4-5","Vs.5-6",
              "Ieg", "Cull")

all <- lapply(iterationlist, setNames, colnames)

###subsets lambs and old sheep from lists by columns - retains in list  
Lambs <- lapply(all, "[", c(22,28,34,40,46)) ##all lambs, up to 4 tooth ~1-2 year olds
All.sheep <- lapply(all, "[", c(21:56)) ##all sheep 

### Eggs 
Eggs.list <- lapply(all, "[[", 57)  # each replicate's eggs
Eggs.mean <- round(Reduce("+", Eggs.list) / reps / 1000, 0)  # mean eggs per 1000

#CI's for eggs 
Eggs.arr <- simplify2array(Eggs.list)  # dims: timesteps × reps
Eggs.q   <- apply(Eggs.arr, 1, quantile, prob = c(0.025, 0.5, 0.975))
df.eggs  <- Eggs.q / 1000

### Prevalence
Lamb.prev <- lapply(Lambs, function(x) {
  (x[,5] + x[,4]) / rowSums(x[,1:5]) * 100})

All.Sheep.prev <- lapply(All.sheep, function(x) {
  (rowSums(x[,26:30]) /
     rowSums(x[,c(2:6,8:12,14:18,20:24,26:30,32:36)])) * 100})

Dog.prev <- lapply(all, function(x) {
  rowSums(x[,11:15]) / rowSums(x[,1:20]) * 100})

## Prevalence 

Lamb.prev <- lapply(Lamb.prev, function(x) replace(x, is.na(x), 0))
All.Sheep.prev <- lapply(All.Sheep.prev, function(x) replace(x, is.na(x), 0))
Dog.prev <- lapply(Dog.prev, function(x) replace(x, is.na(x), 0))

# 95% CI's (lower), median (as not normally dist), and upper bound CI)
df.lamb  <- apply(simplify2array(Lamb.prev), 1, quantile, prob = c(0.025, 0.5, 0.975))
df.sheep <- apply(simplify2array(All.Sheep.prev), 1, quantile, prob = c(0.025, 0.5, 0.975))
df.dog   <-  apply(simplify2array(Dog.prev), 1, quantile, prob = c(0.025, 0.5, 0.975))

## mean value of every 12 timesteps (each year) 
df.eggs <- sapply(seq(1, ncol(df.eggs), by = 12), function(i) {
  rowMeans(df.eggs[, i:min(i+11, ncol(df.eggs))], na.rm = TRUE)})

df.lamb <- sapply(seq(1, ncol(df.lamb), by = 12), function(i) {
  rowMeans(df.lamb[, i:min(i + 11, ncol(df.lamb))], na.rm = TRUE)})

df.sheep <- sapply(seq(1, ncol(df.sheep), by = 12), function(i) {
  rowMeans(df.sheep[, i:min(i + 11, ncol(df.sheep))], na.rm = TRUE)})

df.dog <- sapply(seq(1, ncol(df.dog), by = 12), function(i) {
  rowMeans(df.dog[, i:min(i + 11, ncol(df.dog))], na.rm = TRUE)})

## Normalise eggs (relative reduction, not absolute count)
df.eggs <- df.eggs / (df.eggs[2,20]) 
df.eggs[df.eggs < 0] <- 0 

### Convert to data frames
df.sheep <- as.data.frame(df.sheep)
df.lamb <- as.data.frame(df.lamb)
df.eggs <- as.data.frame(df.eggs)
df.dog <- as.data.frame(df.dog)


colnames(df.eggs) <- 1:ncol(df.eggs)
colnames(df.lamb) <- 1:ncol(df.lamb)
colnames(df.sheep) <- 1:ncol(df.sheep)
colnames(df.dog) <- 1:ncol(df.dog)

plot(as.numeric(df.sheep[2, ]), type = 'l')

#### ELIMINATION thresholds 
elim.threshold <- c(20,10,0)

timesteps <- length(Dog.prev[[1]])

reaches.elim.d <- matrix(0, nrow = timesteps, 
                         ncol = length(elim.threshold))
for (i in 1:timesteps) {
  for (j in 1:length(elim.threshold)) {
    # Extract the ith timepoint across all replicates
    values_at_i <- sapply(Dog.prev, function(x) x[i])
    count <- sum(values_at_i <= elim.threshold[j])
    percentage <- count / length(Dog.prev) * 100
    reaches.elim.d[i, j] <- percentage
  }
}
colnames(reaches.elim.d) <- c(20,10,0)

reaches.elim.s <- matrix(0, nrow = timesteps, 
                         ncol = length(elim.threshold))

for (i in 1:timesteps) {
  for (j in 1:length(elim.threshold)) {
    # Extract the ith timepoint across all replicates
    values_at_i <- sapply(All.Sheep.prev, function(x) x[i])
    count <- sum(values_at_i <= elim.threshold[j])
    percentage <- count / length(All.Sheep.prev) * 100
    reaches.elim.s[i, j] <- percentage
  }
}

colnames(reaches.elim.s) <- c(20,10,0)

###Saving output files of simulations (vac per year, q x months worming per year)
### rows "2.50%", "50%","97.50%"
my.list <- list(Final.lamb = df.lamb, Final.sheep= df.sheep, Final.dog = df.dog, Final.eggs = df.eggs, WHO.Elim.dog = reaches.elim.d, WHO.Elim.sheep = reaches.elim.s)
#mapply(write.csv, my.list, file = paste0(names(my.list), vf,".", wf, '.csv'), row.names = F)
mapply(write.csv, my.list, file = paste0(names(my.list),scenario,'.csv'), row.names = F)

## See Figures.model.R for graphs