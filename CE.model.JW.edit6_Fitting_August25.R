#### Model fitting code for CEST model August 2025. Written by J.Widdicombe ####  
## 20 year burn in prior to any interventions
## Target prev 4.5 and 56.3 in dogs and sheep respectively (2009) following 29 years of PZQ application, prior to EG95 vac.  
# As per Mujica et. al 2021 ## Fitted to adult sheep prev (>2 years old)  
rm(list=ls())  

library(rstudioapi)
source('C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Model.fitting.R') 

#### Time parameters- year starts beginning of lambing season - Spring
yr <- 20+29   #20 yr burn in/29 years (before) EG95 vaccine. 
timesteps <- seq(1,yr*12,1) 
dt = 1/12 #monthly

#Set interventions
worm.eff <- 0.65 #coverage, assumes 100% efficacy of PZQ
treat.dogs = T 
treat.lambs = F

wf = 3 #worming frequency every x month 
Calendar.Year <- matrix(1:length(timesteps), ncol = 12, byrow=T)
Worm.protocol <- Calendar.Year[21:yr,] #start PZQ interventions after burn in 
Worm.protocol <- sort(c(Worm.protocol))  
Worm.protocol <- Worm.protocol[seq(1, length(Worm.protocol), wf)] ##sets worming to every x month

Calendar.Year <- matrix(1:length(timesteps), ncol = 12, byrow=T)
Vac.protocol <- Calendar.Year[49:nrow(Calendar.Year),1]
Vac.protocol2 <- Calendar.Year[49:nrow(Calendar.Year),2]

##sine wave for egg decay rate by season
x = seq(1, 12, 1)
  for (i in x) {
  season <- 0.11*sin(x/4)+ 0.01}
season <- rep(season, 20) 

reps <- 10000

## Fitting - ABC-SMC fitting  
##### PRIORS - based on random generation of priors 
## ABC code used to generate new particles from priors following rejection/acceptance - the below follows after first run/fit 

  for (fit in 0:5) {
      
        if (fit > 0) {
  ParSim2 <- read.csv(paste0("C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Fitting/ParticleSim", fit, ".csv"))
      }

        if (fit == 0){
  
  sheep_beta_vec <-  runif(reps, 0.9, 1) #runif(reps, 0.8, 1.3) # #
  dog_beta_vec <- runif(reps,3,6) 
  
      } else {
        
        sheep_beta_vec <- runif(reps, min(ParSim2$cbetaL), max(ParSim2$cbetaL))
        dog_beta_vec <- runif(reps, min(ParSim2$cbetaD), max(ParSim2$cbetaD))
        
      }


Sim.list <- c()
iterationlist <- replicate(n = length(sheep_beta_vec),
                           expr ={as.data.frame(matrix(NA, nrow = length(timesteps), ncol= 58))},
                           simplify = F)

set.seed(1)

for (s in 1:length(sheep_beta_vec)) {
  
  Sim.list[[1]] <- stepFStoc(Sd.IP,Ed.IP,Id.IP,Rd.IP, ##Dogs Initial parameters
                             Ss.IP,Es.IP,Es2.IP,Es3.IP,Is.IP,Vs.IP, ##Sheep Initial parameters
                             Eggs.IP, ##Eggs
                             dog_beta_vec [s],
                             sheep_beta_vec[s],
                             Cs.IP)
  
  
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
                               Sim.list[[i-1]][58])##cull sheep 
    
    #### AGEING animals yearly
    if ( (i/12) %% 1 == 0) { 
      # Sheep
      Sim.list[[i]][c(21:56,58)] <- demographics.sheep(Sim.list[[i]][21:26],Sim.list[[i]][27:32],
                                                 Sim.list[[i]][33:38],Sim.list[[i]][39:44],
                                                 Sim.list[[i]][45:50],Sim.list[[i]][51:56],
                                                 Sim.list[[i-1]][58])
      
      ## Dogs
      Sim.list[[i]][1:20] <-  demographics.dogs(Sim.list[[i]][1:5],Sim.list[[i]][6:10],
                                                Sim.list[[i]][11:15],Sim.list[[i]][16:20])
      
    } 
    
    
    ####INTERVENTIONS
    ## Dog worming
    if(i %in% Worm.protocol & treat.dogs==T) { ##Worming every 3 months
      Sim.list[[i]][1:20] <- doDogDeworm(Sim.list[[i]][1:5],Sim.list[[i]][6:10],
                                         Sim.list[[i]][11:15], Sim.list[[i]][16:20])
        } 
    
    ## Sheep vaccination 
    if(i %in% Vac.protocol & treat.lambs==T) {
      vac.eff= sample(c(0, 0.78), prob = c(0.40,0.60), 1, replace = T) ##coverage is 60% (Deterministic)
            tmp <- doLambVac(Sim.list[[i]][c(21,22)],Sim.list[[i]][c(27,28)],Sim.list[[i]][c(33,34)], Sim.list[[i]][c(39,40)],vac.eff)
      Sim.list[[i]][c(21,27,33,39)] <- tmp[c(1,3,5,7)] #lambs (not vaccinated)
      Sim.list[[i]][c(22,28,34,40)] <- tmp[c(2,4,6,8)] #1y old
      Sim.list[[i]][51] <- sum(tmp[c(9,11,13,15)]) #Vac lambs
      Sim.list[[i]][52] <- sum(tmp[c(10,12,14,16)]) #Vaccinated 1y old
      
    } ##lamb vaccination 
    
    if(i %in% Vac.protocol2 & treat.lambs==T) {
      
      tmp <- doLambVac(Sim.list[[i]][21],Sim.list[[i]][27],Sim.list[[i]][33], Sim.list[[i]][39], vac.eff)
      Sim.list[[i]][c(21,27,33,39)] <- tmp[1:4] #lambs(not vaccinated)
      Sim.list[[i]][51] <- sum(tmp[5:8]) #lambs (vaccinated)
      
    } ##lamb vac 'if' statement
    
  } #for function
  
  iterationlist[[s]] <-  as.data.frame(matrix(unlist(Sim.list),nrow=length(Sim.list),byrow=TRUE))
  
} ##for betas

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

rm(iterationlist)

###subsets lambs and old sheep from lists by columns - retains in list  
Lambs <- lapply(all, "[", c(22,28,34,40,46)) ## ~1-2 year olds
All.sheep <- lapply(all, "[", c(21:56)) ##all sheep  

Lamb.prev <- lapply(seq_along(Lambs), function(i) (Lambs[[i]][,5]+Lambs[[i]][,4])/rowSums(Lambs[[i]][,1:5])*100) ##include final year of E class too

#All.Sheep.prev <- sapply(seq_along(All.sheep), function(i) {(rowSums(All.sheep[[i]][,27:30])/rowSums(All.sheep[[i]][,c(3:6, 9:12, 15:18, 21:24, 27:30)]))*100}) ## subsets 2+ y/o 
#Dog.prev <- lapply(seq_along(all), function(i) rowSums(all[[i]][,11:15])/rowSums(all[[i]][,1:20])*100)

## Subsets the mean value of the last 12 timesteps (year) of the simulations 
Final.P.Lambs <- vapply(Lamb.prev, function(x) mean(tail(x, 12)), numeric(1))

Final.P.sheep <- vapply(All.sheep, function(x) {
  prev <- (rowSums(x[,27:30]) /
             rowSums(x[,c(3:6,9:12,15:18,21:24,27:30)])) * 100
  mean(tail(prev, 12))
}, numeric(1))

Final.P.dogs <- vapply(all, function(x) {
  prev <- rowSums(x[,11:15]) / rowSums(x[,1:20]) * 100
  mean(tail(prev, 12))
}, numeric(1))

Final.prev <- data.frame(
  dog_beta_vec,
  sheep_beta_vec,
  Final.P.dogs,
  Final.P.Lambs,
  Final.P.sheep
)

colnames(Final.prev)<- c('dog_beta_vec', 'sheep_beta_vec', 'prevD','prevL', 'prevS')
summary(Final.prev)


if (fit == 0 ){
    write.csv(Final.prev, file= "Final.prev.csv", row.names = F) 
  
      } else {
        
    write.csv(Final.prev , file = paste0("FittingBetas.", fit, ".csv"), row.names = F)} 

#### ABC fitting for Echino model Aug 2025 ###
#### Includes Sequential MCMC resamplig (5 fits) of CEST model
# Final.prev dataframe is prevalence outputs for model given prior beta's of sheep and dog. 

source("C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Fitting/run_ABC_fit.R")
run_ABC_fit(fit)

}

