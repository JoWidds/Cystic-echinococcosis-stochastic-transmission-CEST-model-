#### Define initial parameters 
###### 
init.prams <-  c(Sd = c(0,0,0,0,0), #### population refreshed every 5 years 
                 Ed = c(2,0,0,1,0),
                 Id = c(0,1,0,1,1), 
                 Rd = c(0,0,0,0,1), 
                 Ss = c(40,20,20,20,20,20), ####initial pop = 700 sheep, 7 dogs (1st year of lambs x 2, so ongoing population actually 600)
                 Es = c(40,20,20,20,20,20),  
                 Es2 = c(40,20,20,20,20,20), 
                 Es3 = c(40,20,20,20,20,20),
                 Is = c(40,20,20,20,20,20),
                 Vs = c(0,0,0,0,0,0),
                 Ieg = 250*550*2*12*7)*10    ##egg deposition * 12 months * no. dogs 

Sd.IP <-init.prams[1:5]
Ed.IP <-init.prams[6:10]
Id.IP <-init.prams[11:15]
Rd.IP <-init.prams[16:20]
Ss.IP <-init.prams[21:26]
Es.IP <-init.prams[27:32]
Es2.IP <-init.prams[33:38]
Es3.IP <-init.prams[39:44]
Is.IP <-init.prams[45:50]
Vs.IP <-init.prams[51:56]
Eggs.IP <-init.prams[57]
Cs.IP <- init.prams[50]/sum(init.prams[26],init.prams[32],init.prams[38], init.prams[44]) ##infected culled sheep  

#Stochastic model
###############
stepFStoc <- function(Sd,Ed,Id,Rd,Ss,Es,Es2,Es3,Is,Vs,Ieg,betad,betas,cs){
   
  ##Dogs 
  tmpSd = Sd
  tmpEd = Ed
  tmpId = Id
  tmpRd = Rd 
  
  ##Sheep
  tmpSs = Ss
  tmpEs = Es
  tmpEs2 = Es2
  tmpEs3 = Es3
  tmpIs = Is
  tmpVs = Vs
  
  ##Eggs
  tmpIeg = Ieg
  
  #Priors (months)
  del = 250*550*2 ##egg deposition based on Craig and Larrieu 2006 
  kd = 1/1.5 ##latent period dog ~6 weeks 
  shE = 3 #shape of Erlang  (number of Exsposed states) 
  ks = 1/4 ##latent period sheep - 4 months 
  nv = season ##egg viablity by season 
  eps = tmpIeg/sum(tmpSs,tmpEs,tmpEs2,tmpEs3,tmpIs,tmpVs) ##eggs per sheep 
  yd = 1/36 ##mean infectious period/recovery rate dogs(gamma) 
  
    
  #derivatives (transitions between categories)
  ## Dogs
  dSE = rbinom(length(tmpSd),tmpSd, 1-exp(-betad*cs*dt)) ##rate of the transitions
  dEI = rbinom(length(tmpEd),tmpEd, 1-exp(-kd*dt))
  dIR = rbinom(length(tmpId),tmpId, 1-exp(-yd*dt))
  Sd =  tmpSd - dSE ##S to E 
  Ed =  tmpEd + dSE - dEI ##E to I 
  Id =  tmpId + dEI - dIR ##I to R 
  Rd =  tmpRd + dIR ## R
  
  ## Sheep 
  sSE = rbinom(length(tmpSs),tmpSs, 1-exp(-betas*eps*dt*shE)) ##rate of the transitions 
  sEE2 = rbinom(length(tmpEs),tmpEs, (1-exp(-ks*dt*shE)))
  sE2E3 = rbinom(length(tmpEs2),tmpEs2, (1-exp(-ks*dt*shE)))
  sE3I = rbinom(length(tmpEs3),tmpEs3, (1-exp(-ks*dt*shE))) 
  
  Ss = tmpSs - sSE ##S to E 
  Es = tmpEs + sSE - sEE2 ##E to I
  Es2 = tmpEs2 + sEE2 - sE2E3
  Es3 = tmpEs3 + sE2E3 - sE3I
  Is = tmpIs + sE3I ##I 
  Vs = tmpVs 
  
  ##Egg viability
  Ieg =  tmpIeg + rpois(1,del*sum(tmpId)*dt) - rpois(1,tmpIeg*nv*dt)
  
 if (sum(Id<0)) Id = 0
 if (sum(Sd<0)) Sd = 0
 if (sum(Es3<0)) Es = 0
 if (Ieg < 0)  Ieg = 5
 if (length(tmpSs < 1)) tmpSs = 1
  
 
  return(c(Sd,Ed,Id,Rd,Ss,Es,Es2,Es3,Is,Vs,Ieg,cs))
  
    }


##Demographic info and assumptions 

demographics.sheep <- function(tmp.Ss1.6,
                               tmp.Es1.6,
                               tmp.Es2.1.6, 
                               tmp.Es3.1.6, 
                               tmp.Is1.6,
                               tmp.Vs1.6,
                               cs){
  
  
#### Removes last year of sheep and repopulates new lambs (50% culled, 50% replacement)
culledLambs <- table(factor(sample(c(rep(1,tmp.Ss1.6[1]),
                                       rep(2,tmp.Es1.6[1]),
                                       rep(3,tmp.Es2.1.6[1]),
                                       rep(4,tmp.Es3.1.6[1]),
                                       rep(5,tmp.Is1.6[1]),
                                       rep(6,tmp.Vs1.6[1])),
                                     sum(c(tmp.Ss1.6[1],
                                           tmp.Es1.6[1],
                                           tmp.Es2.1.6[1],
                                           tmp.Es3.1.6[1],
                                           tmp.Is1.6[1],
                                           tmp.Vs1.6[1]))/2),
                              levels = 1:6))
  
  tmp.Ss1.6[1]   <- tmp.Ss1.6[1] - culledLambs[1] ##removes 50% of lambs - sold or slaughtered
  tmp.Es1.6[1]   <- tmp.Es1.6[1] - culledLambs[2]
  tmp.Es2.1.6[1] <- tmp.Es2.1.6[1]- culledLambs[3]
  tmp.Es3.1.6[1] <- tmp.Es3.1.6[1]- culledLambs[4]
  tmp.Is1.6[1]   <- tmp.Is1.6[1] - culledLambs[5]
  tmp.Vs1.6[1]   <- tmp.Vs1.6[1] - culledLambs[6]
  
#Mortality/Death by natural causes 
newBorns <- ###adds in new lambs 
    sum(c(tmp.Ss1.6[6], tmp.Es1.6[6], tmp.Es2.1.6[6], tmp.Es3.1.6[6], tmp.Is1.6[6],tmp.Vs1.6[6]))*2 
  
  tmp.Ss1.6  <- c(newBorns, tmp.Ss1.6[1:5])
  tmp.Es1.6 <- c(0, tmp.Es1.6[1:5])
  tmp.Es2.1.6 <- c(0,tmp.Es2.1.6[1:5])
  tmp.Es3.1.6 <- c(0,tmp.Es3.1.6[1:5])
  tmp.Is1.6 <- c(0, tmp.Is1.6[1:5]) 
  tmp.Vs1.6 <- c(0, tmp.Vs1.6[1:5]) 
  
## Tracking proportion of infected fallen stock
#cs <-  (culledLambs[5] + tmp.Is1.6[6])/ (sum(culledLambs) + newBorns/2)
cs <- mean(culledLambs[5]/sum(culledLambs),tmp.Is1.6[6]/newBorns/2)
   
    return(c(tmp.Ss1.6, tmp.Es1.6, tmp.Es2.1.6, tmp.Es3.1.6, tmp.Is1.6, tmp.Vs1.6, cs))
  
    }


demographics.dogs <- function(Sd1.5,Ed1.5,Id1.5,Rd1.5) {  
  
  
  tmp.Sd1.5 = Sim.list[[i-1]][1:5]
  tmp.Ed1.5 = Sim.list[[i-1]][6:10]
  tmp.Id1.5 = Sim.list[[i-1]][11:15]
  tmp.Rd1.5 = Sim.list[[i-1]][16:20]
  
  ##Adds in death by natural causes
  tmp.dog <- c(tmp.Sd1.5,tmp.Ed1.5,tmp.Id1.5,tmp.Rd1.5) ##all dogs 
  tmp.dog.desc <- sort(tmp.dog, decreasing = T) ##all dogs ordered by highest - > lowest 
  empty.dog <- tmp.dog.desc[tmp.dog.desc==0] ##rest of dog groups not containing dogs 
  tmp.dog <- tmp.dog[tmp.dog!=0] ##reduces vector to those categories actually containing dogs
  
  #rate of mortality per year across all catagories  
  mort.dogs <- rep(0.1, length(tmp.dog)) 
  Death.nat.causes <- rbinom(length(tmp.dog), 1, prob = mort.dogs) ##random generation of dogs to be retired 
  tmp.dog <- tmp.dog- Death.nat.causes ## removed random dog from population as per mortality rate 
  
  tmp.dog <- c(tmp.dog,empty.dog) ##rejoins new population with empty groups 
  dog.groups <- names(c(tmp.Sd1.5,tmp.Ed1.5,tmp.Id1.5,tmp.Rd1.5))   
  
  tmp.dog <- tmp.dog[order(factor(names(tmp.dog), levels = c(dog.groups)))] ##sorts back to original order  
  
  ##Ageing out of dogs (Q 5 years)and re-populating those lost via natural causes 
  Pups <- sum(c(tmp.dog[5],  tmp.dog[10], tmp.dog[15], tmp.dog[20], Death.nat.causes)) ## pups = sum all animals in final year + death natural causes 
  
  tmp.dog[1:5] <- c(Pups, tmp.dog[1:4])
  tmp.dog[6:10] <- c(0, tmp.dog[6:9])
  tmp.dog[11:15] <- c(0, tmp.dog[11:14])
  tmp.dog[16:20] <- c(0, tmp.dog[16:19])
  
  return(c(tmp.dog[1:5],tmp.dog[6:10],
           tmp.dog[11:15],tmp.dog[16:20])) 
  
  
    }


# ###### Intervention scenarios 

#### Dog worming tx 
doDogDeworm <- function (Sd1.5,Ed1.5,Id1.5,Rd1.5) {  
  
  tmp.Sd1.5 = Sim.list[[i]][1:5]
  tmp.Ed1.5 = Sim.list[[i]][6:10]
  tmp.Id1.5 = Sim.list[[i]][11:15]
  tmp.Rd1.5 = Sim.list[[i]][16:20]
  
  
  #### Worming 
  tmp.dog <- c(tmp.Sd1.5,tmp.Ed1.5,tmp.Id1.5,tmp.Rd1.5) ##all dogs 
  tmp.dog[16:20] <- 0 ###don't include those in R group 
  tmp.dog.desc <- sort(tmp.dog, decreasing = T) ##all dogs ordered by highest - > lowest 
  empty.dog <- tmp.dog.desc[tmp.dog.desc==0]  ##rest of dog groups not containing dogs 
  tmp.dog <- tmp.dog.desc[tmp.dog.desc!=0] ##reduces vector to those categories actually containing dogs
  
  
  ###tmp.dogs.new <- tmp.dog- round((tmp.dog*worm.eff), digits = 0) 
  worm <- rep(worm.eff, length(tmp.dog)) ##worming efficacy %
  wormed.dogs <- rbinom(length(tmp.dog), 1, prob = worm)
  tmp.dogs.new <- tmp.dog  - wormed.dogs  
  tmp.dog <- c(tmp.dogs.new,empty.dog) ##rejoins new population with empty groups 
  dog.groups <- names(c(tmp.Sd1.5,tmp.Ed1.5,tmp.Id1.5))   
  
  tmp.dog <- tmp.dog[order(factor(names(tmp.dog), levels = c(dog.groups)))] ##sorts back to original order  
  
  Sim.list[[i]][1] <- tmp.dog[1] + sum(wormed.dogs) ## adds wormed dogs back into S pop 
  Sim.list[[i]][2:5] <-  tmp.dog[2:5]
  Sim.list[[i]][6:10] <- tmp.dog[6:10]
  Sim.list[[i]][11:15] <- tmp.dog[11:15]
  Sim.list[[i]][16:20] <-  Sim.list[[i]][16:20] ### those dogs in recovery groups remain there 
  
    return(c(Sim.list[[i]][1:5],Sim.list[[i]][6:10],
           Sim.list[[i]][11:15],Sim.list[[i]][16:20]))  
    }  


doLambVac <- function (tmp.Ss1.2, tmp.Es1.2, tmp.Es2.1.2, tmp.Es3.1.2, vac.eff) {
  
  tmp.Ss1.2 = Sim.list[[i]][21:22] #Susceptible yr 1&2
  tmp.Es1.2 = Sim.list[[i]][27:28] #Exposed.1 yr 1&2 
  tmp.Es2.1.2 =  Sim.list[[i]][33:34] #Exposed.2 yr 1&2 
  tmp.Es3.1.2 = Sim.list[[i]][39:40] #Exposed.3 yr 1&2 
 
  lambs <- c(tmp.Ss1.2, tmp.Es1.2, tmp.Es2.1.2, tmp.Es3.1.2) ##all the lambs 
  vaclamb <- round(lambs*vac.eff, digits = 0) #all the vaccinated lambs 
  tmp.lamb <- lambs - vaclamb  #unvaccinated lambs 
  
  tmp <- c(tmp.lamb, vaclamb) ####not vaccinated, vaccinated  
  
  return(tmp)  ##vaccinated lambs
  
  
    }


doLambVac2 <- function (tmp.Ss1.2, tmp.Es1.2, tmp.Es2.1.2, tmp.Es3.1.2, vac.eff) { ##does just the lambs again, not 1st years 
  
  tmp.Ss1.2 = Sim.list[[i]][21] #Susceptible yr 1
  tmp.Es1.2 = Sim.list[[i]][27] #Exposed.1 yr 1 
  tmp.Es2.1.2 =  Sim.list[[i]][33] #Exposed.2 yr 1 
  tmp.Es3.1.2 = Sim.list[[i]][39] #Exposed.3 yr 1 
  
  lambs <- c(tmp.Ss1.2, tmp.Es1.2, tmp.Es2.1.2, tmp.Es3.1.2) ##all the lambs 
  vaclamb <- round(lambs*vac.eff, digits = 0) #all the vaccinated lambs 
  tmp.lamb <- lambs - vaclamb  #unvaccinated lambs 
  
  tmp <- c(tmp.lamb, vaclamb) ####not vaccinated, vaccinated  
  
  return(tmp)  ##vaccinated lambs
      
  }

