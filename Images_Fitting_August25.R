##### Figures for fitting of CEST model- August 2025 #####
rm(list=ls())  
setwd ("C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Fitting")
library(rstudioapi)
library(scales) ##needed for alpha value in polygon 

Final.prev <- read.csv('C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Fitting/Final.prev.csv') #priors
correct_param1 <- read.csv('C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Fitting/ParticleSim1.csv') ##First fit 
correct_param2 <- read.csv('C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Fitting/ParticleSim2.csv') ##Second fit  
correct_param3 <- read.csv('C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Fitting/ParticleSim3.csv') ##third fit
correct_param4 <- read.csv('C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Fitting/ParticleSim4.csv') ##fourth fit
correct_param5 <- read.csv('C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Fitting/ParticleSim5.csv') ##fifth
correct_param6 <- read.csv('C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Fitting/ParticleSim6.csv') ##Posteriors 



svg("PosteriorsD&L_SMC.2.svg",
    height = 6, width = 13)
par(mfrow=c(1,2))

plot(density(correct_param6$cbetaD),col="#FFFFFF", main="", 
     xlim = c(min(Final.prev$dog_beta_vec)-3,max(correct_param6$cbetaD)+2),
     ylim = c(0, 1),
     bty="n",
     xlab = expression(paste("Prior and Posterior distributions of", ~ beta, ~"Dog")))
lines(density(Final.prev$dog_beta_vec), col='blue') #prior
lines(density(correct_param1$cbetaD), col=alpha('blue', .45), lty=6) #1st fit  
lines(density(correct_param2$cbetaD), col=alpha('blue', .45), lty=2) #2nd fit  
lines(density(correct_param3$cbetaD), col=alpha('blue', .45), lty=5) #3rd fit 
lines(density(correct_param4$cbetaD), col=alpha('blue', .45), lty=3) #4th fit 
lines(density(correct_param5$cbetaD), col=alpha('blue', .45), lty=1) #final fit 
lines(density(correct_param6$cbetaD),col="red", lwd = 1) ##posterior 

plot(density(correct_param6$cbetaL),col = "#FFFFFF", main="",
     xlim = c(min(Final.prev$sheep_beta_vec)-0.1,max(correct_param6$cbetaL)+.1),
     ylim = c(0, 20),
     bty="n",
     xlab = expression(paste("Prior and Posterior distributions of", ~ beta, ~"Lamb")))
lines(density(Final.prev$sheep_beta_vec), col='blue') #prior 
lines(density(correct_param1$cbetaL), col=alpha('blue', .45), lty=6) #1st fit  
lines(density(correct_param2$cbetaL), col=alpha('blue', .45), lty=2) #2nd fit  
lines(density(correct_param3$cbetaL), col=alpha('blue', .45), lty=5) #3rd fit 
lines(density(correct_param4$cbetaL), col=alpha('blue', .45), lty=3) #4th fit 
lines(density(correct_param5$cbetaL), col=alpha('blue', .45), lty=1) #final fit 
lines(density(correct_param6$cbetaL),col = "red",lwd = 1) ##posterior 




legend('topleft', legend=c("prior","ABC-SMC 1","ABC-SMC 2", "ABC-SMC 3", "ABC-SMC 4", "ABC-SMC 5", "posterior"), lty = c(1,6,2,5,3,1,1),
       col=c("blue", "blue", "blue", alpha("blue", .45),alpha("blue", .85),alpha("blue", .45), "red"), cex=0.8, bty = "n")

dev.off()

D_max <- max(Final.prev$dog_beta_vec)#Prior 
D_max1 <- max(correct_param1$cbetaD) #First fit
D_max2 <- max(correct_param2$cbetaD) #Second
D_max3 <- max(correct_param3$cbetaD) #Third
D_max4 <- max(correct_param4$cbetaD) #Fourth 
D_max5 <- max(correct_param5$cbetaD) #Fifth 
D_max6 <- max(correct_param6$cbetaD) #posterior 

S_max <- max(correct_param6$cbetaL)  #prior 
S_max1 <- max(correct_param1$cbetaL) #First fit 
S_max2 <- max(correct_param2$cbetaL) #Second
S_max3 <- max(correct_param3$cbetaL) #Third 
S_max4 <- max(correct_param4$cbetaL) #Fourth 
S_max5 <- max(correct_param5$cbetaL) #Fifth 
S_max6 <- max(correct_param6$cbetaL) #posterior 

svg("PosteriorsD&L_SMC.svg",
    height = 6, width = 13)
par(mfrow=c(1,2))


plot(density(correct_param6$cbetaD/D_max),col="#FFFFFF", main="", 
     xlim = c(0.1,1.2),
     ylim = c(0,5),
     bty="n",
     xlab = expression(paste("Prior and Posterior distributions of", ~ beta, ~"Dog")),
     ylab = "Scaled density")
lines(density(Final.prev$dog_beta_vec/D_max), col='blue')#prior
lines(density(correct_param1$cbetaD/D_max1), col=alpha('blue', 1), lty=6) #1st fit  
lines(density(correct_param2$cbetaD/D_max2), col=alpha('blue', 1), lty=2) #2nd fit  
lines(density(correct_param3$cbetaD/D_max3), col=alpha('blue', .45), lty=5) #3rd fit 
lines(density(correct_param4$cbetaD/D_max4), col=alpha('blue', .85), lty=3) #4th fit 
lines(density(correct_param5$cbetaD/D_max5), col=alpha('blue', .45), lty=1) #final fit 
lines(density(correct_param6$cbetaD/D_max6),col="red", lwd = 1.2) ##posterior 


plot(density(correct_param6$cbetaL/S_max),col = "#FFFFFF", main= "",
     xlim = c(0.8,1.1),
     ylim = c(0,20),
     bty="n",
     xlab = expression(paste("Prior and Posterior distributions of", ~ beta, ~"Sheep")),
     ylab = "Scaled density")
lines(density(Final.prev$sheep_beta_vec/S_max), col='blue') 
#lines(density(Final.prev$sheep_beta_vec), col='blue') #prior 
lines(density(correct_param1$cbetaL/S_max1), col=alpha('blue', 1), lty=6) #1st fit
lines(density(correct_param2$cbetaL/S_max2), col=alpha('blue', 1), lty=2) #2nd fit  
lines(density(correct_param3$cbetaL/S_max3), col=alpha('blue', .45), lty=5) #3rd fit 
lines(density(correct_param4$cbetaL/S_max4), col=alpha('blue', .85), lty=3) #4th fit 
lines(density(correct_param5$cbetaL/S_max5), col=alpha('blue', .45), lty=1) #final fit 
lines(density(correct_param6$cbetaL/S_max6),col = "red",lwd = 1.2) ##posterior 

legend('topright', legend=c("prior","ABC-SMC 1","ABC-SMC 2", "ABC-SMC 3", "ABC-SMC 4", "ABC-SMC 5", "posterior"), lty = c(1,6,2,5,3,1,1),
       col=c("blue", "blue", "blue", alpha("blue", .45),alpha("blue", .85),alpha("blue", .45), "red"), cex=0.8, bty = "n", inset = 0.05)

dev.off()

#Beta values fitted to prev 4.5% and 56.3% in dogs and sheep respectively (2009)
Final.fit.outputs <- read.csv('C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Fitting/Sim5a.csv') ##Posteriors 

svg("Fitting_to_values.svg",
    height = 6, width = 15)

par(mfrow=c(1,2))
plot(x = Final.fit.outputs$prevD, y = Final.fit.outputs$prevS, 
     xlim = c(0,12),
     ylim = c(47,60),
     cex = 1,
     pch=4, 
     bty="n",
     xlab = "Final prevalence of Dogs",
     ylab = "Final prevalence of Sheep")
     abline(h= 56.3,col = "red",lwd = 1.2)
     abline(v= 4.5,col = "darkgreen",lwd = 1.2)
     
     legend('topright', legend=c("Fitting point Dogs", "Fitting point Sheep > 2 yo"), lty = c(1,1),
            col=c("darkgreen", "red"), cex=0.8, bty = "n", inset = 0.05)
     
     dev.off()