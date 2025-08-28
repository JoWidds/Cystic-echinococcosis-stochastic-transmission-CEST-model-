
run_ABC_fit <- function(fit) {
  

library(scales) ##needed for alpha value in polygon 

Final.prev <-Final.prev[,-4]## remove the adult sheep and fit to lambs  #Final.prev[,-4]## remove the lamb prev and fit to adult sheep  
data <-  data.frame(4.5, 56.3) #65.2) # target prevalence values dogs/all sheep 
colnames(data)<- c('prevD','prevS') #naming the columns c('prevD','prevS')

M <- nrow(Final.prev) 
k <- apply(Final.prev[3:4], 2, sd) # Calculate the Empirical standard deviation, standardizing the data#


# Define rho(S(x),S(y)) - This is basically the distance to your "target prevalence"
Sim <- Final.prev
rho<- sqrt(((Sim$prevD-data[1,1])^2)/k[1] + ((Sim$prevS-data[1,2])^2)/k[2])
Sim <- cbind(Sim, rho) 

write.csv(Sim, file = paste0('Sim', fit,'a', '.csv'), row.names = FALSE)


# First fit, rejection based, we keep the lowest rho, ie. those closest to target prev (best fitting ones)
N=1000
prop<-N/M
error <- quantile(Sim$rho,probs=prop) #take 0.1 lowest rows, lower error = decreased distance from target prev 
SimKept <- subset(Sim,Sim$rho<error) #keep ones below threshold we've just implemented 

# Weight the values saved 
weight<-(1/error)*(1-(SimKept$rho/error)^2) 

# Second fit regression based relationship - Apply the weighted linear regression E(theta|S(x))
lbetaD <- lm(dog_beta_vec ~ prevD+prevS, data = SimKept, weights=weight) 
lbetaL <- lm(sheep_beta_vec ~ prevD+prevS, data = SimKept, weights=weight) 

# Calculate E(theta|S(y))
predbetaD <-predict(lbetaD,data)
predbetaL <-predict(lbetaL,data)

# Correct the parameters
cbetaD <- SimKept$dog_beta_vec-lbetaD$fitted.values+predbetaD
cbetaL <- SimKept$sheep_beta_vec -lbetaL$fitted.values+predbetaL
correct_param<-as.data.frame(cbind(cbetaD,cbetaL))
correct_param <- correct_param[correct_param$cbetaD >= 0, ] ##remove negative values?! 

write.csv(correct_param, file = paste0('ParticleSim', fit+1, '.csv'), row.names = FALSE)

}
