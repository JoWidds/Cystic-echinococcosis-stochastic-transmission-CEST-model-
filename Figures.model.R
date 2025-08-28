rm(list=ls())

#### Importing all files #### 
#names <- 1:60
files = list.files(pattern= "*.csv")
myfiles = lapply(files, function(x) read.csv(x, stringsAsFactors = T))
names(myfiles) <- c("dog1", "dog2", "dog3", "dog4", 
                    "eggs1", "eggs2", "eggs3","eggs4",    
                    "lamb1", "lamb2", "lamb3","lamb4", 
                    "sheep1", "sheep2","sheep3","sheep4",   
                    "Elim.dog1", "Elim.dog2", "Elim.dog3", "Elim.dog4",  
                    "Elim.sheep1", "Elim.sheep2", "Elim.sheep3", "Elim.sheep4")

library(scales) ##needed for alpha value in polygon 

#### Timeline  horizon for plots 
# 20+29+16+10  
yr = 20+29+16+10 #20 yr burn in (start 1960), then worming 29 years(1980), EG95 16 years(2009).
Year <- seq(1,yr, 1)
plot <- 26#10# 55
x <- Year[((yr-plot)):yr] ##specify from which year onwards (16 yrs vac onwards) 

labels <- seq(from =1960+yr-plot, to = 1960+yr, by =1)

### Dogs plot ####
svg(file = "C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Final Model Simulations/Figures/Model.projections.dogs&sheep.svg",
    height = 9, width = 9)
    #height = 11, width = 9)
par(mfcol=c(3,2))
#par(mfrow=c(5,1))
par(mar = c(5.1,5.1,2.1,1.0), xaxs = "i", yaxs = "i") ##(bottom, left, top, right)


### Scenario 2 
plot(x, myfiles[["dog1"]][2,(yr-plot):yr], type = "l", lty = 1, lwd =1, 
     main = "Dogs", 
     xlab = "Timeline  (years) ", ylab = "Prevalence %", 
     ylim = c(0, 20), xaxt = 'n', bty = "l",
     pch = 20, col = "#FFFFFF", 
     cex.main = 1.5, cex.axis =1.5, cex.lab = 1.5)
mtext("LF Scenario"   , side = 3, adj = 0.05, cex = 0.8)
axis(1, at= x, labels = labels)
abline(v = 65, lty = "dotted", lwd = 2, col = "black")

lines(x, myfiles[["dog1"]][2,(yr-plot):yr], type = "l", lwd=1.7, col = "gray54") #reference scenario 
polygon(c(x, rev(x)), 
        c(myfiles[["dog1"]][1,(yr-plot):yr], rev(myfiles[["dog1"]][3,(yr-plot):yr])), 
        col = alpha("gray54", 0.45), border = NA) 
### Add alternative scenario 2
lines(x, myfiles[["dog2"]][2,(yr-plot):yr], type = "l", lty = 1, lwd=1.7, col = "#3399FF")
polygon(c(x, rev(x)), 
        c(myfiles[["dog2"]][1,(yr-plot):yr], rev(myfiles[["dog2"]][3,(yr-plot):yr])), 
        col = alpha("#3399FF", 0.30), border = NA) 

### Scenario 3 
plot(x, myfiles[["dog1"]][2,(yr-plot):yr], type = "l", lty = 1, lwd=1.7, 
     #main = "Prevalence in dogs", 
     xlab = "Timeline  (years) ", ylab = "Prevalence %", 
     ylim = c(0, 20), xaxt = 'n', bty = "l",
     pch = 20, col = "#FFFFFF", 
     cex.main = 1.5, cex.axis =1.5, cex.lab = 1.5)
mtext("HF Scenario"   , side = 3, adj = 0.05, cex = 0.8)
axis(1, at= x, labels = labels)
abline(v = 65, lty = "dotted", lwd = 2, col = "black")

lines(x,myfiles[["dog1"]][2,(yr-plot):yr], type = "l", lwd=1.7, col = "gray54") #reference scenario (1.3)  
polygon(c(x, rev(x)), 
        c(myfiles[["dog1"]][1,(yr-plot):yr], rev(myfiles[["dog1"]][3,(yr-plot):yr])), 
        col = alpha("gray54", 0.45), border = NA) 
### Add alternative scenario 3
lines(x, myfiles[["dog3"]][2,(yr-plot):yr], type = "l", lty = 1, lwd=1.7, col = "#CD6839")
polygon(c(x, rev(x)), 
        c(myfiles[["dog3"]][1,(yr-plot):yr], rev(myfiles[["dog3"]][3,(yr-plot):yr])), 
        col = alpha("#CD6839", 0.30), border = NA) 

### Scenario 4 
plot(x, myfiles[["dog1"]][2,(yr-plot):yr], type = "l", lty = 1, lwd=1.7, 
     #main = "Prevalence in dogs", 
     xlab = "Timeline  (years) ", ylab = "Prevalence %", 
     ylim = c(0, 20), xaxt = 'n', bty = "l",
     pch = 20, col = "#FFFFFF", 
     cex.main = 1.5, cex.axis =1.5, cex.lab = 1.5)
mtext("IC Scenario"   , side = 3, adj = 0.05, cex = 0.8)
axis(1, at= x, labels = labels)
abline(v = 65, lty = "dotted", lwd = 2, col = "black")

lines(x, myfiles[["dog1"]][2,(yr-plot):yr], type = "l", lwd=1.7, col = "gray54") #reference scenario (1.3)  
polygon(c(x, rev(x)), 
        c(myfiles[["dog1"]][1,(yr-plot):yr], rev(myfiles[["dog1"]][3,(yr-plot):yr])), 
        col = alpha("gray54", 0.45), border = NA) 
### Add alternative scenario 4
lines(x, myfiles[["dog4"]][2,(yr-plot):yr], type = "l", lty = 1, lwd=1.7, col = "#912CEE")
polygon(c(x, rev(x)), 
        c(myfiles[["dog4"]][1,(yr-plot):yr], rev(myfiles[["dog4"]][3,(yr-plot):yr])), 
        col = alpha("#912CEE", 0.2), border = NA) 

### sheep plots ###
### Scenario 2 
plot(x, myfiles[["sheep1"]][2,(yr-plot):yr], type = "l", lty = 1, lwd=1.7, 
     main = "Sheep", 
     xlab = "Timeline  (years) ", ylab = "Prevalence %", 
     ylim = c(0, 60), xaxt = 'n', bty = "l",
     pch = 20, col = "#FFFFFF", 
     cex.main = 1.5, cex.axis =1.5, cex.lab = 1.5)
axis(1, at= x, labels = labels)
abline(v = 65, lty = "dotted", lwd = 2, col = "black")

lines(x, myfiles[["sheep1"]][2,(yr-plot):yr], type = "l", lwd=1.7, col = "gray54") #reference scenario (1.3)  
polygon(c(x, rev(x)), 
        c(myfiles[["sheep1"]][1,(yr-plot):yr], rev(myfiles[["sheep1"]][3,(yr-plot):yr])), 
        col = alpha("gray54", 0.45), border = NA) 
### Add alternative scenario 2
lines(x, myfiles[["sheep2"]][2,(yr-plot):yr], type = "l", lty = 1, lwd=1.7, col = "#3399FF")
polygon(c(x, rev(x)), 
        c(myfiles[["sheep2"]][1,(yr-plot):yr], rev(myfiles[["sheep2"]][3,(yr-plot):yr])), 
        col = alpha("#3399FF", 0.30), border = NA) 

### Scenario 3 
plot(x, myfiles[["sheep1"]][2,(yr-plot):yr], type = "l", lty = 1, lwd=1.7, 
     xlab = "Timeline  (years) ", ylab = "Prevalence %", 
     ylim = c(0, 60), xaxt = 'n', bty = "l",
     pch = 20, col = "#FFFFFF", 
     cex.main = 1.5, cex.axis =1.5, cex.lab = 1.5)
axis(1, at= x, labels = labels)
abline(v = 65, lty = "dotted", lwd = 2, col = "black")

lines(x, myfiles[["sheep1"]][2,(yr-plot):yr], type = "l", lwd=1.7, col = "gray54") #reference scenario (1.3)  
polygon(c(x, rev(x)), 
        c(myfiles[["sheep1"]][1,(yr-plot):yr], rev(myfiles[["sheep1"]][3,(yr-plot):yr])), 
        col = alpha("gray54", 0.45), border = NA) 
### Add alternative scenario 3
lines(x, myfiles[["sheep3"]][2,(yr-plot):yr], type = "l", lty = 1, lwd=1.7, col = "#CD6839")
polygon(c(x, rev(x)), 
        c(myfiles[["sheep3"]][1,(yr-plot):yr], rev(myfiles[["sheep3"]][3,(yr-plot):yr])), 
        col = alpha("#CD6839", 0.30), border = NA) 

### Scenario 4 
plot(x, myfiles[["sheep1"]][2,(yr-plot):yr], type = "l", lty = 1, lwd=1.7, 
     xlab = "Timeline  (years) ", ylab = "Prevalence %", 
     ylim = c(0, 60), xaxt = 'n', bty = "l",
     pch = 20, col = "#FFFFFF", 
     cex.main = 1.5, cex.axis =1.5, cex.lab = 1.5)
axis(1, at= x, labels = labels)
abline(v = 65, lty = "dotted", lwd = 2, col = "black")

lines(x, myfiles[["sheep1"]][2,(yr-plot):yr], type = "l", lwd=1.7, col = "gray54") #reference scenario (1.3)  
polygon(c(x, rev(x)), 
        c(myfiles[["sheep1"]][1,(yr-plot):yr], rev(myfiles[["sheep1"]][3,(yr-plot):yr])), 
        col = alpha("gray54", 0.45), border = NA) 
### Add alternative scenario 4
lines(x, myfiles[["sheep4"]][2,(yr-plot):yr], type = "l", lty = 1, lwd=1.7, col = "#912CEE")
polygon(c(x, rev(x)), 
        c(myfiles[["sheep4"]][1,(yr-plot):yr], rev(myfiles[["sheep4"]][3,(yr-plot):yr])), 
        col = alpha("#912CEE", 0.2), border = NA) 

dev.off() 

##Final prevalence values for 2025 and 2035  

#End retrospective analysis (2025)
n <- yr-10          
    nth_columns <- lapply(myfiles, function(df) {
                if (n <= ncol(df)) {
                    df[, n]
                        } else {
                NULL  
            }
        })

nth_columns_retro <- nth_columns[sapply(nth_columns, function(x) !is.null(x))]

#End prospective analysis (2035)
n <- yr          
nth_columns <- lapply(myfiles, function(df) {
  if (n <= ncol(df)) {
    df[, n]
  } else {
    NULL  
  }
})

nth_columns_prospective <- nth_columns[sapply(nth_columns, function(x) !is.null(x))]


#### EGG plots ######
plot <- 10 #26#10# 55
x <- Year[((yr-plot)):yr] 
labels <- seq(from =1960+yr-plot, to = 1960+yr, by =1)

###Median egg proportion (scenario/counterfactual)
Egg.S2 <- as.numeric(myfiles$eggs2[2,(yr-plot):yr] / myfiles$eggs1[2,(yr-plot):yr]) 
Egg.S3 <- as.numeric(myfiles$eggs3[2,(yr-plot):yr] / myfiles$eggs1[2,(yr-plot):yr]) 
Egg.S4 <- as.numeric(myfiles$eggs4[2,(yr-plot):yr] / myfiles$eggs1[2,(yr-plot):yr])  

eggs.med <-  matrix(data = c(Egg.S2,Egg.S3,Egg.S4), nrow = 3, byrow = T)
eggs.med <- data.frame(eggs.med)

###Low egg proportion (scenario/counterfactual)
Egg.S2 <- as.numeric(myfiles$eggs2[1,(yr-plot):yr] / myfiles$eggs1[1,(yr-plot):yr]) 
Egg.S3 <- as.numeric(myfiles$eggs3[1,(yr-plot):yr] / myfiles$eggs1[1,(yr-plot):yr])  
Egg.S4 <- as.numeric(myfiles$eggs4[1,(yr-plot):yr] / myfiles$eggs1[1,(yr-plot):yr])  


eggs.low <-  matrix(data = c(Egg.S2,Egg.S3,Egg.S4), nrow = 3, byrow = T)
eggs.low <- data.frame(eggs.low)

###High egg proportion (scenario/counterfactual)

Egg.S2 <- as.numeric(myfiles$eggs2[3,(yr-plot):yr] / myfiles$eggs1[3,(yr-plot):yr]) 
Egg.S3 <- as.numeric(myfiles$eggs3[3,(yr-plot):yr]/ myfiles$eggs1[3,(yr-plot):yr])  
Egg.S4 <- as.numeric(myfiles$eggs4[3,(yr-plot):yr]/ myfiles$eggs1[3,(yr-plot):yr])  

eggs.high <-  matrix(data = c(Egg.S2,Egg.S3,Egg.S4),nrow = 3, byrow = T)
eggs.high <- data.frame(eggs.high)



### Eggs plot #### natural log log e/ln 
svg(file = "C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Final Model Simulations/Figures/Model.projections.eggs.svg", 
    height = 6.5, width = 12)
par(mfrow=c(1,1))
par(mar = c(5.1,6.0,3.5,1.0), xaxs = "i", yaxs = "i") ##(bottom, left, top, right)

### Scenario 2 
plot(log(x), log(myfiles$eggs1[2,(yr-plot):yr]/myfiles$eggs1[2,(yr-plot):yr]), 
     type = "l", lty = 2, lwd =1, 
     #main = "Proportional change of E.granulosus eggs \nrelative to counterfactual scenario", 
     xlab = "Timeline  (years)", ylab = "Proportion (Log e)", 
     ylim = c(-0.03, 0.06), 
     xaxt = 'n', bty = "l", 
     pch = 20, col = "#000000", 
     cex.main = 1.5, cex.axis =1.5, cex.lab = 1.5, 
     xaxs = "i",
     yaxs = "i")
#mtext("Scenario 2", side = 3, adj = 0.05, cex = 0.8)
axis(1, at= log(x), labels = labels)
lines(log(x), log(eggs.med[1,]), type = "l", lwd = 1.7, col = "#3399FF") #reference scenario 2  
lines(log(x), log(eggs.med[2,]), type = "l", lwd = 1.7, col = "#CD6839") #reference scenario 3  
lines(log(x), log(eggs.med[3,]), type = "l", lwd = 1.7, col = "#912CEE") #reference scenario 4  


legend("topleft", inset=.01, legend=c("CF Scenario", "LF Scenario", "HF Scenario", "IC Scenario"), lwd = 2, 
       col = c("#000000", "#3399FF","#CD6839","#912CEE"), lty = c(2, 1,1,1),
       bty = "n") 

dev.off() 


### Elim plots 
Elim.dog1 <- myfiles$Elim.dog1[seq(1, nrow(myfiles$Elim.dog1), 12),]
Elim.dog2 <- myfiles$Elim.dog2[seq(1, nrow(myfiles$Elim.dog2), 12),]
Elim.dog3 <- myfiles$Elim.dog3[seq(1, nrow(myfiles$Elim.dog3), 12),]
Elim.dog4 <- myfiles$Elim.dog4[seq(1, nrow(myfiles$Elim.dog4), 12),]


Elim.sheep1 <- myfiles$Elim.sheep1[seq(1, nrow(myfiles$Elim.sheep1), 12),]
Elim.sheep2 <- myfiles$Elim.sheep2[seq(1, nrow(myfiles$Elim.sheep2), 12),]
Elim.sheep3 <- myfiles$Elim.sheep3[seq(1, nrow(myfiles$Elim.sheep3), 12),]
Elim.sheep4 <- myfiles$Elim.sheep4[seq(1, nrow(myfiles$Elim.sheep4), 12),]


Elim.dog1[yr,] #scenario 1 
Elim.dog2[yr,] #scenario 2
Elim.dog3[yr,] #scenario 3
Elim.dog4[yr,] #scenario 4
 
Elim.sheep1[yr,] #scenario 1 
Elim.sheep2[yr,] #scenario 2
Elim.sheep3[yr,] #scenario 3
Elim.sheep4[yr,] #scenario 4

#x <- Year[(yr-plot):yr] ##specify from which year onwards (burn in of 20 yrs)
#labels <- c("0","","","","","5","","","","","10","","","","","15","","","","","20")

#### Interuprtion of tranmission PLOTS 
svg(file = "C:/Users/jw0104/OneDrive - University of Surrey/Documents/Argentina - echino/Modelling/Final Model Simulations/Figures/Model.projections.IofT.svg", 
    height = 6, width = 12)
par(mfrow=c(2,3))
par(mar = c(5.1,6.0,3.5,1.0), xaxs = "i", yaxs = "i") ##(bottom, left, top, right)
#labels <- c("0","","","","","5","","","","","10","","","","","15","","","","","20","","","","","25", "","","","","30","","","","","35","","","","","40")

### Dogs 20 % 
plot(x, Elim.dog1[(yr-plot):yr,1], 
     type = "l", lty = 1, lwd =1, 
     main = "Dogs", 
     xlab = "Timeline  (years)", ylab = "Percentage", 
     ylim = c(60, 100), xaxt = 'n', bty = "l",
     pch = 20, col = "#000000", 
     cex.main = 1.5, cex.axis =1.5, cex.lab = 1.5, 
     xaxs = "i",
     yaxs = "i")
mtext("20 %", side = 3, adj = 0.05, cex = 0.8)
axis(1, at= x, labels = labels)
lines(x, Elim.dog2[(yr-plot):yr,1], type = "l", lwd =1, col = "#3399FF") #reference scenario 2
lines(x, Elim.dog3[(yr-plot):yr,1], type = "l", lwd =1, col = "#CD6839") #reference scenario 3
lines(x, Elim.dog4[(yr-plot):yr,1], type = "l", lwd =1, col = "#912CEE") #reference scenario 4



## 10%
plot(x, Elim.dog1[(yr-plot):yr,2], 
     type = "l", lty = 1, lwd =1, 
     #main = "Dogs", 
     xlab = "Timeline  (years)", ylab = "Percentage", 
     ylim = c(60, 100), xaxt = 'n', bty = "l",
     pch = 20, col = "#000000", 
     cex.main = 1.5, cex.axis =1.5, cex.lab = 1.5, 
     xaxs = "i",
     yaxs = "i")
mtext("10 %", side = 3, adj = 0.05, cex = 0.8)
axis(1, at= x, labels = labels)
lines(x, Elim.dog2[(yr-plot):yr,2], type = "l", lwd =1, col = "#3399FF") #reference scenario 2
lines(x, Elim.dog3[(yr-plot):yr,2], type = "l", lwd =1, col = "#CD6839") #reference scenario 3
lines(x, Elim.dog4[(yr-plot):yr,2], type = "l", lwd =1, col = "#912CEE") #reference scenario 4



### 0%
plot(x, Elim.dog1[(yr-plot):yr,3], 
     type = "l", lty = 1, lwd =1, 
     #main = "Dogs", 
     xlab = "Timeline  (years)", ylab = "Percentage", 
     ylim = c(60, 100), xaxt = 'n', bty = "l",
     pch = 20, col = "#000000", 
     cex.main = 1.5, cex.axis =1.5, cex.lab = 1.5, 
     xaxs = "i",
     yaxs = "i")
mtext("0 %", side = 3, adj = 0.05, cex = 0.8)
axis(1, at= x, labels = labels)
lines(x, Elim.dog2[(yr-plot):yr,3], type = "l", lwd =1, col = "#3399FF") #reference scenario 2
lines(x, Elim.dog3[(yr-plot):yr,3], type = "l", lwd =1, col = "#CD6839") #reference scenario 3
lines(x, Elim.dog4[(yr-plot):yr,3], type = "l", lwd =1, col = "#912CEE") #reference scenario 4


### Sheep 20 % 
plot(x, Elim.sheep1[(yr-plot):yr,1], 
     type = "l", lty = 1, lwd =1, 
     main = "Sheep", 
     xlab = "Timeline  (years)", ylab = "Percentage", 
     ylim = c(60, 100), xaxt = 'n', bty = "l",
     pch = 20, col = "#000000", 
     cex.main = 1.5, cex.axis =1.5, cex.lab = 1.5, 
     xaxs = "i",
     yaxs = "i")
mtext("20 %", side = 3, adj = 0.05, cex = 0.8)
axis(1, at= x, labels = labels)
lines(x, Elim.sheep2[(yr-plot):yr,1], type = "l", lwd =1, col = "#3399FF") #reference scenario 2
lines(x, Elim.sheep3[(yr-plot):yr,1], type = "l", lwd =1, col = "#CD6839") #reference scenario 3 
lines(x, Elim.sheep4[(yr-plot):yr,1], type = "l", lwd =1, col = "#912CEE") #reference scenario 4



## 10%
plot(x, Elim.sheep1[(yr-plot):yr,2], 
     type = "l", lty = 1, lwd =1, 
     #main = "Dogs", 
     xlab = "Timeline  (years)", ylab = "Percentage", 
     ylim = c(60, 100), xaxt = 'n', bty = "l",
     pch = 20, col = "#000000", 
     cex.main = 1.5, cex.axis =1.5, cex.lab = 1.5, 
     xaxs = "i",
     yaxs = "i")
mtext("10 %", side = 3, adj = 0.05, cex = 0.8)
axis(1, at= x, labels = labels)
lines(x, Elim.sheep2[(yr-plot):yr,2], type = "l", lwd =1, col = "#3399FF") #reference scenario 2
lines(x, Elim.sheep3[(yr-plot):yr,2], type = "l", lwd =1, col = "#CD6839") #reference scenario 3
lines(x, Elim.sheep4[(yr-plot):yr,2], type = "l", lwd =1, col = "#912CEE") #reference scenario 4


### 0%
plot(x, Elim.sheep1[(yr-plot):yr,3], 
     type = "l", lty = 1, lwd =1, 
     #main = "Dogs", 
     xlab = "Timeline  (years)", ylab = "Percentage", 
     ylim = c(60, 100), xaxt = 'n', bty = "l",
     pch = 20, col = "#000000", 
     cex.main = 1.5, cex.axis =1.5, cex.lab = 1.5, 
     xaxs = "i",
     yaxs = "i")
mtext("0 %", side = 3, adj = 0.05, cex = 0.8)
axis(1, at= x, labels = labels)
lines(x, Elim.sheep2[(yr-plot):yr,3], type = "l", lwd =1, col = "#3399FF") #reference scenario 2 
lines(x, Elim.sheep3[(yr-plot):yr,3], type = "l", lwd =1, col = "#CD6839") #reference scenario 3 
lines(x, Elim.sheep4[(yr-plot):yr,3], type = "l", lwd =1, col = "#912CEE") #reference scenario 4 

legend("topright", legend=c("CF Scenario", "LF Scenario", "HF Scenario", "IC Scenario"), lwd = 2,
       col = c("#000000","#3399FF","#CD6839","#912CEE"),
       bty = "n")



dev.off() 


