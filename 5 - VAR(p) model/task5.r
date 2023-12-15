
Phi1 <- c(-0.4,0.1,0.2)
Phi1 <- rbind(Phi1,c(0.1,0.2,0.3))
Phi1 <- rbind(Phi1,c(0.1,0.3,0.2))
Phi1

Phi2 <- c(-0.4,0.3,0.2)
Phi2 <- rbind(Phi2,c(0,0.3,0.2))
Phi2 <- rbind(Phi2,c(0.3,-0.2,0.2))
Phi2

Phi <- cbind(Phi1,Phi2)
Phi

Omega <- c(1,0,0)
Omega <- rbind(Omega,c(0,2,0))
Omega <- rbind(Omega,c(0,0,4))
Omega

I <- diag(1,3,3)
Zero <- diag(0,3,3)
IZ <- cbind(I,Zero)
F <- rbind(Phi,IZ)
F

eval <- eigen(F,only.values= TRUE)
modev <- Mod(eval$values)
barplot(modev,col = "blue",main = " Abs value of eigen values")
abline(h=1,col = "black")


data <- read.csv(file = file.choose(), header = TRUE)
head(data)

ccf(data$x, data$y, lwd = 5, main = "Cross-correlation of x_t and y_t",col = "blue")
 
res <- list(variant = num,
            eigen_values = modev,
            stationarity = 'Yes'/'No',
            delay        = numnum,
            ahead        = 'x'/'y'
            
)

res

saveRDS(res,'result.rds')
