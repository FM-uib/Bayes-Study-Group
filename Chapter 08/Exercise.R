library(here)
library(rjags)
library(plyr)

# 8.1
frogs <- data.frame(mass = c(10,20,23,32,35),
                    length = c(5,7,10,12,15))

# length[i] = a + b * weight[i] + e[i] and e is normal(0, sigma2)

5 = a * 1 + b * 10 + r1
7 = a * 1 + b * 20 + r2


plot(frogs$length, frogs$mass, 
     xlab = "length", ylab = "mass")

# Linear Model in R
fm <- lm(data = frogs, mass ~ length)
abline(fm, lwd = 3)

# RJAGS
data <- list(x = frogs$length, y = frogs$mass, n = nrow(frogs))

inits <- function() list(alpha=rnorm(1), beta=rnorm(1), sigma = rlnorm(1)) 

params <- c("alpha", "beta", "sigma", "predicted")

jagsModel <- jags.model(file = here("models","linreg.txt"),
                        data = data,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 100) # Burn-in

Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 1000) #Draws from posterior
plot(Samples)
summary(Samples)

mcmc <- as.data.frame(do.call(rbind, Samples))

means<-as.data.frame(t(mcmc[,c(3:7)] %>% summarise_all(funs(mean))))

lines(frogs$length, means[,1], col = "red", lwd = 3)
                    
# 8.2

frogs <- data.frame(mass = c(10,20,23,32,35,NA,NA,NA,NA,NA),
                    length = c(5,7,10,12,15,16,17,18,19,20))
# Bundle Data
data <- list(x = frogs$length, y = frogs$mass, n = nrow(frogs))

# Inits function
inits <- function() list(alpha=rnorm(1), beta=rnorm(1), sigma = rlnorm(1)) 

# Parameters to estimate
params <- c("alpha","beta", "sigma", "predicted", "y")

# Set up Model
jagsModel <- jags.model(file = here("models","linreg.txt"),
                        data = data,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 100) # Burn-in

Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 1000) #Draws from posterior

plot(Samples)
summary(Samples)

mcmc <- as.data.frame(do.call(rbind, Samples))

means<-as.data.frame(t(mcmc[,c(14:23)] %>% summarise_all(funs(mean))))

plot(frogs$length, means[,1], 
     xlab = "length", ylab = "mass")

# 8.3

hares <- read.table(file = here("data","hares.data.txt"), header = TRUE)

# removes NAs, mean over the different years. alternatively only take last X years.
# to avoid pseudoreplication
hares.ymean <- hares %>%
  filter(!is.na(mean.density) & landuse == "grass") %>% # filter out NAs
  group_by(year) %>% 
  summarize(mean.density = mean(mean.density), # mean yearly density
            n = n())

data <- list(x = hares.ymean$year-1991, y = hares.ymean$mean.density, n = nrow(hares.ymean))

inits <- function() list(alpha=rnorm(1), beta=rnorm(1), sigma = rlnorm(1)) 

params <- c("alpha","beta", "sigma", "residual", "predicted")

jagsModel <- jags.model(file = here("models","linreg.txt"),
                        data = data,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 100) # Burn-in

Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 1000) #Draws from posterior
plot(Samples)
summary(Samples)

mcmc <- as.data.frame(do.call(rbind, Samples))
means<-as.data.frame(t(mcmc[,c(3:19)] %>% summarise_all(funs(mean))))

plot(x = hares.ymean$year-1991, y = hares.ymean$mean.density)
abline(mean(mcmc[,"alpha"]),mean(mcmc[,"beta"]), col = "blue")
lines(hares.ymean$year-1991, means[,1], lwd = 3, col = "red")

# predicted vs data
plot(hares.ymean$mean.density, means[,1], xlab = "data", ylab = "predicted")
abline(0,1)

# Residuals
resi<-as.data.frame(t(mcmc[,c(20:36)] %>% summarise_all(funs(mean))))
plot(hares.ymean$year, resi[,1])
abline(0,0)

# A quadratic regression would fit better.